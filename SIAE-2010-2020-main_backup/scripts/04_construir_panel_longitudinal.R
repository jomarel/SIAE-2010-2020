# ============================================================
# 3_unir todo y crear panel.R
# Apila los df_YYYY.csv en un panel longitudinal df_final.
# Detecta conflictos de tipo, limpia variables all-NA y
# genera auditoría completa del panel.
#
# Protección especial: u1..u104 y u900 nunca se eliminan.
# ============================================================

config_path <- if (file.exists("scripts/00_config.R")) "scripts/00_config.R" else "00_config.R"
source(config_path)

required_pkgs <- c("dplyr", "readr")
missing_pkgs <- required_pkgs[!vapply(required_pkgs, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing_pkgs) > 0) {
  stop(
    "Faltan paquetes: ", paste(missing_pkgs, collapse = ", "),
    "\nInstálalos con install.packages().",
    call. = FALSE
  )
}

library(dplyr)
library(readr)

# ============================================================
# CONSTANTES
# ============================================================

PROTECTED_VARS <- c(paste0("u", 1:104), "u900", "NCODI", "anyo")

is_protected <- function(name) name %in% PROTECTED_VARS

# ============================================================
# 1. LEER df_YYYY.csv
# ============================================================

year_files <- list.files(
  path      = LEGACY_BASE_DIR,
  pattern   = "^df_[0-9]{4}\\.csv$",
  full.names = TRUE
)

if (length(year_files) == 0) {
  stop(
    "No se encontraron archivos df_YYYY.csv en: ", LEGACY_BASE_DIR,
    "\nAsegúrate de ejecutar el script 2 antes.",
    call. = FALSE
  )
}

years_avail <- as.integer(sub("^df_([0-9]{4})\\.csv$", "\\1", basename(year_files)))
ord         <- order(years_avail)
year_files  <- year_files[ord]
years_avail <- years_avail[ord]

message("Archivos encontrados: ", length(year_files),
        " (", min(years_avail), "-", max(years_avail), ")")

list_df <- vector("list", length(year_files))

for (idx in seq_along(year_files)) {
  tmp <- readr::read_delim(
    file           = year_files[idx],
    delim          = ",",
    col_types      = cols(.default = col_character()),
    show_col_types = FALSE,
    progress       = FALSE,
    name_repair    = "minimal"
  ) %>% as.data.frame(stringsAsFactors = FALSE)

  if (!("NCODI" %in% names(tmp))) {
    stop("Falta NCODI en: ", basename(year_files[idx]), call. = FALSE)
  }

  tmp$NCODI <- trimws(as.character(tmp$NCODI))
  tmp$anyo  <- as.integer(years_avail[idx])
  tmp       <- tmp %>% select(anyo, NCODI, everything())

  dup_n <- sum(duplicated(tmp[, c("NCODI", "anyo")]))
  if (dup_n > 0) {
    stop(
      "Duplicados (NCODI, anyo) en ", basename(year_files[idx]), ": ", dup_n,
      call. = FALSE
    )
  }

  list_df[[idx]] <- tmp
}

# ============================================================
# 2. DETECTAR CONFLICTOS DE TIPO ENTRE AÑOS
# ============================================================

message("\nDetectando conflictos de tipo entre años...")

# Para cada variable, registrar el tipo detectado en cada año
type_conflict_rows <- list()

# Función para inferir el tipo de una columna character
infer_type <- function(x) {
  x2 <- trimws(x)
  x2[x2 == ""] <- NA
  if (all(is.na(x2))) return("all_na")
  suppressWarnings(xn <- as.numeric(x2))
  if (all(is.na(x2) == is.na(xn))) return("numeric")
  return("character")
}

# Recopilar tipos por variable y año
type_map <- list()  # type_map[[var]][[year]] = "numeric"|"character"|"all_na"

for (idx in seq_along(list_df)) {
  yy  <- years_avail[idx]
  tmp <- list_df[[idx]]
  for (v in names(tmp)) {
    if (!(v %in% names(type_map))) type_map[[v]] <- list()
    type_map[[v]][[as.character(yy)]] <- infer_type(tmp[[v]])
  }
}

# Identificar conflictos: misma variable, tipos distintos entre años (excluyendo all_na)
resolution_rows <- list()

for (v in names(type_map)) {
  types_by_year <- unlist(type_map[[v]])
  real_types    <- types_by_year[types_by_year != "all_na"]
  if (length(unique(real_types)) <= 1) next  # sin conflicto

  for (yy_str in names(type_map[[v]])) {
    t_detected <- type_map[[v]][[yy_str]]
    if (t_detected == "all_na") next

    resolution_rows[[length(resolution_rows) + 1]] <- data.frame(
      variable      = v,
      year          = as.integer(yy_str),
      type_detected = t_detected,
      resolution    = "convertir_a_character",
      stringsAsFactors = FALSE
    )
  }
}

type_conflict_df <- if (length(resolution_rows) > 0) {
  dplyr::bind_rows(resolution_rows)
} else {
  data.frame(variable=character(), year=integer(),
             type_detected=character(), resolution=character(),
             stringsAsFactors=FALSE)
}

message("Conflictos de tipo detectados: ", nrow(type_conflict_df),
        " (en ", length(unique(type_conflict_df$variable)), " variables)")

write.csv(type_conflict_df, PANEL_TYPE_CONFLICTS_PATH, row.names = FALSE, na = "")

# Forzar character en variables con conflicto (ya están en character: read_delim con col_character)
# list_df ya tiene todo como character → OK para el bind_rows

# ============================================================
# 3. APILAR AÑOS EN PANEL
# ============================================================

message("\nApilando años en panel final...")

df_final <- dplyr::bind_rows(list_df) %>%
  arrange(anyo, NCODI)

n_rows_raw <- nrow(df_final)
n_cols_raw <- ncol(df_final)

message("Panel raw: ", n_rows_raw, " filas × ", n_cols_raw, " columnas")

# ============================================================
# 4. VERIFICAR UNICIDAD DE CLAVE (NCODI, anyo)
# ============================================================

dup_keys <- duplicated(df_final[, c("NCODI", "anyo")])
if (any(dup_keys)) {
  dups_detail <- df_final[dup_keys | duplicated(df_final[, c("NCODI", "anyo")], fromLast = TRUE),
                           c("NCODI", "anyo")]
  message("Primeros duplicados encontrados:")
  print(head(dups_detail, 20))
  stop(
    "La clave (NCODI, anyo) NO es única en el panel: ",
    sum(dup_keys), " filas duplicadas.",
    call. = FALSE
  )
}

message("Unicidad de (NCODI, anyo): OK")

# ============================================================
# 5. CONVERSIÓN DE TIPOS (después del bind_rows)
# ============================================================

message("\nConvirtiendo tipos de columnas...")

cols_to_convert <- setdiff(names(df_final), c("NCODI"))

df_final[cols_to_convert] <- lapply(cols_to_convert, function(v) {
  x <- df_final[[v]]
  x <- trimws(x)
  x[x == ""] <- NA

  # Si tiene conflicto de tipo → mantener como character
  if (v %in% type_conflict_df$variable) return(x)

  # Intentar numérico solo si ningún non-NA se pierde y hay al menos un non-NA
  if (all(is.na(x))) return(x)  # all-NA: no convertir

  suppressWarnings(x_num <- as.numeric(x))
  if (all(is.na(x) == is.na(x_num))) return(x_num)

  x
})

# ============================================================
# 6. ELIMINAR VARIABLES ALL-NA EN TODOS LOS AÑOS
#    Excepción: u1..u104, u900, NCODI, anyo
# ============================================================

message("\nEliminando variables completamente vacías (all-NA en todos los años)...")

is_all_na_global <- function(x) {
  if (is.character(x)) all(is.na(x) | trimws(x) == "")
  else                  all(is.na(x))
}

cols_all_na <- names(df_final)[vapply(df_final, is_all_na_global, logical(1))]
cols_all_na <- setdiff(cols_all_na, PROTECTED_VARS)

message("Variables all-NA a eliminar: ", length(cols_all_na))

if (length(cols_all_na) > 0) {
  writeLines(cols_all_na, VARS_DROPPED_ALL_NA_PATH)
  message("Lista guardada en: ", VARS_DROPPED_ALL_NA_PATH)
  df_final <- df_final %>% select(-all_of(cols_all_na))
} else {
  writeLines("# No se eliminaron variables all-NA.", VARS_DROPPED_ALL_NA_PATH)
}

n_cols_clean <- ncol(df_final)

# ============================================================
# 7. ESTADÍSTICAS PARA AUDITORÍA
# ============================================================

n_hospitales_total   <- n_distinct(df_final$NCODI)
anyo_min             <- min(df_final$anyo, na.rm = TRUE)
anyo_max             <- max(df_final$anyo, na.rm = TRUE)
obs_por_anyo         <- table(df_final$anyo, useNA = "ifany")

# Variables con > 50% NAs (excluyendo u1..u104 y u900)
vars_check_na <- setdiff(names(df_final), c(PROTECTED_VARS))
pct_na_by_var <- vapply(df_final[vars_check_na], function(x) {
  if (is.character(x)) mean(is.na(x) | trimws(x) == "")
  else                  mean(is.na(x))
}, numeric(1))

n_vars_high_na <- sum(pct_na_by_var > 0.50)

# Merges automáticos del script 2 (si el objeto existe en el entorno)
n_auto_merges <- if (exists("N_AUTO_MERGES_APPLIED")) N_AUTO_MERGES_APPLIED else NA_integer_

# ============================================================
# 8. ENRIQUECER CON nombre_hospital (y ccaa_cnh, provincia_cnh)
#    Fuente: data_intermediate/ncodi_hospital_map.csv
#    Generado por 0_exportar_mapeo_cnh.R
# ============================================================

message("\nEnriqueciendo panel con nombre_hospital desde mapeo CNH...")

enrich_with_hospital_name <- function(df, map_path) {
  if (!file.exists(map_path)) {
    warning(
      "Mapeo NCODI-hospital no encontrado: ", map_path,
      "\nEjecuta 0_exportar_mapeo_cnh.R para generarlo.",
      call. = FALSE
    )
    return(df)
  }

  map_df <- tryCatch(
    readr::read_csv(map_path, col_types = readr::cols(.default = "c"),
                    show_col_types = FALSE),
    error = function(e) {
      warning("No se pudo leer el mapeo: ", conditionMessage(e), call. = FALSE)
      NULL
    }
  )
  if (is.null(map_df)) return(df)

  # Seleccionar columnas de enriquecimiento
  cols_enrich <- intersect(
    c("NCODI", "nombre_hospital", "ccaa_cnh", "provincia_cnh",
      "finalidad_cnh", "dependencia_cnh"),
    names(map_df)
  )
  if (!("NCODI" %in% cols_enrich) || length(cols_enrich) < 2) {
    warning("El mapeo no tiene columnas de enriquecimiento válidas.", call. = FALSE)
    return(df)
  }

  map_join <- map_df[, cols_enrich, drop = FALSE] %>%
    dplyr::distinct(NCODI, .keep_all = TRUE)   # un registro por NCODI

  # No sobreescribir si ya existen estas columnas en el panel
  cols_nuevas <- setdiff(cols_enrich, c("NCODI", names(df)))

  if (length(cols_nuevas) == 0) {
    message("  Las columnas de hospital ya estaban en el panel.")
    return(df)
  }

  map_join <- map_join[, c("NCODI", cols_nuevas), drop = FALSE]

  df <- dplyr::left_join(df, map_join, by = "NCODI")

  n_con_nombre <- sum(!is.na(df$nombre_hospital) & df$nombre_hospital != "")
  n_sin_nombre <- nrow(df) - n_con_nombre
  message(sprintf(
    "  Filas con nombre_hospital resuelto: %d | Sin resolver: %d",
    n_con_nombre, n_sin_nombre
  ))
  if (n_sin_nombre > 0) {
    ncodi_sin <- unique(df$NCODI[is.na(df$nombre_hospital) | df$nombre_hospital == ""])
    message("  NCODI sin nombre (primeros 10): ",
            paste(head(ncodi_sin, 10), collapse = ", "))
  }

  df
}

df_final <- enrich_with_hospital_name(df_final, NCODI_HOSPITAL_MAP_PATH)

# Reordenar: anyo, NCODI, nombre_hospital al frente
front_cols <- intersect(c("anyo", "NCODI", "nombre_hospital", "ccaa_cnh", "provincia_cnh"),
                        names(df_final))
df_final <- df_final %>%
  select(all_of(front_cols), everything())

n_cols_clean <- ncol(df_final)   # actualizar tras el join

# ============================================================
# 8b. COMPROBACIONES DE CALIDAD DEL PANEL
# ============================================================

# Función para agrupar variables en familias por raíz de nombre.
# Se usan sólo para limitar comparaciones de colinealidad (evita n^2).
make_root <- function(x) {
  x <- gsub("_(total|hosp|hospital|CEP|cep)$", "", x, ignore.case = TRUE)
  x <- gsub("_[0-9]+$", "", x)
  x
}

# --- 8b.1 Varianza cero: identificar y eliminar ---
message("\nIdentificando columnas de varianza cero...")

numeric_cols_qc <- names(df_final)[vapply(df_final, is.numeric, logical(1))]
numeric_cols_qc <- setdiff(numeric_cols_qc, PROTECTED_VARS)

zero_var_list <- lapply(numeric_cols_qc, function(v) {
  vals    <- df_final[[v]]
  non_na  <- vals[!is.na(vals)]
  if (length(non_na) == 0) return(NULL)
  if (var(non_na) != 0) return(NULL)
  data.frame(
    variable   = v,
    n_nonNA    = length(non_na),
    unique_val = as.character(unique(non_na)[1]),
    is_all_zero = all(non_na == 0),
    stringsAsFactors = FALSE
  )
})

zero_var_df <- dplyr::bind_rows(Filter(Negate(is.null), zero_var_list))

if (nrow(zero_var_df) == 0) {
  zero_var_df <- data.frame(
    variable    = character(), n_nonNA = integer(),
    unique_val  = character(), is_all_zero = logical(),
    stringsAsFactors = FALSE
  )
}

message("  Columnas de varianza cero: ", nrow(zero_var_df))
write.csv(zero_var_df,
          file.path(INT_DIR, "panel_zero_variance.csv"),
          row.names = FALSE, na = "")
message("  Guardado en: panel_zero_variance.csv")

cols_zero_var <- zero_var_df$variable
if (length(cols_zero_var) > 0) {
  df_final <- df_final %>% select(-all_of(cols_zero_var))
  message("  Eliminadas: ", length(cols_zero_var))
}

# --- 8b.2 Colinealidad perfecta dentro de familias de nombres ---
message("\nBuscando colinealidad perfecta dentro de familias de nombres...")

numeric_cols_qc2 <- names(df_final)[vapply(df_final, is.numeric, logical(1))]
numeric_cols_qc2 <- setdiff(numeric_cols_qc2, PROTECTED_VARS)

root_groups <- split(numeric_cols_qc2, make_root(numeric_cols_qc2))
root_groups <- root_groups[lengths(root_groups) >= 2]

message("  Familias con >= 2 variables: ", length(root_groups))

collinear_list <- list()
for (grp_vars in root_groups) {
  sub_mat <- df_final[, grp_vars, drop = FALSE]
  n_g     <- length(grp_vars)
  for (ii in seq_len(n_g - 1)) {
    for (jj in (ii + 1):n_g) {
      va <- grp_vars[ii]; vb <- grp_vars[jj]
      x  <- sub_mat[[va]]; y  <- sub_mat[[vb]]
      mask <- !is.na(x) & !is.na(y)
      if (sum(mask) < 10) next
      r <- tryCatch(cor(x[mask], y[mask]), error = function(e) NA_real_)
      if (!is.na(r) && abs(r) >= 1 - 1e-10) {
        collinear_list[[length(collinear_list) + 1]] <- data.frame(
          var_a = va, var_b = vb,
          correlation = round(r, 8),
          n_obs = sum(mask),
          stringsAsFactors = FALSE
        )
      }
    }
  }
}

collinear_df <- if (length(collinear_list) > 0)
  dplyr::bind_rows(collinear_list)
else
  data.frame(var_a = character(), var_b = character(),
             correlation = numeric(), n_obs = integer(),
             stringsAsFactors = FALSE)

message("  Pares con colinealidad perfecta: ", nrow(collinear_df),
        " (solo log, NO se eliminan)")
write.csv(collinear_df,
          file.path(INT_DIR, "panel_perfect_collinearity.csv"),
          row.names = FALSE, na = "")
message("  Guardado en: panel_perfect_collinearity.csv")

n_cols_clean <- ncol(df_final)   # actualizar tras limpieza de varianza cero

# ============================================================
# 9. GUARDAR PANEL Y OUTPUTS
# ============================================================

save(df_final, file = DF_FINAL_RDATA_PATH)

write.table(
  df_final,
  file      = DF_FINAL_TXT_PATH,
  row.names = FALSE,
  sep       = ";",
  dec       = ",",
  na        = ""
)

n_con_nombre_final <- if ("nombre_hospital" %in% names(df_final))
  sum(!is.na(df_final$nombre_hospital)) else 0L

message("\nPanel guardado:")
message("  RData: ", DF_FINAL_RDATA_PATH)
message("  TXT:   ", DF_FINAL_TXT_PATH)
message("  Filas con nombre_hospital: ", n_con_nombre_final, " / ", nrow(df_final))

# ============================================================
# 10. GENERAR panel_audit_summary.txt
# ============================================================

audit_lines <- c(
  "============================================================",
  "PANEL AUDIT SUMMARY — SIAE",
  paste("Generado:", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
  "============================================================",
  "",
  paste("Hospitales únicos:", n_hospitales_total),
  paste("Rango de años:", anyo_min, "-", anyo_max),
  "",
  "Observaciones por año:",
  paste(capture.output(print(obs_por_anyo)), collapse = "\n"),
  "",
  paste("Variables totales (antes de limpiar all-NA):", n_cols_raw),
  paste("Variables eliminadas (all-NA, no protegidas):", length(cols_all_na)),
  paste("Variables eliminadas (varianza cero):", length(cols_zero_var)),
  paste("Variables totales (después de limpiar):", n_cols_clean),
  "",
  paste("Variables con > 50% NAs (excl. u1..u104, u900):", n_vars_high_na),
  paste("Conflictos de tipo resueltos:", nrow(type_conflict_df)),
  paste("Merges automáticos entre años (script 2):",
        ifelse(is.na(n_auto_merges), "N/A (ejecutar script 2 antes)", n_auto_merges)),
  ""
)

writeLines(audit_lines, PANEL_AUDIT_SUMMARY_PATH)
message("Auditoría guardada en: ", PANEL_AUDIT_SUMMARY_PATH)

# ============================================================
# 10. RESUMEN FINAL EN CONSOLA
# ============================================================

message("\n=== Script 3 completado ===")
message("Hospitales únicos: ", n_hospitales_total)
message("Años: ", anyo_min, "-", anyo_max)
message("Filas: ", nrow(df_final), " | Columnas: ", n_cols_clean)
message("Variables all-NA eliminadas: ", length(cols_all_na))
message("Variables varianza cero eliminadas: ", length(cols_zero_var))
message("Pares con colinealidad perfecta (log): ", nrow(collinear_df))
message("Conflictos de tipo: ", nrow(type_conflict_df))
