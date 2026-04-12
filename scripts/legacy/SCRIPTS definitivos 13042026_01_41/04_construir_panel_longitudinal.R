# ============================================================
# 04_construir_panel_longitudinal.R
# Apila los df_YYYY.csv en un panel longitudinal df_final.
# Detecta conflictos de tipo, limpia variables all-NA y
# genera auditoría completa del panel.
#
# Protección especial: u1..u104 y u900 nunca se eliminan.
# ============================================================

config_path <- file.path(
  dirname(normalizePath(sys.frame(1)$ofile)),
  "00_config.R"
)
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
# CONSTANTES Y AUXILIARES
# ============================================================

PROTECTED_VARS <- c(paste0("u", 1:104), "u900", "NCODI", "anyo")

is_protected <- function(name) name %in% PROTECTED_VARS

to_chr <- function(x) {
  x <- as.character(x)
  x <- trimws(x)
  x[x %in% c("", "NA")] <- NA_character_
  x
}

check_unique_key <- function(df, key = c("NCODI", "anyo"), label = "df") {
  dup_df <- df %>%
    dplyr::count(dplyr::across(dplyr::all_of(key)), name = "n") %>%
    dplyr::filter(n > 1)

  if (nrow(dup_df) > 0) {
    message("\nPrimeros duplicados detectados en ", label, ":")
    print(utils::head(dup_df, 20))
    stop(
      "La clave (", paste(key, collapse = ", "), ") NO es única en ", label,
      ": ", sum(dup_df$n - 1), " filas duplicadas.",
      call. = FALSE
    )
  }
  invisible(TRUE)
}

# ============================================================
# 1. LEER df_YYYY.csv
# ============================================================

year_files <- list.files(
  path       = LEGACY_BASE_DIR,
  pattern    = "^df_[0-9]{4}\\.csv$",
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

  tmp$NCODI <- to_chr(tmp$NCODI)
  tmp$anyo  <- as.integer(years_avail[idx])
  tmp       <- tmp %>% select(anyo, NCODI, everything())

  check_unique_key(tmp, c("NCODI", "anyo"), basename(year_files[idx]))

  list_df[[idx]] <- tmp
}

# ============================================================
# 2. DETECTAR CONFLICTOS DE TIPO ENTRE AÑOS
# ============================================================

message("\nDetectando conflictos de tipo entre años...")

infer_type <- function(x) {
  x2 <- trimws(x)
  x2[x2 == ""] <- NA
  if (all(is.na(x2))) return("all_na")
  suppressWarnings(xn <- as.numeric(x2))
  if (all(is.na(x2) == is.na(xn))) return("numeric")
  return("character")
}

type_map <- list()

for (idx in seq_along(list_df)) {
  yy  <- years_avail[idx]
  tmp <- list_df[[idx]]
  for (v in names(tmp)) {
    if (!(v %in% names(type_map))) type_map[[v]] <- list()
    type_map[[v]][[as.character(yy)]] <- infer_type(tmp[[v]])
  }
}

resolution_rows <- list()

for (v in names(type_map)) {
  types_by_year <- unlist(type_map[[v]])
  real_types    <- types_by_year[types_by_year != "all_na"]
  if (length(unique(real_types)) <= 1) next

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
  data.frame(
    variable=character(), year=integer(),
    type_detected=character(), resolution=character(),
    stringsAsFactors=FALSE
  )
}

message("Conflictos de tipo detectados: ", nrow(type_conflict_df),
        " (en ", length(unique(type_conflict_df$variable)), " variables)")

write.csv(type_conflict_df, PANEL_TYPE_CONFLICTS_PATH, row.names = FALSE, na = "")

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

check_unique_key(df_final, c("NCODI", "anyo"), "panel apilado")
message("Unicidad de (NCODI, anyo): OK")

# ============================================================
# 5. CONVERSIÓN DE TIPOS
# ============================================================

message("\nConvirtiendo tipos de columnas...")

cols_to_convert <- setdiff(names(df_final), c("NCODI"))

df_final[cols_to_convert] <- lapply(cols_to_convert, function(v) {
  x <- df_final[[v]]
  x <- trimws(x)
  x[x == ""] <- NA

  if (v %in% type_conflict_df$variable) return(x)
  if (all(is.na(x))) return(x)

  suppressWarnings(x_num <- as.numeric(x))
  if (all(is.na(x) == is.na(x_num))) return(x_num)

  x
})

# ============================================================
# 6. ELIMINAR VARIABLES ALL-NA
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

vars_check_na <- setdiff(names(df_final), c(PROTECTED_VARS))
pct_na_by_var <- vapply(df_final[vars_check_na], function(x) {
  if (is.character(x)) mean(is.na(x) | trimws(x) == "")
  else                  mean(is.na(x))
}, numeric(1))

n_vars_high_na <- sum(pct_na_by_var > 0.50)

n_auto_merges <- if (exists("N_AUTO_MERGES_APPLIED")) N_AUTO_MERGES_APPLIED else NA_integer_

# ============================================================
# 8. ENRIQUECER CON nombre_hospital
# ============================================================

message("\nEnriqueciendo panel con nombre_hospital desde mapeo CNH...")

enrich_with_hospital_name <- function(df, map_path) {
  if (!file.exists(map_path)) {
    warning(
      "Mapeo NCODI-hospital no encontrado: ", map_path,
      "\nEjecuta 01_exportar_mapeo_cnh.R para generarlo.",
      call. = FALSE
    )
    return(df)
  }

  map_df <- tryCatch(
    readr::read_csv(
      map_path,
      col_types = readr::cols(.default = "c"),
      show_col_types = FALSE
    ),
    error = function(e) {
      warning("No se pudo leer el mapeo: ", conditionMessage(e), call. = FALSE)
      NULL
    }
  )
  if (is.null(map_df)) return(df)

  map_df$NCODI <- to_chr(map_df$NCODI)
  df$NCODI     <- to_chr(df$NCODI)

  cols_enrich <- intersect(
    c("NCODI", "nombre_hospital", "ccaa_cnh", "provincia_cnh",
      "finalidad_cnh", "dependencia_cnh"),
    names(map_df)
  )

  if (!("NCODI" %in% cols_enrich) || length(cols_enrich) < 2) {
    warning("El mapeo no tiene columnas de enriquecimiento válidas.", call. = FALSE)
    return(df)
  }

  dup_map <- map_df %>%
    dplyr::count(NCODI, name = "n") %>%
    dplyr::filter(!is.na(NCODI), n > 1)

  if (nrow(dup_map) > 0) {
    message("  NCODI duplicados en el mapa CNH: ", nrow(dup_map))
    print(utils::head(dup_map, 20))
  }

  map_join <- map_df[, cols_enrich, drop = FALSE] %>%
    dplyr::filter(!is.na(NCODI)) %>%
    dplyr::distinct(NCODI, .keep_all = TRUE)

  cols_nuevas <- setdiff(cols_enrich, c("NCODI", names(df)))

  if (length(cols_nuevas) == 0) {
    message("  Las columnas de hospital ya estaban en el panel.")
    return(df)
  }

  map_join <- map_join[, c("NCODI", cols_nuevas), drop = FALSE]

  n_before <- nrow(df)
  df2 <- dplyr::left_join(df, map_join, by = "NCODI")
  n_after  <- nrow(df2)

  message("  Filas antes del join: ", n_before)
  message("  Filas después del join: ", n_after)

  if (n_after != n_before) {
    stop(
      "El enriquecimiento con nombre_hospital ha cambiado el número de filas del panel. ",
      "Revisa duplicados en el mapa CNH.",
      call. = FALSE
    )
  }

  # volver a comprobar clave tras el join
  check_unique_key(df2, c("NCODI", "anyo"), "panel tras enrich_with_hospital_name")

  if ("nombre_hospital" %in% names(df2)) {
    n_con_nombre <- sum(!is.na(df2$nombre_hospital) & df2$nombre_hospital != "")
    n_sin_nombre <- nrow(df2) - n_con_nombre
    message(sprintf(
      "  Filas con nombre_hospital resuelto: %d | Sin resolver: %d",
      n_con_nombre, n_sin_nombre
    ))
    if (n_sin_nombre > 0) {
      ncodi_sin <- unique(df2$NCODI[is.na(df2$nombre_hospital) | df2$nombre_hospital == ""])
      message("  NCODI sin nombre (primeros 10): ",
              paste(head(ncodi_sin, 10), collapse = ", "))
    }
  }

  df2
}

df_final <- enrich_with_hospital_name(df_final, NCODI_HOSPITAL_MAP_PATH)

front_cols <- intersect(
  c("anyo", "NCODI", "nombre_hospital", "ccaa_cnh", "provincia_cnh"),
  names(df_final)
)

df_final <- df_final %>%
  select(all_of(front_cols), everything())

n_cols_clean <- ncol(df_final)

# ============================================================
# 8b. COMPROBACIONES DE CALIDAD DEL PANEL
# ============================================================

make_root <- function(x) {
  x <- gsub("_(total|hosp|hospital|CEP|cep)$", "", x, ignore.case = TRUE)
  x <- gsub("_[0-9]+$", "", x)
  x
}

message("\nIdentificando columnas de varianza cero...")

numeric_cols_qc <- names(df_final)[vapply(df_final, is.numeric, logical(1))]
numeric_cols_qc <- setdiff(numeric_cols_qc, PROTECTED_VARS)

zero_var_list <- lapply(numeric_cols_qc, function(v) {
  vals   <- df_final[[v]]
  non_na <- vals[!is.na(vals)]
  if (length(non_na) == 0) return(NULL)
  if (var(non_na) != 0) return(NULL)
  data.frame(
    variable    = v,
    n_nonNA     = length(non_na),
    unique_val  = as.character(unique(non_na)[1]),
    is_all_zero = all(non_na == 0),
    stringsAsFactors = FALSE
  )
})

zero_var_df <- dplyr::bind_rows(Filter(Negate(is.null), zero_var_list))

if (nrow(zero_var_df) == 0) {
  zero_var_df <- data.frame(
    variable = character(), n_nonNA = integer(),
    unique_val = character(), is_all_zero = logical(),
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

message("\nBuscando colinealidad perfecta dentro de familias de nombres...")

numeric_cols_qc2 <- names(df_final)[vapply(df_final, is.numeric, logical(1))]
numeric_cols_qc2 <- setdiff(numeric_cols_qc2, PROTECTED_VARS)

root_groups <- split(numeric_cols_qc2, make_root(numeric_cols_qc2))
root_groups <- root_groups[lengths(root_groups) >= 2]

message("  Familias con >= 2 variables: ", length(root_groups))

collinear_list <- list()
for (grp_vars in root_groups) {
  sub_mat <- df_final[, grp_vars, drop = FALSE]
  n_g <- length(grp_vars)
  for (ii in seq_len(n_g - 1)) {
    for (jj in (ii + 1):n_g) {
      va <- grp_vars[ii]
      vb <- grp_vars[jj]
      x  <- sub_mat[[va]]
      y  <- sub_mat[[vb]]
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

collinear_df <- if (length(collinear_list) > 0) {
  dplyr::bind_rows(collinear_list)
} else {
  data.frame(
    var_a = character(), var_b = character(),
    correlation = numeric(), n_obs = integer(),
    stringsAsFactors = FALSE
  )
}

message("  Pares con colinealidad perfecta: ", nrow(collinear_df),
        " (solo log, NO se eliminan)")
write.csv(collinear_df,
          file.path(INT_DIR, "panel_perfect_collinearity.csv"),
          row.names = FALSE, na = "")
message("  Guardado en: panel_perfect_collinearity.csv")

n_cols_clean <- ncol(df_final)
# ============================================================
# 8c. BLINDAJE FINAL DE CLAVE (NCODI, anyo)
#     Si hay duplicados, guardar detalle y colapsar por clave
#     usando el primer valor no-NA de cada columna.
# ============================================================

message("\nBlindaje final de clave (NCODI, anyo)...")

collapse_first_non_na <- function(x) {
  idx <- which(!is.na(x) & !(is.character(x) & trimws(x) == ""))
  if (length(idx) == 0) {
    if (is.character(x)) return(NA_character_)
    if (is.integer(x))   return(NA_integer_)
    if (is.numeric(x))   return(NA_real_)
    return(x[1])
  }
  x[idx[1]]
}

# Normalizar clave una vez más
df_final$NCODI <- trimws(as.character(df_final$NCODI))
df_final$NCODI[df_final$NCODI %in% c("", "NA")] <- NA_character_
df_final$anyo  <- suppressWarnings(as.integer(df_final$anyo))

dup_keys_final <- df_final %>%
  dplyr::count(NCODI, anyo, name = "n") %>%
  dplyr::filter(!is.na(NCODI), !is.na(anyo), n > 1)

message("Duplicados detectados antes de guardar: ", nrow(dup_keys_final))

if (nrow(dup_keys_final) > 0) {
  print(utils::head(dup_keys_final, 20))

  dup_rows_full <- df_final %>%
    dplyr::inner_join(
      dup_keys_final %>% dplyr::select(NCODI, anyo),
      by = c("NCODI", "anyo")
    ) %>%
    dplyr::arrange(NCODI, anyo)

  dup_path <- file.path(INT_DIR, "df_final_duplicados_antes_de_guardar.csv")
  write.csv(dup_rows_full, dup_path, row.names = FALSE, na = "")
  message("Detalle de duplicados guardado en: ", dup_path)

  # Colapsar a una fila por (NCODI, anyo)
  df_final <- df_final %>%
    dplyr::group_by(NCODI, anyo) %>%
    dplyr::summarise(
      dplyr::across(dplyr::everything(), collapse_first_non_na),
      .groups = "drop"
    ) %>%
    dplyr::arrange(anyo, NCODI)

  message("Filas tras colapsar duplicados: ", nrow(df_final))
}

# Comprobación definitiva
dup_keys_after <- df_final %>%
  dplyr::count(NCODI, anyo, name = "n") %>%
  dplyr::filter(!is.na(NCODI), !is.na(anyo), n > 1)

if (nrow(dup_keys_after) > 0) {
  print(utils::head(dup_keys_after, 20))
  stop("Persisten duplicados por (NCODI, anyo) tras el colapsado.", call. = FALSE)
} else {
  message("Clave (NCODI, anyo) verificada: OK")
}
# ============================================================
# 9.  GUARDAR PANEL Y OUTPUTS
# ============================================================

#check_unique_key(df_final, c("NCODI", "anyo"), "df_final final")

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
# 11. RESUMEN FINAL EN CONSOLA
# ============================================================

message("\n=== Script 04 completado ===")
message("Hospitales únicos: ", n_hospitales_total)
message("Años: ", anyo_min, "-", anyo_max)
message("Filas: ", nrow(df_final), " | Columnas: ", n_cols_clean)
message("Variables all-NA eliminadas: ", length(cols_all_na))
message("Variables varianza cero eliminadas: ", length(cols_zero_var))
message("Pares con colinealidad perfecta (log): ", nrow(collinear_df))
message("Conflictos de tipo: ", nrow(type_conflict_df))
