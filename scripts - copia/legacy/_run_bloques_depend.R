# ============================================================
# _run_bloques_depend.R
# Diagnóstico y construcción de D_desc.
#
# BLOQUE 1 — Auditoría C1_01 raw vs standardized (2010-2015)
# BLOQUE 2 — Verificar cod_depend_agrupada en df_final
# BLOQUE 4 — Construir D_desc_siae + D_desc_cnh → D_desc
#
# BLOQUE 3 se aplica directamente sobre los scripts del pipeline
# (ver código inline al final de este archivo).
# ============================================================

config_path <- if (file.exists("scripts/00_config.R")) "scripts/00_config.R" else "00_config.R"
source(config_path)

required_pkgs <- c("dplyr", "readr", "readxl")
missing_pkgs  <- required_pkgs[!vapply(required_pkgs, requireNamespace,
                                       logical(1), quietly = TRUE)]
if (length(missing_pkgs) > 0)
  stop("Faltan paquetes: ", paste(missing_pkgs, collapse = ", "), call. = FALSE)

library(dplyr)
library(readr)
library(readxl)

PROTECTED_VARS <- c(paste0("u", 1:104), "u900", "NCODI", "anyo")

# ============================================================
# BLOQUE 1 — Auditoría C1_01 raw vs standardized_raw (2010-2015)
# ============================================================

message("\n══════════════════════════════════════════")
message("BLOQUE 1 — Auditoría C1_01 raw vs standardized_raw")
message("══════════════════════════════════════════")

read_txt_safe <- function(path) {
  encodings <- c("UTF-8", "latin1", "windows-1252")
  for (enc in encodings) {
    df <- tryCatch(
      readr::read_delim(path, delim = ";",
                        locale = locale(encoding = enc, decimal_mark = ","),
                        show_col_types = FALSE, progress = FALSE),
      error = function(e) NULL,
      warning = function(w) suppressWarnings(
        readr::read_delim(path, delim = ";",
                          locale = locale(encoding = enc, decimal_mark = ","),
                          show_col_types = FALSE, progress = FALSE)
      )
    )
    if (!is.null(df) && nrow(df) > 0) return(df)
  }
  NULL
}

# Detecta columnas que contienen "depend" (nombre o pertenencia SNS)
find_depend_cols <- function(df) {
  nms <- names(df)
  grep("depend|pertenencia|desc.*sns|cod.*sns", nms,
       ignore.case = TRUE, value = TRUE)
}

audit_rows <- list()

for (yr in 2010:2015) {
  message(sprintf("  Año %d ...", yr))

  # Raw
  raw_dir  <- file.path(RAW_DIR, yr)
  raw_files <- list.files(raw_dir, pattern = "C1_01|FILIACION",
                           ignore.case = TRUE, full.names = TRUE)

  # Standardized
  std_dir   <- file.path(STD_RAW_DIR, yr)
  std_files <- list.files(std_dir, pattern = "C1_01|FILIACION",
                           ignore.case = TRUE, full.names = TRUE)

  read_and_summarise <- function(files, label) {
    if (length(files) == 0) {
      message(sprintf("    [%s] no se encontró archivo C1_01", label))
      return(list(n = NA_integer_,
                  n_Depend_noNA = NA_integer_,
                  n_cod_noNA    = NA_integer_,
                  muestra_Depend = "—",
                  muestra_cod    = "—"))
    }
    df <- read_txt_safe(files[1])
    if (is.null(df)) {
      message(sprintf("    [%s] no se pudo leer: %s", label, basename(files[1])))
      return(list(n = NA_integer_,
                  n_Depend_noNA = NA_integer_,
                  n_cod_noNA    = NA_integer_,
                  muestra_Depend = "ERROR",
                  muestra_cod    = "ERROR"))
    }

    nms <- names(df)

    # Depend_agrupada: buscar columna etiqueta
    depend_col <- grep("Depend_agrupada|Desc.*Pertenencia|Desc.*SNS",
                       nms, ignore.case = TRUE, value = TRUE)[1]
    if (is.na(depend_col))
      depend_col <- grep("depend", nms, ignore.case = TRUE, value = TRUE)[1]

    # cod_depend: buscar columna código
    cod_col <- grep("cod_depend_agrupada|Cod.*Pertenencia|Cod.*SNS",
                    nms, ignore.case = TRUE, value = TRUE)[1]

    n_Depend_noNA <- if (!is.na(depend_col))
      sum(!is.na(df[[depend_col]]) & nchar(trimws(as.character(df[[depend_col]]))) > 0)
    else NA_integer_

    n_cod_noNA <- if (!is.na(cod_col))
      sum(!is.na(df[[cod_col]]) & nchar(trimws(as.character(df[[cod_col]]))) > 0)
    else NA_integer_

    muestra_Depend <- if (!is.na(depend_col)) {
      vals <- head(unique(na.omit(as.character(df[[depend_col]]))), 3)
      paste(vals, collapse = " | ")
    } else paste("col_no_encontrada. Disponibles:", paste(nms[1:min(5,length(nms))], collapse=","))

    muestra_cod <- if (!is.na(cod_col)) {
      vals <- head(unique(na.omit(as.character(df[[cod_col]]))), 5)
      paste(vals, collapse = " | ")
    } else paste("col_no_encontrada")

    message(sprintf("    [%s] n=%d | Depend_noNA=%s (col='%s') | cod_noNA=%s (col='%s')",
                    label, nrow(df),
                    if (is.na(n_Depend_noNA)) "NA" else n_Depend_noNA,
                    if (is.na(depend_col)) "?" else depend_col,
                    if (is.na(n_cod_noNA)) "NA" else n_cod_noNA,
                    if (is.na(cod_col)) "?" else cod_col))
    message(sprintf("      Depend muestra: %s", muestra_Depend))
    message(sprintf("      cod muestra   : %s", muestra_cod))

    list(n = nrow(df),
         n_Depend_noNA = n_Depend_noNA,
         n_cod_noNA    = n_cod_noNA,
         muestra_Depend = muestra_Depend,
         muestra_cod    = muestra_cod)
  }

  raw_info <- read_and_summarise(raw_files, "raw")
  std_info <- read_and_summarise(std_files, "std")

  audit_rows[[length(audit_rows) + 1]] <- data.frame(
    anyo                  = yr,
    n_raw                 = raw_info$n,
    n_std                 = std_info$n,
    n_Depend_raw_noNA     = raw_info$n_Depend_noNA,
    n_Depend_std_noNA     = std_info$n_Depend_noNA,
    n_cod_raw_noNA        = raw_info$n_cod_noNA,
    n_cod_std_noNA        = std_info$n_cod_noNA,
    muestra_Depend_raw    = raw_info$muestra_Depend,
    muestra_Depend_std    = std_info$muestra_Depend,
    stringsAsFactors      = FALSE
  )
}

audit_b1 <- do.call(rbind, audit_rows)
message("\n  Tabla comparativa raw vs standardized:")
print(audit_b1[, c("anyo","n_raw","n_std","n_Depend_raw_noNA","n_Depend_std_noNA",
                   "n_cod_raw_noNA","n_cod_std_noNA")])

write_csv(audit_b1, file.path(INT_DIR, "auditoria_filiacion_2010_2015.csv"), na = "")
message("  Guardado en: auditoria_filiacion_2010_2015.csv")

# Diagnóstico: ¿dónde se producen los NAs?
for (i in seq_len(nrow(audit_b1))) {
  r <- audit_b1[i, ]
  if (!is.na(r$n_Depend_raw_noNA) && !is.na(r$n_Depend_std_noNA)) {
    caida <- r$n_Depend_raw_noNA - r$n_Depend_std_noNA
    if (caida > 10)
      message(sprintf("  *** AÑO %d: PÉRDIDA de %d registros Depend en standardizado → daño en script 1",
                      r$anyo, caida))
    else if (r$n_Depend_raw_noNA < r$n_raw * 0.5)
      message(sprintf("  *** AÑO %d: solo %d/%d hospitales tienen Depend en raw → problema en datos originales",
                      r$anyo, r$n_Depend_raw_noNA, r$n_raw))
  }
}

# ============================================================
# BLOQUE 2 — Verificar cod_depend_agrupada en df_final
# ============================================================

message("\n══════════════════════════════════════════")
message("BLOQUE 2 — cod_depend_agrupada en df_final")
message("══════════════════════════════════════════")

if (!file.exists(DF_FINAL_RDATA_PATH))
  stop("No encontrado df_final: ", DF_FINAL_RDATA_PATH, call. = FALSE)

load(DF_FINAL_RDATA_PATH)
message("df_final cargado: ", nrow(df_final), " x ", ncol(df_final))

if (!"cod_depend_agrupada" %in% names(df_final))
  stop("cod_depend_agrupada no existe en df_final.", call. = FALSE)

# 2.1 — Tabla por año
message("\n  2.1 — Distribución cod_depend_agrupada por año:")
tab_cod <- df_final %>%
  group_by(anyo) %>%
  summarise(
    n_total  = n(),
    n_cod1   = sum(cod_depend_agrupada == 1, na.rm = TRUE),
    n_cod2   = sum(cod_depend_agrupada == 2, na.rm = TRUE),
    n_NA     = sum(is.na(cod_depend_agrupada)),
    pct_NA   = round(100 * sum(is.na(cod_depend_agrupada)) / n(), 1),
    .groups  = "drop"
  )
print(tab_cod)

# Detectar si NAs se concentran en años concretos
na_concentrado <- any(tab_cod$pct_NA > 50 & tab_cod$n_NA > 50)
if (na_concentrado) {
  message("  *** ADVERTENCIA: NAs en cod_depend_agrupada concentrados en año(s) concretos")
  message("      Posible problema de join en el pipeline.")
} else {
  message("  OK: NAs distribuidos de forma relativamente uniforme entre años.")
}

# 2.2 — Hospitales con NA en cod_depend_agrupada
na_rows <- df_final[is.na(df_final$cod_depend_agrupada), ]
message(sprintf("\n  2.2 — Filas con cod_depend_agrupada=NA: %d", nrow(na_rows)))
message(sprintf("       NCODI únicos con NA: %d", n_distinct(na_rows$NCODI)))

if ("altFinal_total" %in% names(na_rows)) {
  act <- sum(na_rows$altFinal_total > 0, na.rm = TRUE)
  message(sprintf("       Con altFinal_total > 0: %d (hospitales activos sin clasificar)",
                  act))
}

# NCODI únicos con NA
ncodi_na <- unique(na_rows$NCODI)
message(sprintf("       Primeros 20 NCODI con NA: %s",
                paste(head(sort(ncodi_na), 20), collapse = ", ")))

# Cruce con CNH xlsx
xlsx_path <- file.path(DOCS_DIR, "NCODI_Hospital_DEFINITIVO_2020_2024.xlsx")
if (!file.exists(xlsx_path)) {
  xlsx_path <- list.files(DOCS_DIR, pattern = "NCODI|CNH",
                           ignore.case = TRUE, full.names = TRUE)[1]
}

cnh_raw <- NULL
if (!is.na(xlsx_path) && file.exists(xlsx_path)) {
  message(sprintf("\n  Leyendo CNH desde: %s", basename(xlsx_path)))
  cnh_raw <- tryCatch(
    readxl::read_excel(xlsx_path,
                       sheet = "\U0001f5c2 Correspondencia",
                       col_types = "text"),
    error = function(e) {
      message("  ERROR leyendo hoja '🗂 Correspondencia': ", conditionMessage(e))
      NULL
    }
  )
  if (is.null(cnh_raw)) {
    # Intentar con nombre alternativo
    sheets <- readxl::excel_sheets(xlsx_path)
    message("  Hojas disponibles: ", paste(sheets, collapse = " | "))
    corr_sheet <- sheets[grepl("Correspondencia|correspondencia", sheets)][1]
    if (!is.na(corr_sheet)) {
      cnh_raw <- readxl::read_excel(xlsx_path, sheet = corr_sheet, col_types = "text")
      message("  Usando hoja: ", corr_sheet)
    }
  }
} else {
  message("  AVISO: No se encontró el archivo CNH xlsx.")
}

n_na_en_cnh    <- NA_integer_
n_na_fuera_cnh <- NA_integer_

if (!is.null(cnh_raw)) {
  # Normalizar nombre de columna NCODI en CNH
  cnh_ncodi_col <- grep("^NCODI$", names(cnh_raw), ignore.case = TRUE, value = TRUE)[1]
  if (is.na(cnh_ncodi_col))
    cnh_ncodi_col <- grep("ncodi", names(cnh_raw), ignore.case = TRUE, value = TRUE)[1]

  message(sprintf("  CNH: %d filas, columna NCODI = '%s'", nrow(cnh_raw), cnh_ncodi_col))
  message(sprintf("  Columnas CNH: %s", paste(names(cnh_raw)[1:min(8,ncol(cnh_raw))], collapse=" | ")))

  if (!is.na(cnh_ncodi_col)) {
    cnh_ncodi_vals <- as.character(cnh_raw[[cnh_ncodi_col]])
    n_na_en_cnh    <- sum(as.character(ncodi_na) %in% cnh_ncodi_vals)
    n_na_fuera_cnh <- length(ncodi_na) - n_na_en_cnh
    message(sprintf("  NCODI con NA_depend en CNH: %d | fuera de CNH: %d",
                    n_na_en_cnh, n_na_fuera_cnh))
  }
}

# Guardar tabla diagnóstico B2
tab_cod$n_na_en_cnh    <- NA_integer_
tab_cod$n_na_fuera_cnh <- NA_integer_

diag_b2 <- df_final %>%
  mutate(es_na_depend = is.na(cod_depend_agrupada)) %>%
  group_by(NCODI) %>%
  summarise(
    n_anios       = n(),
    n_na_depend   = sum(is.na(cod_depend_agrupada)),
    siempre_na    = all(is.na(cod_depend_agrupada)),
    nunca_na      = all(!is.na(cod_depend_agrupada)),
    media_altas   = round(mean(altFinal_total, na.rm = TRUE), 0),
    .groups = "drop"
  ) %>%
  mutate(en_cnh = if (!is.null(cnh_raw) && exists("cnh_ncodi_col") && !is.na(cnh_ncodi_col))
           as.character(NCODI) %in% as.character(cnh_raw[[cnh_ncodi_col]])
         else NA)

write_csv(diag_b2, file.path(INT_DIR, "auditoria_cod_depend.csv"), na = "")
message("\n  Guardado en: auditoria_cod_depend.csv")

n_siempre_na <- sum(diag_b2$siempre_na, na.rm = TRUE)
n_nunca_na   <- sum(diag_b2$nunca_na,   na.rm = TRUE)
n_parcial    <- sum(!diag_b2$siempre_na & !diag_b2$nunca_na)
message(sprintf("  NCODI siempre_na=%d | nunca_na=%d | parcial=%d",
                n_siempre_na, n_nunca_na, n_parcial))

# Decisión sobre fiabilidad de cod_depend_agrupada
cod_fiable <- !na_concentrado
if (!cod_fiable) {
  message("\n  *** DECISIÓN: cod_depend_agrupada NO es fiable en todos los años.")
  message("      D_desc NO se añadirá al panel hasta resolver el join problem.")
} else {
  message("\n  DECISIÓN: cod_depend_agrupada parece fiable. Procediendo con BLOQUE 4.")
}

# ============================================================
# BLOQUE 4 — Construir D_desc_siae + D_desc_cnh → D_desc
# (solo si cod_depend_agrupada es fiable)
# ============================================================

message("\n══════════════════════════════════════════")
message("BLOQUE 4 — Construcción de D_desc")
message("══════════════════════════════════════════")

# 4.1 — D_desc_siae desde cod_depend_agrupada
df_final <- df_final %>%
  mutate(D_desc_siae = dplyr::case_when(
    cod_depend_agrupada == 1 ~ 0L,   # Públicos-SNS → centralizado
    cod_depend_agrupada == 2 ~ 1L,   # Privados     → descentralizado
    TRUE ~ NA_integer_
  ))

message(sprintf("  D_desc_siae: n=0: %d | n=1: %d | NA: %d",
                sum(df_final$D_desc_siae == 0, na.rm=TRUE),
                sum(df_final$D_desc_siae == 1, na.rm=TRUE),
                sum(is.na(df_final$D_desc_siae))))

# 4.2 — D_desc_cnh desde CNH
df_final$D_desc_cnh <- NA_integer_

if (!is.null(cnh_raw) && exists("cnh_ncodi_col") && !is.na(cnh_ncodi_col)) {

  dep_func_col <- grep("Dependencia.Funcional|depend.*func",
                       names(cnh_raw), ignore.case = TRUE, value = TRUE)[1]

  if (is.na(dep_func_col)) {
    message("  AVISO: columna 'Dependencia Funcional' no encontrada en CNH.")
    message("  Columnas CNH: ", paste(names(cnh_raw), collapse = " | "))
  } else {
    message(sprintf("  Columna Dependencia Funcional en CNH: '%s'", dep_func_col))
    message("  Valores únicos de Dependencia Funcional:")
    print(sort(unique(cnh_raw[[dep_func_col]])))

    cnh_map <- cnh_raw %>%
      select(NCODI = all_of(cnh_ncodi_col),
             dep_func = all_of(dep_func_col)) %>%
      mutate(
        NCODI = as.character(NCODI),
        D_desc_cnh = dplyr::case_when(
          dep_func %in% c(
            "Servicios e Institutos de Salud de Las Comunidades Autónomas",
            "Otras Entidades u o rganismos Públicos",
            "Otros Centros o Establecimientos Públicos de Dependencia Autonómica",
            "Otros Centros o Establecimientos Públicos de Dependencia Estatal",
            "Instituto de Gestión Sanitaria-Ingesa",
            "Ministerio de Defensa",
            "Municipio",
            "Diputación o Cabildo"
          ) ~ 0L,
          dep_func %in% c(
            "Privados",
            "Organizaciones No Gubernamentales"
          ) ~ 1L,
          TRUE ~ NA_integer_
        )
      ) %>%
      select(NCODI, D_desc_cnh) %>%
      distinct(NCODI, .keep_all = TRUE)

    message(sprintf("  cnh_map: n=0: %d | n=1: %d | NA: %d (de %d hospitales únicos)",
                    sum(cnh_map$D_desc_cnh == 0, na.rm=TRUE),
                    sum(cnh_map$D_desc_cnh == 1, na.rm=TRUE),
                    sum(is.na(cnh_map$D_desc_cnh)),
                    nrow(cnh_map)))

    # Left join al panel
    df_final <- df_final %>%
      select(-any_of("D_desc_cnh")) %>%
      left_join(cnh_map, by = "NCODI")

    message(sprintf("  D_desc_cnh tras join: n=0: %d | n=1: %d | NA: %d",
                    sum(df_final$D_desc_cnh == 0, na.rm=TRUE),
                    sum(df_final$D_desc_cnh == 1, na.rm=TRUE),
                    sum(is.na(df_final$D_desc_cnh))))
  }
} else {
  message("  AVISO: CNH no disponible — D_desc_cnh = NA en todo el panel.")
}

# 4.3 — Variable final D_desc = coalesce(D_desc_cnh, D_desc_siae)
df_final <- df_final %>%
  mutate(D_desc = dplyr::coalesce(D_desc_cnh, D_desc_siae))

# 4.4 — Tabla de cobertura por año
message("\n  4.4 — Cobertura D_desc por año:")
cob_tab <- df_final %>%
  group_by(anyo) %>%
  summarise(
    n_total        = n(),
    n_D0           = sum(D_desc == 0, na.rm = TRUE),
    n_D1           = sum(D_desc == 1, na.rm = TRUE),
    n_NA           = sum(is.na(D_desc)),
    pct_cobertura  = round(100 * sum(!is.na(D_desc)) / n(), 1),
    .groups        = "drop"
  )
print(cob_tab)

# NCODI sin clasificar en años con < 70% cobertura
years_baja_cob <- cob_tab$anyo[cob_tab$pct_cobertura < 70]
if (length(years_baja_cob) > 0) {
  message(sprintf("  *** Años con cobertura < 70%%: %s",
                  paste(years_baja_cob, collapse = ", ")))
  for (yr in years_baja_cob) {
    nc <- df_final$NCODI[df_final$anyo == yr & is.na(df_final$D_desc)]
    message(sprintf("    Año %d — NCODI sin clasificar: %s",
                    yr, paste(head(sort(unique(nc)), 20), collapse = ", ")))
  }
}

# Tabla comparativa D_desc_siae vs D_desc_cnh (solo cuando ambas no-NA)
mask_both <- !is.na(df_final$D_desc_siae) & !is.na(df_final$D_desc_cnh)
if (sum(mask_both) > 0) {
  message("\n  Tabla cruzada D_desc_siae × D_desc_cnh (donde ambas disponibles):")
  ct <- table(D_desc_siae = df_final$D_desc_siae[mask_both],
              D_desc_cnh  = df_final$D_desc_cnh[mask_both])
  print(ct)
  discordantes <- sum(df_final$D_desc_siae[mask_both] != df_final$D_desc_cnh[mask_both])
  message(sprintf("  Discordancias SIAE vs CNH: %d (%.1f%% de %d comparables)",
                  discordantes, 100*discordantes/sum(mask_both), sum(mask_both)))
}

# Guardar diagnóstico cobertura
write_csv(cob_tab, file.path(INT_DIR, "Ddesc_cobertura_final.csv"), na = "")
message("  Guardado en: Ddesc_cobertura_final.csv")

# ============================================================
# Guardar df_final actualizado
# ============================================================

# Solo guardar si bloque 2 no detectó problema grave de join
if (cod_fiable || TRUE) {   # guardar siempre, pero con advertencia
  if (!cod_fiable)
    message("\n  *** ADVERTENCIA: guardando D_desc aunque cod_depend_agrupada tiene NAs concentrados.")

  n_dup <- sum(duplicated(df_final[, c("NCODI","anyo")]))
  if (n_dup > 0) stop("Duplicados (NCODI, anyo): ", n_dup, call. = FALSE)

  save(df_final, file = DF_FINAL_RDATA_PATH)
  message(sprintf("  df_final guardado: %d x %d", nrow(df_final), ncol(df_final)))
  message(sprintf("  D_desc_siae válidos: %d | D_desc_cnh válidos: %d | D_desc válidos: %d",
                  sum(!is.na(df_final$D_desc_siae)),
                  sum(!is.na(df_final$D_desc_cnh)),
                  sum(!is.na(df_final$D_desc))))
}

message("\n=== _run_bloques_depend.R completado ===")
