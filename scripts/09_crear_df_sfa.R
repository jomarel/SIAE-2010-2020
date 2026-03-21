# ============================================================
# 12_crear_df_sfa.R
# Produce df_sfa: dataset reducido y autocontenido para sfaR.
#
# Output:
#   data_intermediate/df_sfa.RData
#   data_intermediate/df_sfa.csv
# ============================================================

config_path <- if (file.exists("scripts/00_config.R")) "scripts/00_config.R" else "00_config.R"
source(config_path)

required_pkgs <- c("dplyr", "readr")
missing_pkgs  <- required_pkgs[!vapply(required_pkgs, requireNamespace,
                                       logical(1), quietly = TRUE)]
if (length(missing_pkgs) > 0)
  stop("Faltan paquetes: ", paste(missing_pkgs, collapse = ", "), call. = FALSE)

library(dplyr)
library(readr)

PROTECTED_VARS <- c(paste0("u", 1:104), "u900", "NCODI", "anyo")

message("\n=== Script 12: Creación de df_sfa ===")

if (!file.exists(DF_FINAL_RDATA_PATH))
  stop("No encontrado df_final: ", DF_FINAL_RDATA_PATH, call. = FALSE)

load(DF_FINAL_RDATA_PATH)
message("df_final cargado: ", nrow(df_final), " x ", ncol(df_final))

df <- df_final
nms <- names(df)

# ============================================================
# PASO 1 — Seleccionar variables
# ============================================================

message("\n--- PASO 1: Selección de variables ---")

# Variables base del modelo
VARS_BASE <- c(
  # Identificadores
  "NCODI", "anyo", "nombre_hospital", "ccaa_cnh",
  # Outputs
  "altQ_pond", "altM_pond", "altTotal_pond",
  "ln_altQ_pond", "ln_altM_pond", "ln_altTotal_pond",
  # Brutos (para diagnóstico)
  "altQ_bruto", "altM_bruto", "altTotal_bruto",
  # Intensidad
  "i_diag", "ln_i_diag", "i_simple", "ln_i_simple",
  # Inputs (logs)
  "ln_L_total", "ln_L_medico", "ln_L_quirur",
  "ln_K_camas", "ln_K_tech", "ln_K_quirof",
  # Inputs brutos
  "L_total", "L_medico", "L_quirur",
  "K_camas", "K_tech_index",
  # Determinantes de ineficiencia
  "D_desc", "D_desc_siae", "D_desc_cnh",
  "pct_sns", "pct_privado", "pct_mutuas", "pct_ingr_SNS",
  "ShareQ", "peso_grd_final", "fuente_peso"
)

# Variables de procedimientos diagnósticos
VARS_PROC <- c("proc_pet","proc_resonancia","proc_tac","proc_angio",
               "proc_spect","proc_gamma","proc_broncoscopia",
               "proc_colonoscopia","proc_ercp","proc_biopsias",
               "proc_densiom","proc_mamo","proc_rx")

VARS_TODAS <- c(VARS_BASE, VARS_PROC)

# Filtrar a las que existen
vars_presentes <- intersect(VARS_TODAS, nms)
vars_ausentes  <- setdiff(VARS_TODAS, nms)

if (length(vars_ausentes) > 0) {
  message("  Variables solicitadas no encontradas en df_final:")
  for (v in vars_ausentes)
    message(sprintf("    - %s", v))
}

message(sprintf("  Variables seleccionadas: %d / %d",
                length(vars_presentes), length(VARS_TODAS)))

df_sfa <- df[, vars_presentes, drop = FALSE]
message(sprintf("  df_sfa inicial: %d x %d", nrow(df_sfa), ncol(df_sfa)))

# ============================================================
# PASO 2 — Filtros de calidad
# ============================================================

message("\n--- PASO 2: Filtros de calidad ---")

n0 <- nrow(df_sfa)

to_num <- function(x) suppressWarnings(as.numeric(x))

# a) Sin actividad (altTotal_bruto <= 0 o NA)
if ("altTotal_bruto" %in% nms) {
  mask_a <- is.na(to_num(df_sfa$altTotal_bruto)) |
            to_num(df_sfa$altTotal_bruto) <= 0
  n_exc_a <- sum(mask_a)
  df_sfa <- df_sfa[!mask_a, ]
  message(sprintf("  a) Excluidas por altTotal_bruto <= 0 o NA: %d obs.", n_exc_a))
}

# b) Sin personal
if ("L_total" %in% names(df_sfa)) {
  mask_b <- is.na(to_num(df_sfa$L_total)) | to_num(df_sfa$L_total) <= 0
  n_exc_b <- sum(mask_b)
  df_sfa <- df_sfa[!mask_b, ]
  message(sprintf("  b) Excluidas por L_total <= 0 o NA: %d obs.", n_exc_b))
}

# c) Sin camas
if ("K_camas" %in% names(df_sfa)) {
  mask_c <- is.na(to_num(df_sfa$K_camas)) | to_num(df_sfa$K_camas) <= 0
  n_exc_c <- sum(mask_c)
  df_sfa <- df_sfa[!mask_c, ]
  message(sprintf("  c) Excluidas por K_camas <= 0 o NA: %d obs.", n_exc_c))
}

# d) Finalidad (larga estancia o psiquiátrico) — si existe en df_sfa o df
fin_col <- grep("Finalidad_agrupada|finalidad", nms, ignore.case = TRUE, value = TRUE)[1]
if (!is.na(fin_col) && fin_col %in% nms) {
  fin_vals <- as.character(df[[fin_col]])[match(rownames(df_sfa), rownames(df))]
  # Mantener esta columna informativa pero filtrar solo larga estancia / psiquiátrico
  fin_excluir <- grepl("larga.estancia|psiqui|psiqu|residencia|PSIQUI",
                        fin_vals, ignore.case = TRUE)
  n_exc_d <- sum(fin_excluir, na.rm = TRUE)
  if (n_exc_d > 0) {
    df_sfa <- df_sfa[!fin_excluir | is.na(fin_excluir), ]
    message(sprintf("  d) Excluidas por Finalidad (larga estancia/psiquiátrico): %d obs.", n_exc_d))
  } else {
    message("  d) Filtro Finalidad: 0 obs. excluidas (variable sin categorías excluyentes detectadas).")
  }
} else {
  message("  d) Filtro Finalidad: variable no encontrada en df_final, omitido.")
}

n1 <- nrow(df_sfa)
message(sprintf("  Total excluidas: %d obs. | df_sfa post-filtro: %d x %d",
                n0 - n1, n1, ncol(df_sfa)))

# ============================================================
# PASO 3 — Términos translog centrados
# ============================================================

message("\n--- PASO 3: Términos translog ---")

# Variables base para centrar
translog_base <- c("ln_L_total","ln_K_camas","ln_K_tech",
                   "ln_L_medico","ln_L_quirur")

for (v in translog_base) {
  if (!(v %in% names(df_sfa))) next
  x <- to_num(df_sfa[[v]])
  mu <- mean(x, na.rm = TRUE)
  vname_c <- paste0(v, "_c")
  df_sfa[[vname_c]] <- x - mu
  message(sprintf("  %s centrada en %.4f → %s", v, mu, vname_c))
}

# Términos cuadráticos e interacciones (si las centradas existen)
if ("ln_L_total_c" %in% names(df_sfa) && "ln_K_camas_c" %in% names(df_sfa)) {
  df_sfa$ln_L_total_c2 <- 0.5 * df_sfa$ln_L_total_c ^ 2
  df_sfa$ln_K_camas_c2 <- 0.5 * df_sfa$ln_K_camas_c ^ 2
  df_sfa$ln_LK_c       <- df_sfa$ln_L_total_c * df_sfa$ln_K_camas_c
  message("  Cuadráticos e interacción L×K creados.")
}

# Tendencia temporal
df_sfa$trend  <- as.integer(df_sfa$anyo) - 2010L
df_sfa$trend2 <- 0.5 * df_sfa$trend ^ 2
message("  trend, trend2 creados.")

# ============================================================
# PASO 4 — Interacciones para ecuación de ineficiencia
# ============================================================

message("\n--- PASO 4: Interacciones D_desc ---")

if ("D_desc" %in% names(df_sfa) && "pct_sns" %in% names(df_sfa)) {
  D  <- to_num(df_sfa$D_desc)
  ps <- to_num(df_sfa$pct_sns)
  df_sfa$desc_pago <- D * ps
  message("  desc_pago = D_desc × pct_sns creado.")
}

if ("D_desc" %in% names(df_sfa) && "ShareQ" %in% names(df_sfa)) {
  D  <- to_num(df_sfa$D_desc)
  sq <- to_num(df_sfa$ShareQ)
  df_sfa$desc_shareQ <- D * sq
  message("  desc_shareQ = D_desc × ShareQ creado.")
}

if (all(c("D_desc","pct_sns","ShareQ") %in% names(df_sfa))) {
  D  <- to_num(df_sfa$D_desc)
  ps <- to_num(df_sfa$pct_sns)
  sq <- to_num(df_sfa$ShareQ)
  df_sfa$desc_pago_shareQ <- D * ps * sq
  message("  desc_pago_shareQ = D_desc × pct_sns × ShareQ creado.")
}

# ============================================================
# PASO 5 — Resumen y guardado
# ============================================================

message("\n--- PASO 5: Resumen y guardado ---")

n_hosp_unicos <- n_distinct(df_sfa$NCODI)
message(sprintf("  df_sfa final: %d obs. | %d hospitales únicos | %d variables",
                nrow(df_sfa), n_hosp_unicos, ncol(df_sfa)))

# Distribución D_desc por año
if ("D_desc" %in% names(df_sfa)) {
  message("\n  Distribución D_desc × anyo:")
  ddesc_tab <- df_sfa %>%
    group_by(anyo) %>%
    summarise(
      n_total  = n(),
      n_D0     = sum(D_desc == 0L, na.rm = TRUE),
      n_D1     = sum(D_desc == 1L, na.rm = TRUE),
      n_NA     = sum(is.na(D_desc)),
      pct_D1   = round(100 * sum(D_desc == 1L, na.rm = TRUE) / n(), 1),
      .groups  = "drop"
    )
  print(ddesc_tab)
}

# Cobertura ln_i_diag por año
if ("ln_i_diag" %in% names(df_sfa)) {
  message("\n  Cobertura ln_i_diag por año (% válidos):")
  idiag_tab <- df_sfa %>%
    group_by(anyo) %>%
    summarise(
      pct_ln_i_diag = round(100 * sum(!is.na(ln_i_diag)) / n(), 1),
      .groups = "drop"
    )
  print(idiag_tab)
}

# Guardar
df_sfa_path_rdata <- file.path(INT_DIR, "df_sfa.RData")
df_sfa_path_csv   <- file.path(INT_DIR, "df_sfa.csv")

save(df_sfa, file = df_sfa_path_rdata)
readr::write_csv2(df_sfa, df_sfa_path_csv, na = "")   # sep=";", dec=","

message(sprintf("\n  Guardado en: %s", df_sfa_path_rdata))
message(sprintf("  Guardado en: %s", df_sfa_path_csv))
message(sprintf("  Variables en df_sfa: %s",
                paste(names(df_sfa), collapse = ", ")))

message("\n=== Script 12 completado ===")
