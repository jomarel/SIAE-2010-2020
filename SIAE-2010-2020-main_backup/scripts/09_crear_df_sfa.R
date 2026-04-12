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

# Términos cuadráticos (ln_LK_c se conserva en df_sfa pero no entra en las
# fronteras — la correlación ln_LK_c↔ln_L_total_c2 es r≈0.95 y provoca
# multicolinealidad severa que degenera la Hessiana)
if ("ln_L_total_c" %in% names(df_sfa) && "ln_K_camas_c" %in% names(df_sfa)) {
  df_sfa$ln_L_total_c2 <- 0.5 * df_sfa$ln_L_total_c ^ 2
  df_sfa$ln_K_camas_c2 <- 0.5 * df_sfa$ln_K_camas_c ^ 2
  df_sfa$ln_LK_c       <- df_sfa$ln_L_total_c * df_sfa$ln_K_camas_c
  message("  Cuadráticos e interacción L×K creados (ln_LK_c solo diagnóstico).")
}

# Verificación de multicolinealidad entre términos que sí entran al modelo
vars_tl_modelo <- c("ln_L_total_c","ln_K_camas_c","ln_K_tech_c",
                    "ln_L_total_c2","ln_K_camas_c2")
vars_tl_ok <- intersect(vars_tl_modelo, names(df_sfa))
if (length(vars_tl_ok) >= 2) {
  mat_cor <- round(cor(df_sfa[vars_tl_ok], use = "complete.obs"), 3)
  message("  Correlación entre términos translog del modelo (objetivo: ningún par > 0.85):")
  print(mat_cor)
  max_off <- max(abs(mat_cor[upper.tri(mat_cor)]))
  message(sprintf("  Correlación máxima fuera de diagonal: %.3f %s",
                  max_off, if (max_off > 0.85) "*** AVISO: supera 0.85" else "(OK)"))
}

# Tendencia temporal
df_sfa$trend  <- as.integer(df_sfa$anyo) - 2010L
df_sfa$trend2 <- 0.5 * df_sfa$trend ^ 2
message("  trend, trend2 creados.")

# Flag de outliers en ln_i_diag (p1-p99) — NO elimina obs, solo marca
if ("ln_i_diag" %in% names(df_sfa)) {
  p1_i  <- quantile(df_sfa$ln_i_diag, 0.01, na.rm = TRUE)
  p99_i <- quantile(df_sfa$ln_i_diag, 0.99, na.rm = TRUE)
  message(sprintf("  Rango ln_i_diag p1-p99: [%.3f, %.3f]", p1_i, p99_i))
  df_sfa$i_diag_outlier <- !is.na(df_sfa$ln_i_diag) &
    (df_sfa$ln_i_diag < p1_i | df_sfa$ln_i_diag > p99_i)
  message(sprintf("  Obs marcadas i_diag_outlier (p1-p99): %d",
                  sum(df_sfa$i_diag_outlier, na.rm = TRUE)))
} else {
  df_sfa$i_diag_outlier <- FALSE
}

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

# ============================================================
# BLOQUE SFA DEFINITIVO — variables para análisis completo
# Todo lo que necesita script 11_estimar_sfa.R
# ============================================================

message("\n--- BLOQUE SFA DEFINITIVO ---")

# Joins auxiliares desde df_final (antes de bloques E y F)
# cod_finalidad_agrupada (necesario para es_agudo)
if (!"cod_finalidad_agrupada" %in% names(df_sfa) &&
    "cod_finalidad_agrupada" %in% names(df_final)) {
  df_sfa <- dplyr::left_join(
    df_sfa,
    df_final %>% dplyr::select(NCODI, anyo, cod_finalidad_agrupada) %>% dplyr::distinct(),
    by = c("NCODI", "anyo")
  )
  message("  [pre-E] cod_finalidad_agrupada añadida desde df_final.")
}

# u-vars para serv_complejos (necesarias para bloques F y G)
u_vars_sc <- c("u40","u41","u42","u49","u86","u87","u93","u94")
u_vars_faltantes <- setdiff(u_vars_sc, names(df_sfa))
if (length(u_vars_faltantes) > 0) {
  u_vars_en_final <- intersect(u_vars_faltantes, names(df_final))
  if (length(u_vars_en_final) > 0) {
    df_sfa <- dplyr::left_join(
      df_sfa,
      df_final %>%
        dplyr::select(dplyr::all_of(c("NCODI","anyo", u_vars_en_final))) %>%
        dplyr::distinct(),
      by = c("NCODI", "anyo")
    )
    message(sprintf("  [pre-F] u-vars añadidas desde df_final: %s",
                    paste(u_vars_en_final, collapse = ", ")))
  }
}

# ── A. camas por servicio (de df_final) ─────────────────────
if (!all(c("camas_cirugia","camas_medicina") %in% names(df_sfa))) {
  vars_cam_disp <- intersect(c("camas_cirugia","camas_medicina"), names(df_final))
  if (length(vars_cam_disp) > 0) {
    df_sfa <- dplyr::left_join(
      df_sfa,
      df_final %>%
        dplyr::select(dplyr::all_of(c("NCODI","anyo", vars_cam_disp))) %>%
        dplyr::distinct(),
      by = c("NCODI", "anyo")
    )
    message(sprintf("  A. %s añadidas.", paste(vars_cam_disp, collapse = ", ")))
  } else {
    message("  A. camas_cirugia / camas_medicina no existen en df_final — omitido.")
  }
}

# ── B. Variables COVID ───────────────────────────────────────
df_sfa <- df_sfa %>%
  dplyr::mutate(
    covid        = as.integer(anyo %in% 2020:2022),
    covid_fuerte = as.integer(anyo == 2020),
    covid_leve   = as.integer(anyo %in% c(2021, 2022))
  )
message("  B. Variables COVID creadas.")

# ── C. Código numérico CCAA desde ccaa_codigo SIAE ──────────
# Usar ccaa_codigo del SIAE (cobertura ~100%) en lugar de
# ccaa_cnh del CNH (8% NAs). Código 21 = La Rioja + Ceuta
# + Melilla en SIAE — desambiguar con ccaa_cnh.

# Eliminar variables ccaa previas para evitar duplicados
vars_ccaa_old <- grep("^ccaa|^d_ccaa_", names(df_sfa),
                      value = TRUE)
df_sfa <- df_sfa %>%
  dplyr::select(-dplyr::any_of(vars_ccaa_old))

# Unir ccaa_codigo y ccaa_cnh desde df_final
mapeo_ccaa <- df_final %>%
  dplyr::select(NCODI, anyo, ccaa_codigo, ccaa_cnh) %>%
  dplyr::distinct()

df_sfa <- dplyr::left_join(df_sfa, mapeo_ccaa,
                            by = c("NCODI", "anyo"))

# Construir código corregido (17 CCAA + Ceuta=18 + Melilla=19)
df_sfa$ccaa_cod <- with(df_sfa, dplyr::case_when(
  ccaa_codigo %in% 1:16                       ~ as.integer(ccaa_codigo),
  ccaa_codigo == 21 & ccaa_cnh == "La Rioja"  ~ 17L,
  ccaa_codigo == 21 & ccaa_cnh == "Ceuta"     ~ 18L,
  ccaa_codigo == 21 & ccaa_cnh == "Melilla"   ~ 19L,
  ccaa_codigo == 21 & is.na(ccaa_cnh)         ~ 17L,
  TRUE ~ NA_integer_
))

# Eliminar columna auxiliar
df_sfa$ccaa_codigo <- NULL

message(sprintf("  C. ccaa_cod: %d válidos, %d NA",
  sum(!is.na(df_sfa$ccaa_cod)),
  sum(is.na(df_sfa$ccaa_cod))))

# ── D. Dummies CCAA (ref: 9=Cataluña; sin Ceuta=18/Melilla=19)
for (cc in setdiff(1:17, 9L)) {
  df_sfa[[paste0("d_ccaa_", cc)]] <- as.integer(
    !is.na(df_sfa$ccaa_cod) & df_sfa$ccaa_cod == cc)
}
message("  D. Dummies d_ccaa_1..17 (excl. 9) creadas.")
message(sprintf("     La Rioja (d_ccaa_17): %d obs",
  sum(df_sfa$d_ccaa_17, na.rm = TRUE)))

# ── E. Dummy hospital agudo ──────────────────────────────────
if ("cod_finalidad_agrupada" %in% names(df_sfa) &&
    !"es_agudo" %in% names(df_sfa)) {
  df_sfa <- df_sfa %>%
    dplyr::mutate(
      es_agudo = dplyr::case_when(
        cod_finalidad_agrupada == 1   ~ 1L,
        is.na(cod_finalidad_agrupada) ~ NA_integer_,
        TRUE ~ 0L
      )
    )
  message(sprintf("  E. es_agudo: %d agudos, %d otros, %d NA",
    sum(df_sfa$es_agudo == 1, na.rm = TRUE),
    sum(df_sfa$es_agudo == 0, na.rm = TRUE),
    sum(is.na(df_sfa$es_agudo))))
} else if ("es_agudo" %in% names(df_sfa)) {
  message("  E. es_agudo ya existe.")
} else {
  message("  E. AVISO: cod_finalidad_agrupada no disponible — es_agudo no creado.")
}

# ── F. Índice servicios complejos ────────────────────────────
if (!"serv_complejos" %in% names(df_sfa)) {
  vars_sc <- intersect(
    c("u40","u41","u42","u49","u86","u87","u93","u94"),
    names(df_sfa))
  if (length(vars_sc) > 0) {
    df_sfa <- df_sfa %>%
      dplyr::mutate(
        serv_complejos = rowSums(
          dplyr::across(dplyr::all_of(vars_sc),
                        ~ as.integer(. == 1)),
          na.rm = TRUE))
    message(sprintf("  F. serv_complejos creado (%d vars).", length(vars_sc)))
  } else {
    df_sfa$serv_complejos <- 0L
    message("  F. AVISO: u-vars no disponibles — serv_complejos = 0.")
  }
} else {
  message("  F. serv_complejos ya existe.")
}

# ── G. Cluster de hospital (5 grupos Ministerio) ─────────────
# Nota: K_camas = camas_funcionamiento (mismo campo, nombre canónico del pipeline)
if (!"grupo_cluster" %in% names(df_sfa)) {
  nm_camas_g <- {
    if ("camas_funcionamiento" %in% names(df_sfa)) "camas_funcionamiento"
    else if ("K_camas" %in% names(df_sfa)) "K_camas"
    else NA_character_
  }
  if (!is.na(nm_camas_g)) {
    hosp_stats <- df_sfa %>%
      dplyr::group_by(NCODI) %>%
      dplyr::summarise(
        med_camas = median(.data[[nm_camas_g]], na.rm = TRUE),
        med_tech  = median(K_tech_index,        na.rm = TRUE),
        max_serv  = if ("serv_complejos" %in% names(df_sfa))
                     max(serv_complejos, na.rm = TRUE)
                   else 0L,
        .groups = "drop"
      ) %>%
      dplyr::mutate(
        max_serv = ifelse(!is.finite(max_serv), 0, max_serv),
        grupo_cluster = dplyr::case_when(
          med_camas <  100 & med_tech <= 1 & max_serv == 0 ~ 1L,
          med_camas <  200 & med_tech <= 2 & max_serv <= 1 ~ 2L,
          med_camas <  400 & med_tech <= 4 & max_serv <= 2 ~ 3L,
          med_camas <  700 & max_serv <= 4                 ~ 4L,
          TRUE                                             ~ 5L
        )
      )
    df_sfa <- dplyr::left_join(
      df_sfa,
      hosp_stats %>% dplyr::select(NCODI, grupo_cluster),
      by = "NCODI")
    message("  G. grupo_cluster creado.")
  } else {
    message("  G. AVISO: camas no disponibles — grupo_cluster no creado.")
  }
} else {
  message("  G. grupo_cluster ya existe.")
}

# Dummies cluster (ref: 1 = comarcal)
for (g in 2:5) {
  vname <- paste0("d_cluster", g)
  if (!vname %in% names(df_sfa) && "grupo_cluster" %in% names(df_sfa)) {
    df_sfa[[vname]] <- as.integer(
      !is.na(df_sfa$grupo_cluster) & df_sfa$grupo_cluster == g)
  }
}
message("  G. d_cluster2-5 creadas.")

# ── H. Grupos de pago (3 categorías institucionales) ─────────
# Pub_Retro (ref): D_desc=0
# Priv_Conc: D_desc=1 y pct_sns >= 0.50 (concertado SNS)
# Priv_Merc: D_desc=1 y pct_sns <  0.50 (mercado privado)
if (!"grupo_pago" %in% names(df_sfa)) {
  df_sfa <- df_sfa %>%
    dplyr::mutate(
      grupo_pago = dplyr::case_when(
        D_desc == 0                    ~ "Pub_Retro",
        D_desc == 1 & pct_sns >= 0.50 ~ "Priv_Conc",
        D_desc == 1 & pct_sns <  0.50 ~ "Priv_Merc",
        TRUE ~ NA_character_
      ),
      d_Priv_Conc = as.integer(!is.na(grupo_pago) & grupo_pago == "Priv_Conc"),
      d_Priv_Merc = as.integer(!is.na(grupo_pago) & grupo_pago == "Priv_Merc"),
      Conc_shareQ = d_Priv_Conc * ShareQ,
      Merc_shareQ = d_Priv_Merc * ShareQ
    )
  message("  H. grupo_pago y dummies creados:")
  print(table(df_sfa$grupo_pago, useNA = "always"))
} else {
  message("  H. grupo_pago ya existe.")
}

# ── I. Verificar ln_L_total_c (centradas) ───────────────────
# Estas ya se crean en PASO 3 del script — solo verificar
if (!"ln_L_total_c" %in% names(df_sfa)) {
  df_sfa <- df_sfa %>%
    dplyr::mutate(
      ln_L_total_c  = ln_L_total  - mean(ln_L_total,  na.rm = TRUE),
      ln_K_camas_c  = ln_K_camas  - mean(ln_K_camas,  na.rm = TRUE),
      ln_K_tech_c   = ln_K_tech   - mean(ln_K_tech,   na.rm = TRUE),
      ln_L_medico_c = ln_L_medico - mean(ln_L_medico, na.rm = TRUE),
      ln_L_quirur_c = ln_L_quirur - mean(ln_L_quirur, na.rm = TRUE)
    )
  message("  I. Variables centradas creadas.")
} else {
  message("  I. Variables centradas ya existen (creadas en PASO 3).")
}

# ── J. D_desc_siae y D_desc_cnh ─────────────────────────────
if (!"D_desc_siae" %in% names(df_sfa)) {
  warning("  J. D_desc_siae no encontrada — revisar script 06.")
} else {
  message("  J. D_desc_siae y D_desc_cnh ya existen.")
}

# ============================================================
# FIN BLOQUE SFA DEFINITIVO
# ============================================================

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
