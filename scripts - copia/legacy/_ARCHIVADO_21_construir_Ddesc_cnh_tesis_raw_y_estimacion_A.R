config_path <- if (file.exists("scripts/00_config.R")) "scripts/00_config.R" else "00_config.R"
source(config_path)

required_pkgs <- c("dplyr", "readr", "sfaR")
missing_pkgs <- required_pkgs[!vapply(required_pkgs, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing_pkgs) > 0) {
  stop("Faltan paquetes: ", paste(missing_pkgs, collapse = ", "), call. = FALSE)
}

library(dplyr)
library(readr)
library(sfaR)

message("\n=== Variante tesis raw CNH + estimacion A/A1 ===")

cmp_path <- file.path(INT_DIR, "cnh_raw_compare", "comparacion_siae_vs_cnh_raw_hospital_anio.csv")
panel_path <- file.path(INT_DIR, "cnh_raw_compare", "cnh_raw_dependencia_panel_2010_2023.csv")
if (!file.exists(cmp_path)) {
  stop("No existe comparacion SIAE-CNH raw: ", cmp_path, call. = FALSE)
}
if (!file.exists(panel_path)) {
  stop("No existe panel raw CNH: ", panel_path, call. = FALSE)
}

load(file.path(INT_DIR, "legacy_tesis_recon", "int", "df_sfa.RData"))
cmp <- read.csv(cmp_path, stringsAsFactors = FALSE)
panel <- read.csv(panel_path, stringsAsFactors = FALSE)

cmp <- cmp %>%
  mutate(
    CODCNH = as.character(CODCNH),
    anyo = suppressWarnings(as.integer(anyo))
  ) %>%
  left_join(
    panel %>%
      transmute(
        anyo = suppressWarnings(as.integer(anyo)),
        CODCNH = as.character(CODCNH),
        cod_dep_cnh_raw = suppressWarnings(as.integer(cod_dep_cnh_raw)),
        dep_desc_raw = as.character(dep_desc_raw),
        fuente_cnh_raw = as.character(fuente_cnh_raw)
      ),
    by = c("anyo", "CODCNH"),
    suffix = c("", "_panel")
  ) %>%
  mutate(
    dep_desc_raw = dplyr::coalesce(dep_desc_raw_panel, dep_desc_raw),
    fuente_cnh_raw = dplyr::coalesce(fuente_cnh_raw_panel, fuente_cnh_raw)
  ) %>%
  select(-dplyr::any_of(c("dep_desc_raw_panel", "fuente_cnh_raw_panel")))

cmp$D_desc_cnh_tesis_raw <- with(cmp, dplyr::case_when(
  !is.na(cod_dep_cnh_raw) & anyo <= 2018 & cod_dep_cnh_raw %in% c(19, 20, 21, 22, 23) ~ 1L,
  !is.na(cod_dep_cnh_raw) & anyo >= 2019 & cod_dep_cnh_raw %in% c(20, 21, 22) ~ 1L,
  !is.na(cod_dep_cnh_raw) ~ 0L,
  TRUE ~ NA_integer_
))

cmp$D_desc_tesis_raw <- ifelse(
  !is.na(cmp$D_desc_cnh_tesis_raw),
  cmp$D_desc_cnh_tesis_raw,
  cmp$D_desc_siae
)

out_dir <- file.path(INT_DIR, "cnh_raw_compare")
write.csv(
  cmp,
  file.path(out_dir, "comparacion_siae_vs_cnh_tesis_raw_hospital_anio.csv"),
  row.names = FALSE,
  na = ""
)

df_model <- df_sfa %>%
  mutate(
    NCODI = as.character(NCODI),
    anyo = suppressWarnings(as.integer(anyo))
  ) %>%
  left_join(
    cmp %>%
      transmute(
        NCODI = as.character(NCODI),
        anyo = suppressWarnings(as.integer(anyo)),
        D_desc_cnh_tesis_raw,
        D_desc_tesis_raw
      ),
    by = c("NCODI", "anyo")
  ) %>%
  mutate(
    desc_pago_tesis_raw = D_desc_tesis_raw * pct_sns,
    desc_shareQ_tesis_raw = D_desc_tesis_raw * ShareQ
  )

summary_raw <- data.frame(
  metrica = c("n_obs", "n_hospitales", "D_desc_tesis_raw_1", "D_desc_tesis_raw_0", "pct_D1"),
  valor = c(
    nrow(df_model),
    dplyr::n_distinct(df_model$NCODI),
    sum(df_model$D_desc_tesis_raw == 1, na.rm = TRUE),
    sum(df_model$D_desc_tesis_raw == 0, na.rm = TRUE),
    round(100 * mean(df_model$D_desc_tesis_raw == 1, na.rm = TRUE), 1)
  )
)
write.csv(summary_raw, file.path(out_dir, "resumen_D_desc_tesis_raw_global.csv"), row.names = FALSE)

df_est_A <- df_model %>%
  filter(
    es_agudo == 1,
    !anyo %in% 2020:2022,
    altTotal_bruto >= 200,
    !is.na(D_desc_tesis_raw), !is.na(pct_sns), !is.na(ShareQ),
    !is.na(ln_L_total_c), !is.na(ln_K_camas_c),
    !is.na(ccaa_cod),
    is.finite(ln_altTotal_pond),
    is.finite(ln_i_diag),
    ln_i_diag > quantile(ln_i_diag, 0.01, na.rm = TRUE)
  )

sample_a <- data.frame(
  n_obs_A = nrow(df_est_A),
  n_hospitales_A = dplyr::n_distinct(df_est_A$NCODI),
  D1_obs = sum(df_est_A$D_desc_tesis_raw == 1, na.rm = TRUE),
  D0_obs = sum(df_est_A$D_desc_tesis_raw == 0, na.rm = TRUE),
  pct_D1 = round(100 * mean(df_est_A$D_desc_tesis_raw == 1, na.rm = TRUE), 1)
)
write.csv(sample_a, file.path(out_dir, "muestra_A_D_desc_tesis_raw.csv"), row.names = FALSE)

ccaa_vars <- grep("^d_ccaa_[0-9]+$", names(df_est_A), value = TRUE)
frontier_str <- paste(
  "~ ln_L_total_c + ln_K_camas_c + ln_K_tech_c +",
  "ln_L_total_c2 + ln_K_camas_c2 + trend + trend2 +",
  "d_cluster2 + d_cluster3 + d_cluster4 + d_cluster5"
)

uhet_A <- as.formula(paste(
  "~ D_desc_tesis_raw + pct_sns + desc_pago_tesis_raw +",
  "ShareQ + desc_shareQ_tesis_raw +",
  paste(ccaa_vars, collapse = " + ")
))
uhet_A1 <- as.formula(paste(
  "~ D_desc_tesis_raw + pct_sns + desc_pago_tesis_raw +",
  paste(ccaa_vars, collapse = " + ")
))
uhet_A_fb <- ~ D_desc_tesis_raw + pct_sns + desc_pago_tesis_raw + ShareQ + desc_shareQ_tesis_raw
uhet_A1_fb <- ~ D_desc_tesis_raw + pct_sns + desc_pago_tesis_raw

es_degenerado <- function(m) {
  if (is.null(m)) return(TRUE)
  ll <- tryCatch(as.numeric(m$mlLoglik), error = function(e) NA_real_)
  if (!is.finite(ll) || abs(ll) > 1e10) return(TRUE)
  cf <- tryCatch(coef(m), error = function(e) NULL)
  if (is.null(cf) || any(!is.finite(cf))) return(TRUE)
  if (any(abs(cf) > 100, na.rm = TRUE)) return(TRUE)
  FALSE
}

fit_try <- function(lhs, uhet_main, uhet_fb, data, label) {
  frontier <- as.formula(paste(lhs, frontier_str))
  configs <- list(
    list(uhet = uhet_main, ud = "tnormal", mt = "bfgs", ht = 1L),
    list(uhet = uhet_main, ud = "tnormal", mt = "bhhh", ht = 1L),
    list(uhet = uhet_main, ud = "tnormal", mt = "nm", ht = 2L),
    list(uhet = uhet_fb,   ud = "hnormal", mt = "bfgs", ht = 1L),
    list(uhet = uhet_fb,   ud = "hnormal", mt = "bhhh", ht = 2L)
  )
  for (cfg in configs) {
    m <- NULL
    capture.output({
      m <- tryCatch(
        suppressWarnings(sfaR::sfacross(
          formula = frontier,
          uhet = cfg$uhet,
          data = data,
          S = 1L,
          udist = cfg$ud,
          method = cfg$mt,
          hessianType = cfg$ht
        )),
        error = function(e) NULL
      )
    })
    if (!is.null(m) && !es_degenerado(m)) {
      message(sprintf("OK [%s]: ll=%.2f udist=%s method=%s", label, as.numeric(m$mlLoglik), cfg$ud, cfg$mt))
      return(m)
    }
  }
  NULL
}

extract_tab <- function(m, label) {
  if (is.null(m)) return(NULL)
  cf <- coef(m)
  se <- tryCatch(sqrt(diag(vcov(m))), error = function(e) rep(NA_real_, length(cf)))
  z <- cf / se
  p <- 2 * pnorm(abs(z), lower.tail = FALSE)
  data.frame(
    modelo = label,
    term = names(cf),
    coef = as.numeric(cf),
    se = as.numeric(se),
    p = as.numeric(p),
    sig = dplyr::case_when(
      is.na(p) ~ "na",
      p < 0.001 ~ "***",
      p < 0.01 ~ "**",
      p < 0.05 ~ "*",
      p < 0.10 ~ ".",
      TRUE ~ "ns"
    ),
    stringsAsFactors = FALSE
  )
}

model_A_tot <- fit_try("ln_altTotal_pond", uhet_A, uhet_A_fb, df_est_A, "A_Total_tesis_raw")
model_A_i <- fit_try("ln_i_diag", uhet_A, uhet_A_fb, df_est_A, "A_I_tesis_raw")
model_A1_tot <- fit_try("ln_altTotal_pond", uhet_A1, uhet_A1_fb, df_est_A, "A1_Total_tesis_raw")
model_A1_i <- fit_try("ln_i_diag", uhet_A1, uhet_A1_fb, df_est_A, "A1_I_tesis_raw")

coef_tab <- bind_rows(
  extract_tab(model_A_tot, "A_Total_tesis_raw"),
  extract_tab(model_A_i, "A_I_tesis_raw"),
  extract_tab(model_A1_tot, "A1_Total_tesis_raw"),
  extract_tab(model_A1_i, "A1_I_tesis_raw")
)
write.csv(coef_tab, file.path(out_dir, "coeficientes_A_A1_tesis_raw.csv"), row.names = FALSE)

model_summary <- bind_rows(
  data.frame(modelo = "A_Total_tesis_raw", n = if (is.null(model_A_tot)) NA else nrow(model_A_tot$dataTable), logLik = if (is.null(model_A_tot)) NA else as.numeric(model_A_tot$mlLoglik), TE = if (is.null(model_A_tot)) NA else mean(sfaR::efficiencies(model_A_tot)$teJLMS, na.rm = TRUE)),
  data.frame(modelo = "A_I_tesis_raw", n = if (is.null(model_A_i)) NA else nrow(model_A_i$dataTable), logLik = if (is.null(model_A_i)) NA else as.numeric(model_A_i$mlLoglik), TE = if (is.null(model_A_i)) NA else mean(sfaR::efficiencies(model_A_i)$teJLMS, na.rm = TRUE)),
  data.frame(modelo = "A1_Total_tesis_raw", n = if (is.null(model_A1_tot)) NA else nrow(model_A1_tot$dataTable), logLik = if (is.null(model_A1_tot)) NA else as.numeric(model_A1_tot$mlLoglik), TE = if (is.null(model_A1_tot)) NA else mean(sfaR::efficiencies(model_A1_tot)$teJLMS, na.rm = TRUE)),
  data.frame(modelo = "A1_I_tesis_raw", n = if (is.null(model_A1_i)) NA else nrow(model_A1_i$dataTable), logLik = if (is.null(model_A1_i)) NA else as.numeric(model_A1_i$mlLoglik), TE = if (is.null(model_A1_i)) NA else mean(sfaR::efficiencies(model_A1_i)$teJLMS, na.rm = TRUE))
)
write.csv(model_summary, file.path(out_dir, "resumen_A_A1_tesis_raw.csv"), row.names = FALSE)

save(model_A_tot, model_A_i, model_A1_tot, model_A1_i,
     file = file.path(out_dir, "modelos_A_A1_tesis_raw.RData"))

message("Comparacion tesis raw guardada en: ", file.path(out_dir, "comparacion_siae_vs_cnh_tesis_raw_hospital_anio.csv"))
message("Coeficientes A/A1 guardados en: ", file.path(out_dir, "coeficientes_A_A1_tesis_raw.csv"))
