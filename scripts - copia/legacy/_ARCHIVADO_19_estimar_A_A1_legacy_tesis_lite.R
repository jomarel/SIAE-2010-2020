config_path <- if (file.exists("scripts/00_config.R")) "scripts/00_config.R" else "00_config.R"
source(config_path)

required_pkgs <- c("dplyr", "sfaR", "readr")
missing_pkgs <- required_pkgs[!vapply(required_pkgs, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing_pkgs) > 0) {
  stop("Faltan paquetes: ", paste(missing_pkgs, collapse = ", "), call. = FALSE)
}

library(dplyr)
library(sfaR)
library(readr)

recon_root <- file.path(INT_DIR, "legacy_tesis_recon")
recon_int <- file.path(recon_root, "int")
recon_out <- file.path(recon_root, "out")

load(file.path(recon_int, "df_sfa.RData"))
message("df_sfa legacy_tesis: ", nrow(df_sfa), " x ", ncol(df_sfa))

ccaa_vars <- grep("^d_ccaa_[0-9]+$", names(df_sfa), value = TRUE)
frontier_str <- paste(
  "~ ln_L_total_c + ln_K_camas_c + ln_K_tech_c +",
  "ln_L_total_c2 + ln_K_camas_c2 + trend + trend2 +",
  "d_cluster2 + d_cluster3 + d_cluster4 + d_cluster5"
)

df_base <- df_sfa %>%
  filter(
    es_agudo == 1,
    !anyo %in% 2020:2022,
    altTotal_bruto > 100,
    !is.na(D_desc), !is.na(pct_sns), !is.na(ShareQ),
    !is.na(ln_L_total_c), !is.na(ln_K_camas_c),
    !is.na(ccaa_cod)
  )

df_est_A <- df_base %>%
  filter(
    altTotal_bruto >= 200,
    is.finite(ln_altTotal_pond),
    is.finite(ln_i_diag),
    ln_i_diag > quantile(ln_i_diag, 0.01, na.rm = TRUE)
  )

message("df_est_A legacy_tesis: ", nrow(df_est_A),
        " obs | ", dplyr::n_distinct(df_est_A$NCODI), " hosp")

uhet_A_full <- as.formula(paste(
  "~ D_desc + pct_sns + desc_pago + ShareQ + desc_shareQ +",
  paste(ccaa_vars, collapse = " + ")
))
uhet_A_nocc <- ~ D_desc + pct_sns + desc_pago + ShareQ + desc_shareQ
uhet_A_min  <- ~ D_desc + pct_sns + desc_pago + ShareQ
fb_A <- list(uhet_A_nocc, uhet_A_min)

uhet_A1_full <- as.formula(paste(
  "~ D_desc + pct_sns + desc_pago +",
  paste(ccaa_vars, collapse = " + ")
))
uhet_A1_nocc <- ~ D_desc + pct_sns + desc_pago
fb_A1 <- list(uhet_A1_nocc, uhet_A1_nocc)

es_degenerado <- function(m) {
  if (is.null(m)) return(TRUE)
  ll <- tryCatch(as.numeric(m$mlLoglik), error = function(e) NA_real_)
  if (!is.finite(ll) || abs(ll) > 1e10) return(TRUE)
  cf <- tryCatch(coef(m), error = function(e) NULL)
  if (is.null(cf) || any(!is.finite(cf))) return(TRUE)
  if (any(abs(cf) > 100, na.rm = TRUE)) return(TRUE)
  FALSE
}

estimar_sfa <- function(lhs, uhet, data, label, fallbacks) {
  frontier <- as.formula(paste(lhs, frontier_str))
  configs <- list(
    list(ud = "tnormal", mt = "bfgs", ht = 1L),
    list(ud = "tnormal", mt = "bhhh", ht = 1L),
    list(ud = "tnormal", mt = "nm",   ht = 2L),
    list(ud = "hnormal", mt = "bfgs", ht = 1L),
    list(ud = "hnormal", mt = "bhhh", ht = 2L)
  )

  for (uh in c(list(uhet), fallbacks)) {
    for (cfg in configs) {
      m <- NULL
      capture.output({
        m <- tryCatch(
          suppressWarnings(sfaR::sfacross(
            formula = frontier,
            uhet = uh,
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
        message(sprintf("OK [%s]: ll=%.2f udist=%s method=%s",
                        label, as.numeric(m$mlLoglik), cfg$ud, cfg$mt))
        return(m)
      }
    }
  }
  NULL
}

extraer_tab <- function(m, label) {
  if (is.null(m)) return(NULL)
  cf <- coef(m)
  se <- tryCatch(sqrt(diag(vcov(m))), error = function(e) rep(NA_real_, length(cf)))
  tstat <- cf / se
  pval <- 2 * pnorm(abs(tstat), lower.tail = FALSE)
  data.frame(
    modelo = label,
    term = names(cf),
    coef = as.numeric(cf),
    se = as.numeric(se),
    p = as.numeric(pval),
    sig = dplyr::case_when(
      is.na(pval) ~ "na",
      pval < 0.001 ~ "***",
      pval < 0.01  ~ "**",
      pval < 0.05  ~ "*",
      pval < 0.10  ~ ".",
      TRUE ~ "ns"
    ),
    stringsAsFactors = FALSE
  )
}

modelo_A_Tot <- estimar_sfa("ln_altTotal_pond", uhet_A_full, df_est_A, "A_Total_legacy_tesis", fb_A)
modelo_A_I   <- estimar_sfa("ln_i_diag", uhet_A_full, df_est_A, "A_I_legacy_tesis", fb_A)
modelo_A1_Tot <- estimar_sfa("ln_altTotal_pond", uhet_A1_full, df_est_A, "A1_Total_legacy_tesis", fb_A1)
modelo_A1_I   <- estimar_sfa("ln_i_diag", uhet_A1_full, df_est_A, "A1_I_legacy_tesis", fb_A1)

tabs <- dplyr::bind_rows(
  extraer_tab(modelo_A_Tot, "A_Total_legacy_tesis"),
  extraer_tab(modelo_A_I, "A_I_legacy_tesis"),
  extraer_tab(modelo_A1_Tot, "A1_Total_legacy_tesis"),
  extraer_tab(modelo_A1_I, "A1_I_legacy_tesis")
)

write_csv(tabs, file.path(recon_out, "coeficientes_A_A1_legacy_tesis_lite.csv"), na = "")

resume_modelo <- function(m, label) {
  data.frame(
    modelo = label,
    n = if (is.null(m)) NA_integer_ else nrow(m$dataTable),
    logLik = if (is.null(m)) NA_real_ else as.numeric(m$mlLoglik),
    TE = if (is.null(m)) NA_real_ else mean(sfaR::efficiencies(m)$teJLMS, na.rm = TRUE),
    stringsAsFactors = FALSE
  )
}

resumen <- dplyr::bind_rows(
  resume_modelo(modelo_A_Tot, "A_Total_legacy_tesis"),
  resume_modelo(modelo_A_I, "A_I_legacy_tesis"),
  resume_modelo(modelo_A1_Tot, "A1_Total_legacy_tesis"),
  resume_modelo(modelo_A1_I, "A1_I_legacy_tesis")
)

write.csv(resumen, file.path(recon_out, "resumen_A_A1_legacy_tesis_lite.csv"), row.names = FALSE)

save(modelo_A_Tot, modelo_A_I, modelo_A1_Tot, modelo_A1_I,
     file = file.path(recon_out, "modelos_A_A1_legacy_tesis_lite.RData"))

message("Resultados guardados en: ", file.path(recon_out, "coeficientes_A_A1_legacy_tesis_lite.csv"))
