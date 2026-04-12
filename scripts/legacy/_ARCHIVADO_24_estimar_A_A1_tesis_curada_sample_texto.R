library(sfaR)

load("data_intermediate/tesis_curada_recon/modelos_A_A1_tesis_curada.RData")

ccaa_vars <- grep("^d_ccaa_[0-9]+$", names(sample_texto), value = TRUE)

frontier_str <- paste(
  "~ ln_L_total_c + ln_K_camas_c + ln_K_tech_c +",
  "ln_L_total_c2 + ln_K_camas_c2 + trend + trend2 +",
  "d_cluster2 + d_cluster3 + d_cluster4 + d_cluster5"
)

form_tl_total <- as.formula(paste("ln_altTotal_pond", frontier_str))
form_tl_int <- as.formula(paste("ln_i_diag", frontier_str))
form_cd_total <- as.formula(
  "ln_altTotal_pond ~ ln_L_total_c + ln_K_camas_c + ln_K_tech_c + trend + trend2 + d_cluster2 + d_cluster3 + d_cluster4 + d_cluster5"
)
form_cd_int <- as.formula(
  "ln_i_diag ~ ln_L_total_c + ln_K_camas_c + ln_K_tech_c + trend + trend2 + d_cluster2 + d_cluster3 + d_cluster4 + d_cluster5"
)

uhet_A <- as.formula(paste(
  "~ D_desc_curada + pct_sns + desc_pago_curada + ShareQ + desc_shareQ_curada +",
  paste(ccaa_vars, collapse = " + ")
))
uhet_A1 <- as.formula(paste(
  "~ D_desc_curada + pct_sns + desc_pago_curada +",
  paste(ccaa_vars, collapse = " + ")
))
uhet_A_simple <- ~ D_desc_curada + pct_sns + ShareQ
uhet_A1_simple <- ~ D_desc_curada + pct_sns

es_degenerado <- function(m) {
  if (is.null(m)) return(TRUE)
  ll <- tryCatch(as.numeric(m$mlLoglik), error = function(e) NA_real_)
  if (!is.finite(ll) || ll > 0 || abs(ll) > 1e10) return(TRUE)
  cf <- tryCatch(coef(m), error = function(e) NULL)
  if (is.null(cf) || any(!is.finite(cf)) || any(abs(cf) > 100, na.rm = TRUE)) return(TRUE)
  FALSE
}

fit_sfa <- function(form_tl, form_cd, uhet_full, uhet_simple, data, label) {
  cfgs <- list(
    list(form = form_tl, uhet = uhet_full, ud = "tnormal", mt = "bfgs", ht = 1L),
    list(form = form_tl, uhet = uhet_full, ud = "tnormal", mt = "bhhh", ht = 1L),
    list(form = form_tl, uhet = uhet_full, ud = "tnormal", mt = "nm", ht = 2L),
    list(form = form_cd, uhet = uhet_simple, ud = "tnormal", mt = "bfgs", ht = 1L),
    list(form = form_tl, uhet = uhet_full, ud = "hnormal", mt = "bfgs", ht = 1L),
    list(form = form_cd, uhet = uhet_simple, ud = "hnormal", mt = "bfgs", ht = 1L)
  )
  for (cfg in cfgs) {
    message(sprintf("[%s] %s/%s", label, cfg$ud, cfg$mt))
    fit <- NULL
    capture.output({
      fit <- tryCatch(
        suppressWarnings(sfacross(
          formula = cfg$form,
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
    if (!is.null(fit) && !es_degenerado(fit)) return(fit)
  }
  NULL
}

extract_terms <- function(model, model_name) {
  cf <- coef(model)
  se <- tryCatch(sqrt(diag(vcov(model))), error = function(e) rep(NA_real_, length(cf)))
  z <- cf / se
  p <- 2 * pnorm(abs(z), lower.tail = FALSE)
  keep <- c("Zu_D_desc_curada", "Zu_pct_sns", "Zu_desc_pago_curada", "Zu_ShareQ", "Zu_desc_shareQ_curada")
  out <- data.frame(
    modelo = model_name,
    term = names(cf),
    coef = as.numeric(cf),
    p = as.numeric(p),
    sig = ifelse(
      p < 0.001, "***",
      ifelse(p < 0.01, "**", ifelse(p < 0.05, "*", ifelse(p < 0.10, ".", "ns")))
    )
  )
  out[out$term %in% keep, , drop = FALSE]
}

m1 <- fit_sfa(form_tl_total, form_cd_total, uhet_A, uhet_A_simple, sample_texto, "A_Total_texto")
m2 <- fit_sfa(form_tl_int, form_cd_int, uhet_A, uhet_A_simple, sample_texto, "A_I_texto")
m3 <- fit_sfa(form_tl_total, form_cd_total, uhet_A1, uhet_A1_simple, sample_texto, "A1_Total_texto")
m4 <- fit_sfa(form_tl_int, form_cd_int, uhet_A1, uhet_A1_simple, sample_texto, "A1_I_texto")

coef_tab <- rbind(
  extract_terms(m1, "A_Total_texto"),
  extract_terms(m2, "A_I_texto"),
  extract_terms(m3, "A1_Total_texto"),
  extract_terms(m4, "A1_I_texto")
)
write.csv(coef_tab, "data_intermediate/tesis_curada_recon/coeficientes_A_A1_tesis_curada_sample_texto.csv", row.names = FALSE)

summary_tab <- data.frame(
  modelo = c("A_Total_texto", "A_I_texto", "A1_Total_texto", "A1_I_texto"),
  n_obs = nrow(sample_texto),
  n_hospitales = length(unique(sample_texto$NCODI)),
  logLik = c(
    if (!is.null(m1)) as.numeric(m1$mlLoglik) else NA_real_,
    if (!is.null(m2)) as.numeric(m2$mlLoglik) else NA_real_,
    if (!is.null(m3)) as.numeric(m3$mlLoglik) else NA_real_,
    if (!is.null(m4)) as.numeric(m4$mlLoglik) else NA_real_
  )
)
write.csv(summary_tab, "data_intermediate/tesis_curada_recon/resumen_A_A1_tesis_curada_sample_texto.csv", row.names = FALSE)

save(m1, m2, m3, m4, file = "data_intermediate/tesis_curada_recon/modelos_A_A1_tesis_curada_sample_texto.RData")
