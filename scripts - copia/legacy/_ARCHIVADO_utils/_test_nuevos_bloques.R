# Script de prueba: estima SOLO los dos bloques nuevos
# Ejecutar con: Rscript scripts/utils/_test_nuevos_bloques.R

source("scripts/00_config.R")
library(sfaR); library(dplyr)

load(file.path(INT_DIR, "df_sfa.RData"))
message("df_sfa: ", nrow(df_sfa), " x ", ncol(df_sfa))

# ── Filtros (idénticos al script 11) ─────────────────────────
df_base <- df_sfa %>% filter(
  es_agudo == 1, !anyo %in% 2020:2022, altTotal_bruto > 100,
  !is.na(D_desc), !is.na(pct_sns), !is.na(ShareQ),
  !is.na(grupo_pago), !is.na(ln_L_total_c), !is.na(ln_K_camas_c),
  !is.na(ccaa_cod))

df_est_B <- df_base %>% filter(
  altQ_bruto >= 200, altM_bruto >= 200,
  is.finite(ln_altQ_pond), is.finite(ln_altM_pond))

df_est_A <- df_base %>% filter(
  altTotal_bruto >= 200,
  is.finite(ln_altTotal_pond), is.finite(ln_i_diag),
  ln_i_diag > quantile(ln_i_diag, 0.01, na.rm = TRUE))

df_est_D <- df_est_B %>%
  mutate(neg_ln_altQ = -ln_altQ_pond,
         ln_ratio_MQ = ln_altM_pond - ln_altQ_pond)

ccaa_vars <- grep("^d_ccaa_[0-9]+$", names(df_base), value = TRUE)
message("Dummies CCAA: ", length(ccaa_vars))

frontier_str <- paste(
  "~ ln_L_total_c + ln_K_camas_c + ln_K_tech_c +",
  "ln_L_total_c2 + ln_K_camas_c2 + trend + trend2 +",
  "d_cluster2 + d_cluster3 + d_cluster4 + d_cluster5")

uhet_full <- as.formula(paste(
  "~ d_Priv_Conc + d_Priv_Merc +",
  "ShareQ + Conc_shareQ + Merc_shareQ +",
  paste(ccaa_vars, collapse = " + ")))
uhet_nocc <- as.formula("~ d_Priv_Conc + d_Priv_Merc + ShareQ + Conc_shareQ + Merc_shareQ")
uhet_min  <- as.formula("~ d_Priv_Conc + d_Priv_Merc + ShareQ")

err_log <- character(0)

es_degenerado <- function(m) {
  if (is.null(m)) return(TRUE)
  ll <- tryCatch(as.numeric(m$mlLoglik), error = function(e) NA)
  if (!is.finite(ll) || abs(ll) > 1e10) return(TRUE)
  cf <- tryCatch(coef(m), error = function(e) NULL)
  if (is.null(cf) || any(!is.finite(cf))) return(TRUE)
  if (any(abs(cf) > 100, na.rm = TRUE)) return(TRUE)
  se <- tryCatch(sqrt(diag(vcov(m))), error = function(e) NULL)
  if (is.null(se)) return(TRUE)
  idx <- grep("^Zu_", names(cf))
  if (any(!is.finite(se[idx]), na.rm = TRUE)) return(TRUE)
  FALSE
}

estimar_sfa <- function(lhs, uhet, data, label) {
  frontier <- as.formula(paste(lhs, frontier_str))
  configs <- list(
    list(ud = "tnormal", mt = "bfgs", ht = 1L),
    list(ud = "tnormal", mt = "bhhh", ht = 1L),
    list(ud = "tnormal", mt = "nm",   ht = 2L),
    list(ud = "hnormal", mt = "bfgs", ht = 1L),
    list(ud = "hnormal", mt = "bhhh", ht = 2L))
  for (uh in list(uhet, uhet_nocc, uhet_min)) {
    for (cfg in configs) {
      tag <- sprintf("[%s] %s/%s uhet=%s",
        label, cfg$ud, cfg$mt, deparse(uh[[2]])[1])
      message("  Prob: ", tag)
      m <- NULL
      capture.output({
        m <- tryCatch(suppressWarnings(sfaR::sfacross(
          formula = frontier, uhet = uh, data = data, S = 1L,
          udist = cfg$ud, method = cfg$mt, hessianType = cfg$ht)),
          error = function(e) {
            err_log <<- c(err_log, paste(tag, e$message)); NULL })
      })
      if (!is.null(m) && !es_degenerado(m)) {
        message(sprintf("  OK [%s]: ll=%.2f udist=%s",
          label, as.numeric(m$mlLoglik), cfg$ud))
        return(m)
      }
    }
  }
  message("  *** FALLÓ: ", label); NULL
}

modelo_D <- readRDS(file.path(INT_DIR, "sfa_modeloD_definitivo.rds"))
message("modelo_D cargado: ll=", round(as.numeric(modelo_D$mlLoglik), 2))

# ════════════════════════════════════════════════════════
# DISEÑO A VARIANTE — D_desc binario + cluster
# ════════════════════════════════════════════════════════
message("\n═══ DISEÑO A VARIANTE — D_desc binario + cluster ═══")

uhet_Ddesc <- as.formula(paste(
  "~ D_desc + pct_sns + desc_pago +",
  "ShareQ + desc_shareQ +",
  paste(ccaa_vars, collapse = " + ")))

df_est_Av <- df_est_A %>% filter(!is.na(ccaa_cod))
message("Muestra Diseño A variante: ", nrow(df_est_Av),
        " obs | ", n_distinct(df_est_Av$NCODI), " hosp")

modelo_Tot_v <- tryCatch({
  m <- estimar_sfa("ln_altTotal_pond", uhet_Ddesc, df_est_Av, "Tot_v")
  if (!is.null(m)) {
    saveRDS(m, file.path(INT_DIR, "sfa_mTot_Ddesc_definitivo.rds"))
    message("Modelo Tot_v guardado.")
  }
  m
}, error = function(e) { message("Tot_v FALLÓ: ", e$message); NULL })

if (!is.null(modelo_Tot_v)) {
  te_Tv <- tryCatch(sfaR::efficiencies(modelo_Tot_v)$teJLMS, error = function(e) NULL)
  if (!is.null(te_Tv))
    message(sprintf("  TE_Tot_v: media=%.3f sd=%.3f", mean(te_Tv, na.rm=TRUE), sd(te_Tv, na.rm=TRUE)))
}

modelo_I_v <- tryCatch({
  m <- estimar_sfa("ln_i_diag", uhet_Ddesc, df_est_Av, "I_v")
  if (!is.null(m)) {
    saveRDS(m, file.path(INT_DIR, "sfa_mI_Ddesc_definitivo.rds"))
    message("Modelo I_v guardado.")
  }
  m
}, error = function(e) { message("I_v FALLÓ: ", e$message); NULL })

if (!is.null(modelo_I_v)) {
  te_Iv <- tryCatch(sfaR::efficiencies(modelo_I_v)$teJLMS, error = function(e) NULL)
  if (!is.null(te_Iv))
    message(sprintf("  TE_I_v: media=%.3f sd=%.3f", mean(te_Iv, na.rm=TRUE), sd(te_Iv, na.rm=TRUE)))
}

if (!is.null(modelo_Tot_v) && !is.null(modelo_I_v)) {
  cf_T <- coef(modelo_Tot_v)
  se_T <- tryCatch(sqrt(diag(vcov(modelo_Tot_v))), error = function(e) rep(NA, length(cf_T)))
  cf_I <- coef(modelo_I_v)
  se_I <- tryCatch(sqrt(diag(vcov(modelo_I_v))),   error = function(e) rep(NA, length(cf_I)))

  pars_show <- c("D_desc", "pct_sns", "desc_pago", "ShareQ", "desc_shareQ")
  message("\n════════ DISEÑO A VARIANTE — D_desc + cluster ════════")
  message(sprintf("%-15s %8s %6s %8s %6s %7s %6s",
    "Parámetro", "α_Tot", "t_Tot", "δ_I", "t_I", "z", "p"))
  for (p in pars_show) {
    nm_p <- paste0("Zu_", p)
    if (!nm_p %in% names(cf_T)) next
    dif  <- cf_T[nm_p] - cf_I[nm_p]
    se_d <- sqrt(se_T[nm_p]^2 + se_I[nm_p]^2)
    z    <- dif / se_d
    pv   <- 2 * (1 - pnorm(abs(z)))
    pred <- if (p == "D_desc")
      ifelse(cf_T[nm_p] < 0 & cf_I[nm_p] > 0, "V P1+P2",
      ifelse(cf_T[nm_p] < 0, "V P1 solo", "X")) else ""
    message(sprintf("%-15s %8.3f %6.2f %8.3f %6.2f %7.3f %6.4f %s",
      p, cf_T[nm_p], cf_T[nm_p]/se_T[nm_p],
         cf_I[nm_p], cf_I[nm_p]/se_I[nm_p],
         z, pv, pred))
  }
}

# ════════════════════════════════════════════════════════
# DISEÑO D ENRIQUECIDO — ODF + interacción D_desc × ratio
# ════════════════════════════════════════════════════════
message("\n═══ DISEÑO D ENRIQUECIDO — ODF + bias D_desc ═══")

frontier_D_enr <- as.formula(paste(
  "neg_ln_altQ ~",
  "ln_ratio_MQ + I(ln_ratio_MQ^2) +",
  "I(ln_ratio_MQ * ln_L_total_c) +",
  "I(ln_ratio_MQ * ln_K_camas_c) +",
  "I(ln_ratio_MQ * D_desc) +",
  "I(ln_ratio_MQ * pct_sns) +",
  sub("^~", "", frontier_str)))

modelo_D_enr <- tryCatch({
  m <- NULL
  for (cfg in list(
    list(ud = "tnormal", mt = "bfgs", ht = 1L),
    list(ud = "tnormal", mt = "bhhh", ht = 1L),
    list(ud = "hnormal", mt = "bfgs", ht = 1L)
  )) {
    message(sprintf("  [D_enr] %s/%s", cfg$ud, cfg$mt))
    m_try <- NULL
    capture.output({
      m_try <- tryCatch(
        suppressWarnings(sfaR::sfacross(
          frontier_D_enr, uhet = uhet_full,
          data = df_est_D, S = 1L,
          udist = cfg$ud, method = cfg$mt,
          hessianType = cfg$ht)),
        error = function(e) { message("  Error: ", e$message); NULL })
    })
    if (is.null(m_try) || es_degenerado(m_try)) {
      message("  Degenerado o error"); next
    }
    m <- m_try
    message(sprintf("  OK [D_enr]: ll=%.2f [%s/%s]",
                    as.numeric(m$mlLoglik), cfg$ud, cfg$mt))
    break
  }

  if (!is.null(m)) {
    saveRDS(m, file.path(INT_DIR, "sfa_modeloD_enriquecido.rds"))
    message("Modelo D enriquecido guardado.")

    cf2 <- coef(m)
    se2 <- tryCatch(sqrt(diag(vcov(m))), error = function(e) rep(NA, length(cf2)))
    idx_int <- grep("ln_ratio_MQ.*D_desc|ln_ratio_MQ.*pct_sns", names(cf2))
    if (length(idx_int) > 0) {
      message("\nInteracciones ratio x estructura/pago:")
      for (i in idx_int)
        message(sprintf("  %-35s coef=%7.3f t=%6.2f",
          names(cf2)[i], cf2[i], cf2[i]/se2[i]))
      message("\nInterpretacion:")
      message("  coef(ratio*D_desc) < 0: descentralizacion sesga hacia CANTIDAD (confirma P1)")
      message("  coef(ratio*D_desc) > 0: sesga hacia INTENSIDAD")
    }

    ll_orig <- tryCatch(as.numeric(modelo_D$mlLoglik), error = function(e) NA)
    ll_enr  <- as.numeric(m$mlLoglik)
    lr_stat <- 2 * (ll_enr - ll_orig)
    message(sprintf("\nComparacion ODF: ll_orig=%.2f  ll_enr=%.2f", ll_orig, ll_enr))
    message(sprintf("LR test (2*dif): %.2f (2 grados libertad)", lr_stat))
    message(sprintf("p-valor LR: %.4f", 1 - pchisq(lr_stat, df = 2)))
  }
  m
}, error = function(e) { message("D_enr FALLO: ", e$message); NULL })

message("\n=== Test bloques nuevos completado ===")
