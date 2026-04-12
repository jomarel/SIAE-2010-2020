# ============================================================
# 11_estimar_sfa.R — Main SFA estimation (frontier::sfa)
# Battese-Coelli 1995 efficiency effects model.
# Formula syntax: y ~ frontier_vars | z_vars
#   where z_vars enter the mean of the truncated normal u_it.
#
# Four estimation designs:
#   A: Quantity (ln_altTotal_pond) vs Intensity (ln_i_diag)
#   B: Surgical (ln_altQ_pond) vs Medical (ln_altM_pond)
#   C: Panel hospital × service (long format)
#   D: Output Distance Function (-ln_altQ ~ ratio + inputs)
#
# Outputs:
#   data_intermediate/sfa_modelo*_disenio*.rds
#   data_intermediate/sfa_eficiencias_disenio*.csv
#   data_intermediate/df_estimacion_canonical_tesis.RData
#   data_intermediate/muestra_canonical_tesis_NCODI_anyo.csv
# ============================================================

config_path <- if (file.exists("scripts/00_config.R")) "scripts/00_config.R" else "00_config.R"
source(config_path)

library(frontier)
library(dplyr)
set.seed(42)

message("\n=== Script 11: SFA estimation (frontier::sfa, BC95) ===")

load(file.path(INT_DIR, "df_sfa.RData"))
message("df_sfa: ", nrow(df_sfa), " x ", ncol(df_sfa))

# ================================================================
# HELPER FUNCTIONS
# ================================================================

# Safe SFA estimation with convergence strategy
# frontier::sfa() uses internal restarts; we vary truncNorm/startVal
safe_sfa <- function(formula, data, label,
                     ineffDecrease = TRUE, maxit = 2000) {
  message(sprintf("\n  Estimating: %s (%d obs)", label, nrow(data)))

  attempts <- list(
    list(truncNorm = TRUE,  restartMax = 20, nRestart = 5),
    list(truncNorm = TRUE,  restartMax = 50, nRestart = 10),
    list(truncNorm = FALSE, restartMax = 20, nRestart = 5)
  )

  for (i in seq_along(attempts)) {
    cfg <- attempts[[i]]
    m <- tryCatch({
      frontier::sfa(
        formula    = formula,
        data       = data,
        ineffDecrease = ineffDecrease,
        truncNorm  = cfg$truncNorm,
        maxit      = maxit,
        restartMax = cfg$restartMax,
        nRestart   = cfg$nRestart,
        printIter  = 0
      )
    }, error = function(e) {
      message(sprintf("    Attempt %d (truncNorm=%s) failed: %s",
                      i, cfg$truncNorm, e$message))
      NULL
    })

    if (!is.null(m)) {
      ll <- tryCatch(as.numeric(logLik(m)), error = function(e) NA)
      coefs <- tryCatch(coef(m), error = function(e) NULL)

      # SE sanity check
      se_ok <- tryCatch({
        s <- summary(m)
        all(is.finite(s$mleParam[, "Std. Error"]))
      }, error = function(e) FALSE)

      # TE sanity: mean should be in (0.05, 1.0]
      te_ok <- tryCatch({
        te <- efficiencies(m)
        if (is.matrix(te) || is.data.frame(te)) te <- te[, 1]
        mte <- mean(as.numeric(te), na.rm = TRUE)
        is.finite(mte) && mte > 0.05 && mte <= 1.0
      }, error = function(e) FALSE)

      is_degen <- !isTRUE(is.finite(ll)) || abs(ll) > 1e6 ||
                  is.null(coefs) || any(abs(coefs) > 100) ||
                  !isTRUE(se_ok) || !isTRUE(te_ok)

      if (!is_degen) {
        message(sprintf("    Converged (attempt %d, truncNorm=%s): logLik=%.1f",
                        i, cfg$truncNorm, ll))
        return(m)
      } else {
        message(sprintf("    Attempt %d (truncNorm=%s): degenerate (logLik=%s, se_ok=%s, te_ok=%s)",
                        i, cfg$truncNorm,
                        ifelse(is.finite(ll), round(ll, 1), "NA"), se_ok, te_ok))
      }
    }
  }

  msg <- sprintf("CONVERGENCE FAILURE: %s after %d attempts", label, length(attempts))
  message(paste("  ***", msg))
  cat(msg, "\n", file = file.path(INT_DIR, "sfa_errores_convergencia.txt"), append = TRUE)
  return(NULL)
}

# Extract TE vector from frontier model
safe_eff <- function(m) {
  te <- efficiencies(m)
  if (is.matrix(te) || is.data.frame(te)) te <- te[, 1]
  as.numeric(te)
}

# Extract coefficient table from frontier model
coef_table <- function(m, label) {
  if (is.null(m)) return(NULL)
  s <- tryCatch(summary(m), error = function(e) NULL)
  if (is.null(s)) return(NULL)
  cf <- s$mleParam
  if (is.null(cf) || nrow(cf) == 0) return(NULL)
  data.frame(
    model = label,
    param = rownames(cf),
    coef  = cf[, "Estimate"],
    se    = cf[, "Std. Error"],
    z     = cf[, "z value"],
    p     = cf[, "Pr(>|z|)"],
    stringsAsFactors = FALSE, row.names = NULL
  )
}

# ================================================================
# VARIABLES AND FORMULAS
# ================================================================

ccaa_dummies <- paste0("d_ccaa_", setdiff(1:17, 9))
ccaa_in_data <- intersect(ccaa_dummies, names(df_sfa))

frontier_rhs <- paste(c(
  "ln_L_total_c", "ln_K_camas_c", "ln_K_tech_c",
  "ln_L_total_c2", "ln_K_camas_c2",
  "trend", "trend2",
  "d_cluster2", "d_cluster3", "d_cluster4", "d_cluster5"
), collapse = " + ")

# Inefficiency determinants — Design A (dichotomous D_desc)
ineff_A_full <- paste(c(
  "D_desc", "pct_sns", "desc_pago", "ShareQ", "desc_shareQ",
  ccaa_in_data
), collapse = " + ")

ineff_A_restr <- paste(c(
  "D_desc", "pct_sns", "desc_pago",
  ccaa_in_data
), collapse = " + ")

# Inefficiency determinants — Designs B/D (3-group)
ineff_3grp <- paste(c(
  "d_Priv_Conc", "d_Priv_Merc", "Conc_shareQ", "Merc_shareQ", "ShareQ",
  ccaa_in_data
), collapse = " + ")

# ================================================================
# DESIGN A — Quantity vs Intensity
# Sample: agudos, no COVID, altTotal_bruto >= 200
# ================================================================

message("\n========== DESIGN A: Quantity vs Intensity ==========")

df_est_A <- df_sfa %>%
  filter(
    es_agudo == 1,
    !anyo %in% 2020:2022,
    altTotal_bruto >= 200,
    !is.na(D_desc), !is.na(pct_sns), !is.na(ShareQ),
    !is.na(ln_L_total_c), !is.na(ln_K_camas_c), !is.na(ln_K_tech_c),
    !is.na(ccaa_cod),
    !is.na(ln_i_diag), is.finite(ln_i_diag),
    !is.na(ln_altTotal_pond), is.finite(ln_altTotal_pond)
  ) %>%
  arrange(NCODI, anyo)

message(sprintf("Design A sample: %d obs, %d hospitals",
                nrow(df_est_A), n_distinct(df_est_A$NCODI)))
message(sprintf("  D_desc=0: %d | D_desc=1: %d",
                sum(df_est_A$D_desc == 0), sum(df_est_A$D_desc == 1)))

# ── Save canonical sample ──────────────────────────────────────
canonical_path_rdata <- file.path(INT_DIR, "df_estimacion_canonical_tesis.RData")
canonical_path_csv   <- file.path(INT_DIR, "muestra_canonical_tesis_NCODI_anyo.csv")

df_estimacion_canonical <- df_est_A
save(df_estimacion_canonical, file = canonical_path_rdata)
write.csv(
  df_est_A %>% select(NCODI, anyo, D_desc, D_desc_fuente_tesis) %>% arrange(NCODI, anyo),
  canonical_path_csv, row.names = FALSE
)
message(sprintf("  Canonical sample saved: %s (%d obs)", canonical_path_rdata, nrow(df_est_A)))

# ── A_Total: production frontier ───────────────────────────────
f_A_Total <- as.formula(paste("ln_altTotal_pond ~", frontier_rhs, "|", ineff_A_full))
m_A_Total <- safe_sfa(f_A_Total, df_est_A, "A_Total")

# ── A_I: intensity frontier ────────────────────────────────────
f_A_I <- as.formula(paste("ln_i_diag ~", frontier_rhs, "|", ineff_A_full))
m_A_I <- safe_sfa(f_A_I, df_est_A, "A_I")

# Save models
if (!is.null(m_A_Total)) saveRDS(m_A_Total, file.path(INT_DIR, "sfa_modeloTotal_disenioA.rds"))
if (!is.null(m_A_I))     saveRDS(m_A_I,     file.path(INT_DIR, "sfa_modeloI_disenioA.rds"))

# TE diagnostics
if (!is.null(m_A_Total)) {
  TE_Total <- safe_eff(m_A_Total)
  message(sprintf("  TE_Total mean: %.3f | logLik: %.1f",
                  mean(TE_Total), as.numeric(logLik(m_A_Total))))
}
if (!is.null(m_A_I)) {
  TE_I <- safe_eff(m_A_I)
  message(sprintf("  TE_I mean: %.3f | logLik: %.1f",
                  mean(TE_I), as.numeric(logLik(m_A_I))))
}
if (!is.null(m_A_Total) && !is.null(m_A_I)) {
  cor_p <- cor(TE_Total, TE_I, use = "complete.obs")
  cor_s <- cor(TE_Total, TE_I, use = "complete.obs", method = "spearman")
  message(sprintf("  TE correlation: Pearson=%.3f, Spearman=%.3f", cor_p, cor_s))
}

# ================================================================
# DESIGN A1 — Restricted (no ShareQ interactions)
# ================================================================

message("\n========== DESIGN A1: Restricted (no ShareQ) ==========")

f_A1_Total <- as.formula(paste("ln_altTotal_pond ~", frontier_rhs, "|", ineff_A_restr))
f_A1_I     <- as.formula(paste("ln_i_diag ~",       frontier_rhs, "|", ineff_A_restr))

m_A1_Total <- safe_sfa(f_A1_Total, df_est_A, "A1_Total")
m_A1_I     <- safe_sfa(f_A1_I,     df_est_A, "A1_I")

if (!is.null(m_A1_Total)) saveRDS(m_A1_Total, file.path(INT_DIR, "sfa_modeloTotal_disenioA1.rds"))
if (!is.null(m_A1_I))     saveRDS(m_A1_I,     file.path(INT_DIR, "sfa_modeloI_disenioA1.rds"))

if (!is.null(m_A1_Total)) {
  message(sprintf("  TE_Total_A1 mean: %.3f | logLik: %.1f",
                  mean(safe_eff(m_A1_Total)), as.numeric(logLik(m_A1_Total))))
}
if (!is.null(m_A1_I)) {
  message(sprintf("  TE_I_A1 mean: %.3f | logLik: %.1f",
                  mean(safe_eff(m_A1_I)), as.numeric(logLik(m_A1_I))))
}

# ================================================================
# DESIGN B — Surgical vs Medical (3-group pago)
# Additional filter: altQ_bruto >= 200, altM_bruto >= 200
# ================================================================

message("\n========== DESIGN B: Surgical vs Medical ==========")

df_est_BD <- df_sfa %>%
  filter(
    es_agudo == 1,
    !anyo %in% 2020:2022,
    altTotal_bruto >= 200,
    !is.na(grupo_pago),
    !is.na(ln_L_total_c), !is.na(ln_K_camas_c), !is.na(ln_K_tech_c),
    !is.na(ccaa_cod), !is.na(ShareQ),
    altQ_bruto >= 200, altM_bruto >= 200,
    is.finite(ln_altQ_pond), is.finite(ln_altM_pond)
  ) %>%
  arrange(NCODI, anyo)

message(sprintf("Design B/D sample: %d obs, %d hospitals",
                nrow(df_est_BD), n_distinct(df_est_BD$NCODI)))

f_B_Q <- as.formula(paste("ln_altQ_pond ~", frontier_rhs, "|", ineff_3grp))
f_B_M <- as.formula(paste("ln_altM_pond ~", frontier_rhs, "|", ineff_3grp))

m_B_Q <- safe_sfa(f_B_Q, df_est_BD, "B_Q")
m_B_M <- safe_sfa(f_B_M, df_est_BD, "B_M")

if (!is.null(m_B_Q)) saveRDS(m_B_Q, file.path(INT_DIR, "sfa_modeloQ_disenioB.rds"))
if (!is.null(m_B_M)) saveRDS(m_B_M, file.path(INT_DIR, "sfa_modeloM_disenioB.rds"))

if (!is.null(m_B_Q) && !is.null(m_B_M)) {
  TE_Q <- safe_eff(m_B_Q)
  TE_M <- safe_eff(m_B_M)
  message(sprintf("  TE_Q mean: %.3f | TE_M mean: %.3f", mean(TE_Q), mean(TE_M)))
  cor_p_B <- cor(TE_Q, TE_M, use = "complete.obs")
  cor_s_B <- cor(TE_Q, TE_M, use = "complete.obs", method = "spearman")
  message(sprintf("  TE corr B: Pearson=%.3f, Spearman=%.3f", cor_p_B, cor_s_B))

  # Coefficient contrast Q vs M
  ct_Q <- coef_table(m_B_Q, "B_Q")
  ct_M <- coef_table(m_B_M, "B_M")
  if (!is.null(ct_Q) && !is.null(ct_M)) {
    merged <- merge(ct_Q, ct_M, by = "param", suffixes = c("_Q", "_M"))
    merged$z_diff <- (merged$coef_Q - merged$coef_M) /
                     sqrt(merged$se_Q^2 + merged$se_M^2)
    merged$p_diff <- 2 * pnorm(-abs(merged$z_diff))
    write.csv(merged, file.path(INT_DIR, "sfa_contrastes_disenioB.csv"),
              row.names = FALSE)
    message("  Saved: sfa_contrastes_disenioB.csv")
  }
}

# ================================================================
# DESIGN D — Output Distance Function (ODF)
# -ln_altQ_pond ~ ln_ratioMQ + inputs | z_vars
# ================================================================

message("\n========== DESIGN D: ODF ==========")

df_D <- df_est_BD %>%
  mutate(
    neg_ln_altQ = -ln_altQ_pond,
    ln_ratioMQ  = ln_altM_pond - ln_altQ_pond
  ) %>%
  arrange(NCODI, anyo)

frontier_D_rhs <- paste(c(
  "ln_ratioMQ",
  "ln_L_total_c", "ln_K_camas_c", "ln_K_tech_c",
  "ln_L_total_c2", "ln_K_camas_c2",
  "trend", "trend2",
  "d_cluster2", "d_cluster3", "d_cluster4", "d_cluster5"
), collapse = " + ")

f_D <- as.formula(paste("neg_ln_altQ ~", frontier_D_rhs, "|", ineff_3grp))
m_D <- safe_sfa(f_D, df_D, "D_ODF")
if (!is.null(m_D)) {
  saveRDS(m_D, file.path(INT_DIR, "sfa_modeloD_odf.rds"))
  message(sprintf("  TE_D mean: %.3f | logLik: %.1f",
                  mean(safe_eff(m_D)), as.numeric(logLik(m_D))))
}

# ================================================================
# DESIGN C — Panel hospital × service (long format)
# Stack Q and M with service dummy C_s (1=surgical, 0=medical)
# ================================================================

message("\n========== DESIGN C: Panel hospital x service ==========")

df_C_long <- bind_rows(
  df_est_BD %>% mutate(dep_val = ln_altQ_pond, C_s = 1L),
  df_est_BD %>% mutate(dep_val = ln_altM_pond, C_s = 0L)
) %>%
  mutate(
    Priv_Conc_Cs = d_Priv_Conc * C_s,
    Priv_Merc_Cs = d_Priv_Merc * C_s,
    ShareQ_Cs    = ShareQ * C_s
  ) %>%
  arrange(NCODI, anyo, C_s)

message(sprintf("  Design C long: %d obs", nrow(df_C_long)))

frontier_C_rhs <- paste(c(
  "C_s",
  "ln_L_total_c", "ln_K_camas_c", "ln_K_tech_c",
  "ln_L_total_c2", "ln_K_camas_c2",
  "trend", "trend2",
  "d_cluster2", "d_cluster3", "d_cluster4", "d_cluster5"
), collapse = " + ")

ineff_C <- paste(c(
  "d_Priv_Conc", "d_Priv_Merc", "C_s",
  "Priv_Conc_Cs", "Priv_Merc_Cs",
  "ShareQ", "ShareQ_Cs",
  ccaa_in_data
), collapse = " + ")

f_C <- as.formula(paste("dep_val ~", frontier_C_rhs, "|", ineff_C))
m_C <- safe_sfa(f_C, df_C_long, "C_panel")
if (!is.null(m_C)) {
  saveRDS(m_C, file.path(INT_DIR, "sfa_modeloC_panel.rds"))
  message(sprintf("  TE_C mean: %.3f | logLik: %.1f",
                  mean(safe_eff(m_C)), as.numeric(logLik(m_C))))
}

# ================================================================
# EFFICIENCY EXPORTS
# ================================================================

message("\n========== Efficiency exports ==========")

if (!is.null(m_A_Total) && !is.null(m_A_I)) {
  write.csv(data.frame(
    NCODI = df_est_A$NCODI, anyo = df_est_A$anyo,
    D_desc = df_est_A$D_desc, pct_sns = df_est_A$pct_sns, ShareQ = df_est_A$ShareQ,
    TE_Total = safe_eff(m_A_Total), TE_I = safe_eff(m_A_I)
  ), file.path(INT_DIR, "sfa_eficiencias_disenioA.csv"), row.names = FALSE)
  message("  Saved: sfa_eficiencias_disenioA.csv")
}

if (!is.null(m_A1_Total) && !is.null(m_A1_I)) {
  write.csv(data.frame(
    NCODI = df_est_A$NCODI, anyo = df_est_A$anyo,
    D_desc = df_est_A$D_desc, pct_sns = df_est_A$pct_sns,
    TE_Total_A1 = safe_eff(m_A1_Total), TE_I_A1 = safe_eff(m_A1_I)
  ), file.path(INT_DIR, "sfa_eficiencias_disenioA1.csv"), row.names = FALSE)
  message("  Saved: sfa_eficiencias_disenioA1.csv")
}

if (!is.null(m_B_Q) && !is.null(m_B_M)) {
  write.csv(data.frame(
    NCODI = df_est_BD$NCODI, anyo = df_est_BD$anyo,
    grupo_pago = df_est_BD$grupo_pago, pct_sns = df_est_BD$pct_sns,
    ShareQ = df_est_BD$ShareQ,
    TE_Q = safe_eff(m_B_Q), TE_M = safe_eff(m_B_M)
  ), file.path(INT_DIR, "sfa_eficiencias_disenioB.csv"), row.names = FALSE)
  message("  Saved: sfa_eficiencias_disenioB.csv")
}

if (!is.null(m_D)) {
  write.csv(data.frame(
    NCODI = df_D$NCODI, anyo = df_D$anyo,
    grupo_pago = df_D$grupo_pago, pct_sns = df_D$pct_sns,
    ShareQ = df_D$ShareQ,
    TE_D = safe_eff(m_D)
  ), file.path(INT_DIR, "sfa_eficiencias_disenioD.csv"), row.names = FALSE)
  message("  Saved: sfa_eficiencias_disenioD.csv")
}

if (!is.null(m_C)) {
  write.csv(data.frame(
    NCODI = df_C_long$NCODI, anyo = df_C_long$anyo,
    C_s = df_C_long$C_s, grupo_pago = df_C_long$grupo_pago,
    TE_C = safe_eff(m_C)
  ), file.path(INT_DIR, "sfa_eficiencias_disenioC.csv"), row.names = FALSE)
  message("  Saved: sfa_eficiencias_disenioC.csv")
}

# ================================================================
# SUMMARY TABLE
# ================================================================

message("\n========== Summary ==========")

models <- list(
  A_Total = m_A_Total, A_I = m_A_I,
  A1_Total = m_A1_Total, A1_I = m_A1_I,
  B_Q = m_B_Q, B_M = m_B_M,
  C_panel = m_C, D_ODF = m_D
)

summary_rows <- lapply(names(models), function(nm) {
  m <- models[[nm]]
  if (is.null(m)) return(data.frame(model = nm, N = NA, logLik = NA,
                                     TE_mean = NA, TE_sd = NA, status = "FAILED"))
  te <- safe_eff(m)
  data.frame(
    model  = nm,
    N      = nobs(m),
    logLik = round(as.numeric(logLik(m)), 1),
    TE_mean = round(mean(te, na.rm = TRUE), 3),
    TE_sd   = round(sd(te, na.rm = TRUE), 3),
    status  = "OK",
    stringsAsFactors = FALSE
  )
})

summary_df <- do.call(rbind, summary_rows)
print(summary_df)
write.csv(summary_df, file.path(INT_DIR, "sfa_resumen_modelos.csv"), row.names = FALSE)

message("\n=== Script 11 complete ===")
