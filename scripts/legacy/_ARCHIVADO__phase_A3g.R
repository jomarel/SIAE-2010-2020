## =============================================================
## Design A-3g — 3-group payment specification
## =============================================================
setwd("G:/Mi unidad/SIAE 2010-2020")
library(dplyr)
library(frontier)

CLEAN_DIR <- "data_clean"
load(file.path(CLEAN_DIR, "df_sfa.RData"))

cat("========== Design A-3g: 3-group payment ==========\n")

## --- Build sample A ---
sample_A <- df_sfa %>%
  filter(es_agudo == 1, !anyo %in% 2020:2022, altTotal_bruto >= 200,
         !is.na(D_desc), !is.na(pct_sns), is.finite(ln_altTotal_pond),
         is.finite(ln_i_diag), !is.na(ln_L_total_c), !is.na(ccaa_cod))

# Re-center on sample
sample_A$ln_L_total_raw <- log(pmax(sample_A$L_total, 0) + 1)
sample_A$ln_K_camas_raw <- log(pmax(sample_A$K_camas, 0) + 1)
sample_A$ln_K_tech_raw  <- log(pmax(sample_A$K_tech_index, 0) + 1)
mu_L <- mean(sample_A$ln_L_total_raw, na.rm = TRUE)
mu_K <- mean(sample_A$ln_K_camas_raw, na.rm = TRUE)
mu_T <- mean(sample_A$ln_K_tech_raw, na.rm = TRUE)
sample_A$ln_L_total_c  <- sample_A$ln_L_total_raw - mu_L
sample_A$ln_K_camas_c  <- sample_A$ln_K_camas_raw - mu_K
sample_A$ln_K_tech_c   <- sample_A$ln_K_tech_raw  - mu_T
sample_A$ln_L_total_c2 <- 0.5 * sample_A$ln_L_total_c^2
sample_A$ln_K_camas_c2 <- 0.5 * sample_A$ln_K_camas_c^2

sample_A <- sample_A %>% filter(!is.na(ln_K_tech_c))

# Winsorize intensity
p1  <- quantile(sample_A$ln_i_diag, 0.01, na.rm = TRUE)
p99 <- quantile(sample_A$ln_i_diag, 0.99, na.rm = TRUE)
sample_A$ln_i_diag_w <- pmin(pmax(sample_A$ln_i_diag, p1), p99)

cat("Sample N:", nrow(sample_A), "\n")

## --- Payment group dummies ---
sample_A$d_Priv_Conc <- as.integer(sample_A$D_desc == 1 & sample_A$pct_sns >= 0.50)
sample_A$d_Priv_Merc <- as.integer(sample_A$D_desc == 1 & sample_A$pct_sns < 0.50)
sample_A$Conc_shareQ <- sample_A$d_Priv_Conc * sample_A$ShareQ
sample_A$Merc_shareQ <- sample_A$d_Priv_Merc * sample_A$ShareQ

cat("Group counts:\n")
cat("  Public:    ", sum(sample_A$D_desc == 0), "\n")
cat("  Priv_Conc: ", sum(sample_A$d_Priv_Conc == 1), "\n")
cat("  Priv_Merc: ", sum(sample_A$d_Priv_Merc == 1), "\n")

## --- CCAA dummies >= 30 obs ---
counts <- tapply(rep(1, nrow(sample_A)), sample_A$ccaa_cod, sum)
ccaa_ok <- names(counts[counts >= 30])
ccaa_ok <- setdiff(ccaa_ok, "9")  # exclude ref
ccaa_d <- paste0("d_ccaa_", ccaa_ok)
ccaa_d <- ccaa_d[ccaa_d %in% names(sample_A)]
ccaa_d <- ccaa_d[vapply(ccaa_d, function(v) var(sample_A[[v]]) > 0, logical(1))]
cat("CCAA dummies:", length(ccaa_d), "\n")

## --- Formulas ---
frontier_rhs <- paste(
  "ln_L_total_c + ln_K_camas_c + ln_K_tech_c +",
  "ln_L_total_c2 + ln_K_camas_c2 +",
  "trend + trend2 +",
  "d_cluster2 + d_cluster3 + d_cluster4 + d_cluster5"
)

z_str <- paste("d_Priv_Conc + d_Priv_Merc + Conc_shareQ + Merc_shareQ + ShareQ +",
               paste(ccaa_d, collapse = " + "))

fml_T <- as.formula(paste("ln_altTotal_pond ~", frontier_rhs, "|", z_str))
fml_I <- as.formula(paste("ln_i_diag_w ~", frontier_rhs, "|", z_str))

## --- Estimate Total ---
cat("\n=== m_A3g_Total ===\n")
t0 <- Sys.time()
m_A3g_T <- tryCatch(
  frontier::sfa(fml_T, data = sample_A, ineffDecrease = TRUE, maxit = 5000),
  error = function(e) { cat("ERROR:", conditionMessage(e), "\n"); NULL }
)
t1 <- Sys.time()
cat("Time:", round(difftime(t1, t0, units = "secs"), 1), "sec\n")

if (!is.null(m_A3g_T)) {
  cf_T <- coef(m_A3g_T); se_T <- sqrt(diag(vcov(m_A3g_T)))
  te_T <- efficiencies(m_A3g_T)
  cat(sprintf("logLik=%.1f  N=%d  TE=%.3f  gamma=%.4f\n",
              logLik(m_A3g_T)[[1]], nobs(m_A3g_T), mean(te_T), cf_T["gamma"]))

  z_idx <- grep("^Z_", names(cf_T))
  for (i in z_idx) {
    tstat <- cf_T[i] / se_T[i]
    sig <- ifelse(abs(tstat) >= 2.576, "***",
           ifelse(abs(tstat) >= 1.96, "**",
           ifelse(abs(tstat) >= 1.645, "*", "")))
    cat(sprintf("  %-20s %10.4f  (%.4f)  t=%7.3f %s\n",
                names(cf_T)[i], cf_T[i], se_T[i], tstat, sig))
  }
  saveRDS(m_A3g_T, file.path(CLEAN_DIR, "sfa_A3g_Total.rds"))
  cat("Saved: sfa_A3g_Total.rds\n")
}

## --- Estimate Intensity ---
cat("\n=== m_A3g_I ===\n")
t0 <- Sys.time()
m_A3g_I <- tryCatch(
  frontier::sfa(fml_I, data = sample_A, ineffDecrease = TRUE, maxit = 5000),
  error = function(e) { cat("ERROR:", conditionMessage(e), "\n"); NULL }
)
t1 <- Sys.time()
cat("Time:", round(difftime(t1, t0, units = "secs"), 1), "sec\n")

if (!is.null(m_A3g_I)) {
  cf_I <- coef(m_A3g_I); se_I <- sqrt(diag(vcov(m_A3g_I)))
  te_I <- efficiencies(m_A3g_I)
  cat(sprintf("logLik=%.1f  N=%d  TE=%.3f  gamma=%.4f\n",
              logLik(m_A3g_I)[[1]], nobs(m_A3g_I), mean(te_I), cf_I["gamma"]))

  z_idx <- grep("^Z_", names(cf_I))
  for (i in z_idx) {
    tstat <- cf_I[i] / se_I[i]
    sig <- ifelse(abs(tstat) >= 2.576, "***",
           ifelse(abs(tstat) >= 1.96, "**",
           ifelse(abs(tstat) >= 1.645, "*", "")))
    cat(sprintf("  %-20s %10.4f  (%.4f)  t=%7.3f %s\n",
                names(cf_I)[i], cf_I[i], se_I[i], tstat, sig))
  }
  saveRDS(m_A3g_I, file.path(CLEAN_DIR, "sfa_A3g_I.rds"))
  cat("Saved: sfa_A3g_I.rds\n")
}

## --- Contrast table A-3g ---
cat("\n=== CONTRAST TABLE A-3g (Total vs Intensity) ===\n")

if (!is.null(m_A3g_T) && !is.null(m_A3g_I)) {
  z_vars <- c("d_Priv_Conc", "d_Priv_Merc", "Conc_shareQ", "Merc_shareQ", "ShareQ")

  contrast_rows <- list()
  for (v in z_vars) {
    nm <- paste0("Z_", v)
    idx_T <- which(names(cf_T) == nm)
    idx_I <- which(names(cf_I) == nm)
    if (length(idx_T) == 0 || length(idx_I) == 0) next

    b_T <- cf_T[idx_T]; s_T <- se_T[idx_T]
    b_I <- cf_I[idx_I]; s_I <- se_I[idx_I]
    z_diff <- (b_T - b_I) / sqrt(s_T^2 + s_I^2)
    p_val <- 2 * (1 - pnorm(abs(z_diff)))

    contrast_rows[[v]] <- data.frame(
      variable = v,
      coef_Total = round(b_T, 4),
      se_Total = round(s_T, 4),
      t_Total = round(b_T / s_T, 3),
      coef_Intensity = round(b_I, 4),
      se_Intensity = round(s_I, 4),
      t_Intensity = round(b_I / s_I, 3),
      z_diff = round(z_diff, 3),
      p_value = round(p_val, 4),
      stringsAsFactors = FALSE
    )
  }

  contrast_df <- do.call(rbind, contrast_rows)
  rownames(contrast_df) <- NULL
  print(contrast_df)
  write.csv(contrast_df, file.path(CLEAN_DIR, "tabla_contrastes_A3g.csv"), row.names = FALSE)
  cat("Saved: tabla_contrastes_A3g.csv\n")

  ## --- Key questions ---
  cat("\n=== KEY QUESTIONS ===\n")

  pc_T <- cf_T["Z_d_Priv_Conc"]; pm_T <- cf_T["Z_d_Priv_Merc"]
  pc_I <- cf_I["Z_d_Priv_Conc"]; pm_I <- cf_I["Z_d_Priv_Merc"]
  se_pc_T <- se_T["Z_d_Priv_Conc"]; se_pm_T <- se_T["Z_d_Priv_Merc"]
  se_pc_I <- se_I["Z_d_Priv_Conc"]; se_pm_I <- se_I["Z_d_Priv_Merc"]

  cat(sprintf("Q1: Priv_Merc more negative than Priv_Conc in quantity?\n"))
  cat(sprintf("    Priv_Conc = %.4f (t=%.2f)  Priv_Merc = %.4f (t=%.2f)\n",
              pc_T, pc_T/se_pc_T, pm_T, pm_T/se_pm_T))
  z_q1 <- (pm_T - pc_T) / sqrt(se_pm_T^2 + se_pc_T^2)
  cat(sprintf("    Diff (Merc-Conc) = %.4f  z = %.3f  p = %.4f  -> %s\n",
              pm_T - pc_T, z_q1, 2*(1-pnorm(abs(z_q1))),
              ifelse(pm_T < pc_T && abs(z_q1) > 1.96, "YES", "NO")))

  cat(sprintf("\nQ2: Priv_Merc more positive than Priv_Conc in intensity?\n"))
  cat(sprintf("    Priv_Conc = %.4f (t=%.2f)  Priv_Merc = %.4f (t=%.2f)\n",
              pc_I, pc_I/se_pc_I, pm_I, pm_I/se_pm_I))
  z_q2 <- (pm_I - pc_I) / sqrt(se_pm_I^2 + se_pc_I^2)
  cat(sprintf("    Diff (Merc-Conc) = %.4f  z = %.3f  p = %.4f  -> %s\n",
              pm_I - pc_I, z_q2, 2*(1-pnorm(abs(z_q2))),
              ifelse(pm_I > pc_I && abs(z_q2) > 1.96, "YES", "NO")))

  cat(sprintf("\nQ3: Are Merc vs Conc differences significant?\n"))
  cat(sprintf("    Quantity:  z = %.3f  p = %.4f  -> %s\n",
              z_q1, 2*(1-pnorm(abs(z_q1))),
              ifelse(abs(z_q1) > 1.96, "SIGNIFICANT", "not significant")))
  cat(sprintf("    Intensity: z = %.3f  p = %.4f  -> %s\n",
              z_q2, 2*(1-pnorm(abs(z_q2))),
              ifelse(abs(z_q2) > 1.96, "SIGNIFICANT", "not significant")))
}

cat("\n========== Design A-3g complete ==========\n")
