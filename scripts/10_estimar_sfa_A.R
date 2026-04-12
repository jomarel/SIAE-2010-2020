## =============================================================
## Phase 6 — SFA Estimation: Design A (quantity vs intensity)
## Uses frontier::sfa()
## =============================================================

setwd("G:/Mi unidad/SIAE 2010-2020")
library(dplyr)
library(frontier)

CLEAN_DIR <- "data_clean"
load(file.path(CLEAN_DIR, "df_sfa.RData"))

to_num <- function(x) suppressWarnings(as.numeric(x))

cat("========== Phase 6: SFA Design A ==========\n")
cat("df_sfa:", nrow(df_sfa), "x", ncol(df_sfa), "\n")

## --- Apply Design A filters ---
df_A <- df_sfa %>%
  filter(
    es_agudo == 1,
    !anyo %in% 2020:2022,
    altTotal_bruto >= 200,
    !is.na(D_desc),
    !is.na(pct_sns),
    is.finite(ln_altTotal_pond),
    is.finite(ln_i_diag),
    !is.na(ln_L_total_c),
    !is.na(ln_K_camas_c),
    !is.na(ccaa_cod)
  )

cat("Design A estimation sample:", nrow(df_A), "obs\n")
cat("Years:", sort(unique(df_A$anyo)), "\n")
cat("% private:", round(100 * mean(df_A$D_desc == 1), 1), "%\n")

## --- Re-center log variables on estimation sample ---
cat("\nRe-centering log variables on estimation sample...\n")
mu_L <- mean(df_A$ln_L_total_c + mean(df_sfa$ln_L_total_c, na.rm=TRUE), na.rm = TRUE)
mu_K <- mean(df_A$ln_K_camas_c + mean(df_sfa$ln_K_camas_c, na.rm=TRUE), na.rm = TRUE)
mu_T <- mean(df_A$ln_K_tech_c + mean(df_sfa$ln_K_tech_c, na.rm=TRUE), na.rm = TRUE)

# Actually, just re-center from raw logs
# ln_L_total_c was = ln_L_total - mu_panel. We want = ln_L_total - mu_sample.
# So: ln_L_total = ln_L_total_c + mu_panel
# And new_c = ln_L_total - mu_sample

# Get the raw log values back (using L_total, K_camas from df_A)
df_A$ln_L_total_raw <- log(pmax(df_A$L_total, 0, na.rm = FALSE) + 1)
df_A$ln_K_camas_raw <- log(pmax(df_A$K_camas, 0, na.rm = FALSE) + 1)
df_A$ln_K_tech_raw  <- log(pmax(df_A$K_tech_index, 0, na.rm = FALSE) + 1)

mu_L_s <- mean(df_A$ln_L_total_raw, na.rm = TRUE)
mu_K_s <- mean(df_A$ln_K_camas_raw, na.rm = TRUE)
mu_T_s <- mean(df_A$ln_K_tech_raw, na.rm = TRUE)

df_A$ln_L_total_c  <- df_A$ln_L_total_raw - mu_L_s
df_A$ln_K_camas_c  <- df_A$ln_K_camas_raw - mu_K_s
df_A$ln_K_tech_c   <- df_A$ln_K_tech_raw  - mu_T_s
df_A$ln_L_total_c2 <- 0.5 * df_A$ln_L_total_c^2
df_A$ln_K_camas_c2 <- 0.5 * df_A$ln_K_camas_c^2

cat(sprintf("Sample centers: ln_L=%.4f, ln_K_camas=%.4f, ln_K_tech=%.4f\n",
            mu_L_s, mu_K_s, mu_T_s))

## --- Handle K_tech NAs ---
# K_tech_index has 21% NA. For estimation, we need complete cases.
# Check how many we lose
n_no_tech <- sum(is.na(df_A$ln_K_tech_c))
cat("Obs missing K_tech:", n_no_tech, "out of", nrow(df_A), "\n")

# For Total model: keep only complete cases
df_A_Total <- df_A %>% filter(!is.na(ln_K_tech_c))
cat("df_A_Total (complete cases):", nrow(df_A_Total), "obs\n")

# For I model: same sample
df_A_I <- df_A_Total

## --- Verify CCAA dummies ---
# Check which CCAA codes are in the sample
ccaa_present <- sort(unique(df_A_Total$ccaa_cod))
cat("CCAA codes in sample:", ccaa_present, "\n")

# Build CCAA dummies for codes present (ref: 9 = Cataluña)
ccaa_for_dummies <- setdiff(ccaa_present, 9)
for (cc in ccaa_for_dummies) {
  vname <- paste0("d_ccaa_", cc)
  df_A_Total[[vname]] <- as.integer(df_A_Total$ccaa_cod == cc)
  df_A_I[[vname]] <- as.integer(df_A_I$ccaa_cod == cc)
}

# Verify no constant dummies (would cause estimation failure)
ccaa_dummy_names <- paste0("d_ccaa_", ccaa_for_dummies)
for (v in ccaa_dummy_names) {
  if (var(df_A_Total[[v]]) == 0) {
    cat("  WARNING: dropping constant dummy", v, "\n")
    ccaa_dummy_names <- setdiff(ccaa_dummy_names, v)
  }
}
cat("CCAA dummies for model:", paste(ccaa_dummy_names, collapse=", "), "\n")
# Varianza de los inputs re-centrados
cat("Var ln_L_total_c:", var(df_A_I$ln_L_total_c, na.rm=TRUE), "\n")
cat("Var ln_K_camas_c:", var(df_A_I$ln_K_camas_c, na.rm=TRUE), "\n")
cat("Var ln_K_tech_c: ", var(df_A_I$ln_K_tech_c,  na.rm=TRUE), "\n")

# Correlación entre trend y clusters
cat("Cor trend-cluster2:", cor(df_A_I$trend, df_A_I$d_cluster2, use="complete"), "\n")

# Rango de la matriz de diseño de la frontera
X <- model.matrix(~ ln_L_total_c + ln_K_camas_c + ln_K_tech_c +
                    ln_L_total_c2 + ln_K_camas_c2 + trend + trend2 +
                    d_cluster2 + d_cluster3 + d_cluster4 + d_cluster5,
                  data = df_A_I)
cat("Rango matriz frontera:", Matrix::rankMatrix(X)[1], "de", ncol(X), "\n")
## --- Build formulas ---
frontier_rhs <- paste(c(
  "ln_L_total_c", "ln_K_camas_c", "ln_K_tech_c",
  "ln_L_total_c2", "ln_K_camas_c2",
  "trend", "trend2",
  "d_cluster2", "d_cluster3", "d_cluster4", "d_cluster5"
), collapse = " + ")

z_rhs <- paste(c(
  "D_desc", "pct_sns", "desc_pago", "ShareQ", "desc_shareQ",
  ccaa_dummy_names
), collapse = " + ")

formula_Total <- as.formula(paste("ln_altTotal_pond ~", frontier_rhs, "|", z_rhs))
formula_I     <- as.formula(paste("ln_i_diag ~", frontier_rhs, "|", z_rhs))

cat("\nFormula Total:", deparse(formula_Total, width.cutoff = 200), "\n")
cat("Formula I:", deparse(formula_I, width.cutoff = 200), "\n")

## --- Estimate m_A_Total ---
cat("\n=== Estimating m_A_Total ===\n")
t0 <- Sys.time()
m_A_Total <- tryCatch(
  frontier::sfa(formula_Total, data = df_A_Total,
                ineffDecrease = TRUE, maxit = 2000),
  error = function(e) {
    cat("ERROR:", conditionMessage(e), "\n")
    NULL
  }
)
t1 <- Sys.time()
cat("Time:", round(difftime(t1, t0, units = "secs"), 1), "sec\n")

if (!is.null(m_A_Total)) {
  cat("\n--- m_A_Total results ---\n")
  cat("logLik:", logLik(m_A_Total), "\n")
  cat("N:", nobs(m_A_Total), "\n")

  te_Total <- efficiencies(m_A_Total)
  cat("TE mean:", round(mean(te_Total), 4), "\n")
  cat("TE median:", round(median(te_Total), 4), "\n")

  # D_desc coefficient from z-equation
  coefs <- coef(m_A_Total)
  se <- sqrt(diag(vcov(m_A_Total)))
  z_names <- names(coefs)

  d_desc_idx <- grep("D_desc", z_names)
  if (length(d_desc_idx) > 0) {
    for (idx in d_desc_idx) {
      tstat <- coefs[idx] / se[idx]
      cat(sprintf("  %s: coef=%.4f, se=%.4f, t=%.3f, sign=%s\n",
                  z_names[idx], coefs[idx], se[idx], tstat,
                  ifelse(coefs[idx] < 0, "NEGATIVE (reduces inefficiency)", "POSITIVE")))
    }
  }

  cat("\nFull coefficient table:\n")
  coef_df <- data.frame(
    param = z_names,
    coef = round(coefs, 5),
    se = round(se, 5),
    t_stat = round(coefs / se, 3)
  )
  print(coef_df)

  saveRDS(m_A_Total, file.path(CLEAN_DIR, "sfa_A_Total.rds"))
  cat("Saved:", file.path(CLEAN_DIR, "sfa_A_Total.rds"), "\n")
} else {
  cat("m_A_Total estimation FAILED\n")
}

## --- Estimate m_A_I ---
cat("\n=== Estimating m_A_I ===\n")
t0 <- Sys.time()
m_A_I <- tryCatch(
  frontier::sfa(formula_I, data = df_A_I,
                ineffDecrease = TRUE, maxit = 2000),
  error = function(e) {
    cat("ERROR:", conditionMessage(e), "\n")
    NULL
  }
)
t1 <- Sys.time()
cat("Time:", round(difftime(t1, t0, units = "secs"), 1), "sec\n")

if (!is.null(m_A_I)) {
  cat("\n--- m_A_I results ---\n")
  cat("logLik:", logLik(m_A_I), "\n")
  cat("N:", nobs(m_A_I), "\n")

  te_I <- efficiencies(m_A_I)
  cat("TE mean:", round(mean(te_I), 4), "\n")
  cat("TE median:", round(median(te_I), 4), "\n")

  coefs <- coef(m_A_I)
  se <- sqrt(diag(vcov(m_A_I)))
  z_names <- names(coefs)

  d_desc_idx <- grep("D_desc", z_names)
  if (length(d_desc_idx) > 0) {
    for (idx in d_desc_idx) {
      tstat <- coefs[idx] / se[idx]
      cat(sprintf("  %s: coef=%.4f, se=%.4f, t=%.3f, sign=%s\n",
                  z_names[idx], coefs[idx], se[idx], tstat,
                  ifelse(coefs[idx] < 0, "NEGATIVE (reduces inefficiency)", "POSITIVE")))
    }
  }

  cat("\nFull coefficient table:\n")
  coef_df <- data.frame(
    param = z_names,
    coef = round(coefs, 5),
    se = round(se, 5),
    t_stat = round(coefs / se, 3)
  )
  print(coef_df)

  saveRDS(m_A_I, file.path(CLEAN_DIR, "sfa_A_I.rds"))
  cat("Saved:", file.path(CLEAN_DIR, "sfa_A_I.rds"), "\n")
} else {
  cat("m_A_I estimation FAILED\n")
}

cat("\n========== Phase 6 complete ==========\n")
