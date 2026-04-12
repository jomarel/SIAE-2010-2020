## =============================================================
## Phase 6b — Re-estimate intensity model (ln_i_diag)
## =============================================================

setwd("G:/Mi unidad/SIAE 2010-2020")
library(dplyr)
library(frontier)

CLEAN_DIR <- "data_clean"
load(file.path(CLEAN_DIR, "df_sfa.RData"))
to_num <- function(x) suppressWarnings(as.numeric(x))

cat("========== Phase 6b: Intensity model re-estimation ==========\n")

## --- Step 1: Check sample and i_diag distribution ---
cat("\n=== STEP 1: Sample check ===\n")

df_A_I <- df_sfa %>%
  filter(
    es_agudo == 1,
    !anyo %in% 2020:2022,
    altTotal_bruto >= 200,
    !is.na(D_desc),
    !is.na(pct_sns),
    is.finite(ln_altTotal_pond),
    is.finite(ln_i_diag),
    !is.na(ln_L_total_c),
    !is.na(ccaa_cod)
  )

cat("n =", nrow(df_A_I), "\n")
cat("ln_i_diag: mean =", round(mean(df_A_I$ln_i_diag), 4),
    " sd =", round(sd(df_A_I$ln_i_diag), 4), "\n")
cat("i_diag: median by year:\n")
print(round(tapply(df_A_I$i_diag, df_A_I$anyo, median, na.rm = TRUE), 2))

# Re-center on this sample
df_A_I$ln_L_total_raw <- log(pmax(df_A_I$L_total, 0) + 1)
df_A_I$ln_K_camas_raw <- log(pmax(df_A_I$K_camas, 0) + 1)
df_A_I$ln_K_tech_raw  <- log(pmax(df_A_I$K_tech_index, 0) + 1)

mu_L <- mean(df_A_I$ln_L_total_raw, na.rm = TRUE)
mu_K <- mean(df_A_I$ln_K_camas_raw, na.rm = TRUE)
mu_T <- mean(df_A_I$ln_K_tech_raw, na.rm = TRUE)

df_A_I$ln_L_total_c  <- df_A_I$ln_L_total_raw - mu_L
df_A_I$ln_K_camas_c  <- df_A_I$ln_K_camas_raw - mu_K
df_A_I$ln_K_tech_c   <- df_A_I$ln_K_tech_raw  - mu_T
df_A_I$ln_L_total_c2 <- 0.5 * df_A_I$ln_L_total_c^2
df_A_I$ln_K_camas_c2 <- 0.5 * df_A_I$ln_K_camas_c^2

cat(sprintf("Sample centers: ln_L=%.4f, ln_K=%.4f, ln_T=%.4f\n", mu_L, mu_K, mu_T))

# Drop obs with NA in K_tech
n_before <- nrow(df_A_I)
df_A_I <- df_A_I %>% filter(!is.na(ln_K_tech_c))
cat("After dropping K_tech NA:", nrow(df_A_I), "(dropped", n_before - nrow(df_A_I), ")\n")

# OLS skewness check
if (!requireNamespace("moments", quietly = TRUE)) install.packages("moments")
library(moments)

ols_I <- lm(ln_i_diag ~ ln_L_total_c + ln_K_camas_c + ln_K_tech_c +
              ln_L_total_c2 + ln_K_camas_c2 + trend + trend2 +
              d_cluster2 + d_cluster3 + d_cluster4 + d_cluster5,
            data = df_A_I)

skew_val <- moments::skewness(residuals(ols_I))
cat("\nOLS residual skewness:", round(skew_val, 4), "\n")
cat("(must be NEGATIVE for production SFA to identify inefficiency)\n")
cat("OLS R²:", round(summary(ols_I)$r.squared, 4), "\n")

## --- Step 2: Keep CCAA with >= 30 obs ---
cat("\n=== STEP 2: CCAA filtering ===\n")

ccaa_counts <- df_A_I %>%
  group_by(ccaa_cod) %>%
  summarise(n = n(), .groups = "drop") %>%
  filter(n >= 30)

ccaa_keep <- ccaa_counts$ccaa_cod
ccaa_keep <- ccaa_keep[ccaa_keep != 9]  # exclude ref = Cataluña

ccaa_dummies_keep <- paste0("d_ccaa_", ccaa_keep)
ccaa_dummies_keep <- ccaa_dummies_keep[ccaa_dummies_keep %in% names(df_A_I)]

cat("CCAA dummies kept:", length(ccaa_dummies_keep), "\n")
cat(" ", paste(ccaa_dummies_keep, collapse = ", "), "\n")

# Verify no constant dummies
for (v in ccaa_dummies_keep) {
  if (var(df_A_I[[v]]) == 0) {
    cat("  Dropping constant dummy:", v, "\n")
    ccaa_dummies_keep <- setdiff(ccaa_dummies_keep, v)
  }
}

## --- Step 3: Estimate with frontier::sfa() ---
cat("\n=== STEP 3: SFA estimation (translog) ===\n")

fml_I <- as.formula(paste(
  "ln_i_diag ~",
  "ln_L_total_c + ln_K_camas_c + ln_K_tech_c +",
  "ln_L_total_c2 + ln_K_camas_c2 + trend + trend2 +",
  "d_cluster2 + d_cluster3 + d_cluster4 + d_cluster5 |",
  "D_desc + pct_sns + desc_pago + ShareQ + desc_shareQ +",
  paste(ccaa_dummies_keep, collapse = " + ")
))

cat("Formula:", deparse(fml_I, width.cutoff = 200), "\n\n")

t0 <- Sys.time()
m_I <- tryCatch(
  frontier::sfa(fml_I, data = df_A_I,
                ineffDecrease = TRUE, maxit = 5000),
  error = function(e) { cat("ERROR:", conditionMessage(e), "\n"); NULL }
)
t1 <- Sys.time()
cat("Time:", round(difftime(t1, t0, units = "secs"), 1), "sec\n")

gamma_boundary <- FALSE

if (!is.null(m_I)) {
  cat("\n--- m_I (translog) results ---\n")
  cat("logLik:", logLik(m_I)[[1]], "\n")
  cat("N:", nobs(m_I), "\n")
  te_I <- efficiencies(m_I)
  cat("TE mean:", round(mean(te_I), 4), "\n")

  coefs_all <- coef(m_I)
  gamma_val <- coefs_all["gamma"]
  cat("gamma:", round(gamma_val, 6), "\n")

  if (gamma_val >= 0.999) {
    cat("*** gamma at boundary — results unreliable ***\n")
    gamma_boundary <- TRUE
  }

  sm_I <- summary(m_I)
  print(sm_I)

  saveRDS(m_I, file.path(CLEAN_DIR, "sfa_A_I_v2.rds"))
  cat("Saved: data_clean/sfa_A_I_v2.rds\n")
} else {
  gamma_boundary <- TRUE
}

## --- Step 4: Fallback if gamma at boundary ---
if (gamma_boundary) {
  cat("\n=== STEP 4: Fallback strategies ===\n")
  cat("OLS residual skewness was:", round(skew_val, 4), "\n")

  if (skew_val > 0) {
    cat("\nSkewness is POSITIVE — trying COST frontier (ineffDecrease=FALSE)\n")

    t0 <- Sys.time()
    m_I_cost <- tryCatch(
      frontier::sfa(fml_I, data = df_A_I,
                    ineffDecrease = FALSE, maxit = 5000),
      error = function(e) { cat("ERROR:", conditionMessage(e), "\n"); NULL }
    )
    t1 <- Sys.time()
    cat("Time:", round(difftime(t1, t0, units = "secs"), 1), "sec\n")

    if (!is.null(m_I_cost)) {
      cat("\n--- m_I_cost results ---\n")
      cat("logLik:", logLik(m_I_cost)[[1]], "\n")
      cat("N:", nobs(m_I_cost), "\n")
      te_cost <- efficiencies(m_I_cost)
      cat("TE mean:", round(mean(te_cost), 4), "\n")

      coefs_cost <- coef(m_I_cost)
      gamma_cost <- coefs_cost["gamma"]
      cat("gamma:", round(gamma_cost, 6), "\n")

      sm_cost <- summary(m_I_cost)
      print(sm_cost)

      saveRDS(m_I_cost, file.path(CLEAN_DIR, "sfa_A_I_cost.rds"))
      cat("Saved: data_clean/sfa_A_I_cost.rds\n")
    }
  }

  # Also try Cobb-Douglas regardless
  cat("\nTrying Cobb-Douglas (no quadratic terms):\n")

  fml_I_CD <- as.formula(paste(
    "ln_i_diag ~",
    "ln_L_total_c + ln_K_camas_c + ln_K_tech_c +",
    "trend + trend2 +",
    "d_cluster2 + d_cluster3 + d_cluster4 + d_cluster5 |",
    "D_desc + pct_sns + desc_pago + ShareQ + desc_shareQ +",
    paste(ccaa_dummies_keep, collapse = " + ")
  ))

  t0 <- Sys.time()
  m_I_CD <- tryCatch(
    frontier::sfa(fml_I_CD, data = df_A_I,
                  ineffDecrease = TRUE, maxit = 5000),
    error = function(e) { cat("CD ERROR:", conditionMessage(e), "\n"); NULL }
  )
  t1 <- Sys.time()
  cat("Time:", round(difftime(t1, t0, units = "secs"), 1), "sec\n")

  if (!is.null(m_I_CD)) {
    cat("\n--- m_I_CD (Cobb-Douglas, production) results ---\n")
    cat("logLik:", logLik(m_I_CD)[[1]], "\n")
    cat("N:", nobs(m_I_CD), "\n")
    te_CD <- efficiencies(m_I_CD)
    cat("TE mean:", round(mean(te_CD), 4), "\n")

    coefs_CD <- coef(m_I_CD)
    gamma_CD <- coefs_CD["gamma"]
    cat("gamma:", round(gamma_CD, 6), "\n")

    sm_CD <- summary(m_I_CD)
    print(sm_CD)

    saveRDS(m_I_CD, file.path(CLEAN_DIR, "sfa_A_I_CD.rds"))
    cat("Saved: data_clean/sfa_A_I_CD.rds\n")

    # If CD also fails, try CD as cost
    if (gamma_CD >= 0.999 && skew_val > 0) {
      cat("\nCD also at boundary — trying CD cost frontier:\n")
      t0 <- Sys.time()
      m_I_CD_cost <- tryCatch(
        frontier::sfa(fml_I_CD, data = df_A_I,
                      ineffDecrease = FALSE, maxit = 5000),
        error = function(e) { cat("CD cost ERROR:", conditionMessage(e), "\n"); NULL }
      )
      t1 <- Sys.time()
      cat("Time:", round(difftime(t1, t0, units = "secs"), 1), "sec\n")

      if (!is.null(m_I_CD_cost)) {
        cat("\n--- m_I_CD_cost results ---\n")
        cat("logLik:", logLik(m_I_CD_cost)[[1]], "\n")
        cat("N:", nobs(m_I_CD_cost), "\n")
        te_CDc <- efficiencies(m_I_CD_cost)
        cat("TE mean:", round(mean(te_CDc), 4), "\n")
        cat("gamma:", round(coef(m_I_CD_cost)["gamma"], 6), "\n")
        print(summary(m_I_CD_cost))
        saveRDS(m_I_CD_cost, file.path(CLEAN_DIR, "sfa_A_I_CD_cost.rds"))
        cat("Saved: data_clean/sfa_A_I_CD_cost.rds\n")
      }
    }
  }
}

cat("\n========== Phase 6b complete ==========\n")
