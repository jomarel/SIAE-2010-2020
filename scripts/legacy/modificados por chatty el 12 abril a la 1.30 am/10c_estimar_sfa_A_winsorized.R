## =============================================================
## Phase 6c — Intensity model: Winsorize + sfaR fallback
## =============================================================

setwd("G:/Mi unidad/SIAE 2010-2020")
library(dplyr)
library(frontier)

CLEAN_DIR <- "data_clean"
load(file.path(CLEAN_DIR, "df_sfa.RData"))
to_num <- function(x) suppressWarnings(as.numeric(x))

cat("========== Phase 6c: Intensity model (winsorized + sfaR) ==========\n")

## --- Apply Design A filters ---
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

# Re-center on sample
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

# Drop K_tech NA
df_A_I <- df_A_I %>% filter(!is.na(ln_K_tech_c))
cat("Sample N:", nrow(df_A_I), "\n")

## =============================================================
## APPROACH 1 — Winsorize ln_i_diag at p1/p99
## =============================================================
cat("\n========== APPROACH 1: Winsorize ==========\n")

if (!requireNamespace("moments", quietly = TRUE)) install.packages("moments")
library(moments)

p1  <- quantile(df_A_I$ln_i_diag, 0.01, na.rm = TRUE)
p99 <- quantile(df_A_I$ln_i_diag, 0.99, na.rm = TRUE)
df_A_I$ln_i_diag_w <- pmin(pmax(df_A_I$ln_i_diag, p1), p99)

cat("Winsorized ln_i_diag:\n")
cat("  original sd:", round(sd(df_A_I$ln_i_diag), 4), "\n")
cat("  winsorized sd:", round(sd(df_A_I$ln_i_diag_w), 4), "\n")
cat("  n clipped at p1:", sum(df_A_I$ln_i_diag < p1), "\n")
cat("  n clipped at p99:", sum(df_A_I$ln_i_diag > p99), "\n")

# Re-check OLS skewness after winsorizing
ols_w <- lm(ln_i_diag_w ~ ln_L_total_c + ln_K_camas_c +
              ln_K_tech_c + ln_L_total_c2 + ln_K_camas_c2 +
              trend + trend2 + d_cluster2 + d_cluster3 +
              d_cluster4 + d_cluster5, data = df_A_I)
cat("OLS skewness after winsorizing:",
    round(moments::skewness(residuals(ols_w)), 4), "\n")

# CCAA dummies: keep only with >= 30 obs
ccaa_ok <- names(which(tapply(rep(1, nrow(df_A_I)),
                               df_A_I$ccaa_cod, sum) >= 30))
ccaa_ok <- paste0("d_ccaa_", setdiff(ccaa_ok, "9"))
ccaa_ok <- ccaa_ok[ccaa_ok %in% names(df_A_I)]
cat("CCAA dummies kept:", length(ccaa_ok), "\n")

fml_w <- as.formula(paste(
  "ln_i_diag_w ~",
  "ln_L_total_c + ln_K_camas_c + ln_K_tech_c +",
  "ln_L_total_c2 + ln_K_camas_c2 + trend + trend2 +",
  "d_cluster2 + d_cluster3 + d_cluster4 + d_cluster5 |",
  "D_desc + pct_sns + desc_pago + ShareQ + desc_shareQ +",
  paste(ccaa_ok, collapse = " + ")
))

t0 <- Sys.time()
m_I_w <- tryCatch(
  frontier::sfa(fml_w, data = df_A_I,
                ineffDecrease = TRUE, maxit = 5000),
  error = function(e) { cat("ERROR:", conditionMessage(e), "\n"); NULL }
)
t1 <- Sys.time()
cat("Time:", round(difftime(t1, t0, units = "secs"), 1), "sec\n")

approach1_ok <- FALSE

if (!is.null(m_I_w)) {
  cat("\n=== WINSORIZED INTENSITY MODEL ===\n")
  cat("logLik:", logLik(m_I_w)[[1]], "\n")
  te_w <- efficiencies(m_I_w)
  cat("TE mean:", round(mean(te_w, na.rm = TRUE), 4), "\n")

  gam <- coef(m_I_w)["gamma"]
  cat("gamma:", round(gam, 6), "\n")

  sm_w <- summary(m_I_w)
  print(sm_w)

  if (!is.na(gam) && gam < 0.999) {
    cat("\n*** APPROACH 1 SUCCEEDED — gamma:", round(gam, 6), "***\n")
    approach1_ok <- TRUE

    # Extract D_desc details
    coefs <- coef(m_I_w)
    se <- sqrt(diag(vcov(m_I_w)))
    idx <- grep("D_desc", names(coefs))
    for (i in idx) {
      tstat <- coefs[i] / se[i]
      cat(sprintf("  %s: coef=%.4f, se=%.4f, t=%.3f\n",
                  names(coefs)[i], coefs[i], se[i], tstat))
    }

    saveRDS(m_I_w, file.path(CLEAN_DIR, "sfa_A_I_winsorized.rds"))
    cat("Saved: data_clean/sfa_A_I_winsorized.rds\n")
  } else {
    cat("\n*** APPROACH 1 FAILED — gamma still at boundary:", round(gam, 6), "***\n")
  }
} else {
  cat("APPROACH 1 FAILED — estimation error\n")
}

## =============================================================
## APPROACH 2 — sfaR::sfacross (only if Approach 1 failed)
## =============================================================
if (!approach1_ok) {
  cat("\n========== APPROACH 2: sfaR::sfacross ==========\n")

  if (!requireNamespace("sfaR", quietly = TRUE)) {
    cat("Installing sfaR...\n")
    install.packages("sfaR")
  }
  library(sfaR)

  fml_sfaR <- ln_i_diag_w ~ ln_L_total_c + ln_K_camas_c +
    ln_K_tech_c + ln_L_total_c2 + ln_K_camas_c2 +
    trend + trend2 + d_cluster2 + d_cluster3 +
    d_cluster4 + d_cluster5

  uhet_f <- as.formula(paste(
    "~ D_desc + pct_sns + desc_pago + ShareQ + desc_shareQ +",
    paste(ccaa_ok, collapse = " + ")
  ))

  configs <- list(
    list(ud = "tnormal", mt = "bfgs",  ht = 1L),
    list(ud = "tnormal", mt = "bhhh",  ht = 1L),
    list(ud = "hnormal", mt = "bfgs",  ht = 1L),
    list(ud = "hnormal", mt = "bhhh",  ht = 2L)
  )

  sfaR_ok <- FALSE
  for (cfg in configs) {
    cat(sprintf("\nTrying sfaR [%s/%s]...\n", cfg$ud, cfg$mt))
    m <- tryCatch(
      sfaR::sfacross(fml_sfaR, uhet = uhet_f,
                     data = df_A_I, S = 1L,
                     udist = cfg$ud, method = cfg$mt,
                     hessianType = cfg$ht),
      error = function(e) {
        cat("  Error:", conditionMessage(e), "\n")
        NULL
      }
    )
    if (is.null(m)) next

    ll <- as.numeric(m$mlLoglik)
    cat("  logLik:", round(ll, 1), "\n")
    if (!is.finite(ll) || ll > 0 || abs(ll) > 1e10) {
      cat("  Invalid logLik — skipping\n")
      next
    }

    cf <- coef(m)
    se <- tryCatch(
      sqrt(abs(diag(m$invHessian))),
      error = function(e) rep(NA_real_, length(cf))
    )

    idx <- grep("D_desc", names(cf))
    if (length(idx) == 0) {
      cat("  D_desc not found in coefficients — skipping\n")
      next
    }

    te <- tryCatch(
      mean(sfaR::efficiencies(m)$teJLMS, na.rm = TRUE),
      error = function(e) NA_real_
    )

    cat(sprintf(
      "  OK [%s/%s]: logLik=%.1f TE=%.3f D_desc=%.3f t=%.2f\n",
      cfg$ud, cfg$mt, ll, te,
      cf[idx[1]], cf[idx[1]] / se[idx[1]]
    ))

    cat("\nFull coefficients:\n")
    coef_df <- data.frame(
      param = names(cf),
      coef = round(cf, 5),
      se = round(se, 5),
      t_stat = round(cf / se, 3)
    )
    print(coef_df)

    cat("\nTE summary:\n")
    te_vals <- tryCatch(sfaR::efficiencies(m)$teJLMS,
                        error = function(e) NULL)
    if (!is.null(te_vals)) print(summary(te_vals))

    saveRDS(m, file.path(CLEAN_DIR, "sfa_A_I_sfaR.rds"))
    cat("Saved: data_clean/sfa_A_I_sfaR.rds\n")
    sfaR_ok <- TRUE
    break
  }

  if (!sfaR_ok) {
    cat("\n*** APPROACH 2 ALSO FAILED — no sfaR config converged ***\n")
  }
}

cat("\n========== Phase 6c complete ==========\n")
