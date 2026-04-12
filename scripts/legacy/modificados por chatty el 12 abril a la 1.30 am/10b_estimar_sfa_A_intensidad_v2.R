## =============================================================
## Phase 6b — SFA Estimation: Design A (intensity)
## v2:
##   - principal specification excludes ShareQ
##   - ShareQ specification kept as robustness
##   - uses revised intensity measure with raw numerator i_diag_sum
##   - builds per-case inputs on the estimation sample
##   - uses log(x/altas) for per-case inputs
## =============================================================

setwd("G:/Mi unidad/SIAE 2010-2020")
library(dplyr)
library(frontier)
library(moments)

CLEAN_DIR <- "data_clean"
load(file.path(CLEAN_DIR, "df_sfa.RData"))
load(file.path(CLEAN_DIR, "df_final.RData"))

to_num <- function(x) suppressWarnings(as.numeric(x))
safe_log_ratio <- function(x, denom) {
  ratio <- ifelse(!is.na(x) & !is.na(denom) & denom > 0, x / denom, NA_real_)
  ifelse(!is.na(ratio) & ratio > 0, log(ratio), NA_real_)
}

add_missing_from_final <- function(df_small, df_big, vars) {
  key_small <- paste(df_small$NCODI, df_small$anyo)
  key_big   <- paste(df_big$NCODI, df_big$anyo)
  idx <- match(key_small, key_big)
  for (v in vars) {
    if (!(v %in% names(df_small)) && v %in% names(df_big)) {
      df_small[[v]] <- df_big[[v]][idx]
    }
  }
  df_small
}

get_ccaa_dummies <- function(df, min_n = 30, ref = 9L) {
  counts <- table(df$ccaa_cod)
  keep <- as.integer(names(counts[counts >= min_n]))
  keep <- setdiff(keep, ref)
  dums <- paste0("d_ccaa_", keep)
  dums[dums %in% names(df)]
}

report_model <- function(m, label) {
  cat("\n================", label, "================\n")
  cat("logLik:", round(logLik(m)[[1]], 2), "\n")
  cat("N:", nobs(m), "\n")
  te <- efficiencies(m)
  cat("TE mean:", round(mean(te), 4), "\n")
  cat("TE median:", round(median(te), 4), "\n")
  co <- coef(m)
  if ("gamma" %in% names(co)) cat("gamma:", round(co["gamma"], 6), "\n")
  if ("sigmaSq" %in% names(co)) cat("sigmaSq:", round(co["sigmaSq"], 6), "\n")
  se <- tryCatch(sqrt(diag(vcov(m))), error = function(e) NULL)
  if (!is.null(se)) {
    zidx <- grep("^Z_", names(co))
    if (length(zidx) > 0) {
      cat("\nZ-equation coefficients:\n")
      for (i in zidx) {
        tstat <- co[i] / se[i]
        cat(sprintf("  %-20s %10.4f  se=%8.4f  t=%7.3f\n",
                    names(co)[i], co[i], se[i], tstat))
      }
    }
  }
}

mk_coef_tab <- function(m, vars) {
  co <- coef(m); se <- sqrt(diag(vcov(m)))
  out <- data.frame(param = vars, coef = NA_real_, se = NA_real_, t = NA_real_)
  for (i in seq_along(vars)) {
    nm <- paste0("Z_", vars[i])
    if (nm %in% names(co)) {
      out$coef[i] <- co[nm]
      out$se[i]   <- se[nm]
      out$t[i]    <- co[nm] / se[nm]
    }
  }
  out
}

## ---------- enrich df_sfa ----------
need_vars <- c("L_total","K_camas","K_tech_diag","altTotal_bruto","i_diag",
               "i_diag_sum",
               "D_desc","pct_sns","ShareQ","desc_pago","desc_shareQ",
               "ccaa_cod","es_agudo",
               "d_cluster2","d_cluster3","d_cluster4","d_cluster5",
               paste0("d_ccaa_", setdiff(1:17, 9)))
df_sfa <- add_missing_from_final(df_sfa, df_final, need_vars)

if (!("i_diag_sum" %in% names(df_sfa))) {
  stop("i_diag_sum not found in df_sfa.")
}

cat("========== Phase 6b: SFA Design A (intensity) ==========" , "\n")
cat("df_sfa:", nrow(df_sfa), "x", ncol(df_sfa), "\n")

## ---------- sample ----------
flag_I <- if ("flag_sample_I_main" %in% names(df_sfa)) {
  with(df_sfa, flag_sample_I_main)
} else {
  with(df_sfa,
       es_agudo == 1 &
       !(anyo %in% 2020:2022) &
       altTotal_bruto >= 200 &
       !is.na(i_diag_sum) &
       i_diag_sum >= 1000 &
       is.finite(ln_i_diag) &
       !is.na(D_desc) &
       !is.na(pct_sns) &
       !is.na(ccaa_cod))
}

df_AI <- df_sfa[flag_I, , drop = FALSE]
cat("Intensity sample:", nrow(df_AI), "obs\n")
cat("Years:", paste(sort(unique(df_AI$anyo)), collapse = ", "), "\n")
cat("% private:", round(100 * mean(df_AI$D_desc == 1, na.rm = TRUE), 1), "%\n")
cat("Median i_diag by year:\n")
print(round(tapply(df_AI$i_diag, df_AI$anyo, median, na.rm = TRUE), 2))

## ---------- per-case inputs ----------
den <- to_num(df_AI$altTotal_bruto)
df_AI$ln_L_pc_raw       <- safe_log_ratio(to_num(df_AI$L_total), den)
df_AI$ln_K_camas_pc_raw <- safe_log_ratio(to_num(df_AI$K_camas), den)
df_AI$ln_K_tech_pc_raw  <- safe_log_ratio(to_num(df_AI$K_tech_diag), den)

mu_L <- mean(df_AI$ln_L_pc_raw, na.rm = TRUE)
mu_K <- mean(df_AI$ln_K_camas_pc_raw, na.rm = TRUE)
mu_T <- mean(df_AI$ln_K_tech_pc_raw, na.rm = TRUE)

df_AI$ln_L_pc_c        <- df_AI$ln_L_pc_raw - mu_L
df_AI$ln_K_camas_pc_c  <- df_AI$ln_K_camas_pc_raw - mu_K
df_AI$ln_K_tech_pc_c   <- df_AI$ln_K_tech_pc_raw - mu_T
df_AI$ln_L_pc_c2       <- 0.5 * df_AI$ln_L_pc_c^2
df_AI$ln_K_camas_pc_c2 <- 0.5 * df_AI$ln_K_camas_pc_c^2
df_AI$ln_K_tech_pc_c2  <- 0.5 * df_AI$ln_K_tech_pc_c^2

cat(sprintf("Sample centers: ln_L_pc=%.4f, ln_Kc_pc=%.4f, ln_Kt_pc=%.4f\n", mu_L, mu_K, mu_T))

## ---------- complete cases ----------
ccaa_dummies <- get_ccaa_dummies(df_AI, min_n = 30, ref = 9L)
cat("CCAA dummies kept:", paste(ccaa_dummies, collapse = ", "), "\n")

frontier_vars <- c("ln_i_diag","ln_L_pc_c","ln_K_camas_pc_c","ln_K_tech_pc_c",
                   "ln_L_pc_c2","ln_K_camas_pc_c2","ln_K_tech_pc_c2",
                   "trend","trend2","d_cluster2","d_cluster3","d_cluster4","d_cluster5")
z_vars_main <- c("D_desc","pct_sns","desc_pago", ccaa_dummies)
z_vars_withSQ <- c("D_desc","pct_sns","desc_pago","ShareQ","desc_shareQ", ccaa_dummies)

need_main <- unique(c(frontier_vars, z_vars_main))
df_AI_main <- df_AI %>% filter(if_all(all_of(need_main), ~ !is.na(.x)))
cat("Complete cases (principal noShareQ):", nrow(df_AI_main), "obs\n")

## ---------- OLS skewness diagnostic ----------
ols_I <- lm(ln_i_diag ~ ln_L_pc_c + ln_K_camas_pc_c + ln_K_tech_pc_c +
              ln_L_pc_c2 + ln_K_camas_pc_c2 + ln_K_tech_pc_c2 +
              trend + trend2 + d_cluster2 + d_cluster3 + d_cluster4 + d_cluster5,
            data = df_AI_main)
sk_ols <- moments::skewness(residuals(ols_I))
cat("OLS residual skewness:", round(sk_ols, 4), "\n")
cat("(Production SFA prefers negative skewness.)\n")

## ---------- formulas ----------
frontier_rhs <- paste(c(
  "ln_L_pc_c", "ln_K_camas_pc_c", "ln_K_tech_pc_c",
  "ln_L_pc_c2", "ln_K_camas_pc_c2", "ln_K_tech_pc_c2",
  "trend", "trend2",
  "d_cluster2", "d_cluster3", "d_cluster4", "d_cluster5"
), collapse = " + ")

z_rhs_main <- paste(z_vars_main, collapse = " + ")
z_rhs_withSQ <- paste(z_vars_withSQ, collapse = " + ")

fml_AI_main <- as.formula(paste("ln_i_diag ~", frontier_rhs, "|", z_rhs_main))
fml_AI_withSQ <- as.formula(paste("ln_i_diag ~", frontier_rhs, "|", z_rhs_withSQ))

## ---------- principal model (noShareQ) ----------
cat("\n--- Estimating A_intensity principal (noShareQ) ---\n")
m_AI_main <- frontier::sfa(fml_AI_main, data = df_AI_main,
                           ineffDecrease = TRUE, maxit = 5000)
report_model(m_AI_main, "A_intensity principal (noShareQ)")
saveRDS(m_AI_main, file.path(CLEAN_DIR, "sfa_A_intensity_v2.rds"))

## ---------- withShareQ robustness ----------
need_withSQ <- unique(c(frontier_vars, z_vars_withSQ))
df_AI_withSQ <- df_AI %>% filter(if_all(all_of(need_withSQ), ~ !is.na(.x)))
cat("\nComplete cases (withShareQ):", nrow(df_AI_withSQ), "obs\n")

cat("\n--- Estimating A_intensity robustness withShareQ (raw) ---\n")
m_AI_withSQ_raw <- tryCatch(
  frontier::sfa(fml_AI_withSQ, data = df_AI_withSQ,
                ineffDecrease = TRUE, maxit = 5000),
  error = function(e) { cat("withShareQ RAW ERROR:", conditionMessage(e), "\n"); NULL }
)

m_AI_withSQ_final <- m_AI_withSQ_raw
dep_name_withSQ <- "ln_i_diag"
used_winsor <- FALSE

if (!is.null(m_AI_withSQ_raw)) {
  report_model(m_AI_withSQ_raw, "A_intensity robustness withShareQ (raw)")
  saveRDS(m_AI_withSQ_raw, file.path(CLEAN_DIR, "sfa_A_intensity_withShareQ_raw_v2.rds"))
  if ("gamma" %in% names(coef(m_AI_withSQ_raw)) && coef(m_AI_withSQ_raw)["gamma"] >= 0.999) {
    cat("*** gamma at boundary in withShareQ raw model — winsorization activated ***\n")
    used_winsor <- TRUE
  }
} else {
  used_winsor <- TRUE
}

if (used_winsor) {
  p1  <- quantile(df_AI_withSQ$ln_i_diag, 0.01, na.rm = TRUE)
  p99 <- quantile(df_AI_withSQ$ln_i_diag, 0.99, na.rm = TRUE)
  df_AI_withSQ$ln_i_diag_w <- pmin(pmax(df_AI_withSQ$ln_i_diag, p1), p99)
  dep_name_withSQ <- "ln_i_diag_w"
  fml_AI_withSQ_w <- as.formula(paste("ln_i_diag_w ~", frontier_rhs, "|", z_rhs_withSQ))
  cat("\n--- Estimating A_intensity robustness withShareQ (winsorized) ---\n")
  m_AI_withSQ_final <- frontier::sfa(fml_AI_withSQ_w, data = df_AI_withSQ,
                                     ineffDecrease = TRUE, maxit = 5000)
  report_model(m_AI_withSQ_final, "A_intensity robustness withShareQ (winsorized)")
  saveRDS(m_AI_withSQ_final, file.path(CLEAN_DIR, "sfa_A_intensity_withShareQ_winsor_v2.rds"))
}

## ---------- comparison table ----------
cmp_vars <- c("D_desc","pct_sns","desc_pago","ShareQ","desc_shareQ")
tab_main <- mk_coef_tab(m_AI_main, cmp_vars); names(tab_main)[2:4] <- c("coef_principal","se_principal","t_principal")
tab_withSQ <- mk_coef_tab(m_AI_withSQ_final, cmp_vars); names(tab_withSQ)[2:4] <- c("coef_withShareQ","se_withShareQ","t_withShareQ")
tab_cmp <- dplyr::left_join(tab_main, tab_withSQ, by = "param")
write.csv(tab_cmp, file.path(CLEAN_DIR, "tabla_A_intensity_v2_vs_withShareQ.csv"), row.names = FALSE)

cat("\nSaved:\n")
cat(" - sfa_A_intensity_v2.rds\n")
cat(" - sfa_A_intensity_withShareQ_raw_v2.rds (if estimable)\n")
cat(" - sfa_A_intensity_withShareQ_winsor_v2.rds (if needed)\n")
cat(" - tabla_A_intensity_v2_vs_withShareQ.csv\n")
cat("\n========== Phase 6b intensity complete ==========" , "\n")
