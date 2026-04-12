## =============================================================
## Phase 6 — SFA Estimation: Design A (quantity)
## v2:
##   - principal specification excludes ShareQ
##   - ShareQ specification kept as robustness
##   - quantity sample no longer depends on intensity availability
## =============================================================

setwd("G:/Mi unidad/SIAE 2010-2020")
library(dplyr)
library(frontier)

CLEAN_DIR <- "data_clean"
load(file.path(CLEAN_DIR, "df_sfa.RData"))
load(file.path(CLEAN_DIR, "df_final.RData"))

to_num <- function(x) suppressWarnings(as.numeric(x))
safe_log1 <- function(x) ifelse(!is.na(x), log(pmax(x, 0) + 1), NA_real_)

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
  cat("TE mean:", round(mean(efficiencies(m)), 4), "\n")
  cat("TE median:", round(median(efficiencies(m)), 4), "\n")
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

## ---------- enrich df_sfa if needed ----------
need_vars <- c("L_total","K_camas","K_tech_index","ln_altTotal_pond",
               "altTotal_bruto","D_desc","pct_sns","ShareQ","desc_pago",
               "desc_shareQ","ccaa_cod","es_agudo",
               "d_cluster2","d_cluster3","d_cluster4","d_cluster5",
               paste0("d_ccaa_", setdiff(1:17, 9)))
df_sfa <- add_missing_from_final(df_sfa, df_final, need_vars)

cat("========== Phase 6: SFA Design A (quantity) ==========" , "\n")
cat("df_sfa:", nrow(df_sfa), "x", ncol(df_sfa), "\n")

## ---------- sample ----------
flag_Q <- if ("flag_sample_Q" %in% names(df_sfa)) {
  with(df_sfa, flag_sample_Q)
} else {
  with(df_sfa,
       es_agudo == 1 &
       !(anyo %in% 2020:2022) &
       altTotal_bruto >= 200 &
       !is.na(D_desc) &
       !is.na(pct_sns) &
       is.finite(ln_altTotal_pond) &
       !is.na(ccaa_cod))
}

df_AQ <- df_sfa[flag_Q, , drop = FALSE]
cat("Quantity sample:", nrow(df_AQ), "obs\n")
cat("Years:", paste(sort(unique(df_AQ$anyo)), collapse = ", "), "\n")
cat("% private:", round(100 * mean(df_AQ$D_desc == 1, na.rm = TRUE), 1), "%\n")

## ---------- recenter inputs on estimation sample ----------
df_AQ$ln_L_total_raw <- safe_log1(to_num(df_AQ$L_total))
df_AQ$ln_K_camas_raw <- safe_log1(to_num(df_AQ$K_camas))
df_AQ$ln_K_tech_raw  <- safe_log1(to_num(df_AQ$K_tech_index))

mu_L <- mean(df_AQ$ln_L_total_raw, na.rm = TRUE)
mu_K <- mean(df_AQ$ln_K_camas_raw, na.rm = TRUE)
mu_T <- mean(df_AQ$ln_K_tech_raw,  na.rm = TRUE)

df_AQ$ln_L_total_c  <- df_AQ$ln_L_total_raw - mu_L
df_AQ$ln_K_camas_c  <- df_AQ$ln_K_camas_raw - mu_K
df_AQ$ln_K_tech_c   <- df_AQ$ln_K_tech_raw  - mu_T
df_AQ$ln_L_total_c2 <- 0.5 * df_AQ$ln_L_total_c^2
df_AQ$ln_K_camas_c2 <- 0.5 * df_AQ$ln_K_camas_c^2
df_AQ$ln_K_tech_c2  <- 0.5 * df_AQ$ln_K_tech_c^2

cat(sprintf("Sample centers: ln_L=%.4f, ln_Kc=%.4f, ln_Kt=%.4f\n", mu_L, mu_K, mu_T))

## ---------- complete cases ----------
ccaa_dummies <- get_ccaa_dummies(df_AQ, min_n = 30, ref = 9L)
cat("CCAA dummies kept:", paste(ccaa_dummies, collapse = ", "), "\n")

frontier_vars <- c("ln_altTotal_pond","ln_L_total_c","ln_K_camas_c","ln_K_tech_c",
                   "ln_L_total_c2","ln_K_camas_c2","ln_K_tech_c2",
                   "trend","trend2","d_cluster2","d_cluster3","d_cluster4","d_cluster5")
z_vars_main <- c("D_desc","pct_sns","desc_pago", ccaa_dummies)
z_vars_withSQ <- c("D_desc","pct_sns","desc_pago","ShareQ","desc_shareQ", ccaa_dummies)

needed_main <- unique(c(frontier_vars, z_vars_main))
df_AQ_main <- df_AQ %>% filter(if_all(all_of(needed_main), ~ !is.na(.x)))
cat("Complete cases (principal noShareQ):", nrow(df_AQ_main), "obs\n")

## ---------- formulas ----------
frontier_rhs <- paste(c(
  "ln_L_total_c", "ln_K_camas_c", "ln_K_tech_c",
  "ln_L_total_c2", "ln_K_camas_c2", "ln_K_tech_c2",
  "trend", "trend2",
  "d_cluster2", "d_cluster3", "d_cluster4", "d_cluster5"
), collapse = " + ")

z_rhs_main <- paste(z_vars_main, collapse = " + ")
z_rhs_withSQ <- paste(z_vars_withSQ, collapse = " + ")

fml_AQ_main <- as.formula(paste("ln_altTotal_pond ~", frontier_rhs, "|", z_rhs_main))
fml_AQ_withSQ <- as.formula(paste("ln_altTotal_pond ~", frontier_rhs, "|", z_rhs_withSQ))

cat("\nFormula A_total principal (noShareQ):\n", deparse(fml_AQ_main, width.cutoff = 500), "\n")
cat("\nFormula A_total robustness withShareQ:\n", deparse(fml_AQ_withSQ, width.cutoff = 500), "\n")

## ---------- estimate principal ----------
cat("\n--- Estimating A_total principal (noShareQ) ---\n")
m_A_total <- frontier::sfa(fml_AQ_main, data = df_AQ_main,
                           ineffDecrease = TRUE, maxit = 5000)
report_model(m_A_total, "A_total principal (noShareQ)")
saveRDS(m_A_total, file.path(CLEAN_DIR, "sfa_A_total_v2.rds"))

## ---------- estimate withShareQ robustness ----------
needed_withSQ <- unique(c(frontier_vars, z_vars_withSQ))
df_AQ_withSQ <- df_AQ %>% filter(if_all(all_of(needed_withSQ), ~ !is.na(.x)))
cat("\nComplete cases (withShareQ):", nrow(df_AQ_withSQ), "obs\n")

cat("\n--- Estimating A_total robustness withShareQ ---\n")
m_A_total_withSQ <- frontier::sfa(fml_AQ_withSQ, data = df_AQ_withSQ,
                                  ineffDecrease = TRUE, maxit = 5000)
report_model(m_A_total_withSQ, "A_total robustness withShareQ")
saveRDS(m_A_total_withSQ, file.path(CLEAN_DIR, "sfa_A_total_withShareQ_v2.rds"))

## ---------- comparison table ----------
cmp_vars <- c("D_desc","pct_sns","desc_pago","ShareQ","desc_shareQ")
tab_main <- mk_coef_tab(m_A_total, cmp_vars); names(tab_main)[2:4] <- c("coef_principal","se_principal","t_principal")
tab_withSQ <- mk_coef_tab(m_A_total_withSQ, cmp_vars); names(tab_withSQ)[2:4] <- c("coef_withShareQ","se_withShareQ","t_withShareQ")
tab_cmp <- dplyr::left_join(tab_main, tab_withSQ, by = "param")
write.csv(tab_cmp, file.path(CLEAN_DIR, "tabla_A_total_v2_vs_withShareQ.csv"), row.names = FALSE)

cat("\nSaved:\n")
cat(" - sfa_A_total_v2.rds\n")
cat(" - sfa_A_total_withShareQ_v2.rds\n")
cat(" - tabla_A_total_v2_vs_withShareQ.csv\n")
cat("\n========== Phase 6 quantity complete ==========" , "\n")
