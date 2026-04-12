## =============================================================
## Phase 6b — SFA Estimation: Design A (intensity)
## v3 (revised):
##   - filtro: i_diag >= 1.0 en lugar de i_diag_sum >= 1000 (size-neutral)
##   - usa ln_i_diag_w (winsorizado en script 08) como dep var principal
##   - winsorización activada siempre, no solo en withShareQ
##   - inputs per-capita para todos los inputs (L, K_camas, K_tech_diag)
##     coherente con output i_diag = procedimientos por alta
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
  co <- coef(m); se <- tryCatch(sqrt(diag(vcov(m))), error = function(e) NULL)
  out <- data.frame(param = vars, coef = NA_real_, se = NA_real_, t = NA_real_)
  if (is.null(se)) return(out)
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
need_vars <- c("L_total","K_camas","K_tech_diag","altTotal_bruto",
               "i_diag","i_diag_sum","i_diag_w","ln_i_diag","ln_i_diag_w",
               "D_desc","pct_sns","ShareQ","desc_pago","desc_shareQ",
               "ccaa_cod","es_agudo",
               "d_cluster2","d_cluster3","d_cluster4","d_cluster5",
               paste0("d_ccaa_", setdiff(1:17, 9)))
df_sfa <- add_missing_from_final(df_sfa, df_final, need_vars)

if (!("i_diag" %in% names(df_sfa))) {
  stop("i_diag not found in df_sfa. Run script 08 first.")
}

cat("========== Phase 6b: SFA Design A (intensity) ==========\n")
cat("df_sfa:", nrow(df_sfa), "x", ncol(df_sfa), "\n")

## ---------- CAMBIO 1: filtro size-neutral sobre el ratio i_diag ----------
## Original: i_diag_sum >= 1000 (depende del tamaño del hospital)
## Nuevo:    i_diag >= 1.0      (mínimo 1 procedimiento ponderado por alta)
UMBRAL_I_DIAG <- 1.0

flag_I <- with(df_sfa,
  es_agudo == 1 &
  !(anyo %in% 2020:2022) &
  altTotal_bruto >= 200 &
  !is.na(i_diag) &
  i_diag >= UMBRAL_I_DIAG &
  !is.na(D_desc) &
  !is.na(pct_sns) &
  !is.na(ccaa_cod))

df_AI <- df_sfa[flag_I, , drop = FALSE]
cat("Intensity sample (i_diag >=", UMBRAL_I_DIAG, "):", nrow(df_AI), "obs\n")
cat("Years:", paste(sort(unique(df_AI$anyo)), collapse = ", "), "\n")
cat("% private:", round(100 * mean(df_AI$D_desc == 1, na.rm = TRUE), 1), "%\n")
cat("Median i_diag by year:\n")
print(round(tapply(df_AI$i_diag, df_AI$anyo, median, na.rm = TRUE), 2))

## ---------- CAMBIO 2: usar ln_i_diag_w como variable dependiente ----------
## Si script 08 la generó, usarla directamente.
## Si no, winsorizar aquí como fallback.
if ("ln_i_diag_w" %in% names(df_AI) && !all(is.na(df_AI$ln_i_diag_w))) {
  cat("\nln_i_diag_w disponible desde script 08 — usando directamente.\n")
  p1_used  <- round(exp(min(df_AI$ln_i_diag_w, na.rm = TRUE)), 3)
  p99_used <- round(exp(max(df_AI$ln_i_diag_w, na.rm = TRUE)), 3)
  cat(sprintf("  rango i_diag_w: [%.3f, %.3f]\n", p1_used, p99_used))
} else {
  cat("\nWARNING: ln_i_diag_w no disponible — winsorizing ln_i_diag ahora.\n")
  p1  <- quantile(df_AI$ln_i_diag, 0.01, na.rm = TRUE)
  p99 <- quantile(df_AI$ln_i_diag, 0.99, na.rm = TRUE)
  df_AI$ln_i_diag_w <- pmin(pmax(df_AI$ln_i_diag, p1), p99)
  cat(sprintf("  p1=%.3f  p99=%.3f\n", p1, p99))
}

DEP_VAR <- "ln_i_diag_w"   # variable dependiente en todos los modelos

## ---------- per-case inputs (coherente con output i_diag = proc/alta) ----------
den <- to_num(df_AI$altTotal_bruto)
df_AI$ln_L_pc_raw       <- safe_log_ratio(to_num(df_AI$L_total),    den)
df_AI$ln_K_camas_pc_raw <- safe_log_ratio(to_num(df_AI$K_camas),    den)
df_AI$ln_K_tech_pc_raw  <- safe_log_ratio(to_num(df_AI$K_tech_diag), den)

mu_L <- mean(df_AI$ln_L_pc_raw,       na.rm = TRUE)
mu_K <- mean(df_AI$ln_K_camas_pc_raw, na.rm = TRUE)
mu_T <- mean(df_AI$ln_K_tech_pc_raw,  na.rm = TRUE)

df_AI$ln_L_pc_c        <- df_AI$ln_L_pc_raw       - mu_L
df_AI$ln_K_camas_pc_c  <- df_AI$ln_K_camas_pc_raw - mu_K
df_AI$ln_K_tech_pc_c   <- df_AI$ln_K_tech_pc_raw  - mu_T
df_AI$ln_L_pc_c2       <- 0.5 * df_AI$ln_L_pc_c^2
df_AI$ln_K_camas_pc_c2 <- 0.5 * df_AI$ln_K_camas_pc_c^2
df_AI$ln_K_tech_pc_c2  <- 0.5 * df_AI$ln_K_tech_pc_c^2

cat(sprintf("Sample centers: ln_L_pc=%.4f, ln_Kc_pc=%.4f, ln_Kt_pc=%.4f\n",
            mu_L, mu_K, mu_T))

## ---------- complete cases ----------
ccaa_dummies <- get_ccaa_dummies(df_AI, min_n = 30, ref = 9L)
cat("CCAA dummies kept:", paste(ccaa_dummies, collapse = ", "), "\n")

frontier_vars <- c(DEP_VAR,
                   "ln_L_pc_c", "ln_K_camas_pc_c", "ln_K_tech_pc_c",
                   "ln_L_pc_c2", "ln_K_camas_pc_c2", "ln_K_tech_pc_c2",
                   "trend", "trend2",
                   "d_cluster2", "d_cluster3", "d_cluster4", "d_cluster5")
z_vars_main   <- c("D_desc", "pct_sns", "desc_pago", ccaa_dummies)
z_vars_withSQ <- c("D_desc", "pct_sns", "desc_pago",
                   "ShareQ", "desc_shareQ", ccaa_dummies)

need_main <- unique(c(frontier_vars, z_vars_main))
df_AI_main <- df_AI %>% filter(if_all(all_of(need_main), ~ !is.na(.x)))
cat("Complete cases (principal noShareQ):", nrow(df_AI_main), "obs\n")

## ---------- OLS skewness diagnostic ----------
## Comparar raw vs winsorizado para verificar mejora
ols_raw <- lm(ln_i_diag ~ ln_L_pc_c + ln_K_camas_pc_c + ln_K_tech_pc_c +
                ln_L_pc_c2 + ln_K_camas_pc_c2 + ln_K_tech_pc_c2 +
                trend + trend2 +
                d_cluster2 + d_cluster3 + d_cluster4 + d_cluster5,
              data = df_AI_main)
ols_win <- lm(ln_i_diag_w ~ ln_L_pc_c + ln_K_camas_pc_c + ln_K_tech_pc_c +
                ln_L_pc_c2 + ln_K_camas_pc_c2 + ln_K_tech_pc_c2 +
                trend + trend2 +
                d_cluster2 + d_cluster3 + d_cluster4 + d_cluster5,
              data = df_AI_main)

sk_raw <- moments::skewness(residuals(ols_raw))
sk_win <- moments::skewness(residuals(ols_win))
cat(sprintf("OLS skewness ln_i_diag (raw):     %7.4f\n", sk_raw))
cat(sprintf("OLS skewness ln_i_diag_w (winsor):%7.4f\n", sk_win))
cat("(Production SFA requires negative skewness.)\n")

if (sk_win >= 0) {
  cat("WARNING: skewness still positive after winsorization.\n")
  cat("Results should be interpreted with caution.\n")
}

## ---------- formulas ----------
frontier_rhs <- paste(c(
  "ln_L_pc_c", "ln_K_camas_pc_c", "ln_K_tech_pc_c",
  "ln_L_pc_c2", "ln_K_camas_pc_c2", "ln_K_tech_pc_c2",
  "trend", "trend2",
  "d_cluster2", "d_cluster3", "d_cluster4", "d_cluster5"
), collapse = " + ")

z_rhs_main   <- paste(z_vars_main,   collapse = " + ")
z_rhs_withSQ <- paste(z_vars_withSQ, collapse = " + ")

fml_AI_main   <- as.formula(paste(DEP_VAR, "~", frontier_rhs, "|", z_rhs_main))
fml_AI_withSQ <- as.formula(paste(DEP_VAR, "~", frontier_rhs, "|", z_rhs_withSQ))

## ---------- modelo principal (noShareQ) ----------
cat("\n--- Estimating A_intensity principal (winsorized, noShareQ) ---\n")
m_AI_main <- frontier::sfa(fml_AI_main, data = df_AI_main,
                            ineffDecrease = TRUE, maxit = 5000)
report_model(m_AI_main, "A_intensity principal (winsorized, noShareQ)")

gam_main <- coef(m_AI_main)["gamma"]
if (!is.na(gam_main) && gam_main >= 0.999) {
  cat("WARNING: gamma at boundary in principal model.\n")
  cat("Consider checking extreme values in per-case inputs.\n")
}
saveRDS(m_AI_main, file.path(CLEAN_DIR, "sfa_A_intensity_v2.rds"))

## ---------- withShareQ (robustez) ----------
need_withSQ <- unique(c(frontier_vars, z_vars_withSQ))
df_AI_withSQ <- df_AI %>%
  filter(if_all(all_of(need_withSQ), ~ !is.na(.x)))
cat("\nComplete cases (withShareQ):", nrow(df_AI_withSQ), "obs\n")

cat("\n--- Estimating A_intensity robustness withShareQ ---\n")
m_AI_withSQ <- tryCatch(
  frontier::sfa(fml_AI_withSQ, data = df_AI_withSQ,
                ineffDecrease = TRUE, maxit = 5000),
  error = function(e) {
    cat("withShareQ ERROR:", conditionMessage(e), "\n")
    NULL
  }
)

if (!is.null(m_AI_withSQ)) {
  report_model(m_AI_withSQ, "A_intensity robustness withShareQ (winsorized)")
  saveRDS(m_AI_withSQ,
          file.path(CLEAN_DIR, "sfa_A_intensity_withShareQ_v2.rds"))
} else {
  cat("withShareQ model could not be estimated.\n")
}

## ---------- tabla comparativa ----------
cmp_vars <- c("D_desc", "pct_sns", "desc_pago", "ShareQ", "desc_shareQ")
tab_main <- mk_coef_tab(m_AI_main, cmp_vars)
names(tab_main)[2:4] <- c("coef_principal", "se_principal", "t_principal")

if (!is.null(m_AI_withSQ)) {
  tab_withSQ <- mk_coef_tab(m_AI_withSQ, cmp_vars)
  names(tab_withSQ)[2:4] <- c("coef_withShareQ", "se_withShareQ", "t_withShareQ")
  tab_cmp <- dplyr::left_join(tab_main, tab_withSQ, by = "param")
} else {
  tab_cmp <- tab_main
}

write.csv(tab_cmp,
          file.path(CLEAN_DIR, "tabla_A_intensity_v2_vs_withShareQ.csv"),
          row.names = FALSE)

cat("\nSaved:\n")
cat(" - sfa_A_intensity_v2.rds (principal, winsorized, noShareQ)\n")
if (!is.null(m_AI_withSQ))
  cat(" - sfa_A_intensity_withShareQ_v2.rds (robustez, winsorized)\n")
cat(" - tabla_A_intensity_v2_vs_withShareQ.csv\n")
cat("\n========== Phase 6b intensity complete ==========\n")