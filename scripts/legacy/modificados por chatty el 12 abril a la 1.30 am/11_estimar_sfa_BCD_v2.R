
## =============================================================
## Phase 7 — Designs B, C, D (revised)
## v4:
##   - Design B uses a common-support sample for Q and M
##   - B_Q uses L_quirur + K_quirofanos + K_tech_index
##   - B_M uses L_medico + K_camas + K_tech_index
##   - Design C stacks services with service-specific labor and capital
##   - Design D is redefined as a general multi-output ODF:
##       quantity total (altTotal_pond) + intensity total (i_diag_sum)
## =============================================================

setwd("G:/Mi unidad/SIAE 2010-2020")
library(dplyr)
library(frontier)
library(moments)

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
  te <- efficiencies(m)
  cat("TE mean:", round(mean(te), 4), "\n")
  cat("TE median:", round(median(te), 4), "\n")
  co <- coef(m)
  if ("gamma" %in% names(co)) cat("gamma:", round(co["gamma"], 6), "\n")
  if ("sigmaSq" %in% names(co)) cat("sigmaSq:", round(co["sigmaSq"], 6), "\n")
}

mk_coef_tab <- function(m, vars) {
  co <- coef(m)
  se <- tryCatch(sqrt(diag(vcov(m))), error = function(e) NULL)
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

MIN_ALT_TOTAL <- 200
MIN_ALTQ_B    <- 200
MIN_ALTM_B    <- 200
MIN_I_SUM_D   <- 1000
MIN_CCAA_N    <- 30
EXCL_YEARS    <- 2020:2022
WINSOR_P      <- 0.01

## ---------- enrich df_sfa ----------
need_vars <- c(
  "NCODI","anyo","es_agudo","ccaa_cod",
  "D_desc","pct_sns","ShareQ","desc_pago","desc_shareQ",
  "L_total","L_quirur","L_medico",
  "K_camas","K_quirofanos","K_tech_index",
  "altTotal_bruto","altTotal_pond","ln_altTotal_pond",
  "altQ_bruto","altQ_pond","ln_altQ_pond",
  "altM_bruto","altM_pond","ln_altM_pond",
  "i_diag_sum",
  "trend","trend2",
  "d_cluster2","d_cluster3","d_cluster4","d_cluster5",
  paste0("d_ccaa_", setdiff(1:17, 9))
)
df_sfa <- add_missing_from_final(df_sfa, df_final, need_vars)

if (!("i_diag_sum" %in% names(df_sfa))) {
  stop("i_diag_sum not found in df_sfa.")
}
if (!("K_quirofanos" %in% names(df_sfa))) {
  stop("K_quirofanos not found in df_sfa.")
}

## Rebuild some z vars if needed
if (!("desc_pago" %in% names(df_sfa)) || all(is.na(df_sfa$desc_pago))) {
  df_sfa$desc_pago <- to_num(df_sfa$D_desc) * to_num(df_sfa$pct_sns)
}
df_sfa$d_Priv_Conc <- as.integer(df_sfa$D_desc == 1 & df_sfa$pct_sns >= 0.50)
df_sfa$d_Priv_Merc <- as.integer(df_sfa$D_desc == 1 & df_sfa$pct_sns <  0.50)
df_sfa$Conc_shareQ <- df_sfa$d_Priv_Conc * df_sfa$ShareQ
df_sfa$Merc_shareQ <- df_sfa$d_Priv_Merc * df_sfa$ShareQ

cat("========== Phase 7: Designs B, C, D (revised) ==========\n")
cat("df_sfa:", nrow(df_sfa), "x", ncol(df_sfa), "\n")

## =============================================================
## DESIGN B — common-support sample, Q vs M
## =============================================================
cat("\n\n################################################################\n")
cat("# DESIGN B — common-support sample for Q and M\n")
cat("################################################################\n")

sample_B <- df_sfa %>%
  filter(
    es_agudo == 1,
    !anyo %in% EXCL_YEARS,
    altTotal_bruto >= MIN_ALT_TOTAL,
    altQ_bruto >= MIN_ALTQ_B,
    altM_bruto >= MIN_ALTM_B,
    is.finite(ln_altQ_pond),
    is.finite(ln_altM_pond),
    !is.na(D_desc),
    !is.na(pct_sns),
    !is.na(ShareQ),
    !is.na(desc_pago),
    !is.na(ccaa_cod),
    !is.na(L_quirur),
    !is.na(L_medico),
    !is.na(K_camas),
    !is.na(K_tech_index),
    !is.na(K_quirofanos)
  ) %>%
  mutate(
    ln_L_quirur_raw = safe_log1(to_num(L_quirur)),
    ln_L_medico_raw = safe_log1(to_num(L_medico)),
    ln_K_qx_raw     = safe_log1(to_num(K_quirofanos)),
    ln_K_camas_raw  = safe_log1(to_num(K_camas)),
    ln_K_tech_raw   = safe_log1(to_num(K_tech_index))
  )

# Common centering on the same sample
mu_LQ <- mean(sample_B$ln_L_quirur_raw, na.rm = TRUE)
mu_LM <- mean(sample_B$ln_L_medico_raw, na.rm = TRUE)
mu_KQ <- mean(sample_B$ln_K_qx_raw, na.rm = TRUE)
mu_KM <- mean(sample_B$ln_K_camas_raw, na.rm = TRUE)
mu_T  <- mean(sample_B$ln_K_tech_raw, na.rm = TRUE)

sample_B <- sample_B %>%
  mutate(
    ln_L_quirur_c   = ln_L_quirur_raw - mu_LQ,
    ln_L_medico_c   = ln_L_medico_raw - mu_LM,
    ln_K_qx_c       = ln_K_qx_raw - mu_KQ,
    ln_K_camas_c    = ln_K_camas_raw - mu_KM,
    ln_K_tech_c     = ln_K_tech_raw - mu_T,
    ln_L_quirur_c2  = 0.5 * ln_L_quirur_c^2,
    ln_L_medico_c2  = 0.5 * ln_L_medico_c^2,
    ln_K_qx_c2      = 0.5 * ln_K_qx_c^2,
    ln_K_camas_c2   = 0.5 * ln_K_camas_c^2,
    ln_K_tech_c2    = 0.5 * ln_K_tech_c^2
  )

ccaa_B <- get_ccaa_dummies(sample_B, min_n = MIN_CCAA_N, ref = 9L)
z_rhs_B <- paste(c("d_Priv_Conc", "d_Priv_Merc", "ShareQ", "Conc_shareQ", "Merc_shareQ", ccaa_B),
                 collapse = " + ")

cat("Sample BQ/BM:", nrow(sample_B), "obs\n")
cat("Years:", paste(sort(unique(sample_B$anyo)), collapse = ", "), "\n")
cat("% private:", round(100 * mean(sample_B$D_desc == 1, na.rm = TRUE), 1), "%\n")
cat("Mean ShareQ:", round(mean(sample_B$ShareQ, na.rm = TRUE), 3),
    "| Median ShareQ:", round(median(sample_B$ShareQ, na.rm = TRUE), 3), "\n")

## --- diagnostics ---
ols_BQ <- lm(ln_altQ_pond ~ ln_L_quirur_c + ln_K_qx_c + ln_K_tech_c +
               ln_L_quirur_c2 + ln_K_qx_c2 + ln_K_tech_c2 +
               trend + trend2 + d_cluster2 + d_cluster3 + d_cluster4 + d_cluster5,
             data = sample_B)
ols_BM <- lm(ln_altM_pond ~ ln_L_medico_c + ln_K_camas_c + ln_K_tech_c +
               ln_L_medico_c2 + ln_K_camas_c2 + ln_K_tech_c2 +
               trend + trend2 + d_cluster2 + d_cluster3 + d_cluster4 + d_cluster5,
             data = sample_B)

cat("OLS residual skewness B_Q:", round(moments::skewness(residuals(ols_BQ)), 4), "\n")
cat("OLS residual skewness B_M:", round(moments::skewness(residuals(ols_BM)), 4), "\n")

## --- B_Q ---
frontier_rhs_BQ <- paste(c(
  "ln_L_quirur_c", "ln_K_qx_c", "ln_K_tech_c",
  "ln_L_quirur_c2", "ln_K_qx_c2", "ln_K_tech_c2",
  "trend", "trend2",
  "d_cluster2", "d_cluster3", "d_cluster4", "d_cluster5"
), collapse = " + ")

fml_BQ <- as.formula(paste("ln_altQ_pond ~", frontier_rhs_BQ, "|", z_rhs_B))

cat("\n--- Estimating B_Q ---\n")
m_B_Q <- frontier::sfa(fml_BQ, data = sample_B, ineffDecrease = TRUE, maxit = 5000)
report_model(m_B_Q, "B_Q")
saveRDS(m_B_Q, file.path(CLEAN_DIR, "sfa_B_Q_v3.rds"))

## --- B_M ---
frontier_rhs_BM <- paste(c(
  "ln_L_medico_c", "ln_K_camas_c", "ln_K_tech_c",
  "ln_L_medico_c2", "ln_K_camas_c2", "ln_K_tech_c2",
  "trend", "trend2",
  "d_cluster2", "d_cluster3", "d_cluster4", "d_cluster5"
), collapse = " + ")

fml_BM <- as.formula(paste("ln_altM_pond ~", frontier_rhs_BM, "|", z_rhs_B))

cat("\n--- Estimating B_M ---\n")
m_B_M <- frontier::sfa(fml_BM, data = sample_B, ineffDecrease = TRUE, maxit = 5000)
report_model(m_B_M, "B_M")
saveRDS(m_B_M, file.path(CLEAN_DIR, "sfa_B_M_v3.rds"))

## --- Contrast table B_Q vs B_M ---
vars_B <- c("d_Priv_Conc","d_Priv_Merc","ShareQ","Conc_shareQ","Merc_shareQ")
tab_BQ <- mk_coef_tab(m_B_Q, vars_B); names(tab_BQ)[2:4] <- c("coef_BQ","se_BQ","t_BQ")
tab_BM <- mk_coef_tab(m_B_M, vars_B); names(tab_BM)[2:4] <- c("coef_BM","se_BM","t_BM")
tabla_B <- left_join(tab_BQ, tab_BM, by = "param")
write.csv(tabla_B, file.path(CLEAN_DIR, "tabla_contrastes_B_v3.csv"), row.names = FALSE)

## =============================================================
## DESIGN C — stacked service, same common-support sample as B
## =============================================================
cat("\n\n################################################################\n")
cat("# DESIGN C — stacked service (same sample as B)\n")
cat("################################################################\n")

common_cols <- c("NCODI","anyo","trend","trend2",
                 "d_cluster2","d_cluster3","d_cluster4","d_cluster5",
                 "D_desc","pct_sns","ShareQ","d_Priv_Conc","d_Priv_Merc",
                 "Conc_shareQ","Merc_shareQ","ccaa_cod", ccaa_B)

df_Q <- sample_B[, common_cols, drop = FALSE]
df_Q$C_s <- 1L
df_Q$ln_output <- sample_B$ln_altQ_pond
df_Q$L_serv <- to_num(sample_B$L_quirur)
df_Q$K_serv <- to_num(sample_B$K_quirofanos)
df_Q$K_tech <- to_num(sample_B$K_tech_index)

df_M <- sample_B[, common_cols, drop = FALSE]
df_M$C_s <- 0L
df_M$ln_output <- sample_B$ln_altM_pond
df_M$L_serv <- to_num(sample_B$L_medico)
df_M$K_serv <- to_num(sample_B$K_camas)
df_M$K_tech <- to_num(sample_B$K_tech_index)

sample_C <- bind_rows(df_Q, df_M) %>%
  mutate(
    ln_L_serv_raw = safe_log1(L_serv),
    ln_K_serv_raw = safe_log1(K_serv),
    ln_K_tech_raw = safe_log1(K_tech)
  )

mu_Ls <- mean(sample_C$ln_L_serv_raw, na.rm = TRUE)
mu_Ks <- mean(sample_C$ln_K_serv_raw, na.rm = TRUE)
mu_Ts <- mean(sample_C$ln_K_tech_raw, na.rm = TRUE)

sample_C <- sample_C %>%
  mutate(
    ln_L_serv_c  = ln_L_serv_raw - mu_Ls,
    ln_K_serv_c  = ln_K_serv_raw - mu_Ks,
    ln_K_tech_c  = ln_K_tech_raw - mu_Ts,
    ln_L_serv_c2 = 0.5 * ln_L_serv_c^2,
    ln_K_serv_c2 = 0.5 * ln_K_serv_c^2,
    ln_K_tech_c2 = 0.5 * ln_K_tech_c^2,
    Priv_Conc_Cs = d_Priv_Conc * C_s,
    Priv_Merc_Cs = d_Priv_Merc * C_s,
    ShareQ_Cs    = ShareQ * C_s
  )

ccaa_C <- get_ccaa_dummies(sample_C, min_n = 2 * MIN_CCAA_N, ref = 9L)
frontier_rhs_C <- paste(c(
  "C_s",
  "ln_L_serv_c", "ln_K_serv_c", "ln_K_tech_c",
  "ln_L_serv_c2", "ln_K_serv_c2", "ln_K_tech_c2",
  "trend", "trend2",
  "d_cluster2", "d_cluster3", "d_cluster4", "d_cluster5"
), collapse = " + ")

z_rhs_C <- paste(c(
  "d_Priv_Conc", "d_Priv_Merc", "ShareQ",
  "Priv_Conc_Cs", "Priv_Merc_Cs", "ShareQ_Cs",
  ccaa_C
), collapse = " + ")

fml_C <- as.formula(paste("ln_output ~", frontier_rhs_C, "|", z_rhs_C))

cat("Sample C:", nrow(sample_C), "obs\n")
cat("\n--- Estimating C (stacked service) ---\n")
m_C <- frontier::sfa(fml_C, data = sample_C, ineffDecrease = TRUE, maxit = 5000)
report_model(m_C, "C stacked service")
saveRDS(m_C, file.path(CLEAN_DIR, "sfa_C_stacked_service_v3.rds"))

## =============================================================
## DESIGN D — general multi-output ODF
## Outputs: quantity total (altTotal_pond) and intensity total (i_diag_sum)
## =============================================================
cat("\n\n################################################################\n")
cat("# DESIGN D — general multi-output ODF (quantity + intensity)\n")
cat("################################################################\n")

sample_D <- df_sfa %>%
  filter(
    es_agudo == 1,
    !anyo %in% EXCL_YEARS,
    altTotal_bruto >= MIN_ALT_TOTAL,
    !is.na(D_desc),
    !is.na(pct_sns),
    !is.na(desc_pago),
    !is.na(ccaa_cod),
    !is.na(L_total),
    !is.na(K_camas),
    !is.na(K_tech_index),
    !is.na(altTotal_pond),
    altTotal_pond > 0,
    !is.na(i_diag_sum),
    i_diag_sum >= MIN_I_SUM_D
  ) %>%
  mutate(
    ln_Q_total = log(to_num(altTotal_pond)),
    ln_I_total = log(to_num(i_diag_sum)),
    ln_ratio_IQ = ln_I_total - ln_Q_total,
    neg_ln_Q_total = -ln_Q_total,
    ln_L_total_raw = safe_log1(to_num(L_total)),
    ln_K_camas_raw = safe_log1(to_num(K_camas)),
    ln_K_tech_raw  = safe_log1(to_num(K_tech_index))
  )

# Trim extreme output ratios to reduce ODF instability
p1_ratio  <- quantile(sample_D$ln_ratio_IQ, WINSOR_P, na.rm = TRUE)
p99_ratio <- quantile(sample_D$ln_ratio_IQ, 1 - WINSOR_P, na.rm = TRUE)
sample_D$ln_ratio_IQ_w <- pmin(pmax(sample_D$ln_ratio_IQ, p1_ratio), p99_ratio)

mu_LD <- mean(sample_D$ln_L_total_raw, na.rm = TRUE)
mu_KD <- mean(sample_D$ln_K_camas_raw, na.rm = TRUE)
mu_TD <- mean(sample_D$ln_K_tech_raw, na.rm = TRUE)

sample_D <- sample_D %>%
  mutate(
    ln_L_total_c  = ln_L_total_raw - mu_LD,
    ln_K_camas_c  = ln_K_camas_raw - mu_KD,
    ln_K_tech_c   = ln_K_tech_raw - mu_TD,
    ln_L_total_c2 = 0.5 * ln_L_total_c^2,
    ln_K_camas_c2 = 0.5 * ln_K_camas_c^2,
    ln_K_tech_c2  = 0.5 * ln_K_tech_c^2
  )

ccaa_D <- get_ccaa_dummies(sample_D, min_n = MIN_CCAA_N, ref = 9L)
z_rhs_D <- paste(c("D_desc", "pct_sns", "desc_pago", ccaa_D), collapse = " + ")

frontier_rhs_D <- paste(c(
  "ln_ratio_IQ_w",
  "ln_L_total_c", "ln_K_camas_c", "ln_K_tech_c",
  "ln_L_total_c2", "ln_K_camas_c2", "ln_K_tech_c2",
  "trend", "trend2",
  "d_cluster2", "d_cluster3", "d_cluster4", "d_cluster5"
), collapse = " + ")

fml_D <- as.formula(paste("neg_ln_Q_total ~", frontier_rhs_D, "|", z_rhs_D))

ols_D <- lm(neg_ln_Q_total ~ ln_ratio_IQ_w + ln_L_total_c + ln_K_camas_c + ln_K_tech_c +
              ln_L_total_c2 + ln_K_camas_c2 + ln_K_tech_c2 +
              trend + trend2 + d_cluster2 + d_cluster3 + d_cluster4 + d_cluster5,
            data = sample_D)
cat("Sample D:", nrow(sample_D), "obs\n")
cat("OLS residual skewness D:", round(moments::skewness(residuals(ols_D)), 4), "\n")

cat("\n--- Estimating D (ODF total quantity + total intensity) ---\n")
m_D <- frontier::sfa(fml_D, data = sample_D, ineffDecrease = TRUE, maxit = 5000)
report_model(m_D, "D ODF total quantity + intensity")
saveRDS(m_D, file.path(CLEAN_DIR, "sfa_D_odf_total_intensity_v3.rds"))

cat("\nSaved:\n")
cat(" - sfa_B_Q_v3.rds\n")
cat(" - sfa_B_M_v3.rds\n")
cat(" - tabla_contrastes_B_v3.csv\n")
cat(" - sfa_C_stacked_service_v3.rds\n")
cat(" - sfa_D_odf_total_intensity_v3.rds\n")

cat("\n========== Phase 7 complete ==========\n")
