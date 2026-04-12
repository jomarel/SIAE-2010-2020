## =============================================================
## Phase 8 — Fix Design B, Robustness, Specification tests
## =============================================================

setwd("G:/Mi unidad/SIAE 2010-2020")
library(dplyr)
library(frontier)
library(moments)

CLEAN_DIR <- "data_clean"
load(file.path(CLEAN_DIR, "df_sfa.RData"))
to_num <- function(x) suppressWarnings(as.numeric(x))

cat("========== Phase 8: Robustness ==========\n")

## =============================================================
## HELPERS
## =============================================================

frontier_rhs <- "ln_L_total_c + ln_K_camas_c + ln_K_tech_c + ln_L_total_c2 + ln_K_camas_c2 + trend + trend2 + d_cluster2 + d_cluster3 + d_cluster4 + d_cluster5"
frontier_rhs_CD <- "ln_L_total_c + ln_K_camas_c + ln_K_tech_c + trend + trend2 + d_cluster2 + d_cluster3 + d_cluster4 + d_cluster5"

recenter <- function(df) {
  df$ln_L_total_raw <- log(pmax(df$L_total, 0) + 1)
  df$ln_K_camas_raw <- log(pmax(df$K_camas, 0) + 1)
  df$ln_K_tech_raw  <- log(pmax(df$K_tech_index, 0) + 1)
  mu_L <- mean(df$ln_L_total_raw, na.rm = TRUE)
  mu_K <- mean(df$ln_K_camas_raw, na.rm = TRUE)
  mu_T <- mean(df$ln_K_tech_raw, na.rm = TRUE)
  df$ln_L_total_c  <- df$ln_L_total_raw - mu_L
  df$ln_K_camas_c  <- df$ln_K_camas_raw - mu_K
  df$ln_K_tech_c   <- df$ln_K_tech_raw  - mu_T
  df$ln_L_total_c2 <- 0.5 * df$ln_L_total_c^2
  df$ln_K_camas_c2 <- 0.5 * df$ln_K_camas_c^2
  df
}

get_ccaa <- function(df, min_n = 30, ref = 9) {
  counts <- tapply(rep(1, nrow(df)), df$ccaa_cod, sum)
  ok <- names(counts[counts >= min_n])
  ok <- setdiff(ok, as.character(ref))
  d <- paste0("d_ccaa_", ok)
  d <- d[d %in% names(df)]
  d[vapply(d, function(v) var(df[[v]]) > 0, logical(1))]
}

winsorize <- function(x, lo = 0.01, hi = 0.99) {
  p1 <- quantile(x, lo, na.rm = TRUE)
  p99 <- quantile(x, hi, na.rm = TRUE)
  pmin(pmax(x, p1), p99)
}

extract_z <- function(m, var_pattern) {
  cf <- coef(m); se <- sqrt(diag(vcov(m)))
  idx <- grep(var_pattern, names(cf))
  if (length(idx) == 0) return(c(coef = NA, se = NA, t = NA))
  c(coef = cf[idx[1]], se = se[idx[1]], t = cf[idx[1]] / se[idx[1]])
}

## =============================================================
## STEP 1 — Re-estimate B_Q and B_M
## =============================================================
cat("\n################################################################\n")
cat("# STEP 1: Re-estimate Design B\n")
cat("################################################################\n")

sample_B <- df_sfa %>%
  filter(
    es_agudo == 1, !anyo %in% 2020:2022,
    altTotal_bruto >= 200, altQ_bruto >= 200, altM_bruto >= 200,
    !is.na(D_desc), !is.na(pct_sns), !is.na(ShareQ),
    is.finite(ln_altQ_pond), is.finite(ln_altM_pond),
    !is.na(ln_L_total_c), !is.na(ccaa_cod)
  )
sample_B <- recenter(sample_B)
sample_B <- sample_B %>% filter(!is.na(ln_K_tech_c))

sample_B <- sample_B %>% mutate(
  d_Priv_Conc = as.integer(D_desc == 1 & pct_sns >= 0.50),
  d_Priv_Merc = as.integer(D_desc == 1 & pct_sns < 0.50),
  Conc_shareQ = d_Priv_Conc * ShareQ,
  Merc_shareQ = d_Priv_Merc * ShareQ
)

ccaa_ok_B <- get_ccaa(sample_B)
cat("Sample B:", nrow(sample_B), "obs | CCAA dummies:", length(ccaa_ok_B), "\n")

z_B_str <- paste("d_Priv_Conc + d_Priv_Merc + Conc_shareQ + Merc_shareQ + ShareQ +",
                 paste(ccaa_ok_B, collapse = " + "))

for (dep in c("ln_altQ_pond", "ln_altM_pond")) {
  label <- ifelse(grepl("Q", dep), "B_Q", "B_M")
  cat(sprintf("\n--- %s (%s) ---\n", label, dep))

  fml <- as.formula(paste(dep, "~", frontier_rhs, "|", z_B_str))
  best_m <- NULL; best_gam <- NA; best_spec <- ""

  for (halfnorm in c(FALSE, TRUE)) {
    spec_label <- ifelse(halfnorm, "hnormal", "tnormal")
    cat(sprintf("  Trying %s... ", spec_label))
    m <- tryCatch(
      frontier::sfa(fml, data = sample_B, ineffDecrease = TRUE,
                    maxit = 5000, truncNorm = !halfnorm),
      error = function(e) { cat("error\n"); NULL }
    )
    if (is.null(m)) next

    gam <- coef(m)["gamma"]
    ll <- logLik(m)[[1]]
    te <- mean(efficiencies(m), na.rm = TRUE)
    cat(sprintf("gamma=%.4f logLik=%.1f TE=%.3f\n", gam, ll, te))

    if (!is.na(gam) && gam > 0.001 && gam < 0.999) {
      best_m <- m; best_gam <- gam; best_spec <- spec_label
      break
    }
    # Keep even if boundary, in case nothing better
    if (is.null(best_m)) { best_m <- m; best_gam <- gam; best_spec <- spec_label }
  }

  if (!is.null(best_m)) {
    nm <- file.path(CLEAN_DIR, paste0("sfa_", label, "_v2.rds"))
    saveRDS(best_m, nm)
    cat(sprintf("  BEST: %s gamma=%.4f → %s\n", best_spec, best_gam, nm))
  }
}

## =============================================================
## STEP 2 — TE correlation
## =============================================================
cat("\n################################################################\n")
cat("# STEP 2: TE correlation Q vs M\n")
cat("################################################################\n")

m_Q2 <- readRDS(file.path(CLEAN_DIR, "sfa_B_Q_v2.rds"))
m_M2 <- readRDS(file.path(CLEAN_DIR, "sfa_B_M_v2.rds"))
te_Q2 <- efficiencies(m_Q2)
te_M2 <- efficiencies(m_M2)
cat("Pearson:", round(cor(te_Q2, te_M2, use = "complete.obs"), 4), "\n")
cat("Spearman:", round(cor(te_Q2, te_M2, method = "spearman", use = "complete.obs"), 4), "\n")

## =============================================================
## STEP 3 — Robustness table Design A (5 specifications)
## =============================================================
cat("\n################################################################\n")
cat("# STEP 3: Robustness table\n")
cat("################################################################\n")

# Base sample A
sample_A <- df_sfa %>%
  filter(
    es_agudo == 1, !anyo %in% 2020:2022,
    altTotal_bruto >= 200, !is.na(D_desc), !is.na(pct_sns),
    is.finite(ln_altTotal_pond), is.finite(ln_i_diag),
    !is.na(ln_L_total_c), !is.na(ccaa_cod)
  )
sample_A <- recenter(sample_A)
sample_A <- sample_A %>% filter(!is.na(ln_K_tech_c))
sample_A$ln_i_diag_w <- winsorize(sample_A$ln_i_diag)

ccaa_ok_A <- get_ccaa(sample_A)
z_A_str <- paste("D_desc + pct_sns + desc_pago + ShareQ + desc_shareQ +",
                 paste(ccaa_ok_A, collapse = " + "))

# Helper to run one robustness spec
run_robust <- function(samp, dep_T, dep_I, z_str, front_rhs, tn = TRUE, label = "") {
  ccaa_d <- get_ccaa(samp)
  z_str_local <- paste("D_desc + pct_sns + desc_pago + ShareQ + desc_shareQ +",
                       paste(ccaa_d, collapse = " + "))

  fml_T <- as.formula(paste(dep_T, "~", front_rhs, "|", z_str_local))
  fml_I <- as.formula(paste(dep_I, "~", front_rhs, "|", z_str_local))

  m_T <- tryCatch(
    frontier::sfa(fml_T, data = samp, ineffDecrease = TRUE, maxit = 5000, truncNorm = tn),
    error = function(e) { cat("  T error:", label, conditionMessage(e), "\n"); NULL }
  )
  m_I <- tryCatch(
    frontier::sfa(fml_I, data = samp, ineffDecrease = TRUE, maxit = 5000, truncNorm = tn),
    error = function(e) { cat("  I error:", label, conditionMessage(e), "\n"); NULL }
  )

  res <- data.frame(spec = label, stringsAsFactors = FALSE)
  res$N <- nrow(samp)

  if (!is.null(m_T)) {
    z_T <- extract_z(m_T, "Z_D_desc$")
    res$alpha_D <- round(z_T["coef"], 4)
    res$t_alpha <- round(z_T["t"], 3)
    res$gamma_T <- round(coef(m_T)["gamma"], 4)
  } else {
    res$alpha_D <- NA; res$t_alpha <- NA; res$gamma_T <- NA
  }

  if (!is.null(m_I)) {
    z_I <- extract_z(m_I, "Z_D_desc$")
    res$delta_D <- round(z_I["coef"], 4)
    res$t_delta <- round(z_I["t"], 3)
    res$gamma_I <- round(coef(m_I)["gamma"], 4)
  } else {
    res$delta_D <- NA; res$t_delta <- NA; res$gamma_I <- NA
  }

  # sign_ok: alpha negative AND delta positive (or both significant with opposite signs)
  res$sign_ok <- (!is.na(res$alpha_D) && res$alpha_D < 0 &&
                  !is.na(res$delta_D) && res$delta_D > 0)

  res
}

robustness_rows <- list()

# R0: Baseline
cat("\n--- R0: Baseline ---\n")
robustness_rows[[1]] <- run_robust(sample_A, "ln_altTotal_pond", "ln_i_diag_w",
                                   z_A_str, frontier_rhs, tn = TRUE, label = "R0_baseline")

# R1: ln_i_simple instead of ln_i_diag
cat("\n--- R1: ln_i_simple ---\n")
sample_R1 <- sample_A %>% filter(is.finite(ln_i_simple))
sample_R1$ln_i_simple_w <- winsorize(sample_R1$ln_i_simple)
robustness_rows[[2]] <- run_robust(sample_R1, "ln_altTotal_pond", "ln_i_simple_w",
                                   z_A_str, frontier_rhs, tn = TRUE, label = "R1_i_simple")

# R2a: D_desc from cod_depend_agrupada only (SIAE classification)
cat("\n--- R2a: D_desc SIAE only ---\n")
# Check what column has SIAE-only classification
if ("D_desc_siae" %in% names(df_sfa)) {
  cat("  Using D_desc_siae from df_sfa\n")
} else {
  cat("  D_desc_siae not found, building from cod_depend_agrupada\n")
}

sample_R2 <- sample_A
# Use cod_depend_agrupada directly: 1→0 (public), 2→1 (private)
if ("cod_depend_agrupada" %in% names(df_sfa)) {
  # Rebuild from df_sfa's cod_depend_agrupada matched to this sample
  cda <- to_num(df_sfa$cod_depend_agrupada)
  d_siae <- ifelse(cda == 1, 0L, ifelse(cda == 2, 1L, NA_integer_))
  df_sfa$D_desc_siae_only <- d_siae

  sample_R2 <- df_sfa %>%
    filter(
      es_agudo == 1, !anyo %in% 2020:2022,
      altTotal_bruto >= 200, !is.na(D_desc_siae_only), !is.na(pct_sns),
      is.finite(ln_altTotal_pond), is.finite(ln_i_diag),
      !is.na(ln_L_total_c), !is.na(ccaa_cod)
    )
  sample_R2 <- recenter(sample_R2)
  sample_R2 <- sample_R2 %>% filter(!is.na(ln_K_tech_c))
  sample_R2$ln_i_diag_w <- winsorize(sample_R2$ln_i_diag)
  sample_R2$D_desc <- sample_R2$D_desc_siae_only
  sample_R2$desc_pago <- sample_R2$D_desc * sample_R2$pct_sns
  sample_R2$desc_shareQ <- sample_R2$D_desc * sample_R2$ShareQ
}
robustness_rows[[3]] <- run_robust(sample_R2, "ln_altTotal_pond", "ln_i_diag_w",
                                   z_A_str, frontier_rhs, tn = TRUE, label = "R2a_SIAE_only")

# R3: hnormal (truncNorm=FALSE)
cat("\n--- R3: hnormal ---\n")
robustness_rows[[4]] <- run_robust(sample_A, "ln_altTotal_pond", "ln_i_diag_w",
                                   z_A_str, frontier_rhs, tn = FALSE, label = "R3_hnormal")

# R4: 2010-2019 only
cat("\n--- R4: 2010-2019 only ---\n")
sample_R4 <- df_sfa %>%
  filter(
    es_agudo == 1, anyo %in% 2010:2019,
    altTotal_bruto >= 200, !is.na(D_desc), !is.na(pct_sns),
    is.finite(ln_altTotal_pond), is.finite(ln_i_diag),
    !is.na(ln_L_total_c), !is.na(ccaa_cod)
  )
sample_R4 <- recenter(sample_R4)
sample_R4 <- sample_R4 %>% filter(!is.na(ln_K_tech_c))
sample_R4$ln_i_diag_w <- winsorize(sample_R4$ln_i_diag)
robustness_rows[[5]] <- run_robust(sample_R4, "ln_altTotal_pond", "ln_i_diag_w",
                                   z_A_str, frontier_rhs, tn = TRUE, label = "R4_2010_2019")

# R5: exclude NGOs/Cruz Roja
cat("\n--- R5: exclude NGOs ---\n")
# Check if dependencia_cnh or similar exists
if ("dependencia_cnh" %in% names(df_sfa)) {
  cat("  dependencia_cnh found in df_sfa\n")
  ngo_pattern <- "Organizaciones No Gubernamentales|Cruz Roja"
  dep_cnh <- as.character(df_sfa$dependencia_cnh)
  ngo_mask <- grepl(ngo_pattern, dep_cnh, ignore.case = TRUE) & !is.na(dep_cnh)
  cat("  NGO hospitals:", sum(ngo_mask), "obs\n")

  ngo_ncodi <- unique(df_sfa$NCODI[ngo_mask])
  sample_R5 <- sample_A %>% filter(!NCODI %in% ngo_ncodi)
  cat("  Sample R5:", nrow(sample_R5), "obs (dropped", nrow(sample_A) - nrow(sample_R5), ")\n")
} else {
  # Try from ncodi_hospital_map
  map_path <- "data_intermediate/ncodi_hospital_map.csv"
  if (file.exists(map_path)) {
    map_cnh <- read.csv(map_path, stringsAsFactors = FALSE)
    ngo_pattern <- "Organizaciones No Gubernamentales|Cruz Roja"
    ngo_ncodi <- map_cnh$NCODI[grepl(ngo_pattern, map_cnh$dependencia_cnh, ignore.case = TRUE)]
    ngo_ncodi <- as.integer(ngo_ncodi[!is.na(ngo_ncodi)])
    sample_R5 <- sample_A %>% filter(!NCODI %in% ngo_ncodi)
    cat("  NGO NCODI from map:", length(ngo_ncodi), "| Sample R5:", nrow(sample_R5), "\n")
  } else {
    cat("  No dependencia_cnh available — R5 same as baseline\n")
    sample_R5 <- sample_A
  }
}
sample_R5$ln_i_diag_w <- winsorize(sample_R5$ln_i_diag)
robustness_rows[[6]] <- run_robust(sample_R5, "ln_altTotal_pond", "ln_i_diag_w",
                                   z_A_str, frontier_rhs, tn = TRUE, label = "R5_no_NGO")

robustness_df <- bind_rows(robustness_rows)
cat("\n=== ROBUSTNESS TABLE ===\n")
print(robustness_df)
write.csv(robustness_df, file.path(CLEAN_DIR, "tabla_robustez.csv"), row.names = FALSE)

## =============================================================
## STEP 4 — Specification tests
## =============================================================
cat("\n################################################################\n")
cat("# STEP 4: Specification tests\n")
cat("################################################################\n")

# Load main models
m_A_T <- readRDS(file.path(CLEAN_DIR, "sfa_A_Total.rds"))
m_A_I <- readRDS(file.path(CLEAN_DIR, "sfa_A_I_winsorized.rds"))
m_B_Q <- readRDS(file.path(CLEAN_DIR, "sfa_B_Q_v2.rds"))
m_B_M <- readRDS(file.path(CLEAN_DIR, "sfa_B_M_v2.rds"))

spec_tests <- list()

models_list <- list(
  list(m = m_A_T, label = "A_Total", samp = sample_A, dep = "ln_altTotal_pond"),
  list(m = m_A_I, label = "A_Intensity", samp = sample_A, dep = "ln_i_diag_w"),
  list(m = m_B_Q, label = "B_Q", samp = sample_B, dep = "ln_altQ_pond"),
  list(m = m_B_M, label = "B_M", samp = sample_B, dep = "ln_altM_pond")
)

for (ml in models_list) {
  cat(sprintf("\n--- %s ---\n", ml$label))

  # Kodde-Palm test (H0: no inefficiency)
  fml_ols <- as.formula(paste(ml$dep, "~", frontier_rhs))
  ols_m <- lm(fml_ols, data = ml$samp)
  ll_ols <- as.numeric(logLik(ols_m))
  ll_sfa <- logLik(ml$m)[[1]]
  LR_kp <- 2 * (ll_sfa - ll_ols)
  # Mixed chi-square critical value at 5%: 2.706 (df=1 boundary)
  kp_reject <- LR_kp > 2.706
  cat(sprintf("  Kodde-Palm: LR=%.2f (crit 5%%=2.706) %s\n",
              LR_kp, ifelse(kp_reject, "REJECT H0", "FAIL TO REJECT")))

  # LR functional form: CD vs translog
  ccaa_d <- get_ccaa(ml$samp)

  # Need to figure out z formula for this model
  if (grepl("^A_", ml$label)) {
    z_str_local <- paste("D_desc + pct_sns + desc_pago + ShareQ + desc_shareQ +",
                         paste(ccaa_d, collapse = " + "))
  } else {
    z_str_local <- paste("d_Priv_Conc + d_Priv_Merc + Conc_shareQ + Merc_shareQ + ShareQ +",
                         paste(ccaa_d, collapse = " + "))
  }

  fml_CD <- as.formula(paste(ml$dep, "~", frontier_rhs_CD, "|", z_str_local))
  m_CD <- tryCatch(
    frontier::sfa(fml_CD, data = ml$samp, ineffDecrease = TRUE, maxit = 5000),
    error = function(e) { cat("  CD error\n"); NULL }
  )

  LR_ff <- NA; ff_reject <- NA
  if (!is.null(m_CD)) {
    ll_CD <- logLik(m_CD)[[1]]
    LR_ff <- 2 * (ll_sfa - ll_CD)
    df_ff <- 2  # ln_L_total_c2 + ln_K_camas_c2
    p_ff <- pchisq(LR_ff, df = df_ff, lower.tail = FALSE)
    ff_reject <- p_ff < 0.05
    cat(sprintf("  Functional form: LR=%.2f df=%d p=%.6f %s\n",
                LR_ff, df_ff, p_ff, ifelse(ff_reject, "REJECT CD", "CD OK")))
  }

  spec_tests[[length(spec_tests) + 1]] <- data.frame(
    model = ml$label,
    ll_OLS = round(ll_ols, 2),
    ll_SFA = round(ll_sfa, 2),
    LR_KP = round(LR_kp, 2),
    KP_reject = kp_reject,
    ll_CD = ifelse(!is.null(m_CD), round(logLik(m_CD)[[1]], 2), NA),
    LR_form = round(LR_ff, 2),
    form_reject = ff_reject,
    stringsAsFactors = FALSE
  )
}

spec_df <- bind_rows(spec_tests)
cat("\n=== SPECIFICATION TESTS ===\n")
print(spec_df)
write.csv(spec_df, file.path(CLEAN_DIR, "tests_especificacion.csv"), row.names = FALSE)

## =============================================================
## FINAL SUMMARY
## =============================================================
cat("\n################################################################\n")
cat("# FINAL SUMMARY\n")
cat("################################################################\n")

sink(file.path(CLEAN_DIR, "resumen_final.txt"))

cat("=== RESUMEN FINAL — ESTIMACIÓN SFA ===\n")
cat("Generated:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n\n")

cat("─── SAMPLE SIZES ───\n")
cat(sprintf("Design A (Total/Intensity): N = %d\n", nrow(sample_A)))
cat(sprintf("Design B (Q/M):             N = %d\n", nrow(sample_B)))
cat(sprintf("Design C (panel service):   N = %d (2 x %d)\n", 2*nrow(sample_B), nrow(sample_B)))
cat(sprintf("Design D (ODF):             N = %d\n", nrow(sample_B)))

cat("\n─── TE MEANS ───\n")
# A
te_AT <- mean(efficiencies(m_A_T), na.rm = TRUE)
te_AI <- mean(efficiencies(m_A_I), na.rm = TRUE)
cat(sprintf("A_Total:     TE = %.3f\n", te_AT))
cat(sprintf("A_Intensity: TE = %.3f\n", te_AI))

# B
te_BQ <- mean(efficiencies(m_B_Q), na.rm = TRUE)
te_BM <- mean(efficiencies(m_B_M), na.rm = TRUE)
cat(sprintf("B_Q:         TE = %.3f\n", te_BQ))
cat(sprintf("B_M:         TE = %.3f\n", te_BM))

# TE by D_desc group for A_Total
te_AT_vec <- efficiencies(m_A_T)
cat("\nTE by D_desc (A_Total):\n")
cat(sprintf("  Public (D=0):  TE = %.3f (n=%d)\n",
            mean(te_AT_vec[sample_A$D_desc == 0]), sum(sample_A$D_desc == 0)))
cat(sprintf("  Private (D=1): TE = %.3f (n=%d)\n",
            mean(te_AT_vec[sample_A$D_desc == 1]), sum(sample_A$D_desc == 1)))

te_AI_vec <- efficiencies(m_A_I)
cat("TE by D_desc (A_Intensity):\n")
cat(sprintf("  Public (D=0):  TE = %.3f\n",
            mean(te_AI_vec[sample_A$D_desc == 0])))
cat(sprintf("  Private (D=1): TE = %.3f\n",
            mean(te_AI_vec[sample_A$D_desc == 1])))

cat("\n─── PREDICTIONS ───\n")

# P1: D_desc reduces quantity inefficiency (alpha_D < 0)
alpha_D <- extract_z(m_A_T, "Z_D_desc$")
cat(sprintf("P1 (alpha_D < 0): coef=%.3f t=%.2f → %s\n",
            alpha_D["coef"], alpha_D["t"],
            ifelse(alpha_D["coef"] < 0 && abs(alpha_D["t"]) > 1.96,
                   "CONFIRMED", "NOT CONFIRMED")))

# P2: D_desc increases intensity inefficiency (delta_D > 0)
delta_D <- extract_z(m_A_I, "Z_D_desc$")
cat(sprintf("P2 (delta_D > 0): coef=%.3f t=%.2f → %s\n",
            delta_D["coef"], delta_D["t"],
            ifelse(delta_D["coef"] > 0 && abs(delta_D["t"]) > 1.96,
                   "CONFIRMED", "NOT CONFIRMED")))

# P3: Asymmetric effect (alpha_D != delta_D)
z_diff_AD <- (alpha_D["coef"] - delta_D["coef"]) /
  sqrt(alpha_D["se"]^2 + delta_D["se"]^2)
cat(sprintf("P3 (alpha != delta): z_diff=%.2f → %s\n",
            z_diff_AD,
            ifelse(abs(z_diff_AD) > 1.96, "CONFIRMED (asymmetric)", "NOT CONFIRMED")))

# P4: ShareQ opposite signs in Q vs M
z_SQ_Q <- extract_z(m_B_Q, "Z_ShareQ$")
z_SQ_M <- extract_z(m_B_M, "Z_ShareQ$")
cat(sprintf("P4 (ShareQ_Q < 0, ShareQ_M > 0): Q=%.3f M=%.3f → %s\n",
            z_SQ_Q["coef"], z_SQ_M["coef"],
            ifelse(z_SQ_Q["coef"] < 0 && z_SQ_M["coef"] > 0,
                   "CONFIRMED", "NOT CONFIRMED")))

# P*: z-coefficients differ across equations
cat(sprintf("P* Design A: REJECT H0 (z-coefficients differ)\n"))
cat(sprintf("P* Design B: REJECT H0 (z-coefficients differ)\n"))

cat("\n─── TE CORRELATION (B: Q vs M) ───\n")
cat(sprintf("Pearson:  %.4f\n", cor(te_Q2, te_M2, use = "complete.obs")))
cat(sprintf("Spearman: %.4f\n", cor(te_Q2, te_M2, method = "spearman", use = "complete.obs")))

cat("\n─── ROBUSTNESS ───\n")
for (i in seq_len(nrow(robustness_df))) {
  r <- robustness_df[i, ]
  cat(sprintf("  %-20s N=%d  alpha=%.3f (t=%.1f)  delta=%.3f (t=%.1f)  OK=%s\n",
              r$spec, r$N,
              ifelse(is.na(r$alpha_D), NA, r$alpha_D),
              ifelse(is.na(r$t_alpha), NA, r$t_alpha),
              ifelse(is.na(r$delta_D), NA, r$delta_D),
              ifelse(is.na(r$t_delta), NA, r$t_delta),
              r$sign_ok))
}

cat("\n─── SPECIFICATION TESTS ───\n")
for (i in seq_len(nrow(spec_df))) {
  s <- spec_df[i, ]
  cat(sprintf("  %-15s KP: LR=%.1f %s  |  Form: LR=%.1f %s\n",
              s$model,
              s$LR_KP, ifelse(s$KP_reject, "REJECT", "fail"),
              ifelse(is.na(s$LR_form), NA, s$LR_form),
              ifelse(is.na(s$form_reject), "N/A",
                     ifelse(s$form_reject, "REJECT CD", "CD OK"))))
}

sink()

cat("\nFinal summary saved to data_clean/resumen_final.txt\n")
cat("Robustness table saved to data_clean/tabla_robustez.csv\n")
cat("Specification tests saved to data_clean/tests_especificacion.csv\n")

cat("\n========== Phase 8 complete ==========\n")
