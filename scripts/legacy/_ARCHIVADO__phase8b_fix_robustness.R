## Fix robustness table — extract_z regex was wrong
setwd("G:/Mi unidad/SIAE 2010-2020")
library(dplyr)
library(frontier)
library(moments)

CLEAN_DIR <- "data_clean"
load(file.path(CLEAN_DIR, "df_sfa.RData"))
to_num <- function(x) suppressWarnings(as.numeric(x))

# Debug: check what coefficient names look like
m_test <- readRDS(file.path(CLEAN_DIR, "sfa_A_Total.rds"))
cat("Coefficient names from A_Total:\n")
print(names(coef(m_test)))
cat("\nLooking for D_desc:\n")
print(grep("D_desc", names(coef(m_test)), value = TRUE))

# Fixed extract function
extract_z <- function(m, var_name) {
  cf <- coef(m)
  se <- tryCatch(sqrt(diag(vcov(m))), error = function(e) rep(NA, length(cf)))
  nm <- paste0("Z_", var_name)
  idx <- which(names(cf) == nm)
  if (length(idx) == 0) {
    # Try partial match
    idx <- grep(paste0("Z_", var_name), names(cf), fixed = TRUE)
  }
  if (length(idx) == 0) return(c(coef = NA, se = NA, t = NA))
  c(coef = unname(cf[idx[1]]), se = unname(se[idx[1]]),
    t = unname(cf[idx[1]] / se[idx[1]]))
}

cat("\nTest extract:", extract_z(m_test, "D_desc"), "\n")

## Helpers
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
  p1 <- quantile(x, lo, na.rm = TRUE); p99 <- quantile(x, hi, na.rm = TRUE)
  pmin(pmax(x, p1), p99)
}

frontier_rhs <- "ln_L_total_c + ln_K_camas_c + ln_K_tech_c + ln_L_total_c2 + ln_K_camas_c2 + trend + trend2 + d_cluster2 + d_cluster3 + d_cluster4 + d_cluster5"

run_robust <- function(samp, dep_T, dep_I, front_rhs, label = "") {
  ccaa_d <- get_ccaa(samp)
  z_str <- paste("D_desc + pct_sns + desc_pago + ShareQ + desc_shareQ +",
                 paste(ccaa_d, collapse = " + "))
  fml_T <- as.formula(paste(dep_T, "~", front_rhs, "|", z_str))
  fml_I <- as.formula(paste(dep_I, "~", front_rhs, "|", z_str))

  m_T <- tryCatch(frontier::sfa(fml_T, data = samp, ineffDecrease = TRUE, maxit = 5000),
                  error = function(e) { cat("  T error:", label, "\n"); NULL })
  m_I <- tryCatch(frontier::sfa(fml_I, data = samp, ineffDecrease = TRUE, maxit = 5000),
                  error = function(e) { cat("  I error:", label, "\n"); NULL })

  res <- data.frame(spec = label, N = nrow(samp), stringsAsFactors = FALSE)

  if (!is.null(m_T)) {
    z_T <- extract_z(m_T, "D_desc")
    res$alpha_D <- round(z_T["coef"], 4)
    res$t_alpha <- round(z_T["t"], 3)
    res$gamma_T <- round(coef(m_T)["gamma"], 4)
    res$TE_T    <- round(mean(efficiencies(m_T), na.rm = TRUE), 3)
  } else { res$alpha_D <- NA; res$t_alpha <- NA; res$gamma_T <- NA; res$TE_T <- NA }

  if (!is.null(m_I)) {
    z_I <- extract_z(m_I, "D_desc")
    res$delta_D <- round(z_I["coef"], 4)
    res$t_delta <- round(z_I["t"], 3)
    res$gamma_I <- round(coef(m_I)["gamma"], 4)
    res$TE_I    <- round(mean(efficiencies(m_I), na.rm = TRUE), 3)
  } else { res$delta_D <- NA; res$t_delta <- NA; res$gamma_I <- NA; res$TE_I <- NA }

  res$sign_ok <- (!is.na(res$alpha_D) && res$alpha_D < 0 &&
                  !is.na(res$delta_D) && res$delta_D > 0)
  res
}

## Build base sample A
sample_A <- df_sfa %>%
  filter(es_agudo == 1, !anyo %in% 2020:2022, altTotal_bruto >= 200,
         !is.na(D_desc), !is.na(pct_sns), is.finite(ln_altTotal_pond),
         is.finite(ln_i_diag), !is.na(ln_L_total_c), !is.na(ccaa_cod))
sample_A <- recenter(sample_A)
sample_A <- sample_A %>% filter(!is.na(ln_K_tech_c))
sample_A$ln_i_diag_w <- winsorize(sample_A$ln_i_diag)

rows <- list()

# R0: Baseline
cat("\n--- R0: Baseline ---\n")
rows[[1]] <- run_robust(sample_A, "ln_altTotal_pond", "ln_i_diag_w", frontier_rhs, "R0_baseline")

# R1: ln_i_simple
cat("\n--- R1: ln_i_simple ---\n")
s1 <- sample_A %>% filter(is.finite(ln_i_simple))
s1$ln_i_simple_w <- winsorize(s1$ln_i_simple)
rows[[2]] <- run_robust(s1, "ln_altTotal_pond", "ln_i_simple_w", frontier_rhs, "R1_i_simple")

# R2a: Removed — cod_depend_agrupada not available in df_sfa

# R3: Simplified z (no interactions)
cat("\n--- R3: Simplified z (no interactions) ---\n")
# Drop desc_pago and desc_shareQ from z
run_robust_simple_z <- function(samp, dep_T, dep_I, front_rhs, label) {
  ccaa_d <- get_ccaa(samp)
  z_str <- paste("D_desc + pct_sns + ShareQ +", paste(ccaa_d, collapse = " + "))
  fml_T <- as.formula(paste(dep_T, "~", front_rhs, "|", z_str))
  fml_I <- as.formula(paste(dep_I, "~", front_rhs, "|", z_str))
  m_T <- tryCatch(frontier::sfa(fml_T, data = samp, ineffDecrease = TRUE, maxit = 5000),
                  error = function(e) NULL)
  m_I <- tryCatch(frontier::sfa(fml_I, data = samp, ineffDecrease = TRUE, maxit = 5000),
                  error = function(e) NULL)
  res <- data.frame(spec = label, N = nrow(samp), stringsAsFactors = FALSE)
  if (!is.null(m_T)) {
    z_T <- extract_z(m_T, "D_desc")
    res$alpha_D <- round(z_T["coef"], 4); res$t_alpha <- round(z_T["t"], 3)
    res$gamma_T <- round(coef(m_T)["gamma"], 4); res$TE_T <- round(mean(efficiencies(m_T)), 3)
  } else { res$alpha_D <- NA; res$t_alpha <- NA; res$gamma_T <- NA; res$TE_T <- NA }
  if (!is.null(m_I)) {
    z_I <- extract_z(m_I, "D_desc")
    res$delta_D <- round(z_I["coef"], 4); res$t_delta <- round(z_I["t"], 3)
    res$gamma_I <- round(coef(m_I)["gamma"], 4); res$TE_I <- round(mean(efficiencies(m_I)), 3)
  } else { res$delta_D <- NA; res$t_delta <- NA; res$gamma_I <- NA; res$TE_I <- NA }
  res$sign_ok <- (!is.na(res$alpha_D) && res$alpha_D < 0 && !is.na(res$delta_D) && res$delta_D > 0)
  res
}
rows[[3]] <- run_robust_simple_z(sample_A, "ln_altTotal_pond", "ln_i_diag_w", frontier_rhs, "R3_no_interactions")

# R4: 2010-2019 only
cat("\n--- R4: 2010-2019 only ---\n")
s4 <- df_sfa %>%
  filter(es_agudo == 1, anyo %in% 2010:2019, altTotal_bruto >= 200,
         !is.na(D_desc), !is.na(pct_sns), is.finite(ln_altTotal_pond),
         is.finite(ln_i_diag), !is.na(ln_L_total_c), !is.na(ccaa_cod))
s4 <- recenter(s4)
s4 <- s4 %>% filter(!is.na(ln_K_tech_c))
s4$ln_i_diag_w <- winsorize(s4$ln_i_diag)
rows[[4]] <- run_robust(s4, "ln_altTotal_pond", "ln_i_diag_w", frontier_rhs, "R4_2010_2019")

# R5: exclude NGOs
cat("\n--- R5: exclude NGOs ---\n")
map_path <- "data_intermediate/ncodi_hospital_map.csv"
if (file.exists(map_path)) {
  map_cnh <- read.csv(map_path, stringsAsFactors = FALSE)
  ngo_pattern <- "Organizaciones No Gubernamentales|Cruz Roja"
  ngo_ncodi <- as.integer(map_cnh$NCODI[grepl(ngo_pattern, map_cnh$dependencia_cnh, ignore.case = TRUE)])
  ngo_ncodi <- ngo_ncodi[!is.na(ngo_ncodi)]
  s5 <- sample_A %>% filter(!NCODI %in% ngo_ncodi)
  s5$ln_i_diag_w <- winsorize(s5$ln_i_diag)
  cat("  Excluded NGO hospitals:", nrow(sample_A) - nrow(s5), "obs\n")
  rows[[5]] <- run_robust(s5, "ln_altTotal_pond", "ln_i_diag_w", frontier_rhs, "R5_no_NGO")
} else {
  cat("  ncodi_hospital_map.csv not found — skipping R5\n")
}

robustness_df <- bind_rows(rows)
cat("\n=== ROBUSTNESS TABLE ===\n")
print(as.data.frame(robustness_df))
write.csv(robustness_df, file.path(CLEAN_DIR, "tabla_robustez.csv"), row.names = FALSE)

## Now rebuild resumen_final.txt with correct values
cat("\n=== Rebuilding final summary ===\n")

m_A_T <- readRDS(file.path(CLEAN_DIR, "sfa_A_Total.rds"))
m_A_I <- readRDS(file.path(CLEAN_DIR, "sfa_A_I_winsorized.rds"))
m_B_Q <- readRDS(file.path(CLEAN_DIR, "sfa_B_Q_v2.rds"))
m_B_M <- readRDS(file.path(CLEAN_DIR, "sfa_B_M_v2.rds"))
m_C   <- readRDS(file.path(CLEAN_DIR, "sfa_C_panel.rds"))
m_D   <- readRDS(file.path(CLEAN_DIR, "sfa_D_odf.rds"))

sample_B <- df_sfa %>%
  filter(es_agudo == 1, !anyo %in% 2020:2022, altTotal_bruto >= 200,
         altQ_bruto >= 200, altM_bruto >= 200, !is.na(D_desc), !is.na(pct_sns),
         !is.na(ShareQ), is.finite(ln_altQ_pond), is.finite(ln_altM_pond),
         !is.na(ln_L_total_c), !is.na(ccaa_cod))
sample_B <- recenter(sample_B)
sample_B <- sample_B %>% filter(!is.na(ln_K_tech_c))

sink(file.path(CLEAN_DIR, "resumen_final.txt"))

cat("=== RESUMEN FINAL — ESTIMACIÓN SFA ===\n")
cat("Generated:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n\n")

cat("─── SAMPLE SIZES ───\n")
cat(sprintf("Design A (Total/Intensity): N = %d\n", nrow(sample_A)))
cat(sprintf("Design B (Q/M):             N = %d\n", nrow(sample_B)))
cat(sprintf("Design C (panel service):   N = %d (2 x %d)\n", 2*nrow(sample_B), nrow(sample_B)))
cat(sprintf("Design D (ODF):             N = %d\n", nrow(sample_B)))

cat("\n─── MODEL RESULTS ───\n\n")

print_model <- function(m, label) {
  cf <- coef(m); se <- sqrt(diag(vcov(m)))
  te <- efficiencies(m)
  cat(sprintf("%s: logLik=%.1f  N=%d  TE=%.3f  gamma=%.4f\n",
              label, logLik(m)[[1]], nobs(m), mean(te), cf["gamma"]))
  z_idx <- grep("^Z_", names(cf))
  for (i in z_idx) {
    tstat <- cf[i] / se[i]
    sig <- ifelse(abs(tstat) >= 2.576, "***",
           ifelse(abs(tstat) >= 1.96, "**",
           ifelse(abs(tstat) >= 1.645, "*", "")))
    cat(sprintf("  %-20s %10.4f  (%.4f)  t=%7.3f %s\n",
                names(cf)[i], cf[i], se[i], tstat, sig))
  }
  cat("\n")
}

print_model(m_A_T, "A_Total")
print_model(m_A_I, "A_Intensity (winsorized)")
print_model(m_B_Q, "B_Surgical")
print_model(m_B_M, "B_Medical")
print_model(m_C, "C_Panel")
print_model(m_D, "D_ODF")

cat("─── TE BY D_DESC GROUP ───\n")
te_AT <- efficiencies(m_A_T)
te_AI <- efficiencies(m_A_I)
cat("Design A — Total:\n")
cat(sprintf("  Public (D=0):  TE = %.3f (n=%d)\n", mean(te_AT[sample_A$D_desc == 0]), sum(sample_A$D_desc == 0)))
cat(sprintf("  Private (D=1): TE = %.3f (n=%d)\n", mean(te_AT[sample_A$D_desc == 1]), sum(sample_A$D_desc == 1)))
cat("Design A — Intensity:\n")
cat(sprintf("  Public (D=0):  TE = %.3f\n", mean(te_AI[sample_A$D_desc == 0])))
cat(sprintf("  Private (D=1): TE = %.3f\n", mean(te_AI[sample_A$D_desc == 1])))

te_BQ <- efficiencies(m_B_Q)
te_BM <- efficiencies(m_B_M)
cat("\nTE correlation B (Q vs M):\n")
cat(sprintf("  Pearson:  %.4f\n", cor(te_BQ, te_BM, use = "complete.obs")))
cat(sprintf("  Spearman: %.4f\n", cor(te_BQ, te_BM, method = "spearman", use = "complete.obs")))

cat("\n─── PREDICTIONS ───\n")
alpha <- extract_z(m_A_T, "D_desc")
delta <- extract_z(m_A_I, "D_desc")
z_diff <- (alpha["coef"] - delta["coef"]) / sqrt(alpha["se"]^2 + delta["se"]^2)

cat(sprintf("P1 (alpha_D < 0): %.3f (t=%.2f) → %s\n", alpha["coef"], alpha["t"],
            ifelse(alpha["coef"] < 0 && abs(alpha["t"]) > 1.96, "CONFIRMED", "NOT CONFIRMED")))
cat(sprintf("P2 (delta_D > 0): %.3f (t=%.2f) → %s\n", delta["coef"], delta["t"],
            ifelse(delta["coef"] > 0 && abs(delta["t"]) > 1.96, "CONFIRMED", "NOT CONFIRMED")))
cat(sprintf("P3 (asymmetry):   z_diff=%.2f → %s\n", z_diff,
            ifelse(abs(z_diff) > 1.96, "CONFIRMED", "NOT CONFIRMED")))

z_SQ_Q <- extract_z(m_B_Q, "ShareQ")
z_SQ_M <- extract_z(m_B_M, "ShareQ")
cat(sprintf("P4 (ShareQ_Q<0, ShareQ_M>0): Q=%.3f M=%.3f → %s\n",
            z_SQ_Q["coef"], z_SQ_M["coef"],
            ifelse(z_SQ_Q["coef"] < 0 && z_SQ_M["coef"] > 0, "CONFIRMED", "NOT CONFIRMED")))

cat("P* Design A: REJECT (LR=11125, p<0.001)\n")
cat("P* Design B: REJECT (LR=4211, p<0.001)\n")

cat("\n─── ROBUSTNESS ───\n")
for (i in seq_len(nrow(robustness_df))) {
  r <- robustness_df[i, ]
  cat(sprintf("  %-22s N=%-5d alpha=% 8.3f (t=% 6.2f)  delta=% 8.3f (t=% 6.2f)  %s\n",
              r$spec, r$N,
              ifelse(is.na(r$alpha_D), 0, r$alpha_D),
              ifelse(is.na(r$t_alpha), 0, r$t_alpha),
              ifelse(is.na(r$delta_D), 0, r$delta_D),
              ifelse(is.na(r$t_delta), 0, r$t_delta),
              ifelse(r$sign_ok, "OK", "CHECK")))
}

# Spec tests
spec_df <- read.csv(file.path(CLEAN_DIR, "tests_especificacion.csv"))
cat("\n─── SPECIFICATION TESTS ───\n")
for (i in seq_len(nrow(spec_df))) {
  s <- spec_df[i, ]
  cat(sprintf("  %-15s KP: LR=%7.1f %s  |  Form: LR=%7.1f %s\n",
              s$model, s$LR_KP, ifelse(s$KP_reject, "REJECT", "fail"),
              ifelse(is.na(s$LR_form), 0, s$LR_form),
              ifelse(is.na(s$form_reject), "N/A", ifelse(s$form_reject, "REJECT_CD", "CD_OK"))))
}

sink()

cat("resumen_final.txt updated\n")
cat("Done.\n")
