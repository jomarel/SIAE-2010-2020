## =============================================================
## Phase 7 — Designs B, C, D + Contrasts + P* tests
## =============================================================

setwd("G:/Mi unidad/SIAE 2010-2020")
library(dplyr)
library(frontier)
library(moments)

CLEAN_DIR <- "data_clean"
load(file.path(CLEAN_DIR, "df_sfa.RData"))
to_num <- function(x) suppressWarnings(as.numeric(x))
safe_log <- function(x) ifelse(!is.na(x) & x > 0, log(x), NA_real_)

cat("========== Phase 7: Designs B, C, D ==========\n")
cat("df_sfa:", nrow(df_sfa), "x", ncol(df_sfa), "\n")

## =============================================================
## HELPER: re-center, CCAA filter, winsorize
## =============================================================

recenter_sample <- function(df) {
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
  cat(sprintf("  Centers: ln_L=%.4f, ln_K=%.4f, ln_T=%.4f\n", mu_L, mu_K, mu_T))
  df
}

get_ccaa_dummies <- function(df, min_n = 30, ref = 9) {
  counts <- tapply(rep(1, nrow(df)), df$ccaa_cod, sum)
  ok <- names(counts[counts >= min_n])
  ok <- setdiff(ok, as.character(ref))
  dummies <- paste0("d_ccaa_", ok)
  dummies <- dummies[dummies %in% names(df)]
  # Drop constant
  dummies <- dummies[vapply(dummies, function(v) var(df[[v]]) > 0, logical(1))]
  dummies
}

frontier_rhs <- "ln_L_total_c + ln_K_camas_c + ln_K_tech_c + ln_L_total_c2 + ln_K_camas_c2 + trend + trend2 + d_cluster2 + d_cluster3 + d_cluster4 + d_cluster5"

report_model <- function(m, label) {
  cat(sprintf("\n=== %s ===\n", label))
  cat("logLik:", logLik(m)[[1]], "\n")
  cat("N:", nobs(m), "\n")
  te <- efficiencies(m)
  cat("TE mean:", round(mean(te), 4), "\n")
  cat("TE median:", round(median(te), 4), "\n")
  gam <- coef(m)["gamma"]
  cat("gamma:", round(gam, 6), "\n")
  coefs <- coef(m)
  se <- sqrt(diag(vcov(m)))
  z_idx <- grep("^Z_", names(coefs))
  cat("\nZ-coefficients:\n")
  for (i in z_idx) {
    tstat <- coefs[i] / se[i]
    sig <- ifelse(abs(tstat) >= 2.576, "***",
           ifelse(abs(tstat) >= 1.96, "**",
           ifelse(abs(tstat) >= 1.645, "*", "")))
    cat(sprintf("  %-20s %10.4f  se=%8.4f  t=%7.3f %s\n",
                names(coefs)[i], coefs[i], se[i], tstat, sig))
  }
  cat(sprintf("  %-20s %10.6f\n", "sigmaSq", coefs["sigmaSq"]))
  cat(sprintf("  %-20s %10.6f\n", "gamma", coefs["gamma"]))
  invisible(list(coefs = coefs, se = se, te = te))
}

## =============================================================
## LOAD Design A models for contrast table later
## =============================================================
m_A_Total <- readRDS(file.path(CLEAN_DIR, "sfa_A_Total.rds"))
m_A_I_w   <- readRDS(file.path(CLEAN_DIR, "sfa_A_I_winsorized.rds"))

## =============================================================
## DESIGN B — Surgical vs Medical frontiers
## =============================================================
cat("\n\n################################################################\n")
cat("# DESIGN B — Surgical (Q) vs Medical (M) frontiers\n")
cat("################################################################\n")

sample_B <- df_sfa %>%
  filter(
    es_agudo == 1,
    !anyo %in% 2020:2022,
    altTotal_bruto >= 200,
    !is.na(D_desc),
    !is.na(pct_sns),
    is.finite(ln_altTotal_pond),
    is.finite(ln_i_diag),
    !is.na(ln_L_total_c),
    !is.na(ccaa_cod),
    is.finite(ln_altQ_pond),
    is.finite(ln_altM_pond),
    altQ_bruto >= 200,
    altM_bruto >= 200
  )

sample_B <- recenter_sample(sample_B)
sample_B <- sample_B %>% filter(!is.na(ln_K_tech_c))

# Rebuild payment group dummies on this sample
sample_B$d_Priv_Conc <- as.integer(sample_B$D_desc == 1 & sample_B$pct_sns >= 0.50)
sample_B$d_Priv_Merc <- as.integer(sample_B$D_desc == 1 & sample_B$pct_sns < 0.50)
sample_B$Conc_shareQ <- sample_B$d_Priv_Conc * sample_B$ShareQ
sample_B$Merc_shareQ <- sample_B$d_Priv_Merc * sample_B$ShareQ

cat("Sample B:", nrow(sample_B), "obs\n")
cat("% private:", round(100 * mean(sample_B$D_desc == 1), 1), "%\n")
cat("Pub_Retro:", sum(sample_B$D_desc == 0),
    "| Priv_Conc:", sum(sample_B$d_Priv_Conc),
    "| Priv_Merc:", sum(sample_B$d_Priv_Merc), "\n")

ccaa_B <- get_ccaa_dummies(sample_B)
cat("CCAA dummies:", length(ccaa_B), "\n")

z_B <- paste("d_Priv_Conc + d_Priv_Merc + Conc_shareQ + Merc_shareQ + ShareQ +",
             paste(ccaa_B, collapse = " + "))

## --- m_B_Q (surgical) ---
cat("\n--- Estimating m_B_Q (surgical) ---\n")
fml_B_Q <- as.formula(paste("ln_altQ_pond ~", frontier_rhs, "|", z_B))
t0 <- Sys.time()
m_B_Q <- frontier::sfa(fml_B_Q, data = sample_B, ineffDecrease = TRUE, maxit = 5000)
cat("Time:", round(difftime(Sys.time(), t0, units = "secs"), 1), "sec\n")
res_B_Q <- report_model(m_B_Q, "m_B_Q (surgical)")
saveRDS(m_B_Q, file.path(CLEAN_DIR, "sfa_B_Q.rds"))

## --- m_B_M (medical) ---
cat("\n--- Estimating m_B_M (medical) ---\n")

# Check if winsorization needed
gam_test_M <- NULL
fml_B_M <- as.formula(paste("ln_altM_pond ~", frontier_rhs, "|", z_B))
t0 <- Sys.time()
m_B_M <- frontier::sfa(fml_B_M, data = sample_B, ineffDecrease = TRUE, maxit = 5000)
cat("Time:", round(difftime(Sys.time(), t0, units = "secs"), 1), "sec\n")

gam_M <- coef(m_B_M)["gamma"]
cat("gamma:", round(gam_M, 6), "\n")

if (gam_M >= 0.999) {
  cat("*** gamma at boundary — winsorizing ln_altM_pond ***\n")
  p1m <- quantile(sample_B$ln_altM_pond, 0.01, na.rm = TRUE)
  p99m <- quantile(sample_B$ln_altM_pond, 0.99, na.rm = TRUE)
  sample_B$ln_altM_pond_w <- pmin(pmax(sample_B$ln_altM_pond, p1m), p99m)
  cat("  Clipped:", sum(sample_B$ln_altM_pond < p1m) + sum(sample_B$ln_altM_pond > p99m), "obs\n")
  fml_B_M_w <- as.formula(paste("ln_altM_pond_w ~", frontier_rhs, "|", z_B))
  m_B_M <- frontier::sfa(fml_B_M_w, data = sample_B, ineffDecrease = TRUE, maxit = 5000)
  cat("After winsorizing gamma:", round(coef(m_B_M)["gamma"], 6), "\n")
}

res_B_M <- report_model(m_B_M, "m_B_M (medical)")
saveRDS(m_B_M, file.path(CLEAN_DIR, "sfa_B_M.rds"))

## --- TE correlation Q vs M ---
te_Q <- efficiencies(m_B_Q)
te_M <- efficiencies(m_B_M)
cat("\n--- TE correlation (Q vs M) ---\n")
cat("Pearson:", round(cor(te_Q, te_M), 4), "\n")
cat("Spearman:", round(cor(te_Q, te_M, method = "spearman"), 4), "\n")
cat("(Expected: negative ~-0.14)\n")

## =============================================================
## DESIGN D — Output Distance Function
## =============================================================
cat("\n\n################################################################\n")
cat("# DESIGN D — Output Distance Function\n")
cat("################################################################\n")

sample_D <- sample_B  # same sample
sample_D$neg_ln_altQ <- -sample_D$ln_altQ_pond
sample_D$ln_ratio_MQ <- sample_D$ln_altM_pond - sample_D$ln_altQ_pond

cat("Sample D:", nrow(sample_D), "obs\n")
cat("ln_ratio_MQ: mean =", round(mean(sample_D$ln_ratio_MQ), 3),
    " sd =", round(sd(sample_D$ln_ratio_MQ), 3), "\n")

ccaa_D <- ccaa_B  # same CCAA dummies
z_D <- z_B

fml_D <- as.formula(paste(
  "neg_ln_altQ ~ ln_ratio_MQ +",
  frontier_rhs, "|", z_D
))

cat("\n--- Estimating m_D (ODF) ---\n")
t0 <- Sys.time()
m_D <- frontier::sfa(fml_D, data = sample_D, ineffDecrease = TRUE, maxit = 5000)
cat("Time:", round(difftime(Sys.time(), t0, units = "secs"), 1), "sec\n")

gam_D <- coef(m_D)["gamma"]
if (gam_D >= 0.999) {
  cat("*** gamma at boundary — winsorizing neg_ln_altQ ***\n")
  p1d <- quantile(sample_D$neg_ln_altQ, 0.01, na.rm = TRUE)
  p99d <- quantile(sample_D$neg_ln_altQ, 0.99, na.rm = TRUE)
  sample_D$neg_ln_altQ_w <- pmin(pmax(sample_D$neg_ln_altQ, p1d), p99d)
  fml_D_w <- as.formula(paste("neg_ln_altQ_w ~ ln_ratio_MQ +", frontier_rhs, "|", z_D))
  m_D <- frontier::sfa(fml_D_w, data = sample_D, ineffDecrease = TRUE, maxit = 5000)
}

res_D <- report_model(m_D, "m_D (ODF)")
saveRDS(m_D, file.path(CLEAN_DIR, "sfa_D_odf.rds"))

## =============================================================
## DESIGN C — Panel hospital x service
## =============================================================
cat("\n\n################################################################\n")
cat("# DESIGN C — Panel hospital x service (long format)\n")
cat("################################################################\n")

# Reshape to long: 2 rows per hospital-year
# C_s=1 → surgical (ln_altQ_pond), C_s=0 → medical (ln_altM_pond)
base_cols <- c("NCODI","anyo","ln_L_total_c","ln_K_camas_c","ln_K_tech_c",
               "ln_L_total_c2","ln_K_camas_c2","trend","trend2",
               "d_cluster2","d_cluster3","d_cluster4","d_cluster5",
               "D_desc","pct_sns","ShareQ","d_Priv_Conc","d_Priv_Merc",
               "Conc_shareQ","Merc_shareQ","ccaa_cod",
               ccaa_B)

df_Q <- sample_B[, base_cols]
df_Q$C_s <- 1L
df_Q$ln_output <- sample_B$ln_altQ_pond

df_M <- sample_B[, base_cols]
df_M$C_s <- 0L
df_M$ln_output <- sample_B$ln_altM_pond

sample_C <- bind_rows(df_Q, df_M) %>% arrange(NCODI, anyo, C_s)

# Interaction terms
sample_C$Priv_Conc_Cs <- sample_C$d_Priv_Conc * sample_C$C_s
sample_C$Priv_Merc_Cs <- sample_C$d_Priv_Merc * sample_C$C_s
sample_C$ShareQ_Cs    <- sample_C$ShareQ * sample_C$C_s

cat("Sample C (long):", nrow(sample_C), "obs (", nrow(sample_B), "x 2)\n")

ccaa_C <- ccaa_B  # same
z_C <- paste("d_Priv_Conc + d_Priv_Merc + C_s +",
             "Priv_Conc_Cs + Priv_Merc_Cs +",
             "ShareQ + ShareQ_Cs +",
             paste(ccaa_C, collapse = " + "))

fml_C <- as.formula(paste("ln_output ~", frontier_rhs, "|", z_C))

cat("\n--- Estimating m_C (panel) ---\n")
t0 <- Sys.time()
m_C <- frontier::sfa(fml_C, data = sample_C, ineffDecrease = TRUE, maxit = 5000)
cat("Time:", round(difftime(Sys.time(), t0, units = "secs"), 1), "sec\n")

gam_C <- coef(m_C)["gamma"]
if (gam_C >= 0.999) {
  cat("*** gamma at boundary — winsorizing ln_output ***\n")
  p1c <- quantile(sample_C$ln_output, 0.01, na.rm = TRUE)
  p99c <- quantile(sample_C$ln_output, 0.99, na.rm = TRUE)
  sample_C$ln_output_w <- pmin(pmax(sample_C$ln_output, p1c), p99c)
  fml_C_w <- as.formula(paste("ln_output_w ~", frontier_rhs, "|", z_C))
  m_C <- frontier::sfa(fml_C_w, data = sample_C, ineffDecrease = TRUE, maxit = 5000)
}

res_C <- report_model(m_C, "m_C (panel service)")
saveRDS(m_C, file.path(CLEAN_DIR, "sfa_C_panel.rds"))

## =============================================================
## CONTRAST TABLE — Design A (Total vs Intensity)
## =============================================================
cat("\n\n################################################################\n")
cat("# CONTRAST TABLES\n")
cat("################################################################\n")

make_contrast <- function(m1, m2, vars, label1, label2) {
  c1 <- coef(m1); se1 <- sqrt(diag(vcov(m1)))
  c2 <- coef(m2); se2 <- sqrt(diag(vcov(m2)))
  rows <- lapply(vars, function(v) {
    n1 <- paste0("Z_", v); n2 <- paste0("Z_", v)
    if (!(n1 %in% names(c1)) || !(n2 %in% names(c2))) return(NULL)
    b1 <- c1[n1]; s1 <- se1[n1]
    b2 <- c2[n2]; s2 <- se2[n2]
    z_diff <- (b1 - b2) / sqrt(s1^2 + s2^2)
    p_val <- 2 * (1 - pnorm(abs(z_diff)))
    data.frame(
      variable = v,
      coef_1 = round(b1, 4), se_1 = round(s1, 4),
      coef_2 = round(b2, 4), se_2 = round(s2, 4),
      z_diff = round(z_diff, 3),
      p_value = round(p_val, 4),
      stringsAsFactors = FALSE
    )
  })
  df <- bind_rows(rows)
  names(df)[2:3] <- paste0(c("coef_","se_"), label1)
  names(df)[4:5] <- paste0(c("coef_","se_"), label2)
  df
}

cat("\n--- Contrast Design A: Total vs Intensity ---\n")
vars_A <- c("D_desc","pct_sns","desc_pago","ShareQ","desc_shareQ")
contrast_A <- make_contrast(m_A_Total, m_A_I_w, vars_A, "Total", "Intensity")
print(contrast_A)
write.csv(contrast_A, file.path(CLEAN_DIR, "tabla_contrastes_A.csv"), row.names = FALSE)

cat("\n--- Contrast Design B: Q vs M ---\n")
vars_B <- c("d_Priv_Conc","d_Priv_Merc","ShareQ","Conc_shareQ","Merc_shareQ")
contrast_B <- make_contrast(m_B_Q, m_B_M, vars_B, "Q", "M")
print(contrast_B)
write.csv(contrast_B, file.path(CLEAN_DIR, "tabla_contrastes_B.csv"), row.names = FALSE)

## =============================================================
## P* TEST — Design A
## =============================================================
cat("\n--- P* test Design A ---\n")

# Unrestricted: separate Total and Intensity models
ll_A_unr <- logLik(m_A_Total)[[1]] + logLik(m_A_I_w)[[1]]
cat("Unrestricted logLik (Total + I):", round(ll_A_unr, 2), "\n")

# Restricted: stack both samples, add output_type dummy,
# constrain z-coefficients equal
# Build stacked sample for A
df_A_base <- df_sfa %>%
  filter(
    es_agudo == 1, !anyo %in% 2020:2022,
    altTotal_bruto >= 200, !is.na(D_desc), !is.na(pct_sns),
    is.finite(ln_altTotal_pond), is.finite(ln_i_diag),
    !is.na(ln_L_total_c), !is.na(ccaa_cod)
  )
df_A_base <- recenter_sample(df_A_base)
df_A_base <- df_A_base %>% filter(!is.na(ln_K_tech_c))

# Winsorize ln_i_diag
p1i <- quantile(df_A_base$ln_i_diag, 0.01, na.rm = TRUE)
p99i <- quantile(df_A_base$ln_i_diag, 0.99, na.rm = TRUE)
df_A_base$ln_i_diag_w <- pmin(pmax(df_A_base$ln_i_diag, p1i), p99i)

ccaa_A <- get_ccaa_dummies(df_A_base)

# Stack: output_type=1 → Total, output_type=0 → Intensity
cols_keep <- c("NCODI","anyo","ln_L_total_c","ln_K_camas_c","ln_K_tech_c",
               "ln_L_total_c2","ln_K_camas_c2","trend","trend2",
               "d_cluster2","d_cluster3","d_cluster4","d_cluster5",
               "D_desc","pct_sns","desc_pago","ShareQ","desc_shareQ",
               "ccaa_cod", ccaa_A)

df_stack_T <- df_A_base[, cols_keep]
df_stack_T$output_type <- 1L
df_stack_T$dep_var <- df_A_base$ln_altTotal_pond

df_stack_I <- df_A_base[, cols_keep]
df_stack_I$output_type <- 0L
df_stack_I$dep_var <- df_A_base$ln_i_diag_w

df_stack_A <- bind_rows(df_stack_T, df_stack_I)

z_A_restr <- paste("D_desc + pct_sns + desc_pago + ShareQ + desc_shareQ +",
                   paste(ccaa_A, collapse = " + "))
fml_A_restr <- as.formula(paste(
  "dep_var ~ output_type +", frontier_rhs, "|", z_A_restr
))

cat("Estimating restricted model A (stacked)...\n")
t0 <- Sys.time()
m_A_restr <- tryCatch(
  frontier::sfa(fml_A_restr, data = df_stack_A,
                ineffDecrease = TRUE, maxit = 5000),
  error = function(e) { cat("ERROR:", conditionMessage(e), "\n"); NULL }
)
cat("Time:", round(difftime(Sys.time(), t0, units = "secs"), 1), "sec\n")

if (!is.null(m_A_restr)) {
  ll_A_restr <- logLik(m_A_restr)[[1]]
  n_z_params <- length(grep("^Z_", names(coef(m_A_Total))))
  LR_A <- 2 * (ll_A_unr - ll_A_restr)
  p_A <- pchisq(LR_A, df = n_z_params, lower.tail = FALSE)
  cat(sprintf("Restricted logLik: %.2f\n", ll_A_restr))
  cat(sprintf("LR statistic: %.2f (df=%d)\n", LR_A, n_z_params))
  cat(sprintf("p-value: %.6f\n", p_A))
  if (p_A < 0.05) cat("*** REJECT H0: z-coefficients differ across equations ***\n")
} else {
  cat("Restricted model failed — P* test not computed\n")
}

## =============================================================
## P* TEST — Design B
## =============================================================
cat("\n--- P* test Design B ---\n")

ll_B_unr <- logLik(m_B_Q)[[1]] + logLik(m_B_M)[[1]]
cat("Unrestricted logLik (Q + M):", round(ll_B_unr, 2), "\n")

# Restricted: stack Q and M
cols_B <- c("NCODI","anyo","ln_L_total_c","ln_K_camas_c","ln_K_tech_c",
            "ln_L_total_c2","ln_K_camas_c2","trend","trend2",
            "d_cluster2","d_cluster3","d_cluster4","d_cluster5",
            "D_desc","pct_sns","ShareQ","d_Priv_Conc","d_Priv_Merc",
            "Conc_shareQ","Merc_shareQ","ccaa_cod", ccaa_B)

df_stack_Q <- sample_B[, cols_B]
df_stack_Q$output_type <- 1L
df_stack_Q$dep_var <- sample_B$ln_altQ_pond

df_stack_M <- sample_B[, cols_B]
df_stack_M$output_type <- 0L
# Use winsorized if medical was winsorized
if ("ln_altM_pond_w" %in% names(sample_B)) {
  df_stack_M$dep_var <- sample_B$ln_altM_pond_w
} else {
  df_stack_M$dep_var <- sample_B$ln_altM_pond
}

df_stack_B <- bind_rows(df_stack_Q, df_stack_M)

z_B_restr <- paste("d_Priv_Conc + d_Priv_Merc + Conc_shareQ + Merc_shareQ + ShareQ +",
                   paste(ccaa_B, collapse = " + "))
fml_B_restr <- as.formula(paste(
  "dep_var ~ output_type +", frontier_rhs, "|", z_B_restr
))

cat("Estimating restricted model B (stacked)...\n")
t0 <- Sys.time()
m_B_restr <- tryCatch(
  frontier::sfa(fml_B_restr, data = df_stack_B,
                ineffDecrease = TRUE, maxit = 5000),
  error = function(e) { cat("ERROR:", conditionMessage(e), "\n"); NULL }
)
cat("Time:", round(difftime(Sys.time(), t0, units = "secs"), 1), "sec\n")

if (!is.null(m_B_restr)) {
  ll_B_restr <- logLik(m_B_restr)[[1]]
  n_z_params_B <- length(grep("^Z_", names(coef(m_B_Q))))
  LR_B <- 2 * (ll_B_unr - ll_B_restr)
  p_B <- pchisq(LR_B, df = n_z_params_B, lower.tail = FALSE)
  cat(sprintf("Restricted logLik: %.2f\n", ll_B_restr))
  cat(sprintf("LR statistic: %.2f (df=%d)\n", LR_B, n_z_params_B))
  cat(sprintf("p-value: %.6f\n", p_B))
  if (p_B < 0.05) cat("*** REJECT H0: z-coefficients differ across equations ***\n")
} else {
  cat("Restricted model failed — P* test not computed\n")
}

## =============================================================
## FULL RESULTS REPORT
## =============================================================
cat("\n\n################################################################\n")
cat("# FULL RESULTS REPORT\n")
cat("################################################################\n")

sink(file.path(CLEAN_DIR, "resultados_todos_disenos.txt"))

cat("=== RESULTS: ALL DESIGNS ===\n")
cat("Generated:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n\n")

cat("--- Design A: Total ---\n")
report_model(m_A_Total, "m_A_Total")
cat("\n--- Design A: Intensity (winsorized) ---\n")
report_model(m_A_I_w, "m_A_I_winsorized")
cat("\n--- Design A: Contrast ---\n")
print(contrast_A)

cat("\n\n--- Design B: Surgical (Q) ---\n")
report_model(m_B_Q, "m_B_Q")
cat("\n--- Design B: Medical (M) ---\n")
report_model(m_B_M, "m_B_M")
cat("\n--- Design B: Contrast ---\n")
print(contrast_B)
cat("\n--- TE correlation Q vs M ---\n")
cat("Pearson:", round(cor(te_Q, te_M), 4), "\n")
cat("Spearman:", round(cor(te_Q, te_M, method = "spearman"), 4), "\n")

cat("\n\n--- Design C: Panel service ---\n")
report_model(m_C, "m_C_panel")

cat("\n\n--- Design D: Output Distance Function ---\n")
report_model(m_D, "m_D_ODF")

if (!is.null(m_A_restr)) {
  cat(sprintf("\n\nP* test A: LR=%.2f df=%d p=%.6f\n", LR_A, n_z_params, p_A))
}
if (!is.null(m_B_restr)) {
  cat(sprintf("P* test B: LR=%.2f df=%d p=%.6f\n", LR_B, n_z_params_B, p_B))
}

sink()

cat("\nAll results saved to data_clean/resultados_todos_disenos.txt\n")
cat("Contrast tables saved to data_clean/tabla_contrastes_A.csv and _B.csv\n")

cat("\n========== Phase 7 complete ==========\n")
