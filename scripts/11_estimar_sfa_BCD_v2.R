## =============================================================
## Phase 7 — Designs B, C, D
## v2:
##   - B and C use service-specific labor inputs (L_quirur / L_medico)
##   - B uses common sample for direct Q vs M comparison
##   - C is treated as stacked service design (not a true panel-SFA estimator)
##   - D remains a general ODF benchmark
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
}

mk_contrast <- function(m1, m2, vars, l1, l2) {
  c1 <- coef(m1); s1 <- sqrt(diag(vcov(m1)))
  c2 <- coef(m2); s2 <- sqrt(diag(vcov(m2)))
  rows <- lapply(vars, function(v) {
    nm1 <- paste0("Z_", v); nm2 <- paste0("Z_", v)
    if (!(nm1 %in% names(c1)) || !(nm2 %in% names(c2))) return(NULL)
    z_diff <- (c1[nm1] - c2[nm2]) / sqrt(s1[nm1]^2 + s2[nm2]^2)
    data.frame(
      variable = v,
      coef_1 = c1[nm1], se_1 = s1[nm1],
      coef_2 = c2[nm2], se_2 = s2[nm2],
      z_diff = z_diff,
      p_value = 2 * (1 - pnorm(abs(z_diff)))
    )
  })
  out <- dplyr::bind_rows(rows)
  names(out)[2:3] <- paste0(c("coef_", "se_"), l1)
  names(out)[4:5] <- paste0(c("coef_", "se_"), l2)
  out
}

## ---------- enrich df_sfa ----------
need_vars <- c("L_total","L_quirur","L_medico","K_camas","K_tech_index",
               "altQ_bruto","altM_bruto","ln_altQ_pond","ln_altM_pond",
               "D_desc","pct_sns","ShareQ","ccaa_cod","es_agudo",
               "d_cluster2","d_cluster3","d_cluster4","d_cluster5",
               paste0("d_ccaa_", setdiff(1:17, 9)))
df_sfa <- add_missing_from_final(df_sfa, df_final, need_vars)

cat("========== Phase 7: Designs B, C, D ==========\n")
cat("df_sfa:", nrow(df_sfa), "x", ncol(df_sfa), "\n")

## =============================================================
## DESIGN B — common sample Q and M
## =============================================================
sample_B <- df_sfa %>%
  filter(
    es_agudo == 1,
    !(anyo %in% 2020:2022),
    altTotal_bruto >= 200,
    altQ_bruto >= 200,
    altM_bruto >= 200,
    is.finite(ln_altQ_pond),
    is.finite(ln_altM_pond),
    !is.na(D_desc),
    !is.na(pct_sns),
    !is.na(ccaa_cod)
  )

# payment groups on this sample
sample_B$d_Priv_Conc <- as.integer(sample_B$D_desc == 1 & sample_B$pct_sns >= 0.50)
sample_B$d_Priv_Merc <- as.integer(sample_B$D_desc == 1 & sample_B$pct_sns < 0.50)
sample_B$Conc_shareQ <- sample_B$d_Priv_Conc * sample_B$ShareQ
sample_B$Merc_shareQ <- sample_B$d_Priv_Merc * sample_B$ShareQ

# recenter service-specific labor + common K
sample_B$ln_L_quirur_raw <- safe_log1(to_num(sample_B$L_quirur))
sample_B$ln_L_medico_raw <- safe_log1(to_num(sample_B$L_medico))
sample_B$ln_K_camas_raw  <- safe_log1(to_num(sample_B$K_camas))
sample_B$ln_K_tech_raw   <- safe_log1(to_num(sample_B$K_tech_index))

mu_LQ <- mean(sample_B$ln_L_quirur_raw, na.rm = TRUE)
mu_LM <- mean(sample_B$ln_L_medico_raw, na.rm = TRUE)
mu_KC <- mean(sample_B$ln_K_camas_raw,  na.rm = TRUE)
mu_KT <- mean(sample_B$ln_K_tech_raw,   na.rm = TRUE)

sample_B$ln_L_quirur_c <- sample_B$ln_L_quirur_raw - mu_LQ
sample_B$ln_L_medico_c <- sample_B$ln_L_medico_raw - mu_LM
sample_B$ln_K_camas_c  <- sample_B$ln_K_camas_raw  - mu_KC
sample_B$ln_K_tech_c   <- sample_B$ln_K_tech_raw   - mu_KT

sample_B$ln_L_quirur_c2 <- 0.5 * sample_B$ln_L_quirur_c^2
sample_B$ln_L_medico_c2 <- 0.5 * sample_B$ln_L_medico_c^2
sample_B$ln_K_camas_c2  <- 0.5 * sample_B$ln_K_camas_c^2
sample_B$ln_K_tech_c2   <- 0.5 * sample_B$ln_K_tech_c^2

ccaa_B <- get_ccaa_dummies(sample_B, min_n = 30, ref = 9L)

z_B <- paste(c("d_Priv_Conc","d_Priv_Merc","ShareQ","Conc_shareQ","Merc_shareQ", ccaa_B),
             collapse = " + ")

rhs_Q <- paste(c(
  "ln_L_quirur_c","ln_K_camas_c","ln_K_tech_c",
  "ln_L_quirur_c2","ln_K_camas_c2","ln_K_tech_c2",
  "trend","trend2","d_cluster2","d_cluster3","d_cluster4","d_cluster5"
), collapse = " + ")

rhs_M <- paste(c(
  "ln_L_medico_c","ln_K_camas_c","ln_K_tech_c",
  "ln_L_medico_c2","ln_K_camas_c2","ln_K_tech_c2",
  "trend","trend2","d_cluster2","d_cluster3","d_cluster4","d_cluster5"
), collapse = " + ")

need_BQ <- unique(c("ln_altQ_pond", strsplit(rhs_Q, " \\+ ")[[1]], strsplit(z_B, " \\+ ")[[1]]))
need_BM <- unique(c("ln_altM_pond", strsplit(rhs_M, " \\+ ")[[1]], strsplit(z_B, " \\+ ")[[1]]))
sample_BQ <- sample_B %>% filter(if_all(all_of(need_BQ), ~ !is.na(.x)))
sample_BM <- sample_B %>% filter(if_all(all_of(need_BM), ~ !is.na(.x)))

cat("Sample BQ:", nrow(sample_BQ), "obs\n")
cat("Sample BM:", nrow(sample_BM), "obs\n")

fml_BQ <- as.formula(paste("ln_altQ_pond ~", rhs_Q, "|", z_B))
fml_BM <- as.formula(paste("ln_altM_pond ~", rhs_M, "|", z_B))

cat("\n--- Estimating B_Q ---\n")
m_BQ <- frontier::sfa(fml_BQ, data = sample_BQ, ineffDecrease = TRUE, maxit = 5000)
report_model(m_BQ, "B_Q")
saveRDS(m_BQ, file.path(CLEAN_DIR, "sfa_B_Q_v2.rds"))

cat("\n--- Estimating B_M ---\n")
m_BM <- frontier::sfa(fml_BM, data = sample_BM, ineffDecrease = TRUE, maxit = 5000)
report_model(m_BM, "B_M")
saveRDS(m_BM, file.path(CLEAN_DIR, "sfa_B_M_v2.rds"))

contrast_B <- mk_contrast(m_BQ, m_BM,
                          c("d_Priv_Conc","d_Priv_Merc","ShareQ","Conc_shareQ","Merc_shareQ"),
                          "Q","M")
write.csv(contrast_B, file.path(CLEAN_DIR, "tabla_contrastes_B_v2.csv"), row.names = FALSE)

## =============================================================
## DESIGN C — stacked service design
## =============================================================
base_cols <- c("NCODI","anyo","K_camas","K_tech_index","D_desc","pct_sns","ShareQ",
               "d_Priv_Conc","d_Priv_Merc","Conc_shareQ","Merc_shareQ",
               "ccaa_cod","trend","trend2",
               "d_cluster2","d_cluster3","d_cluster4","d_cluster5", ccaa_B)

df_Q <- sample_BQ[, base_cols, drop = FALSE]
df_Q$service <- "Q"
df_Q$C_s <- 1L
df_Q$L_serv <- sample_BQ$L_quirur
df_Q$ln_output <- sample_BQ$ln_altQ_pond

df_M <- sample_BM[, base_cols, drop = FALSE]
df_M$service <- "M"
df_M$C_s <- 0L
df_M$L_serv <- sample_BM$L_medico
df_M$ln_output <- sample_BM$ln_altM_pond

sample_C <- bind_rows(df_Q, df_M) %>% arrange(NCODI, anyo, desc(C_s))
sample_C$ln_L_serv_raw <- safe_log1(to_num(sample_C$L_serv))
sample_C$ln_K_camas_raw <- safe_log1(to_num(sample_C$K_camas))
sample_C$ln_K_tech_raw  <- safe_log1(to_num(sample_C$K_tech_index))

mu_LS <- mean(sample_C$ln_L_serv_raw, na.rm = TRUE)
mu_KC2 <- mean(sample_C$ln_K_camas_raw, na.rm = TRUE)
mu_KT2 <- mean(sample_C$ln_K_tech_raw, na.rm = TRUE)

sample_C$ln_L_serv_c <- sample_C$ln_L_serv_raw - mu_LS
sample_C$ln_K_camas_c <- sample_C$ln_K_camas_raw - mu_KC2
sample_C$ln_K_tech_c  <- sample_C$ln_K_tech_raw  - mu_KT2
sample_C$ln_L_serv_c2 <- 0.5 * sample_C$ln_L_serv_c^2
sample_C$ln_K_camas_c2 <- 0.5 * sample_C$ln_K_camas_c^2
sample_C$ln_K_tech_c2  <- 0.5 * sample_C$ln_K_tech_c^2

sample_C$Priv_Conc_Cs <- sample_C$d_Priv_Conc * sample_C$C_s
sample_C$Priv_Merc_Cs <- sample_C$d_Priv_Merc * sample_C$C_s
sample_C$ShareQ_Cs    <- sample_C$ShareQ * sample_C$C_s

rhs_C <- paste(c(
  "C_s","ln_L_serv_c","ln_K_camas_c","ln_K_tech_c",
  "I(C_s * ln_L_serv_c)",
  "ln_L_serv_c2","ln_K_camas_c2","ln_K_tech_c2",
  "trend","trend2","d_cluster2","d_cluster3","d_cluster4","d_cluster5"
), collapse = " + ")

z_C <- paste(c("d_Priv_Conc","d_Priv_Merc","C_s",
               "Priv_Conc_Cs","Priv_Merc_Cs",
               "ShareQ","ShareQ_Cs", ccaa_B), collapse = " + ")

need_C <- unique(c("ln_output", gsub("I\\(|\\)", "", strsplit(rhs_C, " \\+ ")[[1]]), strsplit(z_C, " \\+ ")[[1]]))
need_C <- setdiff(need_C, c("C_s * ln_L_serv_c"))
sample_C_est <- sample_C %>% filter(!is.na(ln_output), !is.na(ln_L_serv_c), !is.na(ln_K_camas_c), !is.na(ln_K_tech_c))

fml_C <- as.formula(paste("ln_output ~", rhs_C, "|", z_C))

cat("\n--- Estimating C (stacked service) ---\n")
m_C <- frontier::sfa(fml_C, data = sample_C_est, ineffDecrease = TRUE, maxit = 5000)
report_model(m_C, "C stacked service")
saveRDS(m_C, file.path(CLEAN_DIR, "sfa_C_stacked_service_v2.rds"))

## =============================================================
## DESIGN D — ODF benchmark
## =============================================================
sample_D <- sample_B %>%
  filter(is.finite(ln_altQ_pond), is.finite(ln_altM_pond))

sample_D$ln_L_total_raw <- safe_log1(to_num(sample_D$L_total))
sample_D$ln_K_camas_raw <- safe_log1(to_num(sample_D$K_camas))
sample_D$ln_K_tech_raw  <- safe_log1(to_num(sample_D$K_tech_index))

mu_LT <- mean(sample_D$ln_L_total_raw, na.rm = TRUE)
mu_KCD <- mean(sample_D$ln_K_camas_raw, na.rm = TRUE)
mu_KTD <- mean(sample_D$ln_K_tech_raw, na.rm = TRUE)

sample_D$ln_L_total_c <- sample_D$ln_L_total_raw - mu_LT
sample_D$ln_K_camas_c <- sample_D$ln_K_camas_raw - mu_KCD
sample_D$ln_K_tech_c  <- sample_D$ln_K_tech_raw  - mu_KTD
sample_D$ln_L_total_c2 <- 0.5 * sample_D$ln_L_total_c^2
sample_D$ln_K_camas_c2 <- 0.5 * sample_D$ln_K_camas_c^2
sample_D$ln_K_tech_c2  <- 0.5 * sample_D$ln_K_tech_c^2

sample_D$neg_ln_altQ <- -sample_D$ln_altQ_pond
sample_D$ln_ratio_MQ <- sample_D$ln_altM_pond - sample_D$ln_altQ_pond

rhs_D <- paste(c(
  "ln_ratio_MQ",
  "ln_L_total_c","ln_K_camas_c","ln_K_tech_c",
  "ln_L_total_c2","ln_K_camas_c2","ln_K_tech_c2",
  "trend","trend2","d_cluster2","d_cluster3","d_cluster4","d_cluster5"
), collapse = " + ")

z_D <- paste(c("D_desc","pct_sns","desc_pago","ShareQ","desc_shareQ", ccaa_B), collapse = " + ")
need_D <- unique(c("neg_ln_altQ", strsplit(rhs_D, " \\+ ")[[1]], strsplit(z_D, " \\+ ")[[1]]))
sample_D_est <- sample_D %>% filter(if_all(all_of(need_D), ~ !is.na(.x)))

fml_D <- as.formula(paste("neg_ln_altQ ~", rhs_D, "|", z_D))

cat("\n--- Estimating D (ODF) ---\n")
m_D <- frontier::sfa(fml_D, data = sample_D_est, ineffDecrease = TRUE, maxit = 5000)
report_model(m_D, "D ODF")
saveRDS(m_D, file.path(CLEAN_DIR, "sfa_D_odf_v2.rds"))

cat("\nSaved:\n")
cat(" - sfa_B_Q_v2.rds\n")
cat(" - sfa_B_M_v2.rds\n")
cat(" - tabla_contrastes_B_v2.csv\n")
cat(" - sfa_C_stacked_service_v2.rds\n")
cat(" - sfa_D_odf_v2.rds\n")
cat("\n========== Phase 7 complete ==========\n")
