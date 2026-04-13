## =============================================================
## Script 13 — Robustness analysis
## Specs: R1 COVID included, R2 Cobb-Douglas, R3 hnormal,
##        R4 alternative activity threshold, R5 CNH-only D_desc
## =============================================================

setwd("G:/Mi unidad/SIAE 2010-2020")
suppressPackageStartupMessages({
  library(dplyr)
  library(frontier)
  library(moments)
})

CLEAN_DIR <- "data_clean"
ROB_DIR   <- "outputs/robustez"
dir.create(ROB_DIR, showWarnings=FALSE, recursive=TRUE)

load(file.path(CLEAN_DIR, "df_sfa.RData"))
load(file.path(CLEAN_DIR, "df_final.RData"))

## ── helpers ──────────────────────────────────────────────────
to_num   <- function(x) suppressWarnings(as.numeric(x))
safe_log1 <- function(x) ifelse(!is.na(x), log(pmax(x, 0) + 1), NA_real_)

add_missing_from_final <- function(df_small, df_big, vars) {
  key_s <- paste(df_small$NCODI, df_small$anyo)
  key_b <- paste(df_big$NCODI, df_big$anyo)
  idx   <- match(key_s, key_b)
  for (v in vars) {
    if (!(v %in% names(df_small)) && v %in% names(df_big))
      df_small[[v]] <- df_big[[v]][idx]
  }
  df_small
}

get_ccaa_dummies <- function(df, min_n=30, ref=9L) {
  counts <- table(df$ccaa_cod)
  keep   <- as.integer(names(counts[counts >= min_n]))
  keep   <- setdiff(keep, ref)
  dums   <- paste0("d_ccaa_", keep)
  dums[dums %in% names(df)]
}

report_model <- function(m, label) {
  cat("\n================", label, "================\n")
  cat("logLik:", round(logLik(m)[[1]], 2), "\n")
  cat("N:", nobs(m), "\n")
  te <- efficiencies(m)
  cat("TE mean:", round(mean(te), 4), "\n")
  co <- coef(m)
  if ("gamma" %in% names(co)) cat("gamma:", round(co["gamma"], 6), "\n")
  se <- tryCatch(sqrt(diag(vcov(m))), error=function(e) NULL)
  if (!is.null(se)) {
    zidx <- grep("^Z_", names(co))
    for (i in zidx)
      cat(sprintf("  %-20s %10.4f  se=%8.4f  t=%7.3f\n",
                  names(co)[i], co[i], se[i], co[i]/se[i]))
  }
}

mk_coef_tab <- function(m, vars) {
  co  <- coef(m)
  se  <- tryCatch(sqrt(diag(vcov(m))), error=function(e) NULL)
  out <- data.frame(param=vars, coef=NA_real_, se=NA_real_, t=NA_real_)
  if (is.null(se)) return(out)
  for (i in seq_along(vars)) {
    nm <- paste0("Z_", vars[i])
    if (nm %in% names(co)) {
      out$coef[i] <- co[nm]
      out$se[i]   <- se[nm]
      out$t[i]    <- co[nm]/se[nm]
    }
  }
  out
}

## ── enrich df_sfa ────────────────────────────────────────────
need_vars <- c("L_total","K_camas","K_tech_index","ln_altTotal_pond",
               "altTotal_bruto","D_desc","pct_sns","ShareQ","desc_pago",
               "desc_shareQ","ccaa_cod","es_agudo",
               "d_cluster2","d_cluster3","d_cluster4","d_cluster5",
               paste0("d_ccaa_", setdiff(1:17, 9)))
df_sfa <- add_missing_from_final(df_sfa, df_final, need_vars)

## ── BASE SAMPLE — identical to 10_estimar_sfa_A_v2.R ────────
flag_Q <- with(df_sfa,
  es_agudo == 1 &
  !(anyo %in% 2020:2022) &
  to_num(altTotal_bruto) >= 200 &
  !is.na(D_desc) &
  !is.na(pct_sns) &
  is.finite(to_num(ln_altTotal_pond)) &
  !is.na(ccaa_cod))

df_base <- df_sfa[flag_Q, , drop=FALSE]
cat("Base sample N =", nrow(df_base), "\n")

## Recentre inputs on base sample
df_base$ln_L_raw  <- safe_log1(to_num(df_base$L_total))
df_base$ln_Kc_raw <- safe_log1(to_num(df_base$K_camas))
df_base$ln_Kt_raw <- safe_log1(to_num(df_base$K_tech_index))
mu_L <- mean(df_base$ln_L_raw,  na.rm=TRUE)
mu_K <- mean(df_base$ln_Kc_raw, na.rm=TRUE)
mu_T <- mean(df_base$ln_Kt_raw, na.rm=TRUE)
df_base$ln_L_total_c  <- df_base$ln_L_raw  - mu_L
df_base$ln_K_camas_c  <- df_base$ln_Kc_raw - mu_K
df_base$ln_K_tech_c   <- df_base$ln_Kt_raw - mu_T
df_base$ln_L_total_c2 <- 0.5 * df_base$ln_L_total_c^2
df_base$ln_K_camas_c2 <- 0.5 * df_base$ln_K_camas_c^2
df_base$ln_K_tech_c2  <- 0.5 * df_base$ln_K_tech_c^2

ccaa_dummies <- get_ccaa_dummies(df_base, min_n=30, ref=9L)

frontier_rhs_TL <- paste(c(
  "ln_L_total_c","ln_K_camas_c","ln_K_tech_c",
  "ln_L_total_c2","ln_K_camas_c2","ln_K_tech_c2",
  "trend","trend2","d_cluster2","d_cluster3","d_cluster4","d_cluster5"
), collapse=" + ")

frontier_rhs_CD <- paste(c(
  "ln_L_total_c","ln_K_camas_c","ln_K_tech_c",
  "trend","trend2","d_cluster2","d_cluster3","d_cluster4","d_cluster5"
), collapse=" + ")

z_rhs <- paste(c("D_desc","pct_sns","desc_pago", ccaa_dummies), collapse=" + ")

fml_TL <- as.formula(paste("ln_altTotal_pond ~", frontier_rhs_TL, "|", z_rhs))
fml_CD <- as.formula(paste("ln_altTotal_pond ~", frontier_rhs_CD, "|", z_rhs))

z_vars_main <- c("D_desc","pct_sns","desc_pago")

## ── Complete cases for base ───────────────────────────────────
all_vars_TL <- c("ln_altTotal_pond","ln_L_total_c","ln_K_camas_c","ln_K_tech_c",
                 "ln_L_total_c2","ln_K_camas_c2","ln_K_tech_c2",
                 "trend","trend2","d_cluster2","d_cluster3","d_cluster4","d_cluster5",
                 "D_desc","pct_sns","desc_pago", ccaa_dummies)

df_cc <- df_base %>% filter(if_all(all_of(intersect(all_vars_TL, names(.))), ~ !is.na(.x)))
cat("Base complete cases:", nrow(df_cc), "\n")

## ── Reference model (load from data_clean) ───────────────────
m_ref_Q <- readRDS(file.path(CLEAN_DIR, "sfa_A_total_v2.rds"))
m_ref_I <- readRDS(file.path(CLEAN_DIR, "sfa_A_intensity_v2.rds"))
ll_TL   <- logLik(m_ref_Q)[[1]]

## =============================================================
## R1: COVID years included with covid dummy
## =============================================================
cat("\n========== R1: COVID years included ==========\n")

R1_out_Q <- file.path(ROB_DIR, "sfa_R1_covid_Total.rds")
R1_out_I <- file.path(ROB_DIR, "sfa_R1_covid_Intensity.rds")

if (!file.exists(R1_out_Q) || !file.exists(R1_out_I)) {

  flag_R1 <- with(df_sfa,
    es_agudo == 1 &
    to_num(altTotal_bruto) >= 200 &
    !is.na(D_desc) &
    !is.na(pct_sns) &
    is.finite(to_num(ln_altTotal_pond)) &
    !is.na(ccaa_cod))

  df_R1 <- df_sfa[flag_R1, , drop=FALSE]
  df_R1$ln_L_raw  <- safe_log1(to_num(df_R1$L_total))
  df_R1$ln_Kc_raw <- safe_log1(to_num(df_R1$K_camas))
  df_R1$ln_Kt_raw <- safe_log1(to_num(df_R1$K_tech_index))
  df_R1$ln_L_total_c  <- df_R1$ln_L_raw  - mean(df_R1$ln_L_raw,  na.rm=TRUE)
  df_R1$ln_K_camas_c  <- df_R1$ln_Kc_raw - mean(df_R1$ln_Kc_raw, na.rm=TRUE)
  df_R1$ln_K_tech_c   <- df_R1$ln_Kt_raw - mean(df_R1$ln_Kt_raw, na.rm=TRUE)
  df_R1$ln_L_total_c2 <- 0.5 * df_R1$ln_L_total_c^2
  df_R1$ln_K_camas_c2 <- 0.5 * df_R1$ln_K_camas_c^2
  df_R1$ln_K_tech_c2  <- 0.5 * df_R1$ln_K_tech_c^2
  df_R1$d_covid       <- as.integer(to_num(df_R1$anyo) %in% 2020:2022)

  cc_R1 <- get_ccaa_dummies(df_R1, min_n=30, ref=9L)
  z_rhs_R1 <- paste(c("D_desc","pct_sns","desc_pago","d_covid", cc_R1), collapse=" + ")
  fml_R1 <- as.formula(paste("ln_altTotal_pond ~", frontier_rhs_TL, "|", z_rhs_R1))

  all_v_R1 <- c("ln_altTotal_pond","ln_L_total_c","ln_K_camas_c","ln_K_tech_c",
                "ln_L_total_c2","ln_K_camas_c2","ln_K_tech_c2",
                "trend","trend2","d_cluster2","d_cluster3","d_cluster4","d_cluster5",
                "D_desc","pct_sns","desc_pago","d_covid", cc_R1)
  df_R1_cc <- df_R1 %>% filter(if_all(all_of(intersect(all_v_R1, names(.))), ~ !is.na(.x)))
  cat("R1 complete cases:", nrow(df_R1_cc), "\n")

  m_R1_Q <- tryCatch(
    frontier::sfa(fml_R1, data=df_R1_cc, ineffDecrease=TRUE, maxit=5000),
    error=function(e) { cat("R1 Total ERROR:", conditionMessage(e), "\n"); NULL }
  )
  if (!is.null(m_R1_Q)) { report_model(m_R1_Q, "R1 COVID Total"); saveRDS(m_R1_Q, R1_out_Q) }

  ## R1 Intensity
  flag_R1_I <- flag_R1 & !is.na(to_num(df_sfa$i_diag)) & to_num(df_sfa$i_diag) >= 1.0 &
               is.finite(to_num(df_sfa$ln_i_diag_w %||% df_sfa$ln_i_diag))
  # Simpler: use the same df_R1_cc and swap dep var
  if ("ln_i_diag_w" %in% names(df_sfa) || "ln_i_diag" %in% names(df_sfa)) {
    dep_i <- if ("ln_i_diag_w" %in% names(df_sfa)) "ln_i_diag_w" else "ln_i_diag"
    df_R1_I <- df_sfa[flag_R1 & !is.na(to_num(df_sfa$i_diag)) &
                       to_num(df_sfa$i_diag) >= 1.0, , drop=FALSE]
    df_R1_I$ln_L_raw  <- safe_log1(to_num(df_R1_I$L_total))
    df_R1_I$ln_Kc_raw <- safe_log1(to_num(df_R1_I$K_camas))
    df_R1_I$ln_Kt_raw <- safe_log1(to_num(df_R1_I$K_tech_index))
    df_R1_I$ln_L_total_c  <- df_R1_I$ln_L_raw  - mean(df_R1_I$ln_L_raw,  na.rm=TRUE)
    df_R1_I$ln_K_camas_c  <- df_R1_I$ln_Kc_raw - mean(df_R1_I$ln_Kc_raw, na.rm=TRUE)
    df_R1_I$ln_K_tech_c   <- df_R1_I$ln_Kt_raw - mean(df_R1_I$ln_Kt_raw, na.rm=TRUE)
    df_R1_I$ln_L_total_c2 <- 0.5 * df_R1_I$ln_L_total_c^2
    df_R1_I$ln_K_camas_c2 <- 0.5 * df_R1_I$ln_K_camas_c^2
    df_R1_I$ln_K_tech_c2  <- 0.5 * df_R1_I$ln_K_tech_c^2
    df_R1_I$d_covid        <- as.integer(to_num(df_R1_I$anyo) %in% 2020:2022)

    cc_R1_I <- get_ccaa_dummies(df_R1_I, min_n=30, ref=9L)
    z_rhs_R1_I <- paste(c("D_desc","pct_sns","desc_pago","d_covid", cc_R1_I), collapse=" + ")
    fml_R1_I <- as.formula(paste(dep_i, "~", frontier_rhs_TL, "|", z_rhs_R1_I))
    all_v_R1_I <- c(dep_i, "ln_L_total_c","ln_K_camas_c","ln_K_tech_c",
                    "ln_L_total_c2","ln_K_camas_c2","ln_K_tech_c2",
                    "trend","trend2","d_cluster2","d_cluster3","d_cluster4","d_cluster5",
                    "D_desc","pct_sns","desc_pago","d_covid", cc_R1_I)
    df_R1_I_cc <- df_R1_I %>% filter(if_all(all_of(intersect(all_v_R1_I, names(.))), ~ !is.na(.x)))
    cat("R1 Intensity complete cases:", nrow(df_R1_I_cc), "\n")
    m_R1_I <- tryCatch(
      frontier::sfa(fml_R1_I, data=df_R1_I_cc, ineffDecrease=TRUE, maxit=5000),
      error=function(e) { cat("R1 Intensity ERROR:", conditionMessage(e), "\n"); NULL }
    )
    if (!is.null(m_R1_I)) { report_model(m_R1_I, "R1 COVID Intensity"); saveRDS(m_R1_I, R1_out_I) }
  }
} else {
  cat("R1 files already exist — skipping.\n")
  m_R1_Q <- readRDS(R1_out_Q)
  m_R1_I <- if (file.exists(R1_out_I)) readRDS(R1_out_I) else NULL
}

## =============================================================
## R2: Cobb-Douglas functional form
## =============================================================
cat("\n========== R2: Cobb-Douglas functional form ==========\n")

R2_out_Q <- file.path(ROB_DIR, "sfa_R2_CD_Total.rds")
R2_out_I <- file.path(ROB_DIR, "sfa_R2_CD_Intensity.rds")

if (!file.exists(R2_out_Q) || !file.exists(R2_out_I)) {

  all_vars_CD <- c("ln_altTotal_pond","ln_L_total_c","ln_K_camas_c","ln_K_tech_c",
                   "trend","trend2","d_cluster2","d_cluster3","d_cluster4","d_cluster5",
                   "D_desc","pct_sns","desc_pago", ccaa_dummies)
  df_CD_cc <- df_cc %>% filter(if_all(all_of(intersect(all_vars_CD, names(.))), ~ !is.na(.x)))
  cat("R2 (CD) complete cases:", nrow(df_CD_cc), "\n")

  m_R2_Q <- tryCatch(
    frontier::sfa(fml_CD, data=df_CD_cc, ineffDecrease=TRUE, maxit=5000),
    error=function(e) { cat("R2 Total ERROR:", conditionMessage(e), "\n"); NULL }
  )
  if (!is.null(m_R2_Q)) {
    report_model(m_R2_Q, "R2 CD Total")
    saveRDS(m_R2_Q, R2_out_Q)
    # LR test
    ll_CD <- logLik(m_R2_Q)[[1]]
    LR    <- 2*(ll_TL - ll_CD)
    df_lr <- 5  # quadratic + cross terms removed
    p_lr  <- pchisq(LR, df=df_lr, lower.tail=FALSE)
    cat(sprintf("LR test Translog vs CD: LR=%.2f df=%d p=%.4f\n", LR, df_lr, p_lr))
  }

  ## R2 Intensity
  dep_i <- if ("ln_i_diag_w" %in% names(df_sfa)) "ln_i_diag_w" else "ln_i_diag"
  if (dep_i %in% names(df_cc)) {
    fml_CD_I <- as.formula(paste(dep_i, "~", frontier_rhs_CD, "|", z_rhs))
    df_CD_I_cc <- df_cc %>%
      filter(!is.na(.data[[dep_i]]), is.finite(.data[[dep_i]]))
    cat("R2 Intensity complete cases:", nrow(df_CD_I_cc), "\n")
    m_R2_I <- tryCatch(
      frontier::sfa(fml_CD_I, data=df_CD_I_cc, ineffDecrease=TRUE, maxit=5000),
      error=function(e) { cat("R2 Intensity ERROR:", conditionMessage(e), "\n"); NULL }
    )
    if (!is.null(m_R2_I)) { report_model(m_R2_I, "R2 CD Intensity"); saveRDS(m_R2_I, R2_out_I) }
  } else {
    cat("Intensity dep var not available in df_cc — skipping R2 Intensity.\n")
    m_R2_I <- NULL
  }
} else {
  cat("R2 files already exist — skipping.\n")
  m_R2_Q <- readRDS(R2_out_Q)
  m_R2_I <- if (file.exists(R2_out_I)) readRDS(R2_out_I) else NULL
}

## =============================================================
## R3: Half-normal distribution (truncNorm=FALSE)
## =============================================================
cat("\n========== R3: Half-normal distribution ==========\n")

R3_out_Q <- file.path(ROB_DIR, "sfa_R3_hnormal_Total.rds")
R3_out_I <- file.path(ROB_DIR, "sfa_R3_hnormal_Intensity.rds")

if (!file.exists(R3_out_Q) || !file.exists(R3_out_I)) {

  m_R3_Q <- tryCatch(
    frontier::sfa(fml_TL, data=df_cc, ineffDecrease=TRUE, truncNorm=FALSE, maxit=5000),
    error=function(e) { cat("R3 Total ERROR:", conditionMessage(e), "\n"); NULL }
  )
  if (!is.null(m_R3_Q)) { report_model(m_R3_Q, "R3 Half-normal Total"); saveRDS(m_R3_Q, R3_out_Q) }

  dep_i <- if ("ln_i_diag_w" %in% names(df_cc)) "ln_i_diag_w" else
           if ("ln_i_diag"   %in% names(df_cc)) "ln_i_diag"   else NULL
  if (!is.null(dep_i)) {
    fml_TL_I <- as.formula(paste(dep_i, "~", frontier_rhs_TL, "|", z_rhs))
    df_cc_I  <- df_cc %>% filter(!is.na(.data[[dep_i]]), is.finite(.data[[dep_i]]))
    cat("R3 Intensity complete cases:", nrow(df_cc_I), "\n")
    m_R3_I <- tryCatch(
      frontier::sfa(fml_TL_I, data=df_cc_I, ineffDecrease=TRUE, truncNorm=FALSE, maxit=5000),
      error=function(e) { cat("R3 Intensity ERROR:", conditionMessage(e), "\n"); NULL }
    )
    if (!is.null(m_R3_I)) { report_model(m_R3_I, "R3 Half-normal Intensity"); saveRDS(m_R3_I, R3_out_I) }
  } else {
    cat("Intensity dep var not available — skipping R3 Intensity.\n")
    m_R3_I <- NULL
  }
} else {
  cat("R3 files already exist — skipping.\n")
  m_R3_Q <- readRDS(R3_out_Q)
  m_R3_I <- if (file.exists(R3_out_I)) readRDS(R3_out_I) else NULL
}

## =============================================================
## R4: Alternative activity thresholds (100 and 300)
## =============================================================
cat("\n========== R4: Alternative activity thresholds ==========\n")

for (thr in c(100, 300)) {
  tag <- paste0("thr", thr)
  out_Q <- file.path(ROB_DIR, paste0("sfa_R4a_", tag, "_Total.rds"))
  out_I <- file.path(ROB_DIR, paste0("sfa_R4a_", tag, "_Intensity.rds"))

  if (!file.exists(out_Q)) {
    cat(sprintf("\nR4 threshold=%d\n", thr))

    flag_R4 <- with(df_sfa,
      es_agudo == 1 &
      !(to_num(anyo) %in% 2020:2022) &
      to_num(altTotal_bruto) >= thr &
      !is.na(D_desc) & !is.na(pct_sns) &
      is.finite(to_num(ln_altTotal_pond)) &
      !is.na(ccaa_cod))

    df_R4 <- df_sfa[flag_R4, , drop=FALSE]
    df_R4$ln_L_raw  <- safe_log1(to_num(df_R4$L_total))
    df_R4$ln_Kc_raw <- safe_log1(to_num(df_R4$K_camas))
    df_R4$ln_Kt_raw <- safe_log1(to_num(df_R4$K_tech_index))
    df_R4$ln_L_total_c  <- df_R4$ln_L_raw  - mean(df_R4$ln_L_raw,  na.rm=TRUE)
    df_R4$ln_K_camas_c  <- df_R4$ln_Kc_raw - mean(df_R4$ln_Kc_raw, na.rm=TRUE)
    df_R4$ln_K_tech_c   <- df_R4$ln_Kt_raw - mean(df_R4$ln_Kt_raw, na.rm=TRUE)
    df_R4$ln_L_total_c2 <- 0.5 * df_R4$ln_L_total_c^2
    df_R4$ln_K_camas_c2 <- 0.5 * df_R4$ln_K_camas_c^2
    df_R4$ln_K_tech_c2  <- 0.5 * df_R4$ln_K_tech_c^2

    cc_R4 <- get_ccaa_dummies(df_R4, min_n=30, ref=9L)
    z_rhs_R4 <- paste(c("D_desc","pct_sns","desc_pago", cc_R4), collapse=" + ")
    fml_R4   <- as.formula(paste("ln_altTotal_pond ~", frontier_rhs_TL, "|", z_rhs_R4))

    all_v_R4 <- c("ln_altTotal_pond","ln_L_total_c","ln_K_camas_c","ln_K_tech_c",
                  "ln_L_total_c2","ln_K_camas_c2","ln_K_tech_c2",
                  "trend","trend2","d_cluster2","d_cluster3","d_cluster4","d_cluster5",
                  "D_desc","pct_sns","desc_pago", cc_R4)
    df_R4_cc <- df_R4 %>% filter(if_all(all_of(intersect(all_v_R4, names(.))), ~ !is.na(.x)))
    cat(sprintf("R4 thr=%d complete cases: %d\n", thr, nrow(df_R4_cc)))

    m_R4_Q <- tryCatch(
      frontier::sfa(fml_R4, data=df_R4_cc, ineffDecrease=TRUE, maxit=5000),
      error=function(e) { cat("R4 Total ERROR:", conditionMessage(e), "\n"); NULL }
    )
    if (!is.null(m_R4_Q)) { report_model(m_R4_Q, paste0("R4 thr=", thr, " Total")); saveRDS(m_R4_Q, out_Q) }
  } else {
    cat(sprintf("R4 thr=%d Total already exists — skipping.\n", thr))
    m_R4_Q <- readRDS(out_Q)
  }
}

## =============================================================
## R5: D_desc from CNH only (dependencia_cnh)
## =============================================================
cat("\n========== R5: D_desc from CNH only ==========\n")

R5_out_Q <- file.path(ROB_DIR, "sfa_R5_cnh_Total.rds")
R5_out_I <- file.path(ROB_DIR, "sfa_R5_cnh_Intensity.rds")

if (!file.exists(R5_out_Q)) {
  map_path <- "data_intermediate/ncodi_hospital_map.csv"
  if (!file.exists(map_path)) {
    cat("CNH map not found at", map_path, "— skipping R5.\n")
    m_R5_Q <- NULL; m_R5_I <- NULL
  } else {
    map_df <- tryCatch(read.csv(map_path, stringsAsFactors=FALSE, na.strings=""),
                       error=function(e) NULL)
    if (!is.null(map_df) && "dependencia_cnh" %in% names(map_df)) {
      # Classify D_desc from CNH
      pub_cats  <- c("CCAA","Diputacion","Municipio","Ingesa","IMSERSO",
                     "Defensa","Administracion Central","Administración Central",
                     "Seguridad Social","Otras Administraciones","Mutuas")
      priv_cats <- c("Privados","Organizaciones No Gubernamentales","ONG")

      map_df$NCODI <- trimws(as.character(map_df$NCODI))
      map_df$D_desc_cnh <- dplyr::case_when(
        trimws(map_df$dependencia_cnh) %in% pub_cats  ~ 0L,
        trimws(map_df$dependencia_cnh) %in% priv_cats ~ 1L,
        TRUE ~ NA_integer_
      )
      cat("D_desc_cnh distribution:\n")
      print(table(map_df$D_desc_cnh, useNA="always"))

      # Merge into df_cc
      map_sub <- map_df[, c("NCODI","D_desc_cnh")]
      map_sub  <- map_sub[!is.na(map_sub$NCODI) & !duplicated(map_sub$NCODI), ]

      df_R5 <- df_cc
      df_R5$NCODI <- trimws(as.character(df_R5$NCODI))
      df_R5 <- dplyr::left_join(df_R5, map_sub, by="NCODI")
      df_R5 <- df_R5 %>% filter(!is.na(D_desc_cnh))
      cat("R5 obs with CNH D_desc:", nrow(df_R5), "\n")

      if (nrow(df_R5) < 100) {
        cat("Too few obs for R5 — skipping.\n")
        m_R5_Q <- NULL; m_R5_I <- NULL
      } else {
        # Recompute desc_pago_cnh
        df_R5$desc_pago_cnh <- df_R5$D_desc_cnh * to_num(df_R5$pct_sns)
        cc_R5 <- get_ccaa_dummies(df_R5, min_n=30, ref=9L)
        z_rhs_R5 <- paste(c("D_desc_cnh","pct_sns","desc_pago_cnh", cc_R5), collapse=" + ")
        fml_R5   <- as.formula(paste("ln_altTotal_pond ~", frontier_rhs_TL, "|", z_rhs_R5))

        m_R5_Q <- tryCatch(
          frontier::sfa(fml_R5, data=df_R5, ineffDecrease=TRUE, maxit=5000),
          error=function(e) { cat("R5 Total ERROR:", conditionMessage(e), "\n"); NULL }
        )
        if (!is.null(m_R5_Q)) { report_model(m_R5_Q, "R5 CNH Total"); saveRDS(m_R5_Q, R5_out_Q) }
      }
    } else {
      cat("dependencia_cnh column not found in CNH map — skipping R5.\n")
      m_R5_Q <- NULL; m_R5_I <- NULL
    }
  }
} else {
  cat("R5 file already exists — skipping.\n")
  m_R5_Q <- readRDS(R5_out_Q)
  m_R5_I <- if (file.exists(R5_out_I)) readRDS(R5_out_I) else NULL
}

## =============================================================
## ROBUSTNESS SUMMARY TABLE
## =============================================================
cat("\n========== Building robustness summary table ==========\n")

get_z_row <- function(m, label, dep="Q") {
  if (is.null(m)) {
    return(data.frame(
      Spec=label, dep_var=dep,
      D_desc_coef=NA, D_desc_t=NA,
      pct_sns_coef=NA, pct_sns_t=NA,
      desc_pago_coef=NA, desc_pago_t=NA,
      N=NA, logLik=NA, gamma=NA, TE_mean=NA,
      asymmetry_sign_ok=NA, stringsAsFactors=FALSE
    ))
  }
  co  <- coef(m)
  se  <- tryCatch(sqrt(diag(vcov(m))), error=function(e) NULL)
  get <- function(v) {
    nm <- paste0("Z_", v)
    if (!is.null(se) && nm %in% names(co))
      c(round(co[nm],4), round(co[nm]/se[nm],4))
    else c(NA, NA)
  }
  dd  <- get("D_desc")
  ps  <- get("pct_sns")
  dp  <- get("desc_pago")
  gam <- if ("gamma" %in% names(co)) round(co["gamma"],4) else NA
  te  <- round(mean(efficiencies(m)), 4)
  # asymmetry sign: D_desc < 0 for quantity, > 0 for intensity
  ok  <- if (dep=="Q") !is.na(dd[1]) && dd[1] < 0 else !is.na(dd[1]) && dd[1] > 0
  data.frame(
    Spec=label, dep_var=dep,
    D_desc_coef=dd[1], D_desc_t=dd[2],
    pct_sns_coef=ps[1], pct_sns_t=ps[2],
    desc_pago_coef=dp[1], desc_pago_t=dp[2],
    N=nobs(m), logLik=round(logLik(m)[[1]],2), gamma=gam, TE_mean=te,
    asymmetry_sign_ok=ok,
    stringsAsFactors=FALSE
  )
}

rob_table <- bind_rows(
  get_z_row(m_ref_Q, "Main (noShareQ)",   "Q"),
  get_z_row(m_ref_I, "Main (intensity)",  "I"),
  get_z_row(if (exists("m_R1_Q")) m_R1_Q else NULL, "R1 COVID included", "Q"),
  get_z_row(if (exists("m_R1_I")) m_R1_I else NULL, "R1 COVID included", "I"),
  get_z_row(m_R2_Q, "R2 Cobb-Douglas",   "Q"),
  get_z_row(m_R2_I, "R2 Cobb-Douglas",   "I"),
  get_z_row(m_R3_Q, "R3 Half-normal",    "Q"),
  get_z_row(m_R3_I, "R3 Half-normal",    "I")
)

# Add R4 rows
for (thr in c(100, 300)) {
  tag  <- paste0("thr", thr)
  out_Q <- file.path(ROB_DIR, paste0("sfa_R4a_", tag, "_Total.rds"))
  m_r4  <- if (file.exists(out_Q)) readRDS(out_Q) else NULL
  rob_table <- bind_rows(rob_table, get_z_row(m_r4, paste0("R4 thr=",thr), "Q"))
}

# Add R5
rob_table <- bind_rows(rob_table,
  get_z_row(if (exists("m_R5_Q")) m_R5_Q else NULL, "R5 CNH D_desc", "Q")
)

write.csv(rob_table, file.path(ROB_DIR, "tabla_robustez_completa.csv"), row.names=FALSE)
cat("Robustness summary saved to:", file.path(ROB_DIR, "tabla_robustez_completa.csv"), "\n")
print(rob_table[, c("Spec","dep_var","D_desc_coef","D_desc_t","asymmetry_sign_ok")])

cat("\n========== Script 13 robustness COMPLETE ==========\n")
