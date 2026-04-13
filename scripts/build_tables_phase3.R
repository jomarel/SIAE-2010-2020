## =============================================================
## Phase 3 — Build thesis results tables
## =============================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(frontier)
})

CLEAN_DIR  <- "data_clean"
OUT_DIR    <- "outputs/tablas_tesis"
dir.create(OUT_DIR, showWarnings=FALSE, recursive=TRUE)

load(file.path(CLEAN_DIR, "df_sfa.RData"))
load(file.path(CLEAN_DIR, "df_final.RData"))

to_num <- function(x) suppressWarnings(as.numeric(x))

## enrich df_sfa from df_final if needed
key_s <- paste(df_sfa$NCODI, df_sfa$anyo)
key_f <- paste(df_final$NCODI, df_final$anyo)
idx   <- match(key_s, key_f)

for (vv in c("altTotal_pond","altTotal_bruto","i_diag","peso_grd_final",
             "L_total","K_camas","K_tech_index","K_quirofanos",
             "D_desc","pct_sns","ShareQ","desc_pago")) {
  if (!(vv %in% names(df_sfa)) && vv %in% names(df_final)) {
    df_sfa[[vv]] <- df_final[[vv]][idx]
  }
}

## ─────────────────────────────────────────────────────────────
## TABLE 1 — Descriptive statistics (Design A sample)
## ─────────────────────────────────────────────────────────────
cat("Building Table 1...\n")

samp_A <- df_sfa %>%
  filter(
    es_agudo == 1,
    !anyo %in% 2020:2022,
    to_num(altTotal_bruto) >= 200,
    !is.na(D_desc),
    !is.na(pct_sns),
    is.finite(to_num(ln_altTotal_pond)),
    !is.na(ccaa_cod)
  )

cat("Design A sample N =", nrow(samp_A), "\n")

desc_vars <- c("altTotal_pond","altTotal_bruto","i_diag","peso_grd_final",
               "L_total","K_camas","K_tech_index","K_quirofanos",
               "D_desc","pct_sns","ShareQ","desc_pago")

desc_stats <- function(df, vars) {
  do.call(rbind, lapply(vars, function(v) {
    x <- to_num(df[[v]])
    pct_na <- round(100 * mean(is.na(x)), 1)
    if (all(is.na(x))) {
      return(data.frame(Variable=v, Mean=NA, SD=NA, Min=NA,
                        P25=NA, Median=NA, P75=NA, Max=NA, NA_pct=pct_na))
    }
    data.frame(
      Variable = v,
      Mean     = round(mean(x, na.rm=TRUE), 3),
      SD       = round(sd(x,   na.rm=TRUE), 3),
      Min      = round(min(x,  na.rm=TRUE), 3),
      P25      = round(quantile(x, 0.25, na.rm=TRUE), 3),
      Median   = round(median(x,  na.rm=TRUE), 3),
      P75      = round(quantile(x, 0.75, na.rm=TRUE), 3),
      Max      = round(max(x,  na.rm=TRUE), 3),
      NA_pct   = pct_na
    )
  }))
}

tab1 <- desc_stats(samp_A, desc_vars)
write.csv(tab1, file.path(OUT_DIR, "tabla_descriptivos_A.csv"), row.names=FALSE)
cat("Table 1 saved.\n")

## Table 1b: by D_desc group
tab1b <- do.call(rbind, lapply(desc_vars, function(v) {
  x <- to_num(samp_A[[v]])
  g <- to_num(samp_A$D_desc)
  x0 <- x[!is.na(g) & g==0]
  x1 <- x[!is.na(g) & g==1]
  pval <- tryCatch(t.test(x0, x1)$p.value, error=function(e) NA)
  data.frame(
    Variable   = v,
    Mean_pub   = round(mean(x0, na.rm=TRUE), 3),
    SD_pub     = round(sd(x0,   na.rm=TRUE), 3),
    Mean_priv  = round(mean(x1, na.rm=TRUE), 3),
    SD_priv    = round(sd(x1,   na.rm=TRUE), 3),
    t_test_p   = round(pval, 4)
  )
}))
write.csv(tab1b, file.path(OUT_DIR, "tabla_descriptivos_A_grupos.csv"), row.names=FALSE)
cat("Table 1b saved.\n")

## ─────────────────────────────────────────────────────────────
## TABLE 2 — Evolution by year
## ─────────────────────────────────────────────────────────────
cat("Building Table 2...\n")

tab2 <- samp_A %>%
  group_by(anyo) %>%
  summarise(
    N                  = n(),
    altTotal_pond_mean = round(mean(to_num(altTotal_pond), na.rm=TRUE), 1),
    i_diag_median      = round(median(to_num(i_diag), na.rm=TRUE), 3),
    pct_D_desc         = round(100*mean(to_num(D_desc)==1, na.rm=TRUE), 1),
    pct_sns_mean       = round(mean(to_num(pct_sns), na.rm=TRUE), 3),
    ShareQ_mean        = round(mean(to_num(ShareQ), na.rm=TRUE), 3),
    .groups = "drop"
  )
write.csv(tab2, file.path(OUT_DIR, "tabla_evolucion_anual.csv"), row.names=FALSE)
cat("Table 2 saved.\n")

## ─────────────────────────────────────────────────────────────
## Load models
## ─────────────────────────────────────────────────────────────
mA_Q   <- readRDS(file.path(CLEAN_DIR, "sfa_A_total_v2.rds"))
mA_I   <- readRDS(file.path(CLEAN_DIR, "sfa_A_intensity_v2.rds"))
mA_QwS <- readRDS(file.path(CLEAN_DIR, "sfa_A_total_withShareQ_v2.rds"))
mA_IwS <- readRDS(file.path(CLEAN_DIR, "sfa_A_intensity_withShareQ_v2.rds"))
mB_Q   <- readRDS(file.path(CLEAN_DIR, "sfa_B_Q_v3.rds"))
mB_M   <- readRDS(file.path(CLEAN_DIR, "sfa_B_M_v3.rds"))
mC     <- readRDS(file.path(CLEAN_DIR, "sfa_C_stacked_service_v3.rds"))
mD_v4  <- readRDS(file.path(CLEAN_DIR, "sfa_D_v4_odf.rds"))
mD_v5  <- readRDS(file.path(CLEAN_DIR, "sfa_D_v5_cantidad.rds"))

extract_params <- function(m) {
  co <- coef(m)
  se <- tryCatch(sqrt(diag(vcov(m))), error=function(e) rep(NA_real_, length(co)))
  data.frame(param=names(co), coef=as.numeric(co), se=as.numeric(se),
             t=as.numeric(co)/as.numeric(se), stringsAsFactors=FALSE)
}

## ─────────────────────────────────────────────────────────────
## TABLE 3 — Design A frontier coefficients
## ─────────────────────────────────────────────────────────────
cat("Building Table 3...\n")

coQ <- extract_params(mA_Q)
coI <- extract_params(mA_I)
front_Q <- coQ[!grepl("^Z_", coQ$param), ]
front_I <- coI[!grepl("^Z_", coI$param), ]

all_p <- union(front_Q$param, front_I$param)
tab3  <- data.frame(Parameter=all_p, stringsAsFactors=FALSE)
tab3  <- merge(tab3, front_Q[, c("param","coef","se","t")],
               by.x="Parameter", by.y="param", all.x=TRUE)
names(tab3)[2:4] <- c("Coef_Q","SE_Q","t_Q")
tab3  <- merge(tab3, front_I[, c("param","coef","se","t")],
               by.x="Parameter", by.y="param", all.x=TRUE)
names(tab3)[5:7] <- c("Coef_I","SE_I","t_I")
tab3[, 2:7] <- lapply(tab3[, 2:7], function(x) round(x, 4))
write.csv(tab3, file.path(OUT_DIR, "tabla_A_frontera.csv"), row.names=FALSE)
cat("Table 3 saved.\n")

## ─────────────────────────────────────────────────────────────
## TABLE 4 — Design A z-equation with contrasts
## ─────────────────────────────────────────────────────────────
cat("Building Table 4...\n")

z_vars_A <- c("(Intercept)","D_desc","pct_sns","desc_pago")
zQ <- coQ[coQ$param %in% paste0("Z_", z_vars_A), ]
zI <- coI[coI$param %in% paste0("Z_", z_vars_A), ]
zQ$vname <- sub("^Z_", "", zQ$param)
zI$vname <- sub("^Z_", "", zI$param)

tab4 <- data.frame(Variable=z_vars_A, stringsAsFactors=FALSE)
tab4 <- merge(tab4, zQ[, c("vname","coef","se","t")],
              by.x="Variable", by.y="vname", all.x=TRUE)
names(tab4)[2:4] <- c("Coef_Q","SE_Q","t_Q")
tab4 <- merge(tab4, zI[, c("vname","coef","se","t")],
              by.x="Variable", by.y="vname", all.x=TRUE)
names(tab4)[5:7] <- c("Coef_I","SE_I","t_I")

tab4$z_diff <- (tab4$Coef_Q - tab4$Coef_I) /
               sqrt(tab4$SE_Q^2 + tab4$SE_I^2)
tab4$p_diff <- round(2*(1 - pnorm(abs(tab4$z_diff))), 4)
tab4$z_diff <- round(tab4$z_diff, 3)
tab4$sig    <- ifelse(tab4$p_diff < 0.001, "***",
               ifelse(tab4$p_diff < 0.01, "**",
               ifelse(tab4$p_diff < 0.05, "*", "")))
tab4[, 2:7] <- lapply(tab4[, 2:7], function(x) round(x, 4))
write.csv(tab4, file.path(OUT_DIR, "tabla_A_ineficiencia_contrastes.csv"), row.names=FALSE)
cat("Table 4 saved.\n")

## ─────────────────────────────────────────────────────────────
## TABLE 5 — Design B contrasts Q vs M
## ─────────────────────────────────────────────────────────────
cat("Building Table 5...\n")

coBQ <- extract_params(mB_Q)
coBM <- extract_params(mB_M)
z_vars_B <- c("(Intercept)","d_Priv_Conc","d_Priv_Merc","ShareQ",
               "Conc_shareQ","Merc_shareQ")

zBQ <- coBQ[coBQ$param %in% paste0("Z_", z_vars_B), ]
zBM <- coBM[coBM$param %in% paste0("Z_", z_vars_B), ]
zBQ$vname <- sub("^Z_", "", zBQ$param)
zBM$vname <- sub("^Z_", "", zBM$param)

tab5 <- data.frame(Variable=z_vars_B, stringsAsFactors=FALSE)
tab5 <- merge(tab5, zBQ[, c("vname","coef","se","t")],
              by.x="Variable", by.y="vname", all.x=TRUE)
names(tab5)[2:4] <- c("Coef_Q","SE_Q","t_Q")
tab5 <- merge(tab5, zBM[, c("vname","coef","se","t")],
              by.x="Variable", by.y="vname", all.x=TRUE)
names(tab5)[5:7] <- c("Coef_M","SE_M","t_M")
tab5$z_diff <- (tab5$Coef_Q - tab5$Coef_M) /
               sqrt(tab5$SE_Q^2 + tab5$SE_M^2)
tab5$p_diff <- round(2*(1 - pnorm(abs(tab5$z_diff))), 4)
tab5$z_diff <- round(tab5$z_diff, 3)
tab5[, 2:7]  <- lapply(tab5[, 2:7], function(x) round(x, 4))

# TE summary rows
te_BQ <- efficiencies(mB_Q)
te_BM <- efficiencies(mB_M)
te_rows <- data.frame(
  Variable = c("--- TE_Q_mean", "--- TE_M_mean"),
  Coef_Q   = c(round(mean(te_BQ),4), NA),
  SE_Q     = c(round(sd(te_BQ),4), NA),
  t_Q      = c(NA, NA),
  Coef_M   = c(NA, round(mean(te_BM),4)),
  SE_M     = c(NA, round(sd(te_BM),4)),
  t_M      = c(NA, NA),
  z_diff   = c(NA, NA),
  p_diff   = c(NA, NA),
  stringsAsFactors = FALSE
)
tab5 <- bind_rows(tab5, te_rows)
write.csv(tab5, file.path(OUT_DIR, "tabla_B_contrastes.csv"), row.names=FALSE)
cat("Table 5 saved.\n")

## ─────────────────────────────────────────────────────────────
## TABLE 6 — Design C key coefficients
## ─────────────────────────────────────────────────────────────
cat("Building Table 6...\n")

coC     <- extract_params(mC)
z_vars_C <- c("(Intercept)","d_Priv_Conc","d_Priv_Merc","C_s",
               "Priv_Conc_Cs","Priv_Merc_Cs","ShareQ","ShareQ_Cs")
zC      <- coC[coC$param %in% paste0("Z_", z_vars_C), ]
zC$Variable <- sub("^Z_", "", zC$param)
zC$sig  <- ifelse(abs(zC$t) > 3.29, "***",
           ifelse(abs(zC$t) > 2.58, "**",
           ifelse(abs(zC$t) > 1.96, "*", "")))
tab6    <- zC[, c("Variable","coef","se","t","sig")]
names(tab6)[2:4] <- c("Coef","SE","t_stat")
tab6[, 2:4] <- lapply(tab6[, 2:4], function(x) round(x, 4))
write.csv(tab6, file.path(OUT_DIR, "tabla_C_coeficientes.csv"), row.names=FALSE)
cat("Table 6 saved.\n")

## ─────────────────────────────────────────────────────────────
## TABLE 7 — Design D v4 vs v5
## ─────────────────────────────────────────────────────────────
cat("Building Table 7...\n")

make_D_row <- function(m, lab) {
  co  <- coef(m)
  se  <- tryCatch(sqrt(diag(vcov(m))), error=function(e) NULL)
  ll  <- round(logLik(m)[[1]], 2)
  N   <- nobs(m)
  gam <- if ("gamma" %in% names(co)) round(co["gamma"], 6) else NA
  te  <- round(mean(efficiencies(m)), 4)
  get_z <- function(v) {
    nm <- paste0("Z_", v)
    if (!is.null(se) && nm %in% names(co))
      c(round(co[nm],4), round(se[nm],4), round(co[nm]/se[nm],4))
    else c(NA, NA, NA)
  }
  z_Dd <- get_z("D_desc")
  z_ps <- get_z("pct_sns")
  z_dp <- get_z("desc_pago")
  data.frame(
    Model=lab, N=N, logLik=ll, gamma=gam, TE_mean=te,
    D_desc_coef=z_Dd[1], D_desc_se=z_Dd[2], D_desc_t=z_Dd[3],
    pct_sns_coef=z_ps[1], pct_sns_se=z_ps[2], pct_sns_t=z_ps[3],
    desc_pago_coef=z_dp[1], desc_pago_se=z_dp[2], desc_pago_t=z_dp[3],
    stringsAsFactors=FALSE
  )
}

tab7 <- bind_rows(make_D_row(mD_v4, "D_v4_odf"), make_D_row(mD_v5, "D_v5_cantidad"))
write.csv(tab7, file.path(OUT_DIR, "tabla_D_comparacion.csv"), row.names=FALSE)
cat("Table 7 saved.\n")

## ─────────────────────────────────────────────────────────────
## TABLE 8 — TE distribution summary
## ─────────────────────────────────────────────────────────────
cat("Building Table 8...\n")

te_row <- function(m, label) {
  te <- efficiencies(m)
  data.frame(
    Model   = label,
    N       = length(te),
    TE_mean = round(mean(te),4),
    TE_sd   = round(sd(te),4),
    TE_p10  = round(quantile(te,.10),4),
    TE_p25  = round(quantile(te,.25),4),
    TE_med  = round(median(te),4),
    TE_p75  = round(quantile(te,.75),4),
    TE_p90  = round(quantile(te,.90),4),
    stringsAsFactors=FALSE
  )
}

tab8 <- bind_rows(
  te_row(mA_Q,  "A_total"),
  te_row(mA_I,  "A_intensity"),
  te_row(mB_Q,  "B_Q"),
  te_row(mB_M,  "B_M"),
  te_row(mC,    "C_stacked"),
  te_row(mD_v5, "D_v5")
)
write.csv(tab8, file.path(OUT_DIR, "tabla_TE_distribucion.csv"), row.names=FALSE)
cat("Table 8 saved.\n")

## ─────────────────────────────────────────────────────────────
## TABLE 9 — Predictions summary
## ─────────────────────────────────────────────────────────────
cat("Building Table 9...\n")

co_AQ <- coef(mA_Q);  se_AQ <- sqrt(diag(vcov(mA_Q)))
co_AI <- coef(mA_I);  se_AI <- sqrt(diag(vcov(mA_I)))
co_BQ <- coef(mB_Q);  se_BQ <- sqrt(diag(vcov(mB_Q)))
co_BM <- coef(mB_M);  se_BM <- sqrt(diag(vcov(mB_M)))
co_C  <- coef(mC);    se_C  <- sqrt(diag(vcov(mC)))
co_D5 <- coef(mD_v5); se_D5 <- sqrt(diag(vcov(mD_v5)))

get_coef <- function(co, v) {
  nm <- paste0("Z_", v)
  if (nm %in% names(co)) round(co[nm],3) else NA
}
get_t <- function(co, se, v) {
  nm <- paste0("Z_", v)
  if (nm %in% names(co) && nm %in% names(se)) round(co[nm]/se[nm],2) else NA
}
zdiff_p <- function(c1, s1, c2, s2) {
  z <- (c1 - c2) / sqrt(s1^2 + s2^2)
  list(z=round(z,2), p=round(2*(1-pnorm(abs(z))),4))
}

# Contrasts
asym_dd <- zdiff_p(co_AQ["Z_D_desc"],    se_AQ["Z_D_desc"],
                   co_AI["Z_D_desc"],    se_AI["Z_D_desc"])
asym_dp <- zdiff_p(co_AQ["Z_desc_pago"], se_AQ["Z_desc_pago"],
                   co_AI["Z_desc_pago"], se_AI["Z_desc_pago"])
asym_sq <- zdiff_p(co_BQ["Z_ShareQ"],    se_BQ["Z_ShareQ"],
                   co_BM["Z_ShareQ"],    se_BM["Z_ShareQ"])

tab9 <- data.frame(
  Prediction = c("P1","P2","P3","H_B1","H_B2_Q","H_B2_M","H_B3",
                 "ShareQ_Cs","D_desc_D5"),
  Description = c(
    "D_desc reduces quantity inefficiency",
    "D_desc increases intensity inefficiency",
    "Payment mechanism asymmetric (desc_pago)",
    "Org effects differ Q vs M (ShareQ)",
    "ShareQ negative in B_Q",
    "ShareQ positive in B_M",
    "Moderation stronger in M than Q",
    "Intra-hospital mechanism (ShareQ_Cs)",
    "D_desc robust in multioutput D_v5"
  ),
  Model = c("A_total","A_intensity","A","B","B_Q","B_M","B","C","D_v5"),
  Key_statistic = c(
    sprintf("D_desc=%.3f t=%.2f", get_coef(co_AQ,"D_desc"), get_t(co_AQ,se_AQ,"D_desc")),
    sprintf("D_desc=%.3f t=%.2f", get_coef(co_AI,"D_desc"), get_t(co_AI,se_AI,"D_desc")),
    sprintf("z_diff(desc_pago)=%.2f p=%.4f", asym_dp$z, asym_dp$p),
    sprintf("z_diff(ShareQ)=%.2f p=%.4f",    asym_sq$z, asym_sq$p),
    sprintf("ShareQ=%.3f t=%.2f", get_coef(co_BQ,"ShareQ"), get_t(co_BQ,se_BQ,"ShareQ")),
    sprintf("ShareQ=%.3f t=%.2f", get_coef(co_BM,"ShareQ"), get_t(co_BM,se_BM,"ShareQ")),
    sprintf("|Merc_Q|=%.3f vs |Merc_M|=%.3f",
            abs(get_coef(co_BQ,"Merc_shareQ")), abs(get_coef(co_BM,"Merc_shareQ"))),
    sprintf("ShareQ_Cs=%.3f t=%.2f", get_coef(co_C,"ShareQ_Cs"), get_t(co_C,se_C,"ShareQ_Cs")),
    sprintf("D_desc=%.3f t=%.2f", get_coef(co_D5,"D_desc"), get_t(co_D5,se_D5,"D_desc"))
  ),
  Verdict = c(
    ifelse(get_coef(co_AQ,"D_desc") < 0 & abs(get_t(co_AQ,se_AQ,"D_desc")) > 1.96, "CONFIRMED","NOT CONFIRMED"),
    ifelse(get_coef(co_AI,"D_desc") > 0 & abs(get_t(co_AI,se_AI,"D_desc")) > 1.96, "CONFIRMED","NOT CONFIRMED"),
    ifelse(abs(asym_dp$z) > 1.96, "CONFIRMED","NOT CONFIRMED"),
    ifelse(abs(asym_sq$z) > 1.96, "CONFIRMED","NOT CONFIRMED"),
    ifelse(get_coef(co_BQ,"ShareQ") < 0 & abs(get_t(co_BQ,se_BQ,"ShareQ")) > 1.96, "CONFIRMED","NOT CONFIRMED"),
    ifelse(get_coef(co_BM,"ShareQ") > 0 & abs(get_t(co_BM,se_BM,"ShareQ")) > 1.96, "CONFIRMED","NOT CONFIRMED"),
    ifelse(abs(get_coef(co_BM,"Merc_shareQ")) < abs(get_coef(co_BQ,"Merc_shareQ")),
           "NOT CONFIRMED","CONFIRMED"),
    ifelse(get_coef(co_C,"ShareQ_Cs") < 0 & abs(get_t(co_C,se_C,"ShareQ_Cs")) > 1.96, "CONFIRMED","NOT CONFIRMED"),
    ifelse(get_coef(co_D5,"D_desc") < 0 & abs(get_t(co_D5,se_D5,"D_desc")) > 1.96, "CONFIRMED","NOT CONFIRMED")
  ),
  stringsAsFactors = FALSE
)
write.csv(tab9, file.path(OUT_DIR, "tabla_predicciones_resumen.csv"), row.names=FALSE)
cat("Table 9 saved.\n")

## ─────────────────────────────────────────────────────────────
## TODAS_LAS_TABLAS.txt — combined formatted output
## ─────────────────────────────────────────────────────────────
cat("Writing TODAS_LAS_TABLAS.txt...\n")

fmt <- function(df) paste(capture.output(print(df, row.names=FALSE)), collapse="\n")

sec <- function(title) c("", paste(rep("=",60),collapse=""), title, paste(rep("=",60),collapse=""), "")

all_lines <- c(
  "TABLAS TESIS SFA — SIAE 2010-2023",
  paste("Generado:", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
  sec("TABLA 1: Estadísticos descriptivos (Muestra Diseño A)"),
  fmt(tab1),
  sec("TABLA 1b: Descriptivos por grupo D_desc"),
  fmt(tab1b),
  sec("TABLA 2: Evolución anual"),
  fmt(tab2),
  sec("TABLA 3: Diseño A — Coeficientes frontera (Translog)"),
  fmt(tab3),
  sec("TABLA 4: Diseño A — Ecuación de ineficiencia con contrastes"),
  fmt(tab4),
  sec("TABLA 5: Diseño B — Contrastes Q vs M"),
  fmt(tab5),
  sec("TABLA 6: Diseño C — Coeficientes clave"),
  fmt(tab6),
  sec("TABLA 7: Diseño D — Comparación v4 vs v5"),
  fmt(tab7),
  sec("TABLA 8: Distribución de Eficiencia Técnica (TE)"),
  fmt(tab8),
  sec("TABLA 9: Resumen de predicciones teóricas"),
  fmt(tab9)
)

writeLines(all_lines, file.path(OUT_DIR, "TODAS_LAS_TABLAS.txt"))
cat("TODAS_LAS_TABLAS.txt saved.\n")
cat("PHASE 3 COMPLETE\n")
