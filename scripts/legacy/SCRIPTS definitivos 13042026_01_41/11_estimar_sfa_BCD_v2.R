## =============================================================
## Phase 7 — Designs B, C, D (revised)
## v5:
##   - Design B: umbral 200, inputs específicos por servicio
##   - Design C: panel apilado, misma muestra que B
##   - Design D: ODF cantidad + intensidad POR ALTA (i_diag)
##               sustituye la versión con i_diag_sum
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
  if ("gamma"   %in% names(co)) cat("gamma:",   round(co["gamma"],   6), "\n")
  if ("sigmaSq" %in% names(co)) cat("sigmaSq:", round(co["sigmaSq"], 6), "\n")
  se <- tryCatch(sqrt(diag(vcov(m))), error = function(e) NULL)
  if (!is.null(se)) {
    zidx <- grep("^Z_", names(co))
    if (length(zidx) > 0) {
      cat("\nZ-equation coefficients:\n")
      for (i in zidx) {
        cat(sprintf("  %-22s %9.4f  se=%8.4f  t=%7.3f\n",
                    names(co)[i], co[i], se[i], co[i]/se[i]))
      }
    }
  }
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

## ---------- parámetros ----------
MIN_ALT_TOTAL <- 200
MIN_ALTQ_B    <- 200
MIN_ALTM_B    <- 200
MIN_CCAA_N    <- 30
EXCL_YEARS    <- 2020:2022
WINSOR_P      <- 0.01

## ---------- enrich df_sfa ----------
need_vars <- c(
  "NCODI","anyo","es_agudo","ccaa_cod","grupo_pago",
  "D_desc","pct_sns","ShareQ","desc_pago","desc_shareQ",
  "L_total","L_quirur","L_medico",
  "K_camas","K_quirofanos","K_tech_index",
  "altTotal_bruto","altTotal_pond","ln_altTotal_pond",
  "altQ_bruto","altQ_pond","ln_altQ_pond",
  "altM_bruto","altM_pond","ln_altM_pond",
  "i_diag","i_diag_sum","i_diag_w","ln_i_diag","ln_i_diag_w",
  "trend","trend2",
  "d_cluster2","d_cluster3","d_cluster4","d_cluster5",
  paste0("d_ccaa_", setdiff(1:17, 9))
)
df_sfa <- add_missing_from_final(df_sfa, df_final, need_vars)

## Verificar variables críticas
for (v in c("K_quirofanos","i_diag")) {
  if (!v %in% names(df_sfa))
    stop(paste(v, "not found in df_sfa. Check scripts 07-08."))
}

## Reconstruir variables de grupo si faltan
if (!("desc_pago" %in% names(df_sfa)) || all(is.na(df_sfa$desc_pago)))
  df_sfa$desc_pago <- to_num(df_sfa$D_desc) * to_num(df_sfa$pct_sns)
df_sfa$d_Priv_Conc <- as.integer(df_sfa$D_desc == 1 & df_sfa$pct_sns >= 0.50)
df_sfa$d_Priv_Merc <- as.integer(df_sfa$D_desc == 1 & df_sfa$pct_sns <  0.50)
df_sfa$Conc_shareQ <- df_sfa$d_Priv_Conc * df_sfa$ShareQ
df_sfa$Merc_shareQ <- df_sfa$d_Priv_Merc * df_sfa$ShareQ
if (!("grupo_pago" %in% names(df_sfa)) || all(is.na(df_sfa$grupo_pago))) {
  df_sfa$grupo_pago <- dplyr::case_when(
    df_sfa$D_desc == 0                      ~ "Pub_Retro",
    df_sfa$D_desc == 1 & df_sfa$pct_sns >= 0.50 ~ "Priv_Conc",
    df_sfa$D_desc == 1 & df_sfa$pct_sns <  0.50 ~ "Priv_Merc",
    TRUE ~ NA_character_)
}

cat("========== Phase 7: Designs B, C, D ==========\n")
cat("df_sfa:", nrow(df_sfa), "x", ncol(df_sfa), "\n")

## =============================================================
## DESIGN B — muestra común Q y M, inputs específicos por servicio
## =============================================================
cat("\n\n################################################################\n")
cat("# DESIGN B — common-support sample, service-specific inputs\n")
cat("################################################################\n")

sample_B <- df_sfa %>%
  filter(
    es_agudo == 1,
    !anyo %in% EXCL_YEARS,
    altTotal_bruto >= MIN_ALT_TOTAL,
    altQ_bruto     >= MIN_ALTQ_B,
    altM_bruto     >= MIN_ALTM_B,
    is.finite(ln_altQ_pond),
    is.finite(ln_altM_pond),
    !is.na(D_desc), !is.na(pct_sns), !is.na(ShareQ),
    !is.na(desc_pago), !is.na(ccaa_cod),
    !is.na(L_quirur), !is.na(L_medico),
    !is.na(K_camas), !is.na(K_tech_index), !is.na(K_quirofanos)
  ) %>%
  mutate(
    ln_L_quirur_raw = safe_log1(to_num(L_quirur)),
    ln_L_medico_raw = safe_log1(to_num(L_medico)),
    ln_K_qx_raw     = safe_log1(to_num(K_quirofanos)),
    ln_K_camas_raw  = safe_log1(to_num(K_camas)),
    ln_K_tech_raw   = safe_log1(to_num(K_tech_index))
  )

## Centrar en la muestra común
mu_LQ <- mean(sample_B$ln_L_quirur_raw, na.rm = TRUE)
mu_LM <- mean(sample_B$ln_L_medico_raw, na.rm = TRUE)
mu_KQ <- mean(sample_B$ln_K_qx_raw,     na.rm = TRUE)
mu_KM <- mean(sample_B$ln_K_camas_raw,  na.rm = TRUE)
mu_T  <- mean(sample_B$ln_K_tech_raw,   na.rm = TRUE)

sample_B <- sample_B %>%
  mutate(
    ln_L_quirur_c  = ln_L_quirur_raw - mu_LQ,
    ln_L_medico_c  = ln_L_medico_raw - mu_LM,
    ln_K_qx_c      = ln_K_qx_raw     - mu_KQ,
    ln_K_camas_c   = ln_K_camas_raw  - mu_KM,
    ln_K_tech_c    = ln_K_tech_raw   - mu_T,
    ln_L_quirur_c2 = 0.5 * ln_L_quirur_c^2,
    ln_L_medico_c2 = 0.5 * ln_L_medico_c^2,
    ln_K_qx_c2     = 0.5 * ln_K_qx_c^2,
    ln_K_camas_c2  = 0.5 * ln_K_camas_c^2,
    ln_K_tech_c2   = 0.5 * ln_K_tech_c^2
  )

ccaa_B  <- get_ccaa_dummies(sample_B, min_n = MIN_CCAA_N, ref = 9L)
z_rhs_B <- paste(c("d_Priv_Conc","d_Priv_Merc","ShareQ",
                    "Conc_shareQ","Merc_shareQ", ccaa_B),
                 collapse = " + ")

cat("Sample BQ/BM:", nrow(sample_B), "obs\n")
cat("Years:", paste(sort(unique(sample_B$anyo)), collapse = ", "), "\n")
cat("% private:", round(100*mean(sample_B$D_desc==1, na.rm=TRUE), 1), "%\n")
cat("Mean ShareQ:", round(mean(sample_B$ShareQ, na.rm=TRUE), 3), "\n")

## Diagnóstico skewness
ols_BQ <- lm(ln_altQ_pond ~ ln_L_quirur_c + ln_K_qx_c + ln_K_tech_c +
               ln_L_quirur_c2 + ln_K_qx_c2 + ln_K_tech_c2 +
               trend + trend2 + d_cluster2+d_cluster3+d_cluster4+d_cluster5,
             data = sample_B)
ols_BM <- lm(ln_altM_pond ~ ln_L_medico_c + ln_K_camas_c + ln_K_tech_c +
               ln_L_medico_c2 + ln_K_camas_c2 + ln_K_tech_c2 +
               trend + trend2 + d_cluster2+d_cluster3+d_cluster4+d_cluster5,
             data = sample_B)
cat("OLS skewness B_Q:", round(skewness(residuals(ols_BQ)), 4), "\n")
cat("OLS skewness B_M:", round(skewness(residuals(ols_BM)), 4), "\n")

## B_Q: L_quirur + K_quirofanos + K_tech
frontier_rhs_BQ <- paste(c(
  "ln_L_quirur_c","ln_K_qx_c","ln_K_tech_c",
  "ln_L_quirur_c2","ln_K_qx_c2","ln_K_tech_c2",
  "trend","trend2","d_cluster2","d_cluster3","d_cluster4","d_cluster5"
), collapse = " + ")

fml_BQ <- as.formula(paste("ln_altQ_pond ~", frontier_rhs_BQ, "|", z_rhs_B))
cat("\n--- Estimating B_Q ---\n")
m_B_Q <- frontier::sfa(fml_BQ, data = sample_B, ineffDecrease = TRUE, maxit = 5000)
report_model(m_B_Q, "B_Q")
saveRDS(m_B_Q, file.path(CLEAN_DIR, "sfa_B_Q_v3.rds"))

## B_M: L_medico + K_camas + K_tech
frontier_rhs_BM <- paste(c(
  "ln_L_medico_c","ln_K_camas_c","ln_K_tech_c",
  "ln_L_medico_c2","ln_K_camas_c2","ln_K_tech_c2",
  "trend","trend2","d_cluster2","d_cluster3","d_cluster4","d_cluster5"
), collapse = " + ")

fml_BM <- as.formula(paste("ln_altM_pond ~", frontier_rhs_BM, "|", z_rhs_B))
cat("\n--- Estimating B_M ---\n")
m_B_M <- frontier::sfa(fml_BM, data = sample_B, ineffDecrease = TRUE, maxit = 5000)
report_model(m_B_M, "B_M")
saveRDS(m_B_M, file.path(CLEAN_DIR, "sfa_B_M_v3.rds"))

## Tabla contrastes B
vars_B <- c("d_Priv_Conc","d_Priv_Merc","ShareQ","Conc_shareQ","Merc_shareQ")
tab_BQ <- mk_coef_tab(m_B_Q, vars_B); names(tab_BQ)[2:4] <- c("coef_BQ","se_BQ","t_BQ")
tab_BM <- mk_coef_tab(m_B_M, vars_B); names(tab_BM)[2:4] <- c("coef_BM","se_BM","t_BM")
tabla_B <- left_join(tab_BQ, tab_BM, by = "param")
write.csv(tabla_B, file.path(CLEAN_DIR, "tabla_contrastes_B_v3.csv"), row.names = FALSE)

## =============================================================
## DESIGN C — panel apilado Q/M, misma muestra que B
## =============================================================
cat("\n\n################################################################\n")
cat("# DESIGN C — stacked service (same sample as B)\n")
cat("################################################################\n")

common_cols <- c("NCODI","anyo","trend","trend2",
                 "d_cluster2","d_cluster3","d_cluster4","d_cluster5",
                 "D_desc","pct_sns","ShareQ","d_Priv_Conc","d_Priv_Merc",
                 "Conc_shareQ","Merc_shareQ","ccaa_cod","grupo_pago", ccaa_B)
common_cols <- intersect(common_cols, names(sample_B))

df_Q <- sample_B[, common_cols, drop = FALSE]
df_Q$C_s      <- 1L
df_Q$ln_output <- sample_B$ln_altQ_pond
df_Q$L_serv   <- to_num(sample_B$L_quirur)
df_Q$K_serv   <- to_num(sample_B$K_quirofanos)
df_Q$K_tech   <- to_num(sample_B$K_tech_index)

df_M <- sample_B[, common_cols, drop = FALSE]
df_M$C_s      <- 0L
df_M$ln_output <- sample_B$ln_altM_pond
df_M$L_serv   <- to_num(sample_B$L_medico)
df_M$K_serv   <- to_num(sample_B$K_camas)
df_M$K_tech   <- to_num(sample_B$K_tech_index)

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

ccaa_C <- get_ccaa_dummies(sample_C, min_n = 2*MIN_CCAA_N, ref = 9L)

frontier_rhs_C <- paste(c(
  "C_s",
  "ln_L_serv_c","ln_K_serv_c","ln_K_tech_c",
  "ln_L_serv_c2","ln_K_serv_c2","ln_K_tech_c2",
  "trend","trend2","d_cluster2","d_cluster3","d_cluster4","d_cluster5"
), collapse = " + ")

z_rhs_C <- paste(c(
  "d_Priv_Conc","d_Priv_Merc","ShareQ",
  "Priv_Conc_Cs","Priv_Merc_Cs","ShareQ_Cs",
  ccaa_C
), collapse = " + ")

fml_C <- as.formula(paste("ln_output ~", frontier_rhs_C, "|", z_rhs_C))

cat("Sample C:", nrow(sample_C), "obs\n")
cat("\n--- Estimating C (stacked service) ---\n")
m_C <- frontier::sfa(fml_C, data = sample_C, ineffDecrease = TRUE, maxit = 5000)
report_model(m_C, "C stacked service")
saveRDS(m_C, file.path(CLEAN_DIR, "sfa_C_stacked_service_v3.rds"))

## =============================================================
## DESIGN D — dos especificaciones alternativas
## D_v4: ODF clásica  -ln(Q) ~ ln(i) + inputs  [ineffDecrease=FALSE]
## D_v5: frontera de cantidad con control de intensidad
##        ln(Q) ~ ln(i) + inputs               [ineffDecrease=TRUE]
## =============================================================

## --- muestra común para ambas ---
sample_D <- df_sfa %>%
  filter(
    es_agudo == 1,
    !anyo %in% EXCL_YEARS,
    altTotal_bruto >= MIN_ALT_TOTAL,
    !is.na(D_desc), !is.na(pct_sns), !is.na(desc_pago),
    !is.na(ccaa_cod), !is.na(L_total),
    !is.na(K_camas), !is.na(K_tech_index),
    !is.na(altTotal_pond), altTotal_pond > 0,
    !is.na(i_diag), i_diag >= 1.0
  ) %>%
  mutate(
    ln_Q     = log(to_num(altTotal_pond)),
    neg_ln_Q = -ln_Q,
    ln_i_raw = log(to_num(i_diag)),
    ln_L_raw = safe_log1(to_num(L_total)),
    ln_K_raw = safe_log1(to_num(K_camas)),
    ln_T_raw = safe_log1(to_num(K_tech_index))
  ) %>%
  filter(is.finite(ln_Q), is.finite(ln_i_raw))

## winsorizar intensidad
p1_i  <- quantile(sample_D$ln_i_raw, WINSOR_P,   na.rm=TRUE)
p99_i <- quantile(sample_D$ln_i_raw, 1-WINSOR_P, na.rm=TRUE)
sample_D$ln_i_w <- pmin(pmax(sample_D$ln_i_raw, p1_i), p99_i)
cat(sprintf("ln_i winsor: p1=%.3f  p99=%.3f\n", p1_i, p99_i))

## centrar inputs
mu_LD <- mean(sample_D$ln_L_raw, na.rm=TRUE)
mu_KD <- mean(sample_D$ln_K_raw, na.rm=TRUE)
mu_TD <- mean(sample_D$ln_T_raw, na.rm=TRUE)

sample_D <- sample_D %>%
  mutate(
    ln_L_c  = ln_L_raw - mu_LD,
    ln_K_c  = ln_K_raw - mu_KD,
    ln_T_c  = ln_T_raw - mu_TD,
    ln_L_c2 = 0.5 * ln_L_c^2,
    ln_K_c2 = 0.5 * ln_K_c^2
  )

cat("Sample D:", nrow(sample_D), "obs\n")

## skewness de ambas variables dependientes
ols_neg <- lm(neg_ln_Q ~ ln_i_w + ln_L_c + ln_K_c + ln_T_c +
                ln_L_c2 + ln_K_c2 + trend + trend2 +
                d_cluster2+d_cluster3+d_cluster4+d_cluster5,
              data=sample_D)
ols_pos <- lm(ln_Q ~ ln_i_w + ln_L_c + ln_K_c + ln_T_c +
                ln_L_c2 + ln_K_c2 + trend + trend2 +
                d_cluster2+d_cluster3+d_cluster4+d_cluster5,
              data=sample_D)
cat(sprintf("OLS skewness -ln(Q): %6.4f  (ODF clásica)\n",
    skewness(residuals(ols_neg))))
cat(sprintf("OLS skewness  ln(Q): %6.4f  (frontera cantidad)\n",
    skewness(residuals(ols_pos))))
cat("(producción SFA requiere skewness negativo)\n\n")

## z-vars y frontera comunes
ccaa_D <- get_ccaa_dummies(sample_D, min_n=MIN_CCAA_N, ref=9L)
z_D    <- paste(c("D_desc","pct_sns","desc_pago", ccaa_D),
                collapse=" + ")
frontier_D <- paste(c(
  "ln_i_w",
  "ln_L_c","ln_K_c","ln_T_c",
  "ln_L_c2","ln_K_c2",
  "trend","trend2",
  "d_cluster2","d_cluster3","d_cluster4","d_cluster5"
), collapse=" + ")

## --- D_v4: ODF clásica  [ineffDecrease=FALSE] ---
cat("--- Estimating D_v4 (ODF: -ln_Q, ineffDecrease=FALSE) ---\n")
fml_D_v4 <- as.formula(paste("neg_ln_Q ~", frontier_D, "|", z_D))
m_D_v4 <- tryCatch(
  frontier::sfa(fml_D_v4, data=sample_D,
                ineffDecrease=FALSE, maxit=5000),
  error=function(e) { cat("D_v4 ERROR:", e$message,"\n"); NULL })
if (!is.null(m_D_v4)) {
  report_model(m_D_v4, "D_v4 ODF clasica (-ln_Q, ineffDecrease=FALSE)")
  saveRDS(m_D_v4, file.path(CLEAN_DIR, "sfa_D_v4_odf.rds"))
}

## --- D_v5: frontera cantidad con control intensidad [ineffDecrease=TRUE] ---
cat("\n--- Estimating D_v5 (cantidad: ln_Q, ineffDecrease=TRUE) ---\n")
fml_D_v5 <- as.formula(paste("ln_Q ~", frontier_D, "|", z_D))
m_D_v5 <- tryCatch(
  frontier::sfa(fml_D_v5, data=sample_D,
                ineffDecrease=TRUE, maxit=5000),
  error=function(e) { cat("D_v5 ERROR:", e$message,"\n"); NULL })
if (!is.null(m_D_v5)) {
  report_model(m_D_v5, "D_v5 frontera cantidad (ln_Q, ineffDecrease=TRUE)")
  saveRDS(m_D_v5, file.path(CLEAN_DIR, "sfa_D_v5_cantidad.rds"))
}

## --- comparación rápida ---
cat("\n--- Comparación D_v4 vs D_v5 ---\n")
cat(sprintf("%-6s  %-10s  %-6s  %-6s  %-10s\n",
    "Model","logLik","gamma","TE","D_desc(t)"))
for (lst in list(list(m_D_v4,"D_v4"), list(m_D_v5,"D_v5"))) {
  m <- lst[[1]]; nm <- lst[[2]]
  if (is.null(m)) { cat(nm, ": failed\n"); next }
  co <- coef(m)
  se <- tryCatch(sqrt(diag(vcov(m))),error=function(e) rep(NA,length(co)))
  dd_nm <- if("Z_D_desc" %in% names(co)) "Z_D_desc" else NA
  dd_t  <- if(!is.na(dd_nm)) co[dd_nm]/se[dd_nm] else NA
  cat(sprintf("%-6s  %10.1f  %6.4f  %6.3f  %10.3f\n",
    nm,
    logLik(m)[[1]],
    co["gamma"],
    mean(efficiencies(m), na.rm=TRUE),
    dd_t))
}
saveRDS(m_D, file.path(CLEAN_DIR, "sfa_D_odf_v4.rds"))

## =============================================================
## RESULTADOS E INTERPRETACIÓN
## =============================================================
cat("\n\n################################################################\n")
cat("# RESULTADOS E INTERPRETACIÓN\n")
cat("################################################################\n")

## --- función de contraste z entre dos modelos ---
z_contrast <- function(m1, m2, vars, label1, label2) {
  co1 <- coef(m1)
  se1 <- tryCatch(sqrt(diag(vcov(m1))), error=function(e) rep(NA, length(co1)))
  co2 <- coef(m2)
  se2 <- tryCatch(sqrt(diag(vcov(m2))), error=function(e) rep(NA, length(co2)))
  cat(sprintf("\n%-22s | %9s %6s | %9s %6s | %8s %s\n",
    "Variable", label1, "t", label2, "t", "z_diff", "p"))
  cat(strrep("-", 74), "\n")
  for (v in vars) {
    nm  <- paste0("Z_", v)
    c1  <- if (nm %in% names(co1)) co1[nm] else NA
    s1  <- if (nm %in% names(co1)) se1[nm] else NA
    c2  <- if (nm %in% names(co2)) co2[nm] else NA
    s2  <- if (nm %in% names(co2)) se2[nm] else NA
    z   <- (c1 - c2) / sqrt(s1^2 + s2^2)
    p   <- 2*(1 - pnorm(abs(z)))
    sig <- ifelse(is.na(p), "",
           ifelse(p < 0.001, "***", ifelse(p < 0.01, "**",
           ifelse(p < 0.05,  "*",   ifelse(p < 0.10, ".", "")))))
    cat(sprintf("%-22s | %9.4f %6.2f | %9.4f %6.2f | %8.3f %s\n",
      v, c1, c1/s1, c2, c2/s2, z, sig))
  }
}

## --- Design B: B_Q vs B_M ---
cat("\n=== DESIGN B: Contraste B_Q vs B_M ===\n")
cat("Predicciones del modelo de agencia multitarea:\n")
cat("  H_B1: efectos organizativos difieren entre Q y M\n")
cat("  H_B2: ShareQ modera el efecto (signos opuestos en Q vs M)\n")
cat("  H_B3: moderación de ShareQ más intensa en M que en Q\n\n")

z_contrast(m_B_Q, m_B_M, vars_B, "B_Q", "B_M")

cat("\n--- TE medio por grupo organizativo ---\n")
te_BQ <- efficiencies(m_B_Q)
te_BM <- efficiencies(m_B_M)
for (grp in c("Pub_Retro","Priv_Conc","Priv_Merc")) {
  idx <- !is.na(sample_B$grupo_pago) & sample_B$grupo_pago == grp
  cat(sprintf("  %-12s  TE_Q=%.3f  TE_M=%.3f  n=%d\n",
    grp,
    mean(te_BQ[idx], na.rm = TRUE),
    mean(te_BM[idx], na.rm = TRUE),
    sum(idx)))
}

cat("\n--- Correlación TE_Q vs TE_M ---\n")
cat(sprintf("  Pearson  = %.4f\n", cor(te_BQ, te_BM, use="complete.obs")))
cat(sprintf("  Spearman = %.4f\n",
    cor(te_BQ, te_BM, method="spearman", use="complete.obs")))
cat("  (negativa esperada si hay sustitución de esfuerzo)\n")

## --- Design C ---
cat("\n\n=== DESIGN C: Panel hospital x servicio ===\n")
cat("Predicción clave: ShareQ_Cs negativo y significativo\n\n")
co_C <- coef(m_C)
se_C <- tryCatch(sqrt(diag(vcov(m_C))), error=function(e) rep(NA, length(co_C)))
for (v in c("d_Priv_Conc","d_Priv_Merc","C_s",
            "Priv_Conc_Cs","Priv_Merc_Cs","ShareQ","ShareQ_Cs")) {
  nm <- paste0("Z_", v)
  if (nm %in% names(co_C)) {
    t   <- co_C[nm] / se_C[nm]
    sig <- ifelse(abs(t)>3.3,"***",ifelse(abs(t)>2.6,"**",
           ifelse(abs(t)>1.96,"*", ifelse(abs(t)>1.64,".","  "))))
    cat(sprintf("  %-20s coef=%9.4f  t=%7.3f  %s\n",
      v, co_C[nm], t, sig))
  }
}

## --- Design D: ambas especificaciones ---
cat("\n\n=== DESIGN D: comparación ODF clásica vs frontera cantidad ===\n")

report_D <- function(m, label) {
  if (is.null(m)) { cat(label, ": no estimado\n"); return(invisible(NULL)) }
  co <- coef(m)
  se <- tryCatch(sqrt(diag(vcov(m))), error=function(e) rep(NA,length(co)))
  cat(sprintf("\n-- %s --\n", label))
  cat(sprintf("  N=%-5d  logLik=%.1f  TE=%.3f  gamma=%.4f\n",
    nobs(m), logLik(m)[[1]],
    mean(efficiencies(m), na.rm=TRUE),
    co["gamma"]))
  # coeficiente del segundo output (ln_i_w) en la frontera
  if ("ln_i_w" %in% names(co))
    cat(sprintf("  ln_i_w (frontera):  coef=%8.4f  t=%7.3f\n",
      co["ln_i_w"], co["ln_i_w"]/se["ln_i_w"]))
  # z-variables principales
  for (v in c("D_desc","pct_sns","desc_pago")) {
    nm <- paste0("Z_", v)
    if (nm %in% names(co))
      cat(sprintf("  %-15s coef=%8.4f  t=%7.3f\n",
        v, co[nm], co[nm]/se[nm]))
  }
}

report_D(m_D_v4, "D_v4 ODF clasica [-ln(Q), ineffDecrease=FALSE]")
report_D(m_D_v5, "D_v5 Frontera cantidad [ln(Q), ineffDecrease=TRUE]")

cat("\n  Criterio de selección:\n")
cat("  Usar el modelo con skewness OLS NEGATIVO (ver diagnóstico arriba)\n")
cat("  y gamma entre 0.05 y 0.95\n")

# tabla resumen comparativa
cat(sprintf("\n  %-6s  %-10s  %-6s  %-6s  %-12s  %-8s\n",
    "Model","logLik","gamma","TE","D_desc(coef)","D_desc(t)"))
for (lst in list(list(m_D_v4,"D_v4"), list(m_D_v5,"D_v5"))) {
  m <- lst[[1]]; nm_m <- lst[[2]]
  if (is.null(m)) { cat(sprintf("  %-6s  no estimado\n", nm_m)); next }
  co <- coef(m)
  se <- tryCatch(sqrt(diag(vcov(m))),error=function(e) rep(NA,length(co)))
  dd_c <- if ("Z_D_desc" %in% names(co)) co["Z_D_desc"] else NA
  dd_t <- if ("Z_D_desc" %in% names(co)) co["Z_D_desc"]/se["Z_D_desc"] else NA
  cat(sprintf("  %-6s  %10.1f  %6.4f  %6.3f  %12.4f  %8.3f\n",
    nm_m, logLik(m)[[1]], co["gamma"],
    mean(efficiencies(m), na.rm=TRUE),
    dd_c, dd_t))
}
## --- Resumen verificación predicciones ---
cat("\n\n=== RESUMEN: verificación predicciones modelo teórico ===\n")

check <- function(label, cond, detail) {
  cat(sprintf("  [%s] %-50s (%s)\n",
    ifelse(cond, "✓", "✗"), label, detail))
}

co_BQ <- coef(m_B_Q)
co_BM <- coef(m_B_M)
se_BQ <- tryCatch(sqrt(diag(vcov(m_B_Q))), error=function(e) rep(NA, length(co_BQ)))
se_BM <- tryCatch(sqrt(diag(vcov(m_B_M))), error=function(e) rep(NA, length(co_BM)))

sq_BQ  <- if ("Z_ShareQ" %in% names(co_BQ)) co_BQ["Z_ShareQ"] else NA
sq_BM  <- if ("Z_ShareQ" %in% names(co_BM)) co_BM["Z_ShareQ"] else NA
msq_BQ <- if ("Z_Merc_shareQ" %in% names(co_BQ)) co_BQ["Z_Merc_shareQ"] else NA
msq_BM <- if ("Z_Merc_shareQ" %in% names(co_BM)) co_BM["Z_Merc_shareQ"] else NA
z_sq   <- (sq_BQ - sq_BM) / sqrt(se_BQ["Z_ShareQ"]^2 + se_BM["Z_ShareQ"]^2)
sq_Cs  <- if ("Z_ShareQ_Cs" %in% names(co_C)) co_C["Z_ShareQ_Cs"] else NA

check("H_B1: efectos org. difieren Q vs M (z_ShareQ sign.)",
      !is.na(z_sq) && abs(z_sq) > 1.96,
      sprintf("z_ShareQ=%.2f", z_sq))
check("H_B2: ShareQ negativo en B_Q (complementariedad qx)",
      !is.na(sq_BQ) && sq_BQ < 0,
      sprintf("ShareQ_Q=%.3f", sq_BQ))
check("H_B2: ShareQ positivo en B_M (sustitución médico)",
      !is.na(sq_BM) && sq_BM > 0,
      sprintf("ShareQ_M=%.3f", sq_BM))
check("H_B3: |Merc_shareQ| mayor en M que en Q",
      !is.na(msq_BQ) && !is.na(msq_BM) && abs(msq_BM) > abs(msq_BQ),
      sprintf("|Merc_Q|=%.3f vs |Merc_M|=%.3f", abs(msq_BQ), abs(msq_BM)))
check("C: ShareQ_Cs negativo (intra-hospital)",
      !is.na(sq_Cs) && sq_Cs < 0,
      sprintf("ShareQ_Cs=%.3f", sq_Cs))
# Elegir el modelo D con gamma válido para el check
m_D_ok <- if (!is.null(m_D_v5) &&
               coef(m_D_v5)["gamma"] > 0.05 &&
               coef(m_D_v5)["gamma"] < 0.995) m_D_v5 else m_D_v4
co_D_ok <- if (!is.null(m_D_ok)) coef(m_D_ok) else c(gamma=NA)

check("D: gamma identificable (>0.05)",
      !is.na(co_D_ok["gamma"]) && co_D_ok["gamma"] > 0.05,
      sprintf("D_v4 gamma=%.3f  D_v5 gamma=%.3f",
        ifelse(!is.null(m_D_v4), coef(m_D_v4)["gamma"], NA),
        ifelse(!is.null(m_D_v5), coef(m_D_v5)["gamma"], NA)))
## --- Archivos guardados ---
cat("\n\nSaved:\n")
cat(" - sfa_B_Q_v3.rds\n")
cat(" - sfa_B_M_v3.rds\n")
cat(" - tabla_contrastes_B_v3.csv\n")
cat(" - sfa_C_stacked_service_v3.rds\n")
cat(" - sfa_D_v4_odf.rds       (ODF clásica: -ln_Q, ineffDecrease=FALSE)\n")
cat(" - sfa_D_v5_cantidad.rds  (frontera cantidad: ln_Q, ineffDecrease=TRUE)\n")
cat("\n========== Phase 7 complete ==========\n")