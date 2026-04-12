# ============================================================
# 14_bootstrap_wald.R
# Bootstrap Wald definitivo B=4999
# Test P*: H0: alpha = delta (vectores de ineficiencia iguales)
# Diseño A (D_desc): cantidad vs intensidad
# Diseño B (grupos pago): quirúrgico vs médico
# NOTA: puede tardar 4-8 horas — ejecutar overnight
# ============================================================

config_path <- if (file.exists("scripts/00_config.R"))
  "scripts/00_config.R" else "00_config.R"
source(config_path)
if (!requireNamespace("sfaR", quietly = TRUE))
  stop("Instala sfaR: install.packages('sfaR')")
library(sfaR); library(dplyr)
load(file.path(INT_DIR, "df_sfa.RData"))

B_boot <- 4999
set.seed(42)

message("═══════════════════════════════════════")
message("Bootstrap Wald definitivo B=", B_boot)
message("Inicio: ", format(Sys.time()))
message("═══════════════════════════════════════")

# ── Reconstruir muestras ─────────────────────────────────────
ccaa_vars <- grep("^d_ccaa_[0-9]+$", names(df_sfa),
                  value = TRUE)

df_A <- df_sfa %>%
  filter(es_agudo == 1, !anyo %in% 2020:2022,
         altTotal_bruto >= 200,
         is.finite(ln_altTotal_pond),
         is.finite(ln_i_diag),
         ln_i_diag > quantile(ln_i_diag, 0.01, na.rm = TRUE),
         !is.na(ln_L_total_c), !is.na(ln_K_camas_c),
         (!"d_desc_estimable" %in% names(.) | d_desc_estimable == 1L),
         !is.na(D_desc), !is.na(pct_sns),
         !is.na(grupo_cluster), !is.na(ccaa_cod))

df_B <- df_sfa %>%
  filter(es_agudo == 1, !anyo %in% 2020:2022,
         altQ_bruto >= 200, altM_bruto >= 200,
         is.finite(ln_altQ_pond),
         is.finite(ln_altM_pond),
         !is.na(ln_L_total_c), !is.na(ln_K_camas_c),
         (!"d_desc_estimable" %in% names(.) | d_desc_estimable == 1L),
         !is.na(D_desc), !is.na(pct_sns),
         !is.na(grupo_cluster), !is.na(ccaa_cod))

message("Muestra A: ", nrow(df_A), " obs")
message("Muestra B: ", nrow(df_B), " obs")

# ── Especificaciones ─────────────────────────────────────────
frontier_str <- ~ ln_L_total_c + ln_K_camas_c +
  ln_K_tech_c + ln_L_total_c2 + ln_K_camas_c2 +
  trend + trend2 +
  d_cluster2 + d_cluster3 + d_cluster4 + d_cluster5

# Con CCAA para modelos principales
uhet_A_full <- as.formula(paste(
  "~ D_desc + pct_sns + desc_pago +",
  "ShareQ + desc_shareQ +",
  paste(ccaa_vars, collapse = " + ")))

uhet_B_full <- as.formula(paste(
  "~ d_Priv_Conc + d_Priv_Merc +",
  "ShareQ + Conc_shareQ + Merc_shareQ +",
  paste(ccaa_vars, collapse = " + ")))

# Sin CCAA para bootstrap (más rápido y estable)
uhet_A_boot <- ~ D_desc + pct_sns + desc_pago +
                  ShareQ + desc_shareQ

uhet_B_boot <- ~ d_Priv_Conc + d_Priv_Merc +
                  ShareQ + Conc_shareQ + Merc_shareQ

# ── Detección de degeneración ────────────────────────────────
es_degen <- function(m) {
  if (is.null(m)) return(TRUE)
  ll <- tryCatch(as.numeric(m$mlLoglik),
                 error = function(e) NA)
  if (!is.finite(ll) || abs(ll) > 1e10) return(TRUE)
  cf <- tryCatch(coef(m), error = function(e) NULL)
  if (is.null(cf) || any(!is.finite(cf))) return(TRUE)
  if (any(abs(cf) > 100, na.rm = TRUE)) return(TRUE)
  FALSE
}

# ── Función de estadístico Wald ───────────────────────────────
wald_stat <- function(cf1, cf2, idx1, idx2) {
  if (length(idx1) != length(idx2)) return(NA_real_)
  d <- cf1[idx1] - cf2[idx2]
  as.numeric(t(d) %*% d)
}

# ── Función de estimación bootstrap (un intento rápido) ──────
estimar_boot <- function(fml, uhet, data, udist) {
  m <- NULL
  capture.output({
    m <- tryCatch(
      suppressWarnings(sfaR::sfacross(
        fml, uhet = uhet, data = data, S = 1L,
        udist = udist, method = "bfgs",
        hessianType = 1L)),
      error = function(e) NULL)
  })
  m
}

# ════════════════════════════════════════════════════════
# BOOTSTRAP DISEÑO A — cantidad vs intensidad (D_desc)
# Parámetros: D_desc, pct_sns, desc_pago, ShareQ,
#             desc_shareQ
# ════════════════════════════════════════════════════════

message("\n═══ Bootstrap Diseño A ═══")

m_Tot <- readRDS(file.path(INT_DIR,
         "sfa_mTot_Ddesc_definitivo.rds"))
m_I   <- readRDS(file.path(INT_DIR,
         "sfa_mI_Ddesc_definitivo.rds"))

pars_A  <- c("D_desc", "pct_sns", "desc_pago",
             "ShareQ", "desc_shareQ")
idx_A_T <- paste0("Zu_", pars_A)
idx_A_T <- idx_A_T[idx_A_T %in% names(coef(m_Tot))]
idx_A_I <- paste0("Zu_", pars_A)
idx_A_I <- idx_A_I[idx_A_I %in% names(coef(m_I))]
idx_A_T <- idx_A_T[idx_A_T %in% idx_A_I]
idx_A_I <- idx_A_T

cf_Tot <- coef(m_Tot); cf_I <- coef(m_I)
wald_obs_A <- wald_stat(cf_Tot, cf_I, idx_A_T, idx_A_I)
message(sprintf("Estadístico observado Diseño A: %.4f",
                wald_obs_A))

udist_Tot <- tryCatch(m_Tot$udist,
                      error = function(e) "tnormal")
udist_I   <- tryCatch(m_I$udist,
                      error = function(e) "tnormal")

fml_Tot_b <- update(frontier_str, ln_altTotal_pond ~ .)
fml_I_b   <- update(frontier_str, ln_i_diag ~ .)

wald_boot_A <- rep(NA_real_, B_boot)
n_ok_A <- 0L
t_inicio_A <- Sys.time()

for (b in seq_len(B_boot)) {
  idx_b <- sample(nrow(df_A), replace = TRUE)
  db    <- df_A[idx_b, ]

  mT_b <- estimar_boot(fml_Tot_b, uhet_A_boot,
                       db, udist_Tot)
  mI_b <- estimar_boot(fml_I_b,   uhet_A_boot,
                       db, udist_I)

  if (!es_degen(mT_b) && !es_degen(mI_b)) {
    cf_Tb <- coef(mT_b); cf_Ib <- coef(mI_b)
    idx_Tb <- idx_A_T[idx_A_T %in% names(cf_Tb)]
    idx_Ib <- idx_A_I[idx_A_I %in% names(cf_Ib)]
    if (length(idx_Tb) == length(idx_Ib) &&
        length(idx_Tb) > 0) {
      w_b <- wald_stat(cf_Tb, cf_Ib, idx_Tb, idx_Ib)
      if (is.finite(w_b)) {
        wald_boot_A[b] <- w_b
        n_ok_A <- n_ok_A + 1L
      }
    }
  }

  if (b %% 100 == 0) {
    elapsed <- as.numeric(Sys.time() - t_inicio_A,
                          units = "mins")
    eta <- elapsed / b * (B_boot - b)
    message(sprintf(
      "  A: %d/%d (ok=%d) — %.1f min transcurridos, ETA %.1f min",
      b, B_boot, n_ok_A, elapsed, eta))
    if (b %% 500 == 0)
      saveRDS(wald_boot_A,
              file.path(INT_DIR,
              "sfa_wald_boot_A_parcial.rds"))
  }
}

p_wald_A <- mean(wald_boot_A >= wald_obs_A, na.rm = TRUE)
message(sprintf(
  "\nResultado Bootstrap Diseño A (B_ok=%d/%d):",
  n_ok_A, B_boot))
message(sprintf("  Estadístico observado: %.4f", wald_obs_A))
message(sprintf("  p-valor bootstrap:     %.4f", p_wald_A))
message(sprintf("  Interpretación: %s",
  ifelse(p_wald_A < 0.05,
    "Rechaza H0: vectores de ineficiencia DISTINTOS (P* ✓)",
    "No rechaza H0: vectores similares")))

saveRDS(wald_boot_A,
        file.path(INT_DIR, "sfa_wald_boot_A_definitivo.rds"))

# ════════════════════════════════════════════════════════
# BOOTSTRAP DISEÑO B — quirúrgico vs médico (grupos pago)
# Parámetros: d_Priv_Conc, d_Priv_Merc,
#             ShareQ, Conc_shareQ, Merc_shareQ
# ════════════════════════════════════════════════════════

message("\n═══ Bootstrap Diseño B ═══")

m_Q <- readRDS(file.path(INT_DIR,
       "sfa_modeloQ_definitivo.rds"))
m_M <- readRDS(file.path(INT_DIR,
       "sfa_modeloM_definitivo.rds"))

pars_B  <- c("d_Priv_Conc", "d_Priv_Merc",
             "ShareQ", "Conc_shareQ", "Merc_shareQ")
idx_B_Q <- paste0("Zu_", pars_B)
idx_B_Q <- idx_B_Q[idx_B_Q %in% names(coef(m_Q))]
idx_B_M <- paste0("Zu_", pars_B)
idx_B_M <- idx_B_M[idx_B_M %in% names(coef(m_M))]
idx_B_Q <- idx_B_Q[idx_B_Q %in% idx_B_M]
idx_B_M <- idx_B_Q

cf_Q <- coef(m_Q); cf_M <- coef(m_M)
wald_obs_B <- wald_stat(cf_Q, cf_M, idx_B_Q, idx_B_M)
message(sprintf("Estadístico observado Diseño B: %.4f",
                wald_obs_B))

udist_Q <- tryCatch(m_Q$udist,
                    error = function(e) "tnormal")
udist_M <- tryCatch(m_M$udist,
                    error = function(e) "tnormal")

fml_Q_b <- update(frontier_str, ln_altQ_pond ~ .)
fml_M_b <- update(frontier_str, ln_altM_pond ~ .)

wald_boot_B <- rep(NA_real_, B_boot)
n_ok_B <- 0L
t_inicio_B <- Sys.time()

for (b in seq_len(B_boot)) {
  idx_b <- sample(nrow(df_B), replace = TRUE)
  db    <- df_B[idx_b, ]

  mQ_b <- estimar_boot(fml_Q_b, uhet_B_boot,
                       db, udist_Q)
  mM_b <- estimar_boot(fml_M_b, uhet_B_boot,
                       db, udist_M)

  if (!es_degen(mQ_b) && !es_degen(mM_b)) {
    cf_Qb <- coef(mQ_b); cf_Mb <- coef(mM_b)
    idx_Qb <- idx_B_Q[idx_B_Q %in% names(cf_Qb)]
    idx_Mb <- idx_B_M[idx_B_M %in% names(cf_Mb)]
    if (length(idx_Qb) == length(idx_Mb) &&
        length(idx_Qb) > 0) {
      w_b <- wald_stat(cf_Qb, cf_Mb, idx_Qb, idx_Mb)
      if (is.finite(w_b)) {
        wald_boot_B[b] <- w_b
        n_ok_B <- n_ok_B + 1L
      }
    }
  }

  if (b %% 100 == 0) {
    elapsed <- as.numeric(Sys.time() - t_inicio_B,
                          units = "mins")
    eta <- elapsed / b * (B_boot - b)
    message(sprintf(
      "  B: %d/%d (ok=%d) — %.1f min transcurridos, ETA %.1f min",
      b, B_boot, n_ok_B, elapsed, eta))
    if (b %% 500 == 0)
      saveRDS(wald_boot_B,
              file.path(INT_DIR,
              "sfa_wald_boot_B_parcial.rds"))
  }
}

p_wald_B <- mean(wald_boot_B >= wald_obs_B, na.rm = TRUE)
message(sprintf(
  "\nResultado Bootstrap Diseño B (B_ok=%d/%d):",
  n_ok_B, B_boot))
message(sprintf("  Estadístico observado: %.4f", wald_obs_B))
message(sprintf("  p-valor bootstrap:     %.4f", p_wald_B))
message(sprintf("  Interpretación: %s",
  ifelse(p_wald_B < 0.05,
    "Rechaza H0: vectores de ineficiencia DISTINTOS (P* ✓)",
    "No rechaza H0: vectores similares")))

saveRDS(wald_boot_B,
        file.path(INT_DIR, "sfa_wald_boot_B_definitivo.rds"))

# ════════════════════════════════════════════════════════
# GUARDAR RESULTADOS FINALES
# ════════════════════════════════════════════════════════

resultados_bootstrap <- list(
  diseno_A = list(
    wald_obs = wald_obs_A,
    p_valor  = p_wald_A,
    n_ok     = n_ok_A,
    B        = B_boot,
    pars     = pars_A,
    udist    = c(Tot = udist_Tot, I = udist_I)
  ),
  diseno_B = list(
    wald_obs = wald_obs_B,
    p_valor  = p_wald_B,
    n_ok     = n_ok_B,
    B        = B_boot,
    pars     = pars_B,
    udist    = c(Q = udist_Q, M = udist_M)
  ),
  fecha_inicio = t_inicio_A,
  fecha_fin    = Sys.time()
)

saveRDS(resultados_bootstrap,
        file.path(INT_DIR,
        "sfa_bootstrap_resultados_definitivos.rds"))

sink(file.path(INT_DIR, "sfa_bootstrap_resumen.txt"))
cat("BOOTSTRAP WALD DEFINITIVO\n")
cat("B =", B_boot, "\n")
cat("Fecha:", format(Sys.time()), "\n\n")
cat("DISEÑO A — cantidad vs intensidad (D_desc)\n")
cat(sprintf("  Estadístico: %.4f\n", wald_obs_A))
cat(sprintf("  p-valor:     %.4f\n", p_wald_A))
cat(sprintf("  B_ok:        %d/%d\n", n_ok_A, B_boot))
cat("\nDISEÑO B — quirúrgico vs médico (grupos pago)\n")
cat(sprintf("  Estadístico: %.4f\n", wald_obs_B))
cat(sprintf("  p-valor:     %.4f\n", p_wald_B))
cat(sprintf("  B_ok:        %d/%d\n", n_ok_B, B_boot))
sink()

message("\n═══════════════════════════════════════")
message("Bootstrap completado.")
message("Fin: ", format(Sys.time()))
message("Resultados en sfa_bootstrap_resultados_definitivos.rds")
message("═══════════════════════════════════════")
message("\n=== Script 14 completado ===")
