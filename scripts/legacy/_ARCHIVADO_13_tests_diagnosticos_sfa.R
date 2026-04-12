# ============================================================
# 13_tests_diagnosticos_sfa.R
# Tests formales sobre los modelos SFA estimados:
# 1. Correlación entre ineficiencias (Pearson/Spearman)
# 2. LR Cobb-Douglas vs Translog
# 3. Kodde-Palm (existencia de ineficiencia)
# 4. Wald individual P1 vs P2
# ============================================================

config_path <- if (file.exists("scripts/00_config.R"))
  "scripts/00_config.R" else "00_config.R"
source(config_path)
library(sfaR); library(dplyr)
load(file.path(INT_DIR, "df_sfa.RData"))

# ── Cargar modelos principales ───────────────────────────────
m_Tot  <- readRDS(file.path(INT_DIR,
          "sfa_mTot_Ddesc_definitivo.rds"))
m_I    <- readRDS(file.path(INT_DIR,
          "sfa_mI_Ddesc_definitivo.rds"))
m_Q    <- readRDS(file.path(INT_DIR,
          "sfa_modeloQ_definitivo.rds"))
m_M    <- readRDS(file.path(INT_DIR,
          "sfa_modeloM_definitivo.rds"))
m_Tot_g <- readRDS(file.path(INT_DIR,
           "sfa_modeloTotal_definitivo.rds"))
m_I_g   <- readRDS(file.path(INT_DIR,
           "sfa_modeloI_definitivo.rds"))

# ── Reconstruir muestras exactas ────────────────────────────
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

# ── Función auxiliar ─────────────────────────────────────────
sig_stars <- function(p) {
  if (is.na(p)) return("")
  if (p < 0.001) "***" else if (p < 0.01) "**" else
  if (p < 0.05)  "*"   else if (p < 0.10) "."  else ""
}

# ============================================================
# TEST 1 — CORRELACIÓN ENTRE INEFICIENCIAS
# H0: rho(u1, u2) = 0
# Si r > 0.5 y significativo: sistema bivariado recomendado
# Si r < 0.3: fronteras independientes justificadas
# ============================================================

message("\n╔═══════════════════════════════════════════════╗")
message("║  TEST 1 — CORRELACIÓN ENTRE INEFICIENCIAS    ║")
message("╚═══════════════════════════════════════════════╝")

resultados_cor <- list()

for (info in list(
  list(m1 = m_Tot,   m2 = m_I,
       lab = "A_Ddesc (cantidad vs intensidad)"),
  list(m1 = m_Tot_g, m2 = m_I_g,
       lab = "A_grupos (cantidad vs intensidad)"),
  list(m1 = m_Q,     m2 = m_M,
       lab = "B (quirúrgico vs médico)")
)) {
  te1 <- tryCatch(efficiencies(info$m1)$teJLMS,
                  error = function(e) NULL)
  te2 <- tryCatch(efficiencies(info$m2)$teJLMS,
                  error = function(e) NULL)
  if (is.null(te1) || is.null(te2)) {
    message(info$lab, ": TE no disponible — omitido"); next
  }
  if (length(te1) != length(te2)) {
    message(info$lab, ": longitudes distintas — omitido"); next
  }
  u1 <- 1 - te1; u2 <- 1 - te2
  r_p <- cor.test(u1, u2, method = "pearson")
  r_s <- cor.test(u1, u2, method = "spearman", exact = FALSE)
  resultados_cor[[info$lab]] <- list(pearson = r_p, spearman = r_s)

  interp <- ifelse(abs(r_p$estimate) > 0.5,
    "ALTA — sistema bivariado recomendado",
    ifelse(abs(r_p$estimate) > 0.3,
    "MODERADA — mencionar como limitación",
    "BAJA — fronteras independientes OK"))

  message(sprintf("\n[%s]", info$lab))
  message(sprintf("  Pearson  r=%6.3f  p=%.4f %s",
    r_p$estimate, r_p$p.value, sig_stars(r_p$p.value)))
  message(sprintf("  Spearman r=%6.3f  p=%.4f %s",
    r_s$estimate, r_s$p.value, sig_stars(r_s$p.value)))
  message(sprintf("  → Correlación %s", interp))
}

# ============================================================
# TEST 2 — LR COBB-DOUGLAS VS TRANSLOG
# H0: todos los términos de 2º orden = 0
# (Cobb-Douglas es suficiente)
# Rechazar H0: translog preferida
# ============================================================

message("\n╔═══════════════════════════════════════════════╗")
message("║  TEST 2 — LR COBB-DOUGLAS VS TRANSLOG       ║")
message("╚═══════════════════════════════════════════════╝")

uhet_ddesc <- as.formula(paste(
  "~ D_desc + pct_sns + desc_pago +",
  "ShareQ + desc_shareQ +",
  paste(ccaa_vars, collapse = " + ")))

frontier_CD <- ~ ln_L_total_c + ln_K_camas_c +
  ln_K_tech_c + trend

resultados_lr <- list()

for (info in list(
  list(lhs = "ln_altTotal_pond", m_tl = m_Tot,
       data = df_A, lab = "A_Total"),
  list(lhs = "ln_i_diag",        m_tl = m_I,
       data = df_A, lab = "A_I"),
  list(lhs = "ln_altQ_pond",     m_tl = m_Q,
       data = df_B, lab = "B_Q"),
  list(lhs = "ln_altM_pond",     m_tl = m_M,
       data = df_B, lab = "B_M")
)) {
  fml_cd <- update(frontier_CD, paste(info$lhs, "~ ."))
  m_cd <- NULL
  capture.output({
    m_cd <- tryCatch(
      suppressWarnings(sfaR::sfacross(
        fml_cd, uhet = uhet_ddesc, data = info$data,
        S = 1L, udist = "tnormal", method = "bhhh",
        hessianType = 1L)),
      error = function(e) NULL)
  })
  if (is.null(m_cd)) {
    capture.output({
      m_cd <- tryCatch(
        suppressWarnings(sfaR::sfacross(
          fml_cd, uhet = uhet_ddesc, data = info$data,
          S = 1L, udist = "tnormal", method = "bfgs",
          hessianType = 1L)),
        error = function(e) NULL)
    })
  }

  if (is.null(m_cd)) {
    message(info$lab, ": CD no convergió — omitido"); next
  }

  ll_cd <- as.numeric(m_cd$mlLoglik)
  ll_tl <- as.numeric(info$m_tl$mlLoglik)
  lr    <- 2 * (ll_tl - ll_cd)
  # gl: ln_L_c2, ln_K_c2, trend2, d_cluster2-5 = 7 parámetros
  gl    <- 7L
  pv    <- 1 - pchisq(lr, df = gl)

  resultados_lr[[info$lab]] <- list(
    ll_cd = ll_cd, ll_tl = ll_tl, lr = lr, gl = gl, p = pv)

  message(sprintf("\n[%s]", info$lab))
  message(sprintf("  ll CD=%.2f  ll TL=%.2f", ll_cd, ll_tl))
  message(sprintf("  LR=%.2f  gl=%d  p=%.4f %s",
                  lr, gl, pv, sig_stars(pv)))
  message(sprintf("  → %s",
    ifelse(pv < 0.05,
           "TRANSLOG preferida (rechaza CD)",
           "CD no rechazada — forma funcional OK")))
}

# ============================================================
# TEST 3 — KODDE-PALM (EXISTENCIA DE INEFICIENCIA)
# H0: sigma_u = 0 (no hay ineficiencia — OLS suficiente)
# Distribución mixta: valor crítico 5% = 2.706
# ============================================================

message("\n╔═══════════════════════════════════════════════╗")
message("║  TEST 3 — KODDE-PALM                         ║")
message("╚═══════════════════════════════════════════════╝")

frontier_tl_vars <- c(
  "ln_L_total_c", "ln_K_camas_c", "ln_K_tech_c",
  "ln_L_total_c2", "ln_K_camas_c2", "trend", "trend2",
  "d_cluster2", "d_cluster3", "d_cluster4", "d_cluster5")

resultados_kp <- list()

for (info in list(
  list(lhs = "ln_altTotal_pond", m_sfa = m_Tot,
       data = df_A, lab = "A_Total"),
  list(lhs = "ln_i_diag",        m_sfa = m_I,
       data = df_A, lab = "A_I"),
  list(lhs = "ln_altQ_pond",     m_sfa = m_Q,
       data = df_B, lab = "B_Q"),
  list(lhs = "ln_altM_pond",     m_sfa = m_M,
       data = df_B, lab = "B_M")
)) {
  ll_sfa <- as.numeric(info$m_sfa$mlLoglik)

  vars_ok <- intersect(frontier_tl_vars, names(info$data))
  fml_ols <- as.formula(paste(
    info$lhs, "~",
    paste(vars_ok, collapse = " + ")))

  m_ols <- tryCatch(lm(fml_ols, data = info$data),
                    error = function(e) NULL)
  if (is.null(m_ols)) {
    message(info$lab, ": OLS falló — omitido"); next
  }

  ll_ols <- as.numeric(logLik(m_ols))
  lr     <- 2 * (ll_sfa - ll_ols)
  cv5    <- 2.706   # Kodde-Palm 5%
  cv1    <- 5.412   # Kodde-Palm 1%

  resultados_kp[[info$lab]] <- list(
    ll_sfa = ll_sfa, ll_ols = ll_ols, lr = lr)

  message(sprintf(
    "[%s] ll_SFA=%.2f ll_OLS=%.2f LR=%.2f  %s",
    info$lab, ll_sfa, ll_ols, lr,
    ifelse(lr > cv1,
           "→ Inef. significativa al 1% ✓",
           ifelse(lr > cv5,
           "→ Inef. significativa al 5% ✓",
           "→ Inef. NO significativa ✗"))))
}

# ============================================================
# TEST 4 — WALD INDIVIDUAL P1 VS P2
# H0: alpha_k = delta_k para cada parámetro k
# Test de asimetría entre fronteras
# ============================================================

message("\n╔═══════════════════════════════════════════════╗")
message("║  TEST 4 — WALD INDIVIDUAL P1 vs P2          ║")
message("╚═══════════════════════════════════════════════╝")

resultados_wald <- list()

for (info in list(
  list(m1 = m_Tot, m2 = m_I,
       lab = "Diseño A D_desc: cantidad vs intensidad"),
  list(m1 = m_Q,   m2 = m_M,
       lab = "Diseño B: quirúrgico vs médico")
)) {
  cf1 <- coef(info$m1)
  se1 <- tryCatch(sqrt(diag(vcov(info$m1))),
                  error = function(e) rep(NA, length(cf1)))
  cf2 <- coef(info$m2)
  se2 <- tryCatch(sqrt(diag(vcov(info$m2))),
                  error = function(e) rep(NA, length(cf2)))

  idx1 <- grep("^Zu_", names(cf1))
  idx1 <- idx1[!grepl("ccaa", names(cf1)[idx1])]
  pars <- sub("^Zu_", "", names(cf1)[idx1])
  pars <- pars[pars != "(Intercept)"]

  message(sprintf("\n[%s]", info$lab))
  message(sprintf("  %-20s %8s %6s %8s %6s %7s %6s",
    "Parámetro", "α_1", "t1", "δ_2", "t2", "z", "p"))
  message(paste(rep("─", 65), collapse = ""))

  tab_wald <- data.frame()
  for (p in pars) {
    nm <- paste0("Zu_", p)
    if (!nm %in% names(cf2)) next
    dif  <- cf1[nm] - cf2[nm]
    se_d <- sqrt(se1[nm]^2 + se2[nm]^2)
    z    <- dif / se_d
    pv   <- 2 * (1 - pnorm(abs(z)))
    stars <- sig_stars(pv)

    pred <- if (p == "D_desc")
      ifelse(cf1[nm] < 0 & cf2[nm] > 0, "P1+P2 ✓",
      ifelse(cf1[nm] < 0, "P1 solo ✓", "✗"))
    else if (p %in% c("d_Priv_Conc", "d_Priv_Merc"))
      ifelse(cf1[nm] < 0 & cf2[nm] > 0, "P1+P2 ✓",
      ifelse(cf1[nm] < 0, "P1 ✓", "✗"))
    else if (p == "ShareQ")
      ifelse(sign(cf1[nm]) != sign(cf2[nm]), "P4 ✓", "P4 ✗")
    else if (p %in% c("pct_sns", "desc_pago",
                       "Conc_shareQ", "Merc_shareQ"))
      ifelse(sign(cf1[nm]) != sign(cf2[nm]), "P3 ✓", "P3 ✗")
    else ""

    message(sprintf(
      "  %-20s %8.3f %6.2f %8.3f %6.2f %7.3f %6.4f %s %s",
      p, cf1[nm], cf1[nm] / se1[nm],
      cf2[nm], cf2[nm] / se2[nm],
      z, pv, stars, pred))

    tab_wald <- rbind(tab_wald, data.frame(
      param  = p,
      coef_1 = cf1[nm], t_1 = cf1[nm] / se1[nm],
      coef_2 = cf2[nm], t_2 = cf2[nm] / se2[nm],
      z = z, p = pv,
      stringsAsFactors = FALSE))
  }
  resultados_wald[[info$lab]] <- tab_wald
}

# ============================================================
# GUARDAR TODOS LOS RESULTADOS
# ============================================================

message("\n--- Guardando resultados ---")
tests_completos <- list(
  correlacion   = resultados_cor,
  lr_forma_func = resultados_lr,
  kodde_palm    = resultados_kp,
  wald_p1_p2    = resultados_wald,
  fecha         = Sys.time()
)
saveRDS(tests_completos,
        file.path(INT_DIR, "sfa_tests_completos.rds"))

# Tabla resumen para la tesis
sink(file.path(INT_DIR, "sfa_tests_resumen.txt"))
cat("TESTS DIAGNÓSTICOS SFA\n")
cat(format(Sys.time()), "\n\n")

cat("TEST 1 — CORRELACIÓN\n")
for (nm in names(resultados_cor)) {
  r <- resultados_cor[[nm]]$pearson
  cat(sprintf("  %s: r=%.3f p=%.4f\n",
              nm, r$estimate, r$p.value))
}

cat("\nTEST 2 — LR TRANSLOG vs CD\n")
for (nm in names(resultados_lr)) {
  r <- resultados_lr[[nm]]
  cat(sprintf("  %s: LR=%.2f gl=%d p=%.4f\n",
              nm, r$lr, r$gl, r$p))
}

cat("\nTEST 3 — KODDE-PALM\n")
for (nm in names(resultados_kp)) {
  r <- resultados_kp[[nm]]
  cat(sprintf("  %s: LR=%.2f %s\n",
    nm, r$lr,
    ifelse(r$lr > 2.706, "Significativo", "No significativo")))
}
sink()
message("  sfa_tests_completos.rds guardado")
message("  sfa_tests_resumen.txt guardado")
message("\n=== Script 13 completado ===")
