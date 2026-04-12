source("G:/Mi unidad/SIAE 2010-2020/scripts/00_config.R")
library(sfaR); library(dplyr)
load(file.path(INT_DIR, "df_sfa.RData"))

ccaa_vars <- grep("^d_ccaa_[0-9]+$", names(df_sfa),
                  value=TRUE)

# ── Muestras ─────────────────────────────────────────────────
df_A <- df_sfa %>%
  filter(es_agudo==1, !anyo %in% 2020:2022,
         altTotal_bruto>=200,
         is.finite(ln_altTotal_pond),
         is.finite(ln_i_diag),
         ln_i_diag > quantile(ln_i_diag,0.01,na.rm=TRUE),
         !is.na(ln_L_total_c), !is.na(ln_K_camas_c),
         !is.na(D_desc), !is.na(pct_sns),
         !is.na(grupo_cluster), !is.na(ccaa_cod))

df_B <- df_sfa %>%
  filter(es_agudo==1, !anyo %in% 2020:2022,
         altQ_bruto>=200, altM_bruto>=200,
         is.finite(ln_altQ_pond),
         is.finite(ln_altM_pond),
         !is.na(ln_L_total_c), !is.na(ln_K_camas_c),
         !is.na(D_desc), !is.na(pct_sns),
         !is.na(grupo_cluster), !is.na(ccaa_cod))

cat("Muestra A:", nrow(df_A), "obs\n")
cat("Muestra B:", nrow(df_B), "obs\n")

# ── Log-verosimilitudes de los modelos irrestrictos ──────────
# (ya estimados — cargar directamente)
m_Tot <- readRDS(file.path(INT_DIR,
         "sfa_mTot_Ddesc_definitivo.rds"))
m_I   <- readRDS(file.path(INT_DIR,
         "sfa_mI_Ddesc_definitivo.rds"))
m_Q   <- readRDS(file.path(INT_DIR,
         "sfa_modeloQ_definitivo.rds"))
m_M   <- readRDS(file.path(INT_DIR,
         "sfa_modeloM_definitivo.rds"))

ll_Tot <- as.numeric(m_Tot$mlLoglik)
ll_I   <- as.numeric(m_I$mlLoglik)
ll_Q   <- as.numeric(m_Q$mlLoglik)
ll_M   <- as.numeric(m_M$mlLoglik)

# Log-verosimilitud irrestricta = suma de las dos fronteras
ll_irrestricto_A <- ll_Tot + ll_I
ll_irrestricto_B <- ll_Q   + ll_M

cat(sprintf("\nDiseño A: ll_Tot=%.2f ll_I=%.2f",
            ll_Tot, ll_I))
cat(sprintf("\n  ll_irrestricto = %.2f\n", ll_irrestricto_A))
cat(sprintf("\nDiseño B: ll_Q=%.2f ll_M=%.2f",
            ll_Q, ll_M))
cat(sprintf("\n  ll_irrestricto = %.2f\n", ll_irrestricto_B))

# ── Modelos restringidos (H0: alpha = delta) ─────────────────
# El modelo restringido apila las dos muestras y estima
# una única frontera imponiendo que los coeficientes de
# ineficiencia son los mismos para ambas dimensiones.
# Se añade una dummy de output (d_output2) para capturar
# el nivel distinto de cada frontera.

# Diseño A restringido: apilar ln_altTotal_pond y ln_i_diag
uhet_restr <- as.formula(paste(
  "~ D_desc + pct_sns + desc_pago +",
  "ShareQ + desc_shareQ +",
  paste(ccaa_vars, collapse=" + ")))

frontier_restr <- ~ ln_L_total_c + ln_K_camas_c +
  ln_K_tech_c + ln_L_total_c2 + ln_K_camas_c2 +
  trend + trend2 +
  d_cluster2+d_cluster3+d_cluster4+d_cluster5+
  d_output2  # dummy que diferencia las dos fronteras

cat("\n═══ Modelo restringido Diseño A ═══\n")
cat("Apilando muestras...\n")

# Apilar: output1 = cantidad, output2 = intensidad
df_A1 <- df_A %>%
  mutate(
    ln_y    = ln_altTotal_pond,
    d_output2 = 0L
  )
df_A2 <- df_A %>%
  mutate(
    ln_y    = ln_i_diag,
    d_output2 = 1L
  )
df_A_stack <- bind_rows(df_A1, df_A2)
cat(sprintf("Muestra apilada A: %d obs\n",
            nrow(df_A_stack)))

# Estimar modelo restringido
m_A_restr <- NULL
for (cfg in list(
  list(ud="tnormal",mt="bfgs",ht=1L),
  list(ud="tnormal",mt="bhhh",ht=1L),
  list(ud="hnormal",mt="bfgs",ht=1L),
  list(ud="hnormal",mt="bhhh",ht=2L)
)) {
  cat(sprintf("  Intentando %s/%s...\n",
              cfg$ud, cfg$mt))
  m <- tryCatch(
    suppressWarnings(sfaR::sfacross(
      update(frontier_restr, ln_y ~ .),
      uhet = uhet_restr,
      data = df_A_stack,
      S=1L, udist=cfg$ud,
      method=cfg$mt, hessianType=cfg$ht)),
    error=function(e){
      cat("  Error:", e$message, "\n"); NULL})
  if (is.null(m)) next
  ll  <- as.numeric(m$mlLoglik)
  cf  <- coef(m)
  se  <- tryCatch(sqrt(diag(vcov(m))),
                  error=function(e) rep(NA,length(cf)))
  idx <- grep("^Zu_",names(cf))
  if (!is.finite(ll)||abs(ll)>1e10||
      any(abs(cf)>100,na.rm=TRUE)||
      any(!is.finite(se[idx]),na.rm=TRUE)){
    cat("  Degenerado\n"); next
  }
  cat(sprintf("  OK: ll=%.2f [%s/%s]\n",
              ll, cfg$ud, cfg$mt))
  m_A_restr <- m
  break
}

if (!is.null(m_A_restr)) {
  ll_restr_A <- as.numeric(m_A_restr$mlLoglik)

  # Test LR: LR = 2*(ll_irrestricto - ll_restringido)
  # gl = número de parámetros restringidos =
  #      número de parámetros en uhet (sin intercepto)
  #      porque imponemos que son iguales en ambas fronteras
  n_pars_uhet <- length(grep("^Zu_[^I]",
                             names(coef(m_Tot))))
  # gl = n_pars_uhet porque bajo H0 los coeficientes
  # de ineficiencia son iguales (ahorramos n_pars_uhet params)
  gl_A <- n_pars_uhet
  lr_A <- 2*(ll_irrestricto_A - ll_restr_A)
  pv_A <- 1 - pchisq(lr_A, df=gl_A)

  cat(sprintf("\n[DISEÑO A — Test LR P*]\n"))
  cat(sprintf("  ll irrestricto (Tot+I): %.2f\n",
              ll_irrestricto_A))
  cat(sprintf("  ll restringido (apilado): %.2f\n",
              ll_restr_A))
  cat(sprintf("  LR = %.4f\n", lr_A))
  cat(sprintf("  gl = %d\n", gl_A))
  cat(sprintf("  p-valor = %.6f\n", pv_A))
  cat(sprintf("  Resultado: %s\n",
    ifelse(pv_A < 0.001,
      "Rechaza H0 al 0.1% — vectores distintos (P* ✓✓✓)",
    ifelse(pv_A < 0.01,
      "Rechaza H0 al 1% — vectores distintos (P* ✓✓)",
    ifelse(pv_A < 0.05,
      "Rechaza H0 al 5% — vectores distintos (P* ✓)",
      "No rechaza H0")))))

  saveRDS(m_A_restr,
          file.path(INT_DIR, "sfa_mA_restringido.rds"))
}

# ── Diseño B restringido ──────────────────────────────────────
cat("\n═══ Modelo restringido Diseño B ═══\n")

uhet_restr_B <- as.formula(paste(
  "~ d_Priv_Conc + d_Priv_Merc +",
  "ShareQ + Conc_shareQ + Merc_shareQ +",
  paste(ccaa_vars, collapse=" + ")))

frontier_restr_B <- ~ ln_L_total_c + ln_K_camas_c +
  ln_K_tech_c + ln_L_total_c2 + ln_K_camas_c2 +
  trend + trend2 +
  d_cluster2+d_cluster3+d_cluster4+d_cluster5+
  d_output2

df_B1 <- df_B %>%
  mutate(ln_y=ln_altQ_pond, d_output2=0L)
df_B2 <- df_B %>%
  mutate(ln_y=ln_altM_pond, d_output2=1L)
df_B_stack <- bind_rows(df_B1, df_B2)
cat(sprintf("Muestra apilada B: %d obs\n",
            nrow(df_B_stack)))

m_B_restr <- NULL
for (cfg in list(
  list(ud="tnormal",mt="bfgs",ht=1L),
  list(ud="tnormal",mt="bhhh",ht=1L),
  list(ud="hnormal",mt="bfgs",ht=1L),
  list(ud="hnormal",mt="bhhh",ht=2L)
)) {
  cat(sprintf("  Intentando %s/%s...\n",
              cfg$ud, cfg$mt))
  m <- tryCatch(
    suppressWarnings(sfaR::sfacross(
      update(frontier_restr_B, ln_y ~ .),
      uhet = uhet_restr_B,
      data = df_B_stack,
      S=1L, udist=cfg$ud,
      method=cfg$mt, hessianType=cfg$ht)),
    error=function(e){
      cat("  Error:", e$message, "\n"); NULL})
  if (is.null(m)) next
  ll  <- as.numeric(m$mlLoglik)
  cf  <- coef(m)
  se  <- tryCatch(sqrt(diag(vcov(m))),
                  error=function(e) rep(NA,length(cf)))
  idx <- grep("^Zu_",names(cf))
  if (!is.finite(ll)||abs(ll)>1e10||
      any(abs(cf)>100,na.rm=TRUE)||
      any(!is.finite(se[idx]),na.rm=TRUE)){
    cat("  Degenerado\n"); next
  }
  cat(sprintf("  OK: ll=%.2f [%s/%s]\n",
              ll, cfg$ud, cfg$mt))
  m_B_restr <- m
  break
}

if (!is.null(m_B_restr)) {
  ll_restr_B <- as.numeric(m_B_restr$mlLoglik)
  n_pars_uhet_B <- length(grep("^Zu_[^I]",
                               names(coef(m_Q))))
  gl_B <- n_pars_uhet_B
  lr_B <- 2*(ll_irrestricto_B - ll_restr_B)
  pv_B <- 1 - pchisq(lr_B, df=gl_B)

  cat(sprintf("\n[DISEÑO B — Test LR P*]\n"))
  cat(sprintf("  ll irrestricto (Q+M): %.2f\n",
              ll_irrestricto_B))
  cat(sprintf("  ll restringido (apilado): %.2f\n",
              ll_restr_B))
  cat(sprintf("  LR = %.4f\n", lr_B))
  cat(sprintf("  gl = %d\n", gl_B))
  cat(sprintf("  p-valor = %.6f\n", pv_B))
  cat(sprintf("  Resultado: %s\n",
    ifelse(pv_B < 0.001,
      "Rechaza H0 al 0.1% — vectores distintos (P* ✓✓✓)",
    ifelse(pv_B < 0.01,
      "Rechaza H0 al 1% — vectores distintos (P* ✓✓)",
    ifelse(pv_B < 0.05,
      "Rechaza H0 al 5% — vectores distintos (P* ✓)",
      "No rechaza H0")))))

  saveRDS(m_B_restr,
          file.path(INT_DIR, "sfa_mB_restringido.rds"))
}

cat("\n=== Test LR P* completado ===\n")