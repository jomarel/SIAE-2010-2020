# ============================================================
# 12_robustez_sfa.R
# Análisis de robustez — especificaciones alternativas
# Variantes: i_simple, D_desc_siae, D_desc_cnh,
#            muestra sin ONG, panel 2010-2019,
#            Cobb-Douglas, distribución hnormal
# ============================================================

source(if (file.exists("scripts/00_config.R"))
       "scripts/00_config.R" else "00_config.R")
library(sfaR); library(dplyr)
load(file.path(INT_DIR, "df_sfa.RData"))

# Cargar modelos principales para comparar
m_Tot_ref <- readRDS(file.path(INT_DIR,
             "sfa_mTot_Ddesc_definitivo.rds"))
m_I_ref   <- readRDS(file.path(INT_DIR,
             "sfa_mI_Ddesc_definitivo.rds"))
m_Q_ref   <- readRDS(file.path(INT_DIR,
             "sfa_modeloQ_definitivo.rds"))
m_M_ref   <- readRDS(file.path(INT_DIR,
             "sfa_modeloM_definitivo.rds"))

# Parámetros de interés para comparación de robustez
pars_key <- c("D_desc", "pct_sns", "desc_pago",
              "ShareQ", "desc_shareQ")

# Función de extracción compacta
coefs_key <- function(m, pars = pars_key) {
  if (is.null(m)) return(NULL)
  cf <- coef(m)
  se <- tryCatch(sqrt(diag(vcov(m))),
                 error = function(e) rep(NA, length(cf)))
  idx <- paste0("Zu_", pars)
  idx <- idx[idx %in% names(cf)]
  data.frame(
    param  = sub("^Zu_", "", idx),
    coef   = round(cf[idx], 4),
    t_stat = round(cf[idx] / se[idx], 3),
    row.names = NULL
  )
}

# Función de estimación robusta (con capture.output)
estimar_rob <- function(formula_lhs, uhet, data,
                        label, frontier = NULL) {
  if (is.null(frontier)) {
    frontier <- ~ ln_L_total_c + ln_K_camas_c +
      ln_K_tech_c + ln_L_total_c2 + ln_K_camas_c2 +
      trend + trend2 +
      d_cluster2 + d_cluster3 + d_cluster4 + d_cluster5
  }
  fml <- update(frontier, paste(formula_lhs, "~ ."))
  for (cfg in list(
    list(ud = "tnormal", mt = "bfgs", ht = 1L),
    list(ud = "tnormal", mt = "bhhh", ht = 1L),
    list(ud = "hnormal", mt = "bfgs", ht = 1L),
    list(ud = "hnormal", mt = "bhhh", ht = 2L)
  )) {
    m <- NULL
    capture.output({
      m <- tryCatch(
        suppressWarnings(sfaR::sfacross(
          fml, uhet = uhet, data = data, S = 1L,
          udist = cfg$ud, method = cfg$mt,
          hessianType = cfg$ht)),
        error = function(e) NULL)
    })
    if (is.null(m)) next
    ll   <- as.numeric(m$mlLoglik)
    cf   <- coef(m)
    se   <- tryCatch(sqrt(diag(vcov(m))),
                     error = function(e) rep(NA, length(cf)))
    idx_u <- grep("^Zu_", names(cf))
    if (!is.finite(ll) || abs(ll) > 1e10 ||
        any(abs(cf) > 100, na.rm = TRUE) ||
        any(!is.finite(se[idx_u]), na.rm = TRUE)) next
    message(sprintf("  OK [%s]: ll=%.2f [%s/%s]",
                    label, ll, cfg$ud, cfg$mt))
    return(m)
  }
  message(sprintf("  FALLÓ: %s", label))
  NULL
}

# Especificaciones base
ccaa_vars <- grep("^d_ccaa_[0-9]+$", names(df_sfa), value = TRUE)
uhet_base <- as.formula(paste(
  "~ D_desc + pct_sns + desc_pago +",
  "ShareQ + desc_shareQ +",
  paste(ccaa_vars, collapse = " + ")))

# Muestra base (agudos, sin COVID, con filtros mínimos)
df_base <- df_sfa %>%
  filter(es_agudo == 1, !anyo %in% 2020:2022,
         altTotal_bruto >= 200,
         is.finite(ln_altTotal_pond),
         !is.na(ln_L_total_c), !is.na(ln_K_camas_c),
         !is.na(D_desc), !is.na(pct_sns),
         !is.na(grupo_cluster), !is.na(ccaa_cod))

message("df_base: ", nrow(df_base), " obs | ",
        n_distinct(df_base$NCODI), " hosp")

resultados_rob <- list()

# ── Muestra Diseño A (reutilizada en múltiples robusteces) ──
df_A_base <- df_base %>%
  filter(is.finite(ln_i_diag),
         ln_i_diag > quantile(df_base$ln_i_diag,
                              0.01, na.rm = TRUE))

# ════════════════════════════════════════════════════════
# ROB-1 — Intensidad alternativa: i_simple
# ════════════════════════════════════════════════════════

message("\n═══ ROB-1: Intensidad alternativa i_simple ═══")

df_A_isimple <- df_base %>%
  filter(is.finite(ln_altTotal_pond),
         is.finite(ln_i_simple),
         !is.na(ln_i_simple))

message("Muestra i_simple: ", nrow(df_A_isimple),
        " obs | ", n_distinct(df_A_isimple$NCODI), " hosp")

cat("\nComparación i_diag vs i_simple:\n")
cat(sprintf("  i_diag:   media=%.3f sd=%.3f NAs=%d%%\n",
  mean(df_sfa$i_diag,   na.rm = TRUE),
  sd(df_sfa$i_diag,     na.rm = TRUE),
  round(100 * mean(is.na(df_sfa$ln_i_diag)))))
cat(sprintf("  i_simple: media=%.3f sd=%.3f NAs=%d%%\n",
  mean(df_sfa$i_simple, na.rm = TRUE),
  sd(df_sfa$i_simple,   na.rm = TRUE),
  round(100 * mean(is.na(df_sfa$ln_i_simple)))))
cat(sprintf("  Correlación i_diag ~ i_simple: %.3f\n",
  tryCatch(cor(df_A_isimple$i_diag, df_A_isimple$i_simple,
               use = "pairwise.complete.obs"),
           error = function(e) NA)))

m_Tot_is <- estimar_rob("ln_altTotal_pond",
             uhet_base, df_A_isimple, "Tot_isimple")
m_I_is   <- estimar_rob("ln_i_simple",
             uhet_base, df_A_isimple, "I_isimple")

resultados_rob[["R1_Tot_isimple"]] <- m_Tot_is
resultados_rob[["R1_I_isimple"]]   <- m_I_is

if (!is.null(m_Tot_is))
  saveRDS(m_Tot_is, file.path(INT_DIR, "sfa_rob_Tot_isimple.rds"))
if (!is.null(m_I_is))
  saveRDS(m_I_is,   file.path(INT_DIR, "sfa_rob_I_isimple.rds"))

if (!is.null(m_I_is)) {
  se_ref <- sqrt(diag(vcov(m_I_ref)))
  se_is  <- sqrt(diag(vcov(m_I_is)))
  cat("\n[ROB-1] D_desc en frontera intensidad:\n")
  cat(sprintf("  i_diag  (principal): coef=%7.3f t=%6.2f\n",
    coef(m_I_ref)["Zu_D_desc"],
    coef(m_I_ref)["Zu_D_desc"] / se_ref["Zu_D_desc"]))
  if ("Zu_D_desc" %in% names(coef(m_I_is)))
    cat(sprintf("  i_simple (rob):      coef=%7.3f t=%6.2f\n",
      coef(m_I_is)["Zu_D_desc"],
      coef(m_I_is)["Zu_D_desc"] / se_is["Zu_D_desc"]))
}

# ════════════════════════════════════════════════════════
# ROB-2 — Fuente de D_desc: SIAE vs CNH
# ════════════════════════════════════════════════════════

message("\n═══ ROB-2: D_desc_siae vs D_desc_cnh ═══")

q01 <- quantile(df_base$ln_i_diag, 0.01, na.rm = TRUE)

df_A_siae <- df_base %>%
  mutate(D_desc    = D_desc_siae,
         desc_pago   = D_desc_siae * pct_sns,
         desc_shareQ = D_desc_siae * ShareQ) %>%
  filter(!is.na(D_desc), is.finite(ln_i_diag),
         ln_i_diag > q01)

df_A_cnh  <- df_base %>%
  mutate(D_desc    = D_desc_cnh,
         desc_pago   = D_desc_cnh * pct_sns,
         desc_shareQ = D_desc_cnh * ShareQ) %>%
  filter(!is.na(D_desc), is.finite(ln_i_diag),
         ln_i_diag > q01)

message("Muestra D_desc_siae: ", nrow(df_A_siae), " obs")
message("Muestra D_desc_cnh:  ", nrow(df_A_cnh),  " obs")

m_Tot_siae <- estimar_rob("ln_altTotal_pond",
               uhet_base, df_A_siae, "Tot_Dsiae")
m_I_siae   <- estimar_rob("ln_i_diag",
               uhet_base, df_A_siae, "I_Dsiae")
m_Tot_cnh  <- estimar_rob("ln_altTotal_pond",
               uhet_base, df_A_cnh,  "Tot_Dcnh")
m_I_cnh    <- estimar_rob("ln_i_diag",
               uhet_base, df_A_cnh,  "I_Dcnh")

for (nm in c("m_Tot_siae", "m_I_siae",
             "m_Tot_cnh",  "m_I_cnh")) {
  m <- get(nm)
  resultados_rob[[paste0("R2_", nm)]] <- m
  if (!is.null(m))
    saveRDS(m, file.path(INT_DIR,
            paste0("sfa_rob_", nm, ".rds")))
}

cat("\n[ROB-2] D_desc en cantidad — comparación de fuentes:\n")
cat(sprintf("  %-28s %8s %6s\n", "Especificación", "D_desc", "t"))
for (info in list(
  list(m = m_Tot_ref,  lab = "Principal (coalesce)"),
  list(m = m_Tot_siae, lab = "D_desc_siae"),
  list(m = m_Tot_cnh,  lab = "D_desc_cnh")
)) {
  if (is.null(info$m)) { cat(sprintf("  %-28s %8s\n", info$lab, "n/d")); next }
  cf <- coef(info$m)
  se <- tryCatch(sqrt(diag(vcov(info$m))),
                 error = function(e) rep(NA, length(cf)))
  if (!"Zu_D_desc" %in% names(cf)) next
  cat(sprintf("  %-28s %8.3f %6.2f\n",
    info$lab, cf["Zu_D_desc"],
    cf["Zu_D_desc"] / se["Zu_D_desc"]))
}

# ════════════════════════════════════════════════════════
# ROB-3 — Distribución half-normal
# ════════════════════════════════════════════════════════

message("\n═══ ROB-3: Distribución hnormal ═══")

frontier_base_fml <- ~ ln_L_total_c + ln_K_camas_c +
  ln_K_tech_c + ln_L_total_c2 + ln_K_camas_c2 +
  trend + trend2 +
  d_cluster2 + d_cluster3 + d_cluster4 + d_cluster5

for (info in list(
  list(lhs = "ln_altTotal_pond",
       ref = m_Tot_ref, label = "Tot_hnormal"),
  list(lhs = "ln_i_diag",
       ref = m_I_ref,   label = "I_hnormal")
)) {
  fml_hn <- update(frontier_base_fml,
                   paste(info$lhs, "~ ."))
  m_hn <- NULL
  capture.output({
    m_hn <- tryCatch(
      suppressWarnings(sfaR::sfacross(
        fml_hn, uhet = uhet_base, data = df_A_base,
        S = 1L, udist = "hnormal",
        method = "bfgs", hessianType = 1L)),
      error = function(e) NULL)
  })
  if (is.null(m_hn)) {
    capture.output({
      m_hn <- tryCatch(
        suppressWarnings(sfaR::sfacross(
          fml_hn, uhet = uhet_base, data = df_A_base,
          S = 1L, udist = "hnormal",
          method = "bhhh", hessianType = 2L)),
        error = function(e) NULL)
    })
  }

  if (!is.null(m_hn)) {
    resultados_rob[[paste0("R3_", info$label)]] <- m_hn
    saveRDS(m_hn, file.path(INT_DIR,
            paste0("sfa_rob_", info$label, ".rds")))

    cf_hn <- coef(m_hn)
    se_hn <- tryCatch(sqrt(diag(vcov(m_hn))),
                      error = function(e) rep(NA, length(cf_hn)))
    cf_tn <- coef(info$ref)
    se_tn <- sqrt(diag(vcov(info$ref)))
    ll_hn <- round(as.numeric(m_hn$mlLoglik), 2)
    message(sprintf("  OK [%s]: ll=%.2f", info$label, ll_hn))

    if ("Zu_D_desc" %in% names(cf_hn)) {
      message(sprintf(
        "  [%s] D_desc: tnormal=%.3f(t=%.2f) hnormal=%.3f(t=%.2f)",
        info$label,
        cf_tn["Zu_D_desc"],
        cf_tn["Zu_D_desc"] / se_tn["Zu_D_desc"],
        cf_hn["Zu_D_desc"],
        cf_hn["Zu_D_desc"] / se_hn["Zu_D_desc"]))
    }
  } else {
    message(sprintf("  FALLÓ: %s", info$label))
  }
}

# ════════════════════════════════════════════════════════
# ROB-4 — Muestra temporal: solo 2010-2019
# ════════════════════════════════════════════════════════

message("\n═══ ROB-4: Muestra 2010-2019 (sin 2023) ═══")

df_pre2020 <- df_base %>%
  filter(anyo <= 2019,
         is.finite(ln_i_diag),
         ln_i_diag > q01)

message("Muestra 2010-2019: ", nrow(df_pre2020), " obs | ",
        n_distinct(df_pre2020$NCODI), " hosp")

m_Tot_pre <- estimar_rob("ln_altTotal_pond",
              uhet_base, df_pre2020, "Tot_2010_2019")
m_I_pre   <- estimar_rob("ln_i_diag",
              uhet_base, df_pre2020, "I_2010_2019")

resultados_rob[["R4_Tot_pre"]] <- m_Tot_pre
resultados_rob[["R4_I_pre"]]   <- m_I_pre

if (!is.null(m_Tot_pre))
  saveRDS(m_Tot_pre, file.path(INT_DIR,
          "sfa_rob_Tot_2010_2019.rds"))
if (!is.null(m_I_pre))
  saveRDS(m_I_pre, file.path(INT_DIR,
          "sfa_rob_I_2010_2019.rds"))

# ════════════════════════════════════════════════════════
# ROB-5 — Excluir discordancias ONG (D_desc_siae != D_desc_cnh)
# ════════════════════════════════════════════════════════

message("\n═══ ROB-5: Excluir ONG (discordancias SIAE/CNH) ═══")

df_sin_ong <- df_base %>%
  filter(is.finite(ln_i_diag),
         ln_i_diag > q01,
         D_desc_cnh == D_desc_siae | is.na(D_desc_cnh))

message("Muestra sin discordancias ONG: ",
        nrow(df_sin_ong), " obs | ",
        n_distinct(df_sin_ong$NCODI), " hosp")
message("% descentralizados: ",
        round(100 * mean(df_sin_ong$D_desc, na.rm = TRUE), 1))

m_Tot_song <- estimar_rob("ln_altTotal_pond",
               uhet_base, df_sin_ong, "Tot_sinONG")
m_I_song   <- estimar_rob("ln_i_diag",
               uhet_base, df_sin_ong, "I_sinONG")

resultados_rob[["R5_Tot_sinONG"]] <- m_Tot_song
resultados_rob[["R5_I_sinONG"]]   <- m_I_song

if (!is.null(m_Tot_song))
  saveRDS(m_Tot_song, file.path(INT_DIR,
          "sfa_rob_Tot_sinONG.rds"))
if (!is.null(m_I_song))
  saveRDS(m_I_song, file.path(INT_DIR,
          "sfa_rob_I_sinONG.rds"))

# ════════════════════════════════════════════════════════
# TABLA RESUMEN DE ROBUSTEZ
# ════════════════════════════════════════════════════════

message("\n╔══════════════════════════════════════════════════════╗")
message("║  TABLA RESUMEN — D_desc en cantidad e intensidad    ║")
message("╚══════════════════════════════════════════════════════╝")
message(sprintf("  %-32s %8s %6s %8s %6s",
  "Especificación", "α_Tot", "t_Tot", "δ_I", "t_I"))
message(paste(rep("─", 68), collapse = ""))

specs <- list(
  list(mT = m_Tot_ref,                        mI = m_I_ref,
       lab = "Principal (i_diag, D_desc)"),
  list(mT = m_Tot_is,                         mI = m_I_is,
       lab = "R1: i_simple"),
  list(mT = m_Tot_siae,                       mI = m_I_siae,
       lab = "R2: D_desc_siae"),
  list(mT = m_Tot_cnh,                        mI = m_I_cnh,
       lab = "R2: D_desc_cnh"),
  list(mT = resultados_rob$R3_Tot_hnormal,    mI = resultados_rob$R3_I_hnormal,
       lab = "R3: hnormal"),
  list(mT = m_Tot_pre,                        mI = m_I_pre,
       lab = "R4: 2010-2019"),
  list(mT = m_Tot_song,                       mI = m_I_song,
       lab = "R5: sin ONG")
)

for (s in specs) {
  if (is.null(s$mT) || is.null(s$mI)) {
    message(sprintf("  %-32s %8s", s$lab, "n/d")); next
  }
  cfT <- coef(s$mT)
  seT <- tryCatch(sqrt(diag(vcov(s$mT))),
                  error = function(e) rep(NA, length(cfT)))
  cfI <- coef(s$mI)
  seI <- tryCatch(sqrt(diag(vcov(s$mI))),
                  error = function(e) rep(NA, length(cfI)))
  if (!"Zu_D_desc" %in% names(cfT)) {
    message(sprintf("  %-32s %8s", s$lab, "(sin Zu_D_desc)")); next
  }
  message(sprintf("  %-32s %8.3f %6.2f %8.3f %6.2f",
    s$lab,
    cfT["Zu_D_desc"], cfT["Zu_D_desc"] / seT["Zu_D_desc"],
    cfI["Zu_D_desc"], cfI["Zu_D_desc"] / seI["Zu_D_desc"]))
}

message("\nInterpretación tabla:")
message("  α_Tot < 0: D_desc REDUCE ineficiencia en cantidad (P1 ✓)")
message("  δ_I   > 0: D_desc AUMENTA ineficiencia en intensidad (P2 ✓)")
message("  Signo estable entre specs: evidencia de robustez")

message("\n=== Script 12 completado ===")
