# ============================================================
# 11_estimar_sfa.R — Estimación SFA definitiva
# Especificación: agudos, sin COVID, cluster en frontera,
# grupos pago + CCAA en ineficiencia
# B_boot <- 50 prueba; cambiar a 4999 para versión final
# ============================================================

# ── Carga ────────────────────────────────────────────────────
config_path <- if (file.exists("scripts/00_config.R"))
  "scripts/00_config.R" else "00_config.R"
source(config_path)
if (!requireNamespace("sfaR", quietly = TRUE))
  stop("Instala sfaR: install.packages('sfaR')")
library(sfaR); library(dplyr)

B_boot <- 50
set.seed(42)

load(file.path(INT_DIR, "df_sfa.RData"))
message("df_sfa: ", nrow(df_sfa), " x ", ncol(df_sfa))

# ── Filtros comunes ───────────────────────────────────────────
df_base <- df_sfa %>%
  filter(
    es_agudo == 1,
    !anyo %in% 2020:2022,
    altTotal_bruto > 100,
    !is.na(D_desc), !is.na(pct_sns), !is.na(ShareQ),
    !is.na(grupo_pago),
    !is.na(ln_L_total_c), !is.na(ln_K_camas_c),
    !is.na(ccaa_cod)
  )
message("df_base (agudos, sin COVID): ", nrow(df_base),
        " obs | ", n_distinct(df_base$NCODI), " hosp")
message("D_desc=0: ", sum(df_base$D_desc == 0),
        " | D_desc=1: ", sum(df_base$D_desc == 1))
message("Grupos: ",
        paste(names(table(df_base$grupo_pago)),
              table(df_base$grupo_pago), sep = "=",
              collapse = " | "))

# Muestra Diseño B y D
df_est_B <- df_base %>%
  filter(altQ_bruto >= 200, altM_bruto >= 200,
         is.finite(ln_altQ_pond), is.finite(ln_altM_pond))
message("df_est_B (>=200 altas Q y M): ", nrow(df_est_B),
        " obs | ", n_distinct(df_est_B$NCODI), " hosp")

# Muestra Diseño A
df_est_A <- df_base %>%
  filter(altTotal_bruto >= 200,
         is.finite(ln_altTotal_pond),
         is.finite(ln_i_diag),
         ln_i_diag > quantile(ln_i_diag, 0.01, na.rm = TRUE))
message("df_est_A (i_diag válido): ", nrow(df_est_A),
        " obs | ", n_distinct(df_est_A$NCODI), " hosp")

# Muestra Diseño D (igual que B)
df_est_D <- df_est_B %>%
  mutate(
    neg_ln_altQ = -ln_altQ_pond,
    ln_ratio_MQ = ln_altM_pond - ln_altQ_pond
  )

# ── Especificaciones ──────────────────────────────────────────

# Detectar dummies CCAA disponibles en df_base
ccaa_vars <- grep("^d_ccaa_[0-9]+$", names(df_base), value = TRUE)
message("Dummies CCAA: ", length(ccaa_vars))

# Frontera translog con cluster
frontier_str <- paste(
  "~ ln_L_total_c + ln_K_camas_c + ln_K_tech_c +",
  "ln_L_total_c2 + ln_K_camas_c2 + trend + trend2 +",
  "d_cluster2 + d_cluster3 + d_cluster4 + d_cluster5"
)

# Ineficiencia completa: grupos pago + CCAA
uhet_str_full <- paste(
  "~ d_Priv_Conc + d_Priv_Merc +",
  "ShareQ + Conc_shareQ + Merc_shareQ +",
  paste(ccaa_vars, collapse = " + ")
)

# Ineficiencia sin CCAA (fallback)
uhet_str_nocc <- "~ d_Priv_Conc + d_Priv_Merc +
                   ShareQ + Conc_shareQ + Merc_shareQ"

# Ineficiencia mínima (fallback último recurso)
uhet_str_min <- "~ d_Priv_Conc + d_Priv_Merc + ShareQ"

uhet_full <- as.formula(uhet_str_full)
uhet_nocc <- as.formula(uhet_str_nocc)
uhet_min  <- as.formula(uhet_str_min)

# ── Función de estimación con fallback ────────────────────────
err_log <- character(0)

es_degenerado <- function(m) {
  if (is.null(m)) return(TRUE)
  ll <- tryCatch(as.numeric(m$mlLoglik), error = function(e) NA)
  if (!is.finite(ll) || abs(ll) > 1e10) return(TRUE)
  cf <- tryCatch(coef(m), error = function(e) NULL)
  if (is.null(cf) || any(!is.finite(cf))) return(TRUE)
  if (any(abs(cf) > 100, na.rm = TRUE)) return(TRUE)
  se <- tryCatch(sqrt(diag(vcov(m))), error = function(e) NULL)
  if (is.null(se)) return(TRUE)
  idx <- grep("^Zu_", names(cf))
  if (any(!is.finite(se[idx]), na.rm = TRUE)) return(TRUE)
  FALSE
}

estimar_sfa <- function(lhs, uhet, data, label) {
  frontier <- as.formula(paste(lhs, frontier_str))
  configs <- list(
    list(ud = "tnormal", mt = "bfgs", ht = 1L),
    list(ud = "tnormal", mt = "bhhh", ht = 1L),
    list(ud = "tnormal", mt = "nm",   ht = 2L),
    list(ud = "hnormal", mt = "bfgs", ht = 1L),
    list(ud = "hnormal", mt = "bhhh", ht = 2L)
  )
  for (uh in list(uhet, uhet_nocc, uhet_min)) {
    for (cfg in configs) {
      tag <- sprintf("[%s] %s/%s uhet=%s",
                     label, cfg$ud, cfg$mt,
                     deparse(uh[[2]])[1])
      message("  Prob: ", tag)
      m <- NULL
      capture.output({
        m <- tryCatch(
          suppressWarnings(sfaR::sfacross(
            formula = frontier, uhet = uh, data = data,
            S = 1L, udist = cfg$ud, method = cfg$mt,
            hessianType = cfg$ht)),
          error = function(e) {
            err_log <<- c(err_log, paste(tag, e$message))
            NULL
          })
      })
      if (!is.null(m) && !es_degenerado(m)) {
        message(sprintf("  OK [%s]: ll=%.2f udist=%s uhet=%s",
          label, as.numeric(m$mlLoglik),
          cfg$ud, deparse(uh[[2]])[1]))
        return(m)
      }
    }
  }
  err_log <<- c(err_log, paste("FALLÓ:", label))
  message("  *** FALLÓ: ", label)
  NULL
}

# ── Función de extracción de coeficientes ────────────────────
extraer_tab <- function(m, label) {
  if (is.null(m)) return(NULL)
  cf <- coef(m)
  se <- tryCatch(sqrt(diag(vcov(m))),
                 error = function(e) rep(NA, length(cf)))
  idx <- grep("^Zu_", names(cf))
  data.frame(
    param   = sub("^Zu_", "", names(cf)[idx]),
    coef    = round(cf[idx], 4),
    se      = round(se[idx], 4),
    t_stat  = round(cf[idx] / se[idx], 3),
    modelo  = label,
    row.names = NULL,
    stringsAsFactors = FALSE
  )
}

mostrar_contrastes <- function(m1, m2, l1, l2, titulo) {
  t1 <- extraer_tab(m1, l1)
  t2 <- extraer_tab(m2, l2)
  if (is.null(t1) || is.null(t2)) {
    message(titulo, ": tabla no disponible"); return(NULL)
  }
  tab <- merge(t1[, c("param", "coef", "se", "t_stat")],
               t2[, c("param", "coef", "se", "t_stat")],
               by = "param", suffixes = c("_1", "_2"))
  tab$dif    <- round(tab$coef_1 - tab$coef_2, 4)
  tab$se_dif <- round(sqrt(tab$se_1^2 + tab$se_2^2), 4)
  tab$z      <- round(tab$dif / tab$se_dif, 3)
  tab$p      <- round(2 * (1 - pnorm(abs(tab$z))), 4)
  names(tab)[2:4] <- c(l1, paste0("se_", l1), paste0("t_", l1))
  names(tab)[5:7] <- c(l2, paste0("se_", l2), paste0("t_", l2))
  message("\n════════ ", titulo, " ════════")
  tab_show <- tab[!grepl("^d_ccaa", tab$param), ]
  print(tab_show)
  for (par in c("d_Priv_Conc", "d_Priv_Merc", "ShareQ")) {
    r <- tab[tab$param == par, ]
    if (nrow(r) > 0) {
      message(sprintf("  %s: %s=%.3f(t=%.2f) %s=%.3f(t=%.2f) z=%.3f p=%.4f",
        par, l1, r[[l1]], r[[paste0("t_", l1)]],
             l2, r[[l2]], r[[paste0("t_", l2)]],
             r$z, r$p))
    }
  }
  invisible(tab)
}

# ════════════════════════════════════════════════════════
# DISEÑO D — Función de distancia output (ODF)
# ════════════════════════════════════════════════════════
message("\n═══ DISEÑO D — ODF ═══")

frontier_D <- as.formula(paste(
  "neg_ln_altQ ~",
  "ln_ratio_MQ + I(ln_ratio_MQ^2) +",
  "I(ln_ratio_MQ*ln_L_total_c) +",
  "I(ln_ratio_MQ*ln_K_camas_c) +",
  sub("^~", "", frontier_str)
))

modelo_D <- tryCatch({
  m <- estimar_sfa("neg_ln_altQ", uhet_full, df_est_D, "D")
  if (!is.null(m)) {
    saveRDS(m, file.path(INT_DIR, "sfa_modeloD_definitivo.rds"))
    message("Modelo D guardado.")
    te <- tryCatch(sfaR::efficiencies(m)$teJLMS,
                   error = function(e) NULL)
    if (!is.null(te))
      message(sprintf("  TE_D: media=%.3f sd=%.3f",
                      mean(te, na.rm = TRUE), sd(te, na.rm = TRUE)))
    tab_d <- extraer_tab(m, "D")
    print(tab_d[!grepl("d_ccaa", tab_d$param), ])
  }
  m
}, error = function(e) { message("D FALLÓ: ", e$message); NULL })

# ════════════════════════════════════════════════════════
# DISEÑO B — Dos fronteras quirúrgico/médico
# ════════════════════════════════════════════════════════
message("\n═══ DISEÑO B — Quirúrgico vs Médico ═══")

modelo_Q <- tryCatch({
  m <- estimar_sfa("ln_altQ_pond", uhet_full, df_est_B, "Q")
  if (!is.null(m))
    saveRDS(m, file.path(INT_DIR, "sfa_modeloQ_definitivo.rds"))
  m
}, error = function(e) { message("Q FALLÓ: ", e$message); NULL })

if (!is.null(modelo_Q)) {
  te_Q <- tryCatch(sfaR::efficiencies(modelo_Q)$teJLMS, error = function(e) NULL)
  if (!is.null(te_Q)) {
    df_est_B$TE_Q <- te_Q
    message(sprintf("  TE_Q: media=%.3f sd=%.3f", mean(te_Q, na.rm=TRUE), sd(te_Q, na.rm=TRUE)))
  }
}

modelo_M <- tryCatch({
  m <- estimar_sfa("ln_altM_pond", uhet_full, df_est_B, "M")
  if (!is.null(m))
    saveRDS(m, file.path(INT_DIR, "sfa_modeloM_definitivo.rds"))
  m
}, error = function(e) { message("M FALLÓ: ", e$message); NULL })

if (!is.null(modelo_M)) {
  te_M <- tryCatch(sfaR::efficiencies(modelo_M)$teJLMS, error = function(e) NULL)
  if (!is.null(te_M)) {
    df_est_B$TE_M <- te_M
    message(sprintf("  TE_M: media=%.3f sd=%.3f", mean(te_M, na.rm=TRUE), sd(te_M, na.rm=TRUE)))
  }
}

tab_B <- mostrar_contrastes(modelo_Q, modelo_M,
  "Q_quir", "M_med", "DISEÑO B: Q vs M")

# ════════════════════════════════════════════════════════
# DISEÑO A — Dos fronteras cantidad/intensidad
# ════════════════════════════════════════════════════════
message("\n═══ DISEÑO A — Cantidad vs Intensidad ═══")

modelo_Tot <- tryCatch({
  m <- estimar_sfa("ln_altTotal_pond", uhet_full, df_est_A, "Total")
  if (!is.null(m))
    saveRDS(m, file.path(INT_DIR, "sfa_modeloTotal_definitivo.rds"))
  m
}, error = function(e) { message("Tot FALLÓ: ", e$message); NULL })

if (!is.null(modelo_Tot)) {
  te_Tot <- tryCatch(sfaR::efficiencies(modelo_Tot)$teJLMS, error = function(e) NULL)
  if (!is.null(te_Tot)) {
    df_est_A$TE_Total <- te_Tot
    message(sprintf("  TE_Tot: media=%.3f sd=%.3f", mean(te_Tot, na.rm=TRUE), sd(te_Tot, na.rm=TRUE)))
  }
}

modelo_I <- tryCatch({
  m <- estimar_sfa("ln_i_diag", uhet_full, df_est_A, "I")
  if (!is.null(m))
    saveRDS(m, file.path(INT_DIR, "sfa_modeloI_definitivo.rds"))
  m
}, error = function(e) { message("I FALLÓ: ", e$message); NULL })

if (!is.null(modelo_I)) {
  te_I <- tryCatch(sfaR::efficiencies(modelo_I)$teJLMS, error = function(e) NULL)
  if (!is.null(te_I)) {
    df_est_A$TE_I <- te_I
    message(sprintf("  TE_I: media=%.3f sd=%.3f", mean(te_I, na.rm=TRUE), sd(te_I, na.rm=TRUE)))
  }
}

tab_A <- mostrar_contrastes(modelo_Tot, modelo_I,
  "Tot_cant", "I_intens", "DISEÑO A: Cantidad vs Intensidad")

# ════════════════════════════════════════════════════════
# DISEÑO C — Panel hospital × servicio × año
# ════════════════════════════════════════════════════════
message("\n═══ DISEÑO C — Panel servicio × hosp × año ═══")

modelo_C <- tryCatch({
  df_Q <- df_est_B %>%
    mutate(servicio = "Q", C_s = 1L,
           ln_output = ln_altQ_pond,
           ln_L_s    = ln_L_quirur,
           ln_K_s    = log(pmax(camas_cirugia, 0, na.rm = TRUE) + 1)) %>%
    filter(is.finite(ln_output), !is.na(ln_L_s))

  df_M <- df_est_B %>%
    mutate(servicio = "M", C_s = 0L,
           ln_output = ln_altM_pond,
           ln_L_s    = ln_L_medico,
           ln_K_s    = log(pmax(camas_medicina, 0, na.rm = TRUE) + 1)) %>%
    filter(is.finite(ln_output), !is.na(ln_L_s))

  df_panel <- bind_rows(df_Q, df_M) %>%
    mutate(
      ln_L_s_c  = ln_L_s - mean(ln_L_s, na.rm = TRUE),
      ln_L_s_c2 = 0.5 * ln_L_s_c^2,
      Conc_Cs   = d_Priv_Conc * C_s,
      Merc_Cs   = d_Priv_Merc * C_s,
      ShareQ_Cs = ShareQ * C_s
    ) %>%
    filter(!is.na(ln_L_s_c))

  message("Panel C: ", nrow(df_panel), " obs (",
          n_distinct(df_panel$NCODI), " hosp × 2 servicios)")

  frontier_C <- as.formula(paste(
    "ln_output ~ ln_L_s_c + ln_K_camas_c + ln_K_tech_c +",
    "ln_L_s_c2 + ln_K_camas_c2 +",
    "I(ln_L_s_c*ln_K_camas_c) +",
    "C_s + d_cluster2 + d_cluster3 +",
    "d_cluster4 + d_cluster5 + trend + trend2"
  ))

  uhet_C <- as.formula(paste(
    "~ d_Priv_Conc + d_Priv_Merc + C_s +",
    "Conc_Cs + Merc_Cs + ShareQ + ShareQ_Cs +",
    paste(ccaa_vars, collapse = " + ")
  ))

  m <- NULL
  for (uh in list(uhet_C,
    as.formula("~ d_Priv_Conc+d_Priv_Merc+C_s+Conc_Cs+Merc_Cs+ShareQ+ShareQ_Cs"))) {
    for (cfg in list(
      list(ud = "tnormal", mt = "bfgs", ht = 1L),
      list(ud = "tnormal", mt = "bhhh", ht = 1L),
      list(ud = "hnormal", mt = "bfgs", ht = 1L),
      list(ud = "hnormal", mt = "bhhh", ht = 2L))) {
      message(sprintf("  [C] %s/%s", cfg$ud, cfg$mt))
      m_c <- NULL
      capture.output({
        m_c <- tryCatch(
          suppressWarnings(sfaR::sfacross(
            formula = frontier_C, uhet = uh,
            data = df_panel, S = 1L,
            udist = cfg$ud, method = cfg$mt,
            hessianType = cfg$ht)),
          error = function(e) NULL)
      })
      if (!is.null(m_c) && !es_degenerado(m_c)) {
        m <- m_c
        message(sprintf("  OK [C]: ll=%.2f [%s]",
          as.numeric(m$mlLoglik), cfg$ud))
        break
      }
    }
    if (!is.null(m)) break
  }

  if (!is.null(m)) {
    saveRDS(m, file.path(INT_DIR, "sfa_modeloC_definitivo.rds"))
    message("Modelo C guardado.")
    tab_c <- extraer_tab(m, "C")
    tab_c_show <- tab_c[!grepl("d_ccaa", tab_c$param), ]
    message("\nCoeficientes ineficiencia Diseño C:")
    print(tab_c_show)
    for (par in c("C_s", "Conc_Cs", "Merc_Cs", "ShareQ_Cs")) {
      r <- tab_c[tab_c$param == par, ]
      if (nrow(r) > 0)
        message(sprintf("  %s: coef=%.3f t=%.2f",
          par, r$coef, r$t_stat))
    }
  }
  m
}, error = function(e) { message("C FALLÓ: ", e$message); NULL })

# ════════════════════════════════════════════════════════
# DISEÑO A VARIANTE — D_desc binario + cluster
# ════════════════════════════════════════════════════════
message("\n═══ DISEÑO A VARIANTE — D_desc binario + cluster ═══")

uhet_Ddesc <- as.formula(paste(
  "~ D_desc + pct_sns + desc_pago +",
  "ShareQ + desc_shareQ +",
  paste(ccaa_vars, collapse = " + ")
))

df_est_Av <- df_est_A %>% filter(!is.na(ccaa_cod))
message("Muestra Diseño A variante: ", nrow(df_est_Av),
        " obs | ", n_distinct(df_est_Av$NCODI), " hosp")

modelo_Tot_v <- tryCatch({
  m <- estimar_sfa("ln_altTotal_pond", uhet_Ddesc, df_est_Av, "Tot_v")
  if (!is.null(m)) {
    saveRDS(m, file.path(INT_DIR, "sfa_mTot_Ddesc_definitivo.rds"))
    message("Modelo Tot_v guardado.")
  }
  m
}, error = function(e) { message("Tot_v FALLÓ: ", e$message); NULL })

if (!is.null(modelo_Tot_v)) {
  te_Tv <- tryCatch(sfaR::efficiencies(modelo_Tot_v)$teJLMS, error = function(e) NULL)
  if (!is.null(te_Tv))
    message(sprintf("  TE_Tot_v: media=%.3f sd=%.3f", mean(te_Tv, na.rm=TRUE), sd(te_Tv, na.rm=TRUE)))
}

modelo_I_v <- tryCatch({
  m <- estimar_sfa("ln_i_diag", uhet_Ddesc, df_est_Av, "I_v")
  if (!is.null(m)) {
    saveRDS(m, file.path(INT_DIR, "sfa_mI_Ddesc_definitivo.rds"))
    message("Modelo I_v guardado.")
  }
  m
}, error = function(e) { message("I_v FALLÓ: ", e$message); NULL })

if (!is.null(modelo_I_v)) {
  te_Iv <- tryCatch(sfaR::efficiencies(modelo_I_v)$teJLMS, error = function(e) NULL)
  if (!is.null(te_Iv))
    message(sprintf("  TE_I_v: media=%.3f sd=%.3f", mean(te_Iv, na.rm=TRUE), sd(te_Iv, na.rm=TRUE)))
}

if (!is.null(modelo_Tot_v) && !is.null(modelo_I_v)) {
  cf_T <- coef(modelo_Tot_v)
  se_T <- tryCatch(sqrt(diag(vcov(modelo_Tot_v))), error = function(e) rep(NA, length(cf_T)))
  cf_I <- coef(modelo_I_v)
  se_I <- tryCatch(sqrt(diag(vcov(modelo_I_v))),   error = function(e) rep(NA, length(cf_I)))

  pars_show <- c("D_desc", "pct_sns", "desc_pago", "ShareQ", "desc_shareQ")
  message("\n════════ DISEÑO A VARIANTE — D_desc + cluster ════════")
  message(sprintf("%-15s %8s %6s %8s %6s %7s %6s",
    "Parámetro", "α_Tot", "t_Tot", "δ_I", "t_I", "z", "p"))
  for (p in pars_show) {
    nm_p <- paste0("Zu_", p)
    if (!nm_p %in% names(cf_T)) next
    dif  <- cf_T[nm_p] - cf_I[nm_p]
    se_d <- sqrt(se_T[nm_p]^2 + se_I[nm_p]^2)
    z    <- dif / se_d
    pv   <- 2 * (1 - pnorm(abs(z)))
    pred <- if (p == "D_desc")
      ifelse(cf_T[nm_p] < 0 & cf_I[nm_p] > 0, "✓ P1+P2",
      ifelse(cf_T[nm_p] < 0, "✓ P1 solo", "✗"))
    else ""
    message(sprintf("%-15s %8.3f %6.2f %8.3f %6.2f %7.3f %6.4f %s",
      p, cf_T[nm_p], cf_T[nm_p]/se_T[nm_p],
         cf_I[nm_p], cf_I[nm_p]/se_I[nm_p],
         z, pv, pred))
  }
}

# ════════════════════════════════════════════════════════
# DISEÑO D ENRIQUECIDO — ODF + interacción D_desc × ratio
# ════════════════════════════════════════════════════════
message("\n═══ DISEÑO D ENRIQUECIDO — ODF + bias D_desc ═══")

frontier_D_enr <- as.formula(paste(
  "neg_ln_altQ ~",
  "ln_ratio_MQ + I(ln_ratio_MQ^2) +",
  "I(ln_ratio_MQ * ln_L_total_c) +",
  "I(ln_ratio_MQ * ln_K_camas_c) +",
  "I(ln_ratio_MQ * D_desc) +",
  "I(ln_ratio_MQ * pct_sns) +",
  sub("^~", "", frontier_str)
))

modelo_D_enr <- tryCatch({
  m <- NULL
  for (cfg in list(
    list(ud = "tnormal", mt = "bfgs", ht = 1L),
    list(ud = "tnormal", mt = "bhhh", ht = 1L),
    list(ud = "hnormal", mt = "bfgs", ht = 1L)
  )) {
    message(sprintf("  [D_enr] %s/%s", cfg$ud, cfg$mt))
    m_try <- NULL
    capture.output({
      m_try <- tryCatch(
        suppressWarnings(sfaR::sfacross(
          frontier_D_enr, uhet = uhet_full,
          data = df_est_D, S = 1L,
          udist = cfg$ud, method = cfg$mt,
          hessianType = cfg$ht)),
        error = function(e) { message("  Error: ", e$message); NULL })
    })
    if (is.null(m_try) || es_degenerado(m_try)) {
      message("  Degenerado o error"); next
    }
    m <- m_try
    message(sprintf("  OK [D_enr]: ll=%.2f [%s/%s]",
                    as.numeric(m$mlLoglik), cfg$ud, cfg$mt))
    break
  }

  if (!is.null(m)) {
    saveRDS(m, file.path(INT_DIR, "sfa_modeloD_enriquecido.rds"))
    message("Modelo D enriquecido guardado.")

    cf2 <- coef(m)
    se2 <- tryCatch(sqrt(diag(vcov(m))), error = function(e) rep(NA, length(cf2)))
    idx_int <- grep("ln_ratio_MQ.*D_desc|ln_ratio_MQ.*pct_sns", names(cf2))
    if (length(idx_int) > 0) {
      message("\nInteracciones ratio × estructura/pago:")
      for (i in idx_int)
        message(sprintf("  %-35s coef=%7.3f t=%6.2f",
          names(cf2)[i], cf2[i], cf2[i]/se2[i]))
      message("\nInterpretación:")
      message("  coef(ratio×D_desc) < 0: descentralización sesga hacia CANTIDAD (confirma P1)")
      message("  coef(ratio×D_desc) > 0: sesga hacia INTENSIDAD")
    }

    ll_orig <- tryCatch(as.numeric(modelo_D$mlLoglik), error = function(e) NA)
    ll_enr  <- as.numeric(m$mlLoglik)
    lr_stat <- 2 * (ll_enr - ll_orig)
    message(sprintf("\nComparación ODF: ll_orig=%.2f  ll_enr=%.2f", ll_orig, ll_enr))
    message(sprintf("LR test (2×dif): %.2f (2 grados libertad)", lr_stat))
    message(sprintf("p-valor LR: %.4f", 1 - pchisq(lr_stat, df = 2)))
  }
  m
}, error = function(e) { message("D_enr FALLÓ: ", e$message); NULL })

# ════════════════════════════════════════════════════════
# BOOTSTRAP WALD DISEÑO B (P*)
# ════════════════════════════════════════════════════════
message("\n═══ Bootstrap Wald Diseño B ═══")

p_wald_B    <- NA_real_
wald_boot_B <- rep(NA_real_, B_boot)

if (!is.null(modelo_Q) && !is.null(modelo_M)) {
  cf_Q  <- coef(modelo_Q); cf_M <- coef(modelo_M)
  idx_Q <- grep("^Zu_d_Priv|^Zu_ShareQ|^Zu_Conc|^Zu_Merc",
                names(cf_Q))
  idx_M <- grep("^Zu_d_Priv|^Zu_ShareQ|^Zu_Conc|^Zu_Merc",
                names(cf_M))

  if (length(idx_Q) == length(idx_M) && length(idx_Q) > 0) {
    diff_obs <- cf_Q[idx_Q] - cf_M[idx_M]
    wald_obs <- as.numeric(t(diff_obs) %*% diff_obs)

    udist_Q <- tryCatch(modelo_Q$udist, error = function(e) "tnormal")
    udist_M <- tryCatch(modelo_M$udist, error = function(e) "tnormal")

    message(sprintf("Bootstrap B=%d (sin dummies CCAA)...", B_boot))
    for (b in seq_len(B_boot)) {
      idx_b <- sample(nrow(df_est_B), replace = TRUE)
      db    <- df_est_B[idx_b, ]
      wald_boot_B[b] <- tryCatch({
        # Función auxiliar: intenta bfgs → bhhh para cada modelo bootstrap
        fit_boot <- function(lhs, ud) {
          m_b <- NULL
          for (mt in c("bfgs", "bhhh", "nm")) {
            capture.output({
              m_b <- tryCatch(suppressWarnings(sfaR::sfacross(
                as.formula(paste(lhs, frontier_str)),
                uhet = uhet_nocc, data = db, S = 1L,
                udist = ud, method = mt, hessianType = 1L)),
                error = function(e) NULL)
            })
            if (!is.null(m_b) && !es_degenerado(m_b)) break
            m_b <- NULL
          }
          m_b
        }
        mQ_b <- fit_boot("ln_altQ_pond", udist_Q)
        mM_b <- fit_boot("ln_altM_pond", udist_M)
        if (is.null(mQ_b) || is.null(mM_b)) {
          NA_real_
        } else {
          cQ_b <- coef(mQ_b); cM_b <- coef(mM_b)
          iQ <- grep("^Zu_d_Priv|^Zu_ShareQ|^Zu_Conc|^Zu_Merc",
                     names(cQ_b))
          iM <- grep("^Zu_d_Priv|^Zu_ShareQ|^Zu_Conc|^Zu_Merc",
                     names(cM_b))
          if (length(iQ) != length(iM)) {
            NA_real_
          } else {
            d_b <- cQ_b[iQ] - cM_b[iM]
            as.numeric(t(d_b) %*% d_b)
          }
        }
      }, error = function(e) NA_real_)
      if (b %% 10 == 0)
        message(sprintf("  Boot %d/%d — válidas=%d",
                        b, B_boot, sum(!is.na(wald_boot_B[1:b]))))
    }

    n_ok     <- sum(!is.na(wald_boot_B))
    p_wald_B <- mean(wald_boot_B >= wald_obs, na.rm = TRUE)
    message(sprintf(
      "Wald bootstrap: stat=%.4f p=%.4f (B_ok=%d/%d)",
      wald_obs, p_wald_B, n_ok, B_boot))
    saveRDS(wald_boot_B,
            file.path(INT_DIR, "sfa_wald_boot_definitivo.rds"))
  }
}

# ════════════════════════════════════════════════════════
# EFICIENCIAS Y RESUMEN FINAL
# ════════════════════════════════════════════════════════
message("\n═══ RESUMEN FINAL ═══")

modelos <- list(
  "D — ODF"          = modelo_D,
  "B — Quirúrgico Q" = modelo_Q,
  "B — Médico M"     = modelo_M,
  "A — Cantidad Tot" = modelo_Tot,
  "A — Intensidad I" = modelo_I,
  "C — Panel serv."  = modelo_C
)

for (nm in names(modelos)) {
  m <- modelos[[nm]]
  if (is.null(m)) {
    message(sprintf("  %-25s FALLÓ", nm))
  } else {
    ll   <- tryCatch(as.numeric(m$mlLoglik), error = function(e) NA)
    te   <- tryCatch(sfaR::efficiencies(m)$teJLMS,
                     error = function(e) NULL)
    med  <- if (!is.null(te)) mean(te, na.rm = TRUE) else NA
    dist <- tryCatch(m$udist, error = function(e) "?")
    message(sprintf("  %-25s OK | ll=%10.2f | TE=%.3f [%s]",
      nm, ll, med, dist))
  }
}

# ── Verificación predicciones P1-P4 ──────────────────────────
message("\n═══ VERIFICACIÓN PREDICCIONES P1-P4 ═══")
message("(+) = confirma predicción del modelo teórico")
message("(-) = no confirma")

verificar_pred <- function(tab, nombre_diseno) {
  if (is.null(tab)) {
    message(nombre_diseno, ": no disponible"); return()
  }
  message("\n", nombre_diseno, ":")
  col1 <- names(tab)[2]
  col2 <- names(tab)[5]

  r <- tab[tab$param == "d_Priv_Conc", ]
  if (nrow(r) > 0) {
    message(sprintf("  P1 Priv_Conc en %s: %.3f(t=%.2f) %s",
      col1, r[[col1]], r[[paste0("t_", col1)]],
      ifelse(r[[col1]] < 0, "(+)", "(-)")))
    message(sprintf("  P2 Priv_Conc en %s: %.3f(t=%.2f) %s",
      col2, r[[col2]], r[[paste0("t_", col2)]],
      ifelse(r[[col2]] > 0, "(+)", "(-)")))
  }
  s <- tab[tab$param == "ShareQ", ]
  if (nrow(s) > 0) {
    message(sprintf("  P4 ShareQ: %s=%.3f %s=%.3f %s",
      col1, s[[col1]], col2, s[[col2]],
      ifelse(sign(s[[col1]]) != sign(s[[col2]]),
             "(+) signos opuestos", "(-) mismo signo")))
  }
}

verificar_pred(tab_B, "Diseño B (Q vs M)")
verificar_pred(tab_A, "Diseño A (Cantidad vs Intensidad)")

# ── Guardar log errores ───────────────────────────────────────
if (length(err_log) > 0) {
  writeLines(c(format(Sys.time()), "", err_log),
             file.path(INT_DIR, "sfa_errores_definitivo.txt"))
  message("\n", length(err_log),
          " errores — ver sfa_errores_definitivo.txt")
} else {
  message("\nSin errores de convergencia.")
}

# ════════════════════════════════════════════════════════
# BLOQUE FINAL — SALIDA COMPLETA DE TODOS LOS MODELOS
# ════════════════════════════════════════════════════════
rds_exists <- function(f) file.exists(file.path(INT_DIR, f))

modelos_todos <- list(
  "D_ODF"       = if (rds_exists("sfa_modeloD_definitivo.rds"))
                    readRDS(file.path(INT_DIR, "sfa_modeloD_definitivo.rds")) else NULL,
  "D_ODF_enr"   = if (rds_exists("sfa_modeloD_enriquecido.rds"))
                    readRDS(file.path(INT_DIR, "sfa_modeloD_enriquecido.rds")) else NULL,
  "B_Q"         = if (rds_exists("sfa_modeloQ_definitivo.rds"))
                    readRDS(file.path(INT_DIR, "sfa_modeloQ_definitivo.rds")) else NULL,
  "B_M"         = if (rds_exists("sfa_modeloM_definitivo.rds"))
                    readRDS(file.path(INT_DIR, "sfa_modeloM_definitivo.rds")) else NULL,
  "A_Tot"       = if (rds_exists("sfa_modeloTotal_definitivo.rds"))
                    readRDS(file.path(INT_DIR, "sfa_modeloTotal_definitivo.rds")) else NULL,
  "A_I"         = if (rds_exists("sfa_modeloI_definitivo.rds"))
                    readRDS(file.path(INT_DIR, "sfa_modeloI_definitivo.rds")) else NULL,
  "A_Tot_Ddesc" = if (rds_exists("sfa_mTot_Ddesc_definitivo.rds"))
                    readRDS(file.path(INT_DIR, "sfa_mTot_Ddesc_definitivo.rds")) else NULL,
  "A_I_Ddesc"   = if (rds_exists("sfa_mI_Ddesc_definitivo.rds"))
                    readRDS(file.path(INT_DIR, "sfa_mI_Ddesc_definitivo.rds")) else NULL,
  "C_panel"     = if (rds_exists("sfa_modeloC_definitivo.rds"))
                    readRDS(file.path(INT_DIR, "sfa_modeloC_definitivo.rds")) else NULL
)

# ── SECCIÓN 1: Resumen convergencia y TE ─────────────────────
cat("\n")
cat("╔══════════════════════════════════════════════════════╗\n")
cat("║  RESUMEN GENERAL — TODOS LOS MODELOS                ║\n")
cat("╚══════════════════════════════════════════════════════╝\n")
cat(sprintf("%-20s %10s %8s %8s %s\n", "Modelo","ll","TE_media","TE_sd","dist"))
cat(paste(rep("─", 60), collapse=""), "\n")
for (nm in names(modelos_todos)) {
  m <- modelos_todos[[nm]]
  if (is.null(m)) { cat(sprintf("%-20s %10s\n", nm, "no disponible")); next }
  ll   <- round(as.numeric(m$mlLoglik), 2)
  dist <- tryCatch(m$udist, error = function(e) "?")
  te   <- tryCatch(sfaR::efficiencies(m)$teJLMS, error = function(e) NULL)
  med  <- if (!is.null(te)) round(mean(te, na.rm=TRUE), 3) else NA
  sdte <- if (!is.null(te)) round(sd(te,   na.rm=TRUE), 3) else NA
  cat(sprintf("%-20s %10.2f %8.3f %8.3f [%s]\n", nm, ll, med, sdte, dist))
}

# ── SECCIÓN 2: Frontera de producción ────────────────────────
cat("\n")
cat("╔══════════════════════════════════════════════════════╗\n")
cat("║  FRONTERA DE PRODUCCIÓN — coeficientes principales  ║\n")
cat("╚══════════════════════════════════════════════════════╝\n")
for (nm in names(modelos_todos)) {
  m <- modelos_todos[[nm]]
  if (is.null(m)) next
  cf <- coef(m)
  se <- tryCatch(sqrt(diag(vcov(m))), error = function(e) rep(NA, length(cf)))
  idx <- grep("^ln_|^trend|^d_cluster|^C_s|^ln_ratio", names(cf))
  if (length(idx) == 0) next
  cat(sprintf("\n[%s]\n", nm))
  cat(sprintf("  %-30s %8s %6s\n", "Parámetro","coef","t"))
  for (i in idx)
    cat(sprintf("  %-30s %8.4f %6.2f\n", names(cf)[i], cf[i], cf[i]/se[i]))
}

# ── SECCIÓN 3: Ecuación de ineficiencia ──────────────────────
cat("\n")
cat("╔══════════════════════════════════════════════════════╗\n")
cat("║  ECUACIÓN DE INEFICIENCIA — determinantes           ║\n")
cat("║  (excluidas dummies CCAA por brevedad)              ║\n")
cat("╚══════════════════════════════════════════════════════╝\n")
for (nm in names(modelos_todos)) {
  m <- modelos_todos[[nm]]
  if (is.null(m)) next
  cf <- coef(m)
  se <- tryCatch(sqrt(diag(vcov(m))), error = function(e) rep(NA, length(cf)))
  idx <- grep("^Zu_", names(cf))
  idx <- idx[!grepl("ccaa", names(cf)[idx])]
  if (length(idx) == 0) next
  cat(sprintf("\n[%s]\n", nm))
  cat(sprintf("  %-25s %8s %6s %8s\n", "Parámetro","coef","t","p"))
  for (i in idx) {
    t_val <- cf[i] / se[i]
    p_val <- 2 * (1 - pnorm(abs(t_val)))
    sig <- if (is.na(p_val)) "" else
           if (p_val < 0.001) "***" else
           if (p_val < 0.01)  "**"  else
           if (p_val < 0.05)  "*"   else
           if (p_val < 0.10)  "."   else ""
    cat(sprintf("  %-25s %8.4f %6.2f %8.4f %s\n",
      sub("^Zu_", "", names(cf)[i]), cf[i], t_val, p_val, sig))
  }
}

# ── SECCIÓN 4: Contrastes P1-P4 por pares ────────────────────
cat("\n")
cat("╔══════════════════════════════════════════════════════╗\n")
cat("║  CONTRASTES P1-P4 — COMPARACIÓN DE PARES            ║\n")
cat("╚══════════════════════════════════════════════════════╝\n")

contrastar_par <- function(m1, m2, nm1, nm2, pars_interes, titulo) {
  if (is.null(m1) || is.null(m2)) {
    cat(titulo, ": no disponible\n"); return(invisible(NULL))
  }
  cf1 <- coef(m1); se1 <- tryCatch(sqrt(diag(vcov(m1))), error=function(e) rep(NA,length(cf1)))
  cf2 <- coef(m2); se2 <- tryCatch(sqrt(diag(vcov(m2))), error=function(e) rep(NA,length(cf2)))
  cat(sprintf("\n%s\n", titulo))
  cat(sprintf("  %-20s %8s %6s %8s %6s %7s %6s\n",
    "Parámetro", nm1,"t1", nm2,"t2","z","p"))
  cat(paste(rep("─", 72), collapse=""), "\n")
  for (p in pars_interes) {
    nm_p <- paste0("Zu_", p)
    if (!nm_p %in% names(cf1) || !nm_p %in% names(cf2)) next
    dif  <- cf1[nm_p] - cf2[nm_p]
    se_d <- sqrt(se1[nm_p]^2 + se2[nm_p]^2)
    z    <- dif / se_d
    pv   <- 2 * (1 - pnorm(abs(z)))
    sig  <- if (is.na(pv)) "" else
            if (pv < 0.001) "***" else
            if (pv < 0.01)  "**"  else
            if (pv < 0.05)  "*"   else
            if (pv < 0.10)  "."   else ""
    cat(sprintf("  %-20s %8.3f %6.2f %8.3f %6.2f %7.3f %6.4f %s\n",
      p,
      cf1[nm_p], cf1[nm_p]/se1[nm_p],
      cf2[nm_p], cf2[nm_p]/se2[nm_p],
      z, pv, sig))
  }
}

contrastar_par(modelos_todos$A_Tot, modelos_todos$A_I,
  "α_Total", "δ_Intens",
  c("d_Priv_Conc","d_Priv_Merc","ShareQ","Conc_shareQ","Merc_shareQ"),
  "DISEÑO A (grupos pago): Cantidad vs Intensidad")

contrastar_par(modelos_todos$A_Tot_Ddesc, modelos_todos$A_I_Ddesc,
  "α_Total", "δ_Intens",
  c("D_desc","pct_sns","desc_pago","ShareQ","desc_shareQ"),
  "DISEÑO A (D_desc): Cantidad vs Intensidad")

contrastar_par(modelos_todos$B_Q, modelos_todos$B_M,
  "α_Q", "δ_M",
  c("d_Priv_Conc","d_Priv_Merc","ShareQ","Conc_shareQ","Merc_shareQ"),
  "DISEÑO B: Quirúrgico vs Médico")

# ── SECCIÓN 5: Eficiencias por grupo organizativo ────────────
cat("\n")
cat("╔══════════════════════════════════════════════════════╗\n")
cat("║  EFICIENCIAS TÉCNICAS POR GRUPO                     ║\n")
cat("╚══════════════════════════════════════════════════════╝\n")

load(file.path(INT_DIR, "df_sfa.RData"))

for (nm in c("B_Q","B_M","A_Tot","A_I","A_Tot_Ddesc","A_I_Ddesc")) {
  m <- modelos_todos[[nm]]
  if (is.null(m)) next
  te <- tryCatch(sfaR::efficiencies(m)$teJLMS, error = function(e) NULL)
  if (is.null(te)) next

  is_A <- grepl("^A", nm)
  df_te <- if (is_A) {
    df_sfa %>% filter(
      es_agudo == 1, !anyo %in% 2020:2022,
      altTotal_bruto >= 200,
      is.finite(ln_altTotal_pond), is.finite(ln_i_diag),
      !is.na(ln_L_total_c), !is.na(ln_K_camas_c),
      !is.na(D_desc), !is.na(pct_sns), !is.na(ShareQ),
      !is.na(grupo_pago), !is.na(ccaa_cod))
  } else {
    df_sfa %>% filter(
      es_agudo == 1, !anyo %in% 2020:2022,
      altQ_bruto >= 200, altM_bruto >= 200,
      is.finite(ln_altQ_pond), is.finite(ln_altM_pond),
      !is.na(ln_L_total_c), !is.na(ln_K_camas_c),
      !is.na(D_desc), !is.na(pct_sns), !is.na(ShareQ),
      !is.na(grupo_pago), !is.na(ccaa_cod))
  }

  if (length(te) != nrow(df_te)) {
    cat(sprintf("\n[%s] — longitud TE (%d) ≠ muestra (%d), omitido\n",
        nm, length(te), nrow(df_te)))
    next
  }
  df_te$TE <- te

  cat(sprintf("\n[%s]\n", nm))
  res <- df_te %>%
    group_by(D_desc, grupo_cluster) %>%
    summarise(n = n(),
              TE_media = round(mean(TE, na.rm=TRUE), 3),
              TE_sd    = round(sd(TE,   na.rm=TRUE), 3),
              .groups = "drop") %>%
    arrange(D_desc, grupo_cluster)
  print(as.data.frame(res))
}

# ── SECCIÓN 6: Tabla resumen predicciones P1-P4 ──────────────
cat("\n")
cat("╔══════════════════════════════════════════════════════╗\n")
cat("║  TABLA RESUMEN — PREDICCIONES DEL MODELO TEÓRICO   ║\n")
cat("╚══════════════════════════════════════════════════════╝\n")
cat("\nReferencia: Pub_Retro = hospitales públicos SNS\n")
cat("(+) = confirma predicción  (-) = no confirma\n\n")
cat(sprintf("%-8s %-30s %-16s %-16s %-8s\n",
    "Pred.","Descripción","Diseño A (D_desc)","Diseño B","Veredicto"))
cat(paste(rep("─", 80), collapse=""), "\n")

get_coef_m <- function(m, param) {
  if (is.null(m)) return(NA_real_)
  cf <- coef(m); nm_p <- paste0("Zu_", param)
  if (!nm_p %in% names(cf)) return(NA_real_)
  cf[nm_p]
}

a_p1 <- get_coef_m(modelos_todos$A_Tot_Ddesc, "D_desc")
b_p1_conc <- get_coef_m(modelos_todos$B_Q, "d_Priv_Conc")
b_p1_merc <- get_coef_m(modelos_todos$B_Q, "d_Priv_Merc")
cat(sprintf("%-8s %-30s %-16s %-16s %-8s\n", "P1",
  "Desc ↓ inef. cantidad",
  ifelse(!is.na(a_p1), sprintf("%.3f %s", a_p1, ifelse(a_p1 < 0,"(+)","(-)")), "n/d"),
  ifelse(!is.na(b_p1_conc), sprintf("Conc:%.2f Merc:%.2f", b_p1_conc, b_p1_merc), "n/d"),
  ifelse(!is.na(a_p1) && a_p1 < 0, "✓", "✗")))

a_p2 <- get_coef_m(modelos_todos$A_I_Ddesc, "D_desc")
b_p2_conc <- get_coef_m(modelos_todos$B_M, "d_Priv_Conc")
cat(sprintf("%-8s %-30s %-16s %-16s %-8s\n", "P2",
  "Desc ↑ inef. intensidad",
  ifelse(!is.na(a_p2), sprintf("%.3f %s", a_p2, ifelse(a_p2 > 0,"(+)","(-)")), "n/d"),
  ifelse(!is.na(b_p2_conc), sprintf("Conc:%.2f", b_p2_conc), "n/d"),
  ifelse(!is.na(a_p2) && a_p2 > 0, "✓", "✗")))

a_p3_t <- get_coef_m(modelos_todos$A_Tot_Ddesc, "pct_sns")
a_p3_i <- get_coef_m(modelos_todos$A_I_Ddesc,   "pct_sns")
b_p3_q <- get_coef_m(modelos_todos$B_Q, "d_Priv_Merc")
b_p3_m <- get_coef_m(modelos_todos$B_M, "d_Priv_Merc")
cat(sprintf("%-8s %-30s %-16s %-16s %-8s\n", "P3",
  "Pago difiere entre dims.",
  ifelse(!is.na(a_p3_t) && !is.na(a_p3_i),
    sprintf("T:%.2f I:%.2f", a_p3_t, a_p3_i), "n/d"),
  ifelse(!is.na(b_p3_q) && !is.na(b_p3_m),
    sprintf("Q:%.2f M:%.2f", b_p3_q, b_p3_m), "n/d"),
  ifelse(!is.na(a_p3_t) && !is.na(a_p3_i) &&
         sign(a_p3_t) != sign(a_p3_i), "✓", "✗")))

a_p4_t <- get_coef_m(modelos_todos$A_Tot_Ddesc, "ShareQ")
a_p4_i <- get_coef_m(modelos_todos$A_I_Ddesc,   "ShareQ")
b_p4_q <- get_coef_m(modelos_todos$B_Q, "ShareQ")
b_p4_m <- get_coef_m(modelos_todos$B_M, "ShareQ")
cat(sprintf("%-8s %-30s %-16s %-16s %-8s\n", "P4",
  "ψ₁₂ modera asimétr.",
  ifelse(!is.na(a_p4_t) && !is.na(a_p4_i),
    sprintf("T:%.2f I:%.2f %s", a_p4_t, a_p4_i,
      ifelse(sign(a_p4_t) != sign(a_p4_i),"(+)","(-)")), "n/d"),
  ifelse(!is.na(b_p4_q) && !is.na(b_p4_m),
    sprintf("Q:%.2f M:%.2f %s", b_p4_q, b_p4_m,
      ifelse(sign(b_p4_q) != sign(b_p4_m),"(+)","(-)")), "n/d"),
  ifelse((!is.na(a_p4_t) && !is.na(a_p4_i) && sign(a_p4_t) != sign(a_p4_i)) ||
         (!is.na(b_p4_q) && !is.na(b_p4_m) && sign(b_p4_q) != sign(b_p4_m)), "✓", "✗")))

cat("\n=== Salida completa finalizada ===\n")

message("\n=== Script 11 completado ===")
