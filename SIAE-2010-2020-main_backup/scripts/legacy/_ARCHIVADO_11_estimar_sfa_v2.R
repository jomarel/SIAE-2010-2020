# ============================================================
# 11_estimar_sfa.R
# Estimación de fronteras estocásticas (SFA) — 4 diseños.
#
# DISEÑO D — Función de distancia output multiproducto
# DISEÑO B — Sistema dos fronteras quirúrgico/médico
# DISEÑO A — Sistema dos fronteras cantidad/intensidad
# DISEÑO C — Panel hospital × servicio × año (reshape)
#
# Contrastes: tablas de coeficientes uhet + Wald bootstrap
#
# NOTA: B_boot <- 50 para ejecución de prueba.
# Si todos los modelos convergen, relanzar con B_boot <- 4999.
#
# Output: data_intermediate/sfa_*.rds, sfa_*.csv
# ============================================================

config_path <- if (file.exists("scripts/00_config.R")) "scripts/00_config.R" else "00_config.R"
source(config_path)

if (!requireNamespace("sfaR", quietly = TRUE))
  stop("Instala sfaR con install.packages('sfaR')")

required_pkgs <- c("sfaR", "dplyr")
missing_pkgs  <- required_pkgs[!vapply(required_pkgs, requireNamespace,
                                       logical(1), quietly = TRUE)]
if (length(missing_pkgs) > 0)
  stop("Faltan paquetes: ", paste(missing_pkgs, collapse = ", "), call. = FALSE)

library(sfaR)
library(dplyr)

PROTECTED_VARS <- c(paste0("u", 1:104), "u900", "NCODI", "anyo")

# ============================================================
# Parámetro bootstrap — cambiar a 4999 para versión definitiva
# ============================================================
B_boot <- 50

message("\n=== Script 11: Estimación SFA (B_boot = ", B_boot, ") ===")
message("    Para versión definitiva: cambiar B_boot <- 4999")

# ============================================================
# Carga de datos
# ============================================================

df_sfa_path <- file.path(INT_DIR, "df_sfa.RData")
if (!file.exists(df_sfa_path))
  stop("No encontrado df_sfa. Ejecuta script 09 antes.", call. = FALSE)

load(df_sfa_path)
message("df_sfa cargado: ", nrow(df_sfa), " x ", ncol(df_sfa))

# Log de errores de convergencia
err_log <- character(0)
log_error <- function(modelo_label, metodo, mensaje) {
  entry <- sprintf("[%s] Error con %s: %s", modelo_label, metodo, mensaje)
  err_log <<- c(err_log, entry)
}

# ============================================================
# MUESTRA DE ESTIMACIÓN COMÚN
# ============================================================

message("\n--- Muestra de estimación ---")

df_est <- df_sfa %>%
  filter(
    !is.na(ln_altQ_pond)    & is.finite(ln_altQ_pond),
    !is.na(ln_altM_pond)    & is.finite(ln_altM_pond),
    !is.na(ln_altTotal_pond) & is.finite(ln_altTotal_pond),
    !is.na(ln_L_total_c),
    !is.na(ln_K_camas_c),
    !is.na(D_desc),
    !is.na(pct_sns),
    altTotal_bruto > 100
  )

message("Muestra estimación: ", nrow(df_est), " obs | ",
        n_distinct(df_est$NCODI), " hospitales")
message("D_desc=0: ", sum(df_est$D_desc == 0),
        " | D_desc=1: ", sum(df_est$D_desc == 1))

# Submuestra con i_diag válido y sin outliers p1-p99 (Diseño A)
df_est_i <- df_est %>%
  filter(!is.na(ln_i_diag) & is.finite(ln_i_diag) &
           !i_diag_outlier)

message("Muestra con i_diag válido: ", nrow(df_est_i),
        " obs | ", n_distinct(df_est_i$NCODI), " hospitales")

# Diagnóstico rápido de rango de variables clave
message("\nRangos de outputs (log):")
for (v in c("ln_altQ_pond","ln_altM_pond","ln_altTotal_pond","ln_i_diag")) {
  if (v %in% names(df_est)) {
    x <- df_est[[v]]
    message(sprintf("  %-22s  min=%.2f  med=%.2f  max=%.2f  NAs=%d",
                    v, min(x,na.rm=TRUE), median(x,na.rm=TRUE),
                    max(x,na.rm=TRUE), sum(is.na(x))))
  }
}
message("Rangos de inputs centrados (deben estar ~0):")
for (v in c("ln_L_total_c","ln_K_camas_c","ln_K_tech_c")) {
  if (v %in% names(df_est)) {
    x <- df_est[[v]]
    message(sprintf("  %-22s  min=%.2f  med=%.2f  max=%.2f",
                    v, min(x,na.rm=TRUE), median(x,na.rm=TRUE),
                    max(x,na.rm=TRUE)))
  }
}

# ============================================================
# ESPECIFICACIONES COMUNES
# ============================================================

# Frontera translog REDUCIDA — se eliminó ln_LK_c y las interacciones con
# ln_K_tech_c por multicolinealidad severa (r > 0.90 con los cuadráticos)
frontier_rhs <- ~ ln_L_total_c + ln_K_camas_c + ln_K_tech_c +
  ln_L_total_c2 + ln_K_camas_c2 +
  trend + trend2

# Ecuación de ineficiencia estándar
uhet_formula <- ~ D_desc + pct_sns + desc_pago +
                  ShareQ + desc_shareQ

# Ecuación de ineficiencia reducida (sin interacciones — fallback)
uhet_simple <- ~ D_desc + pct_sns + ShareQ

# ── Detección de solución degenerada ──
# Criterio: log-lik no finito o cualquier coeficiente > 100 en valor absoluto
# (escala log; exp(100) ~ 2.7e43 es claramente fuera de rango)
es_degenerado <- function(m) {
  if (is.null(m)) return(TRUE)
  ll <- tryCatch(as.numeric(m$mlLoglik), error = function(e) NA_real_)
  # ll debe ser negativo; positivo o |ll|>1e6 indica degeneración
  if (!is.finite(ll) || ll > 0 || abs(ll) > 1e6) {
    message(sprintf("    [Degenerado] log-lik = %g", ll))
    return(TRUE)
  }
  cf <- tryCatch(coef(m), error = function(e) NULL)
  if (is.null(cf)) return(TRUE)
  if (any(!is.finite(cf))) {
    message("    [Degenerado] coeficientes no finitos")
    return(TRUE)
  }
  if (any(abs(cf) > 100, na.rm = TRUE)) {
    extremos <- cf[abs(cf) > 100]
    message(sprintf("    [Degenerado] coef extremo: %s = %g",
                    names(extremos)[1], extremos[1]))
    return(TRUE)
  }
  FALSE
}

# ── Imprimir resumen básico del modelo ──
imprimir_resumen <- function(m, label) {
  ll <- tryCatch(as.numeric(m$mlLoglik), error = function(e) NA_real_)
  cf <- tryCatch(coef(m), error = function(e) NULL)
  message(sprintf("  [%s] log-lik = %g | npar = %d",
                  label, ll, if (!is.null(cf)) length(cf) else NA_integer_))
  # Intentar summary completo; puede fallar si hay valores extremos
  tryCatch(
    print(summary(m)),
    error = function(e)
      message("  (resumen completo no disponible: ", e$message, ")")
  )
}

# ── Función auxiliar de estimación con fallback de método, distribución y forma funcional ──
#
# Secuencia de 7 intentos (los pasos 4 y 6-7 requieren formula_cd != NULL):
#   1. tnormal, bfgs,  hessianType=1L, translog + uhet completo
#   2. tnormal, bhhh,  hessianType=1L, translog + uhet completo
#   3. tnormal, nm,    hessianType=2L, translog + uhet completo
#   4. tnormal, bfgs,  hessianType=1L, Cobb-Douglas + uhet simple
#   5. hnormal, bfgs,  hessianType=1L, translog + uhet completo
#   6. hnormal, bfgs,  hessianType=1L, Cobb-Douglas + uhet simple
#   7. hnormal, nm,    hessianType=2L, Cobb-Douglas + uhet simple
# Si todos producen soluciones degeneradas → NULL
#
estimar_sfa <- function(formula, formula_cd = NULL, uhet, data, label) {

  configs <- list(
    list(form = formula,    uhet_f = uhet,        udist = "tnormal", method = "bfgs", hess = 1L, tag = "TL"),
    list(form = formula,    uhet_f = uhet,        udist = "tnormal", method = "bhhh", hess = 1L, tag = "TL"),
    list(form = formula,    uhet_f = uhet,        udist = "tnormal", method = "nm",   hess = 2L, tag = "TL"),
    list(form = formula_cd, uhet_f = uhet_simple, udist = "tnormal", method = "bfgs", hess = 1L, tag = "CD"),
    list(form = formula,    uhet_f = uhet,        udist = "hnormal", method = "bfgs", hess = 1L, tag = "TL"),
    list(form = formula_cd, uhet_f = uhet_simple, udist = "hnormal", method = "bfgs", hess = 1L, tag = "CD"),
    list(form = formula_cd, uhet_f = uhet_simple, udist = "hnormal", method = "nm",   hess = 2L, tag = "CD")
  )
  # Omitir intentos con CD si no se proporcionó formula_cd
  configs <- Filter(function(cfg) !is.null(cfg$form), configs)

  for (cfg in configs) {
    message(sprintf("  [%s] %-8s udist=%-8s method=%-4s hessianType=%dL  (n=%d)",
                    label, cfg$tag, cfg$udist, cfg$method, cfg$hess, nrow(data)))
    resultado <- NULL
    capture.output({
      resultado <- tryCatch(
        suppressWarnings(
          sfaR::sfacross(
            formula     = cfg$form,
            uhet        = cfg$uhet_f,
            data        = data,
            S           = 1L,
            udist       = cfg$udist,
            method      = cfg$method,
            hessianType = cfg$hess
          )
        ),
        error = function(e) {
          log_error(label, paste0(cfg$tag, "/", cfg$method), conditionMessage(e))
          message("    Error: ", conditionMessage(e))
          NULL
        }
      )
    })

    if (!is.null(resultado)) {
      if (!es_degenerado(resultado)) {
        message(sprintf("  Modelo %s OK: %s udist=%s method=%s  ll=%g",
                        label, cfg$tag, cfg$udist, cfg$method,
                        tryCatch(as.numeric(resultado$mlLoglik), error = function(e) NA)))
        resultado$.forma_funcional <- cfg$tag   # anotar en el objeto
        return(resultado)
      } else {
        log_error(label, paste0(cfg$tag, "/", cfg$method),
                  paste0("Solucion degenerada (udist=", cfg$udist, ")"))
      }
    }
  }

  msg <- paste("Modelo", label, "no convergio a solucion no degenerada en ningun intento.")
  err_log <<- c(err_log, msg)
  message("  *** ", msg)
  NULL   # Devuelve NULL; cada bloque exterior maneja NULL
}

# ── Extracción segura de eficiencias ──
get_te <- function(modelo) {
  if (is.null(modelo)) return(NULL)
  ef <- tryCatch(sfaR::efficiencies(modelo), error = function(e) NULL)
  if (is.null(ef)) {
    n <- tryCatch(nrow(modelo$dataTable), error = function(e) NULL)
    return(if (!is.null(n)) rep(NA_real_, n) else NULL)
  }
  if ("teJLMS" %in% names(ef)) return(ef$teJLMS)
  if (is.data.frame(ef) && ncol(ef) >= 1) return(ef[[1]])
  if (is.numeric(ef)) return(ef)
  NULL
}

# ============================================================
# DISEÑO D — Función de distancia output multiproducto
# ============================================================

message("\n═══════════════════════════════════════════")
message("DISEÑO D: Función de distancia output (ODF)")
message("═══════════════════════════════════════════")

# Añadir columnas ANTES del tryCatch para que persistan en el ámbito global
df_est <- df_est %>%
  mutate(
    neg_ln_altQ = -ln_altQ_pond,
    ln_ratio_MQ  = ln_altM_pond - ln_altQ_pond
  )

formula_D <- neg_ln_altQ ~
  ln_ratio_MQ +
  ln_L_total_c + ln_K_camas_c + ln_K_tech_c +
  ln_L_total_c2 + ln_K_camas_c2 +
  I(ln_ratio_MQ ^ 2) +
  I(ln_ratio_MQ * ln_L_total_c) +
  I(ln_ratio_MQ * ln_K_camas_c) +
  trend + trend2

formula_D_cd <- neg_ln_altQ ~
  ln_ratio_MQ +
  ln_L_total_c + ln_K_camas_c + ln_K_tech_c +
  trend

uhet_D <- ~ D_desc + pct_sns + desc_pago + ShareQ + desc_shareQ

te_d_tmp <- NULL
modelo_D <- tryCatch({
  m <- estimar_sfa(formula_D, formula_D_cd, uhet_D, df_est, "D")
  if (!is.null(m)) {
    saveRDS(m, file.path(INT_DIR, "sfa_modeloD_odf.rds"))
    message("Modelo D guardado.")
    imprimir_resumen(m, "D")
    te_d_tmp <<- get_te(m)   # <<- simple, sin indexación
  }
  m
}, error = function(e) {
  msg <- paste("DISEÑO D FALLÓ:", conditionMessage(e))
  err_log <<- c(err_log, msg)
  message("  *** ", msg)
  NULL
})
if (!is.null(te_d_tmp)) {
  df_est[["TE_D"]] <- te_d_tmp   # asignación indexada fuera del tryCatch
  message(sprintf("  TE_D: media=%.3f  sd=%.3f",
                  mean(te_d_tmp, na.rm = TRUE), sd(te_d_tmp, na.rm = TRUE)))
}

# ============================================================
# DISEÑO B — Sistema dos fronteras quirúrgico/médico
# ============================================================

message("\n═══════════════════════════════════════════")
message("DISEÑO B: Fronteras quirúrgico/médico")
message("═══════════════════════════════════════════")

formula_Q    <- update(frontier_rhs, ln_altQ_pond ~ .)
formula_M    <- update(frontier_rhs, ln_altM_pond ~ .)
formula_Q_cd <- ln_altQ_pond ~
  ln_L_total_c + ln_K_camas_c + ln_K_tech_c + trend
formula_M_cd <- ln_altM_pond ~
  ln_L_total_c + ln_K_camas_c + ln_K_tech_c + trend

te_q_tmp <- NULL
modelo_Q <- tryCatch({
  m <- estimar_sfa(formula_Q, formula_Q_cd, uhet_formula, df_est, "Q")
  if (!is.null(m)) {
    saveRDS(m, file.path(INT_DIR, "sfa_modeloQ_disenioB.rds"))
    imprimir_resumen(m, "Q")
    te_q_tmp <<- get_te(m)
  }
  m
}, error = function(e) {
  msg <- paste("DISEÑO B — modelo Q FALLÓ:", conditionMessage(e))
  err_log <<- c(err_log, msg)
  message("  *** ", msg)
  NULL
})
if (!is.null(te_q_tmp)) {
  df_est[["TE_Q"]] <- te_q_tmp
  message(sprintf("  TE_Q: media=%.3f", mean(te_q_tmp, na.rm = TRUE)))
}

te_m_tmp <- NULL
modelo_M <- tryCatch({
  m <- estimar_sfa(formula_M, formula_M_cd, uhet_formula, df_est, "M")
  if (!is.null(m)) {
    saveRDS(m, file.path(INT_DIR, "sfa_modeloM_disenioB.rds"))
    imprimir_resumen(m, "M")
    te_m_tmp <<- get_te(m)
  }
  m
}, error = function(e) {
  msg <- paste("DISEÑO B — modelo M FALLÓ:", conditionMessage(e))
  err_log <<- c(err_log, msg)
  message("  *** ", msg)
  NULL
})
if (!is.null(te_m_tmp)) {
  df_est[["TE_M"]] <- te_m_tmp
  message(sprintf("  TE_M: media=%.3f", mean(te_m_tmp, na.rm = TRUE)))
}

# ============================================================
# DISEÑO A — Sistema dos fronteras cantidad/intensidad
# ============================================================

message("\n═══════════════════════════════════════════")
message("DISEÑO A: Fronteras cantidad/intensidad")
message("═══════════════════════════════════════════")

formula_Total    <- update(frontier_rhs, ln_altTotal_pond ~ .)
formula_I        <- update(frontier_rhs, ln_i_diag ~ .)
formula_Total_cd <- ln_altTotal_pond ~
  ln_L_total_c + ln_K_camas_c + ln_K_tech_c + trend
formula_I_cd     <- ln_i_diag ~
  ln_L_total_c + ln_K_camas_c + ln_K_tech_c + trend

te_tot_tmp <- NULL
modelo_Total <- tryCatch({
  m <- estimar_sfa(formula_Total, formula_Total_cd, uhet_formula, df_est_i, "Total")
  if (!is.null(m)) {
    saveRDS(m, file.path(INT_DIR, "sfa_modeloTotal_disenioA.rds"))
    imprimir_resumen(m, "Total")
    te_tot_tmp <<- get_te(m)
  }
  m
}, error = function(e) {
  msg <- paste("DISEÑO A — modelo Total FALLÓ:", conditionMessage(e))
  err_log <<- c(err_log, msg)
  message("  *** ", msg)
  NULL
})
if (!is.null(te_tot_tmp)) {
  df_est_i[["TE_Total"]] <- te_tot_tmp
  message(sprintf("  TE_Total: media=%.3f", mean(te_tot_tmp, na.rm = TRUE)))
}

te_i_tmp <- NULL
modelo_I <- tryCatch({
  m <- estimar_sfa(formula_I, formula_I_cd, uhet_formula, df_est_i, "I")
  if (!is.null(m)) {
    saveRDS(m, file.path(INT_DIR, "sfa_modeloI_disenioA.rds"))
    imprimir_resumen(m, "I")
    te_i_tmp <<- get_te(m)
  }
  m
}, error = function(e) {
  msg <- paste("DISEÑO A — modelo I FALLÓ:", conditionMessage(e))
  err_log <<- c(err_log, msg)
  message("  *** ", msg)
  NULL
})
if (!is.null(te_i_tmp)) {
  df_est_i[["TE_I"]] <- te_i_tmp
  message(sprintf("  TE_I: media=%.3f", mean(te_i_tmp, na.rm = TRUE)))
}

# ============================================================
# DISEÑO C — Panel hospital × servicio × año
# ============================================================

message("\n═══════════════════════════════════════════")
message("DISEÑO C: Panel hospital × servicio × año")
message("═══════════════════════════════════════════")

df_efic_C <- data.frame()

# Diseño C requiere camas por servicio (cirugía/medicina) — verificar disponibilidad
vars_C_req  <- c("ln_L_quirur", "ln_L_medico", "camas_cirugia", "camas_medicina")
vars_C_miss <- vars_C_req[!vars_C_req %in% names(df_est)]

te_c_tmp <- NULL
modelo_C <- if (length(vars_C_miss) > 0) {
  msg_c <- paste0("DISEÑO C OMITIDO — variables no disponibles en df_sfa: ",
                  paste(vars_C_miss, collapse = ", "),
                  ". Requiere camas por especialidad (no en SIAE estándar).")
  err_log <- c(err_log, msg_c)
  message("  *** ", msg_c)
  NULL
} else {
  tryCatch({
    df_Q <- df_est %>%
      mutate(
        servicio  = "Q",
        C_s       = 1L,
        ln_output = ln_altQ_pond,
        ln_L_s    = ln_L_quirur,
        ln_K_s    = log(pmax(camas_cirugia, 0, na.rm = TRUE) + 1)
      ) %>%
      filter(!is.na(ln_output) & is.finite(ln_output) &
             !is.na(ln_L_s)   & is.finite(ln_L_s))

    df_M_c <- df_est %>%
      mutate(
        servicio  = "M",
        C_s       = 0L,
        ln_output = ln_altM_pond,
        ln_L_s    = ln_L_medico,
        ln_K_s    = log(pmax(camas_medicina, 0, na.rm = TRUE) + 1)
      ) %>%
      filter(!is.na(ln_output) & is.finite(ln_output) &
             !is.na(ln_L_s)   & is.finite(ln_L_s))

    df_panel <- bind_rows(df_Q, df_M_c) %>%
      mutate(
        ln_L_s_c     = ln_L_s - mean(ln_L_s, na.rm = TRUE),
        desc_Cs      = D_desc * C_s,
        pago_Cs      = pct_sns * C_s,
        desc_pago_Cs = D_desc * pct_sns * C_s
      ) %>%
      filter(!is.na(ln_L_s_c))

    message("Panel C: ", nrow(df_panel), " obs (",
            n_distinct(df_panel$NCODI), " hospitales x 2 servicios)")

    formula_C    <- ln_output ~
      ln_L_s_c + ln_K_camas_c + ln_K_tech_c +
      I(ln_L_s_c ^ 2) + ln_K_camas_c2 +
      I(ln_L_s_c * ln_K_camas_c) +
      C_s + trend + trend2
    formula_C_cd <- ln_output ~
      ln_L_s_c + ln_K_camas_c + ln_K_tech_c + C_s + trend
    uhet_C <- ~ D_desc + C_s + desc_Cs + pct_sns + pago_Cs + desc_pago_Cs

    m <- estimar_sfa(formula_C, formula_C_cd, uhet_C, df_panel, "C")
    if (!is.null(m)) {
      saveRDS(m, file.path(INT_DIR, "sfa_modeloC_panel.rds"))
      imprimir_resumen(m, "C")
      te_c_tmp <<- get_te(m)
      if (!is.null(te_c_tmp)) {
        df_panel$TE_C <- te_c_tmp
        df_efic_C <<- df_panel %>%
          dplyr::select(dplyr::any_of(
            c("NCODI", "anyo", "servicio", "C_s", "D_desc", "TE_C")))
      }
    }
    m
  }, error = function(e) {
    msg <- paste("DISEÑO C FALLÓ:", conditionMessage(e))
    err_log <<- c(err_log, msg)
    message("  *** ", msg)
    NULL
  })
}
if (!is.null(te_c_tmp))
  message(sprintf("  TE_C: media=%.3f", mean(te_c_tmp, na.rm = TRUE)))

# ============================================================
# CONTRASTES DE HIPÓTESIS — DISEÑOS A Y B
# ============================================================

message("\n═══════════════════════════════════════════")
message("Contrastes de hipótesis")
message("═══════════════════════════════════════════")

extraer_uhet <- function(modelo, prefix) {
  if (is.null(modelo)) return(data.frame(parametro=character(0), coef=numeric(0),
                                         se=numeric(0), modelo=character(0)))
  cf  <- coef(modelo)
  se  <- tryCatch(sqrt(diag(vcov(modelo))), error = function(e) rep(NA_real_, length(cf)))
  idx <- grep("^Zu_", names(cf))
  if (length(idx) == 0) {
    message("  AVISO: no se encontraron coeficientes Z_ en ", prefix)
    return(data.frame(parametro=character(0), coef=numeric(0),
                      se=numeric(0), modelo=character(0)))
  }
  data.frame(
    parametro = sub("^Zu_", "", names(cf)[idx]),
    coef      = cf[idx],
    se        = se[idx],
    modelo    = prefix,
    row.names = NULL,
    stringsAsFactors = FALSE
  )
}

# Diseño B
tabla_B <- NULL
if (!is.null(modelo_Q) && !is.null(modelo_M)) {
  tab_Q <- extraer_uhet(modelo_Q, "alpha_Q")
  tab_M <- extraer_uhet(modelo_M, "delta_M")
  if (nrow(tab_Q) > 0 && nrow(tab_M) > 0) {
    tabla_B <- merge(tab_Q, tab_M, by = "parametro",
                     suffixes = c("_Q", "_M")) %>%
      mutate(
        diferencia = coef_Q - coef_M,
        se_diff    = sqrt(se_Q ^ 2 + se_M ^ 2),
        z_stat     = diferencia / se_diff,
        p_valor    = 2 * (1 - pnorm(abs(z_stat)))
      )
    message("\nDiseño B — Tabla comparativa alpha (Q) vs delta (M):")
    print(tabla_B)
  }
} else {
  message("  Diseño B incompleto — tabla no disponible.")
}

# Diseño A
tabla_A <- NULL
if (!is.null(modelo_Total) && !is.null(modelo_I)) {
  tab_Tot <- extraer_uhet(modelo_Total, "alpha_Tot")
  tab_I   <- extraer_uhet(modelo_I,    "delta_I")
  if (nrow(tab_Tot) > 0 && nrow(tab_I) > 0) {
    tabla_A <- merge(tab_Tot, tab_I, by = "parametro",
                     suffixes = c("_Tot", "_I")) %>%
      mutate(
        diferencia = coef_Tot - coef_I,
        se_diff    = sqrt(se_Tot ^ 2 + se_I ^ 2),
        z_stat     = diferencia / se_diff,
        p_valor    = 2 * (1 - pnorm(abs(z_stat)))
      )
    message("\nDiseño A — Tabla comparativa alpha (Total) vs delta (I):")
    print(tabla_A)
  }
} else {
  message("  Diseño A incompleto — tabla no disponible.")
}

# ── Test Wald bootstrap Diseño B ──
p_wald_B    <- NA_real_
n_boot_ok   <- 0L
wald_obs_B  <- NA_real_
wald_boot_B <- rep(NA_real_, B_boot)

if (!is.null(modelo_Q) && !is.null(modelo_M)) {
  # Bootstrap paramétrico no factible (modelo Q requiere NM con >2000 iteraciones;
  # las réplicas bootstrap no convergen en tiempo razonable).
  # Se usa aproximación asintótica chi-cuadrado(k) donde k = nº parámetros uhet.
  # NOTA: SE del modelo Q son Inf (Nelder-Mead no produce Hessiana),
  # por lo que el Wald asintótico estándar tampoco está disponible para Q.
  # Se reporta el estadístico ||alpha_Q - delta_M||² como indicador descriptivo.

  idx_uhet_Q <- grep("^Zu_", names(coef(modelo_Q)))
  idx_uhet_M <- grep("^Zu_", names(coef(modelo_M)))

  if (length(idx_uhet_Q) == length(idx_uhet_M) && length(idx_uhet_Q) > 0) {
    diff_obs_B <- coef(modelo_Q)[idx_uhet_Q] - coef(modelo_M)[idx_uhet_M]
    wald_obs_B <- as.numeric(t(diff_obs_B) %*% diff_obs_B)
    k_wald_B   <- length(diff_obs_B)

    # Aproximación chi^2(k) — solo válida si ambos modelos tienen SE finitos
    se_Q_uhet <- tryCatch(
      sqrt(diag(solve(-modelo_Q$invHessian)))[idx_uhet_Q],
      error = function(e) rep(Inf, k_wald_B)
    )
    se_ok <- all(is.finite(se_Q_uhet))

    if (se_ok) {
      # Wald estándar: W = diff' * Sigma^{-1} * diff (aprox: suma de z^2)
      se_M_uhet <- sqrt(diag(solve(-modelo_M$invHessian)))[idx_uhet_M]
      var_diff   <- se_Q_uhet^2 + se_M_uhet^2
      wald_chi2  <- sum(diff_obs_B^2 / var_diff)
      p_wald_B   <- pchisq(wald_chi2, df = k_wald_B, lower.tail = FALSE)
      message(sprintf("\nTest Wald asintótico Diseño B (aprox. chi²(%d)):", k_wald_B))
      message(sprintf("  Estadístico W:    %.4f", wald_chi2))
      message(sprintf("  p-valor chi²:     %.4f", p_wald_B))
    } else {
      p_wald_B <- NA_real_
      msg_boot <- paste0(
        "Test Wald Diseño B NO DISPONIBLE: modelo Q estimado con Nelder-Mead ",
        "(se=Inf). Bootstrap paramétrico tampoco factible (réplicas no convergen). ",
        "Se reporta ||alpha_Q - delta_M||² = ", round(wald_obs_B, 4),
        " como estadístico descriptivo (sin p-valor válido).")
      message("\n  *** ", msg_boot)
      err_log <<- c(err_log, msg_boot)
    }
    message(sprintf("  ||alpha_Q - delta_M||² (descriptivo) = %.4f", wald_obs_B))
    n_boot_ok   <- 0L
    wald_boot_B <- rep(NA_real_, B_boot)
  } else {
    message("  AVISO: no se puede calcular Wald — coefs uhet no coinciden.")
  }
} else {
  message("  Bootstrap Wald omitido — Diseño B no estimado completamente.")
}

# ============================================================
# GUARDAR TODOS LOS RESULTADOS
# ============================================================

message("\n--- Guardando resultados ---")

# Tablas de contrastes
if (!is.null(tabla_B)) {
  write.csv(tabla_B,
            file.path(INT_DIR, "sfa_contrastes_disenioB.csv"),
            row.names = FALSE, na = "")
  message("  sfa_contrastes_disenioB.csv guardado.")
}
if (!is.null(tabla_A)) {
  write.csv(tabla_A,
            file.path(INT_DIR, "sfa_contrastes_disenioA.csv"),
            row.names = FALSE, na = "")
  message("  sfa_contrastes_disenioA.csv guardado.")
}

# Eficiencias
vars_efic_B <- intersect(c("NCODI","anyo","D_desc","pct_sns","ShareQ",
                            "TE_Q","TE_M","TE_D"),
                          names(df_est))
df_efic_B <- df_est[, vars_efic_B, drop = FALSE]
write.csv(df_efic_B,
          file.path(INT_DIR, "sfa_eficiencias_disenioB.csv"),
          row.names = FALSE, na = "")
message("  sfa_eficiencias_disenioB.csv guardado (",
        nrow(df_efic_B), " obs).")

vars_efic_A <- intersect(c("NCODI","anyo","D_desc","pct_sns","ShareQ",
                            "TE_Total","TE_I"),
                          names(df_est_i))
df_efic_A <- df_est_i[, vars_efic_A, drop = FALSE]
write.csv(df_efic_A,
          file.path(INT_DIR, "sfa_eficiencias_disenioA.csv"),
          row.names = FALSE, na = "")
message("  sfa_eficiencias_disenioA.csv guardado (",
        nrow(df_efic_A), " obs).")

if (nrow(df_efic_C) > 0) {
  write.csv(df_efic_C,
            file.path(INT_DIR, "sfa_eficiencias_disenioC.csv"),
            row.names = FALSE, na = "")
  message("  sfa_eficiencias_disenioC.csv guardado (",
          nrow(df_efic_C), " obs).")
}

# Log de errores
if (length(err_log) > 0) {
  writeLines(c(paste("Log generado:", Sys.time()), "", err_log),
             file.path(INT_DIR, "sfa_errores_convergencia.txt"))
  message("\n*** Se registraron ", length(err_log),
          " errores/warnings — ver sfa_errores_convergencia.txt")
} else {
  message("\n✓ Sin errores de convergencia.")
  log_path <- file.path(INT_DIR, "sfa_errores_convergencia.txt")
  if (file.exists(log_path)) file.remove(log_path)
}

# ============================================================
# RESUMEN FINAL DE CONVERGENCIA
# ============================================================

message("\n═══════════════════════════════════════════")
message("RESUMEN FINAL — Script 11")
message("═══════════════════════════════════════════")

check_modelo <- function(m, label) {
  if (is.null(m)) {
    message(sprintf("  %-22s  FALLÓ", label))
  } else {
    ll  <- tryCatch(as.numeric(m$mlLoglik), error = function(e) NA_real_)
    te  <- get_te(m)
    n   <- if (!is.null(te)) sum(!is.na(te)) else NA_integer_
    med <- if (!is.null(te)) mean(te, na.rm=TRUE) else NA_real_
    # Detectar distribución usada
    dist_usada <- tryCatch(m$udist, error = function(e) "?")
    message(sprintf("  %-22s  OK  |  ll=%9.2f  TE medio=%.3f  (n=%d)  [%s]",
                    label, ll, med, n, dist_usada))
  }
}

check_modelo(modelo_D,     "Diseño D (ODF)")
check_modelo(modelo_Q,     "Diseño B — modelo Q")
check_modelo(modelo_M,     "Diseño B — modelo M")
check_modelo(modelo_Total, "Diseño A — modelo Total")
check_modelo(modelo_I,     "Diseño A — modelo I")
check_modelo(modelo_C,     "Diseño C — panel")

modelos_ok <- sum(!sapply(list(modelo_D, modelo_Q, modelo_M,
                               modelo_Total, modelo_I, modelo_C), is.null))

message(sprintf("\n  Modelos estimados: %d / 6", modelos_ok))
message(sprintf("  Bootstrap réplicas válidas: %d / %d", n_boot_ok, B_boot))

if (modelos_ok == 6 && n_boot_ok >= 0.8 * B_boot) {
  message("\n  *** TODOS LOS MODELOS CONVERGIERON (B_boot=", B_boot, ")")
  message("  *** Puedes relanzar con B_boot <- 4999 para la versión definitiva.")
} else if (modelos_ok == 6) {
  message("\n  *** Todos los modelos convergieron pero el bootstrap falló parcialmente.")
  message("  *** Revisa sfa_errores_convergencia.txt.")
} else {
  message(sprintf("\n  *** %d modelos fallaron. Revisa sfa_errores_convergencia.txt.",
                  6 - modelos_ok))
}

message("\n=== Script 11 completado ===")
