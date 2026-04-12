# ============================================================
# 11_estimar_sfa_A_A1_v2.R — Estimación separada de Diseños A y A1
# A  = Diseño A original del script definitivo
# A1 = Igual que A, misma muestra y misma frontera, pero SIN
#      ShareQ ni ninguna de sus interacciones en ineficiencia
#
# Esta versión busca 00_config.R de forma robusta para que no
# dependa del directorio de trabajo desde el que se lance.
# ============================================================

# ── Localizar script y config ────────────────────────────────
get_script_dir <- function() {
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("^--file=", args, value = TRUE)
  if (length(file_arg) > 0) {
    return(dirname(normalizePath(sub("^--file=", "", file_arg[1]), mustWork = FALSE)))
  }

  ofile <- tryCatch(normalizePath(sys.frames()[[1]]$ofile, mustWork = FALSE),
                    error = function(e) "")
  if (nzchar(ofile)) {
    return(dirname(ofile))
  }

  normalizePath(getwd(), mustWork = FALSE)
}

script_dir <- get_script_dir()
wd_dir     <- normalizePath(getwd(), mustWork = FALSE)

cand_cfg <- unique(c(
  file.path(wd_dir, "scripts", "00_config.R"),
  file.path(wd_dir, "00_config.R"),
  file.path(script_dir, "scripts", "00_config.R"),
  file.path(script_dir, "00_config.R"),
  file.path(dirname(script_dir), "scripts", "00_config.R"),
  file.path(dirname(script_dir), "00_config.R")
))

config_path <- cand_cfg[file.exists(cand_cfg)][1]
if (is.na(config_path)) {
  stop(
    paste0(
      "No encuentro 00_config.R.\n",
      "He buscado en:\n - ", paste(cand_cfg, collapse = "\n - "), "\n\n",
      "Solución: guarda este script dentro de la carpeta del proyecto o ejecuta setwd('ruta/al/proyecto') antes de lanzarlo."
    )
  )
}

source(config_path)
message("Config cargado desde: ", normalizePath(config_path, mustWork = FALSE))

# ── Paquetes ────────────────────────────────────────────────
if (!requireNamespace("sfaR", quietly = TRUE)) {
  stop("Instala sfaR: install.packages('sfaR')")
}
if (!requireNamespace("dplyr", quietly = TRUE)) {
  stop("Instala dplyr: install.packages('dplyr')")
}

library(sfaR)
library(dplyr)

set.seed(42)

# ── Cargar datos ────────────────────────────────────────────
if (!exists("INT_DIR")) {
  stop("INT_DIR no existe tras cargar 00_config.R")
}

ruta_df <- file.path(INT_DIR, "df_sfa.RData")
if (!file.exists(ruta_df)) {
  stop("No encuentro df_sfa.RData en: ", ruta_df)
}

load(ruta_df)
message("df_sfa: ", nrow(df_sfa), " x ", ncol(df_sfa))

# ── Filtros base: idénticos a los del script original para A ──
# Nota: se mantiene !is.na(ShareQ) para que A1 use EXACTAMENTE
# la misma muestra que A y la comparación sea estricta.
df_base <- df_sfa %>%
  filter(
    es_agudo == 1,
    !anyo %in% 2020:2022,
    altTotal_bruto > 100,
    (!"d_desc_estimable" %in% names(.) | d_desc_estimable == 1L),
    !is.na(D_desc), !is.na(pct_sns), !is.na(ShareQ),
    !is.na(ln_L_total_c), !is.na(ln_K_camas_c),
    !is.na(ccaa_cod)
  )

message("df_base (agudos, sin COVID): ", nrow(df_base),
        " obs | ", dplyr::n_distinct(df_base$NCODI), " hosp")

# Muestra Diseño A (idéntica al script original)
df_est_A <- df_base %>%
  filter(
    altTotal_bruto >= 200,
    is.finite(ln_altTotal_pond),
    is.finite(ln_i_diag),
    ln_i_diag > quantile(ln_i_diag, 0.01, na.rm = TRUE)
  )

message("df_est_A (i_diag válido): ", nrow(df_est_A),
        " obs | ", dplyr::n_distinct(df_est_A$NCODI), " hosp")

# Rama tesis: replicar la construcción descrita en la memoria
# D_desc_tesis = coalesce(D_desc_cnh, D_desc_siae), sin exclusión adicional
df_base_tesis <- df_sfa %>%
  filter(
    es_agudo == 1,
    !anyo %in% 2020:2022,
    !is.na(D_desc_tesis), !is.na(pct_sns), !is.na(ShareQ),
    !is.na(ln_L_total_c), !is.na(ln_K_camas_c),
    !is.na(ccaa_cod)
  )

message("df_base_tesis (agudos, sin COVID, coalesce CNH/SIAE): ",
        nrow(df_base_tesis), " obs | ",
        dplyr::n_distinct(df_base_tesis$NCODI), " hosp")

df_est_A_tesis <- df_base_tesis %>%
  filter(
    altTotal_bruto >= 200,
    is.finite(ln_altTotal_pond),
    is.finite(ln_i_diag),
    ln_i_diag > quantile(ln_i_diag, 0.01, na.rm = TRUE)
  )

message("df_est_A_tesis (muestra tesis): ", nrow(df_est_A_tesis),
        " obs | ", dplyr::n_distinct(df_est_A_tesis$NCODI), " hosp")

# ── Especificaciones ────────────────────────────────────────
ccaa_vars <- grep("^d_ccaa_[0-9]+$", names(df_base), value = TRUE)
message("Dummies CCAA detectadas: ", length(ccaa_vars))

frontier_str <- paste(
  "~ ln_L_total_c + ln_K_camas_c + ln_K_tech_c +",
  "ln_L_total_c2 + ln_K_camas_c2 + trend + trend2 +",
  "d_cluster2 + d_cluster3 + d_cluster4 + d_cluster5"
)

# A original: igual que el script principal
uhet_A_full <- as.formula(paste(
  "~ D_desc + pct_sns + desc_pago +",
  "ShareQ + desc_shareQ +",
  paste(ccaa_vars, collapse = " + ")
))

uhet_A_nocc <- ~ D_desc + pct_sns + desc_pago +
                 ShareQ + desc_shareQ
uhet_A_min  <- ~ D_desc + pct_sns + desc_pago + ShareQ
fb_A <- list(uhet_A_nocc, uhet_A_min)

# A1: misma frontera y misma muestra, pero sin ShareQ ni interacciones
uhet_A1_full <- as.formula(paste(
  "~ D_desc + pct_sns + desc_pago +",
  paste(ccaa_vars, collapse = " + ")
))

uhet_A1_nocc <- ~ D_desc + pct_sns + desc_pago
uhet_A1_min  <- ~ D_desc + pct_sns + desc_pago
fb_A1 <- list(uhet_A1_nocc, uhet_A1_min)

# Rama tesis: especificación descrita en el PDF
uhet_A_tesis_full <- as.formula(paste(
  "~ D_desc_tesis + pct_sns + desc_pago_tesis +",
  "ShareQ + desc_shareQ_tesis +",
  paste(ccaa_vars, collapse = " + ")
))

uhet_A_tesis_nocc <- ~ D_desc_tesis + pct_sns + desc_pago_tesis +
  ShareQ + desc_shareQ_tesis
uhet_A_tesis_min  <- ~ D_desc_tesis + pct_sns + desc_pago_tesis + ShareQ
fb_A_tesis <- list(uhet_A_tesis_nocc, uhet_A_tesis_min)

uhet_A1_tesis_full <- as.formula(paste(
  "~ D_desc_tesis + pct_sns + desc_pago_tesis +",
  paste(ccaa_vars, collapse = " + ")
))

uhet_A1_tesis_nocc <- ~ D_desc_tesis + pct_sns + desc_pago_tesis
uhet_A1_tesis_min  <- ~ D_desc_tesis + pct_sns + desc_pago_tesis
fb_A1_tesis <- list(uhet_A1_tesis_nocc, uhet_A1_tesis_min)

# ── Utilidades ──────────────────────────────────────────────
err_log <- character(0)

es_degenerado <- function(m) {
  if (is.null(m)) return(TRUE)
  ll <- tryCatch(as.numeric(m$mlLoglik), error = function(e) NA_real_)
  if (!is.finite(ll) || abs(ll) > 1e10) return(TRUE)

  cf <- tryCatch(coef(m), error = function(e) NULL)
  if (is.null(cf) || any(!is.finite(cf))) return(TRUE)
  if (any(abs(cf) > 100, na.rm = TRUE)) return(TRUE)

  se <- tryCatch(sqrt(diag(vcov(m))), error = function(e) NULL)
  if (is.null(se)) return(TRUE)

  idx <- grep("^Zu_", names(cf))
  if (length(idx) > 0 && any(!is.finite(se[idx]), na.rm = TRUE)) return(TRUE)
  FALSE
}

estimar_sfa <- function(lhs, uhet, data, label, fallbacks) {
  frontier <- as.formula(paste(lhs, frontier_str))
  configs <- list(
    list(ud = "tnormal", mt = "bfgs", ht = 1L),
    list(ud = "tnormal", mt = "bhhh", ht = 1L),
    list(ud = "tnormal", mt = "nm",   ht = 2L),
    list(ud = "hnormal", mt = "bfgs", ht = 1L),
    list(ud = "hnormal", mt = "bhhh", ht = 2L)
  )

  for (uh in c(list(uhet), fallbacks)) {
    for (cfg in configs) {
      tag <- sprintf("[%s] %s/%s uhet=%s",
                     label, cfg$ud, cfg$mt,
                     deparse(uh[[2]])[1])
      message("  Prob: ", tag)

      m <- NULL
      capture.output({
        m <- tryCatch(
          suppressWarnings(sfaR::sfacross(
            formula = frontier,
            uhet = uh,
            data = data,
            S = 1L,
            udist = cfg$ud,
            method = cfg$mt,
            hessianType = cfg$ht
          )),
          error = function(e) {
            err_log <<- c(err_log, paste(tag, e$message))
            NULL
          }
        )
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

extraer_tab <- function(m, label) {
  if (is.null(m)) return(NULL)
  cf <- coef(m)
  se <- tryCatch(sqrt(diag(vcov(m))),
                 error = function(e) rep(NA_real_, length(cf)))
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
    message(titulo, ": tabla no disponible")
    return(NULL)
  }

  tab <- merge(
    t1[, c("param", "coef", "se", "t_stat")],
    t2[, c("param", "coef", "se", "t_stat")],
    by = "param", suffixes = c("_1", "_2")
  )
  tab$dif    <- round(tab$coef_1 - tab$coef_2, 4)
  tab$se_dif <- round(sqrt(tab$se_1^2 + tab$se_2^2), 4)
  tab$z      <- round(tab$dif / tab$se_dif, 3)
  tab$p      <- round(2 * (1 - pnorm(abs(tab$z))), 4)

  names(tab)[2:4] <- c(l1, paste0("se_", l1), paste0("t_", l1))
  names(tab)[5:7] <- c(l2, paste0("se_", l2), paste0("t_", l2))

  message("\n════════ ", titulo, " ════════")
  tab_show <- tab[!grepl("^d_ccaa", tab$param), ]
  print(tab_show)
  invisible(tab)
}

comparar_mismos_outputs <- function(mA, mA1, nombre_A, nombre_A1, titulo) {
  tA  <- extraer_tab(mA, nombre_A)
  tA1 <- extraer_tab(mA1, nombre_A1)
  if (is.null(tA) || is.null(tA1)) {
    message(titulo, ": tabla no disponible")
    return(NULL)
  }

  tab <- merge(
    tA[,  c("param", "coef", "se", "t_stat")],
    tA1[, c("param", "coef", "se", "t_stat")],
    by = "param", all = TRUE, suffixes = c("_A", "_A1")
  )

  tab_show <- tab[!grepl("^d_ccaa", tab$param), ]
  message("\n════════ ", titulo, " ════════")
  print(tab_show)
  invisible(tab)
}

# ════════════════════════════════════════════════════════
# DISEÑO A — original
# ════════════════════════════════════════════════════════
# Modelo A con D_desc
message("\n═══ Modelo A con D_desc ═══")

modelo_A_Tot <- tryCatch({
  m <- estimar_sfa("ln_altTotal_pond", uhet_A_full, df_est_A, "A_Total", fb_A)
  if (!is.null(m)) {
    saveRDS(m, file.path(INT_DIR, "sfa_A_Total_separado.rds"))
    saveRDS(m, file.path(INT_DIR, "sfa_A_Total_Ddesc_separado.rds"))
  }
  m
}, error = function(e) {
  message("A_Total FALLÓ: ", e$message)
  NULL
})

modelo_A_I <- tryCatch({
  m <- estimar_sfa("ln_i_diag", uhet_A_full, df_est_A, "A_I", fb_A)
  if (!is.null(m)) {
    saveRDS(m, file.path(INT_DIR, "sfa_A_I_separado.rds"))
    saveRDS(m, file.path(INT_DIR, "sfa_A_I_Ddesc_separado.rds"))
  }
  m
}, error = function(e) {
  message("A_I FALLÓ: ", e$message)
  NULL
})

if (!is.null(modelo_A_Tot)) {
  te <- tryCatch(sfaR::efficiencies(modelo_A_Tot)$teJLMS, error = function(e) NULL)
  if (!is.null(te)) {
    message(sprintf("  TE_A_Total: media=%.3f sd=%.3f", mean(te, na.rm = TRUE), sd(te, na.rm = TRUE)))
  }
}
if (!is.null(modelo_A_I)) {
  te <- tryCatch(sfaR::efficiencies(modelo_A_I)$teJLMS, error = function(e) NULL)
  if (!is.null(te)) {
    message(sprintf("  TE_A_I: media=%.3f sd=%.3f", mean(te, na.rm = TRUE), sd(te, na.rm = TRUE)))
  }
}

tab_A <- mostrar_contrastes(modelo_A_Tot, modelo_A_I,
                            "A_Total", "A_I",
                            "DISEÑO A: Cantidad vs Intensidad")

# ════════════════════════════════════════════════════════
# DISEÑO A1 — sin ShareQ ni interacciones
# ════════════════════════════════════════════════════════
# Modelo A1 con D_desc
message("\n═══ Modelo A1 con D_desc ═══")

modelo_A1_Tot <- tryCatch({
  m <- estimar_sfa("ln_altTotal_pond", uhet_A1_full, df_est_A, "A1_Total", fb_A1)
  if (!is.null(m)) {
    saveRDS(m, file.path(INT_DIR, "sfa_A1_Total_separado.rds"))
    saveRDS(m, file.path(INT_DIR, "sfa_A1_Total_Ddesc_separado.rds"))
  }
  m
}, error = function(e) {
  message("A1_Total FALLÓ: ", e$message)
  NULL
})

modelo_A1_I <- tryCatch({
  m <- estimar_sfa("ln_i_diag", uhet_A1_full, df_est_A, "A1_I", fb_A1)
  if (!is.null(m)) {
    saveRDS(m, file.path(INT_DIR, "sfa_A1_I_separado.rds"))
    saveRDS(m, file.path(INT_DIR, "sfa_A1_I_Ddesc_separado.rds"))
  }
  m
}, error = function(e) {
  message("A1_I FALLÓ: ", e$message)
  NULL
})

if (!is.null(modelo_A1_Tot)) {
  te <- tryCatch(sfaR::efficiencies(modelo_A1_Tot)$teJLMS, error = function(e) NULL)
  if (!is.null(te)) {
    message(sprintf("  TE_A1_Total: media=%.3f sd=%.3f", mean(te, na.rm = TRUE), sd(te, na.rm = TRUE)))
  }
}
if (!is.null(modelo_A1_I)) {
  te <- tryCatch(sfaR::efficiencies(modelo_A1_I)$teJLMS, error = function(e) NULL)
  if (!is.null(te)) {
    message(sprintf("  TE_A1_I: media=%.3f sd=%.3f", mean(te, na.rm = TRUE), sd(te, na.rm = TRUE)))
  }
}

tab_A1 <- mostrar_contrastes(modelo_A1_Tot, modelo_A1_I,
                             "A1_Total", "A1_I",
                             "DISEÑO A1: Cantidad vs Intensidad (sin ShareQ)")

# ════════════════════════════════════════════════════════
# COMPARACIONES A vs A1
# ════════════════════════════════════════════════════════
comp_Tot <- comparar_mismos_outputs(modelo_A_Tot, modelo_A1_Tot,
                                    "A_Total", "A1_Total",
                                    "COMPARACIÓN A vs A1 — Output total")

comp_I <- comparar_mismos_outputs(modelo_A_I, modelo_A1_I,
                                  "A_I", "A1_I",
                                  "COMPARACIÓN A vs A1 — Intensidad")

# ============================================================
# DISEÑO A TESIS — réplica fiel de la memoria
# ============================================================
# Modelo A con D_desc_tesis
message("\n═══ Modelo A con D_desc_tesis (réplica tesis) ═══")

modelo_A_Tot_tesis <- tryCatch({
  m <- estimar_sfa(
    "ln_altTotal_pond", uhet_A_tesis_full, df_est_A_tesis,
    "A_Total_tesis", fb_A_tesis
  )
  if (!is.null(m)) {
    saveRDS(m, file.path(INT_DIR, "sfa_A_Total_tesis.rds"))
  }
  m
}, error = function(e) {
  message("A_Total_tesis FALLÓ: ", e$message)
  NULL
})

modelo_A_I_tesis <- tryCatch({
  m <- estimar_sfa(
    "ln_i_diag", uhet_A_tesis_full, df_est_A_tesis,
    "A_I_tesis", fb_A_tesis
  )
  if (!is.null(m)) {
    saveRDS(m, file.path(INT_DIR, "sfa_A_I_tesis.rds"))
  }
  m
}, error = function(e) {
  message("A_I_tesis FALLÓ: ", e$message)
  NULL
})

tab_A_tesis <- mostrar_contrastes(
  modelo_A_Tot_tesis, modelo_A_I_tesis,
  "A_Total_tesis", "A_I_tesis",
  "DISEÑO A TESIS: Cantidad vs Intensidad"
)

# ============================================================
# DISEÑO A1 TESIS — misma muestra, sin ShareQ
# ============================================================
# Modelo A1 con D_desc_tesis
message("\n═══ Modelo A1 con D_desc_tesis (réplica tesis) ═══")

modelo_A1_Tot_tesis <- tryCatch({
  m <- estimar_sfa(
    "ln_altTotal_pond", uhet_A1_tesis_full, df_est_A_tesis,
    "A1_Total_tesis", fb_A1_tesis
  )
  if (!is.null(m)) {
    saveRDS(m, file.path(INT_DIR, "sfa_A1_Total_tesis.rds"))
  }
  m
}, error = function(e) {
  message("A1_Total_tesis FALLÓ: ", e$message)
  NULL
})

modelo_A1_I_tesis <- tryCatch({
  m <- estimar_sfa(
    "ln_i_diag", uhet_A1_tesis_full, df_est_A_tesis,
    "A1_I_tesis", fb_A1_tesis
  )
  if (!is.null(m)) {
    saveRDS(m, file.path(INT_DIR, "sfa_A1_I_tesis.rds"))
  }
  m
}, error = function(e) {
  message("A1_I_tesis FALLÓ: ", e$message)
  NULL
})

tab_A1_tesis <- mostrar_contrastes(
  modelo_A1_Tot_tesis, modelo_A1_I_tesis,
  "A1_Total_tesis", "A1_I_tesis",
  "DISEÑO A1 TESIS: Cantidad vs Intensidad (sin ShareQ)"
)

comp_Tot_tesis <- comparar_mismos_outputs(
  modelo_A_Tot_tesis, modelo_A1_Tot_tesis,
  "A_Total_tesis", "A1_Total_tesis",
  "COMPARACIÓN A vs A1 TESIS — Output total"
)

comp_I_tesis <- comparar_mismos_outputs(
  modelo_A_I_tesis, modelo_A1_I_tesis,
  "A_I_tesis", "A1_I_tesis",
  "COMPARACIÓN A vs A1 TESIS — Intensidad"
)

# ── Guardar tablas ──────────────────────────────────────────
write_tab_safe <- function(tab, fname) {
  if (!is.null(tab)) {
    tryCatch(
      utils::write.csv(tab, file.path(INT_DIR, fname), row.names = FALSE),
      error = function(e) {
        warning(sprintf("No se pudo guardar %s: %s", fname, e$message),
                call. = FALSE)
      }
    )
  }
}

write_tab_safe(extraer_tab(modelo_A_Tot,  "A_Total"),  "tabla_A_Total_inef.csv")
write_tab_safe(extraer_tab(modelo_A_Tot,  "A_Total"),  "tabla_A_Total_Ddesc_inef.csv")
write_tab_safe(extraer_tab(modelo_A_I,    "A_I"),      "tabla_A_I_inef.csv")
write_tab_safe(extraer_tab(modelo_A_I,    "A_I"),      "tabla_A_I_Ddesc_inef.csv")
write_tab_safe(extraer_tab(modelo_A1_Tot, "A1_Total"), "tabla_A1_Total_inef.csv")
write_tab_safe(extraer_tab(modelo_A1_Tot, "A1_Total"), "tabla_A1_Total_Ddesc_inef.csv")
write_tab_safe(extraer_tab(modelo_A1_I,   "A1_I"),     "tabla_A1_I_inef.csv")
write_tab_safe(extraer_tab(modelo_A1_I,   "A1_I"),     "tabla_A1_I_Ddesc_inef.csv")
write_tab_safe(tab_A,   "contraste_A_total_vs_intens.csv")
write_tab_safe(tab_A,   "contraste_A_Ddesc_total_vs_intens.csv")
write_tab_safe(tab_A1,  "contraste_A1_total_vs_intens.csv")
write_tab_safe(tab_A1,  "contraste_A1_Ddesc_total_vs_intens.csv")
write_tab_safe(comp_Tot, "comparacion_A_vs_A1_total.csv")
write_tab_safe(comp_Tot, "comparacion_A_vs_A1_Ddesc_total.csv")
write_tab_safe(comp_I,   "comparacion_A_vs_A1_intens.csv")
write_tab_safe(comp_I,   "comparacion_A_vs_A1_Ddesc_intens.csv")
write_tab_safe(extraer_tab(modelo_A_Tot_tesis,  "A_Total_tesis"),  "tabla_A_Total_tesis_inef.csv")
write_tab_safe(extraer_tab(modelo_A_I_tesis,    "A_I_tesis"),      "tabla_A_I_tesis_inef.csv")
write_tab_safe(extraer_tab(modelo_A1_Tot_tesis, "A1_Total_tesis"), "tabla_A1_Total_tesis_inef.csv")
write_tab_safe(extraer_tab(modelo_A1_I_tesis,   "A1_I_tesis"),     "tabla_A1_I_tesis_inef.csv")
write_tab_safe(tab_A_tesis,   "contraste_A_tesis_total_vs_intens.csv")
write_tab_safe(tab_A1_tesis,  "contraste_A1_tesis_total_vs_intens.csv")
write_tab_safe(comp_Tot_tesis, "comparacion_A_vs_A1_tesis_total.csv")
write_tab_safe(comp_I_tesis,   "comparacion_A_vs_A1_tesis_intens.csv")

sig_code <- function(tab, param) {
  if (is.null(tab) || !("param" %in% names(tab)) || !("t_stat" %in% names(tab))) return("nd")
  s <- tab[tab$param == param, , drop = FALSE]
  if (nrow(s) == 0 || anyNA(s$t_stat)) return("nd")
  p <- 2 * (1 - pnorm(abs(s$t_stat[1])))
  if (p < 0.01) return("***")
  if (p < 0.05) return("**")
  if (p < 0.10) return("*")
  "ns"
}

coef_val <- function(tab, param) {
  if (is.null(tab) || !("param" %in% names(tab)) || !("coef" %in% names(tab))) return(NA_real_)
  s <- tab[tab$param == param, , drop = FALSE]
  if (nrow(s) == 0) return(NA_real_)
  s$coef[1]
}

resumen_tesis <- dplyr::bind_rows(
  data.frame(
    modelo = "A_Total_tesis",
    ecuacion = "Cantidad",
    n = nrow(df_est_A_tesis),
    coef_D_desc_tesis = coef_val(extraer_tab(modelo_A_Tot_tesis, "A_Total_tesis"), "D_desc_tesis"),
    sig_D_desc_tesis = sig_code(extraer_tab(modelo_A_Tot_tesis, "A_Total_tesis"), "D_desc_tesis"),
    coef_pct_sns = coef_val(extraer_tab(modelo_A_Tot_tesis, "A_Total_tesis"), "pct_sns"),
    sig_pct_sns = sig_code(extraer_tab(modelo_A_Tot_tesis, "A_Total_tesis"), "pct_sns"),
    coef_interaccion_pago = coef_val(extraer_tab(modelo_A_Tot_tesis, "A_Total_tesis"), "desc_pago_tesis"),
    sig_interaccion_pago = sig_code(extraer_tab(modelo_A_Tot_tesis, "A_Total_tesis"), "desc_pago_tesis"),
    coef_ShareQ = coef_val(extraer_tab(modelo_A_Tot_tesis, "A_Total_tesis"), "ShareQ"),
    sig_ShareQ = sig_code(extraer_tab(modelo_A_Tot_tesis, "A_Total_tesis"), "ShareQ"),
    coef_interaccion_ShareQ = coef_val(extraer_tab(modelo_A_Tot_tesis, "A_Total_tesis"), "desc_shareQ_tesis"),
    sig_interaccion_ShareQ = sig_code(extraer_tab(modelo_A_Tot_tesis, "A_Total_tesis"), "desc_shareQ_tesis")
  ),
  data.frame(
    modelo = "A_I_tesis",
    ecuacion = "Intensidad",
    n = nrow(df_est_A_tesis),
    coef_D_desc_tesis = coef_val(extraer_tab(modelo_A_I_tesis, "A_I_tesis"), "D_desc_tesis"),
    sig_D_desc_tesis = sig_code(extraer_tab(modelo_A_I_tesis, "A_I_tesis"), "D_desc_tesis"),
    coef_pct_sns = coef_val(extraer_tab(modelo_A_I_tesis, "A_I_tesis"), "pct_sns"),
    sig_pct_sns = sig_code(extraer_tab(modelo_A_I_tesis, "A_I_tesis"), "pct_sns"),
    coef_interaccion_pago = coef_val(extraer_tab(modelo_A_I_tesis, "A_I_tesis"), "desc_pago_tesis"),
    sig_interaccion_pago = sig_code(extraer_tab(modelo_A_I_tesis, "A_I_tesis"), "desc_pago_tesis"),
    coef_ShareQ = coef_val(extraer_tab(modelo_A_I_tesis, "A_I_tesis"), "ShareQ"),
    sig_ShareQ = sig_code(extraer_tab(modelo_A_I_tesis, "A_I_tesis"), "ShareQ"),
    coef_interaccion_ShareQ = coef_val(extraer_tab(modelo_A_I_tesis, "A_I_tesis"), "desc_shareQ_tesis"),
    sig_interaccion_ShareQ = sig_code(extraer_tab(modelo_A_I_tesis, "A_I_tesis"), "desc_shareQ_tesis")
  ),
  data.frame(
    modelo = "A1_Total_tesis",
    ecuacion = "Cantidad",
    n = nrow(df_est_A_tesis),
    coef_D_desc_tesis = coef_val(extraer_tab(modelo_A1_Tot_tesis, "A1_Total_tesis"), "D_desc_tesis"),
    sig_D_desc_tesis = sig_code(extraer_tab(modelo_A1_Tot_tesis, "A1_Total_tesis"), "D_desc_tesis"),
    coef_pct_sns = coef_val(extraer_tab(modelo_A1_Tot_tesis, "A1_Total_tesis"), "pct_sns"),
    sig_pct_sns = sig_code(extraer_tab(modelo_A1_Tot_tesis, "A1_Total_tesis"), "pct_sns"),
    coef_interaccion_pago = coef_val(extraer_tab(modelo_A1_Tot_tesis, "A1_Total_tesis"), "desc_pago_tesis"),
    sig_interaccion_pago = sig_code(extraer_tab(modelo_A1_Tot_tesis, "A1_Total_tesis"), "desc_pago_tesis"),
    coef_ShareQ = NA_real_,
    sig_ShareQ = "nd",
    coef_interaccion_ShareQ = NA_real_,
    sig_interaccion_ShareQ = "nd"
  ),
  data.frame(
    modelo = "A1_I_tesis",
    ecuacion = "Intensidad",
    n = nrow(df_est_A_tesis),
    coef_D_desc_tesis = coef_val(extraer_tab(modelo_A1_I_tesis, "A1_I_tesis"), "D_desc_tesis"),
    sig_D_desc_tesis = sig_code(extraer_tab(modelo_A1_I_tesis, "A1_I_tesis"), "D_desc_tesis"),
    coef_pct_sns = coef_val(extraer_tab(modelo_A1_I_tesis, "A1_I_tesis"), "pct_sns"),
    sig_pct_sns = sig_code(extraer_tab(modelo_A1_I_tesis, "A1_I_tesis"), "pct_sns"),
    coef_interaccion_pago = coef_val(extraer_tab(modelo_A1_I_tesis, "A1_I_tesis"), "desc_pago_tesis"),
    sig_interaccion_pago = sig_code(extraer_tab(modelo_A1_I_tesis, "A1_I_tesis"), "desc_pago_tesis"),
    coef_ShareQ = NA_real_,
    sig_ShareQ = "nd",
    coef_interaccion_ShareQ = NA_real_,
    sig_interaccion_ShareQ = "nd"
  )
)

out_tesis_dir <- file.path(OUT_DIR, "sfa_ddesc_tesis")
dir.create(out_tesis_dir, recursive = TRUE, showWarnings = FALSE)
write.table(
  resumen_tesis,
  file = file.path(out_tesis_dir, "comparativa_modelos_A_A1_tesis.tsv"),
  sep = "\t",
  row.names = FALSE,
  quote = TRUE,
  na = ""
)

# ── Resumen final ───────────────────────────────────────────
message("\n═══ RESUMEN FINAL A / A1 ═══")
resumen_modelo <- function(m, nombre) {
  if (is.null(m)) {
    message(sprintf("  %-12s FALLÓ", nombre))
    return(invisible(NULL))
  }
  ll   <- tryCatch(as.numeric(m$mlLoglik), error = function(e) NA_real_)
  te   <- tryCatch(sfaR::efficiencies(m)$teJLMS, error = function(e) NULL)
  med  <- if (!is.null(te)) mean(te, na.rm = TRUE) else NA_real_
  dist <- tryCatch(m$udist, error = function(e) "?")
  message(sprintf("  %-12s OK | ll=%10.2f | TE=%.3f [%s]", nombre, ll, med, dist))
}

resumen_modelo(modelo_A_Tot,  "A_Total")
resumen_modelo(modelo_A_I,    "A_I")
resumen_modelo(modelo_A1_Tot, "A1_Total")
resumen_modelo(modelo_A1_I,   "A1_I")

if (length(err_log) > 0) {
  writeLines(c(format(Sys.time()), "", err_log),
             file.path(INT_DIR, "sfa_errores_A_A1_separado.txt"))
  message("\n", length(err_log), " errores — ver sfa_errores_A_A1_separado.txt")
} else {
  message("\nSin errores de convergencia.")
}

message("\n=== Script A / A1 completado ===")
