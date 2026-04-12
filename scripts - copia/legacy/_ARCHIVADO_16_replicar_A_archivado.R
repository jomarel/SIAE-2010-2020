config_path <- if (file.exists("scripts/00_config.R")) "scripts/00_config.R" else "00_config.R"
source(config_path)

if (!requireNamespace("sfaR", quietly = TRUE)) {
  stop("Instala sfaR: install.packages('sfaR')")
}
if (!requireNamespace("dplyr", quietly = TRUE)) {
  stop("Instala dplyr: install.packages('dplyr')")
}

library(sfaR)
library(dplyr)

set.seed(42)

load(file.path(INT_DIR, "df_sfa.RData"))
message("df_sfa: ", nrow(df_sfa), " x ", ncol(df_sfa))

# Replica del script archivado que generó los contrastes históricos:
# - sin filtro de agudos
# - sin exclusión COVID
# - sin CCAA en ineficiencia
# - sin cluster en frontera
# - D_desc basada en la coalescencia CNH/SIAE descrita en la tesis
df_rep <- df_sfa %>%
  mutate(
    D_desc_rep = dplyr::coalesce(D_desc_tesis, D_desc),
    desc_pago_rep = if ("desc_pago_tesis" %in% names(.)) desc_pago_tesis else D_desc_rep * pct_sns,
    desc_shareQ_rep = if ("desc_shareQ_tesis" %in% names(.)) desc_shareQ_tesis else D_desc_rep * ShareQ
  )

df_est <- df_rep %>%
  filter(
    !is.na(ln_altQ_pond) & is.finite(ln_altQ_pond),
    !is.na(ln_altM_pond) & is.finite(ln_altM_pond),
    !is.na(ln_altTotal_pond) & is.finite(ln_altTotal_pond),
    !is.na(ln_L_total_c),
    !is.na(ln_K_camas_c),
    !is.na(D_desc_rep),
    !is.na(pct_sns),
    altTotal_bruto > 100
  )

df_est_i <- df_est %>%
  filter(!is.na(ln_i_diag) & is.finite(ln_i_diag) & !i_diag_outlier)

message("Replica archivada: df_est = ", nrow(df_est), " obs | ",
        n_distinct(df_est$NCODI), " hosp")
message("Replica archivada: df_est_i = ", nrow(df_est_i), " obs | ",
        n_distinct(df_est_i$NCODI), " hosp")

frontier_rhs_str <- paste(
  "ln_L_total_c + ln_K_camas_c + ln_K_tech_c +",
  "ln_L_total_c2 + ln_K_camas_c2 + trend + trend2"
)

uhet_formula <- ~ D_desc_rep + pct_sns + desc_pago_rep + ShareQ + desc_shareQ_rep
uhet_simple  <- ~ D_desc_rep + pct_sns + ShareQ

es_degenerado <- function(m) {
  if (is.null(m)) return(TRUE)
  ll <- tryCatch(as.numeric(m$mlLoglik), error = function(e) NA_real_)
  if (!is.finite(ll) || ll > 0 || abs(ll) > 1e6) return(TRUE)
  cf <- tryCatch(coef(m), error = function(e) NULL)
  if (is.null(cf) || any(!is.finite(cf))) return(TRUE)
  if (any(abs(cf) > 100, na.rm = TRUE)) return(TRUE)
  FALSE
}

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
  configs <- Filter(function(cfg) !is.null(cfg$form), configs)

  for (cfg in configs) {
    message(sprintf("  [%s] %s/%s/%s", label, cfg$tag, cfg$udist, cfg$method))
    m <- NULL
    capture.output({
      m <- tryCatch(
        suppressWarnings(
          sfaR::sfacross(
            formula = cfg$form,
            uhet = cfg$uhet_f,
            data = data,
            S = 1L,
            udist = cfg$udist,
            method = cfg$method,
            hessianType = cfg$hess
          )
        ),
        error = function(e) NULL
      )
    })
    if (!is.null(m) && !es_degenerado(m)) return(m)
  }
  NULL
}

extraer_tab <- function(m, label) {
  if (is.null(m)) return(NULL)
  cf <- coef(m)
  se <- tryCatch(sqrt(diag(vcov(m))), error = function(e) rep(NA_real_, length(cf)))
  idx <- grep("^Zu_", names(cf))
  data.frame(
    param = sub("^Zu_", "", names(cf)[idx]),
    coef = round(cf[idx], 6),
    se = round(se[idx], 6),
    t_stat = round(cf[idx] / se[idx], 6),
    modelo = label,
    row.names = NULL,
    stringsAsFactors = FALSE
  )
}

formula_A_tot <- as.formula(paste("ln_altTotal_pond ~", frontier_rhs_str))
formula_A_i   <- as.formula(paste("ln_i_diag ~", frontier_rhs_str))
formula_A_cd_tot <- ln_altTotal_pond ~ ln_L_total_c + ln_K_camas_c + ln_K_tech_c + trend
formula_A_cd_i   <- ln_i_diag ~ ln_L_total_c + ln_K_camas_c + ln_K_tech_c + trend

message("\n=== Replica archivada: Diseño A ===")
modelo_A_tot_arch <- estimar_sfa(formula_A_tot, formula_A_cd_tot, uhet_formula, df_est_i, "A_Total_arch")
modelo_A_i_arch   <- estimar_sfa(formula_A_i,   formula_A_cd_i,   uhet_formula, df_est_i, "A_I_arch")

tab_tot <- extraer_tab(modelo_A_tot_arch, "A_Total_arch")
tab_i   <- extraer_tab(modelo_A_i_arch,   "A_I_arch")

tab_contr <- merge(
  tab_tot[, c("param", "coef", "se")],
  tab_i[,   c("param", "coef", "se")],
  by = "param",
  suffixes = c("_Tot", "_I")
) %>%
  mutate(
    diferencia = coef_Tot - coef_I,
    se_diff = sqrt(se_Tot^2 + se_I^2),
    z_stat = diferencia / se_diff,
    p_valor = 2 * (1 - pnorm(abs(z_stat)))
  )

out_dir <- file.path(OUT_DIR, "sfa_replicas_archivado")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

if (!is.null(modelo_A_tot_arch)) saveRDS(modelo_A_tot_arch, file.path(INT_DIR, "sfa_A_Total_archivado_replica.rds"))
if (!is.null(modelo_A_i_arch))   saveRDS(modelo_A_i_arch,   file.path(INT_DIR, "sfa_A_I_archivado_replica.rds"))
if (!is.null(tab_tot)) utils::write.csv(tab_tot, file.path(INT_DIR, "tabla_A_Total_archivado_replica.csv"), row.names = FALSE)
if (!is.null(tab_i))   utils::write.csv(tab_i,   file.path(INT_DIR, "tabla_A_I_archivado_replica.csv"), row.names = FALSE)
if (!is.null(tab_contr)) utils::write.csv(tab_contr, file.path(out_dir, "contraste_A_archivado_replica.csv"), row.names = FALSE)

message("Replica archivada guardada en: ", out_dir)
