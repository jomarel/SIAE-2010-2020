config_path <- if (file.exists("scripts/00_config.R")) {
  "scripts/00_config.R"
} else {
  "00_config.R"
}
source(config_path)

if (!requireNamespace("sfaR", quietly = TRUE)) {
  stop("Instala sfaR: install.packages('sfaR')")
}
if (!requireNamespace("dplyr", quietly = TRUE)) {
  stop("Instala dplyr: install.packages('dplyr')")
}

library(sfaR)
library(dplyr)

rds_exists <- function(f) file.exists(file.path(INT_DIR, f))

modelos_todos <- list(
  D_ODF_Ddesc = if (rds_exists("sfa_modeloD_Ddesc_definitivo.rds"))
    readRDS(file.path(INT_DIR, "sfa_modeloD_Ddesc_definitivo.rds")) else NULL,
  A_Tot_Ddesc = if (rds_exists("sfa_mTot_Ddesc_definitivo.rds"))
    readRDS(file.path(INT_DIR, "sfa_mTot_Ddesc_definitivo.rds")) else NULL,
  A_I_Ddesc = if (rds_exists("sfa_mI_Ddesc_definitivo.rds"))
    readRDS(file.path(INT_DIR, "sfa_mI_Ddesc_definitivo.rds")) else NULL,
  A1_Tot_Ddesc = if (rds_exists("sfa_A1_Total_Ddesc_separado.rds"))
    readRDS(file.path(INT_DIR, "sfa_A1_Total_Ddesc_separado.rds")) else NULL,
  A1_I_Ddesc = if (rds_exists("sfa_A1_I_Ddesc_separado.rds"))
    readRDS(file.path(INT_DIR, "sfa_A1_I_Ddesc_separado.rds")) else NULL,
  B_Q_Ddesc = if (rds_exists("sfa_modeloQ_Ddesc_definitivo.rds"))
    readRDS(file.path(INT_DIR, "sfa_modeloQ_Ddesc_definitivo.rds")) else NULL,
  B_M_Ddesc = if (rds_exists("sfa_modeloM_Ddesc_definitivo.rds"))
    readRDS(file.path(INT_DIR, "sfa_modeloM_Ddesc_definitivo.rds")) else NULL,
  C_panel_Ddesc = if (rds_exists("sfa_modeloC_Ddesc_definitivo.rds"))
    readRDS(file.path(INT_DIR, "sfa_modeloC_Ddesc_definitivo.rds")) else NULL
)

sig_code <- function(m, param) {
  if (is.null(m)) return("nd")
  cf <- tryCatch(coef(m), error = function(e) NULL)
  vc <- tryCatch(vcov(m), error = function(e) NULL)
  nm <- paste0("Zu_", param)
  if (is.null(cf) || is.null(vc) || !nm %in% names(cf) || !nm %in% rownames(vc)) return("nd")
  se <- sqrt(diag(vc))[nm]
  if (!is.finite(se) || se == 0) return("nd")
  p <- 2 * (1 - pnorm(abs(cf[nm] / se)))
  if (is.na(p)) "nd" else if (p < 0.01) "***" else if (p < 0.05) "**" else if (p < 0.10) "*" else "ns"
}

coef_val <- function(m, param) {
  if (is.null(m)) return(NA_real_)
  cf <- tryCatch(coef(m), error = function(e) NULL)
  nm <- paste0("Zu_", param)
  if (is.null(cf) || !nm %in% names(cf)) return(NA_real_)
  unname(cf[nm])
}

n_model <- function(m) {
  te <- tryCatch(sfaR::efficiencies(m)$teJLMS, error = function(e) NULL)
  if (is.null(te)) return(NA_integer_)
  length(te)
}

model_specs_ddesc <- list(
  list(key = "D_ODF_Ddesc", design = "D", equation = "ODF",
       vars = "D_desc + pct_sns + D_desc:pct_sns + ShareQ + D_desc:Q_share + CCAA"),
  list(key = "A_Tot_Ddesc", design = "A", equation = "Total",
       vars = "D_desc + pct_sns + D_desc:pct_sns + ShareQ + D_desc:Q_share + CCAA"),
  list(key = "A_I_Ddesc", design = "A", equation = "Intensidad",
       vars = "D_desc + pct_sns + D_desc:pct_sns + ShareQ + D_desc:Q_share + CCAA"),
  list(key = "A1_Tot_Ddesc", design = "A1", equation = "Total",
       vars = "D_desc + pct_sns + D_desc:pct_sns + CCAA"),
  list(key = "A1_I_Ddesc", design = "A1", equation = "Intensidad",
       vars = "D_desc + pct_sns + D_desc:pct_sns + CCAA"),
  list(key = "B_Q_Ddesc", design = "B", equation = "Quirurgico",
       vars = "D_desc + pct_sns + D_desc:pct_sns + ShareQ + D_desc:Q_share + CCAA"),
  list(key = "B_M_Ddesc", design = "B", equation = "Medico",
       vars = "D_desc + pct_sns + D_desc:pct_sns + ShareQ + D_desc:Q_share + CCAA"),
  list(key = "C_panel_Ddesc", design = "C", equation = "Panel servicio",
       vars = "D_desc + pct_sns + D_desc:pct_sns + C_s + ShareQ + D_desc:Q_share + ShareQ:C_s + CCAA")
)

tabla_ddesc <- dplyr::bind_rows(lapply(model_specs_ddesc, function(sp) {
  m <- modelos_todos[[sp$key]]
  data.frame(
    diseno = sp$design,
    ecuacion = sp$equation,
    modelo_objeto = sp$key,
    variables_institucionales = sp$vars,
    coef_D_desc = round(coef_val(m, "D_desc"), 6),
    sig_D_desc = sig_code(m, "D_desc"),
    coef_pct_sns = round(coef_val(m, "pct_sns"), 6),
    sig_pct_sns = sig_code(m, "pct_sns"),
    coef_D_desc_pct_sns = round(coef_val(m, "desc_pago"), 6),
    sig_D_desc_pct_sns = sig_code(m, "desc_pago"),
    coef_Q_share = round(coef_val(m, "ShareQ"), 6),
    sig_Q_share = sig_code(m, "ShareQ"),
    coef_D_desc_Q_share = round(coef_val(m, "desc_shareQ"), 6),
    sig_D_desc_Q_share = sig_code(m, "desc_shareQ"),
    logLik = round(tryCatch(as.numeric(m$mlLoglik), error = function(e) NA_real_), 4),
    AIC = round(tryCatch(AIC(m), error = function(e) NA_real_), 4),
    BIC = round(tryCatch(BIC(m), error = function(e) NA_real_), 4),
    N = n_model(m),
    stringsAsFactors = FALSE
  )
}))

out_ddesc_dir <- file.path(OUT_DIR, "sfa_ddesc")
dir.create(out_ddesc_dir, recursive = TRUE, showWarnings = FALSE)

tsv_ddesc <- file.path(out_ddesc_dir, "comparativa_modelos_sfa_Ddesc.tsv")
utils::write.table(tabla_ddesc, file = tsv_ddesc, sep = "\t",
                   row.names = FALSE, quote = TRUE, na = "")

message("Tabla comparativa D_desc guardada en: ", tsv_ddesc)
