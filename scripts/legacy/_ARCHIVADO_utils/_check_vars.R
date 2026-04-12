source("scripts/00_config.R")
load(file.path(INT_DIR, "df_sfa.RData"))

vars_check <- c(
  "altQ_pond","altM_pond","altTotal_pond",
  "ln_altQ_pond","ln_altM_pond","ln_altTotal_pond",
  "i_diag","ln_i_diag","i_simple","ln_i_simple",
  "ShareQ","peso_grd_final",
  "L_total","L_medico","L_quirur",
  "K_camas","K_tech_index",
  "ln_L_total_c","ln_K_camas_c","ln_K_tech_c",
  "ln_L_total_c2","ln_K_camas_c2","ln_LK_c",
  "trend","trend2",
  "D_desc","D_desc_siae","D_desc_cnh",
  "pct_sns","pct_privado","pct_ingr_SNS",
  "desc_pago","desc_shareQ",
  "grupo_pago","d_Priv_Conc","d_Priv_Merc",
  "Conc_shareQ","Merc_shareQ",
  "grupo_cluster","d_cluster2","d_cluster3",
  "d_cluster4","d_cluster5",
  "es_agudo","cod_finalidad_agrupada",
  "ccaa_cnh","ccaa_cod",
  "d_ccaa_1","d_ccaa_8","d_ccaa_13","d_ccaa_16",
  "covid","covid_fuerte","covid_leve",
  "camas_cirugia","camas_medicina",
  "NCODI","anyo","nombre_hospital"
)

cat(sprintf("df_sfa: %d obs x %d variables\n\n", nrow(df_sfa), ncol(df_sfa)))
cat(sprintf("%-30s %-6s %s\n", "Variable", "Estado", "pct_NA"))
cat(paste(rep("-", 50), collapse=""), "\n")

n_ok    <- 0L
n_falta <- 0L
faltan  <- character(0)

for (v in vars_check) {
  if (v %in% names(df_sfa)) {
    pct <- round(100 * mean(is.na(df_sfa[[v]])), 1)
    cat(sprintf("  %-28s OK     %.1f%%\n", v, pct))
    n_ok <- n_ok + 1L
  } else {
    cat(sprintf("  %-28s FALTA  ---\n", v))
    n_falta <- n_falta + 1L
    faltan  <- c(faltan, v)
  }
}

cat("\n")
cat(sprintf("Variables OK:    %d\n", n_ok))
cat(sprintf("Variables FALTA: %d\n", n_falta))

if (n_falta == 0L) {
  message("PIPELINE REPRODUCIBLE — listo para script 11")
} else {
  message("FALTAN ", n_falta, " variables — revisar:")
  for (v in faltan) message("  - ", v)
}
