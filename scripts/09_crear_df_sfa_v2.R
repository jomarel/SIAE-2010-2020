## =============================================================
## Phase 5 — Audit + Build df_sfa + Final verification
## v2:
##   - includes revised hospital-only intensity variables
##   - includes both general and diagnostic technology indices
##   - final verification reports quantity sample and suggested intensity sample
##   - script 10 is NOT modified here
## =============================================================

setwd("G:/Mi unidad/SIAE 2010-2020")
library(dplyr)

CLEAN_DIR <- "data_clean"
load(file.path(CLEAN_DIR, "df_final.RData"))

to_num <- function(x) suppressWarnings(as.numeric(x))

cat("========== Phase 5: Audit + df_sfa ==========\n")
cat("df_final:", nrow(df_final), "x", ncol(df_final), "\n")

## =============================================================
## AUDIT
## =============================================================
cat("\n=== AUDIT ===\n")

SFA_VARS <- c(
  "NCODI","anyo","ccaa_codigo","ccaa_cnh","es_agudo",
  "D_desc","pct_sns","ShareQ",
  "altTotal_bruto","altTotal_pond","ln_altTotal_pond",
  "altQ_bruto","altQ_pond","ln_altQ_pond",
  "altM_bruto","altM_pond","ln_altM_pond",
  "i_diag_hosp_num","i_diag","ln_i_diag","i_simple","ln_i_simple",
  "L_total","L_medico","L_quirur",
  "K_camas","K_tech_index","K_tech_diag",
  "ln_L_total","ln_K_camas","ln_K_tech","ln_K_tech_diag",
  "ln_L_total_c","ln_K_camas_c","ln_K_tech_c","ln_K_tech_diag_c",
  "ln_L_total_c2","ln_K_camas_c2","ln_K_tech_c2","ln_K_tech_diag_c2",
  "trend","trend2",
  "desc_pago","desc_shareQ",
  "grupo_cluster","d_cluster2","d_cluster3","d_cluster4","d_cluster5",
  "grupo_pago","d_Priv_Conc","d_Priv_Merc","Conc_shareQ","Merc_shareQ",
  "peso_grd_final","cod_depend_agrupada","finalidad_cnh",
  "cod_finalidad_agrupada","nombre_hospital"
)

audit_rows <- lapply(SFA_VARS, function(v) {
  if (!(v %in% names(df_final))) {
    return(data.frame(variable=v, pct_NA=100, min_val="", median_val="", max_val="",
                      stringsAsFactors=FALSE))
  }
  x <- to_num(df_final[[v]])
  pna <- round(100 * mean(is.na(x)), 1)
  if (all(is.na(x))) {
    return(data.frame(variable=v, pct_NA=pna, min_val="all_NA", median_val="all_NA", max_val="all_NA",
                      stringsAsFactors=FALSE))
  }
  data.frame(
    variable = v,
    pct_NA = pna,
    min_val = as.character(round(min(x, na.rm = TRUE), 3)),
    median_val = as.character(round(median(x, na.rm = TRUE), 3)),
    max_val = as.character(round(max(x, na.rm = TRUE), 3)),
    stringsAsFactors = FALSE
  )
})

audit_df <- bind_rows(audit_rows)
for (i in seq_len(nrow(audit_df))) {
  r <- audit_df[i, ]
  cat(sprintf("  %-25s NA=%5.1f%%  min=%-10s med=%-10s max=%s\n",
              r$variable, r$pct_NA, r$min_val, r$median_val, r$max_val))
}

write.csv(audit_df, file.path(CLEAN_DIR, "auditoria_variables.csv"),
          row.names = FALSE)
cat("Audit saved to data_clean/auditoria_variables.csv\n")

high_na <- audit_df %>% filter(pct_NA > 50)
if (nrow(high_na) > 0) {
  cat("\n*** WARNING: Variables with >50% NA:\n")
  print(high_na)
}

## =============================================================
## BUILD df_sfa
## =============================================================
cat("\n=== BUILD df_sfa ===\n")

# Construct ccaa_cod (17 CCAA + Ceuta=18 + Melilla=19)
df_final$ccaa_cod <- with(df_final, case_when(
  to_num(ccaa_codigo) %in% 1:16 ~ as.integer(ccaa_codigo),
  to_num(ccaa_codigo) == 21 & ccaa_cnh == "La Rioja"  ~ 17L,
  to_num(ccaa_codigo) == 21 & ccaa_cnh == "Ceuta"     ~ 18L,
  to_num(ccaa_codigo) == 21 & ccaa_cnh == "Melilla"   ~ 19L,
  to_num(ccaa_codigo) == 21 & is.na(ccaa_cnh)         ~ 17L,
  TRUE ~ NA_integer_
))

cat("ccaa_cod: non-NA =", sum(!is.na(df_final$ccaa_cod)),
    "| NA =", sum(is.na(df_final$ccaa_cod)), "\n")

# CCAA dummies (ref: 9 = Cataluña)
for (cc in setdiff(1:17, 9L)) {
  df_final[[paste0("d_ccaa_", cc)]] <- as.integer(
    !is.na(df_final$ccaa_cod) & df_final$ccaa_cod == cc
  )
}

sfa_vars <- c(
  "NCODI","anyo","nombre_hospital","ccaa_cnh","ccaa_cod",
  "es_agudo","cod_finalidad_agrupada","finalidad_cnh",
  "D_desc","pct_sns","ShareQ",
  "altTotal_bruto","altTotal_pond","ln_altTotal_pond",
  "altQ_bruto","altQ_pond","ln_altQ_pond",
  "altM_bruto","altM_pond","ln_altM_pond",
  "i_diag_hosp_num","i_diag","ln_i_diag","i_simple","ln_i_simple",
  "L_total","K_camas","K_tech_index","K_tech_diag",
  "ln_L_total_c","ln_K_camas_c","ln_K_tech_c","ln_K_tech_diag_c",
  "ln_L_total_c2","ln_K_camas_c2","ln_K_tech_c2","ln_K_tech_diag_c2",
  "trend","trend2",
  "desc_pago","desc_shareQ",
  "grupo_cluster","d_cluster2","d_cluster3","d_cluster4","d_cluster5",
  "grupo_pago","d_Priv_Conc","d_Priv_Merc","Conc_shareQ","Merc_shareQ",
  "peso_grd_final",
  paste0("d_ccaa_", setdiff(1:17, 9))
)

vars_present <- intersect(sfa_vars, names(df_final))
vars_missing <- setdiff(sfa_vars, names(df_final))
if (length(vars_missing) > 0) {
  cat("Missing from df_sfa:", paste(vars_missing, collapse = ", "), "\n")
}

df_sfa <- df_final[, vars_present, drop = FALSE]
df_sfa$NCODI <- as.integer(df_sfa$NCODI)

# Suggested sample flags for future estimation
df_sfa$flag_sample_Q <- with(df_sfa,
  es_agudo == 1 &
  !(anyo %in% 2020:2022) &
  altTotal_bruto >= 200 &
  !is.na(D_desc) &
  !is.na(pct_sns) &
  is.finite(ln_altTotal_pond) &
  !is.na(ln_L_total_c) &
  !is.na(ccaa_cod)
)

df_sfa$flag_sample_I_main <- with(df_sfa,
  es_agudo == 1 &
  !(anyo %in% 2020:2022) &
  altTotal_bruto >= 200 &
  !is.na(i_diag_hosp_num) &
  i_diag_hosp_num >= 1000 &
  is.finite(ln_i_diag) &
  !is.na(D_desc) &
  !is.na(pct_sns) &
  !is.na(ln_L_total_c) &
  !is.na(ccaa_cod)
)

df_sfa$flag_sample_I_loose <- with(df_sfa,
  es_agudo == 1 &
  !(anyo %in% 2020:2022) &
  altTotal_bruto >= 200 &
  !is.na(i_diag_hosp_num) &
  i_diag_hosp_num >= 500 &
  is.finite(ln_i_diag) &
  !is.na(D_desc) &
  !is.na(pct_sns) &
  !is.na(ln_L_total_c) &
  !is.na(ccaa_cod)
)

df_sfa$flag_sample_I_strict <- with(df_sfa,
  es_agudo == 1 &
  !(anyo %in% 2020:2022) &
  altTotal_bruto >= 200 &
  !is.na(i_diag_hosp_num) &
  i_diag_hosp_num >= 2500 &
  is.finite(ln_i_diag) &
  !is.na(D_desc) &
  !is.na(pct_sns) &
  !is.na(ln_L_total_c) &
  !is.na(ccaa_cod)
)

cat("df_sfa:", nrow(df_sfa), "x", ncol(df_sfa), "\n")

save(df_sfa, file = file.path(CLEAN_DIR, "df_sfa.RData"))
cat("Saved data_clean/df_sfa.RData\n")

## =============================================================
## Final verification
## =============================================================
cat("\n=== FINAL VERIFICATION ===\n")

df_Q <- df_sfa %>% filter(flag_sample_Q)
cat("Quantity sample (suggested / current script-10 compatible):",
    nrow(df_Q), "obs\n")

q_yr <- df_Q %>%
  group_by(anyo) %>%
  summarise(
    n_obs = n(),
    mean_altT_pond_k = round(mean(altTotal_pond, na.rm = TRUE) / 1000, 1),
    pct_D1 = round(100 * mean(D_desc == 1, na.rm = TRUE), 1),
    .groups = "drop"
  )
print(as.data.frame(q_yr))

df_I <- df_sfa %>% filter(flag_sample_I_main)
cat("\nIntensity sample (suggested, main filter i_diag_hosp_num >= 1000):",
    nrow(df_I), "obs\n")

i_yr <- df_I %>%
  group_by(anyo) %>%
  summarise(
    n_obs = n(),
    median_i_diag = round(median(i_diag, na.rm = TRUE), 2),
    median_i_diag_num = round(median(i_diag_hosp_num, na.rm = TRUE), 1),
    pct_D1 = round(100 * mean(D_desc == 1, na.rm = TRUE), 1),
    .groups = "drop"
  )
print(as.data.frame(i_yr))

cat("\nIntensity sample sizes by filter:\n")
cat("  main   (>=1000):", sum(df_sfa$flag_sample_I_main, na.rm = TRUE), "\n")
cat("  loose  (>= 500):", sum(df_sfa$flag_sample_I_loose, na.rm = TRUE), "\n")
cat("  strict (>=2500):", sum(df_sfa$flag_sample_I_strict, na.rm = TRUE), "\n")

save(df_final, file = file.path(CLEAN_DIR, "df_final.RData"))
cat("\nAll Phase 5 outputs saved.\n")
