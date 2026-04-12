options(stringsAsFactors = FALSE)

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
})

project_dir <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
out_dir <- file.path(project_dir, "outputs", "audit_Ddesc")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

load(file.path(project_dir, "data_intermediate", "df_sfa.RData"))
df_sfa_actual <- df_sfa
load(file.path(project_dir, "data_intermediate", "legacy_tesis_recon", "int", "df_sfa.RData"))
df_sfa_legacy <- df_sfa

summ <- function(dat, dvar, label) {
  x <- dat[[dvar]]
  tibble(
    sample = label,
    n_obs = nrow(dat),
    n_hospitales = dplyr::n_distinct(dat$NCODI),
    n_D0 = sum(x == 0, na.rm = TRUE),
    n_D1 = sum(x == 1, na.rm = TRUE),
    n_NA = sum(is.na(x)),
    pct_priv = 100 * mean(x == 1, na.rm = TRUE)
  )
}

cur_A <- df_sfa_actual %>%
  filter(
    es_agudo == 1,
    !anyo %in% 2020:2022,
    altTotal_bruto > 100,
    !is.na(D_desc), !is.na(pct_sns), !is.na(ShareQ), !is.na(grupo_pago),
    !is.na(ln_L_total_c), !is.na(ln_K_camas_c), !is.na(ln_K_tech_c),
    !is.na(ccaa_cod), !is.na(ln_i_diag), is.finite(ln_i_diag),
    !is.na(ln_altTotal_pond), is.finite(ln_altTotal_pond)
  )

tesis_filters_current <- df_sfa_actual %>%
  filter(
    es_agudo == 1,
    !anyo %in% 2020:2022,
    !is.na(D_desc_tesis), !is.na(pct_sns), !is.na(ShareQ),
    !is.na(ln_L_total_c), !is.na(ln_K_camas_c), !is.na(ccaa_cod)
  ) %>%
  filter(
    altTotal_bruto >= 200,
    is.finite(ln_altTotal_pond),
    is.finite(ln_i_diag),
    ln_i_diag > quantile(ln_i_diag, 0.01, na.rm = TRUE)
  )

tesis_filters_current_D <- df_sfa_actual %>%
  filter(
    es_agudo == 1,
    !anyo %in% 2020:2022,
    !is.na(D_desc), !is.na(pct_sns), !is.na(ShareQ),
    !is.na(ln_L_total_c), !is.na(ln_K_camas_c), !is.na(ccaa_cod)
  ) %>%
  filter(
    altTotal_bruto >= 200,
    is.finite(ln_altTotal_pond),
    is.finite(ln_i_diag),
    ln_i_diag > quantile(ln_i_diag, 0.01, na.rm = TRUE)
  )

tesis_filters_legacy <- df_sfa_legacy %>%
  filter(
    es_agudo == 1,
    !anyo %in% 2020:2022,
    !is.na(D_desc), !is.na(pct_sns), !is.na(ShareQ),
    !is.na(ln_L_total_c), !is.na(ln_K_camas_c), !is.na(ccaa_cod)
  ) %>%
  filter(
    altTotal_bruto >= 200,
    is.finite(ln_altTotal_pond),
    is.finite(ln_i_diag),
    ln_i_diag > quantile(ln_i_diag, 0.01, na.rm = TRUE)
  )

resumen <- bind_rows(
  summ(df_sfa_actual, "D_desc", "df_sfa_actual_total_D_desc"),
  summ(df_sfa_actual, "D_desc_tesis", "df_sfa_actual_total_D_desc_tesis"),
  summ(cur_A, "D_desc", "current_script_design_A"),
  summ(tesis_filters_current_D, "D_desc", "current_df_tesis_filters_but_D_desc"),
  summ(tesis_filters_current, "D_desc_tesis", "current_df_tesis_filters_D_desc_tesis"),
  summ(df_sfa_legacy, "D_desc", "legacy_recon_total_D_desc"),
  summ(tesis_filters_legacy, "D_desc", "legacy_recon_tesis_filters_D_desc")
)

write_csv(resumen, file.path(out_dir, "investigacion_df_sfa_vs_tesis_resumen.csv"), na = "")

writeLines(
  c(
    "Investigacion df_sfa final vs texto/ramas de tesis",
    "",
    "Hallazgos clave:",
    "1. df_sfa actual total no es comparable con la muestra de tesis: tiene 10614 hospital-anio.",
    "2. La muestra de tesis debe compararse tras filtros de estimacion, no contra el df_sfa completo.",
    "3. Con filtros de tesis sobre el df_sfa actual, la muestra queda en 5474 obs y 643 hospitales.",
    "4. Con esos mismos filtros, el df_sfa actual da 61.363% privados; la rama legacy_tesis reconstruida da 48.794% privados.",
    "5. Por tanto, la principal divergencia no viene del tamaño muestral en esa rama, sino de la construccion de D_desc.",
    "6. En el pipeline actual D_desc y D_desc_tesis son practicamente iguales en la muestra filtrada; ambos quedan lejos de la rama legacy_tesis.",
    "7. El script actual 11_estimar_sfa.R ademas usa para Design A una muestra mas amplia (5590 obs) porque parte de altTotal_bruto > 100 y no aplica el corte p1 de ln_i_diag antes del conteo inicial de muestra."
  ),
  file.path(out_dir, "investigacion_df_sfa_vs_tesis.txt"),
  useBytes = TRUE
)

message("Investigacion guardada en: ", out_dir)
