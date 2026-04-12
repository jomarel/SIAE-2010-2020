# ============================================================
# 11_estimar_sfa_A_A1_panel.R - Estimacion panel de Diseños A y A1
# Usa sfa::psfm con NCODI como unidad panel.
# Mantiene cluster en frontera y CCAA en ineficiencia.
# ============================================================

config_path <- if (file.exists("scripts/00_config.R")) {
  "scripts/00_config.R"
} else {
  "00_config.R"
}
source(config_path)
source(file.path("scripts", "utils", "panel_sfa_helpers.R"))

if (!requireNamespace("sfa", quietly = TRUE)) {
  stop("Instala sfa: install.packages('sfa')")
}
if (!requireNamespace("dplyr", quietly = TRUE)) {
  stop("Instala dplyr: install.packages('dplyr')")
}

library(sfa)
library(dplyr)

dir.create(file.path(INT_DIR, "panel_models"), showWarnings = FALSE, recursive = TRUE)
dir.create(file.path(OUT_DIR, "sfa_panel"), showWarnings = FALSE, recursive = TRUE)

load(file.path(INT_DIR, "df_sfa.RData"))
message("df_sfa: ", nrow(df_sfa), " x ", ncol(df_sfa))

df_base <- df_sfa %>%
  filter(
    es_agudo == 1,
    !anyo %in% 2020:2022,
    altTotal_bruto >= 200,
    !is.na(D_desc), !is.na(pct_sns), !is.na(ShareQ),
    !is.na(ln_L_total_c), !is.na(ln_K_camas_c),
    !is.na(ccaa_cod),
    is.finite(ln_altTotal_pond),
    is.finite(ln_i_diag)
  ) %>%
  filter(ln_i_diag > quantile(ln_i_diag, 0.01, na.rm = TRUE)) %>%
  arrange(NCODI, anyo)

message(
  "Muestra panel A/A1: ", nrow(df_base),
  " obs | ", dplyr::n_distinct(df_base$NCODI), " hospitales"
)

frontier_rhs <- paste(
  "ln_L_total_c + ln_K_camas_c + ln_K_tech_c +",
  "ln_L_total_c2 + ln_K_camas_c2 + trend + trend2"
)

z_A_rhs <- "D_desc + pct_sns + desc_pago + ShareQ + desc_shareQ + factor(ccaa_cod)"
z_A1_rhs <- "D_desc + pct_sns + desc_pago + factor(ccaa_cod)"
z_A <- c("D_desc", "pct_sns", "desc_pago", "ShareQ", "desc_shareQ")
z_A1 <- c("D_desc", "pct_sns", "desc_pago")

# Modelo A con D_desc
message("\n=== Modelo A panel con D_desc ===")
form_A_total <- as.formula(
  paste("ln_altTotal_pond ~", frontier_rhs, "|", z_A_rhs)
)
form_A_intens <- as.formula(
  paste("ln_i_diag ~", frontier_rhs, "|", z_A_rhs)
)

fit_A_total <- panel_fit_psfm(form_A_total, df_base, "A_panel_Total")
fit_A_intens <- panel_fit_psfm(form_A_intens, df_base, "A_panel_I")

saveRDS(fit_A_total, file.path(INT_DIR, "panel_models", "sfa_A_panel_Total.rds"))
saveRDS(fit_A_intens, file.path(INT_DIR, "panel_models", "sfa_A_panel_I.rds"))

# Modelo A1 con D_desc
message("\n=== Modelo A1 panel con D_desc ===")
form_A1_total <- as.formula(
  paste("ln_altTotal_pond ~", frontier_rhs, "|", z_A1_rhs)
)
form_A1_intens <- as.formula(
  paste("ln_i_diag ~", frontier_rhs, "|", z_A1_rhs)
)

fit_A1_total <- panel_fit_psfm(form_A1_total, df_base, "A1_panel_Total")
fit_A1_intens <- panel_fit_psfm(form_A1_intens, df_base, "A1_panel_I")

saveRDS(fit_A1_total, file.path(INT_DIR, "panel_models", "sfa_A1_panel_Total.rds"))
saveRDS(fit_A1_intens, file.path(INT_DIR, "panel_models", "sfa_A1_panel_I.rds"))

exported <- panel_export_bundle(
  fits = list(
    A_panel_Total = fit_A_total,
    A_panel_I = fit_A_intens,
    A1_panel_Total = fit_A1_total,
    A1_panel_I = fit_A1_intens
  ),
  z_map = list(
    A_panel_Total = z_A,
    A_panel_I = z_A,
    A1_panel_Total = z_A1,
    A1_panel_I = z_A1
  ),
  out_prefix = file.path(OUT_DIR, "sfa_panel", "A_A1_panel")
)

print(exported$stats)
message("Salidas panel A/A1 guardadas en: ", file.path(OUT_DIR, "sfa_panel"))
