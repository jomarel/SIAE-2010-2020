# ============================================================
# 11_estimar_sfa_panel.R - Estimacion panel de Diseños D, A, B y C
# Usa sfa::psfm con NCODI como unidad panel.
# Mantiene la especificacion principal de los scripts definitivos.
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
    altTotal_bruto > 100,
    !is.na(grupo_pago),
    !is.na(ln_L_total_c), !is.na(ln_K_camas_c),
    !is.na(ccaa_cod)
  ) %>%
  arrange(NCODI, anyo)

message(
  "Muestra base panel: ", nrow(df_base),
  " obs | ", dplyr::n_distinct(df_base$NCODI), " hospitales"
)

frontier_rhs <- paste(
  "ln_L_total_c + ln_K_camas_c + ln_K_tech_c +",
  "ln_L_total_c2 + ln_K_camas_c2 + trend + trend2"
)
z_gran_rhs <- "d_Priv_Conc + d_Priv_Merc + ShareQ + Conc_shareQ + Merc_shareQ + factor(ccaa_cod)"
z_gran <- c("d_Priv_Conc", "d_Priv_Merc", "ShareQ", "Conc_shareQ", "Merc_shareQ")

# Modelo D con panel
df_D <- df_base %>%
  filter(
    altQ_bruto >= 200,
    altM_bruto >= 200,
    is.finite(ln_altQ_pond),
    is.finite(ln_altM_pond)
  ) %>%
  mutate(
    neg_ln_altQ = -ln_altQ_pond,
    ln_ratio_MQ = ln_altM_pond - ln_altQ_pond
  ) %>%
  arrange(NCODI, anyo)

message(
  "Muestra D/B panel: ", nrow(df_D),
  " obs | ", dplyr::n_distinct(df_D$NCODI), " hospitales"
)

message("\n=== Modelo D panel ===")
front_D <- as.formula(paste(
  "neg_ln_altQ ~ ln_ratio_MQ + I(ln_ratio_MQ^2) +",
  "I(ln_ratio_MQ * ln_L_total_c) + I(ln_ratio_MQ * ln_K_camas_c) +",
  frontier_rhs,
  "|",
  z_gran_rhs
))
fit_D <- panel_fit_psfm(front_D, df_D, "D_panel")
saveRDS(fit_D, file.path(INT_DIR, "panel_models", "sfa_D_panel.rds"))

# Modelo B con panel
message("\n=== Modelo B panel ===")
front_Bq <- as.formula(
  paste("ln_altQ_pond ~", frontier_rhs, "|", z_gran_rhs)
)
front_Bm <- as.formula(
  paste("ln_altM_pond ~", frontier_rhs, "|", z_gran_rhs)
)
fit_Bq <- panel_fit_psfm(front_Bq, df_D, "B_panel_Q")
fit_Bm <- panel_fit_psfm(front_Bm, df_D, "B_panel_M")
saveRDS(fit_Bq, file.path(INT_DIR, "panel_models", "sfa_B_panel_Q.rds"))
saveRDS(fit_Bm, file.path(INT_DIR, "panel_models", "sfa_B_panel_M.rds"))

# Modelo A granular con panel
df_A <- df_base %>%
  filter(
    altTotal_bruto >= 200,
    is.finite(ln_altTotal_pond),
    is.finite(ln_i_diag)
  ) %>%
  filter(ln_i_diag > quantile(ln_i_diag, 0.01, na.rm = TRUE)) %>%
  arrange(NCODI, anyo)

message(
  "Muestra A panel: ", nrow(df_A),
  " obs | ", dplyr::n_distinct(df_A$NCODI), " hospitales"
)

message("\n=== Modelo A panel (granular) ===")
front_A_total <- as.formula(
  paste("ln_altTotal_pond ~", frontier_rhs, "|", z_gran_rhs)
)
front_A_intens <- as.formula(
  paste("ln_i_diag ~", frontier_rhs, "|", z_gran_rhs)
)
fit_A_total <- panel_fit_psfm(front_A_total, df_A, "A_panel_Total_granular")
fit_A_intens <- panel_fit_psfm(front_A_intens, df_A, "A_panel_I_granular")
saveRDS(fit_A_total, file.path(INT_DIR, "panel_models", "sfa_A_panel_Total_granular.rds"))
saveRDS(fit_A_intens, file.path(INT_DIR, "panel_models", "sfa_A_panel_I_granular.rds"))

# Modelo C con panel hospital-servicio
message("\n=== Modelo C panel ===")
df_Q <- df_D %>%
  mutate(
    servicio = "Q",
    C_s = 1L,
    ln_output = ln_altQ_pond,
    ln_L_s = ln_L_quirur
  ) %>%
  filter(is.finite(ln_output), !is.na(ln_L_s))

df_M <- df_D %>%
  mutate(
    servicio = "M",
    C_s = 0L,
    ln_output = ln_altM_pond,
    ln_L_s = ln_L_medico
  ) %>%
  filter(is.finite(ln_output), !is.na(ln_L_s))

df_C <- bind_rows(df_Q, df_M) %>%
  mutate(
    id_panel = paste(NCODI, servicio, sep = "__"),
    ln_L_s_c = ln_L_s - mean(ln_L_s, na.rm = TRUE),
    ln_L_s_c2 = 0.5 * ln_L_s_c^2,
    Conc_Cs = d_Priv_Conc * C_s,
    Merc_Cs = d_Priv_Merc * C_s,
    ShareQ_Cs = ShareQ * C_s
  ) %>%
  filter(!is.na(ln_L_s_c)) %>%
  arrange(id_panel, anyo)

message(
  "Muestra C panel: ", nrow(df_C),
  " obs | ", dplyr::n_distinct(df_C$id_panel), " unidades hospital-servicio"
)

z_C_rhs <- "d_Priv_Conc + d_Priv_Merc + C_s + Conc_Cs + Merc_Cs + ShareQ + ShareQ_Cs + factor(ccaa_cod)"
z_C <- c("d_Priv_Conc", "d_Priv_Merc", "C_s", "Conc_Cs", "Merc_Cs", "ShareQ", "ShareQ_Cs")
front_C <- as.formula(paste(
  "ln_output ~ ln_L_s_c + ln_K_camas_c + ln_K_tech_c +",
  "ln_L_s_c2 + ln_K_camas_c2 + I(ln_L_s_c * ln_K_camas_c) +",
  "C_s + trend + trend2",
  "|",
  z_C_rhs
))
fit_C <- panel_fit_psfm(front_C, df_C, "C_panel", individual = "id_panel")
saveRDS(fit_C, file.path(INT_DIR, "panel_models", "sfa_C_panel.rds"))

exported <- panel_export_bundle(
  fits = list(
    D_panel = fit_D,
    B_panel_Q = fit_Bq,
    B_panel_M = fit_Bm,
    A_panel_Total_granular = fit_A_total,
    A_panel_I_granular = fit_A_intens,
    C_panel = fit_C
  ),
  z_map = list(
    D_panel = z_gran,
    B_panel_Q = z_gran,
    B_panel_M = z_gran,
    A_panel_Total_granular = z_gran,
    A_panel_I_granular = z_gran,
    C_panel = z_C
  ),
  out_prefix = file.path(OUT_DIR, "sfa_panel", "DBAC_panel")
)

print(exported$stats)
message("Salidas panel D/B/A/C guardadas en: ", file.path(OUT_DIR, "sfa_panel"))
