config_path <- if (file.exists("scripts/00_config.R")) "scripts/00_config.R" else "00_config.R"
source(config_path)

required_pkgs <- c("dplyr", "readr", "readxl")
missing_pkgs <- required_pkgs[!vapply(required_pkgs, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing_pkgs) > 0) {
  stop("Faltan paquetes: ", paste(missing_pkgs, collapse = ", "), call. = FALSE)
}

library(dplyr)
library(readr)
library(readxl)

message("\n=== Reconstruccion df_sfa desde scripts legacy de tesis ===")

base_input <- file.path(LEGACY_BASE_DIR, "df_final.RData")
if (!file.exists(base_input)) {
  stop("No existe la base legacy principal: ", base_input, call. = FALSE)
}

recon_root <- file.path(INT_DIR, "legacy_tesis_recon")
recon_int <- file.path(recon_root, "int")
recon_out <- file.path(recon_root, "out")
recon_legacy <- file.path(recon_root, "legacy")
dir.create(recon_root, recursive = TRUE, showWarnings = FALSE)
dir.create(recon_int, recursive = TRUE, showWarnings = FALSE)
dir.create(recon_out, recursive = TRUE, showWarnings = FALSE)
dir.create(recon_legacy, recursive = TRUE, showWarnings = FALSE)

load(base_input)
if (!exists("df_final")) {
  stop("El archivo base no contiene df_final.", call. = FALSE)
}

df_base <- df_final
save(df_final, file = file.path(recon_legacy, "df_final.RData"))
write.csv(
  data.frame(
    objeto = "df_final_base_legacy",
    n_obs = nrow(df_base),
    n_cols = ncol(df_base),
    n_hospitales = dplyr::n_distinct(df_base$NCODI)
  ),
  file.path(recon_out, "sample_summary_base_legacy.csv"),
  row.names = FALSE
)

run_script_with_overrides <- function(script_path) {
  lines <- readLines(script_path, warn = FALSE, encoding = "UTF-8")
  keep <- !grepl("^config_path <-", lines) & !grepl("^source\\(config_path\\)", lines)
  expr <- paste(lines[keep], collapse = "\n")
  env <- new.env(parent = globalenv())

  source(config_path, local = env)
  env$INT_DIR <- recon_int
  env$OUT_DIR <- recon_out
  env$LEGACY_BASE_DIR <- recon_legacy
  env$DF_FINAL_RDATA_PATH <- file.path(recon_legacy, "df_final.RData")
  env$DF_FINAL_TXT_PATH <- file.path(recon_legacy, "df_final.txt")
  env$DF_FINAL_CON_MIX_TXT_PATH <- file.path(recon_legacy, "df_final_con_mix.txt")

  eval(parse(text = expr), envir = env)
  invisible(env)
}

message("\n--- Ejecutando legacy/_run_bloques_depend.R ---")
run_script_with_overrides(file.path(SCRIPTS_DIR, "legacy", "_run_bloques_depend.R"))

load(file.path(recon_legacy, "df_final.RData"))
df_after_depend <- df_final

cmp <- data.frame(
  NCODI = df_after_depend$NCODI,
  anyo = df_after_depend$anyo,
  D_desc_base = if ("D_desc" %in% names(df_base)) df_base$D_desc else NA_integer_,
  D_desc_legacy = df_after_depend$D_desc,
  D_desc_siae_legacy = df_after_depend$D_desc_siae,
  D_desc_cnh_legacy = df_after_depend$D_desc_cnh,
  stringsAsFactors = FALSE
)
cmp$cambio_D_desc <- cmp$D_desc_base != cmp$D_desc_legacy
cmp$cambio_D_desc[is.na(cmp$D_desc_base) & is.na(cmp$D_desc_legacy)] <- FALSE
write.csv(cmp, file.path(recon_out, "comparacion_D_desc_base_vs_legacy.csv"), row.names = FALSE, na = "")

summary_d <- data.frame(
  metrica = c("n_obs", "n_hospitales", "D_desc_base_validos", "D_desc_legacy_validos", "D_desc_cambiadas"),
  valor = c(
    nrow(df_after_depend),
    dplyr::n_distinct(df_after_depend$NCODI),
    sum(!is.na(cmp$D_desc_base)),
    sum(!is.na(cmp$D_desc_legacy)),
    sum(cmp$cambio_D_desc, na.rm = TRUE)
  )
)
write.csv(summary_d, file.path(recon_out, "resumen_D_desc_legacy.csv"), row.names = FALSE)

message("\n--- Ejecutando 09_crear_df_sfa.R con D_desc legacy ---")
run_script_with_overrides(file.path(SCRIPTS_DIR, "09_crear_df_sfa.R"))

load(file.path(recon_int, "df_sfa.RData"))

df_A <- df_sfa %>%
  mutate(
    anyo = suppressWarnings(as.numeric(anyo)),
    es_agudo = suppressWarnings(as.numeric(es_agudo)),
    altTotal_bruto = suppressWarnings(as.numeric(altTotal_bruto))
  ) %>%
  filter(
    es_agudo == 1,
    !anyo %in% 2020:2022,
    altTotal_bruto >= 200
  )

ccaa_vars <- grep("^d_ccaa_[0-9]+$", names(df_A), value = TRUE)
vars_A <- c(
  "ln_altTotal_pond", "ln_i_diag",
  "ln_L_total_c", "ln_K_camas_c", "ln_K_tech_c",
  "ln_L_total_c2", "ln_K_camas_c2",
  "trend", "trend2",
  "d_cluster2", "d_cluster3", "d_cluster4", "d_cluster5",
  "D_desc", "pct_sns", "desc_pago", "ShareQ", "desc_shareQ",
  "ccaa_cod", ccaa_vars
)
vars_A <- vars_A[vars_A %in% names(df_A)]

df_A_complete <- df_A %>%
  filter(stats::complete.cases(dplyr::across(dplyr::all_of(vars_A)))) %>%
  filter(ln_i_diag > quantile(ln_i_diag, 0.01, na.rm = TRUE))

sample_A_summary <- data.frame(
  escenario = c("A_target_base", "A_target_complete"),
  n_obs = c(nrow(df_A), nrow(df_A_complete)),
  n_hospitales = c(dplyr::n_distinct(df_A$NCODI), dplyr::n_distinct(df_A_complete$NCODI)),
  D_desc_1 = c(sum(df_A$D_desc == 1, na.rm = TRUE), sum(df_A_complete$D_desc == 1, na.rm = TRUE)),
  D_desc_0 = c(sum(df_A$D_desc == 0, na.rm = TRUE), sum(df_A_complete$D_desc == 0, na.rm = TRUE)),
  ccaa_nonmissing = c(sum(!is.na(df_A$ccaa_cod)), sum(!is.na(df_A_complete$ccaa_cod))),
  stringsAsFactors = FALSE
)
write.csv(sample_A_summary, file.path(recon_out, "sample_A_summary_legacy_tesis.csv"), row.names = FALSE)

message("\n=== Reconstruccion legacy_tesis completada ===")
message("df_final reconstruido: ", file.path(recon_legacy, "df_final.RData"))
message("df_sfa reconstruido: ", file.path(recon_int, "df_sfa.RData"))
message("Resumen muestra A: ", file.path(recon_out, "sample_A_summary_legacy_tesis.csv"))
