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

message("\n=== Analisis legacy_tesis: Cuadro 5.3 + A/A1 ===")

recon_root <- file.path(INT_DIR, "legacy_tesis_recon")
recon_int <- file.path(recon_root, "int")
recon_out <- file.path(recon_root, "out")
df_path <- file.path(recon_int, "df_sfa.RData")
if (!file.exists(df_path)) {
  stop("No existe df_sfa legacy_tesis: ", df_path, call. = FALSE)
}

load(df_path)

df_A <- df_sfa %>%
  mutate(
    anyo = suppressWarnings(as.numeric(anyo)),
    es_agudo = suppressWarnings(as.numeric(es_agudo)),
    altTotal_bruto = suppressWarnings(as.numeric(altTotal_bruto)),
    pct_sns = suppressWarnings(as.numeric(pct_sns))
  ) %>%
  filter(
    es_agudo == 1,
    !anyo %in% 2020:2022,
    altTotal_bruto >= 200,
    !is.na(ln_altTotal_pond), is.finite(ln_altTotal_pond),
    !is.na(ln_i_diag), is.finite(ln_i_diag),
    !is.na(ln_L_total_c),
    !is.na(ln_K_camas_c),
    !is.na(ccaa_cod),
    !is.na(D_desc),
    !is.na(pct_sns),
    !is.na(ShareQ)
  ) %>%
  filter(ln_i_diag > quantile(ln_i_diag, 0.01, na.rm = TRUE))

message("Muestra A legacy_tesis: ", nrow(df_A), " obs | ",
        dplyr::n_distinct(df_A$NCODI), " hospitales")

cnh <- read_excel(
  file.path(DOCS_DIR, "NCODI_Hospital_DEFINITIVO_2020_2024.xlsx"),
  sheet = "\U0001f5c2 Correspondencia",
  col_types = "text"
)
names(cnh) <- make.names(names(cnh))
cnh$NCODI <- as.character(cnh$NCODI)
df_A$NCODI <- as.character(df_A$NCODI)

df_cmp <- dplyr::left_join(
  df_A,
  cnh %>%
    dplyr::select(
      NCODI,
      Dependencia.Funcional,
      Pertenencia.SNS,
      CCAA,
      Nombre.Hospital..mejor.disponible.
    ),
  by = "NCODI"
)

dep <- df_cmp$Dependencia.Funcional
df_cmp$cat_tesis <- dplyr::case_when(
  dep %in% c(
    "Servicios e Institutos de Salud de Las Comunidades Autónomas",
    "Instituto de Gestión Sanitaria-Ingesa",
    "Ministerio de Defensa",
    "Otros Centros o Establecimientos Públicos de Dependencia Estatal"
  ) ~ "SAS / integrado SNS",
  dep %in% c("Diputación o Cabildo", "Municipio") ~ "Diputaciones/ayuntamientos",
  dep == "Mútuas Colaboradoras con La Seguridad Social" ~ "MATEPSS",
  dep == "Organizaciones No Gubernamentales" ~ "Cruz Roja / iglesia",
  dep %in% c(
    "Otras Entidades u o rganismos Públicos",
    "Otros Centros o Establecimientos Públicos de Dependencia Autonómica"
  ) ~ "Fundaciones públicas auts.",
  dep == "Privados" & df_cmp$pct_sns >= 0.50 ~ "Soc. mercantil con concierto",
  dep == "Privados" & df_cmp$pct_sns < 0.50 ~ "Soc. mercantil de mercado",
  TRUE ~ "Sin clasificar"
)

cat_order <- c(
  "SAS / integrado SNS",
  "Diputaciones/ayuntamientos",
  "MATEPSS",
  "Cruz Roja / iglesia",
  "Fundaciones públicas auts.",
  "Soc. mercantil con concierto",
  "Soc. mercantil de mercado",
  "Sin clasificar"
)

cuadro53 <- df_cmp %>%
  count(cat_tesis, name = "N_obs") %>%
  mutate(
    pct_muestra = round(100 * N_obs / sum(N_obs), 1),
    Ddesc = dplyr::case_when(
      cat_tesis %in% c("SAS / integrado SNS", "Diputaciones/ayuntamientos") ~ "0",
      cat_tesis == "Fundaciones públicas auts." ~ "1 (A) / 0 (R2b)",
      cat_tesis == "Sin clasificar" ~ "NA",
      TRUE ~ "1"
    ),
    Robustez = dplyr::case_when(
      cat_tesis %in% c("SAS / integrado SNS", "Diputaciones/ayuntamientos",
                       "Soc. mercantil con concierto", "Soc. mercantil de mercado") ~ "Estable",
      cat_tesis == "MATEPSS" ~ "R2b: excluye",
      cat_tesis == "Cruz Roja / iglesia" ~ "R5: excluye",
      cat_tesis == "Fundaciones públicas auts." ~ "Ambiguo",
      TRUE ~ "Revisar"
    )
  ) %>%
  arrange(match(cat_tesis, cat_order))

write.csv(cuadro53,
          file.path(recon_out, "cuadro53_comparacion_legacy_tesis.csv"),
          row.names = FALSE, na = "")

ddesc_summary <- data.frame(
  metrica = c("n_obs_A", "n_hospitales_A", "D_desc_1_obs", "D_desc_0_obs", "D_desc_1_pct"),
  valor = c(
    nrow(df_A),
    dplyr::n_distinct(df_A$NCODI),
    sum(df_A$D_desc == 1, na.rm = TRUE),
    sum(df_A$D_desc == 0, na.rm = TRUE),
    round(100 * mean(df_A$D_desc == 1, na.rm = TRUE), 1)
  )
)
write.csv(ddesc_summary,
          file.path(recon_out, "D_desc_resumen_muestra_A_legacy_tesis.csv"),
          row.names = FALSE)

run_script_with_overrides <- function(script_path) {
  lines <- readLines(script_path, warn = FALSE, encoding = "UTF-8")
  keep <- !grepl("^config_path <-", lines) & !grepl("^source\\(config_path\\)", lines)
  expr <- paste(lines[keep], collapse = "\n")
  env <- new.env(parent = globalenv())

  source(config_path, local = env)
  env$INT_DIR <- recon_int
  env$OUT_DIR <- recon_out
  env$LEGACY_BASE_DIR <- file.path(recon_root, "legacy")
  eval(parse(text = expr), envir = env)
  invisible(env)
}

message("\n--- Ejecutando 11_estimar_sfa_A_A1.R sobre legacy_tesis_recon ---")
run_script_with_overrides(file.path(SCRIPTS_DIR, "11_estimar_sfa_A_A1.R"))

message("\n=== Analisis legacy_tesis completado ===")
message("Cuadro 5.3 comparativo: ", file.path(recon_out, "cuadro53_comparacion_legacy_tesis.csv"))
message("Resumen D_desc A: ", file.path(recon_out, "D_desc_resumen_muestra_A_legacy_tesis.csv"))
