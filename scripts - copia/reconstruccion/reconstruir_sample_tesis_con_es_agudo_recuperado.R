options(stringsAsFactors = FALSE)

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(tibble)
})

project_dir <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
out_dir <- file.path(project_dir, "outputs", "audit_Ddesc")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

num <- function(x) suppressWarnings(as.numeric(as.character(x)))
norm_txt <- function(x) {
  y <- as.character(x)
  y <- trimws(y)
  y[y %in% c("", "NA")] <- NA_character_
  y <- iconv(y, from = "", to = "ASCII//TRANSLIT")
  y <- toupper(y)
  y <- gsub("[[:space:]]+", " ", y)
  y
}
nvl_chr <- function(x) ifelse(is.na(x), "", x)

load(file.path(project_dir, "data_intermediate", "backup_like_recon", "int", "df_sfa.RData"))
df_backup <- df_sfa
load(file.path(project_dir, "data_intermediate", "df_sfa.RData"))
df_current <- df_sfa

for (nm in c("df_backup", "df_current")) {
  x <- get(nm)
  for (v in intersect(c("anyo", "es_agudo", "altTotal_bruto", "pct_sns", "D_desc_siae"), names(x))) {
    x[[v]] <- num(x[[v]])
  }
  x$NCODI <- as.character(x$NCODI)
  assign(nm, x)
}

map <- read_csv(file.path(project_dir, "data_intermediate", "ncodi_hospital_map.csv"), show_col_types = FALSE) %>%
  transmute(
    NCODI = as.character(NCODI),
    CODCNH = as.character(CODCNH),
    dependencia_cnh_map = as.character(dependencia_cnh),
    pertenencia_sns_cnh = as.character(pertenencia_sns_cnh)
  )

raw_cnh <- read_csv(
  file.path(project_dir, "data_intermediate", "cnh_raw_compare", "cnh_raw_dependencia_panel_2010_2023.csv"),
  show_col_types = FALSE
) %>%
  transmute(
    anyo = num(anyo),
    CODCNH = as.character(CODCNH),
    cod_dep_cnh_raw = num(cod_dep_cnh_raw),
    dep_desc_raw = as.character(dep_desc_raw)
  ) %>%
  arrange(anyo, CODCNH, desc(!is.na(dep_desc_raw))) %>%
  distinct(anyo, CODCNH, .keep_all = TRUE)

build_sample_texto <- function(df) {
  df %>%
    left_join(map, by = "NCODI") %>%
    left_join(raw_cnh, by = c("anyo", "CODCNH")) %>%
    mutate(
      dep_raw_norm = norm_txt(dep_desc_raw),
      dep_map_norm = norm_txt(dependencia_cnh_map),
      dep_any_norm = coalesce(dep_raw_norm, dep_map_norm),
      cat_cnh_tesis = case_when(
        grepl("PENITEN|INTERNACIONAL|DEFENSA", nvl_chr(dep_any_norm)) ~ "Otros",
        grepl("DIPUT|MUNICIP|CABILDO", nvl_chr(dep_any_norm)) ~ "Diputaciones/ayuntamientos",
        grepl("MATEP|MUTUA", nvl_chr(dep_any_norm)) ~ "MATEPSS",
        grepl("CRUZ ROJA|IGLESIA|ORGANIZACIONES NO GUBERNAMENTALES|CARITAS|AECC", paste(nvl_chr(dep_raw_norm), nvl_chr(dep_map_norm))) ~ "Cruz Roja / iglesia",
        cod_dep_cnh_raw == 22 ~ "Fundaciones publicas auts.",
        grepl("OTROS PUBLICOS|OTRAS ENTIDADES U ORGANISMOS PUBLICOS|OTROS CENTROS O ESTABLECIMIENTOS PUBLICOS DE DEPENDENCIA AUTONOMICA", nvl_chr(dep_any_norm)) ~ "Fundaciones publicas auts.",
        grepl("^PRIVADOS?$|PRIVADO NO BENEFICO", nvl_chr(dep_any_norm)) & pct_sns >= 0.50 ~ "Soc. mercantil con concierto",
        grepl("^PRIVADOS?$|PRIVADO NO BENEFICO", nvl_chr(dep_any_norm)) & pct_sns < 0.50 ~ "Soc. mercantil de mercado",
        grepl("SERVICIO|INSTITUTO CATALAN DE LA SALUD|INGESA|COMUNIDAD AUTONOMA", nvl_chr(dep_any_norm)) ~ "SAS / integrado SNS",
        TRUE ~ NA_character_
      ),
      D_desc_cnh_curada = case_when(
        cat_cnh_tesis %in% c("SAS / integrado SNS", "Diputaciones/ayuntamientos") ~ 0L,
        cat_cnh_tesis %in% c(
          "MATEPSS",
          "Cruz Roja / iglesia",
          "Fundaciones publicas auts.",
          "Soc. mercantil con concierto",
          "Soc. mercantil de mercado"
        ) ~ 1L,
        TRUE ~ NA_integer_
      ),
      D_desc_curada = dplyr::coalesce(D_desc_cnh_curada, D_desc_siae)
    ) %>%
    filter(
      es_agudo == 1,
      !anyo %in% 2020:2022,
      altTotal_bruto >= 200,
      is.finite(ln_altTotal_pond),
      is.finite(ln_i_diag),
      !is.na(ln_L_total_c),
      !is.na(ln_K_camas_c),
      !is.na(ccaa_cod),
      !is.na(D_desc_curada),
      !is.na(pct_sns),
      !is.na(ShareQ)
    )
}

target_pdf <- tribble(
  ~anyo, ~n_target, ~pct_target,
  2010, 463, 47.1,
  2011, 471, 47.3,
  2012, 468, 47.2,
  2013, 455, 47.0,
  2014, 452, 46.9,
  2015, 461, 46.8,
  2016, 458, 47.4,
  2017, 455, 47.9,
  2018, 449, 48.1,
  2019, 444, 47.3,
  2023, 389, 48.7
)

df_recovered <- df_backup %>%
  left_join(
    df_current %>%
      select(NCODI, anyo, es_agudo_current = es_agudo) %>%
      distinct(),
    by = c("NCODI", "anyo")
  ) %>%
  mutate(
    es_agudo_original = es_agudo,
    es_agudo = coalesce(es_agudo, es_agudo_current),
    recovery_rule = case_when(
      !is.na(es_agudo_original) ~ "backup_kept",
      is.na(es_agudo_original) & !is.na(es_agudo_current) ~ "filled_from_current_df_sfa",
      TRUE ~ "still_missing"
    )
  )

sample_original <- build_sample_texto(df_backup)
sample_recovered <- build_sample_texto(df_recovered)

summary_year <- function(df, label) {
  df %>%
    group_by(anyo) %>%
    summarise(
      version = label,
      n_obs = n(),
      n_hospitales = n_distinct(NCODI),
      n_D0 = sum(D_desc_curada == 0L, na.rm = TRUE),
      n_D1 = sum(D_desc_curada == 1L, na.rm = TRUE),
      pct_desc = round(100 * mean(D_desc_curada == 1L, na.rm = TRUE), 3),
      .groups = "drop"
    )
}

comparison <- bind_rows(
  summary_year(sample_original, "backup_original"),
  summary_year(sample_recovered, "backup_es_agudo_recovered")
) %>%
  right_join(target_pdf, by = "anyo") %>%
  mutate(
    diff_n = n_obs - n_target,
    diff_pct = round(pct_desc - pct_target, 3)
  ) %>%
  arrange(version, anyo)

recovery_counts <- df_recovered %>%
  count(anyo, recovery_rule, name = "n_rows") %>%
  arrange(anyo, recovery_rule)

added_rows <- anti_join(
  sample_recovered %>% select(NCODI, anyo),
  sample_original %>% select(NCODI, anyo),
  by = c("NCODI", "anyo")
) %>%
  left_join(
    sample_recovered %>%
      select(NCODI, anyo, nombre_hospital, es_agudo_current, recovery_rule, D_desc_curada, D_desc_siae, cat_cnh_tesis),
    by = c("NCODI", "anyo")
  ) %>%
  arrange(anyo, NCODI)

totals <- tibble(
  version = c("backup_original", "backup_es_agudo_recovered"),
  n_obs = c(nrow(sample_original), nrow(sample_recovered)),
  n_hospitales = c(n_distinct(sample_original$NCODI), n_distinct(sample_recovered$NCODI)),
  pct_desc = c(
    round(100 * mean(sample_original$D_desc_curada == 1L, na.rm = TRUE), 3),
    round(100 * mean(sample_recovered$D_desc_curada == 1L, na.rm = TRUE), 3)
  )
)

write_csv(comparison, file.path(out_dir, "reconstruccion_es_agudo_tesis_vs_pdf.csv"), na = "")
write_csv(recovery_counts, file.path(out_dir, "reconstruccion_es_agudo_fuente_relleno.csv"), na = "")
write_csv(added_rows, file.path(out_dir, "reconstruccion_es_agudo_filas_anadidas.csv"), na = "")
write_csv(totals, file.path(out_dir, "reconstruccion_es_agudo_totales.csv"), na = "")

txt <- c(
  "Reconstruccion de sample_texto con es_agudo recuperado",
  "",
  sprintf("sample_original = %s obs", nrow(sample_original)),
  sprintf("sample_recovered = %s obs", nrow(sample_recovered)),
  sprintf("filas anadidas por recuperar es_agudo = %s", nrow(added_rows)),
  "",
  "Conclusiones:",
  sprintf("- El relleno desde df_sfa actual anade %s filas a la muestra tesis-curada local.", nrow(added_rows)),
  sprintf("- La muestra recuperada queda en %s obs, frente a 4765 del PDF.", nrow(sample_recovered)),
  "- Si la muestra recuperada supera claramente 4765, entonces el problema no es solo la perdida de 2010-2011 en es_agudo; tambien falta otra regla de filtrado o una base distinta.",
  "- Ver CSVs exportados para detalle anual y filas anadidas."
)

writeLines(txt, file.path(out_dir, "reconstruccion_es_agudo_NOTA.txt"), useBytes = TRUE)

message("Reconstruccion es_agudo exportada a: ", out_dir)
