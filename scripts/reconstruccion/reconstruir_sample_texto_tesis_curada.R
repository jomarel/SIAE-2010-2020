options(stringsAsFactors = FALSE)

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
})

project_dir <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
out_dir <- file.path(project_dir, "outputs", "audit_Ddesc")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

load(file.path(project_dir, "data_intermediate", "backup_like_recon", "int", "df_sfa.RData"))

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
    dep_desc_raw = as.character(dep_desc_raw),
    fuente_cnh_raw = as.character(fuente_cnh_raw)
  ) %>%
  arrange(anyo, CODCNH, desc(!is.na(dep_desc_raw))) %>%
  distinct(anyo, CODCNH, .keep_all = TRUE)

df <- df_sfa
for (v in intersect(c("anyo", "es_agudo", "altTotal_bruto", "pct_sns"), names(df))) {
  df[[v]] <- num(df[[v]])
}
df$NCODI <- as.character(df$NCODI)

df <- df %>%
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
        "MATEPSS", "Cruz Roja / iglesia",
        "Fundaciones publicas auts.",
        "Soc. mercantil con concierto",
        "Soc. mercantil de mercado"
      ) ~ 1L,
      TRUE ~ NA_integer_
    ),
    D_desc_curada = dplyr::coalesce(D_desc_cnh_curada, D_desc_siae)
  )

sample_texto <- df %>%
  filter(
    es_agudo == 1,
    !anyo %in% 2020:2022,
    altTotal_bruto >= 200,
    !is.na(ln_altTotal_pond), is.finite(ln_altTotal_pond),
    !is.na(ln_i_diag), is.finite(ln_i_diag),
    !is.na(ln_L_total_c), !is.na(ln_K_camas_c),
    !is.na(ccaa_cod),
    !is.na(D_desc_curada),
    !is.na(pct_sns),
    !is.na(ShareQ)
  )

target <- tibble::tribble(
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

summary_year <- sample_texto %>%
  group_by(anyo) %>%
  summarise(
    n_obs = n(),
    n_hospitales = n_distinct(NCODI),
    n_D0 = sum(D_desc_curada == 0L, na.rm = TRUE),
    n_D1 = sum(D_desc_curada == 1L, na.rm = TRUE),
    pct_desc = round(100 * mean(D_desc_curada == 1L, na.rm = TRUE), 3),
    .groups = "drop"
  ) %>%
  right_join(target, by = "anyo") %>%
  mutate(
    diff_n = n_obs - n_target,
    diff_pct = round(pct_desc - pct_target, 3)
  )

summary_cat <- sample_texto %>%
  count(cat_cnh_tesis, sort = TRUE, name = "n_obs") %>%
  mutate(pct = round(100 * n_obs / sum(n_obs), 3))

write_csv(
  sample_texto %>%
    select(
      NCODI, anyo, nombre_hospital,
      D_desc_curada, D_desc_siae, D_desc_cnh, CODCNH,
      pct_sns, cat_cnh_tesis, cod_dep_cnh_raw,
      dep_desc_raw, dependencia_cnh_map, pertenencia_sns_cnh
    ),
  file.path(out_dir, "sample_texto_reconstruida_hospital_anyo.csv"),
  na = ""
)

write_csv(summary_year, file.path(out_dir, "sample_texto_reconstruida_vs_pdf_por_anyo.csv"), na = "")
write_csv(summary_cat, file.path(out_dir, "sample_texto_reconstruida_categoria_cnh.csv"), na = "")

message("sample_texto reconstruida: ", nrow(sample_texto), " obs")
