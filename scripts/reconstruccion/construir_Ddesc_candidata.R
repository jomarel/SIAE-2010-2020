options(stringsAsFactors = FALSE)

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
})

project_dir <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
out_dir <- file.path(project_dir, "outputs", "audit_Ddesc")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

paths <- list(
  df_final_rdata = file.path(project_dir, "data_legacy_outputs", "df_final.RData"),
  df_sfa_rdata = file.path(project_dir, "data_intermediate", "df_sfa.RData"),
  ncodi_map = file.path(project_dir, "data_intermediate", "ncodi_hospital_map.csv")
)

to_chr <- function(x) {
  x <- as.character(x)
  x <- trimws(x)
  x[x %in% c("", "NA", "NULL")] <- NA_character_
  x
}

to_int <- function(x) suppressWarnings(as.integer(as.character(x)))

norm_ascii <- function(x) {
  x <- to_chr(x)
  x <- iconv(x, from = "", to = "ASCII//TRANSLIT")
  x <- toupper(x)
  x
}

pct_private <- function(x) {
  n0 <- sum(x == 0L, na.rm = TRUE)
  n1 <- sum(x == 1L, na.rm = TRUE)
  if ((n0 + n1) == 0) return(NA_real_)
  100 * n1 / (n0 + n1)
}

load_df <- function(path, preferred) {
  env <- new.env(parent = emptyenv())
  load(path, envir = env)
  get(preferred, envir = env)
}

df_final <- load_df(paths$df_final_rdata, "df_final")
df_sfa <- load_df(paths$df_sfa_rdata, "df_sfa")
map <- read_csv(paths$ncodi_map, show_col_types = FALSE)

df_final <- df_final %>%
  transmute(
    NCODI = to_int(NCODI),
    anyo = to_int(anyo),
    nombre_hospital = to_chr(nombre_hospital),
    cod_depend_agrupada = to_int(cod_depend_agrupada),
    Depend_agrupada = to_chr(Depend_agrupada),
    D_desc_actual_df_final = if ("D_desc" %in% names(df_final)) to_int(D_desc) else NA_integer_,
    D_desc_tesis_df_final = if ("D_desc_tesis" %in% names(df_final)) to_int(D_desc_tesis) else NA_integer_
  )

map <- map %>%
  transmute(
    NCODI = to_int(NCODI),
    CODCNH = if ("CODCNH" %in% names(map)) to_chr(CODCNH) else NA_character_,
    pertenencia_sns_cnh = to_chr(pertenencia_sns_cnh),
    dependencia_cnh = if ("dependencia_cnh" %in% names(map)) to_chr(dependencia_cnh) else NA_character_,
    D_desc_cnh_simple = case_when(
      norm_ascii(pertenencia_sns_cnh) == "PUBLICOS-SNS" ~ 0L,
      norm_ascii(pertenencia_sns_cnh) == "PRIVADOS" ~ 1L,
      TRUE ~ NA_integer_
    )
  ) %>%
  distinct(NCODI, .keep_all = TRUE)

candidate <- df_final %>%
  left_join(map, by = "NCODI") %>%
  mutate(
    D_desc_siae_recomendada = case_when(
      cod_depend_agrupada == 1L ~ 0L,
      cod_depend_agrupada == 2L ~ 1L,
      TRUE ~ NA_integer_
    ),
    fuente_recomendada = case_when(
      cod_depend_agrupada %in% c(1L, 2L) ~ "SIAE_cod_depend_agrupada",
      TRUE ~ "Sin clasificacion SIAE segura"
    ),
    D_desc_candidata = D_desc_siae_recomendada,
    validacion_cnh = case_when(
      is.na(D_desc_siae_recomendada) & is.na(D_desc_cnh_simple) ~ "Sin SIAE y sin CNH",
      !is.na(D_desc_siae_recomendada) & is.na(D_desc_cnh_simple) ~ "Sin CNH comparable",
      is.na(D_desc_siae_recomendada) & !is.na(D_desc_cnh_simple) ~ "Sin SIAE; solo CNH",
      D_desc_siae_recomendada == D_desc_cnh_simple ~ "SIAE y CNH coinciden",
      TRUE ~ "SIAE y CNH discrepan"
    )
  ) %>%
  arrange(anyo, NCODI)

candidate_sfa_view <- df_sfa %>%
  transmute(
    NCODI = to_int(NCODI),
    anyo = to_int(anyo),
    D_desc_actual_df_sfa = to_int(D_desc)
  ) %>%
  right_join(candidate, by = c("NCODI", "anyo")) %>%
  mutate(
    en_df_sfa_actual = !is.na(D_desc_actual_df_sfa),
    cambio_vs_df_sfa = case_when(
      !en_df_sfa_actual ~ "No esta en df_sfa actual",
      is.na(D_desc_actual_df_sfa) & is.na(D_desc_candidata) ~ "Sin cambio",
      is.na(D_desc_actual_df_sfa) != is.na(D_desc_candidata) ~ "Cambio NA vs valor",
      D_desc_actual_df_sfa != D_desc_candidata ~ "Cambio 0/1",
      TRUE ~ "Sin cambio"
    )
  )

resumen_candidata <- bind_rows(
  candidate_sfa_view %>%
    group_by(anyo) %>%
    summarise(
      version = "D_desc_candidata_siae",
      n_total = n(),
      n_D0 = sum(D_desc_candidata == 0L, na.rm = TRUE),
      n_D1 = sum(D_desc_candidata == 1L, na.rm = TRUE),
      n_NA = sum(is.na(D_desc_candidata)),
      pct_private = pct_private(D_desc_candidata),
      .groups = "drop"
    ),
  candidate_sfa_view %>%
    filter(en_df_sfa_actual) %>%
    group_by(anyo) %>%
    summarise(
      version = "D_desc_candidata_siae_en_df_sfa",
      n_total = n(),
      n_D0 = sum(D_desc_candidata == 0L, na.rm = TRUE),
      n_D1 = sum(D_desc_candidata == 1L, na.rm = TRUE),
      n_NA = sum(is.na(D_desc_candidata)),
      pct_private = pct_private(D_desc_candidata),
      .groups = "drop"
    ),
  candidate_sfa_view %>%
    filter(en_df_sfa_actual) %>%
    group_by(anyo) %>%
    summarise(
      version = "D_desc_actual_df_sfa",
      n_total = n(),
      n_D0 = sum(D_desc_actual_df_sfa == 0L, na.rm = TRUE),
      n_D1 = sum(D_desc_actual_df_sfa == 1L, na.rm = TRUE),
      n_NA = sum(is.na(D_desc_actual_df_sfa)),
      pct_private = pct_private(D_desc_actual_df_sfa),
      .groups = "drop"
    )
) %>% arrange(version, anyo)

resumen_validacion <- candidate_sfa_view %>%
  count(validacion_cnh, cambio_vs_df_sfa, name = "n_hospital_anyo") %>%
  arrange(desc(n_hospital_anyo), validacion_cnh, cambio_vs_df_sfa)

write_csv(
  candidate_sfa_view %>%
    select(
      NCODI, anyo, nombre_hospital,
      cod_depend_agrupada, Depend_agrupada,
      D_desc_candidata, fuente_recomendada,
      pertenencia_sns_cnh, dependencia_cnh, D_desc_cnh_simple, validacion_cnh,
      D_desc_actual_df_final, D_desc_tesis_df_final, D_desc_actual_df_sfa,
      en_df_sfa_actual, cambio_vs_df_sfa
    ),
  file.path(out_dir, "D_desc_candidata_hospital_anyo.csv"),
  na = ""
)

write_csv(resumen_candidata, file.path(out_dir, "resumen_D_desc_candidata.csv"), na = "")
write_csv(resumen_validacion, file.path(out_dir, "validacion_D_desc_candidata.csv"), na = "")

writeLines(
  c(
    "D_desc candidata recomendada",
    "",
    "Regla recomendada:",
    "Usar D_desc_siae derivada de cod_depend_agrupada.",
    "Recodificacion: 1 -> 0 (publico), 2 -> 1 (privado), resto -> NA.",
    "",
    "CNH en esta propuesta se conserva solo como validacion externa, no como fuente prioritaria."
  ),
  file.path(out_dir, "NOTA_D_desc_candidata.txt"),
  useBytes = TRUE
)

message("Candidata exportada en: ", out_dir)
