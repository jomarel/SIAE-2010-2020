options(stringsAsFactors = FALSE)

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(stringr)
  library(tidyr)
})

project_dir <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
out_dir <- file.path(project_dir, "outputs", "audit_Ddesc")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

paths <- list(
  df_final_rdata = file.path(project_dir, "data_legacy_outputs", "df_final.RData"),
  df_sfa_rdata = file.path(project_dir, "data_intermediate", "df_sfa.RData"),
  ncodi_map = file.path(project_dir, "data_intermediate", "ncodi_hospital_map.csv"),
  cobertura = file.path(project_dir, "data_intermediate", "Ddesc_cobertura_final.csv"),
  out23 = file.path(project_dir, "outputs", "tabla_dependencia_siae_cnh_2023.csv"),
  legacy_dir = file.path(project_dir, "scripts", "legacy"),
  year_dir = file.path(project_dir, "data_legacy_outputs")
)

req_files <- c("df_final_rdata", "df_sfa_rdata", "ncodi_map", "cobertura")
missing_req <- req_files[!file.exists(unlist(paths[req_files]))]
if (length(missing_req) > 0) {
  stop("Faltan archivos requeridos: ", paste(missing_req, collapse = ", "), call. = FALSE)
}

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
  x <- gsub("[[:space:]]+", " ", x)
  x
}

pct_private <- function(x) {
  n0 <- sum(x == 0L, na.rm = TRUE)
  n1 <- sum(x == 1L, na.rm = TRUE)
  if ((n0 + n1) == 0) return(NA_real_)
  100 * n1 / (n0 + n1)
}

load_rdata_object <- function(path, preferred = NULL) {
  env <- new.env(parent = emptyenv())
  objs <- load(path, envir = env)
  if (!is.null(preferred) && preferred %in% objs) {
    nm <- preferred
  } else {
    is_df <- vapply(objs, function(nm) is.data.frame(get(nm, envir = env)), logical(1))
    if (!any(is_df)) {
      stop("No hay data.frames en ", path, call. = FALSE)
    }
    nm <- objs[which(is_df)[1]]
  }
  list(
    object_name = nm,
    object_names = objs,
    value = get(nm, envir = env)
  )
}

find_first_col <- function(nms, patterns) {
  for (pat in patterns) {
    hit <- grep(pat, nms, ignore.case = TRUE, value = TRUE)
    if (length(hit) > 0) return(hit[1])
  }
  NA_character_
}

read_year_dep_file <- function(path) {
  cols <- c("anyo", "NCODI", "nombre_hospital", "cod_depend_agrupada", "Depend_agrupada")
  tryCatch(
    readr::read_csv(path, show_col_types = FALSE, col_select = any_of(cols)),
    error = function(e) read.csv(path, check.names = FALSE, stringsAsFactors = FALSE)[, intersect(cols, names(read.csv(path, nrows = 1, check.names = FALSE))), drop = FALSE]
  )
}

summarise_binary_by_year <- function(df, value_col, version, base_set) {
  val <- df[[value_col]]
  tibble(
    anyo = to_int(df$anyo),
    value = to_int(val)
  ) %>%
    group_by(anyo) %>%
    summarise(
      n_total = n(),
      n_D0 = sum(value == 0L, na.rm = TRUE),
      n_D1 = sum(value == 1L, na.rm = TRUE),
      n_NA = sum(is.na(value)),
      pct_private = pct_private(value),
      .groups = "drop"
    ) %>%
    mutate(version = version, base_set = base_set) %>%
    select(base_set, version, anyo, n_total, n_D0, n_D1, n_NA, pct_private)
}

df_final_info <- load_rdata_object(paths$df_final_rdata, preferred = "df_final")
df_sfa_info <- load_rdata_object(paths$df_sfa_rdata, preferred = "df_sfa")

df_final <- df_final_info$value
df_sfa <- df_sfa_info$value
map <- read_csv(paths$ncodi_map, show_col_types = FALSE)
cov_ref <- read_csv(paths$cobertura, show_col_types = FALSE)
out23 <- if (file.exists(paths$out23)) read_csv(paths$out23, show_col_types = FALSE) else NULL

df_final$NCODI <- to_int(df_final$NCODI)
df_final$anyo <- to_int(df_final$anyo)
df_sfa$NCODI <- to_int(df_sfa$NCODI)
df_sfa$anyo <- to_int(df_sfa$anyo)
map$NCODI <- to_int(map$NCODI)

year_files <- list.files(paths$year_dir, pattern = "^df_[0-9]{4}\\.csv$", full.names = TRUE)
year_dep <- bind_rows(lapply(year_files, function(path) {
  dat <- read_year_dep_file(path)
  dat$file_name <- basename(path)
  dat
})) %>%
  mutate(
    nombre_hospital = if ("nombre_hospital" %in% names(.)) nombre_hospital else NA_character_,
    cod_depend_agrupada = if ("cod_depend_agrupada" %in% names(.)) cod_depend_agrupada else NA_integer_,
    Depend_agrupada = if ("Depend_agrupada" %in% names(.)) Depend_agrupada else NA_character_
  ) %>%
  mutate(
    NCODI = to_int(NCODI),
    anyo = to_int(anyo),
    cod_depend_agrupada = to_int(cod_depend_agrupada),
    Depend_agrupada = to_chr(Depend_agrupada),
    nombre_hospital = to_chr(nombre_hospital)
  )

df_final_dep <- df_final %>%
  transmute(
    NCODI = to_int(NCODI),
    anyo = to_int(anyo),
    nombre_hospital = to_chr(nombre_hospital),
    cod_depend_agrupada = to_int(cod_depend_agrupada),
    Depend_agrupada = to_chr(Depend_agrupada),
    D_desc_existente_df_final = to_int(D_desc),
    D_desc_siae_existente_df_final = to_int(D_desc_siae),
    D_desc_cnh_existente_df_final = to_int(D_desc_cnh),
    D_desc_tesis_existente_df_final = if ("D_desc_tesis" %in% names(df_final)) to_int(D_desc_tesis) else NA_integer_
  )

year_dep_compare <- df_final_dep %>%
  left_join(
    year_dep %>%
      rename(
        nombre_hospital_year = nombre_hospital,
        cod_depend_agrupada_year = cod_depend_agrupada,
        Depend_agrupada_year = Depend_agrupada
      ),
    by = c("NCODI", "anyo")
  ) %>%
  mutate(
    cod_match_yearfile = dplyr::case_when(
      is.na(cod_depend_agrupada) & is.na(cod_depend_agrupada_year) ~ TRUE,
      cod_depend_agrupada == cod_depend_agrupada_year ~ TRUE,
      TRUE ~ FALSE
    ),
    label_match_yearfile = dplyr::case_when(
      is.na(Depend_agrupada) & is.na(Depend_agrupada_year) ~ TRUE,
      norm_ascii(Depend_agrupada) == norm_ascii(Depend_agrupada_year) ~ TRUE,
      TRUE ~ FALSE
    )
  )

siae_evidence <- bind_rows(
  df_final_dep %>% transmute(source = "df_final", anyo, cod_depend_agrupada, Depend_agrupada),
  year_dep %>% transmute(source = "year_file", anyo, cod_depend_agrupada, Depend_agrupada)
) %>%
  mutate(
    cod_depend_agrupada = to_int(cod_depend_agrupada),
    Depend_agrupada = to_chr(Depend_agrupada)
  ) %>%
  count(source, anyo, cod_depend_agrupada, Depend_agrupada, name = "n_obs") %>%
  arrange(anyo, cod_depend_agrupada, desc(n_obs), source)

siae_code_summary <- bind_rows(
  df_final_dep %>% transmute(source = "df_final", anyo, cod_depend_agrupada, Depend_agrupada),
  year_dep %>% transmute(source = "year_file", anyo, cod_depend_agrupada, Depend_agrupada)
) %>%
  mutate(
    cod_depend_agrupada = to_int(cod_depend_agrupada),
    dep_norm = norm_ascii(Depend_agrupada)
  ) %>%
  group_by(source, anyo, cod_depend_agrupada) %>%
  summarise(
    n_obs = n(),
    etiquetas_no_na = paste(sort(unique(dep_norm[!is.na(dep_norm)])), collapse = " | "),
    .groups = "drop"
  ) %>%
  arrange(source, anyo, cod_depend_agrupada)

df_final_recon_siae <- df_final_dep %>%
  mutate(
    dep_norm = norm_ascii(Depend_agrupada),
    D_desc_siae_recon = case_when(
      cod_depend_agrupada == 1L ~ 0L,
      cod_depend_agrupada == 2L ~ 1L,
      TRUE ~ NA_integer_
    ),
    regla_siae = case_when(
      cod_depend_agrupada == 1L & !is.na(dep_norm) & str_detect(dep_norm, "PUBLIC") ~ "cod=1 con etiqueta publica -> D_desc_siae=0",
      cod_depend_agrupada == 1L & is.na(dep_norm) ~ "cod=1 sin etiqueta; evidencia anual consistente con Publicos-SNS -> D_desc_siae=0",
      cod_depend_agrupada == 2L & !is.na(dep_norm) & str_detect(dep_norm, "PRIVAD") ~ "cod=2 con etiqueta privados -> D_desc_siae=1",
      cod_depend_agrupada == 2L & is.na(dep_norm) ~ "cod=2 sin etiqueta; evidencia anual consistente con Privados -> D_desc_siae=1",
      is.na(cod_depend_agrupada) ~ "cod_depend_agrupada ausente -> NA",
      TRUE ~ "codigo fuera de {1,2} -> NA"
    ),
    observacion_siae = case_when(
      anyo <= 2015 & cod_depend_agrupada == 1L & is.na(Depend_agrupada) ~ "En 2010-2015 falta la etiqueta textual del codigo 1 en df_final, pero no hay evidencia de inversion del codigo",
      anyo == 2017 & is.na(cod_depend_agrupada) ~ "Fila con NA en cod_depend_agrupada",
      TRUE ~ NA_character_
    )
  )

map_cnh <- map %>%
  transmute(
    NCODI = to_int(NCODI),
    CODCNH_map = if ("CODCNH" %in% names(map)) to_chr(CODCNH) else NA_character_,
    pertenencia_sns_cnh = to_chr(pertenencia_sns_cnh),
    dependencia_cnh = if ("dependencia_cnh" %in% names(map)) to_chr(dependencia_cnh) else NA_character_,
    clase_centro_map = if ("clase_centro" %in% names(map)) to_chr(clase_centro) else NA_character_,
    D_desc_cnh_recon = case_when(
      norm_ascii(pertenencia_sns_cnh) == "PUBLICOS-SNS" ~ 0L,
      norm_ascii(pertenencia_sns_cnh) == "PRIVADOS" ~ 1L,
      TRUE ~ NA_integer_
    ),
    criterio_cnh = case_when(
      norm_ascii(pertenencia_sns_cnh) == "PUBLICOS-SNS" ~ "pertenencia_sns_cnh = Publicos-SNS -> D_desc_cnh=0",
      norm_ascii(pertenencia_sns_cnh) == "PRIVADOS" ~ "pertenencia_sns_cnh = Privados -> D_desc_cnh=1",
      is.na(pertenencia_sns_cnh) ~ "pertenencia_sns_cnh ausente -> NA",
      TRUE ~ "categoria no binaria de pertenencia_sns_cnh -> NA"
    )
  ) %>%
  distinct(NCODI, .keep_all = TRUE)

categorias_cnh_no_binarias <- map_cnh %>%
  filter(is.na(D_desc_cnh_recon)) %>%
  count(pertenencia_sns_cnh, dependencia_cnh, clase_centro_map, name = "n_hospitales") %>%
  arrange(desc(n_hospitales), pertenencia_sns_cnh)

if (nrow(categorias_cnh_no_binarias) == 0) {
  categorias_cnh_no_binarias <- tibble(
    pertenencia_sns_cnh = character(),
    dependencia_cnh = character(),
    clase_centro_map = character(),
    n_hospitales = integer()
  )
}

df_recon_full <- df_final_recon_siae %>%
  left_join(map_cnh, by = "NCODI") %>%
  mutate(
    D_desc_map_coalesce = dplyr::coalesce(D_desc_cnh_recon, D_desc_siae_recon)
  )

df_compare <- df_sfa %>%
  select(
    NCODI, anyo, nombre_hospital,
    D_desc_actual_df_sfa = D_desc,
    D_desc_siae_actual_df_sfa = D_desc_siae,
    D_desc_cnh_actual_df_sfa = D_desc_cnh,
    D_desc_tesis_actual_df_sfa = any_of("D_desc_tesis")
  ) %>%
  mutate(D_desc_tesis_actual_df_sfa = if ("D_desc_tesis_actual_df_sfa" %in% names(.)) to_int(D_desc_tesis_actual_df_sfa) else NA_integer_) %>%
  left_join(
    df_recon_full %>%
      select(
        NCODI, anyo,
        cod_depend_agrupada, Depend_agrupada,
        D_desc_siae_recon, regla_siae, observacion_siae,
        CODCNH_map, pertenencia_sns_cnh, dependencia_cnh, clase_centro_map,
        D_desc_cnh_recon, criterio_cnh,
        D_desc_map_coalesce,
        D_desc_existente_df_final,
        D_desc_siae_existente_df_final,
        D_desc_cnh_existente_df_final,
        D_desc_tesis_existente_df_final
      ),
    by = c("NCODI", "anyo")
  )

trazabilidad <- df_compare %>%
  arrange(anyo, NCODI) %>%
  select(
    NCODI, anyo, nombre_hospital,
    cod_depend_agrupada, Depend_agrupada,
    D_desc_siae_recon, regla_siae, observacion_siae,
    CODCNH_map, pertenencia_sns_cnh, dependencia_cnh, clase_centro_map,
    D_desc_cnh_recon, criterio_cnh,
    D_desc_map_coalesce,
    D_desc_actual_df_sfa,
    D_desc_siae_actual_df_sfa,
    D_desc_cnh_actual_df_sfa,
    D_desc_tesis_actual_df_sfa,
    D_desc_existente_df_final,
    D_desc_siae_existente_df_final,
    D_desc_cnh_existente_df_final,
    D_desc_tesis_existente_df_final
  )

resumen_anual <- bind_rows(
  summarise_binary_by_year(df_compare, "D_desc_actual_df_sfa", "df_sfa_actual_D_desc", "df_sfa_all_years"),
  summarise_binary_by_year(df_compare, "D_desc_siae_actual_df_sfa", "df_sfa_actual_D_desc_siae", "df_sfa_all_years"),
  summarise_binary_by_year(df_compare, "D_desc_cnh_actual_df_sfa", "df_sfa_actual_D_desc_cnh", "df_sfa_all_years"),
  summarise_binary_by_year(df_compare, "D_desc_tesis_actual_df_sfa", "df_sfa_actual_D_desc_tesis", "df_sfa_all_years"),
  summarise_binary_by_year(df_compare, "D_desc_siae_recon", "reconstruida_D_desc_siae", "df_sfa_all_years"),
  summarise_binary_by_year(df_compare, "D_desc_cnh_recon", "reconstruida_D_desc_cnh", "df_sfa_all_years"),
  summarise_binary_by_year(df_compare, "D_desc_map_coalesce", "reconstruida_D_desc_coalesce_map", "df_sfa_all_years"),
  summarise_binary_by_year(df_recon_full, "D_desc_existente_df_final", "df_final_existente_D_desc", "df_final_all_years"),
  summarise_binary_by_year(df_recon_full, "D_desc_siae_existente_df_final", "df_final_existente_D_desc_siae", "df_final_all_years"),
  summarise_binary_by_year(df_recon_full, "D_desc_cnh_existente_df_final", "df_final_existente_D_desc_cnh", "df_final_all_years"),
  summarise_binary_by_year(df_recon_full, "D_desc_tesis_existente_df_final", "df_final_existente_D_desc_tesis", "df_final_all_years"),
  summarise_binary_by_year(df_recon_full, "D_desc_siae_recon", "reconstruida_D_desc_siae", "df_final_all_years"),
  summarise_binary_by_year(df_recon_full, "D_desc_cnh_recon", "reconstruida_D_desc_cnh", "df_final_all_years"),
  summarise_binary_by_year(df_recon_full, "D_desc_map_coalesce", "reconstruida_D_desc_coalesce_map", "df_final_all_years")
) %>%
  arrange(base_set, version, anyo)

cov_ref2 <- cov_ref %>%
  transmute(
    anyo = to_int(anyo),
    ref_n_D0 = to_int(n_D0),
    ref_n_D1 = to_int(n_D1),
    ref_pct_private = if ("pct_private" %in% names(cov_ref)) as.numeric(pct_private) else 100 * to_int(n_D1) / (to_int(n_D0) + to_int(n_D1))
  )

comparacion_cobertura <- resumen_anual %>%
  left_join(cov_ref2, by = "anyo") %>%
  mutate(
    abs_diff_pct_private = abs(pct_private - ref_pct_private),
    abs_diff_n_D0 = abs(n_D0 - ref_n_D0),
    abs_diff_n_D1 = abs(n_D1 - ref_n_D1)
  )

mad_versiones <- comparacion_cobertura %>%
  group_by(base_set, version) %>%
  summarise(
    mean_abs_diff_pct_private = mean(abs_diff_pct_private, na.rm = TRUE),
    mean_abs_diff_n_D0 = mean(abs_diff_n_D0, na.rm = TRUE),
    mean_abs_diff_n_D1 = mean(abs_diff_n_D1, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(mean_abs_diff_pct_private, mean_abs_diff_n_D0, mean_abs_diff_n_D1)

discordancias_siae_cnh <- df_recon_full %>%
  filter(
    !is.na(D_desc_siae_recon),
    !is.na(D_desc_cnh_recon),
    D_desc_siae_recon != D_desc_cnh_recon
  ) %>%
  select(
    NCODI, anyo, nombre_hospital,
    cod_depend_agrupada, Depend_agrupada, D_desc_siae_recon, regla_siae,
    pertenencia_sns_cnh, dependencia_cnh, D_desc_cnh_recon, criterio_cnh
  ) %>%
  arrange(anyo, NCODI)

discordancias_actual_vs_reconstruida <- bind_rows(
  df_compare %>%
    filter(
      !(is.na(D_desc_actual_df_sfa) & is.na(D_desc_siae_recon)),
      is.na(D_desc_actual_df_sfa) != is.na(D_desc_siae_recon) | D_desc_actual_df_sfa != D_desc_siae_recon
    ) %>%
    transmute(
      comparacion = "actual_df_sfa_vs_siae_reconstruida",
      NCODI, anyo, nombre_hospital,
      valor_actual = D_desc_actual_df_sfa,
      valor_reconstruido = D_desc_siae_recon,
      cod_depend_agrupada, Depend_agrupada, regla_siae,
      pertenencia_sns_cnh, dependencia_cnh, criterio_cnh
    ),
  df_compare %>%
    filter(
      !(is.na(D_desc_actual_df_sfa) & is.na(D_desc_cnh_recon)),
      is.na(D_desc_actual_df_sfa) != is.na(D_desc_cnh_recon) | D_desc_actual_df_sfa != D_desc_cnh_recon
    ) %>%
    transmute(
      comparacion = "actual_df_sfa_vs_cnh_reconstruida",
      NCODI, anyo, nombre_hospital,
      valor_actual = D_desc_actual_df_sfa,
      valor_reconstruido = D_desc_cnh_recon,
      cod_depend_agrupada, Depend_agrupada, regla_siae,
      pertenencia_sns_cnh, dependencia_cnh, criterio_cnh
    ),
  df_compare %>%
    filter(
      !(is.na(D_desc_actual_df_sfa) & is.na(D_desc_map_coalesce)),
      is.na(D_desc_actual_df_sfa) != is.na(D_desc_map_coalesce) | D_desc_actual_df_sfa != D_desc_map_coalesce
    ) %>%
    transmute(
      comparacion = "actual_df_sfa_vs_coalesce_map_reconstruida",
      NCODI, anyo, nombre_hospital,
      valor_actual = D_desc_actual_df_sfa,
      valor_reconstruido = D_desc_map_coalesce,
      cod_depend_agrupada, Depend_agrupada, regla_siae,
      pertenencia_sns_cnh, dependencia_cnh, criterio_cnh
    )
) %>%
  arrange(comparacion, anyo, NCODI)

diagnostico_2023_rows <- list()

if (!is.null(out23)) {
  out23 <- out23 %>%
    mutate(
      NCODI = to_int(NCODI),
      anyo = to_int(anyo),
      D_desc = to_int(D_desc),
      D_desc_siae = to_int(D_desc_siae),
      D_desc_cnh = to_int(D_desc_cnh),
      cod_depend_agrupada = to_int(cod_depend_agrupada)
    )

  f23 <- df_recon_full %>% filter(anyo == 2023)
  s23 <- df_compare %>% filter(anyo == 2023)
  o23 <- out23 %>% filter(anyo == 2023)

  keys_out23 <- o23 %>% distinct(NCODI, anyo)
  keys_f23 <- f23 %>% distinct(NCODI, anyo)
  keys_s23 <- s23 %>% distinct(NCODI, anyo)

  diagnostico_2023_rows[[1]] <- tibble(
    seccion = "resumen_bases",
    detalle = c("n_out23", "n_df_final_2023", "n_df_sfa_2023", "out23_en_df_final", "out23_en_df_sfa"),
    valor = c(
      nrow(keys_out23),
      nrow(keys_f23),
      nrow(keys_s23),
      nrow(inner_join(keys_out23, keys_f23, by = c("NCODI", "anyo"))),
      nrow(inner_join(keys_out23, keys_s23, by = c("NCODI", "anyo")))
    )
  )

  diagnostico_2023_rows[[2]] <- bind_rows(
    anti_join(keys_f23, keys_out23, by = c("NCODI", "anyo")) %>%
      left_join(f23 %>% select(NCODI, anyo, nombre_hospital), by = c("NCODI", "anyo")) %>%
      mutate(seccion = "solo_df_final_2023", detalle = nombre_hospital, valor = NA_real_),
    anti_join(keys_s23, keys_out23, by = c("NCODI", "anyo")) %>%
      left_join(s23 %>% select(NCODI, anyo, nombre_hospital), by = c("NCODI", "anyo")) %>%
      mutate(seccion = "solo_df_sfa_2023", detalle = nombre_hospital, valor = NA_real_),
    anti_join(keys_out23, keys_s23, by = c("NCODI", "anyo")) %>%
      left_join(o23 %>% select(NCODI, anyo, nombre_hospital), by = c("NCODI", "anyo")) %>%
      mutate(seccion = "solo_out23", detalle = nombre_hospital, valor = NA_real_)
  ) %>%
    mutate(detalle = paste0("NCODI=", NCODI, " | ", detalle)) %>%
    select(seccion, detalle, valor)

  val_cmp <- full_join(
    f23 %>% select(NCODI, anyo, D_desc_df_final = D_desc_existente_df_final, D_desc_siae_df_final = D_desc_siae_existente_df_final, D_desc_cnh_df_final = D_desc_cnh_existente_df_final),
    o23 %>% select(NCODI, anyo, D_desc_out23 = D_desc, D_desc_siae_out23 = D_desc_siae, D_desc_cnh_out23 = D_desc_cnh),
    by = c("NCODI", "anyo")
  )

  make_cmp_stats <- function(a, b, label) {
    tibble(
      seccion = "comparacion_valores_out23_vs_df_final",
      detalle = c(
        paste0(label, "_both_non_na"),
        paste0(label, "_equal"),
        paste0(label, "_diff"),
        paste0(label, "_df_final_na_out23_no"),
        paste0(label, "_df_final_no_out23_na")
      ),
      valor = c(
        sum(!is.na(a) & !is.na(b)),
        sum(!is.na(a) & !is.na(b) & a == b),
        sum(!is.na(a) & !is.na(b) & a != b),
        sum(is.na(a) & !is.na(b)),
        sum(!is.na(a) & is.na(b))
      )
    )
  }

  diagnostico_2023_rows[[3]] <- bind_rows(
    make_cmp_stats(val_cmp$D_desc_df_final, val_cmp$D_desc_out23, "D_desc"),
    make_cmp_stats(val_cmp$D_desc_siae_df_final, val_cmp$D_desc_siae_out23, "D_desc_siae"),
    make_cmp_stats(val_cmp$D_desc_cnh_df_final, val_cmp$D_desc_cnh_out23, "D_desc_cnh")
  )

  diagnostico_2023_rows[[4]] <- bind_rows(
    summarise_binary_by_year(o23, "D_desc", "out23_D_desc", "out23_only"),
    summarise_binary_by_year(o23, "D_desc_siae", "out23_D_desc_siae", "out23_only"),
    summarise_binary_by_year(o23, "D_desc_cnh", "out23_D_desc_cnh", "out23_only")
  ) %>%
    mutate(
      seccion = "resumen_distribuciones_2023",
      detalle = paste0(version, " | n_total=", n_total, " | n_D0=", n_D0, " | n_D1=", n_D1, " | n_NA=", n_NA),
      valor = pct_private
    ) %>%
    select(seccion, detalle, valor)
}

diagnostico_2023 <- if (length(diagnostico_2023_rows) > 0) bind_rows(diagnostico_2023_rows) else tibble(
  seccion = "archivo_no_encontrado",
  detalle = "outputs/tabla_dependencia_siae_cnh_2023.csv no existe",
  valor = NA_real_
)

legacy_files <- list.files(paths$legacy_dir, pattern = "(depend|Ddesc|cnh)", full.names = TRUE, ignore.case = FALSE)
legacy_info <- if (length(legacy_files) > 0) {
  tibble(
    path = legacy_files,
    name = basename(legacy_files),
    modified = file.info(legacy_files)$mtime
  ) %>% arrange(desc(modified))
} else {
  tibble(path = character(), name = character(), modified = as.POSIXct(character()))
}

legacy_summary_lines <- c(
  "Auditoria de scripts legacy relacionados con depend / Ddesc / cnh",
  paste0("Fecha de ejecucion: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
  ""
)

if (nrow(legacy_info) == 0) {
  legacy_summary_lines <- c(legacy_summary_lines, "No se encontraron scripts legacy coincidentes.")
} else {
  for (i in seq_len(nrow(legacy_info))) {
    path <- legacy_info$path[i]
    lines <- readLines(path, warn = FALSE, encoding = "UTF-8")
    hits <- grep("D_desc|cod_depend_agrupada|Depend_agrupada|pertenencia_sns_cnh|coalesce\\(|case_when\\(", lines, ignore.case = TRUE)
    excerpt_idx <- unique(unlist(lapply(head(hits, 6), function(idx) seq.int(max(1, idx - 1), min(length(lines), idx + 1)))))
    excerpt <- if (length(excerpt_idx) > 0) paste0(sprintf("%4d", excerpt_idx), ": ", lines[excerpt_idx]) else "  (sin lineas clave detectadas)"
    legacy_summary_lines <- c(
      legacy_summary_lines,
      paste0("Archivo: ", legacy_info$name[i]),
      paste0("Modificado: ", format(legacy_info$modified[i], "%Y-%m-%d %H:%M:%S")),
      "Resumen heuristico:",
      paste0(
        "  - ",
        if (any(grepl("cod_depend_agrupada == 1.*0|== 1L ~ 0", lines))) {
          "Recodifica SIAE como 1 -> 0 (publico)."
        } else {
          "No se detecto recodificacion explicita 1 -> 0."
        }
      ),
      paste0(
        "  - ",
        if (any(grepl("cod_depend_agrupada == 2.*1|== 2L ~ 1", lines))) {
          "Recodifica SIAE como 2 -> 1 (privado)."
        } else {
          "No se detecto recodificacion explicita 2 -> 1."
        }
      ),
      paste0(
        "  - ",
        if (any(grepl("coalesce\\(D_desc_cnh, D_desc_siae\\)|coalesce\\(D_desc_tesis_raw, D_desc_siae\\)|coalesce\\(D_desc_cnh_curada, D_desc_siae\\)", lines))) {
          "Aparece una logica de prioridad CNH sobre SIAE con coalescencia."
        } else {
          "No se detecta coalescencia CNH -> SIAE en este script."
        }
      ),
      "Fragmentos relevantes:",
      excerpt,
      ""
    )
  }
}

best_match <- mad_versiones %>% slice(1)
best_version_text <- if (nrow(best_match) == 1) {
  paste0(best_match$base_set[1], " / ", best_match$version[1], " (MAD % privado = ", round(best_match$mean_abs_diff_pct_private[1], 6), ")")
} else {
  "No disponible"
}

siae_year_change_flag <- siae_code_summary %>%
  filter(source == "df_final", cod_depend_agrupada %in% c(1L, 2L)) %>%
  group_by(cod_depend_agrupada) %>%
  summarise(n_etiquetas = n_distinct(etiquetas_no_na[etiquetas_no_na != ""]), .groups = "drop")

conclusion_lines <- c(
  "CONCLUSION D_desc",
  "",
  paste0("Objeto cargado de df_final.RData: ", df_final_info$object_name),
  paste0("Objeto cargado de df_sfa.RData: ", df_sfa_info$object_name),
  "",
  "1. Variable fuente SIAE recomendada",
  "Usar cod_depend_agrupada como fuente primaria de SIAE para reconstruir D_desc_siae.",
  "La evidencia empírica observada en df_final y en los CSV anuales muestra que el codigo 2 aparece asociado sistematicamente a 'Privados'.",
  "Cuando la etiqueta textual esta presente, el codigo 1 aparece asociado a 'Publicos-SNS'.",
  "",
  "2. Recodificacion correcta de SIAE",
  "Aplicar: cod_depend_agrupada = 1 -> D_desc_siae = 0 (publico); cod_depend_agrupada = 2 -> D_desc_siae = 1 (privado); otros/NA -> NA.",
  "",
  "3. Cambio de codificacion por año",
  if (all(siae_year_change_flag$n_etiquetas <= 1)) {
    "No se encontro evidencia de cambio de codificacion por año. La principal incidencia es que en 2010-2015 suele faltar la etiqueta textual del codigo 1 en df_final."
  } else {
    "Se detectaron variaciones en etiquetas por año; revisar el detalle de auditoria_fuente_siae.csv antes de fijar una regla unica."
  },
  "",
  "4. Prioridad SIAE vs CNH",
  "Para la reconstruccion binaria estricta pedida aqui, CNH desde ncodi_hospital_map.csv y pertenencia_sns_cnh es limpio pero demasiado reductivo: solo clasifica Publicos-SNS vs Privados y pierde matiz institucional.",
  "SIAE es la fuente mas directa para el binario publico/privado del proyecto. CNH sirve mejor como contraste o validacion externa que como sustituto unico.",
  "",
  "5. Version que mejor reproduce Ddesc_cobertura_final.csv",
  paste0("La version con menor media de diferencia absoluta en % privado dentro de esta auditoria fue: ", best_version_text, "."),
  "Revisar resumen_anual_Ddesc_versiones.csv y comparacion_cobertura_Ddesc.csv para el ranking completo.",
  "",
  "6. Explicacion mas probable de la discrepancia con el df_sfa actual",
  "La version actual del pipeline en scripts/06_construir_Ddesc_pago_inputs.R no usa una coalescencia simple simetrica.",
  "Su logica actual clasifica usando textos de CNH, propaga informacion por hospital, y excluye el caso 'SIAE publico sin CNH' de la variable final D_desc.",
  "Ademas, df_sfa no contiene todos los hospital-anio de df_final, por lo que la base efectiva de comparacion cambia.",
  "El CSV outputs/tabla_dependencia_siae_cnh_2023.csv tampoco coincide con df_sfa actual porque usa una base 2023 distinta y una logica de CNH distinta a la que hoy esta en df_final/df_sfa."
)

write_csv(resumen_anual, file.path(out_dir, "resumen_anual_Ddesc_versiones.csv"), na = "")
write_csv(trazabilidad, file.path(out_dir, "trazabilidad_Ddesc_hospital_anyo.csv"), na = "")
write_csv(discordancias_siae_cnh, file.path(out_dir, "discordancias_siae_cnh.csv"), na = "")
write_csv(discordancias_actual_vs_reconstruida, file.path(out_dir, "discordancias_actual_vs_reconstruida.csv"), na = "")
write_csv(categorias_cnh_no_binarias, file.path(out_dir, "categorias_cnh_no_binarias.csv"), na = "")
write_csv(diagnostico_2023, file.path(out_dir, "diagnostico_2023.csv"), na = "")
writeLines(legacy_summary_lines, file.path(out_dir, "resumen_scripts_legacy_Ddesc.txt"), useBytes = TRUE)
writeLines(conclusion_lines, file.path(out_dir, "CONCLUSION_Ddesc.txt"), useBytes = TRUE)

write_csv(siae_evidence, file.path(out_dir, "auditoria_fuente_siae.csv"), na = "")
write_csv(siae_code_summary, file.path(out_dir, "auditoria_codigos_siae_por_anyo.csv"), na = "")
write_csv(year_dep_compare, file.path(out_dir, "comparacion_df_final_vs_archivos_anuales_dependencia.csv"), na = "")
write_csv(comparacion_cobertura, file.path(out_dir, "comparacion_cobertura_Ddesc.csv"), na = "")
write_csv(mad_versiones, file.path(out_dir, "mad_versiones_vs_cobertura.csv"), na = "")

message("Auditoria completada en: ", out_dir)
