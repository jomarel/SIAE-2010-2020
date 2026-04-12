options(stringsAsFactors = FALSE)

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
})

project_dir <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
out_dir <- file.path(project_dir, "outputs", "audit_Ddesc")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

num <- function(x) suppressWarnings(as.numeric(as.character(x)))

safe_load_single <- function(path) {
  env <- new.env(parent = emptyenv())
  objs <- load(path, envir = env)
  list(path = path, objects = objs, env = env)
}

get_df <- function(loaded, preferred = c("df_sfa", "df_final")) {
  for (nm in preferred) {
    if (nm %in% loaded$objects) return(get(nm, envir = loaded$env))
  }
  for (nm in loaded$objects) {
    obj <- get(nm, envir = loaded$env)
    if (is.data.frame(obj)) return(obj)
  }
  NULL
}

normalize_df <- function(df) {
  if (is.null(df) || !is.data.frame(df)) return(df)
  for (v in intersect(c("anyo", "es_agudo", "altTotal_bruto", "D_desc", "D_desc_tesis", "D_desc_siae", "pct_sns", "ShareQ"), names(df))) {
    df[[v]] <- num(df[[v]])
  }
  if ("NCODI" %in% names(df)) df$NCODI <- as.character(df$NCODI)
  df
}

sample_tesis_like <- function(df, dvar = "D_desc", alt_cut = 200) {
  if (is.null(df) || !is.data.frame(df) || !dvar %in% names(df)) return(NULL)
  keep <- c(
    "es_agudo", "anyo", "altTotal_bruto", dvar, "pct_sns", "ShareQ",
    "ln_L_total_c", "ln_K_camas_c", "ccaa_cod", "ln_i_diag", "ln_altTotal_pond"
  )
  if (!all(keep %in% names(df))) return(NULL)
  out <- df %>%
    filter(
      es_agudo == 1,
      !anyo %in% 2020:2022,
      altTotal_bruto >= alt_cut,
      !is.na(.data[[dvar]]),
      !is.na(pct_sns),
      !is.na(ShareQ),
      !is.na(ln_L_total_c),
      !is.na(ln_K_camas_c),
      !is.na(ccaa_cod),
      is.finite(ln_i_diag),
      is.finite(ln_altTotal_pond)
    )
  if (nrow(out) == 0) return(out)
  out
}

year_summary <- function(df, dvar) {
  if (is.null(df) || nrow(df) == 0 || !dvar %in% names(df)) return(tibble())
  df %>%
    group_by(anyo) %>%
    summarise(
      n_obs = n(),
      n_hospitales = n_distinct(NCODI),
      n_D0 = sum(.data[[dvar]] == 0, na.rm = TRUE),
      n_D1 = sum(.data[[dvar]] == 1, na.rm = TRUE),
      pct_D1 = round(100 * mean(.data[[dvar]] == 1, na.rm = TRUE), 3),
      .groups = "drop"
    )
}

inspect_model_bundle <- function(path) {
  loaded <- safe_load_single(path)
  rows <- list()
  for (nm in loaded$objects) {
    obj <- get(nm, envir = loaded$env)
    call_txt <- tryCatch(paste(deparse(obj$call), collapse = " "), error = function(e) NA_character_)
    rows[[length(rows) + 1L]] <- tibble(
      file = normalizePath(path, winslash = "/", mustWork = TRUE),
      object = nm,
      class = paste(class(obj), collapse = "|"),
      nobs_slot = tryCatch(as.numeric(obj$Nobs), error = function(e) NA_real_),
      has_dataTable = isTRUE("dataTable" %in% names(obj)),
      call_txt = call_txt
    )
  }
  bind_rows(rows)
}

extract_model_data <- function(path, object_name = NULL) {
  loaded <- safe_load_single(path)
  nm <- object_name
  if (is.null(nm)) {
    nm <- loaded$objects[[1]]
  }
  obj <- get(nm, envir = loaded$env)
  dt <- tryCatch(obj$dataTable, error = function(e) NULL)
  if (is.null(dt)) return(NULL)
  dt <- as.data.frame(dt)
  normalize_df(dt)
}

rebuild_tesis_curada_sample <- function(project_dir) {
  df_loaded <- safe_load_single(file.path(project_dir, "data_intermediate", "backup_like_recon", "int", "df_sfa.RData"))
  df <- normalize_df(get_df(df_loaded))
  if (is.null(df)) return(NULL)

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
      dep_desc_raw = as.character(dep_desc_raw)
    ) %>%
    arrange(anyo, CODCNH, desc(!is.na(dep_desc_raw))) %>%
    distinct(anyo, CODCNH, .keep_all = TRUE)

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
      D_desc_curada = case_when(
        cat_cnh_tesis %in% c("SAS / integrado SNS", "Diputaciones/ayuntamientos") ~ 0,
        cat_cnh_tesis %in% c("MATEPSS", "Cruz Roja / iglesia", "Fundaciones publicas auts.", "Soc. mercantil con concierto", "Soc. mercantil de mercado") ~ 1,
        TRUE ~ NA_real_
      ),
      D_desc_curada = coalesce(D_desc_curada, D_desc_siae),
      desc_pago_curada = D_desc_curada * pct_sns,
      desc_shareQ_curada = D_desc_curada * ShareQ
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

  df
}

make_signature <- function(df, vars) {
  tmp <- df[, vars, drop = FALSE]
  for (v in vars) {
    if (is.numeric(tmp[[v]])) {
      tmp[[v]] <- sprintf("%.10f", tmp[[v]])
    } else {
      tmp[[v]] <- as.character(tmp[[v]])
    }
  }
  do.call(paste, c(tmp, sep = "||"))
}

target_pdf <- tibble::tribble(
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

paths <- list(
  current_df_sfa = file.path(project_dir, "data_intermediate", "df_sfa.RData"),
  backup_like_df_sfa = file.path(project_dir, "data_intermediate", "backup_like_recon", "int", "df_sfa.RData"),
  backup_like_pre = file.path(project_dir, "data_intermediate", "backup_like_recon", "out", "df_sfa_main_pre_backup_like.RData"),
  legacy_tesis_df_sfa = file.path(project_dir, "data_intermediate", "legacy_tesis_recon", "int", "df_sfa.RData"),
  tesis_curada_models = file.path(project_dir, "data_intermediate", "tesis_curada_recon", "modelos_A_A1_tesis_curada_sample_texto.RData"),
  legacy_tesis_models = file.path(project_dir, "data_intermediate", "legacy_tesis_recon", "out", "modelos_A_A1_legacy_tesis_lite.RData")
)

source_rows <- list()
missing_rows <- list()
sample_rows <- list()

for (nm in names(paths)) {
  path <- paths[[nm]]
  if (!file.exists(path)) next
  if (grepl("modelos_", basename(path))) next
  loaded <- safe_load_single(path)
  df <- normalize_df(get_df(loaded))
  if (is.null(df)) next

  source_rows[[length(source_rows) + 1L]] <- tibble(
    source = nm,
    path = normalizePath(path, winslash = "/", mustWork = TRUE),
    n_rows = nrow(df),
    n_hospitales = if ("NCODI" %in% names(df)) n_distinct(df$NCODI) else NA_integer_,
    anyo_min = if ("anyo" %in% names(df)) min(df$anyo, na.rm = TRUE) else NA_real_,
    anyo_max = if ("anyo" %in% names(df)) max(df$anyo, na.rm = TRUE) else NA_real_,
    has_D_desc = "D_desc" %in% names(df),
    has_D_desc_tesis = "D_desc_tesis" %in% names(df),
    has_es_agudo = "es_agudo" %in% names(df)
  )

  if (all(c("anyo", "es_agudo") %in% names(df))) {
    missing_rows[[length(missing_rows) + 1L]] <- df %>%
      group_by(anyo) %>%
      summarise(
        source = nm,
        n_all = n(),
        n_es_agudo_1 = sum(es_agudo == 1, na.rm = TRUE),
        n_es_agudo_0 = sum(es_agudo == 0, na.rm = TRUE),
        n_es_agudo_na = sum(is.na(es_agudo)),
        .groups = "drop"
      )
  }

  for (dvar in intersect(c("D_desc", "D_desc_tesis"), names(df))) {
    smp <- sample_tesis_like(df, dvar = dvar, alt_cut = 200)
    if (is.null(smp) || nrow(smp) == 0) next
    sample_rows[[length(sample_rows) + 1L]] <- year_summary(smp, dvar) %>%
      mutate(source = nm, dvar = dvar, sample_rule = "tesis_like_alt>=200")
  }
}

model_rows <- bind_rows(
  inspect_model_bundle(paths$tesis_curada_models),
  inspect_model_bundle(paths$legacy_tesis_models)
)

sample_texto_exact <- extract_model_data(paths$tesis_curada_models, "m1")
sample_texto_map <- rebuild_tesis_curada_sample(project_dir)

mapping_stats <- tibble()
if (!is.null(sample_texto_exact) && !is.null(sample_texto_map) && !"anyo" %in% names(sample_texto_exact)) {
  common_vars <- intersect(
    names(sample_texto_exact),
    names(sample_texto_map)
  )
  common_vars <- setdiff(common_vars, c("NCODI", "anyo", "nombre_hospital", "CODCNH", "ccaa", "grupo_pago"))
  if (length(common_vars) > 0) {
    exact_sig <- make_signature(sample_texto_exact, common_vars)
    map_sig <- make_signature(sample_texto_map, common_vars)
    map_lookup <- sample_texto_map %>%
      mutate(sig = map_sig) %>%
      group_by(sig) %>%
      mutate(sig_rank = row_number()) %>%
      ungroup() %>%
      select(sig, sig_rank, NCODI, anyo)
    sample_texto_exact <- sample_texto_exact %>%
      mutate(sig = exact_sig) %>%
      group_by(sig) %>%
      mutate(sig_rank = row_number()) %>%
      ungroup() %>%
      left_join(map_lookup, by = c("sig", "sig_rank"))
    mapping_stats <- tibble(
      n_exact = nrow(sample_texto_exact),
      n_map = nrow(sample_texto_map),
      n_exact_with_match = sum(!is.na(sample_texto_exact$anyo)),
      n_exact_without_match = sum(is.na(sample_texto_exact$anyo)),
      n_unique_sig_exact = n_distinct(exact_sig),
      n_unique_sig_map = n_distinct(map_sig)
    )
  }
}

if (!is.null(sample_texto_exact)) {
  dvar_exact <- if ("D_desc_curada" %in% names(sample_texto_exact)) "D_desc_curada" else if ("D_desc" %in% names(sample_texto_exact)) "D_desc" else NULL
  exact_year <- if (!is.null(dvar_exact)) year_summary(sample_texto_exact, dvar_exact) else tibble()
} else {
  exact_year <- tibble()
}

comparison_pdf <- exact_year %>%
  select(anyo, n_obs, n_hospitales, n_D0, n_D1, pct_D1) %>%
  right_join(target_pdf, by = "anyo") %>%
  mutate(
    diff_n = n_obs - n_target,
    diff_pct = round(pct_D1 - pct_target, 3)
  )

source_df <- bind_rows(source_rows)
missing_df <- bind_rows(missing_rows)
sample_df <- bind_rows(sample_rows)

write_csv(source_df, file.path(out_dir, "rastreo_tesis_fuentes.csv"), na = "")
write_csv(missing_df, file.path(out_dir, "rastreo_tesis_es_agudo_por_fuente.csv"), na = "")
write_csv(sample_df, file.path(out_dir, "rastreo_tesis_muestras_por_fuente.csv"), na = "")
write_csv(model_rows, file.path(out_dir, "rastreo_tesis_tipos_modelo_sfa.csv"), na = "")
write_csv(comparison_pdf, file.path(out_dir, "rastreo_tesis_sample_texto_vs_pdf.csv"), na = "")
write_csv(mapping_stats, file.path(out_dir, "rastreo_tesis_sample_texto_mapping.csv"), na = "")

txt <- c(
  "Rastreo tesis vs repo",
  "",
  sprintf("Fecha: %s", Sys.time()),
  "",
  "Modelos inspeccionados:",
  paste(
    sprintf(
      "- %s / %s / class=%s / Nobs=%s / call=%s",
      basename(model_rows$file),
      model_rows$object,
      model_rows$class,
      ifelse(is.na(model_rows$nobs_slot), "NA", as.character(model_rows$nobs_slot)),
      model_rows$call_txt
    ),
    collapse = "\n"
  ),
  "",
  "Resumen sample_texto exacta (extraida de m1$dataTable):",
  if (nrow(comparison_pdf) > 0) paste(
    sprintf(
      "- %s: n=%s (target=%s, diff=%s) pct_desc=%s (target=%s, diff=%s)",
      comparison_pdf$anyo,
      ifelse(is.na(comparison_pdf$n_obs), "NA", comparison_pdf$n_obs),
      comparison_pdf$n_target,
      ifelse(is.na(comparison_pdf$diff_n), "NA", comparison_pdf$diff_n),
      ifelse(is.na(comparison_pdf$pct_D1), "NA", comparison_pdf$pct_D1),
      comparison_pdf$pct_target,
      ifelse(is.na(comparison_pdf$diff_pct), "NA", comparison_pdf$diff_pct)
    ),
    collapse = "\n"
  ) else "- No se pudo extraer sample_texto exacta",
  "",
  "Mapping dataTable -> muestra reconstruida:",
  if (nrow(mapping_stats) > 0) paste(
    sprintf(
      "- n_exact=%s | n_map=%s | con match=%s | sin match=%s | firmas exact=%s | firmas map=%s",
      mapping_stats$n_exact,
      mapping_stats$n_map,
      mapping_stats$n_exact_with_match,
      mapping_stats$n_exact_without_match,
      mapping_stats$n_unique_sig_exact,
      mapping_stats$n_unique_sig_map
    ),
    collapse = "\n"
  ) else "- No hizo falta o no fue posible mapear la dataTable",
  "",
  "Alertas principales:",
  sprintf(
    "- sample_texto exacta total = %s frente a target PDF = 4765",
    if (!is.null(sample_texto_exact)) nrow(sample_texto_exact) else "NA"
  ),
  "- Si 2010-2011 no aparecen en sample_texto exacta, la rama local 'tesis_curada' no reproduce la tabla anual del PDF aunque si reproduzca el tipo de modelo."
)

writeLines(txt, file.path(out_dir, "rastreo_tesis_y_tipo_sfa.txt"), useBytes = TRUE)

message("Rastreo tesis y tipo SFA exportado a: ", out_dir)
