config_path <- if (file.exists("scripts/00_config.R")) "scripts/00_config.R" else "00_config.R"
source(config_path)

required_pkgs <- c("dplyr", "readr", "stringr")
missing_pkgs <- required_pkgs[!vapply(required_pkgs, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing_pkgs) > 0) {
  stop("Faltan paquetes: ", paste(missing_pkgs, collapse = ", "), call. = FALSE)
}

library(dplyr)
library(readr)
library(stringr)

message("\n=== Reconstruccion D_desc_cnh desde raw CNH ===")

df_path <- file.path(LEGACY_BASE_DIR, "df_final.RData")
if (!file.exists(df_path)) stop("No existe df_final base: ", df_path, call. = FALSE)
load(df_path)

normalize_id <- function(x) {
  x <- as.character(x)
  x <- trimws(x)
  x[x %in% c("", "NA", "NULL")] <- NA_character_
  x
}

read_delim_guess <- function(path) {
  encs <- c("UTF-8", "UTF-16LE", "latin1", "windows-1252")
  for (enc in encs) {
    first_line <- tryCatch(readLines(path, n = 1, warn = FALSE, encoding = enc), error = function(e) NULL)
    if (is.null(first_line) || length(first_line) == 0) next
    delim <- if (grepl(";", iconv(first_line, from = enc, to = "UTF-8", sub = ""), fixed = TRUE)) ";" else "\t"
    all_lines <- tryCatch(readLines(path, warn = FALSE, encoding = enc), error = function(e) NULL)
    if (is.null(all_lines)) next
    all_lines <- iconv(all_lines, from = enc, to = "UTF-8", sub = "")
    tc <- textConnection(all_lines)
    dat <- tryCatch(
      utils::read.delim(
        tc,
        sep = delim,
        header = TRUE,
        quote = "\"",
        check.names = FALSE,
        stringsAsFactors = FALSE
      ),
      error = function(e) NULL
    )
    close(tc)
    if (!is.null(dat)) {
      names(dat) <- iconv(names(dat), from = "UTF-8", to = "ASCII//TRANSLIT", sub = "")
      return(dat)
    }
  }
  stop("No pude leer el archivo: ", path, call. = FALSE)
}

read_noheader_guess <- function(path, delim) {
  encs <- c("UTF-8", "UTF-16LE", "latin1", "windows-1252")
  for (enc in encs) {
    all_lines <- tryCatch(readLines(path, warn = FALSE, encoding = enc), error = function(e) NULL)
    if (is.null(all_lines)) next
    all_lines <- iconv(all_lines, from = enc, to = "UTF-8", sub = "")
    tc <- textConnection(all_lines)
    dat <- tryCatch(
      utils::read.delim(
        tc,
        sep = delim,
        header = FALSE,
        quote = "\"",
        check.names = FALSE,
        stringsAsFactors = FALSE
      ),
      error = function(e) NULL
    )
    close(tc)
    if (!is.null(dat) && ncol(dat) > 1) return(dat)
  }
  stop("No pude leer sin cabecera: ", path, call. = FALSE)
}

find_col <- function(nms, patterns) {
  for (p in patterns) {
    hit <- grep(p, nms, ignore.case = TRUE, value = TRUE)[1]
    if (!is.na(hit)) return(hit)
  }
  NA_character_
}

build_pre2019 <- function(year) {
  dir_y <- file.path(RAW_DIR, "CNH", as.character(year))
  data_file <- list.files(dir_y, pattern = "^CNH[ _].*\\.txt$|^CNH 20.*\\.txt$", full.names = TRUE, ignore.case = TRUE)[1]
  dict_file <- file.path(dir_y, "CH_FUNCIONAL.txt")
  if (is.na(data_file) || !file.exists(data_file) || !file.exists(dict_file)) return(NULL)

  if (year <= 2014) {
    dat <- read_noheader_guess(data_file, ";")
    codcnh_col <- names(dat)[1]
    codfu_col <- names(dat)[14]
    descfu_col <- NA_character_
  } else if (year == 2015) {
    dat <- read_noheader_guess(data_file, "\t")
    codcnh_col <- names(dat)[1]
    codfu_col <- names(dat)[19]
    descfu_col <- names(dat)[20]
  } else if (year == 2016) {
    tmp_utf8 <- tempfile(fileext = ".txt")
    cmd <- sprintf(
      "Get-Content -LiteralPath '%s' | Set-Content -LiteralPath '%s' -Encoding utf8",
      gsub("'", "''", data_file),
      gsub("'", "''", tmp_utf8)
    )
    system2("powershell", c("-NoProfile", "-Command", cmd), stdout = TRUE, stderr = TRUE)
    dat <- read_delim_guess(tmp_utf8)
    names(dat) <- make.names(names(dat))
    codcnh_col <- find_col(names(dat), c("^CODCNH$", "CODCNH"))
    codfu_col <- find_col(names(dat), c("^CODFU$", "CODFU"))
    descfu_col <- find_col(names(dat), c("DEPENDENCIA.FUNCIONAL", "DESFU"))
  } else {
    dat <- read_delim_guess(data_file)
    names(dat) <- make.names(names(dat))
    codcnh_col <- find_col(names(dat), c("^CODCNH$", "CODCNH"))
    codfu_col <- find_col(names(dat), c("^CODFU$", "CODFU"))
    descfu_col <- find_col(names(dat), c("DEPENDENCIA.FUNCIONAL", "DESFU"))
  }

  dic <- read_delim_guess(dict_file)
  names(dic) <- make.names(names(dic))
  dic_cod_col <- find_col(names(dic), c("^CODFU$", "CODFU"))
  dic_desc_col <- find_col(names(dic), c("^DESFU$", "DESFU"))

  if (is.na(codcnh_col) || is.na(codfu_col)) {
    stop(
      "No pude identificar CODCNH/CODFU en CNH ", year,
      " | archivo=", basename(data_file),
      " | cols=", paste(names(dat), collapse = " | "),
      call. = FALSE
    )
  }

  out <- dat %>%
    transmute(
      anyo = year,
      CODCNH = normalize_id(.data[[codcnh_col]]),
      cod_dep_cnh_raw = suppressWarnings(as.integer(.data[[codfu_col]])),
      dep_desc_raw = if (!is.na(descfu_col)) as.character(.data[[descfu_col]]) else NA_character_
    )

  if (all(is.na(out$dep_desc_raw)) && !is.na(dic_cod_col) && !is.na(dic_desc_col)) {
    dic2 <- dic %>%
      transmute(
        cod_dep_cnh_raw = suppressWarnings(as.integer(.data[[dic_cod_col]])),
        dep_desc_raw = as.character(.data[[dic_desc_col]])
      ) %>%
      distinct(cod_dep_cnh_raw, .keep_all = TRUE)
    out <- out %>% left_join(dic2, by = "cod_dep_cnh_raw", suffix = c("", "_dict")) %>%
      mutate(dep_desc_raw = coalesce(dep_desc_raw, dep_desc_raw_dict)) %>%
      select(-dep_desc_raw_dict)
  }

  out %>%
    mutate(
      D_desc_cnh_raw = case_when(
        cod_dep_cnh_raw %in% c(20L, 21L, 22L, 23L) ~ 1L,
        !is.na(cod_dep_cnh_raw) ~ 0L,
        TRUE ~ NA_integer_
      ),
      fuente_cnh_raw = paste0("CNH_raw_", year)
    ) %>%
    distinct(anyo, CODCNH, .keep_all = TRUE)
}

build_post2019 <- function(year) {
  dir_y <- file.path(RAW_DIR, "CNH", as.character(year))
  data_file <- list.files(dir_y, pattern = "DIRECTORIO.*HOSPITALES.*\\.txt$", full.names = TRUE, ignore.case = TRUE)[1]
  if (is.na(data_file) || !file.exists(data_file)) return(NULL)
  if (year == 2020) {
    tmp_utf8 <- tempfile(fileext = ".txt")
    cmd <- sprintf(
      "Get-Content -LiteralPath '%s' | Set-Content -LiteralPath '%s' -Encoding utf8",
      gsub("'", "''", data_file),
      gsub("'", "''", tmp_utf8)
    )
    system2("powershell", c("-NoProfile", "-Command", cmd), stdout = TRUE, stderr = TRUE)
    dat <- read_delim_guess(tmp_utf8)
  } else {
    dat <- read_delim_guess(data_file)
  }
  names(dat) <- make.names(names(dat))
  codcnh_col <- find_col(names(dat), c("^CODCNH$", "CODCNH"))
  depcode_col <- find_col(names(dat), c("Cd..Dep..Funcional", "C.d..Dep..Funcional", "Cod..Dep..Funcional", "cod.dep", "^Cod.*Dep.*Funcional$"))
  depdesc_col <- find_col(names(dat), c("^Dependencia.Funcional$", "Dependencia.Funcional"))

  if (is.na(codcnh_col) || is.na(depcode_col)) {
    stop(
      "No pude identificar CODCNH/cod.dep en CNH ", year,
      " | archivo=", basename(data_file),
      " | cols=", paste(names(dat), collapse = " | "),
      call. = FALSE
    )
  }

  dat %>%
    transmute(
      anyo = year,
      CODCNH = normalize_id(.data[[codcnh_col]]),
      cod_dep_cnh_raw = suppressWarnings(as.integer(.data[[depcode_col]])),
      dep_desc_raw = if (!is.na(depdesc_col)) as.character(.data[[depdesc_col]]) else NA_character_
    ) %>%
    mutate(
      D_desc_cnh_raw = case_when(
        cod_dep_cnh_raw %in% c(20L, 21L, 22L) ~ 1L,
        !is.na(cod_dep_cnh_raw) ~ 0L,
        TRUE ~ NA_integer_
      ),
      fuente_cnh_raw = paste0("CNH_raw_", year)
    ) %>%
    distinct(anyo, CODCNH, .keep_all = TRUE)
}

pre_list <- lapply(2010:2018, build_pre2019)
post_list <- lapply(2020:2023, build_post2019)
cnh_panel <- bind_rows(pre_list, post_list)

if (!any(cnh_panel$anyo == 2020)) stop("No pude leer 2020 para imputar 2019.", call. = FALSE)

cnh_2019 <- cnh_panel %>%
  filter(anyo == 2020) %>%
  mutate(anyo = 2019L, fuente_cnh_raw = "imputado_desde_2020")

cnh_panel <- bind_rows(cnh_panel, cnh_2019) %>%
  arrange(CODCNH, anyo)

df_cmp <- df_final %>%
  mutate(
    anyo = suppressWarnings(as.integer(anyo)),
    NCODI = normalize_id(NCODI),
    CODCNH = normalize_id(CODCNH),
    D_desc_siae = case_when(
      cod_depend_agrupada == 1 ~ 0L,
      cod_depend_agrupada == 2 ~ 1L,
      TRUE ~ NA_integer_
    )
  ) %>%
  left_join(cnh_panel, by = c("anyo", "CODCNH")) %>%
  mutate(
    coincide_siae_cnh_raw = case_when(
      is.na(D_desc_siae) | is.na(D_desc_cnh_raw) ~ NA,
      D_desc_siae == D_desc_cnh_raw ~ 1L,
      TRUE ~ 0L
    ),
    D_desc_cnh_existing = if ("D_desc_cnh" %in% names(.)) D_desc_cnh else NA_integer_
  )

out_dir <- file.path(INT_DIR, "cnh_raw_compare")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

write.csv(cnh_panel,
          file.path(out_dir, "cnh_raw_dependencia_panel_2010_2023.csv"),
          row.names = FALSE, na = "")

write.csv(df_cmp %>%
            select(NCODI, anyo, CODCNH, cod_depend_agrupada, D_desc_siae,
                   D_desc_cnh_existing, D_desc_cnh_raw, dep_desc_raw,
                   fuente_cnh_raw, coincide_siae_cnh_raw),
          file.path(out_dir, "comparacion_siae_vs_cnh_raw_hospital_anio.csv"),
          row.names = FALSE, na = "")

summary_year <- df_cmp %>%
  filter(!is.na(CODCNH)) %>%
  group_by(anyo) %>%
  summarise(
    n_match_codcnh = n(),
    n_cnh_raw = sum(!is.na(D_desc_cnh_raw)),
    n_siae = sum(!is.na(D_desc_siae)),
    n_coinciden = sum(coincide_siae_cnh_raw == 1, na.rm = TRUE),
    n_discrepan = sum(coincide_siae_cnh_raw == 0, na.rm = TRUE),
    pct_coinciden = round(100 * n_coinciden / pmax(1, n_coinciden + n_discrepan), 1),
    .groups = "drop"
  )

summary_codes <- df_cmp %>%
  filter(!is.na(CODCNH), !is.na(cod_dep_cnh_raw)) %>%
  count(anyo, cod_dep_cnh_raw, dep_desc_raw, D_desc_cnh_raw, name = "n_obs") %>%
  arrange(anyo, cod_dep_cnh_raw)

summary_global <- df_cmp %>%
  filter(!is.na(CODCNH)) %>%
  summarise(
    n_obs_match = n(),
    n_hospitales_match = n_distinct(NCODI),
    n_codcnh_match = n_distinct(CODCNH),
    n_cnh_raw = sum(!is.na(D_desc_cnh_raw)),
    n_siae = sum(!is.na(D_desc_siae)),
    n_coinciden = sum(coincide_siae_cnh_raw == 1, na.rm = TRUE),
    n_discrepan = sum(coincide_siae_cnh_raw == 0, na.rm = TRUE),
    pct_coinciden = round(100 * n_coinciden / pmax(1, n_coinciden + n_discrepan), 1)
  )

write.csv(summary_year,
          file.path(out_dir, "resumen_anual_siae_vs_cnh_raw.csv"),
          row.names = FALSE, na = "")
write.csv(summary_codes,
          file.path(out_dir, "resumen_codigos_cnh_raw_por_anio.csv"),
          row.names = FALSE, na = "")
write.csv(summary_global,
          file.path(out_dir, "resumen_global_siae_vs_cnh_raw.csv"),
          row.names = FALSE, na = "")

message("Panel CNH raw guardado en: ", file.path(out_dir, "cnh_raw_dependencia_panel_2010_2023.csv"))
message("Comparacion hospital-año guardada en: ", file.path(out_dir, "comparacion_siae_vs_cnh_raw_hospital_anio.csv"))
message("Resumen anual guardado en: ", file.path(out_dir, "resumen_anual_siae_vs_cnh_raw.csv"))
message("Resumen codigos guardado en: ", file.path(out_dir, "resumen_codigos_cnh_raw_por_anio.csv"))
message("Resumen global guardado en: ", file.path(out_dir, "resumen_global_siae_vs_cnh_raw.csv"))
