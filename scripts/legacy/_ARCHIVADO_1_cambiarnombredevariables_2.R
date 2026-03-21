config_path <- if (file.exists("scripts/00_config.R")) "scripts/00_config.R" else "00_config.R"
source(config_path)

required_pkgs <- c("tidyverse", "fs", "tzdb", "bit64")
missing_pkgs <- required_pkgs[!vapply(required_pkgs, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing_pkgs) > 0) {
  stop(
    "Faltan paquetes: ",
    paste(missing_pkgs, collapse = ", "),
    "\nInstálalos manualmente con install.packages()."
  )
}

library(tidyverse)
library(fs)
library(tzdb)
library(bit64)

STD_RAW_DIR <- file.path(INT_DIR, "standardized_raw")
dir_create(STD_RAW_DIR, recurse = TRUE)

extract_module_id <- function(path) {
  nm <- basename(path)
  m <- stringr::str_match(nm, "(^|[_\\-\\s])(\\d{2})([_\\-\\s]|\\.)")
  ifelse(is.na(m[, 3]), NA_integer_, as.integer(m[, 3]))
}

safe_to_utf8 <- function(x, from = "") {
  y <- tryCatch(
    iconv(x, from = from, to = "UTF-8", sub = "byte"),
    error = function(e) rep(NA_character_, length(x))
  )
  y[is.na(y)] <- ""
  y
}

clean_header_tokens <- function(x) {
  x <- safe_to_utf8(x, from = "")
  x <- gsub('"', "", x, fixed = TRUE, useBytes = TRUE)
  trimws(x)
}

normalize_header_tokens <- function(x) {
  x <- clean_header_tokens(x)
  x <- iconv(x, from = "UTF-8", to = "ASCII//TRANSLIT", sub = "byte")
  x[is.na(x)] <- ""
  tolower(trimws(x))
}

read_lines_with_fallback <- function(path, n = 1) {
  encodings <- c("UTF-8", "latin1", "windows-1252")

  for (enc in encodings) {
    lines_raw <- tryCatch(
      readLines(path, n = n, warn = FALSE, encoding = enc),
      error = function(e) character(0)
    )

    if (length(lines_raw) == 0) next

    lines <- safe_to_utf8(lines_raw, from = enc)
    if (any(nchar(lines, type = "bytes") > 0)) {
      return(lines)
    }
  }

  character(0)
}

is_header_line_txt <- function(line) {
  if (length(line) == 0 || nchar(line[[1]], type = "bytes") == 0) return(FALSE)

  line_safe <- safe_to_utf8(line[[1]], from = "")
  if (is.na(line_safe) || nchar(line_safe, type = "bytes") == 0) return(FALSE)

  fields <- strsplit(line_safe, ";", fixed = TRUE, useBytes = TRUE)[[1]]
  if (length(fields) <= 1) return(FALSE)

  fields_norm <- normalize_header_tokens(fields)
  marker_hit <- any(fields_norm %in% c("ncodi", "anyo", "ano"))
  var_like_count <- sum(grepl("^[a-z_][a-z0-9_]*$", fields_norm))

  marker_hit || var_like_count >= 2
}

infer_formacion_header <- function() {
  candidate_years <- c("2014", "2013", "2012", "2011", "2010")

  for (yy in candidate_years) {
    folder <- file.path(RAW_DIR, yy)
    if (!dir_exists(folder)) next

    files <- dir_ls(folder, regexp = "(?i)\\.(txt)$", recurse = FALSE)
    if (length(files) == 0) next

    for (f in files) {
      module_id <- extract_module_id(f)
      if (is.na(module_id) || module_id != 7) next

      line1 <- read_lines_with_fallback(f, n = 1)
      if (!is_header_line_txt(line1)) next

      header <- clean_header_tokens(strsplit(line1[[1]], ";", fixed = TRUE, useBytes = TRUE)[[1]])
      return(header)
    }
  }

  stop("No se pudo inferir de forma segura la cabecera para Formacion (modulo 07).")
}

load_formacion_header_2015 <- function(path, year) {
  prev_year <- as.integer(year) - 1L
  prev_dir <- file.path(RAW_DIR, as.character(prev_year))
  if (!dir_exists(prev_dir)) {
    stop("No existe carpeta del anio previo para Formacion 2015: ", prev_dir)
  }

  prev_files <- dir_ls(prev_dir, regexp = "(?i)\\.(txt)$", recurse = FALSE)
  module_ids <- extract_module_id(prev_files)
  prev_files <- sort(prev_files[!is.na(module_ids) & module_ids == 7L])
  if (length(prev_files) == 0) {
    stop("No se encontro archivo modulo 07 en anio previo para Formacion 2015.")
  }

  source_file <- prev_files[[1]]
  if (length(prev_files) > 1) {
    formacion_hits <- prev_files[
      stringr::str_detect(
        stringr::str_to_lower(basename(prev_files)),
        "formaci"
      )
    ]
    if (length(formacion_hits) == 1) {
      source_file <- formacion_hits[[1]]
    } else {
      stop("Hay varios candidatos modulo 07 en anio previo y no son univocos.")
    }
  }

  prev_line <- read_lines_with_fallback(source_file, n = 1)
  if (length(prev_line) == 0 || !nzchar(prev_line[[1]])) {
    stop("No se pudo leer cabecera de Formacion en archivo previo: ", source_file)
  }
  header <- clean_header_tokens(strsplit(prev_line[[1]], ";", fixed = TRUE)[[1]])

  current_line <- read_lines_with_fallback(path, n = 1)
  if (length(current_line) == 0 || !nzchar(current_line[[1]])) {
    stop("No se pudo leer primera fila de datos en Formacion 2015: ", path)
  }
  current_fields <- clean_header_tokens(strsplit(current_line[[1]], ";", fixed = TRUE)[[1]])

  if (length(header) != 112L) {
    stop("Cabecera de Formacion del anio previo no tiene 112 campos: ", length(header))
  }
  if (length(current_fields) != 112L) {
    stop("Primera fila de Formacion 2015 no tiene 112 campos: ", length(current_fields))
  }
  if (length(header) != length(current_fields)) {
    stop(
      "No coincide numero de campos entre cabecera previa y Formacion 2015: ",
      length(header), " vs ", length(current_fields)
    )
  }

  list(header = header, source_file = source_file)
}

read_txt_file <- function(path, year) {
  module_id <- extract_module_id(path)
  line1 <- read_lines_with_fallback(path, n = 1)

  has_header <- is_header_line_txt(line1)

  if (!has_header && year == 2015 && !is.na(module_id) && module_id == 7) {
    df <- readr::read_delim(
      path,
      delim = ";",
      col_names = load_formacion_header_2015(path, year)$header,
      locale = locale(decimal_mark = ","),
      show_col_types = FALSE,
      progress = FALSE
    )
    return(df)
  }

  if (!has_header) {
    stop("Archivo TXT sin cabecera no reconocido: ", path)
  }

  readr::read_delim(
    path,
    delim = ";",
    locale = locale(decimal_mark = ","),
    show_col_types = FALSE,
    progress = FALSE
  )
}

read_xml_file <- function(path) {
  lines <- tryCatch(
    readLines(path, warn = FALSE, encoding = "UTF-8"),
    error = function(e) character(0)
  )
  if (length(lines) == 0) stop("No se pudo leer XML: ", path)

  record_tag <- NA_character_
  for (ln in lines) {
    ln <- trimws(ln)
    if (grepl("^<\\?xml", ln)) next
    if (grepl("^<dataroot", ln)) next
    if (grepl("^<[^/!?][^>]*>$", ln)) {
      tg <- sub("^<([^>]+)>$", "\\1", ln)
      if (!grepl("^dataroot$", tg, ignore.case = TRUE)) {
        record_tag <- tg
        break
      }
    }
  }

  if (is.na(record_tag)) {
    stop("No se encontró nodo de registro en XML: ", path)
  }

  rows <- list()
  current <- list()
  in_record <- FALSE

  for (ln in lines) {
    ln <- trimws(ln)

    if (!in_record && grepl(paste0("^<", record_tag, ">$"), ln)) {
      in_record <- TRUE
      current <- list()
      next
    }

    if (in_record && grepl(paste0("^</", record_tag, ">$"), ln)) {
      rows[[length(rows) + 1]] <- current
      in_record <- FALSE
      next
    }

    if (in_record && grepl("^<[^/!?][^>]*>.*</[^>]+>$", ln)) {
      tag <- sub("^<([^>]+)>.*$", "\\1", ln)
      val <- sub("^<[^>]+>(.*)</[^>]+>$", "\\1", ln)
      val <- gsub("&amp;", "&", val, fixed = TRUE)
      val <- gsub("&lt;", "<", val, fixed = TRUE)
      val <- gsub("&gt;", ">", val, fixed = TRUE)
      val <- gsub("&quot;", '"', val, fixed = TRUE)
      val <- gsub("&apos;", "'", val, fixed = TRUE)
      current[[tag]] <- val
    }
  }

  if (length(rows) == 0) {
    stop("No se pudieron extraer registros del XML: ", path)
  }

  dplyr::bind_rows(lapply(rows, tibble::as_tibble))
}

standardize_names <- function(df) {
  names(df) <- trimws(safe_to_utf8(names(df), from = ""))

  names_norm <- tolower(iconv(names(df), from = "UTF-8", to = "ASCII//TRANSLIT", sub = "byte"))
  names_norm[is.na(names_norm)] <- ""
  names(df)[names_norm %in% c("ano", "anyo")] <- "anyo"
  names(df)[tolower(names(df)) == "year"] <- "anyo"

  names(df)[names(df) == "camas_instaladas"] <- "camas_instalada"
  names(df)[names(df) == "camas instaladas"] <- "camas_instalada"
  names(df)[names(df) == "Camas_instaladas"] <- "camas_instalada"
  names(df)[names(df) == "Camas instaladas"] <- "camas_instalada"

  names(df)[names(df) == "Cod CCAA (Todas)"] <- "ccaa_codigo"
  names(df)[names(df) == "ccaa_Codigo"] <- "ccaa_codigo"
  names(df)[names(df) == "Desc CCAA (Todas)"] <- "ccaa"

  names(df)[names(df) == "Cod Grupo Finalidad"] <- "cod_finalidad_agrupada"
  names(df)[names(df) == "FINALIDAD agrupada para Anonimizacion"] <- "Finalidad_agrupada"

  names(df)[names(df) == "Cod Pertenencia SNS"] <- "cod_depend_agrupada"
  names(df)[names(df) == "Desc Pertenencia SNS"] <- "Depend_agrupada"

  names(df)[names(df) == "60_totalCompra"] <- "G_totalCompra"
  names(df)[names(df) == "61_variaExistencias"] <- "G_variaExistencias"
  names(df)[names(df) == "62_servExteriores"] <- "G_servExteriores"
  names(df)[names(df) == "64_gastoPersonal"] <- "G_gastoPersonal"
  names(df)[names(df) == "68_dotaAmortizacion"] <- "G_dotaAmortizacion"
  names(df)[names(df) == "69_perdidaDeterioro"] <- "G_perdidaDeterioro"
  names(df)[names(df) == "6X_restoGasto"] <- "G_restoGasto"
  names(df)[names(df) == "tot_compraGasto"] <- "G_totGastos"

  names(df)[names(df) == "70_tIngresos"] <- "I_totIngresosPS"
  names(df)[names(df) == "700_particular"] <- "I_particular"
  names(df)[names(df) == "701_AsegPriv"] <- "I_AsegPriv"
  names(df)[names(df) == "701_1_AsistSanitaria"] <- "I_AsistSanitaria"
  names(df)[names(df) == "701_2_AccTrafic"] <- "I_AccTrafic"
  names(df)[names(df) == "702_MATEPSS"] <- "I_MATEPSS"
  names(df)[names(df) == "704_SNS"] <- "I_SNS"
  names(df)[names(df) == "705_1_FdirectaSS"] <- "I_FdirectaSS"
  names(df)[names(df) == "705_2_FdirectaAPriv_MATEPSS"] <- "I_FdirectaAPriv_MATEPSS"
  names(df)[names(df) == "706_OyEntiPublica"] <- "I_OyEntiPublica"
  names(df)[names(df) == "708_bonificaciones"] <- "I_bonificaciones"
  names(df)[names(df) == "709_Otros_Ips"] <- "I_Otros_Ips"
  names(df)[names(df) == "74_Total_Subvencion"] <- "I_Total_Subvencion"
  names(df)[names(df) == "7X_restoIngresos"] <- "I_restoIngresos"
  names(df)[names(df) == "total_ventasIngresos"] <- "I_totIngresos"

  df
}

year_dirs <- dir_ls(RAW_DIR, type = "directory", recurse = FALSE)
years <- year_dirs %>%
  path_file() %>%
  stringr::str_extract("^\\d{4}$") %>%
  stats::na.omit() %>%
  sort()

for (yy in years) {
  year_num <- as.integer(yy)
  in_dir <- file.path(RAW_DIR, yy)
  out_dir <- file.path(STD_RAW_DIR, yy)
  dir_create(out_dir, recurse = TRUE)

  files <- dir_ls(in_dir, recurse = FALSE)
  files <- files[stringr::str_detect(files, "(?i)\\.(txt|xml)$")]

  for (f in files) {
    ext <- tolower(tools::file_ext(f))

    df <- if (ext == "txt") {
      read_txt_file(f, year_num)
    } else {
      read_xml_file(f)
    }

    df <- df %>%
      mutate(across(where(is.character), ~ iconv(., from = "", to = "UTF-8")))

    df <- standardize_names(df)

    if (!("anyo" %in% names(df))) {
      df$anyo <- year_num
    } else {
      df$anyo <- ifelse(is.na(df$anyo) | df$anyo == "", year_num, df$anyo)
    }

    if ("NCODI" %in% names(df)) {
      df$NCODI <- as.character(df$NCODI)
    }

    out_name <- paste0(tools::file_path_sans_ext(safe_to_utf8(basename(f), from = "")), ".txt")
    out_path <- file.path(out_dir, out_name)

    write_delim(df, out_path, delim = ";", quote = "all")
  }
}