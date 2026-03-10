# encoding: UTF-8
# ================================
# 1_cambiarnombredevariables_2.R
# Lee los TXT/XML anuales de RAW_DIR, estandariza nombres de columnas
# y guarda los archivos procesados en INT_DIR/standardized_raw/
# ================================

config_path <- if (file.exists("scripts/00_config.R")) "scripts/00_config.R" else "00_config.R"
source(config_path)

required_pkgs <- c("tidyverse", "tzdb", "bit64")
missing_pkgs <- required_pkgs[!vapply(required_pkgs, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing_pkgs) > 0) {
  stop(
    "Faltan paquetes: ",
    paste(missing_pkgs, collapse = ", "),
    "\nInstalalos manualmente con install.packages()."
  )
}

library(tidyverse)
library(tzdb)
library(bit64)

STD_RAW_DIR <- file.path(INT_DIR, "standardized_raw")
dir.create(STD_RAW_DIR, recursive = TRUE, showWarnings = FALSE)

# ── Utilidades de codificación ───────────────────────────────────────────────

extract_module_id <- function(path) {
  # Usamos base R (regmatches + regexpr) en lugar de stringr::str_match
  # para evitar el fallo "invalid UTF-8" cuando los nombres de archivo
  # contienen caracteres especiales en Windows.
  nm <- basename(path)
  # Forzamos a bytes para que grepl/regexpr no intenten validar UTF-8
  m <- regmatches(nm, regexpr("(^|[_\\-\\s])(\\d{2})([_\\-\\s]|\\.)", nm, perl = FALSE))
  result <- rep(NA_integer_, length(nm))
  hit <- nchar(m) > 0
  if (any(hit)) {
    digits <- regmatches(m[hit], regexpr("\\d{2}", m[hit]))
    result[hit] <- as.integer(digits)
  }
  result
}

# Convierte x a UTF-8 de forma segura; nunca falla, devuelve "" si no puede
safe_to_utf8 <- function(x, from = "") {
  y <- tryCatch(
    iconv(x, from = from, to = "UTF-8", sub = ""),   # sub="" descarta bytes
    error = function(e) rep(NA_character_, length(x)) #   invalidos en vez de
  )                                                   #   sustituirlos por <xx>
  y[is.na(y)] <- ""
  y
}

clean_header_tokens <- function(x) {
  x <- safe_to_utf8(x, from = "")
  x <- gsub('"', "", x, fixed = TRUE, useBytes = TRUE)
  trimws(x)
}

# CORRECCIÓN: sub = "byte" devuelve strings con secuencias "<xx>" que luego
# hacen fallar a sub(..., perl=TRUE). Cambiamos a sub = "" (descarta el char)
# y quitamos perl = TRUE de los grepl posteriores (innecesario para ASCII).
normalize_header_tokens <- function(x) {
  x <- clean_header_tokens(x)
  x <- iconv(x, from = "UTF-8", to = "ASCII//TRANSLIT", sub = "")
  x[is.na(x)] <- ""
  tolower(trimws(x))
}

is_header_line_txt <- function(line) {
  if (length(line) == 0 || nchar(line[[1]], type = "bytes") == 0) return(FALSE)

  line_safe <- safe_to_utf8(line[[1]], from = "")
  if (is.na(line_safe) || nchar(line_safe, type = "bytes") == 0) return(FALSE)

  fields <- strsplit(line_safe, ";", fixed = TRUE, useBytes = TRUE)[[1]]
  if (length(fields) <= 1) return(FALSE)

  fields_norm   <- normalize_header_tokens(fields)
  marker_hit    <- any(fields_norm %in% c("ncodi", "anyo", "ano"))
  var_like_count <- sum(grepl("^[a-z_][a-z0-9_]*$", fields_norm))

  marker_hit || var_like_count >= 2
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
    if (any(nchar(lines, type = "bytes") > 0)) return(lines)
  }
  character(0)
}

infer_formacion_header <- function() {
  candidate_years <- c("2014", "2013", "2012", "2011", "2010")

  for (yy in candidate_years) {
    folder <- file.path(RAW_DIR, yy)
    if (!dir.exists(folder)) next

    files <- list.files(folder, pattern = "\\.txt$", full.names = TRUE, ignore.case = TRUE)
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
  stop("No se pudo inferir la cabecera para Formacion (modulo 07).")
}

load_formacion_header_2015 <- function(path, year) {
  prev_year <- as.integer(year) - 1L
  prev_dir  <- file.path(RAW_DIR, as.character(prev_year))
  if (!dir.exists(prev_dir))
    stop("No existe carpeta del anio previo para Formacion 2015: ", prev_dir)

  prev_files <- list.files(prev_dir, pattern = "\\.txt$", full.names = TRUE, ignore.case = TRUE)
  module_ids <- extract_module_id(prev_files)
  prev_files <- sort(prev_files[!is.na(module_ids) & module_ids == 7L])
  if (length(prev_files) == 0)
    stop("No se encontro archivo modulo 07 en anio previo para Formacion 2015.")

  source_file <- prev_files[[1]]
  if (length(prev_files) > 1) {
    formacion_hits <- prev_files[
      grepl("formaci", tolower(basename(prev_files)), fixed = TRUE)]
    if (length(formacion_hits) == 1) {
      source_file <- formacion_hits[[1]]
    } else {
      stop("Hay varios candidatos modulo 07 en anio previo y no son univocos.")
    }
  }

  prev_line <- read_lines_with_fallback(source_file, n = 1)
  if (length(prev_line) == 0 || !nzchar(prev_line[[1]]))
    stop("No se pudo leer cabecera de Formacion en archivo previo: ", source_file)

  header <- clean_header_tokens(strsplit(prev_line[[1]], ";", fixed = TRUE)[[1]])

  current_line <- read_lines_with_fallback(path, n = 1)
  if (length(current_line) == 0 || !nzchar(current_line[[1]]))
    stop("No se pudo leer primera fila de Formacion 2015: ", path)

  current_fields <- clean_header_tokens(strsplit(current_line[[1]], ";", fixed = TRUE)[[1]])

  if (length(header) != 112L)
    stop("Cabecera de Formacion del anio previo no tiene 112 campos: ", length(header))
  if (length(current_fields) != 112L)
    stop("Primera fila de Formacion 2015 no tiene 112 campos: ", length(current_fields))

  list(header = header, source_file = source_file)
}

read_txt_file <- function(path, year) {
  module_id <- extract_module_id(path)
  line1     <- read_lines_with_fallback(path, n = 1)
  has_header <- is_header_line_txt(line1)

  if (!has_header && year == 2015 && !is.na(module_id) && module_id == 7) {
    df <- readr::read_delim(
      path,
      delim          = ";",
      col_names      = load_formacion_header_2015(path, year)$header,
      locale         = locale(decimal_mark = ","),
      show_col_types = FALSE,
      progress       = FALSE
    )
    return(df)
  }

  if (!has_header)
    stop("Archivo TXT sin cabecera no reconocido: ", path)

  readr::read_delim(
    path,
    delim          = ";",
    locale         = locale(decimal_mark = ","),
    show_col_types = FALSE,
    progress       = FALSE
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
    if (grepl("^<\\?xml", ln))    next
    if (grepl("^<dataroot", ln))  next
    if (grepl("^<[^/!?][^>]*>$", ln)) {
      tg <- sub("^<([^>]+)>$", "\\1", ln)
      if (!grepl("^dataroot$", tg, ignore.case = TRUE)) {
        record_tag <- tg
        break
      }
    }
  }
  if (is.na(record_tag)) stop("No se encontro nodo de registro en XML: ", path)

  rows       <- list()
  current    <- list()
  in_record  <- FALSE

  for (ln in lines) {
    ln <- trimws(ln)
    if (!in_record && grepl(paste0("^<", record_tag, ">$"), ln)) {
      in_record <- TRUE
      current   <- list()
      next
    }
    if (in_record && grepl(paste0("^</", record_tag, ">$"), ln)) {
      rows[[length(rows) + 1]] <- current
      in_record <- FALSE
      next
    }
    if (in_record && grepl("^<[^/!?][^>]*>.*</[^>]+>$", ln)) {
      tag <- sub("^<([^>]+)>.*$",      "\\1", ln)
      val <- sub("^<[^>]+>(.*)</[^>]+>$", "\\1", ln)
      val <- gsub("&amp;",  "&",  val, fixed = TRUE)
      val <- gsub("&lt;",   "<",  val, fixed = TRUE)
      val <- gsub("&gt;",   ">",  val, fixed = TRUE)
      val <- gsub("&quot;", '"',  val, fixed = TRUE)
      val <- gsub("&apos;", "'",  val, fixed = TRUE)
      current[[tag]] <- val
    }
  }

  if (length(rows) == 0) stop("No se pudieron extraer registros del XML: ", path)
  dplyr::bind_rows(lapply(rows, tibble::as_tibble))
}

# ── Estandarización de nombres de columna ─────────────────────────────────────
#
# CORRECCIÓN PRINCIPAL:
# El script original comparaba nombres como "AÃÂ±o" (strings corruptos por
# doble codificación Windows-1252 → UTF-8). Eso hace que sub(perl=TRUE) falle
# con "input string is invalid UTF-8".
#
# Solución: normalizar AMBOS lados a ASCII antes de comparar. La función
# to_ascii() convierte "Año", "AÃ±o", "AÃÂ±o" y cualquier variante al
# mismo "Ano", haciendo las comparaciones independientes de la codificación.

to_ascii <- function(x) {
  x <- safe_to_utf8(x)                                       # garantiza UTF-8
  x <- iconv(x, from = "UTF-8", to = "ASCII//TRANSLIT", sub = "")
  tolower(trimws(x))
}

standardize_names <- function(df) {
  nms       <- trimws(names(df))
  nms_ascii <- to_ascii(nms)   # versión normalizada para comparar

  # Año / year → anyo
  nms[nms_ascii %in% c("ano", "anyo", "year")] <- "anyo"

  # Variantes de camas instaladas
  nms[nms_ascii %in% c("camas_instaladas", "camas instaladas")] <- "camas_instalada"

  # Identificadores geográficos
  nms[nms_ascii == "cod ccaa (todas)"]  <- "ccaa_codigo"
  nms[nms_ascii == "ccaa_codigo"]       <- "ccaa_codigo"   # ya correcto, por si acaso
  nms[nms_ascii == "desc ccaa (todas)"] <- "ccaa"

  # Finalidad
  nms[nms_ascii == "cod grupo finalidad"]                    <- "cod_finalidad_agrupada"
  nms[nms_ascii == "finalidad agrupada para anonimizacion"]  <- "Finalidad_agrupada"

  # Dependencia / SNS
  nms[nms_ascii == "cod pertenencia sns"]  <- "cod_depend_agrupada"
  nms[nms_ascii == "desc pertenencia sns"] <- "Depend_agrupada"

  # Gastos
  nms[nms_ascii == "60_totalcompra"]       <- "G_totalCompra"
  nms[nms_ascii == "61_variaexistencias"]  <- "G_variaExistencias"
  nms[nms_ascii == "62_servexteriores"]    <- "G_servExteriores"
  nms[nms_ascii == "64_gastopersonal"]     <- "G_gastoPersonal"
  nms[nms_ascii == "68_dotaamortizacion"]  <- "G_dotaAmortizacion"
  nms[nms_ascii == "69_perdidadeterioro"]  <- "G_perdidaDeterioro"
  nms[nms_ascii == "6x_restogasto"]        <- "G_restoGasto"
  nms[nms_ascii == "tot_compragasto"]      <- "G_totGastos"

  # Ingresos
  nms[nms_ascii == "70_tingresos"]                 <- "I_totIngresosPS"
  nms[nms_ascii == "700_particular"]               <- "I_particular"
  nms[nms_ascii == "701_asegpriv"]                 <- "I_AsegPriv"
  nms[nms_ascii == "701_1_asistsanitaria"]         <- "I_AsistSanitaria"
  nms[nms_ascii == "701_2_acctrafic"]              <- "I_AccTrafic"
  nms[nms_ascii == "702_matepss"]                  <- "I_MATEPSS"
  nms[nms_ascii == "704_sns"]                      <- "I_SNS"
  nms[nms_ascii == "705_1_fdirectass"]             <- "I_FdirectaSS"
  nms[nms_ascii == "705_2_fdirectaapriv_matepss"]  <- "I_FdirectaAPriv_MATEPSS"
  nms[nms_ascii == "706_oyentipublica"]            <- "I_OyEntiPublica"
  nms[nms_ascii == "708_bonificaciones"]           <- "I_bonificaciones"
  nms[nms_ascii == "709_otros_ips"]                <- "I_Otros_Ips"
  nms[nms_ascii == "74_total_subvencion"]          <- "I_Total_Subvencion"
  nms[nms_ascii == "7x_restoingresos"]             <- "I_restoIngresos"
  nms[nms_ascii == "total_ventasingresos"]         <- "I_totIngresos"

  names(df) <- nms
  df
}

# ── Bucle principal: leer, estandarizar y guardar ────────────────────────────

year_dirs <- list.dirs(RAW_DIR, full.names = TRUE, recursive = FALSE)
# Usamos base R para extraer años: evita fallo UTF-8 de stringr en rutas Windows
year_names <- basename(year_dirs)
years <- sort(year_names[grepl("^[0-9]{4}$", year_names, perl = FALSE)])

if (length(years) == 0) stop("No se encontraron carpetas de anios en RAW_DIR: ", RAW_DIR)
message("Anos detectados: ", paste(years, collapse = ", "))

for (yy in years) {
  year_num <- as.integer(yy)
  in_dir   <- file.path(RAW_DIR, yy)
  out_dir  <- file.path(STD_RAW_DIR, yy)
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

  files <- list.files(in_dir, full.names = TRUE, recursive = FALSE)
  files <- files[grepl("\\.(txt|xml)$", files, ignore.case = TRUE, perl = FALSE)]

  if (length(files) == 0) {
    message("  [", yy, "] Sin archivos TXT/XML, se omite.")
    next
  }

  for (f in files) {
    ext <- tolower(tools::file_ext(f))

    df <- tryCatch(
      {
        if (ext == "txt") read_txt_file(f, year_num) else read_xml_file(f)
      },
      error = function(e) {
        message("  [", yy, "] ERROR leyendo ", basename(f), ": ", conditionMessage(e))
        NULL
      }
    )
    if (is.null(df)) next

    # Forzar contenido de texto a UTF-8 limpio
    df <- df %>%
      mutate(across(where(is.character), ~ safe_to_utf8(., from = "")))

    df <- standardize_names(df)

    if (!("anyo" %in% names(df))) {
      df$anyo <- year_num
    } else {
      df$anyo <- ifelse(is.na(df$anyo) | df$anyo == "", year_num, df$anyo)
    }

    if ("NCODI" %in% names(df)) df$NCODI <- as.character(df$NCODI)

    out_name <- paste0(tools::file_path_sans_extension(basename(f)), ".txt")
    out_path <- file.path(out_dir, out_name)

    write_delim(df, out_path, delim = ";", quote = "all")
    message("  [", yy, "] OK: ", basename(f),
            " (", nrow(df), " filas, ", ncol(df), " cols)")
  }
}

message("Script 1 completado. Archivos en: ", STD_RAW_DIR)
