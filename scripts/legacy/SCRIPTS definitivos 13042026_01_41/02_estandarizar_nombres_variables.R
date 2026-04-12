# ============================================================
# 2_estandarizar_nombres_variables.R
# Lee archivos TXT/XML de RAW_DIR, estandariza nombres de
# variables y escribe los resultados en STD_RAW_DIR/.
#
# Protección especial: variables u1..u104 y u900 (Unidades
# Asistenciales R.D. 1277/2003) nunca son modificadas.
# ============================================================

config_path <- file.path(
  dirname(normalizePath(sys.frame(1)$ofile)),
  "00_config.R"
)
source(config_path)

required_pkgs <- c("tidyverse", "fs", "readr")
missing_pkgs <- required_pkgs[!vapply(required_pkgs, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing_pkgs) > 0) {
  stop(
    "Faltan paquetes: ", paste(missing_pkgs, collapse = ", "),
    "\nInstálalos con install.packages().",
    call. = FALSE
  )
}

library(tidyverse)
library(fs)
library(readr)

dir_create(STD_RAW_DIR, recurse = TRUE)

# ============================================================
# MAPEO cod_centro → NCODI (del CNH)
# Generado por 0_exportar_mapeo_cnh.R
# ============================================================

load_ncodi_map <- function(path) {
  if (!file.exists(path)) {
    message(
      "Mapeo NCODI-hospital no encontrado en: ", path,
      "\nEjecuta 0_exportar_mapeo_cnh.R para generarlo.",
      "\nLos archivos con cod_centro pero sin NCODI serán omitidos por el script 2."
    )
    return(NULL)
  }
  m <- tryCatch(
    readr::read_csv(path, col_types = readr::cols(.default = "c"), show_col_types = FALSE),
    error = function(e) {
      warning("No se pudo leer el mapeo NCODI: ", conditionMessage(e), call. = FALSE)
      NULL
    }
  )
  if (is.null(m) || !all(c("NCODI", "cod_centro") %in% names(m))) {
    warning("El mapeo NCODI no tiene las columnas esperadas (NCODI, cod_centro).", call. = FALSE)
    return(NULL)
  }
  m <- m[!is.na(m$cod_centro) & nchar(trimws(m$cod_centro)) > 0, ]
  # Lookup: cod_centro → NCODI
  stats::setNames(trimws(m$NCODI), trimws(m$cod_centro))
}

NCODI_LOOKUP <- load_ncodi_map(NCODI_HOSPITAL_MAP_PATH)

# Función: si el df tiene cod_centro pero no NCODI, añade NCODI usando el lookup
translate_cod_centro_to_ncodi <- function(df) {
  if ("NCODI" %in% names(df)) return(df)            # ya tiene NCODI: nada que hacer
  if (!("cod_centro" %in% names(df))) return(df)    # sin cod_centro: nada que hacer
  if (is.null(NCODI_LOOKUP)) return(df)              # sin mapeo disponible

  df$NCODI <- NCODI_LOOKUP[trimws(as.character(df$cod_centro))]
  # Casos no resueltos: intentar derivar CODCNH desde cod_centro y buscar
  unresolved <- is.na(df$NCODI) & !is.na(df$cod_centro) & nchar(trimws(df$cod_centro)) >= 8
  if (any(unresolved)) {
    codcnh_derived <- substr(trimws(df$cod_centro[unresolved]), 3, 8)
    # Buscar en el mapeo por CODCNH derivado
    if (file.exists(NCODI_HOSPITAL_MAP_PATH)) {
      map_full <- tryCatch(
        readr::read_csv(NCODI_HOSPITAL_MAP_PATH, col_types = readr::cols(.default = "c"),
                        show_col_types = FALSE),
        error = function(e) NULL
      )
      if (!is.null(map_full) && "CODCNH" %in% names(map_full)) {
        codcnh_lookup <- stats::setNames(trimws(map_full$NCODI), trimws(map_full$CODCNH))
        df$NCODI[unresolved] <- codcnh_lookup[codcnh_derived]
      }
    }
  }

  n_resolved   <- sum(!is.na(df$NCODI))
  n_unresolved <- sum(is.na(df$NCODI))
  message(sprintf(
    "    cod_centro → NCODI: %d resueltos, %d sin resolver",
    n_resolved, n_unresolved
  ))
  df
}

# ============================================================
# VARIABLES PROTEGIDAS — nunca renombrar ni eliminar
# ============================================================

PROTECTED_VARS <- c(
  paste0("u", 1:104),
  "u900",
  "NCODI",
  "anyo"
)

is_protected <- function(name) {
  name %in% PROTECTED_VARS
}

# ============================================================
# FUNCIONES AUXILIARES DE ENCODING
# ============================================================

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

# ============================================================
# CARGA DE MAPEO EXPLÍCITO var_mapping_explicit.csv
# ============================================================

load_explicit_mapping <- function(path) {
  if (!file.exists(path)) {
    message("var_mapping_explicit.csv no encontrado en: ", path, " — se omite.")
    return(data.frame(original = character(0), canonical = character(0),
                      stringsAsFactors = FALSE))
  }
  m <- tryCatch(
    read.csv(path, stringsAsFactors = FALSE, encoding = "UTF-8", na.strings = ""),
    error = function(e) {
      warning("No se pudo leer var_mapping_explicit.csv: ", conditionMessage(e), call. = FALSE)
      data.frame(original = character(0), canonical = character(0), stringsAsFactors = FALSE)
    }
  )
  m <- m[!is.na(m$original) & nchar(trimws(m$original)) > 0, ]
  m$original   <- trimws(m$original)
  m$canonical  <- trimws(m$canonical)
  m
}

# PROTECCIÓN: usar mapeo congelado si existe (inmutable entre re-ejecuciones de script 2)
mapping_path_s1 <- if (exists("VAR_MAPPING_FROZEN_PATH") &&
                        file.exists(VAR_MAPPING_FROZEN_PATH)) {
  message("  Usando mapeo congelado: var_mapping_explicit_FROZEN.csv")
  VAR_MAPPING_FROZEN_PATH
} else {
  message("  Usando mapeo dinámico: var_mapping_explicit.csv (sin copia congelada)")
  VAR_MAPPING_EXPLICIT_PATH
}
EXPLICIT_MAP <- load_explicit_mapping(mapping_path_s1)

# Vector con nombre; facilita lookup O(1)
explicit_lookup <- stats::setNames(EXPLICIT_MAP$canonical, EXPLICIT_MAP$original)

# ============================================================
# standardize_names()
# Aplica renombrados en este orden de prioridad:
#   1. Variables protegidas → intocables
#   2. Mapeo explícito (var_mapping_explicit.csv)
#   3. Artefactos XML _xNNNN_ → token legible
#   4. Reglas hardcoded por casos conocidos
# Devuelve lista: df (con nombres corregidos) + n_renamed (int)
# ============================================================

resolve_xml_artifacts <- function(name) {
  # Tabla de sustituciones hexadecimales conocidas
  subs <- c(
    "_x0020_" = "_",    # espacio
    "_x0028_" = "_",    # (
    "_x0029_" = "_",    # )
    "_x00e9_" = "e",    # é
    "_x00f3_" = "o",    # ó
    "_x00fa_" = "u",    # ú
    "_x00ed_" = "i",    # í
    "_x00e1_" = "a",    # á
    "_x00f1_" = "n",    # ñ
    "_x00c1_" = "A",    # Á
    "_x00c9_" = "E",    # É
    "_x00cd_" = "I",    # Í
    "_x00d3_" = "O",    # Ó
    "_x00da_" = "U",    # Ú
    "_x0036_" = "6",    # dígito 6
    "_x0037_" = "7",    # dígito 7
    "_x0038_" = "8",    # dígito 8
    "_x0039_" = "9",    # dígito 9
    "_x0030_" = "0",    # dígito 0
    "_x0031_" = "1",
    "_x0032_" = "2",
    "_x0033_" = "3",
    "_x0034_" = "4",
    "_x0035_" = "5"
  )
  for (pat in names(subs)) {
    name <- gsub(pat, subs[[pat]], name, fixed = TRUE)
  }
  # Limpiar dobles guiones bajos residuales y bordes
  name <- gsub("_+", "_", name)
  name <- gsub("^_|_$", "", name)
  name
}

standardize_names <- function(df) {
  original_names <- names(df)
  new_names      <- original_names
  n_renamed      <- 0L

  for (i in seq_along(new_names)) {
    nm <- new_names[i]

    # 1. Variables protegidas: no tocar
    if (is_protected(nm)) next

    # 2. Mapeo explícito
    if (!is.na(explicit_lookup[nm])) {
      candidate <- explicit_lookup[[nm]]
      if (!is_protected(candidate) && candidate != nm) {
        new_names[i] <- candidate
        n_renamed <- n_renamed + 1L
        next
      }
    }

    # 3. Resolver artefactos XML _xNNNN_
    nm_resolved <- resolve_xml_artifacts(nm)
    if (nm_resolved != nm) {
      # Tras resolver artefactos, intentar mapeo explícito de nuevo
      if (!is.na(explicit_lookup[nm_resolved])) {
        candidate <- explicit_lookup[[nm_resolved]]
        if (!is_protected(candidate)) {
          new_names[i] <- candidate
          n_renamed <- n_renamed + 1L
          next
        }
      }
      new_names[i] <- nm_resolved
      n_renamed <- n_renamed + 1L
      nm <- nm_resolved
    }

    # 4. Reglas hardcoded — normalización de año
    nm_low <- tolower(iconv(nm, from = "UTF-8", to = "ASCII//TRANSLIT", sub = "byte"))
    nm_low[is.na(nm_low)] <- ""
    if (nm_low %in% c("ano", "anyo", "año", "a<f1>o")) {
      new_names[i] <- "anyo"
      if (nm != "anyo") n_renamed <- n_renamed + 1L
      next
    }
    if (nm == "year") { new_names[i] <- "anyo"; n_renamed <- n_renamed + 1L; next }

    # 4b. Camas instaladas
    nm_clean <- trimws(nm)
    if (nm_clean %in% c(
      "camas_instaladas", "camas instaladas", "Camas_instaladas", "Camas instaladas",
      "Camas_Instaladas", "CAMAS_INSTALADAS"
    )) {
      new_names[i] <- "camas_instalada"; n_renamed <- n_renamed + 1L; next
    }

    # 4c. CCAA
    if (nm_clean %in% c("Cod CCAA (Todas)", "ccaa_Codigo", "Cod_CCAA__Todas_")) {
      new_names[i] <- "ccaa_codigo"; n_renamed <- n_renamed + 1L; next
    }
    if (nm_clean %in% c("Desc CCAA (Todas)", "Desc_CCAA__Todas_")) {
      new_names[i] <- "ccaa"; n_renamed <- n_renamed + 1L; next
    }

    # 4d. Finalidad (con y sin espacio, mayúscula inicial)
    if (nm_clean %in% c(
      "Cod Grupo Finalidad", "cod_finalidad agrupada",
      "Cod_finalidad_agrupada", "Cod_Grupo_Finalidad"
    )) {
      new_names[i] <- "cod_finalidad_agrupada"; n_renamed <- n_renamed + 1L; next
    }
    if (nm_clean %in% c(
      "FINALIDAD agrupada para Anonimizacion",
      "FINALIDAD_agrupada_para_Anonimizacion",
      "Finalidad_agrupada_para_Anonimizacion"
    )) {
      new_names[i] <- "Finalidad_agrupada"; n_renamed <- n_renamed + 1L; next
    }

    # 4e. Dependencia / SNS
    if (nm_clean %in% c("Cod Pertenencia SNS", "Cod_Pertenencia_SNS")) {
      new_names[i] <- "cod_depend_agrupada"; n_renamed <- n_renamed + 1L; next
    }
    if (nm_clean %in% c("Desc Pertenencia SNS", "Desc_Pertenencia_SNS")) {
      new_names[i] <- "Depend_agrupada"; n_renamed <- n_renamed + 1L; next
    }

    # 4f. Gastos (nombres legacy sin artefacto XML)
    gastos_map <- c(
      "60_totalCompra"        = "G_totalCompra",
      "61_variaExistencias"   = "G_variaExistencias",
      "62_servExteriores"     = "G_servExteriores",
      "64_gastoPersonal"      = "G_gastoPersonal",
      "68_dotaAmortizacion"   = "G_dotaAmortizacion",
      "69_perdidaDeterioro"   = "G_perdidaDeterioro",
      "6X_restoGasto"         = "G_restoGasto",
      "tot_compraGasto"       = "G_totGastos"
    )
    if (nm_clean %in% names(gastos_map)) {
      new_names[i] <- gastos_map[[nm_clean]]; n_renamed <- n_renamed + 1L; next
    }

    # 4g. Ingresos (nombres legacy sin artefacto XML)
    ingresos_map <- c(
      "70_tIngresos"                    = "I_totIngresosPS",
      "700_particular"                  = "I_particular",
      "701_AsegPriv"                    = "I_AsegPriv",
      "701_1_AsistSanitaria"            = "I_AsistSanitaria",
      "701_2_AccTrafic"                 = "I_AccTrafic",
      "702_MATEPSS"                     = "I_MATEPSS",
      "704_SNS"                         = "I_SNS",
      "705_1_FdirectaSS"                = "I_FdirectaSS",
      "705_2_FdirectaAPriv_MATEPSS"     = "I_FdirectaAPriv_MATEPSS",
      "706_OyEntiPublica"               = "I_OyEntiPublica",
      "708_bonificaciones"              = "I_bonificaciones",
      "709_Otros_Ips"                   = "I_Otros_Ips",
      "74_Total_Subvencion"             = "I_Total_Subvencion",
      "7X_restoIngresos"                = "I_restoIngresos",
      "total_ventasIngresos"            = "I_totIngresos"
    )
    if (nm_clean %in% names(ingresos_map)) {
      new_names[i] <- ingresos_map[[nm_clean]]; n_renamed <- n_renamed + 1L; next
    }
  }

  # Verificación de seguridad: ninguna variable protegida ha cambiado de nombre
  for (i in seq_along(original_names)) {
    if (is_protected(original_names[i]) && new_names[i] != original_names[i]) {
      warning(
        "standardize_names() intentó renombrar variable protegida '",
        original_names[i], "' → '", new_names[i], "'. Se revierte.",
        call. = FALSE
      )
      new_names[i] <- original_names[i]
    }
  }

  names(df) <- new_names
  list(df = df, n_renamed = n_renamed)
}

# ============================================================
# DETECCIÓN DE CABECERA EN TXT
# ============================================================

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

is_header_line_txt <- function(line) {
  if (length(line) == 0 || nchar(line[[1]], type = "bytes") == 0) return(FALSE)
  line_safe <- safe_to_utf8(line[[1]], from = "")
  if (is.na(line_safe) || nchar(line_safe, type = "bytes") == 0) return(FALSE)
  fields <- strsplit(line_safe, ";", fixed = TRUE, useBytes = TRUE)[[1]]
  if (length(fields) <= 1) return(FALSE)
  fields_norm <- normalize_header_tokens(fields)
  marker_hit     <- any(fields_norm %in% c("ncodi", "anyo", "ano"))
  var_like_count <- sum(grepl("^[a-z_][a-z0-9_]*$", fields_norm))
  marker_hit || var_like_count >= 2
}

# ============================================================
# MÓDULO 07 FORMACIÓN 2015 — SIN CABECERA
# Toma la cabecera del año anterior (2014).
# ============================================================

extract_module_id <- function(path) {
  nm <- basename(path)
  m  <- stringr::str_match(nm, "(^|[_\\-\\s])(\\d{2})([_\\-\\s]|\\.)")
  ifelse(is.na(m[, 3]), NA_integer_, as.integer(m[, 3]))
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
      header <- clean_header_tokens(
        strsplit(line1[[1]], ";", fixed = TRUE, useBytes = TRUE)[[1]]
      )
      return(header)
    }
  }
  stop("No se pudo inferir la cabecera para Formacion (modulo 07).", call. = FALSE)
}

load_formacion_header_2015 <- function(path, year) {
  prev_year <- as.integer(year) - 1L
  prev_dir  <- file.path(RAW_DIR, as.character(prev_year))
  if (!dir_exists(prev_dir)) {
    stop("No existe carpeta del año previo para Formacion 2015: ", prev_dir, call. = FALSE)
  }
  prev_files <- dir_ls(prev_dir, regexp = "(?i)\\.(txt)$", recurse = FALSE)
  module_ids <- extract_module_id(prev_files)
  prev_files <- sort(prev_files[!is.na(module_ids) & module_ids == 7L])
  if (length(prev_files) == 0) {
    stop("No se encontró archivo modulo 07 en año previo para Formacion 2015.", call. = FALSE)
  }
  source_file <- prev_files[[1]]
  if (length(prev_files) > 1) {
    hits <- prev_files[stringr::str_detect(tolower(basename(prev_files)), "formaci")]
    if (length(hits) == 1) {
      source_file <- hits[[1]]
    } else {
      stop("Varios candidatos modulo 07 en año previo, no son unívocos.", call. = FALSE)
    }
  }
  prev_line <- read_lines_with_fallback(source_file, n = 1)
  if (length(prev_line) == 0 || !nzchar(prev_line[[1]])) {
    stop("No se pudo leer cabecera de Formacion en archivo previo: ", source_file, call. = FALSE)
  }
  header <- clean_header_tokens(strsplit(prev_line[[1]], ";", fixed = TRUE)[[1]])

  current_line <- read_lines_with_fallback(path, n = 1)
  if (length(current_line) == 0 || !nzchar(current_line[[1]])) {
    stop("No se pudo leer primera fila de Formacion 2015: ", path, call. = FALSE)
  }
  current_fields <- clean_header_tokens(strsplit(current_line[[1]], ";", fixed = TRUE)[[1]])

  if (length(header) != 112L) {
    stop("Cabecera de Formacion del año previo no tiene 112 campos: ", length(header), call. = FALSE)
  }
  if (length(current_fields) != 112L) {
    stop("Primera fila de Formacion 2015 no tiene 112 campos: ", length(current_fields), call. = FALSE)
  }
  list(header = header, source_file = source_file)
}

# ============================================================
# LECTURA DE ARCHIVOS
# ============================================================

read_txt_file <- function(path, year) {
  module_id <- extract_module_id(path)
  line1     <- read_lines_with_fallback(path, n = 1)
  has_header <- is_header_line_txt(line1)

  if (!has_header && year == 2015 && !is.na(module_id) && module_id == 7L) {
    info <- load_formacion_header_2015(path, year)
    df <- readr::read_delim(
      path,
      delim       = ";",
      col_names   = info$header,
      locale      = locale(decimal_mark = ","),
      show_col_types = FALSE,
      progress    = FALSE
    )
    return(df)
  }

  if (!has_header) {
    stop("Archivo TXT sin cabecera no reconocido: ", path, call. = FALSE)
  }

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
  if (length(lines) == 0) stop("No se pudo leer XML: ", path, call. = FALSE)

  record_tag <- NA_character_
  for (ln in lines) {
    ln <- trimws(ln)
    if (grepl("^<\\?xml", ln) || grepl("^<dataroot", ln)) next
    if (grepl("^<[^/!?][^>]*>$", ln)) {
      tg <- sub("^<([^>]+)>$", "\\1", ln)
      if (!grepl("^dataroot$", tg, ignore.case = TRUE)) { record_tag <- tg; break }
    }
  }
  if (is.na(record_tag)) stop("No se encontró nodo de registro en XML: ", path, call. = FALSE)

  rows <- list(); current <- list(); in_record <- FALSE
  for (ln in lines) {
    ln <- trimws(ln)
    if (!in_record && grepl(paste0("^<", record_tag, ">$"), ln)) {
      in_record <- TRUE; current <- list(); next
    }
    if (in_record && grepl(paste0("^</", record_tag, ">$"), ln)) {
      rows[[length(rows) + 1]] <- current; in_record <- FALSE; next
    }
    if (in_record && grepl("^<[^/!?][^>]*>.*</[^>]+>$", ln)) {
      tag <- sub("^<([^>]+)>.*$", "\\1", ln)
      val <- sub("^<[^>]+>(.*)</[^>]+>$", "\\1", ln)
      val <- gsub("&amp;",  "&",  val, fixed = TRUE)
      val <- gsub("&lt;",   "<",  val, fixed = TRUE)
      val <- gsub("&gt;",   ">",  val, fixed = TRUE)
      val <- gsub("&quot;", '"',  val, fixed = TRUE)
      val <- gsub("&apos;", "'",  val, fixed = TRUE)
      current[[tag]] <- val
    }
  }
  if (length(rows) == 0) stop("No se extrajeron registros del XML: ", path, call. = FALSE)
  dplyr::bind_rows(lapply(rows, tibble::as_tibble))
}

# ============================================================
# PROCESAMIENTO POR AÑO Y MÓDULO
# ============================================================

year_dirs <- dir_ls(RAW_DIR, type = "directory", recurse = FALSE)
years <- year_dirs %>%
  path_file() %>%
  stringr::str_extract("^\\d{4}$") %>%
  stats::na.omit() %>%
  sort()

if (length(years) == 0) stop("No se encontraron carpetas de año en RAW_DIR: ", RAW_DIR)

# Log global
log_rows <- list()

for (yy in years) {
  year_num <- as.integer(yy)
  in_dir   <- file.path(RAW_DIR, yy)
  out_dir  <- file.path(STD_RAW_DIR, yy)
  dir_create(out_dir, recurse = TRUE)

  files <- dir_ls(in_dir, recurse = FALSE)
  files <- files[stringr::str_detect(files, "(?i)\\.(txt|xml)$")]

  if (length(files) == 0) {
    warning("Sin archivos TXT/XML para el año ", yy, " en ", in_dir, call. = FALSE)
    next
  }

  message(sprintf("\n=== Año %s (%d archivos) ===", yy, length(files)))

  for (f in files) {
    ext       <- tolower(tools::file_ext(f))
    fname     <- basename(f)
    n_cols_in <- NA_integer_
    n_renamed <- NA_integer_
    error_msg <- ""

    df <- tryCatch({
      raw_df <- if (ext == "txt") read_txt_file(f, year_num) else read_xml_file(f)

      # Normalizar encoding de caracteres
      raw_df <- raw_df %>%
        mutate(across(where(is.character), ~ iconv(., from = "", to = "UTF-8")))

      raw_df
    }, error = function(e) {
      error_msg <<- conditionMessage(e)
      warning(sprintf("[%s/%s] ERROR lectura: %s", yy, fname, error_msg), call. = FALSE)
      NULL
    })

    if (is.null(df)) {
      log_rows[[length(log_rows) + 1]] <- data.frame(
        anyo = yy, archivo = fname,
        cols_in = NA_integer_, cols_out = NA_integer_,
        n_renamed = NA_integer_, error = error_msg,
        stringsAsFactors = FALSE
      )
      next
    }

    n_cols_in <- ncol(df)

    # Aplicar estandarización de nombres
    std_result <- tryCatch(
      standardize_names(df),
      error = function(e) {
        error_msg <<- conditionMessage(e)
        warning(sprintf("[%s/%s] ERROR standardize_names: %s", yy, fname, error_msg), call. = FALSE)
        NULL
      }
    )

    if (is.null(std_result)) {
      log_rows[[length(log_rows) + 1]] <- data.frame(
        anyo = yy, archivo = fname,
        cols_in = n_cols_in, cols_out = NA_integer_,
        n_renamed = NA_integer_, error = error_msg,
        stringsAsFactors = FALSE
      )
      next
    }

    df        <- std_result$df
    n_renamed <- std_result$n_renamed

    # Traducir cod_centro → NCODI cuando el archivo no tiene NCODI
    df <- translate_cod_centro_to_ncodi(df)
# Si tras la traducción sigue sin NCODI, omitir el archivo
    if (!"NCODI" %in% names(df)) {
      message(sprintf("  [OMITIDO] %s (año %s) — sin NCODI tras traducción",
                      fname, yy))
      next
    }


    # Garantizar columna anyo
    if (!("anyo" %in% names(df))) {
      df$anyo <- year_num
    } else {
      df$anyo <- ifelse(is.na(df$anyo) | as.character(df$anyo) == "", year_num, df$anyo)
    }

    # NCODI siempre character
    if ("NCODI" %in% names(df)) df$NCODI <- as.character(df$NCODI)

    n_cols_out <- ncol(df)

    # Escribir archivo estandarizado
    out_name <- paste0(
      tools::file_path_sans_ext(safe_to_utf8(fname, from = "")),
      ".txt"
    )
    out_path <- file.path(out_dir, out_name)

    tryCatch(
      write_delim(df, out_path, delim = ";", quote = "all"),
      error = function(e) {
        error_msg <<- conditionMessage(e)
        warning(sprintf("[%s/%s] ERROR escritura: %s", yy, fname, error_msg), call. = FALSE)
      }
    )

    message(sprintf(
      "  [%s] %s | cols: %d → %d | renombradas: %d",
      yy, fname, n_cols_in, n_cols_out, n_renamed
    ))

    log_rows[[length(log_rows) + 1]] <- data.frame(
      anyo = yy, archivo = fname,
      cols_in = n_cols_in, cols_out = n_cols_out,
      n_renamed = n_renamed, error = error_msg,
      stringsAsFactors = FALSE
    )
  }
}

# ============================================================
# GUARDAR LOG
# ============================================================

log_df <- dplyr::bind_rows(log_rows)
log_path <- file.path(INT_DIR, "log_script1_estandarizacion.csv")
write.csv(log_df, log_path, row.names = FALSE, na = "")

message("\n=== Script 1 completado ===")
message("Log guardado en: ", log_path)
message("Total archivos procesados: ", nrow(log_df))
message("Total con errores: ", sum(nchar(log_df$error) > 0))
message("Total renombrados acumulados: ", sum(log_df$n_renamed, na.rm = TRUE))
