# ================================
# 15_auditoria_variables_raw.R
# Inventario sistemático de ficheros (txt/xml) y variables por año (2010-2023)
# Salidas en data_intermediate/:
#   - files_by_year.csv
#   - file_presence_matrix.csv
#   - vars_by_year_file.csv
#   - vars_summary.csv
#   - vars_errors.csv
#   - common_files_all_years.txt
#   - common_vars_all_years.txt
# ================================

# Cargar configuración (RAW_DIR, INT_DIR, etc.)
config_path <- if (file.exists("scripts/00_config.R")) "scripts/00_config.R" else "00_config.R"
source(config_path)

suppressPackageStartupMessages({
  library(dplyr)
  library(stringr)
  library(purrr)
  library(readr)
  library(tidyr)
  library(xml2)
})

# Años disponibles como subcarpetas (p.ej. 2010..2023)
years <- list.dirs(RAW_DIR, full.names = FALSE, recursive = FALSE) |>
  str_extract("\\d{4}") |>
  na.omit() |>
  unique() |>
  sort()

stopifnot(length(years) > 0)

dir.create(INT_DIR, showWarnings = FALSE, recursive = TRUE)

# Normaliza nombre de archivo para comparar entre años
normalize_fname <- function(x) {
  x |>
    basename() |>
    str_to_lower() |>
    str_replace_all("\\s+", "_")
}

# Variables desde TXT: intenta leer la 1ª línea como cabecera (robusto a codificaciones)
vars_from_txt <- function(path) {
  header <- tryCatch(
    readLines(path, n = 1, warn = FALSE, encoding = "UTF-8"),
    error = function(e) NA_character_
  )
  if (length(header) == 0 || is.na(header) || header == "") return(character(0))
  
  # Forzar a UTF-8 ignorando bytes inválidos
  header <- iconv(header, from = "", to = "UTF-8", sub = "")
  
  # Quitar BOM si existe
  header <- sub("^\ufeff", "", header)
  
  # Quitar comillas
  header <- gsub('"', "", header, fixed = TRUE)
  
  # Separador probable
  sep <- if (grepl(";", header, fixed = TRUE)) ";" else if (grepl("\t", header, fixed = TRUE)) "\t" else ","
  
  vars <- strsplit(header, sep, fixed = TRUE)[[1]] |> trimws()
  vars <- vars[vars != ""]
  unique(vars)
}

# Variables desde XML (heurístico): nombres de nodos y atributos
vars_from_xml <- function(path) {
  doc <- tryCatch(read_xml(path), error = function(e) NULL)
  if (is.null(doc)) return(character(0))
  
  nodes <- xml_find_all(doc, ".//*")
  node_names <- xml_name(nodes)
  node_names <- node_names[!is.na(node_names)]
  
  attrs <- map(nodes, xml_attrs)
  attr_names <- unique(unlist(map(attrs, names), use.names = FALSE))
  attr_names <- attr_names[!is.na(attr_names)]
  
  unique(c(node_names, attr_names))
}

# Lista de ficheros por año
list_files_year <- function(year) {
  folder <- file.path(RAW_DIR, year)
  stopifnot(dir.exists(folder))
  
  files <- list.files(folder, full.names = TRUE, recursive = FALSE)
  files <- files[grepl("\\.(txt|xml)$", files, ignore.case = TRUE)]
  
  tibble(
    year = as.integer(year),
    file_path = files,
    file_name = basename(files),
    file_norm = normalize_fname(files),
    ext = tolower(tools::file_ext(files))
  )
}

files_by_year <- purrr::map_dfr(years, list_files_year)

# Guardar inventario de ficheros
readr::write_csv(files_by_year, file.path(INT_DIR, "files_by_year.csv"))

# Matriz de presencia de ficheros por año (útil para ver qué falta)
file_presence_matrix <- files_by_year |>
  distinct(year, file_norm) |>
  mutate(present = 1L) |>
  pivot_wider(names_from = file_norm, values_from = present, values_fill = 0L)

readr::write_csv(file_presence_matrix, file.path(INT_DIR, "file_presence_matrix.csv"))

# Ficheros comunes a todos los años
common_files <- files_by_year |>
  distinct(year, file_norm) |>
  count(file_norm, name = "n_years") |>
  filter(n_years == length(unique(files_by_year$year))) |>
  arrange(file_norm) |>
  pull(file_norm)

readr::write_lines(common_files, file.path(INT_DIR, "common_files_all_years.txt"))

# Extractor seguro (no rompe si un archivo está mal)
safe_extract_vars <- purrr::safely(function(path, ext) {
  if (ext == "txt") vars_from_txt(path) else vars_from_xml(path)
})

# Extraer variables por archivo/año (con registro de errores)
tmp <- files_by_year |>
  mutate(
    res = map2(file_path, ext, safe_extract_vars),
    vars = map(res, "result"),
    err = map(res, "error"),
    err_msg = map_chr(err, ~ if (is.null(.x)) "" else conditionMessage(.x))
  )

# Guardar errores (si los hay)
vars_errors <- tmp |>
  filter(err_msg != "") |>
  select(year, file_path, file_name, ext, err_msg)

readr::write_csv(vars_errors, file.path(INT_DIR, "vars_errors.csv"))

# Mantener solo los archivos sin error y desanidar variables
vars_by_year_file <- tmp |>
  filter(err_msg == "") |>
  select(year, file_norm, file_name, ext, vars) |>
  unnest(vars) |>
  mutate(vars = str_trim(vars)) |>
  filter(!is.na(vars), vars != "") |>
  distinct()

readr::write_csv(vars_by_year_file, file.path(INT_DIR, "vars_by_year_file.csv"))

# Resumen: nº de variables por archivo y año
vars_summary <- vars_by_year_file |>
  group_by(year, file_norm, file_name, ext) |>
  summarise(n_vars = n_distinct(vars), .groups = "drop") |>
  arrange(year, file_norm)

readr::write_csv(vars_summary, file.path(INT_DIR, "vars_summary.csv"))

# Variables comunes a todos los años (global, sin distinguir archivo)
common_vars <- vars_by_year_file |>
  distinct(year, vars) |>
  count(vars, name = "n_years") |>
  filter(n_years == length(unique(vars_by_year_file$year))) |>
  arrange(vars) |>
  pull(vars)

readr::write_lines(common_vars, file.path(INT_DIR, "common_vars_all_years.txt"))

message("Auditoría completada. Resultados en: ", INT_DIR)
message("Revisa también: ", file.path(INT_DIR, "vars_errors.csv"))