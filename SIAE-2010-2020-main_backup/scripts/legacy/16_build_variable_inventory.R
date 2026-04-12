# Build a variable inventory from raw SIAE yearly .txt files
# Expected folder structure:
# project_root/
#   data_raw/
#     2010/
#     2011/
#     ...
#     2023/ # or any available year folders
#   outputs/
#
# The script creates:
#   outputs/variable_inventory.csv
#   outputs/variable_presence_by_year.csv
#   outputs/variable_name_changes_suspected.csv
#   outputs/variable_inventory_summary.txt

source("scripts/00_setup_packages.R")

library(dplyr)
library(tidyr)
library(readr)
library(purrr)
library(stringr)
library(janitor)
library(fs)
library(tibble)

project_root <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
data_raw_dir <- file.path(project_root, "data_raw")
outputs_dir  <- file.path(project_root, "outputs")
dir_create(outputs_dir)

if (!dir_exists(data_raw_dir)) {
  stop("Folder not found: ", data_raw_dir, call. = FALSE)
}

# ------------------------------------------------------------
# DETECCIÓN AUTOMÁTICA DE LAS CARPETAS DE AÑOS EN data_raw
#
# Objetivo:
# Identificar automáticamente las carpetas que representan años
# (por ejemplo 2010, 2011, ..., 2023) dentro de data_raw, sin
# tener que escribir los años manualmente en el código.
#
# Esto permite que el script siga funcionando aunque en el
# futuro se añadan nuevos años al dataset.
# ------------------------------------------------------------

# Listar todas las carpetas que están directamente dentro de data_raw
year_dirs <- dir_ls(data_raw_dir, type = "directory", recurse = FALSE)

# Quedarse solo con las carpetas cuyo nombre tiene formato de año (4 dígitos)
# Ejemplos válidos: "2010", "2015", "2023"
# Ejemplos descartados: "temp", "backup", etc.
year_dirs <- year_dirs[str_detect(path_file(year_dirs), "^[0-9]{4}$")]

# Ordenar las carpetas por año de forma numérica
# Esto asegura que el orden sea 2010, 2011, ..., 2023
# y no un orden alfabético incorrecto.
year_dirs <- year_dirs[order(as.integer(path_file(year_dirs)))]

# Si no se encuentra ninguna carpeta con formato de año,
# se detiene el script porque no hay datos que procesar.
if (length(year_dirs) == 0) {
  stop("No se encontraron carpetas de años dentro de data_raw/", call. = FALSE)
}

# Extraer los años como números para poder trabajar con ellos
years_detected <- as.integer(path_file(year_dirs))

# Mostrar en consola los años detectados.
# Sirve para comprobar rápidamente que el script está
# leyendo todos los años esperados (por ejemplo hasta 2023).
message("Años detectados en data_raw: ", paste(years_detected, collapse = ", "))

# ------------------------------------------------------------
# COMPROBACIÓN DE POSIBLES AÑOS FALTANTES
#
# Si los datos deberían cubrir un rango continuo (por ejemplo
# 2010–2023), esta comprobación detecta si falta alguna carpeta
# intermedia, lo cual podría indicar que falta algún dataset.
# ------------------------------------------------------------

expected_years <- seq(min(years_detected), max(years_detected))
missing_years <- setdiff(expected_years, years_detected)

# Si faltan años en la secuencia, se muestra un aviso.
# El script continúa, pero informa del posible problema.
if (length(missing_years) > 0) {
  warning(
    "Faltan las siguientes carpetas de años en data_raw/: ",
    paste(missing_years, collapse = ", ")
  )
}

read_header_only <- function(file) {
  out <- tryCatch(
    suppressMessages(
      read_delim(
        file = file,
        delim = ";",
        locale = locale(decimal_mark = ","),
        n_max = 0,
        show_col_types = FALSE,
        progress = FALSE,
        guess_max = 10
      )
    ),
    error = function(e) NULL
  )
  out
}

extract_vars_from_file <- function(file, year) {
  header_df <- read_header_only(file)

  if (is.null(header_df)) {
    return(tibble(
      year = year,
      file_name = path_file(file),
      file_path = file,
      variable_original = NA_character_,
      variable_clean = NA_character_,
      read_ok = FALSE
    ))
  }

  vars <- names(header_df)

  tibble(
    year = year,
    file_name = path_file(file),
    file_path = file,
    variable_original = vars,
    variable_clean = vars %>% make_clean_names(),
    read_ok = TRUE
  )
}

inventory_long <- map_dfr(year_dirs, function(folder) {
  year <- as.integer(path_file(folder))
  files <- dir_ls(folder, regexp = "\\.txt$", recurse = FALSE)

  if (length(files) == 0) {
    return(tibble(
      year = year,
      file_name = NA_character_,
      file_path = NA_character_,
      variable_original = NA_character_,
      variable_clean = NA_character_,
      read_ok = FALSE
    ))
  }

  map_dfr(files, extract_vars_from_file, year = year)
})

inventory_long <- inventory_long %>%
  mutate(
    variable_original = str_trim(variable_original),
    variable_clean = str_trim(variable_clean)
  )

write_csv(inventory_long, file.path(outputs_dir, "variable_inventory_long.csv"), na = "")

variable_inventory <- inventory_long %>%
  filter(!is.na(variable_original), read_ok) %>%
  group_by(variable_clean) %>%
  summarise(
    n_years = n_distinct(year),
    years_present = paste(sort(unique(year)), collapse = "|"),
    files_present = paste(sort(unique(file_name)), collapse = " | "),
    original_names = paste(sort(unique(variable_original)), collapse = " | "),
    .groups = "drop"
  ) %>%
  arrange(desc(n_years), variable_clean)

write_csv(variable_inventory, file.path(outputs_dir, "variable_inventory.csv"), na = "")

presence_wide <- inventory_long %>%
  filter(!is.na(variable_original), read_ok) %>%
  distinct(variable_clean, year) %>%
  mutate(present = 1L) %>%
  pivot_wider(names_from = year, values_from = present, values_fill = 0L) %>%
  arrange(variable_clean)

write_csv(presence_wide, file.path(outputs_dir, "variable_presence_by_year.csv"), na = "")

suspected_name_changes <- inventory_long %>%
  filter(!is.na(variable_original), read_ok) %>%
  group_by(variable_clean) %>%
  summarise(
    n_original_names = n_distinct(variable_original),
    original_names = paste(sort(unique(variable_original)), collapse = " | "),
    years_present = paste(sort(unique(year)), collapse = ","),
    .groups = "drop"
  ) %>%
  filter(n_original_names > 1) %>%
  arrange(desc(n_original_names), variable_clean)

write_delim(
  suspected_name_changes,
  file.path(outputs_dir, "variable_name_changes_suspected.csv"),
  delim = ";",
  na = ""
)

summary_lines <- c(
  paste0("Project root: ", project_root),
  paste0("Year folders detected: ", length(year_dirs)),
  paste0("Variables (clean names): ", nrow(variable_inventory)),
  paste0("Variables present in all years: ", sum(variable_inventory$n_years == length(year_dirs))),
  paste0("Variables missing in at least one year: ", sum(variable_inventory$n_years < length(year_dirs))),
  paste0("Potential renaming cases: ", nrow(suspected_name_changes))
)

writeLines(summary_lines, file.path(outputs_dir, "variable_inventory_summary.txt"))

message("Inventory completed. Files written to: ", outputs_dir)
