# ------------------------------------------------------------
# 17_build_variable_dictionary_draft.R
#
# Objetivo:
# Crear un borrador de diccionario maestro de variables a partir
# de los posibles cambios de nombre detectados en la auditoría.
#
# Entrada:
#   outputs/variable_name_changes_suspected.csv
#
# Salida:
#   docs/variable_dictionary_draft.csv
#
# Qué hace:
# - Lee el archivo con posibles renombrados detectados.
# - Separa cada conjunto de nombres originales en filas
#   individuales.
# - Propone un nombre estandarizado para cada caso.
# - Genera un CSV revisable manualmente.
#
# Importante:
# Este script NO modifica todavía ningún fichero de datos ni la
# lógica del pipeline. Solo genera un borrador del diccionario.
# ------------------------------------------------------------

source("scripts/00_setup_packages.R")

library(dplyr)
library(tidyr)
library(readr)
library(stringr)
library(janitor)
library(fs)
library(tibble)
library(purrr)

# ------------------------------------------------------------
# Definir rutas del proyecto
# ------------------------------------------------------------
project_root <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
outputs_dir  <- file.path(project_root, "outputs")
docs_dir     <- file.path(project_root, "docs")

dir_create(docs_dir)

input_file  <- file.path(outputs_dir, "variable_name_changes_suspected.csv")
output_file <- file.path(docs_dir, "variable_dictionary_draft.csv")

# Comprobar que existe el archivo de entrada
if (!file_exists(input_file)) {
  stop("No se encontró el archivo de entrada: ", input_file, call. = FALSE)
}

# ------------------------------------------------------------
# Leer el archivo de posibles cambios de nombre
#
# Forzamos algunas columnas a texto para evitar que R convierta
# cadenas como "2010,2011,2012,..." en números gigantes o en
# notación científica.
# ------------------------------------------------------------
suspected <- read_delim(
  input_file,
  delim = ";",
  show_col_types = FALSE,
  col_types = cols(
    variable_clean = col_character(),
    n_original_names = col_double(),
    original_names = col_character(),
    years_present = col_character()
  )
)
if (nrow(suspected) == 0) {
  stop("El archivo de posibles cambios de nombre está vacío.", call. = FALSE)
}

# ------------------------------------------------------------
# Tabla de excepciones manuales
#
# Aquí puedes corregir nombres generados automáticamente por
# make_clean_names() que no sean deseables como nombre final.
#
# Ejemplo:
# "a_o" viene normalmente de "Año" o "año", pero para trabajar
# en el proyecto es mucho más útil llamarlo "anyo".
# ------------------------------------------------------------
manual_standard_names <- c(
  "a_o" = "anyo"
)

# ------------------------------------------------------------
# Función para proponer el nombre estandarizado
#
# - Si la variable_clean está en la tabla manual, usa ese nombre.
# - Si no, deja variable_clean como propuesta por defecto.
# ------------------------------------------------------------
propose_standard_name <- function(variable_clean) {
  recode(variable_clean, !!!manual_standard_names, .default = variable_clean)
}

# ------------------------------------------------------------
# Construcción del borrador del diccionario
#
# Pasos:
# 1. Proponer un nombre estandarizado.
# 2. Separar la columna original_names en una lista usando "|".
# 3. Expandir esa lista para tener una fila por cada nombre
#    original detectado.
# 4. Añadir columnas auxiliares para revisión manual.
# ------------------------------------------------------------
dictionary_draft <- suspected %>%
  mutate(
    standardized_name = propose_standard_name(variable_clean),

    # Convertir la cadena con varios nombres originales en una lista
    # Ejemplo:
    # "Año | año" -> c("Año", "año")
    original_names = str_split(original_names, "\\s*\\|\\s*")
  ) %>%
  # Expandir la lista anterior: una fila por cada nombre original
  tidyr::unnest(original_names) %>%
  rename(
    original_name = original_names
  ) %>%
  mutate(
    # Eliminar posibles espacios sobrantes
    original_name = str_trim(original_name),

    # Indicador de uso:
    # 1 = usar este mapeo en la estandarización
    # 0 = no usar / descartar / revisar más
    use = 1L,

    # Estado inicial de revisión
    review_status = "revisar",

    # Campo libre para anotar comentarios o decisiones
    notes = NA_character_
  ) %>%
  select(
    original_name,
    standardized_name,
    variable_clean,
    years_present,
    n_original_names,
    use,
    review_status,
    notes
  ) %>%
  arrange(standardized_name, original_name)

# ------------------------------------------------------------
# Guardar el archivo final
#
# Se usa separador ";" para que Excel en español lo abra mejor.
# ------------------------------------------------------------
write_delim(
  dictionary_draft,
  output_file,
  delim = ";",
  na = ""
)

message("Borrador de diccionario creado en: ", output_file)
message("Revisa especialmente las columnas 'original_name', 'standardized_name' y 'notes'.")