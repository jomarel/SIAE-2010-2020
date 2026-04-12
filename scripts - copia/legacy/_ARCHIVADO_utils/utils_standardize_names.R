# ------------------------------------------------------------
# utils_standardize_names.R
#
# Funciones auxiliares para estandarizar nombres de variables
# usando un diccionario maestro.
# ------------------------------------------------------------

source("scripts/00_setup_packages.R")

library(dplyr)
library(readr)
library(stringr)
library(tibble)

# ------------------------------------------------------------
# Leer el diccionario maestro de variables
#
# Entrada:
#   dictionary_file: ruta al CSV del diccionario
#
# Salida:
#   dataframe con las columnas del diccionario
# ------------------------------------------------------------
load_variable_dictionary <- function(dictionary_file = "docs/variable_dictionary.csv") {

  if (!file.exists(dictionary_file)) {
    stop("No se encontró el diccionario: ", dictionary_file, call. = FALSE)
  }

  dict <- read_delim(
    dictionary_file,
    delim = ";",
    show_col_types = FALSE,
    col_types = cols(
      original_name = col_character(),
      standardized_name = col_character(),
      variable_clean = col_character(),
      years_present = col_character(),
      n_original_names = col_double(),
      use = col_double(),
      review_status = col_character(),
      notes = col_character()
    )
  )

  dict <- dict %>%
    mutate(
      original_name = str_trim(original_name),
      standardized_name = str_trim(standardized_name)
    ) %>%
    filter(use == 1)

  return(dict)
}

# ------------------------------------------------------------
# Estandarizar nombres de columnas de un dataframe
#
# Entrada:
#   df   : dataframe cuyas columnas se quieren renombrar
#   dict : diccionario cargado con load_variable_dictionary()
#
# Salida:
#   el mismo dataframe con nombres estandarizados cuando
#   exista una correspondencia en el diccionario
#
# Nota:
#   Solo renombra las variables presentes en el dataframe.
# ------------------------------------------------------------
standardize_variable_names <- function(df, dict) {

  current_names <- names(df)

  # Crear vector de renombrado:
  # nombres = nombre actual
  # valores = nombre estandarizado
  rename_map <- dict %>%
    filter(original_name %in% current_names) %>%
    distinct(original_name, standardized_name)

  # Comprobar si dos nombres originales distintos del mismo df
  # quieren transformarse en el mismo nombre final.
  duplicated_targets <- rename_map %>%
    count(standardized_name) %>%
    filter(n > 1)

  if (nrow(duplicated_targets) > 0) {
    stop(
      paste0(
        "Conflicto en el renombrado: varias columnas del dataframe ",
        "se mapearían al mismo nombre estandarizado: ",
        paste(duplicated_targets$standardized_name, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  # Convertir a vector nombrado para rename()
  rename_vector <- rename_map$original_name
  names(rename_vector) <- rename_map$standardized_name

  # Si no hay nada que renombrar, devolver el df tal cual
  if (length(rename_vector) == 0) {
    return(df)
  }

  df <- df %>%
    rename(!!!rename_vector)

  return(df)
}