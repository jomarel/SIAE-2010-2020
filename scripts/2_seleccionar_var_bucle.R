config_path <- if (file.exists("scripts/00_config.R")) "scripts/00_config.R" else "00_config.R"
source(config_path)

required_pkgs <- c("dplyr", "tidyr", "fs", "stringr", "readr")
missing_pkgs <- required_pkgs[!vapply(required_pkgs, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing_pkgs) > 0) {
  stop(
    "Faltan paquetes: ",
    paste(missing_pkgs, collapse = ", "),
    "\nInstálalos manualmente con install.packages()."
  )
}

library(dplyr)
library(tidyr)
library(fs)
library(stringr)
library(readr)

STD_RAW_DIR <- file.path(INT_DIR, "standardized_raw")

year_dirs <- fs::dir_ls(STD_RAW_DIR, type = "directory", recurse = FALSE)
years <- year_dirs %>%
  fs::path_file() %>%
  stringr::str_extract("^\\d{4}$") %>%
  stats::na.omit() %>%
  sort() %>%
  as.integer()

stopifnot(length(years) > 0)

normalize_key_names <- function(df, year) {
  names(df) <- trimws(names(df))

  names(df)[names(df) == "Año"] <- "anyo"
  names(df)[names(df) == "año"] <- "anyo"
  names(df)[names(df) == "Anyo"] <- "anyo"
  names(df)[names(df) == "AÃ±o"] <- "anyo"
  names(df)[names(df) == "year"] <- "anyo"

  if (!("NCODI" %in% names(df))) {
    cand <- grep("^NCODI$", names(df), ignore.case = TRUE, value = TRUE)
    if (length(cand) == 1) names(df)[names(df) == cand] <- "NCODI"
  }

  if (!("NCODI" %in% names(df))) {
    return(NULL)
  }

  df$NCODI <- as.character(df$NCODI)

  if (!("anyo" %in% names(df))) {
    df$anyo <- year
  } else {
    df$anyo <- ifelse(is.na(df$anyo) | df$anyo == "", year, df$anyo)
  }

  df$anyo <- as.integer(df$anyo)

  df
}

for (year in years) {
  folder <- file.path(STD_RAW_DIR, as.character(year))
  stopifnot(dir.exists(folder))

  files <- list.files(path = folder, pattern = "\\.txt$", full.names = TRUE)
  stopifnot(length(files) > 0)

  df_list <- list()

  for (f in files) {
    tmp <- readr::read_delim(
      f,
      delim = ";",
      locale = locale(decimal_mark = ","),
      show_col_types = FALSE,
      progress = FALSE,
      name_repair = "minimal"
    ) %>%
      as.data.frame(stringsAsFactors = FALSE)

    tmp <- normalize_key_names(tmp, year)

    if (is.null(tmp)) {
      warning(sprintf("Archivo sin NCODI (se omite): %s", basename(f)))
      next
    }

    # Quitar columnas duplicadas exactas dentro del propio archivo
    tmp <- tmp[, !duplicated(names(tmp)), drop = FALSE]

    # Validar unicidad de clave
    if (any(duplicated(tmp[, c("NCODI", "anyo")]))) {
      warning(sprintf(
        "Hay duplicados de clave (NCODI, anyo) en %s. Se conserva la primera fila por clave.",
        basename(f)
      ))
      tmp <- tmp %>%
        distinct(NCODI, anyo, .keep_all = TRUE)
    }

    attr(tmp, "source_file") <- basename(f)
    df_list[[length(df_list) + 1]] <- tmp
  }

  if (length(df_list) == 0) {
    warning(sprintf("No hay archivos válidos para el año %s", year))
    next
  }

  df <- df_list[[1]]

  if (length(df_list) > 1) {
    for (i in 2:length(df_list)) {
      rhs <- df_list[[i]]

      common_vars <- intersect(names(df), names(rhs))
      common_vars <- setdiff(common_vars, c("NCODI", "anyo"))

      if (length(common_vars) > 0) {
        warning(sprintf(
          "Columnas repetidas entre módulos en %s: %s. Se eliminan del módulo derecho.",
          attr(rhs, "source_file"),
          paste(common_vars, collapse = ", ")
        ))
        rhs <- rhs %>% select(-all_of(common_vars))
      }

      df <- full_join(df, rhs, by = c("NCODI", "anyo"))
    }
  }

  df_year <- df %>%
    select(anyo, NCODI, everything()) %>%
    arrange(anyo, NCODI)

  write.table(
    df_year,
    file.path(LEGACY_BASE_DIR, paste0("df_", year, ".csv")),
    sep = ",",
    row.names = FALSE,
    col.names = TRUE,
    na = ""
  )

  message(sprintf("Año %s procesado. Filas: %s. Columnas: %s", year, nrow(df_year), ncol(df_year)))
}