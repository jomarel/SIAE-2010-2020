config_path <- if (file.exists("scripts/00_config.R")) "scripts/00_config.R" else "00_config.R"
source(config_path)

library(dplyr)
library(tidyr)
library(fs)

STD_RAW_DIR <- file.path(INT_DIR, "standardized_raw")

year_dirs <- fs::dir_ls(STD_RAW_DIR, type = "directory", recurse = FALSE)
years <- year_dirs %>%
  fs::path_file() %>%
  stringr::str_extract("^\\d{4}$") %>%
  stats::na.omit() %>%
  sort() %>%
  as.integer()

stopifnot(length(years) > 0)

for (year in years) {
  folder <- file.path(STD_RAW_DIR, as.character(year))
  stopifnot(dir.exists(folder))

  files <- list.files(path = folder, pattern = "\\.txt$", full.names = TRUE)

  df_list <- list()

  for (f in files) {
    tmp <- read.csv(
      f,
      sep = ";",
      dec = ",",
      stringsAsFactors = FALSE,
      check.names = FALSE
    )

    names(tmp) <- trimws(names(tmp))

    # Refuerzo de estandarización del año
    names(tmp)[names(tmp) == "Año"] <- "anyo"
    names(tmp)[names(tmp) == "año"] <- "anyo"
    names(tmp)[names(tmp) == "Anyo"] <- "anyo"
    names(tmp)[names(tmp) == "AÃ±o"] <- "anyo"
    names(tmp)[names(tmp) == "year"] <- "anyo"

    if (!("NCODI" %in% names(tmp))) {
      cand <- grep("^NCODI$", names(tmp), ignore.case = TRUE, value = TRUE)
      if (length(cand) == 1) names(tmp)[names(tmp) == cand] <- "NCODI"
    }

    if (!("NCODI" %in% names(tmp))) {
      warning(sprintf("Archivo sin NCODI (se omite): %s", basename(f)))
      message("Columnas en ", basename(f), ": ", paste(names(tmp), collapse = ", "))
      next
    }

    tmp$NCODI <- as.character(tmp$NCODI)

    if (!("anyo" %in% names(tmp))) tmp$anyo <- year

    df_list[[length(df_list) + 1]] <- tmp
  }

  stopifnot(length(df_list) > 0)

  df <- df_list[[1]]
  if (!("anyo" %in% names(df))) df$anyo <- year

  if (length(df_list) > 1) {
    for (i in 2:length(df_list)) {
      drop_cols <- intersect(c("anyo", "año", "Año", "year"), names(df_list[[i]]))
      rhs <- if (length(drop_cols) > 0) dplyr::select(df_list[[i]], -all_of(drop_cols)) else df_list[[i]]
      df <- full_join(df, rhs, by = "NCODI")
    }
  }

  if (!("anyo" %in% names(df))) df$anyo <- year

  df_year <- df %>% select(anyo, NCODI, everything())

  write.table(
    df_year,
    file.path(LEGACY_BASE_DIR, paste0("df_", year, ".csv")),
    sep = ",",
    row.names = FALSE,
    col.names = TRUE
  )
}
