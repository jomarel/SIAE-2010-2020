config_path <- if (file.exists("scripts/00_config.R")) "scripts/00_config.R" else "00_config.R"
source(config_path)

library(dplyr)
library(tidyr)

years <- 2010:2020

for (year in years) {
  
  folder <- file.path(RAW_DIR, as.character(year))
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
    
    # Quita espacios en nombres (p. ej. "NCODI " -> "NCODI")
    names(tmp) <- trimws(names(tmp))
    
    # Si NCODI no existe, intenta localizar variantes (caso/espacios)
    if (!("NCODI" %in% names(tmp))) {
      cand <- grep("^NCODI$", names(tmp), ignore.case = TRUE, value = TRUE)
      if (length(cand) == 1) names(tmp)[names(tmp) == cand] <- "NCODI"
    }
    
    # Si sigue sin NCODI, saltamos este fichero (si no, romperá el join)
    if (!("NCODI" %in% names(tmp))) {
      warning(sprintf("Archivo sin NCODI (se omite): %s", basename(f)))
      if (!("NCODI" %in% names(tmp))) {
        message("Columnas en ", basename(f), ": ", paste(names(tmp), collapse = ", "))
        warning(sprintf("Archivo sin NCODI (se omite): %s", basename(f)))
        next
      }
        next
    }
    
    # Fuerza NCODI a character para evitar problemas de formatos/ceros
    tmp$NCODI <- as.character(tmp$NCODI)
    
    # Asegura anyo
    if (!("anyo" %in% names(tmp))) tmp$anyo <- year
    
    df_list[[length(df_list) + 1]] <- tmp
  }
  
  stopifnot(length(df_list) > 0)
  
  df <- df_list[[1]]
  if (!("anyo" %in% names(df))) df$anyo <- year
  
  if (length(df_list) > 1) {
    for (i in 2:length(df_list)) {
      df <- full_join(df, df_list[[i]] %>% select(-anyo), by = "NCODI")
    }
  }
  
  # Si por lo que sea anyo no existe aún:
  if (!("anyo" %in% names(df))) df$anyo <- year
  
  df_year <- df %>% select(anyo, NCODI, everything())
  
  # OJO: aquí tu select gigante de variables irá después,
  # pero primero conviene comprobar que existen.
  # df_year <- df %>% select( ... tu lista ... )
  
  write.table(
    df_year,
    paste0("h:/Mi unidad/Tesis/Datos con R/SIAE 2010-2020/df_", year, ".csv"),
    sep = ",",
    row.names = FALSE,
    col.names = TRUE
  )
}