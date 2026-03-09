config_path <- if (file.exists("scripts/00_config.R")) "scripts/00_config.R" else "00_config.R"
source(config_path)
# Crea una lista vacía
list_df <- list()

# Detecta los ficheros anuales disponibles (df_YYYY.csv) y ordénalos por año
year_files <- list.files(
  path = LEGACY_BASE_DIR,
  pattern = "^df_[0-9]{4}\\.csv$",
  full.names = TRUE
)

years <- as.integer(sub("^df_([0-9]{4})\\.csv$", "\\1", basename(year_files)))
ord <- order(years)
year_files <- year_files[ord]
years <- years[ord]

stopifnot(length(year_files) > 0)

# Lee cada archivo y almacena el data frame en la lista
for (idx in seq_along(year_files)) {
  list_df[[idx]] <- read.csv(year_files[idx])
  list_df[[idx]]$anyo <- years[idx]
}

# Usa do.call() junto con rbind() para unir los data frames en un solo data frame
df_final <- do.call(rbind, list_df)
#write.csv(df, "df_final.txt", row.names = FALSE)
 save(df_final, file = DF_FINAL_RDATA_PATH)
write.table(df_final, DF_FINAL_TXT_PATH, row.names = FALSE, sep = ";", dec = ",")
 


########################################################################################
# Bloque exploratorio desactivado para ejecucion batch del pipeline
if (FALSE) {
  # load(DF_FINAL_RDATA_PATH)
  # str(df)

  df_long <- melt(df, id.vars = c("anyo", "NCODI"))

  # Cast the long dataframe into a panel format
  df_panel <- dcast(df_long, NCODI + variable ~ anyo, value.var = "value")

  # View the panel dataframe
  # head(df_panel)
}




