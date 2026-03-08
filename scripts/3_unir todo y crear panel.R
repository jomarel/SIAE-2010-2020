config_path <- if (file.exists("scripts/00_config.R")) "scripts/00_config.R" else "00_config.R"
source(config_path)
# Crea una lista vacía
list_df <- list()

# Lee cada archivo y almacena el data frame en la lista
for (i in 2010:2020) {
  list_df[[i-2009]] <- read.csv(file.path(LEGACY_BASE_DIR, paste0("df_", i, ".csv")))
  list_df[[i-2009]]$anyo <- i
}

# Usa do.call() junto con rbind() para unir los data frames en un solo data frame
df_final <- do.call(rbind, list_df)
#write.csv(df, "df_final.txt", row.names = FALSE)
 save(df_final, file = DF_FINAL_RDATA_PATH)
write.table(df_final, DF_FINAL_TXT_PATH, row.names = FALSE, sep = ";", dec = ",")
 




########################################################################################
# load(DF_FINAL_RDATA_PATH)
# str(df)

df_long <- melt(df, id.vars = c("anyo", "NCODI"))

# Cast the long dataframe into a panel format
df_panel <- dcast(df_long, NCODI + variable ~ anyo, value.var = "value")

# View the panel dataframe
# head(df_panel)



