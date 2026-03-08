config_path <- if (file.exists("scripts/00_config.R")) "scripts/00_config.R" else "00_config.R"
source(config_path)
# Instalar paquetes (si no están instalados)
install.packages("dplyr")
install.packages("sjlabelled")

# Cargar los paquetes necesarios
library(dplyr)
library(sjlabelled)

# Cargar el archivo .RData
load(DF_DEPURADO_RDATA_PATH)
# Obtener los nombres y etiquetas de las variables
nombres <- names(df_depurado)
etiquetas <- get_label(df_depurado)

# Crear un data frame con los nombres y etiquetas
df_etiquetas <- data.frame(
  Nombre_Variable = nombres,
  Etiqueta = etiquetas
)
# Guardar en un archivo .txt
write.table(df_etiquetas, 
            file = NOMBRES_ETIQUETAS_TXT_PATH, 
            sep = "\t", 
            row.names = FALSE, 
            quote = FALSE, 
            fileEncoding = "UTF-8")
# Reordenar columnas del DataFrame
df_depurado <- df_depurado %>%
  select(NCODI, anyo, ccaa, peso, mix, everything())
save(df_depurado,file=DF_DEPURADO_RDATA_PATH)

write.table(df_depurado, DF_DEPURADO_CSV_PATH, sep = ";", row.names = FALSE)



# Crear una cadena que combine nombres y etiquetas para el encabezado
encabezado <- paste0("# ", paste(df_etiquetas$Nombre_Variable, "(", df_etiquetas$Etiqueta, ")", collapse = "\t"))

# Crear un archivo .txt con el encabezado y los datos
archivo_salida <- DF_CON_ETIQUETAS_CSV_PATH

# Escribir el encabezado manualmente y luego el DataFrame
write(encabezado, file = archivo_salida)

# Escribir el DataFrame sin duplicar los nombres de columnas
write.table(df_depurado, 
            file = archivo_salida, 
            sep = ";", 
            row.names = FALSE, 
            col.names = FALSE,  # Cambiado a FALSE para evitar duplicar columnas
            append = TRUE, 
            quote = FALSE, 
            fileEncoding = "UTF-8")


