if (!require(devtools)) install.packages("devtools")
devtools::install_github("boxuancui/DataExplorer")
library(dplyr)
library(DataExplorer)
library(ggplot2)
# create_report(df_completo,y="mix")
# create_report(df_completo)

# 1. Primero cargar dplyr
library(dplyr)

# 2. Cargar los datos

# Cargar el archivo actualizado
load("h:/Mi unidad/Tesis/Datos con R/SIAE 2010-2020/df_completo.RData")


# 3. Corregir Depend_agrupada
df_completo <- df_completo %>%
  mutate(Depend_agrupada = gsub("Publicos-SNS|Publicos_SNS", "Publicos", Depend_agrupada))

# 4. Eliminar NA en Depend_agrupada
df_completo <- df_completo %>%
  filter(!is.na(Depend_agrupada))

# 5. Corregir Finalidad_agrupada por NCODI
# Primero veamos cuántos NA hay
print("Número de NA en Finalidad_agrupada:")
sum(is.na(df_completo$Finalidad_agrupada))

# Cargar el paquete tidyr para usar fill()
library(tidyr)

# Eliminar NCODI 39
df_completo_actualizado <- df_completo %>%
  filter(NCODI != 39)

# Rellenar Finalidad_agrupada hacia adelante y hacia atrás dentro de cada grupo
df_completo_actualizado <- df_completo_actualizado %>%
  group_by(NCODI) %>%
  fill(Finalidad_agrupada, .direction = "downup") %>%
  ungroup()


# 7. Identificar y eliminar columnas completamente NA
missing_columns <- names(df_completo_actualizado)[colSums(is.na(df_completo_actualizado)) == nrow(df_completo_actualizado)]
print("Columnas completamente vacías:")
print(missing_columns)

df_completo_actualizado <- df_completo_actualizado %>%
  select(where(~!all(is.na(.))))

# Verificar dimensiones finales
print("Dimensiones finales del dataframe:")
dim(df_completo_actualizado)


# Guardar el dataframe 'df_completo' actualizado en un archivo RData
save(df_completo_actualizado, file = "h:/Mi unidad/Tesis/Datos con R/SIAE 2010-2020/df_completo_actualizado.RData")

# Cargar el archivo .RData
load("h:\\Mi unidad\\Tesis\\Datos con R\\SIAE 2010-2020\\df_completo_actualizado.RData")


# Read the file with tab-separated values
diccionario <- read.delim("variables_descripcion_305.csv", 
                          sep = ";", 
                          header = FALSE,
                          col.names = c("variable", "descripcion"),
                          stringsAsFactors = FALSE)

# Preview the first few rows
head(diccionario)


# Instalar si no lo tienes
install.packages("labelled")
library(labelled)

# Crear un vector con las etiquetas
etiquetas <- setNames(diccionario$descripcion, diccionario$variable)

# Aplicar las etiquetas
df_completo_actualizado <- set_variable_labels(df_completo_actualizado, .labels = etiquetas)

# Para verificar que funcionó
var_label(df_completo_actualizado)


# Primero guardemos los cambios en un nuevo objeto
df_etiquetado <- set_variable_labels(df_completo_actualizado, .labels = etiquetas)

# Verifiquemos que funcionó
head(var_label(df_etiquetado))

# Si funcionó, entonces reasignamos
df_completo_actualizado <- df_etiquetado


save(df_completo_actualizado,file="h:/Mi unidad/Tesis/Datos con R/SIAE 2010-2020/df_completo_actualizado.RData")

# Guardar el dataframe con la variable 'mix'
write.table(df_completo_actualizado, file="h:/Mi unidad/Tesis/Datos con R/SIAE 2010-2020/df_completo_actualizado.txt", sep = "\t", row.names = FALSE)
