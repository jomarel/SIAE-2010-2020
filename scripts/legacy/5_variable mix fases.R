# Manipula y limpieza de datos
library(dplyr)
library(tidyr)

# Visualización
library(ggplot2)
#install.packages("corrplot")
library(corrplot)

# Modelado
library(glmnet)      # Para LASSO y Ridge
library(randomForest)
library(caret)
library(e1071)       # Para SVR si decides usarlo
library(gbm)         # Para Gradient Boosting
library(haven)
library(caret)
#Con esto, hemos completado la Fase 1 de exploración y preprocesamiento de datos. Hemos:
# Cargado los datos.
# Dividido el conjunto de datos en dos subconjuntos: con peso y sin peso.
# Eliminado variables con demasiados valores faltantes.
# Imputado valores faltantes en variables numéricas y categóricas.
# Estandarizado las variables numéricas.
# Aplicado los mismos pasos al conjunto de datos sin peso.
# ¿Hay algún paso hasta aquí que quieras revisar o alguna pregunta que desees hacer antes de continuar con la siguiente fase?

df_final <- read.table("h:\\Mi unidad\\Tesis\\Datos con R\\SIAE 2010-2020\\df_final.txt", sep = ";", dec = ",", header=TRUE)

pesos <- read.table("h:\\Mi unidad\\Tesis\\Datos con R\\SIAE 2010-2020\\pesos.txt",sep = ";", dec = ",", header = TRUE)
# Unir las bases de datos por NCODI y anyo
df <- merge(df_final, pesos, by = c("NCODI", "anyo"), all = TRUE)



# Suponiendo que la variable peso está en la última posición
df <- df %>% select(NCODI, anyo, peso, everything())
write.table(df, "h:/Mi unidad/Tesis/Datos con R/SIAE 2010-2020/df.txt", row.names = FALSE, sep = ";", dec = ",")



# df <- read.table("h:/Mi unidad/Tesis/Datos con R/SIAE 2010-2020/df.txt", 
#                    header = TRUE, 
#                    sep = ";",        # Cambia a sep="," si el separador es coma
)


# Leer el archivo df_lab.dta  esta en dta porque así incoropra ya las etiquetas.
data <- read_dta("h:/Mi unidad/Tesis/Datos con R/SIAE 2010-2020/df_lab.dta")



# Conjunto de datos con "peso" disponible
data_peso <- data %>% filter(!is.na(peso))

# Conjunto de datos sin "peso"
data_sin_peso <- data %>% filter(is.na(peso))
# Resumen estadístico de las variables numéricas
summary(data_peso)

# Visualizar las primeras filas del conjunto de datos
head(data_peso)

# Calcular el porcentaje de valores faltantes por variable
missing_pct <- sapply(data_peso, function(x) sum(is.na(x)) / length(x)) * 100

# Convertir a data frame para facilitar la visualización
missing_df <- data.frame(variable = names(missing_pct), missing_pct = missing_pct)

# Ordenar de mayor a menor porcentaje de valores faltantes
missing_df <- missing_df %>% arrange(desc(missing_pct))

# Visualizar las variables con mayor porcentaje de valores faltantes
print(missing_df)
# Identificar variables con más del 50% de valores faltantes
vars_to_remove <- missing_df %>% filter(missing_pct > 50) %>% pull(variable)

# Eliminar estas variables del conjunto de datos
data_peso_clean <- data_peso %>% select(-one_of(vars_to_remove))
# Obtener nombres de variables numéricas
numeric_vars <- names(data_peso_clean)[sapply(data_peso_clean, is.numeric)]

# Suponiendo que 'numeric_vars' es un vector con los nombres de las variables numéricas
# Excluir 'NCODI' de 'numeric_vars'
numeric_vars <- setdiff(numeric_vars, "NCODI")



# Imputar valores faltantes con la media
for (var in numeric_vars) {
  data_peso_clean[[var]][is.na(data_peso_clean[[var]])] <- mean(data_peso_clean[[var]], na.rm = TRUE)
}
# Obtener nombres de variables categóricas
categorical_vars <- names(data_peso_clean)[sapply(data_peso_clean, is.character)]

# Función para calcular la moda
get_mode <- function(v) {
  uniqv <- unique(v[!is.na(v)])
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

# Imputar valores faltantes con la moda
for (var in categorical_vars) {
  mode_value <- get_mode(data_peso_clean[[var]])
  data_peso_clean[[var]][is.na(data_peso_clean[[var]])] <- mode_value
}
# Estandarizar variables numéricas
data_peso_clean[numeric_vars] <- scale(data_peso_clean[numeric_vars])


# Eliminar las mismas variables
data_sin_peso_clean <- data_sin_peso %>% select(-one_of(vars_to_remove))
# Imputar valores faltantes con la media calculada en data_peso_clean
for (var in numeric_vars) {
  mean_value <- mean(data_peso_clean[[var]], na.rm = TRUE)
  data_sin_peso_clean[[var]][is.na(data_sin_peso_clean[[var]])] <- mean_value
}
# Imputar valores faltantes con la moda calculada en data_peso_clean
for (var in categorical_vars) {
  mode_value <- get_mode(data_peso_clean[[var]])
  data_sin_peso_clean[[var]][is.na(data_sin_peso_clean[[var]])] <- mode_value
}
# Obtener la media y desviación estándar de cada variable numérica en data_peso_clean
mean_vars <- sapply(data_peso_clean[numeric_vars], mean)
sd_vars <- sapply(data_peso_clean[numeric_vars], sd)

# Estandarizar data_sin_peso_clean utilizando los parámetros de data_peso_clean
for (var in numeric_vars) {
  data_sin_peso_clean[[var]] <- (data_sin_peso_clean[[var]] - mean_vars[var]) / sd_vars[var]
}




# Fase 2: Análisis Exploratorio de Datos (EDA)
# 
# En esta fase, exploraremos las relaciones entre la variable objetivo peso y las demás variables del conjunto de datos. El objetivo es identificar variables que puedan tener una relación significativa con peso y que podrían ser útiles en la predicción del "mix" hospitalario.


numeric_vars <- setdiff(numeric_vars, "peso")  # Excluir 'peso' de las variables independientes

# Crear un data frame solo con variables numéricas
numeric_data <- data_peso_clean[, c("peso", numeric_vars)]

# Verificar el número total de valores faltantes
total_na <- sum(is.na(numeric_data))
cat("Total de valores faltantes en numeric_data:", total_na, "\n")

# Verificar el número de filas y columnas
cat("Dimensiones de numeric_data:", dim(numeric_data), "\n")

# Verificar el número de casos completos
n_complete_cases <- sum(complete.cases(numeric_data))
cat("Número de casos completos:", n_complete_cases, "\n")
# Calcular el número de valores faltantes por variable
na_per_variable <- colSums(is.na(numeric_data))

# Identificar variables con todos los valores como NA
variables_all_na <- names(na_per_variable[na_per_variable == nrow(numeric_data)])

# Mostrar las variables con todos los valores como NA
cat("Variables con todos sus valores como NA:\n")
print(variables_all_na)
# Eliminar variables con todos sus valores como NA
numeric_data <- numeric_data[, !(names(numeric_data) %in% variables_all_na)]
# Número de casos completos después de eliminar variables con todos sus valores NA
n_complete_cases <- sum(complete.cases(numeric_data))
cat("Número de casos completos después de limpiar variables:", n_complete_cases, "\n")
# Actualizar numeric_data
numeric_data <- data_peso_clean[, c("peso", numeric_vars)]

# Verificar nuevamente valores faltantes
total_na <- sum(is.na(numeric_data))
cat("Total de valores faltantes después de la imputación:", total_na, "\n")

# Calcular la matriz de correlación
cor_matrix <- cor(numeric_data)


# Convertir la matriz de correlación en un data frame
cor_peso <- as.data.frame(cor_matrix[,"peso"])

# Renombrar la columna para mayor claridad
colnames(cor_peso) <- "correlation_with_peso"

# Ordenar las variables por valor absoluto de la correlación
cor_peso$variable <- rownames(cor_peso)
cor_peso <- cor_peso %>% 
  filter(variable != "peso") %>%  # Excluir la variable 'peso' en sí misma
  arrange(desc(abs(correlation_with_peso)))

# Mostrar las variables con mayor correlación con 'peso'
head(cor_peso, 20)  # Mostrar las 20 variables más correlacionadas

# Guardar la tabla de correlaciones con 'peso' en un archivo CSV
write.csv(cor_peso, "h:/Mi unidad/Tesis/Datos con R/SIAE 2010-2020/correlacion_con_peso.csv", row.names = FALSE)






