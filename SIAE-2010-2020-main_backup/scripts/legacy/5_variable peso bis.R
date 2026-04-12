# Actualizar el paquete 'Matrix'
install.packages("Matrix")
# Cargar el paquete Matrix
library(Matrix)
R.version.string

# Verificar la versión
packageVersion("Matrix")

install.packages("car")
install.packages("caret")
install.packages("tidyverse")
# Cargar librerías necesarias
library(tidyverse)
library(caret)
library(car)

# Paso 1: Cargar y fusionar datos
df_final <- read.table("h:\\Mi unidad\\Tesis\\Datos con R\\SIAE 2010-2020\\df_final.txt", sep = ";", dec = ",", header=TRUE)

pesos <- read.table("h:\\Mi unidad\\Tesis\\Datos con R\\SIAE 2010-2020\\pesos.txt",sep = ";", dec = ",", header = TRUE)
# Unir las bases de datos por NCODI y anyo
df <- merge(df_final, pesos, by = c("NCODI", "anyo"), all = TRUE)



# Obtener los nombres de las variables del dataframe df
nombres_df <- names(df)
library(Hmisc)
write.table(nombres_df, file = "h:/Mi unidad/Tesis/Datos con R/R con Chatbot/nombres_variables.txt", 
            row.names = FALSE, col.names = FALSE, quote = FALSE)




# Cargar los nombres de las variables desde el archivo de texto
nombres_variables <- read.table("h:/Mi unidad/Tesis/Datos con R/R con Chatbot/nombres_variables.txt", 
                                header = FALSE, stringsAsFactors = FALSE)

# Leer el archivo con codificación UTF-8
archivo_descripciones <- read.table("h:/Mi unidad/Tesis/Datos con R/R con Chatbot/variables_descripcion_305.txt", 
                                    header = FALSE, sep = "\t", stringsAsFactors = FALSE, fileEncoding = "UTF-8")

# Si UTF-8 no funciona correctamente, prueba con Latin-1
# archivo_descripciones <- read.table("h:/Mi unidad/Tesis/D




# Añadir las descripciones como etiquetas a las variables del dataframe df
for (i in seq_along(nombres_df)) {
  label(df[[nombres_df[i]]]) <- archivo_descripciones[i, 2]  # Segunda columna con las descripciones
}

# Suponiendo que la variable peso está en la última posición
df <- df %>% select(NCODI, anyo, peso, everything())
write.table(df, "h:/Mi unidad/Tesis/Datos con R/SIAE 2010-2020/df.txt", row.names = FALSE, sep = ";", dec = ",")



# Paso 2: Seleccionar variables y preparar datos
# Definir las variables tecnológicas
tecnologia_vars <- c("quirofanos_instalada", "acelerador_hospital", "TAC_hospital", "RNM_hospital")

# Crear un dataframe con estas variables
df_tecnologia <- df[, tecnologia_vars]

# Crear un índice lógico de filas sin valores perdidos
complete_cases <- complete.cases(df_tecnologia)



# Ejecutar el PCA en las filas completas
pca_tecnologia <- prcomp(df_tecnologia[complete_cases, ], scale. = TRUE)

# Inicializar la columna PC1_tecnologia con NA
df$PC1_tecnologia <- NA

# Asignar los scores del PCA a las filas correspondientes
df$PC1_tecnologia[complete_cases] <- pca_tecnologia$x[,1]


# Verificar la clase en el conjunto de entrenamiento
class(df_train$camas_instalada)
# Verificar la clase en el conjunto de predicción
class(df_pred$camas_instalada)

# Convertir a numérica en df_train
df_train$camas_instalada <- as.numeric(as.character(df_train$camas_instalada))

# Convertir a numérica en df_pred
df_pred$camas_instalada <- as.numeric(as.character(df_pred$camas_instalada))


# Crear el conjunto de entrenamiento con datos de peso conocidos y sin valores perdidos en PC1_tecnologia
df_train <- df[!is.na(df$peso) & !is.na(df$PC1_tecnologia), ]

# Ajustar el modelo nuevamente
modelo <- lm(peso ~ PC1_tecnologia + total_cMedicos + camas_instalada, data = df_train)
summary(modelo)

# Revisar el resumen del modelo
summary(modelo)

# Verificar multicolinealidad
vif(modelo)

# Realizar la predicción
df_pred$mix <- predict(modelo, newdata = df_pred)

# Verificar tipos de datos
str(df_train[, c("peso", "PC1_tecnologia", "total_cMedicos", "camas_instalada")])
str(df_pred[, c("PC1_tecnologia", "total_cMedicos", "camas_instalada")])

# Verificar valores perdidos
colSums(is.na(df_train[, c("peso", "PC1_tecnologia", "total_cMedicos", "camas_instalada")]))
colSums(is.na(df_pred[, c("PC1_tecnologia", "total_cMedicos", "camas_instalada")]))

df$mix <- df$peso
df$mix[is.na(df$peso) & !is.na(df$PC1_tecnologia)] <- df_pred$mix





# Número de elementos a reemplazar en df$mix
num_reemplazar <- sum(is.na(df$peso) & !is.na(df$PC1_tecnologia))

# Número de predicciones disponibles
num_predicciones <- length(df_pred$mix)

# Mostrar los números
cat("Número de elementos a reemplazar:", num_reemplazar, "\n")
cat("Número de predicciones disponibles:", num_predicciones, "\n")
#Si los números son diferentes, es porque hay filas en df_pred que no tienen una predicción (posiblemente porque tienen valores perdidos en las variables predictoras).

# Filas en df_pred con predicciones no NA
filas_con_prediccion <- which(!is.na(df_pred$mix))

# Obtener los IDs de fila originales
ids_filas_pred <- df_pred$row_id[filas_con_prediccion]
# Si no lo has hecho ya, agregar un identificador de fila
df$row_id <- seq_len(nrow(df))
df_pred$row_id <- df$row_id[is.na(df$peso) & !is.na(df$PC1_tecnologia)]


# Paso 1: Agregar un identificador único a df
df$row_id <- seq_len(nrow(df))

# Paso 2: Preparar el conjunto de predicción con row_id
condicion_prediccion <- is.na(df$peso) & !is.na(df$PC1_tecnologia)
df_pred <- df[condicion_prediccion, ]

# Paso 3: Filtrar df_pred para casos completos en las variables del modelo
variables_modelo <- c("PC1_tecnologia", "total_cMedicos", "camas_instalada")
df_pred <- df_pred[complete.cases(df_pred[, variables_modelo]), ]

# Paso 4: Realizar la predicción
df_pred$mix <- predict(modelo, newdata = df_pred)

# Paso 5: Asignar las predicciones al dataframe original usando row_id
# Inicializar df$mix con los valores conocidos de peso
df$mix <- df$peso

# Asignar las predicciones a las filas correspondientes en df
df$mix[df_pred$row_id] <- df_pred$mix

# Paso 6: (Opcional) Remover row_id si no es necesario
# df$row_id <- NULL
# Verificar dimensiones
cat("Número de filas en df:", nrow(df), "\n")
cat("Número de filas en df_pred:", nrow(df_pred), "\n")
cat("Longitud del vector de row_id en df_pred:", length(df_pred$row_id), "\n")

# Verificar que los row_id sean únicos
cat("Row IDs únicos en df_pred:", length(unique(df_pred$row_id)), "\n")

# Verificar que no haya valores NA en df_pred$mix
cat("Número de predicciones NA en df_pred$mix:", sum(is.na(df_pred$mix)), "\n")

# Crear un nuevo dataframe con las columnas NCODI, peso y mix
df_resultado <- df[, c("NCODI", "peso", "mix")]

# Ver las primeras filas del nuevo dataframe
head(df_resultado)
# Ver el dataframe completo
View(df_resultado)
# Filtrar hospitales con peso original desconocido y mix estimado
hospitales_estimados <- df_resultado[is.na(df_resultado$peso) & !is.na(df_resultado$mix), ]

# Ver los hospitales estimados
View(hospitales_estimados)

# Instalar y cargar ggplot2 si no lo tienes instalado
# install.packages("ggplot2")
library(ggplot2)

# Crear un gráfico de dispersión
ggplot(df_resultado, aes(x = peso, y = mix)) +
  geom_point(alpha = 0.6) +
  labs(title = "Comparación entre Peso Original y Mix Estimado",
       x = "Peso Original",
       y = "Mix Estimado") +
  theme_minimal()
# Inicializar df$mix con los valores conocidos de peso
df$mix <- df$peso

# Asignar las predicciones a las filas correspondientes en df usando row_id
df$mix[df_pred$row_id] <- df_pred$mix
# Crear un nuevo dataframe que incluya solo las filas donde mix no es NA
df_con_mix <- df[!is.na(df$mix), ]
# Verificar el número de filas y columnas
dim(df_con_mix)

# Ver las primeras filas del dataframe
head(df_con_mix)

# Verificar que no hay valores NA en mix
sum(is.na(df_con_mix$mix))
# Resetear los índices de fila
df_con_mix <- df_con_mix %>% dplyr::arrange(NCODI, anyo, mix) %>% dplyr::mutate(row_number = row_number())

# Ver las primeras filas
head(df_con_mix)
# Histograma de mix
ggplot(df_con_mix, aes(x = mix)) +
  geom_histogram(binwidth = 0.1, fill = "blue", color = "black", alpha = 0.7) +
  labs(title = "Distribución de Mix entre Hospitales", x = "Mix", y = "Frecuencia") +
  theme_minimal()

# Paso 1: Crear un vector con el orden deseado de las columnas
# Colocamos primero 'NCODI', 'anyo' y 'mix', luego el resto de las columnas
columnas_ordenadas <- c("NCODI", "anyo", "mix", setdiff(names(df_con_mix), c("NCODI", "anyo", "mix")))

# Paso 2: Reordenar las columnas del dataframe
df_con_mix <- df_con_mix[, columnas_ordenadas]

# Paso 3: Verificar que las columnas están en el orden correcto
head(df_con_mix)


# Verificar que las variables existen en el dataframe
variables_necesarias <- c("mix", "camas_instalada", "salas_rx_Hospital", "acelerador_hospital", "TAC_hospital")
missing_vars <- setdiff(variables_necesarias, names(df_con_mix))

if(length(missing_vars) > 0){
  cat("Las siguientes variables faltan en df_con_mix:", paste(missing_vars, collapse = ", "), "\n")
} else {
  cat("Todas las variables están presentes en df_con_mix.\n")
}
# Instalar gridExtra si no está instalado
install.packages("gridExtra")

library(gridExtra)

# Crear los gráficos individualmente
plot1 <- ggplot(df_con_mix, aes(x = camas_instalada, y = mix)) +
  geom_point(color = "blue", alpha = 0.6) +
  labs(title = "Mix vs Camas Instaladas", x = "Camas Instaladas", y = "Mix") +
  theme_minimal()

plot2 <- ggplot(df_con_mix, aes(x = salas_rx_Hospital, y = mix)) +
  geom_point(color = "green", alpha = 0.6) +
  labs(title = "Mix vs Salas RX en Hospital", x = "Salas RX en Hospital", y = "Mix") +
  theme_minimal()

plot3 <- ggplot(df_con_mix, aes(x = factor(acelerador_hospital), y = mix)) +
  geom_boxplot(fill = "orange", alpha = 0.6) +
  labs(title = "Mix vs Aceleradores en Hospital", x = "Aceleradores en Hospital", y = "Mix") +
  theme_minimal()

plot4 <- ggplot(df_con_mix, aes(x = TAC_hospital, y = mix)) +
  geom_point(color = "purple", alpha = 0.6) +
  labs(title = "Mix vs TAC en Hospital", x = "Equipos TAC en Hospital", y = "Mix") +
  theme_minimal()

# Mostrar los gráficos en un panel
grid.arrange(plot1, plot2, plot3, plot4, ncol = 2)


# Convertir de factor a numérico
df_con_mix$camas_instalada <- as.numeric(as.character(df_con_mix$camas_instalada))
class(df_con_mix$camas_instalada)
sum(is.na(df_con_mix$camas_instalada))
head(df_con_mix$camas_instalada)

correlacion_camas <- cor(df_con_mix$mix, df_con_mix$camas_instalada, use = "complete.obs")
correlacion_salas_rx <- cor(df_con_mix$mix, df_con_mix$salas_rx_Hospital, use = "complete.obs")
correlacion_TAC <- cor(df_con_mix$mix, df_con_mix$TAC_hospital, use = "complete.obs")

cat("Correlación entre mix y camas_instalada:", round(correlacion_camas, 2), "\n")
cat("Correlación entre mix y salas_rx_Hospital:", round(correlacion_salas_rx, 2), "\n")
cat("Correlación entre mix y TAC_hospital:", round(correlacion_TAC, 2), "\n")
# Correlación con 'salas_rx_Hospital'
correlacion_salas_rx <- cor(df_con_mix$mix, df_con_mix$salas_rx_Hospital, use = "complete.obs")
cat("Correlación entre mix y salas_rx_Hospital:", round(correlacion_salas_rx, 2), "\n")

# Correlación con 'TAC_hospital'
correlacion_TAC <- cor(df_con_mix$mix, df_con_mix$TAC_hospital, use = "complete.obs")
cat("Correlación entre mix y TAC_hospital:", round(correlacion_TAC, 2), "\n")


















# Seleccionar variables numéricas relevantes
# Lista de variables que intentas seleccionar
variables_modelo <- c("peso", "camas_funcionamiento", "quirofanos_instalada", "acelerador_hospital",
                      "TAC_hospital", "RNM_hospital", "total_cMedicos", "altFinal_total",
                      "estancias_total", "G_totGastos", "I_totIngresos")
# Nombres de columnas en df
nombres_df <- names(df)

# Identificar las variables que no están en df
variables_no_encontradas <- setdiff(variables_modelo, nombres_df)

# Mostrar las variables que no se encontraron
if(length(variables_no_encontradas) > 0){
  cat("Las siguientes variables no se encontraron en df:\n")
  print(variables_no_encontradas)
} else {
  cat("Todas las variables se encontraron en df.\n")
}


# Crear un dataframe con las variables seleccionadas
df_modelo <- df[, variables_modelo]



# Verificar la clase de cada variable en df_modelo
str(df_modelo)












# Calcular matriz de correlación
correlaciones <- cor(df_modelo, use = "complete.obs")

# Visualizar la matriz de correlación
library(corrplot)
corrplot(correlaciones, method = "number", type = "upper")

install.packages("glmnet")
install.packages("randomForest")

# Cargar librerías necesarias
library(caret)
library(glmnet)
library(randomForest)

# Paso 1: Preparación de los Datos

#selecciono las variables con más de un 0,7 de correlación: 

variables_modelo <- c("peso", "camas_funcionamiento", "quirofanos_instalada", "total_cMedicos",    "altFinal_total","estancias_total", "G_totGastos", "I_totIngresos")

# Crear el dataframe con las variables seleccionadas
df_modelo <- df[, c("peso", variables_modelo)]

# Eliminar filas con valores perdidos en las variables seleccionadas
df_modelo <- df_modelo[complete.cases(df_modelo), ]

# Convertir factores a numéricos
for (var in names(df_modelo)) {
  if (is.factor(df_modelo[[var]])) {
    df_modelo[[var]] <- as.numeric(as.character(df_modelo[[var]]))
  }
}

# Dividir los datos en conjuntos de entrenamiento y prueba
set.seed(123)
train_index <- createDataPartition(df_modelo$peso, p = 0.8, list = FALSE)
df_train <- df_modelo[train_index, ]
df_test <- df_modelo[-train_index, ]

# Paso 2: Ajustar los Modelos

# Modelo Lineal Múltiple
modelo_lm <- lm(peso ~ ., data = df_train)

# Modelo Lasso
x_train <- model.matrix(peso ~ ., data = df_train)[, -1]
y_train <- df_train$peso
set.seed(123)
modelo_lasso_cv <- cv.glmnet(x_train, y_train, alpha = 1, nfolds = 10)
lambda_optimo <- modelo_lasso_cv$lambda.min
modelo_lasso <- glmnet(x_train, y_train, alpha = 1, lambda = lambda_optimo)

# Modelo Random Forest
set.seed(123)
modelo_rf <- randomForest(peso ~ ., data = df_train, na.action = na.omit)

# Paso 3: Evaluar el Rendimiento de los Modelos

# Preparar los datos de prueba
x_test <- model.matrix(peso ~ ., data = df_test)[, -1]
y_test <- df_test$peso

# Predicciones
pred_lm <- predict(modelo_lm, newdata = df_test)
pred_lasso <- predict(modelo_lasso, newx = x_test)
pred_rf <- predict(modelo_rf, newdata = df_test)

# Calcular métricas
rmse_lm <- sqrt(mean((y_test - pred_lm)^2))
mae_lm <- mean(abs(y_test - pred_lm))

rmse_lasso <- sqrt(mean((y_test - pred_lasso)^2))
mae_lasso <- mean(abs(y_test - pred_lasso))

rmse_rf <- sqrt(mean((y_test - pred_rf)^2))
mae_rf <- mean(abs(y_test - pred_rf))

# Resultados
resultados <- data.frame(
  Modelo = c("Regresión Lineal Múltiple", "Regresión Lasso", "Random Forest"),
  RMSE = c(rmse_lm, rmse_lasso, rmse_rf),
  MAE = c(mae_lm, mae_lasso, mae_rf)
)
print(resultados)

# Paso 4: Evaluar el Modelo

# Evaluar multicolinealidad
vif_valores <- vif(modelo_lm)
print(vif_valores)

# Gráficos de diagnóstico
par(mfrow = c(2, 2))
plot(modelo_lm)

# Paso 6: Utilizar el Modelo para Estimar 'mix'

# Preparar el conjunto de predicción
df_pred <- df[is.na(df$peso), ]
variables_predictoras <- variables_modelo

for (var in variables_predictoras) {
  if (is.factor(df_pred[[var]])) {
    df_pred[[var]] <- as.numeric(as.character(df_pred[[var]]))
  }
}

df_pred <- df_pred[complete.cases(df_pred[, variables_predictoras]), ]

# Realizar las predicciones
df_pred$mix <- predict(modelo_lm, newdata = df_pred)

# Asignar las predicciones al dataframe original
df$mix[is.na(df$peso)] <- df_pred$mix
# Resumen del modelo lineal múltiple
summary(modelo_lm)

vif_valores <- vif(modelo_lm)
print(vif_valores)
# Verificar que 'mix' está completo
sum(is.na(df$mix))

