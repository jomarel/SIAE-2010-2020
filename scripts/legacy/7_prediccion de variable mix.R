# Fase 3: Construcción del Modelo Predictivo para Predecir "peso"
# 
# Ahora que hemos seleccionado las variables numéricas más relevantes y hemos manejado los problemas de multicolinealidad y variables problemáticas, estamos listos para construir el modelo predictivo que nos permitirá estimar el peso (case mix) en los hospitales donde esta variable no está disponible.
# 
# En esta fase, te guiaré paso a paso para:
#   
#   Preparar el conjunto de datos para el modelado.
# Dividir los datos en conjuntos de entrenamiento y validación.
# Construir el modelo predictivo utilizando regresión lineal y LASSO.
# Evaluar el rendimiento del modelo.
# Aplicar el modelo a los datos sin peso para predecir el "mix".

data_final <- readRDS("H:/Mi unidad/Tesis/Datos con R/SIAE 2010-2020/data_final.rds")



# Verificar el conjunto de datos final
str(data_final)
# Número de observaciones y variables
cat("Número de observaciones:", nrow(data_final), "\n")
cat("Número de variables (incluyendo 'peso'):", ncol(data_final), "\n")

# Establecer una semilla para reproducibilidad
set.seed(123)

# Crear índices de entrenamiento
train_indices <- sample(seq_len(nrow(data_final)), size = 0.7 * nrow(data_final))

# Dividir los datos
train_data <- data_final[train_indices, ]
test_data <- data_final[-train_indices, ]

# Obtener los nombres de las variables predictoras
predictor_vars <- setdiff(names(train_data), "peso")

# Crear la fórmula del modelo
formula_model <- as.formula(paste("peso ~", paste(predictor_vars, collapse = " + ")))

# Ajustar el modelo en el conjunto de entrenamiento
lm_model <- lm(formula_model, data = train_data)

# Resumen del modelo
summary(lm_model)
# Gráfico de residuos vs. valores ajustados
plot(lm_model$fitted.values, lm_model$residuals,
     xlab = "Valores Ajustados", ylab = "Residuos",
     main = "Residuos vs. Valores Ajustados")
abline(h = 0, col = "red")

# Histograma de los residuos
hist(lm_model$residuals, breaks = 20, main = "Histograma de los Residuos",
     xlab = "Residuos", col = "lightblue")

# Prueba de normalidad de Shapiro-Wilk
shapiro.test(lm_model$residuals)
# Calcular el VIF
library(car)
vif_values <- vif(lm_model)
print(vif_values)


# Excluir 'NCODI' y otras variables no predictoras de 'predictor_vars'
predictor_vars <- setdiff(predictor_vars, c("NCODI", "peso"))


# Verificar las variables predictoras
print(predictor_vars)


# Matriz de variables predictoras y vector de respuesta
x_train <- as.matrix(train_data[, predictor_vars])
y_train <- train_data$peso


# Verificar valores faltantes en x_train
if (any(is.na(x_train))) {
  cat("Hay valores faltantes en x_train.\n")
}

# Verificar valores faltantes en y_train
if (any(is.na(y_train))) {
  cat("Hay valores faltantes en y_train.\n")
}



if (!require(glmnet)) install.packages("glmnet")
library(glmnet)

# Ajustar el modelo LASSO
set.seed(123)
cv_lasso <- cv.glmnet(x_train, y_train, alpha = 1, nfolds = 10)

# Obtener el lambda óptimo
best_lambda <- cv_lasso$lambda.min
cat("Mejor lambda:", best_lambda, "\n")

# Ajustar el modelo final
lasso_model <- glmnet(x_train, y_train, alpha = 1, lambda = best_lambda)

# Matriz de variables predictoras para los nuevos datos
x_new <- as.matrix(data_sin_peso_clean[, predictor_vars])
# Verificar valores faltantes en x_new
if (any(is.na(x_new))) {
  cat("Hay valores faltantes en x_new. Imputar o eliminar valores faltantes antes de predecir.\n")
}

# Reordenar x_new si es necesario
x_new <- x_new[, colnames(x_train)]

# Realizar las predicciones
data_sin_peso_clean$predicted_peso <- predict(lasso_model, s = best_lambda, newx = x_new)

# Resumen de las predicciones
summary(data_sin_peso_clean$predicted_peso)

# Verificar si hay valores NA o NaN en las predicciones
if (any(is.na(data_sin_peso_clean$predicted_peso))) {
  cat("Hay valores NA en las predicciones.\n")
}
if (any(is.nan(data_sin_peso_clean$predicted_peso))) {
  cat("Hay valores NaN en las predicciones.\n")
}


# Coeficientes del modelo
coeficientes <- coef(lasso_model)
selected_vars_lasso <- rownames(coeficientes)[coeficientes[,1] != 0]
selected_vars_lasso <- setdiff(selected_vars_lasso, "(Intercept)")

cat("Variables seleccionadas por LASSO:\n")
print(selected_vars_lasso)
# Crear la fórmula del modelo
formula_lasso <- as.formula(paste("peso ~", paste(selected_vars_lasso, collapse = " + ")))

# Ajustar el modelo
lm_model_lasso <- lm(formula_lasso, data = train_data)

# Resumen del modelo
summary(lm_model_lasso)

# Predicciones
predictions_lm <- predict(lm_model, newdata = test_data)
# Predicciones
predictions_lasso <- predict(lasso_model, s = best_lambda, newx = x_test)
# Función para calcular métricas
calcular_metricas <- function(actual, predicho) {
  mse <- mean((actual - predicho)^2)
  rmse <- sqrt(mse)
  ss_res <- sum((actual - predicho)^2)
  ss_tot <- sum((actual - mean(actual))^2)
  r_squared <- 1 - (ss_res / ss_tot)
  return(list(MSE = mse, RMSE = rmse, R_squared = r_squared))
}
metricas_lm <- calcular_metricas(y_test, predictions_lm)
cat("Modelo de Regresión Lineal:\n")
print(metricas_lm)
metricas_lasso <- calcular_metricas(y_test, predictions_lasso)
cat("Modelo LASSO:\n")
print(metricas_lasso)

# Paso 6: Aplicar el Modelo a los Datos sin peso para Predecir el "mix" 

# Supongamos que ya has cargado el conjunto de datos sin 'peso' en 'data_sin_peso'

# Verificar las primeras filas del conjunto de datos
head(data_sin_peso)
# Lista de variables numéricas seleccionadas por LASSO
numeric_vars <- selected_vars_lasso



# Realizar las predicciones
data_sin_peso_clean$predicted_peso <- predict(lasso_model, s = best_lambda, newx = x_new)
hist(data_sin_peso_clean$predicted_peso, breaks = 20, main = "Distribución de las Predicciones de 'peso'", xlab = "Predicciones de 'peso'", col = "lightblue")

# # Paso 8: Integrar las Predicciones al Conjunto de Datos
# 8.1. Unificar los Conjuntos de Datos
# Ahora tienes dos conjuntos de datos:
#   data_peso_clean: Conjunto de datos original donde peso estaba disponible.
# data_sin_peso_clean: Conjunto de datos donde has predicho peso utilizando el modelo LASSO.

# Asegurar que las columnas coincidan
# Si hay columnas en data_sin_peso_clean que no están en data_peso_clean, debes agregarlas o eliminarlas según corresponda.
# Paso 1: Renombrar 'predicted_peso' a 'peso' y eliminar 'predicted_peso' de data_sin_peso_clean
data_sin_peso_clean$peso <- data_sin_peso_clean$predicted_peso
data_sin_peso_clean$predicted_peso <- NULL  # Eliminar la columna 'predicted_peso'

# Paso 2: Agregar 'tipo_peso' en ambos data frames
data_peso_clean$tipo_peso <- "original"
data_sin_peso_clean$tipo_peso <- "predicho"

# Paso 3: Obtener nombres de columnas
colnames_peso <- colnames(data_peso_clean)
colnames_sin_peso <- colnames(data_sin_peso_clean)

# Paso 4: Verificar que 'predicted_peso' no está en colnames_sin_peso
if ("predicted_peso" %in% colnames_sin_peso) {
  stop("La columna 'predicted_peso' aún está presente en data_sin_peso_clean. Por favor, elimínala antes de continuar.")
}

# Paso 5: Identificar columnas faltantes y agregarlas
cols_only_in_peso <- setdiff(colnames_peso, colnames_sin_peso)
cols_only_in_sin_peso <- setdiff(colnames_sin_peso, colnames_peso)

for (col in cols_only_in_peso) {
  data_sin_peso_clean[[col]] <- NA
}

for (col in cols_only_in_sin_peso) {
  data_peso_clean[[col]] <- NA
}

# Paso 6: Recalcular 'all_columns' y reordenar las columnas
all_columns <- union(colnames_peso, colnames_sin_peso)
data_peso_clean <- data_peso_clean[, all_columns]
data_sin_peso_clean <- data_sin_peso_clean[, all_columns]

# Paso 7: Convertir factores a caracteres
data_peso_clean[] <- lapply(data_peso_clean, function(x) if(is.factor(x)) as.character(x) else x)
data_sin_peso_clean[] <- lapply(data_sin_peso_clean, function(x) if(is.factor(x)) as.character(x) else x)

# Paso 8: Unir los data frames
data_completo <- rbind(data_peso_clean, data_sin_peso_clean)
