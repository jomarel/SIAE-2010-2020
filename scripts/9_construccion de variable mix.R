# Cargar las librerías necesarias
library(dplyr)
library(caret)
library(glmnet)
library(randomForest)

install.packages("editData")
library("editData")
# Leer las bases de datos
df_final <- read.table("h:\\Mi unidad\\Tesis\\Datos con R\\SIAE 2010-2020\\df_final.txt", sep = ";", dec = ",", header = TRUE)
pesos <- read.table("h:\\Mi unidad\\Tesis\\Datos con R\\SIAE 2010-2020\\pesos.txt", sep = ";", dec = ",", header = TRUE)

# Realizar el merge para unir los pesos a la base 'df_final'
df <- left_join(df_final, pesos, by = c("NCODI", "anyo"))

# Listar las variables a excluir (identificadores y categóricas)
variables_excluir <- c("NCODI", "anyo", "ccaa_codigo", "ccaa", 
                       "cod_finalidad_agrupada", "Finalidad_agrupada", 
                       "cod_depend_agrupada", "Depend_agrupada")

# Identificar variables numéricas (convertibles a numéricas)
variables_numericas <- names(df)[!(names(df) %in% variables_excluir)]
df[variables_numericas] <- lapply(df[variables_numericas], function(x) as.numeric(as.character(x)))

# Calcular porcentaje de NA por variable
porcentaje_na <- sapply(df[variables_numericas], function(x) mean(is.na(x)) * 100)

# Visualizar variables con alto porcentaje de NA
variables_muchos_na <- names(porcentaje_na[porcentaje_na > 50])

# Excluir variables con más de 50% de NA
variables_numericas <- setdiff(variables_numericas, variables_muchos_na)

# Imputar NA con la mediana
for (var in variables_numericas) {
  mediana <- median(df[[var]], na.rm = TRUE)
  df[[var]][is.na(df[[var]])] <- mediana
}

# Conjunto de entrenamiento
df_entrenamiento <- df %>% filter(!is.na(peso))

# Conjunto de predicción
df_prediccion <- df %>% filter(is.na(peso))

# Matriz de características (excluyendo 'peso' y variables no numéricas)
X_entrenamiento <- as.matrix(df_entrenamiento %>% select(all_of(variables_numericas)))
y_entrenamiento <- df_entrenamiento$peso

# Identificar variables con varianza cero
nzv <- nearZeroVar(X_entrenamiento, saveMetrics = TRUE)
variables_cero_varianza <- rownames(nzv[nzv$zeroVar == TRUE, ])

# Remover variables con varianza cero de X_entrenamiento
X_entrenamiento <- X_entrenamiento[, !(colnames(X_entrenamiento) %in% variables_cero_varianza)]

# Estandarizar las variables
preProcValues <- preProcess(X_entrenamiento, method = c("center", "scale"))
X_entrenamiento <- predict(preProcValues, X_entrenamiento)

# Ajustar modelo Lasso
set.seed(123)
cv_lasso <- cv.glmnet(X_entrenamiento, y_entrenamiento, alpha = 1, nfolds = 10)

# Obtener el valor óptimo de lambda
lambda_optimo <- cv_lasso$lambda.min



# Variables seleccionadas
coeficientes <- coef(cv_lasso, s = lambda_optimo)
variables_seleccionadas <- rownames(coeficientes)[which(coeficientes != 0)]
variables_seleccionadas <- variables_seleccionadas[variables_seleccionadas != "(Intercept)"]

# Matriz de características con variables seleccionadas
X_entrenamiento_sel <- X_entrenamiento[, variables_seleccionadas, drop = FALSE]

# Estandarizar las variables seleccionadas
preProcValues <- preProcess(X_entrenamiento_sel, method = c("center", "scale"))
X_entrenamiento_sel <- predict(preProcValues, X_entrenamiento_sel)

# Preparar el conjunto de predicción con las mismas variables seleccionadas
X_prediccion <- as.matrix(df_prediccion %>% select(all_of(variables_seleccionadas)))

# Asegurarse de que X_prediccion tenga las mismas columnas que X_entrenamiento_sel
# Si faltan columnas en X_prediccion, agregarlas con ceros o NA
faltantes <- setdiff(colnames(X_entrenamiento_sel), colnames(X_prediccion))
if(length(faltantes) > 0){
  for(faltante in faltantes){
    X_prediccion[[faltante]] <- NA  # O 0, dependiendo de cómo quieras manejarlo
  }
}

# Reordenar las columnas para que coincidan
X_prediccion <- X_prediccion[, colnames(X_entrenamiento_sel), drop = FALSE]

# Estandarizar las variables del conjunto de predicción
X_prediccion <- predict(preProcValues, X_prediccion)

# --- Tu código anterior hasta entrenar el modelo Random Forest ---

# Entrenar el modelo Random Forest
mtry_optimo <- min(145, ncol(X_entrenamiento_sel)) # Ajusta `mtry` al máximo permitido
set.seed(123)
modelo_rf <- randomForest(x = X_entrenamiento_sel, y = y_entrenamiento, 
                          mtry = mtry_optimo, 
                          importance = TRUE)

# --- Generar predicciones en el conjunto de entrenamiento ---
# Predicciones en el conjunto de entrenamiento
predicciones_entrenamiento <- predict(modelo_rf, X_entrenamiento_sel)

# Añadir predicciones al conjunto de entrenamiento
df_entrenamiento$mix <- predicciones_entrenamiento

# --- Generar predicciones en el conjunto de predicción ---
# Predicciones en el conjunto de predicción
predicciones <- predict(modelo_rf, X_prediccion)

# Añadir predicciones al conjunto de predicción
df_prediccion$mix <- predicciones

# --- Combinar los conjuntos sin reemplazar 'mix' con 'peso' ---
df_completo <- bind_rows(df_entrenamiento, df_prediccion)
df_completo <- df_completo %>% arrange(NCODI, anyo,mix,peso)
save(df_completo, file="df_completo.RData")
# --- Evaluar el modelo ---
df_evaluacion <- df_completo %>% filter(!is.na(peso))
peso_real <- df_evaluacion$peso
peso_predicho <- df_evaluacion$mix

# Calcular las métricas
MAE <- mean(abs(peso_real - peso_predicho))
MSE <- mean((peso_real - peso_predicho)^2)
RMSE <- sqrt(MSE)
MAPE <- mean(abs((peso_real - peso_predicho) / peso_real)) * 100
R2_value <- R2(peso_predicho, peso_real)

cat("MAE:", round(MAE, 2), "\n")
cat("RMSE:", round(RMSE, 2), "\n")
cat("MAPE:", round(MAPE, 2), "%\n")
cat("R²:", round(R2_value, 2), "\n")

# --- Crear los gráficos de evaluación ---
library(ggplot2)

# Agregar los residuos al dataframe
df_evaluacion <- df_evaluacion %>%
  mutate(residuals = peso_real - peso_predicho,
         mean_values = (peso_real + peso_predicho) / 2)

# 1. Gráfico de Dispersión
ggplot(df_evaluacion, aes(x = peso_real, y = peso_predicho)) +
  geom_point(color = 'blue', alpha = 0.6) +
  geom_abline(slope = 1, intercept = 0, color = 'red', linetype = 'dashed') +
  labs(title = "Peso Real vs Peso Predicho",
       x = "Peso Real",
       y = "Peso Predicho") +
  theme_minimal()

# 2. Gráfico de Residuales
ggplot(df_evaluacion, aes(x = peso_predicho, y = residuals)) +
  geom_point(color = 'darkgreen', alpha = 0.6) +
  geom_hline(yintercept = 0, color = 'red', linetype = 'dashed') +
  labs(title = "Residuales vs Peso Predicho",
       x = "Peso Predicho",
       y = "Residuales") +
  theme_minimal()

# 3. Histograma de Residuales
ggplot(df_evaluacion, aes(x = residuals)) +
  geom_histogram(binwidth = 0.5, fill = 'orange', color = 'black', alpha = 0.7) +
  labs(title = "Distribución de Residuales",
       x = "Residuales",
       y = "Frecuencia") +
  theme_minimal()

# 4. Gráfico Q-Q de Residuales
qqnorm(df_evaluacion$residuals)
qqline(df_evaluacion$residuals, col = "red", lwd = 2)

# 5. Gráfico Bland-Altman
ggplot(df_evaluacion, aes(x = mean_values, y = residuals)) +
  geom_point(color = 'purple', alpha = 0.6) +
  geom_hline(yintercept = mean(df_evaluacion$residuals), color = 'blue', linetype = 'dashed') +
  labs(title = "Gráfico Bland-Altman",
       x = "Promedio (Peso Real y Predicho)",
       y = "Diferencia (Peso Real - Predicho)") +
  theme_minimal()

# Guardar el dataframe actualizado en un archivo RData
save(df_completo, file = "h:/Mi unidad/Tesis/Datos con R/SIAE 2010-2020/df_completo.RData")


# Guardar el dataframe con la variable 'mix'
write.table(df_completo, "h:/Mi unidad/Tesis/Datos con R/SIAE 2010-2020/df_final_con_mix.txt", sep = ";", row.names = FALSE)

