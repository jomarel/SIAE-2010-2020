# Paso 1: Seleccionar Variables Numéricas Basadas en su Correlación con 'peso'
# Ya tienes la matriz de correlación entre las variables numéricas independientes y la variable objetivo peso. Vamos a utilizar esta información para seleccionar las variables numéricas más relevantes.
# 
# 1.1. Establecer un Umbral de Correlación
# Podemos establecer un umbral de correlación para seleccionar las variables que tienen una relación significativa con peso. Un umbral común es |correlación| ≥ 0.3, pero puedes ajustarlo según la cantidad de variables y la fuerza de las relaciones observadas.


# NO VOY A INCLUIR VARIABLES CATEGORICAS... SOLO HAY ESTAS: "ccaa"               "Finalidad_agrupada" "Depend_agrupada"    "camas_instalada"    "otroRegimen"  Y NO MERECE LA PENA DADO QUE SON TODOS HOSPITALES PÚBLICOS, AGUDOS, Y CON POCOS DATOS POR CCCAA PARA QUE ESO SEA RELEVANTE





# df_final <- read.delim("h:/Mi unidad/Tesis/Datos con R/SIAE 2010-2020/correlacion_con_peso.csv")


# Definir el umbral de correlación
threshold <- 0.5

# Seleccionar variables con correlación absoluta mayor al umbral
selected_numeric_vars <- cor_peso %>% 
  filter(abs(correlation_with_peso) >= threshold) %>% 
  arrange(desc(abs(correlation_with_peso))) %>% 
  pull(variable)

# Mostrar las variables seleccionadas y sus correlaciones
selected_numeric_vars_df <- cor_peso %>% 
  filter(variable %in% selected_numeric_vars)

print(selected_numeric_vars_df)

# Calcular valores p para las correlaciones
p_values <- sapply(selected_numeric_vars, function(var) {
  cor.test(data_peso_clean[[var]], data_peso_clean$peso)$p.value
})

# Crear un data frame con correlaciones y valores p
selected_numeric_vars_df <- selected_numeric_vars_df %>%
  mutate(p_value = p_values[variable])

# Filtrar variables con p < 0.05
selected_numeric_vars_df <- selected_numeric_vars_df %>% 
  filter(p_value < 0.05)

# Actualizar la lista de variables seleccionadas
selected_numeric_vars <- selected_numeric_vars_df$variable

print(selected_numeric_vars_df)

# Crear un data frame con las variables seleccionadas
data_model <- data_peso_clean[, c("peso", selected_numeric_vars)]

# NO MANEJO VARIABLES CON VARIANZA CERO O CASI CERO PORQUE NO HAY NINGUNA


# Calcular la matriz de correlación
cor_matrix <- cor(data_model[, -1])  # Excluimos 'peso'

# Encontrar pares de variables con correlación alta (por ejemplo, |correlación| > 0.9)
high_cor_pairs <- findCorrelation(cor_matrix, cutoff = 0.9, names = TRUE, exact = TRUE)

cat("Variables altamente correlacionadas (correlación > 0.9):\n")
print(high_cor_pairs)

# Eliminar variables altamente correlacionadas
if (length(high_cor_pairs) > 0) {
  data_model <- data_model[, !names(data_model) %in% high_cor_pairs]
}

#  ELIMINADAS LAS MUY CORRELACIONADAS PASAMOS A AJUSTAR MODELO DE REGRESION LINEAL (PASO 3)
# Obtener los nombres de las variables predictoras (excluyendo 'peso')
predictor_vars <- setdiff(names(data_model), "peso")

# Crear la fórmula del modelo
formula_model <- as.formula(paste("peso ~", paste(predictor_vars, collapse = " + ")))

# Verificar la fórmula
print(formula_model)


# Ajustar el modelo de regresión lineal
lm_model <- lm(formula_model, data = data_model)

# Resumen del modelo
summary(lm_model)






# Paso 4: Verificar Multicolinealidad entre Variables Independientes
# Dado que estamos incluyendo variables numéricas y categóricas (codificadas), es importante verificar la multicolinealidad.
# data_model_encoded <- data_model


# Calcular el VIF
# Instalar y cargar el paquete car si no lo tienes
if (!require(car)) install.packages("car")
library(car)

# Calcular el VIF
vif_values <- vif(lm_model)

# Mostrar los valores de VIF
print(vif_values)


# Mostrar variables con VIF alto
vif_threshold <- 5
high_vif_vars <- names(vif_values[vif_values > vif_threshold])
cat("Variables con VIF mayor a", vif_threshold, ":\n")
print(high_vif_vars)

# Función para eliminar variables con VIF alto
reduce_vif <- function(data, threshold = 5) {
  repeat {
    # Crear fórmula del modelo
    predictor_vars <- setdiff(names(data), "peso")
    formula_model <- as.formula(paste("peso ~", paste(predictor_vars, collapse = " + ")))
    
    # Ajustar el modelo
    lm_model <- lm(formula_model, data = data)
    
    # Calcular VIF
    vif_values <- vif(lm_model)
    
    # Obtener el máximo VIF
    max_vif <- max(vif_values)
    
    if (max_vif > threshold) {
      # Variable con mayor VIF
      var_to_remove <- names(vif_values)[which.max(vif_values)]
      cat("Eliminando variable por alto VIF:", var_to_remove, "\n")
      
      # Eliminar la variable del conjunto de datos
      data <- data %>% select(-one_of(var_to_remove))
    } else {
      break
    }
  }
  return(data)
}

# Aplicar la función
data_model_vif <- reduce_vif(data_model, threshold = vif_threshold)

# Crear fórmula con las variables restantes
predictor_vars <- setdiff(names(data_model_vif), "peso")
formula_model <- as.formula(paste("peso ~", paste(predictor_vars, collapse = " + ")))

# Ajustar el modelo
lm_model <- lm(formula_model, data = data_model_vif)

# Resumen del modelo
summary(lm_model)

# Paso 6: Ajustar el Modelo con Regularización (LASSO)  (EL PASO 5 ERA ELIMINAR VARIABLES CON ALIASING QUE QUEDARAN, COMO NO QUEDABA NINGUNA NO LO APLICO)
# Para manejar la multicolinealidad y realizar selección de variables, es recomendable utilizar métodos de regularización como LASSO.


# Separar variables independientes y dependiente
x <- as.matrix(data_model_vif[, predictor_vars])
y <- data_model_vif$peso
# Instalar y cargar glmnet si no está cargado
if (!require(glmnet)) install.packages("glmnet")
library(glmnet)

# Ajustar modelo LASSO con validación cruzada
set.seed(123)
cv_lasso <- cv.glmnet(x, y, alpha = 1, nfolds = 10)

# Obtener el lambda óptimo
best_lambda <- cv_lasso$lambda.min
cat("Mejor lambda:", best_lambda, "\n")

# Ajustar el modelo final con el mejor lambda
lasso_model <- glmnet(x, y, alpha = 1, lambda = best_lambda)

# Coeficientes del modelo
coeficientes <- coef(lasso_model)
selected_vars_lasso <- rownames(coeficientes)[coeficientes[,1] != 0]
selected_vars_lasso <- setdiff(selected_vars_lasso, "(Intercept)")
cat("Variables seleccionadas por LASSO:\n")
print(selected_vars_lasso)
# Crear el conjunto de datos con las variables seleccionadas
data_final <- data_model_vif[, c("peso", selected_vars_lasso)]

# Paso 7: Ajustar el Modelo Final con Variables Seleccionadas

# Crear fórmula con las variables seleccionadas
formula_final <- as.formula(paste("peso ~", paste(selected_vars_lasso, collapse = " + ")))

# Ajustar el modelo
final_model <- lm(formula_final, data = data_final)

# Resumen del modelo
summary(final_model)


saveRDS(data_final, 
        file = "H:/Mi unidad/Tesis/Datos con R/SIAE 2010-2020/data_final.rds")

# # write.csv(data_final, "H:/Mi unidad/Tesis/Datos con R/SIAE 2010-2020/data_final.csv",
# row.names = FALSE)
