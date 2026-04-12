# ============================================================
# 9_construccion de variable mix.R
# Construye la variable proxy de case-mix (mix) mediante
# un modelo Random Forest entrenado en hospitales con peso
# GRD real, y lo imputa al resto.
#
# Input:  df_final  (DF_FINAL_RDATA_PATH)
#         pesos     (PESOS_TXT_PATH)
# Output: df_completo (DF_COMPLETO_RDATA_PATH)
#         + DF_FINAL_CON_MIX_TXT_PATH
# ============================================================

config_path <- if (file.exists("scripts/00_config.R")) "scripts/00_config.R" else "00_config.R"
source(config_path)

# ------------------------------------------------------------
# Verificar paquetes
# ------------------------------------------------------------
required_pkgs <- c("dplyr", "caret", "glmnet", "randomForest", "ggplot2")
missing_pkgs  <- required_pkgs[!vapply(required_pkgs, requireNamespace,
                                       logical(1), quietly = TRUE)]
if (length(missing_pkgs) > 0) {
  stop(
    "Faltan paquetes: ", paste(missing_pkgs, collapse = ", "),
    "\nInstálalos con install.packages().",
    call. = FALSE
  )
}

library(dplyr)
library(caret)
library(glmnet)
library(randomForest)

# ------------------------------------------------------------
# 1. Cargar df_final
# ------------------------------------------------------------
if (!file.exists(DF_FINAL_RDATA_PATH)) {
  stop("No se encontró df_final en: ", DF_FINAL_RDATA_PATH,
       "\nEjecuta el script 3 antes.", call. = FALSE)
}

load(DF_FINAL_RDATA_PATH)

if (!exists("df_final")) {
  stop("El objeto df_final no existe en ", DF_FINAL_RDATA_PATH, call. = FALSE)
}

message("df_final cargado: ", nrow(df_final), " filas × ", ncol(df_final), " columnas.")

# ------------------------------------------------------------
# 2. Cargar tabla de pesos GRD y unir
# ------------------------------------------------------------
if (!file.exists(PESOS_TXT_PATH)) {
  stop("No se encontró el archivo de pesos en: ", PESOS_TXT_PATH, call. = FALSE)
}

pesos <- read.table(PESOS_TXT_PATH, sep = ";", dec = ",", header = TRUE,
                    stringsAsFactors = FALSE)
pesos$NCODI <- trimws(as.character(pesos$NCODI))

message("Pesos GRD cargados: ", nrow(pesos), " filas.")
message("  Hospitales con peso real: ", n_distinct(pesos$NCODI))

df <- left_join(df_final, pesos, by = c("NCODI", "anyo"))

n_con_peso <- sum(!is.na(df$peso))
n_sin_peso <- sum(is.na(df$peso))
message(sprintf("  Obs. con peso real: %d | Sin peso (a imputar): %d",
                n_con_peso, n_sin_peso))

# ------------------------------------------------------------
# 3. Preparar variables predictoras
# Variables protegidas: identificadores + categóricas + target
# ------------------------------------------------------------
PROTECTED_VARS <- c(paste0("u", 1:104), "u900", "NCODI", "anyo",
                    "ccaa_codigo", "ccaa", "nombre_hospital",
                    "cod_centro", "CODCNH", "ccaa_cnh", "provincia_cnh",
                    "finalidad_cnh", "dependencia_cnh",
                    "cod_finalidad_agrupada", "Finalidad_agrupada",
                    "cod_depend_agrupada", "Depend_agrupada",
                    "peso", "mix")

variables_excluir <- intersect(PROTECTED_VARS, names(df))

variables_numericas <- names(df)[!(names(df) %in% variables_excluir)]

# Convertir a numérico
df[variables_numericas] <- lapply(df[variables_numericas], function(x) {
  suppressWarnings(as.numeric(as.character(x)))
})

# Excluir variables con más de 50% de NA
pct_na <- vapply(df[variables_numericas], function(x) mean(is.na(x)), numeric(1))
variables_numericas <- variables_numericas[pct_na <= 0.50]

message("Variables predictoras disponibles: ", length(variables_numericas))

# Imputar NA con la mediana (solo en predictores, no en target)
for (var in variables_numericas) {
  med <- median(df[[var]], na.rm = TRUE)
  if (!is.na(med)) df[[var]][is.na(df[[var]])] <- med
}

# ------------------------------------------------------------
# 4. Separar entrenamiento / predicción
# ------------------------------------------------------------
df_entrenamiento <- df %>% filter(!is.na(peso))
df_prediccion    <- df %>% filter(is.na(peso))

message(sprintf("Conjunto entrenamiento: %d obs. | Predicción: %d obs.",
                nrow(df_entrenamiento), nrow(df_prediccion)))

if (nrow(df_entrenamiento) < 30) {
  stop("Muy pocas observaciones con peso real para entrenar el modelo: ",
       nrow(df_entrenamiento), call. = FALSE)
}

X_entrenamiento <- as.matrix(df_entrenamiento[, variables_numericas, drop = FALSE])
y_entrenamiento <- df_entrenamiento$peso

# ------------------------------------------------------------
# 5. Selección de variables con LASSO
# ------------------------------------------------------------
message("\nEjecutando LASSO para selección de variables...")

# Eliminar predictores con varianza cero
nzv               <- caret::nearZeroVar(X_entrenamiento, saveMetrics = TRUE)
vars_nzv          <- rownames(nzv)[nzv$zeroVar]
X_entrenamiento   <- X_entrenamiento[, !(colnames(X_entrenamiento) %in% vars_nzv), drop = FALSE]

message("Variables tras eliminar varianza cero: ", ncol(X_entrenamiento))

# Estandarizar
preproc_lasso       <- caret::preProcess(X_entrenamiento, method = c("center", "scale"))
X_ent_std           <- predict(preproc_lasso, X_entrenamiento)

set.seed(123)
cv_lasso      <- glmnet::cv.glmnet(X_ent_std, y_entrenamiento, alpha = 1, nfolds = 10)
lambda_opt    <- cv_lasso$lambda.min

coefs         <- coef(cv_lasso, s = lambda_opt)
vars_lasso    <- rownames(coefs)[which(coefs != 0)]
vars_lasso    <- setdiff(vars_lasso, "(Intercept)")

message("Variables seleccionadas por LASSO: ", length(vars_lasso))

if (length(vars_lasso) == 0) {
  warning("LASSO no seleccionó ninguna variable. Usando todas las disponibles.", call. = FALSE)
  vars_lasso <- colnames(X_ent_std)
}

X_ent_sel <- X_ent_std[, vars_lasso, drop = FALSE]

# Estandarizar de nuevo sobre el subconjunto seleccionado
preproc_rf    <- caret::preProcess(X_ent_sel, method = c("center", "scale"))
X_ent_sel     <- predict(preproc_rf, X_ent_sel)

# ------------------------------------------------------------
# 6. Preparar conjunto de predicción
# ------------------------------------------------------------
X_pred_raw <- as.matrix(df_prediccion[, colnames(X_entrenamiento), drop = FALSE])

# Aplicar preprocesamiento del LASSO (center/scale del paso anterior)
X_pred_std <- predict(preproc_lasso, X_pred_raw)

# Seleccionar variables LASSO
faltantes <- setdiff(vars_lasso, colnames(X_pred_std))
if (length(faltantes) > 0) {
  for (v in faltantes) X_pred_std <- cbind(X_pred_std, 0)
  colnames(X_pred_std)[
    (ncol(X_pred_std) - length(faltantes) + 1):ncol(X_pred_std)
  ] <- faltantes
}
X_pred_sel <- X_pred_std[, vars_lasso, drop = FALSE]

# Aplicar preprocesamiento RF
X_pred_sel <- predict(preproc_rf, X_pred_sel)

# ------------------------------------------------------------
# 7. Entrenar Random Forest
# ------------------------------------------------------------
message("\nEntrenando Random Forest...")

# mtry: heurístico p/3 para regresión, acotado al número de variables
mtry_opt <- max(1L, min(floor(ncol(X_ent_sel) / 3), ncol(X_ent_sel)))

set.seed(123)
modelo_rf <- randomForest::randomForest(
  x          = X_ent_sel,
  y          = y_entrenamiento,
  mtry       = mtry_opt,
  ntree      = 500,
  importance = TRUE
)

message(sprintf("  R² (OOB): %.3f | RMSE (OOB): %.3f",
                1 - modelo_rf$mse[500] / var(y_entrenamiento),
                sqrt(modelo_rf$mse[500])))

# ------------------------------------------------------------
# 8. Predicciones y ensamblado
# ------------------------------------------------------------
df_entrenamiento$mix <- predict(modelo_rf, X_ent_sel)

if (nrow(df_prediccion) > 0) {
  df_prediccion$mix <- predict(modelo_rf, X_pred_sel)
} else {
  message("No hay observaciones sin peso: mix = peso para todos.")
}

df_completo <- dplyr::bind_rows(df_entrenamiento, df_prediccion) %>%
  arrange(NCODI, anyo)

# ------------------------------------------------------------
# 9. Evaluar el modelo en el conjunto de entrenamiento
# ------------------------------------------------------------
df_eval     <- df_completo %>% filter(!is.na(peso))
peso_real   <- df_eval$peso
peso_pred   <- df_eval$mix

MAE   <- mean(abs(peso_real - peso_pred))
RMSE  <- sqrt(mean((peso_real - peso_pred)^2))
MAPE  <- mean(abs((peso_real - peso_pred) / peso_real)) * 100
R2    <- caret::R2(peso_pred, peso_real)

message("\n--- Métricas del modelo (in-sample) ---")
message(sprintf("  MAE:  %.4f", MAE))
message(sprintf("  RMSE: %.4f", RMSE))
message(sprintf("  MAPE: %.2f%%", MAPE))
message(sprintf("  R²:   %.4f", R2))

# ------------------------------------------------------------
# 10. Guardar diagnósticos gráficos (en INT_DIR, no interactivos)
# ------------------------------------------------------------
if (requireNamespace("ggplot2", quietly = TRUE)) {
  library(ggplot2)

  df_eval <- df_eval %>%
    mutate(residuos = peso_real - peso_pred,
           media_vals = (peso_real + peso_pred) / 2)

  plots_dir <- file.path(INT_DIR, "mix_model_plots")
  dir.create(plots_dir, showWarnings = FALSE, recursive = TRUE)

  # Dispersión real vs predicho
  p1 <- ggplot(df_eval, aes(x = peso_real, y = peso_pred)) +
    geom_point(alpha = 0.5, color = "steelblue") +
    geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
    labs(title = "Peso real vs predicho (mix)",
         x = "Peso real (GRD)", y = "Mix predicho") +
    theme_minimal()
  ggsave(file.path(plots_dir, "dispersion_real_vs_predicho.png"), p1,
         width = 7, height = 5)

  # Residuos
  p2 <- ggplot(df_eval, aes(x = peso_pred, y = residuos)) +
    geom_point(alpha = 0.5, color = "darkgreen") +
    geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
    labs(title = "Residuos vs mix predicho", x = "Mix predicho", y = "Residuo") +
    theme_minimal()
  ggsave(file.path(plots_dir, "residuos.png"), p2, width = 7, height = 5)

  # Bland-Altman
  p3 <- ggplot(df_eval, aes(x = media_vals, y = residuos)) +
    geom_point(alpha = 0.5, color = "purple") +
    geom_hline(yintercept = mean(df_eval$residuos), color = "blue",
               linetype = "dashed") +
    labs(title = "Bland-Altman (mix)", x = "Media", y = "Diferencia") +
    theme_minimal()
  ggsave(file.path(plots_dir, "bland_altman.png"), p3, width = 7, height = 5)

  message("Gráficos guardados en: ", plots_dir)
}

# ------------------------------------------------------------
# 11. Guardar outputs
# ------------------------------------------------------------
save(df_completo, file = DF_COMPLETO_RDATA_PATH)
message("\ndf_completo guardado en: ", DF_COMPLETO_RDATA_PATH)
message("  Filas: ", nrow(df_completo), " | Columnas: ", ncol(df_completo))
message("  Obs. con mix: ", sum(!is.na(df_completo$mix)))

write.table(df_completo, DF_FINAL_CON_MIX_TXT_PATH,
            sep = ";", dec = ",", row.names = FALSE, na = "")
message("TXT con mix guardado en: ", DF_FINAL_CON_MIX_TXT_PATH)

message("\n=== Script 9 completado ===")
