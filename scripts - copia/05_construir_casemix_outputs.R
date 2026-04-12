# ============================================================
# 05_construir_casemix_outputs.R
# Construye peso_grd (proxy de case-mix) para todo el panel
# 2010-2023 mediante dos modelos validados temporalmente
# (train 2010-2013, test 2014-2015) y los aplica a los
# años 2016-2023 sin datos GRD reales.
#
# Input:  df_final  (DF_FINAL_RDATA_PATH)
#         pesos     (PESOS_TXT_PATH  →  data_intermediate/pesos.txt)
# Output: df_final enriquecido (sobreescribe DF_FINAL_RDATA_PATH)
#         + artefactos en data_intermediate/
#
# Variables nuevas añadidas a df_final:
#   peso_grd_real, peso_grd_modeloA, peso_grd_modeloB,
#   peso_grd_final, fuente_peso,
#   altQ_bruto, altM_bruto, altTotal_bruto, ShareQ,
#   altQ_pond, altM_pond, altTotal_pond,
#   ln_altQ_pond, ln_altM_pond, ln_altTotal_pond
# ============================================================

config_path <- file.path(
  dirname(normalizePath(sys.frame(1)$ofile)),
  "00_config.R"
)
source(config_path)

# ── Verificar paquetes ────────────────────────────────────────
required_pkgs <- c("dplyr", "glmnet", "randomForest")
missing_pkgs  <- required_pkgs[
  !vapply(required_pkgs, requireNamespace, logical(1), quietly = TRUE)
]
if (length(missing_pkgs) > 0) {
  stop(
    "Faltan paquetes: ", paste(missing_pkgs, collapse = ", "),
    "\nInstálalos con install.packages().",
    call. = FALSE
  )
}

library(dplyr)
library(glmnet)
library(randomForest)

# ── Constantes ───────────────────────────────────────────────
PROTECTED_VARS  <- c(paste0("u", 1:104), "u900", "NCODI", "anyo")
ANYO_TRAIN_MAX  <- 2013
ANYO_TEST_MIN   <- 2014
ANYO_TEST_MAX   <- 2015
RANGO_PESO      <- c(0.40, 2.50)
FALLBACK_AÑO    <- 2015   # medianas de este año para 2016-2023

# ── Funciones auxiliares ─────────────────────────────────────
pct_na_col <- function(x) {
  if (is.character(x)) mean(is.na(x) | trimws(x) == "")
  else                  mean(is.na(x))
}

compute_metrics <- function(actual, predicted) {
  mask <- !is.na(actual) & !is.na(predicted)
  if (sum(mask) < 5) {
    return(list(R2 = NA_real_, RMSE = NA_real_,
                MAE = NA_real_, MAPE = NA_real_, n_test = sum(mask)))
  }
  a <- actual[mask]; p <- predicted[mask]
  ss_res <- sum((a - p)^2)
  ss_tot <- sum((a - mean(a))^2)
  list(
    R2     = 1 - ss_res / ss_tot,
    RMSE   = sqrt(mean((a - p)^2)),
    MAE    = mean(abs(a - p)),
    MAPE   = mean(abs((a - p) / a), na.rm = TRUE) * 100,
    n_test = sum(mask)
  )
}

safe_log <- function(x) ifelse(!is.na(x) & x > 0, log(x), NA_real_)

get_num_col <- function(df, colname) {
  if (colname %in% names(df))
    suppressWarnings(as.numeric(df[[colname]]))
  else
    rep(NA_real_, nrow(df))
}

# ── Cargar datos ─────────────────────────────────────────────
if (!file.exists(DF_FINAL_RDATA_PATH))
  stop("No se encontró df_final en: ", DF_FINAL_RDATA_PATH,
       "\nEjecuta el script 3 primero.", call. = FALSE)

load(DF_FINAL_RDATA_PATH)
message("df_final cargado: ", nrow(df_final), " filas x ", ncol(df_final), " columnas.")

if (!file.exists(PESOS_TXT_PATH))
  stop("No se encontró pesos en: ", PESOS_TXT_PATH, call. = FALSE)

pesos <- read.table(PESOS_TXT_PATH, sep = ";", dec = ",",
                    header = TRUE, stringsAsFactors = FALSE)
pesos$NCODI <- trimws(as.character(pesos$NCODI))
message("Pesos GRD cargados: ", nrow(pesos), " filas | años ",
        min(pesos$anyo), "-", max(pesos$anyo),
        " | ", n_distinct(pesos$NCODI), " NCODI únicos.")

# ============================================================
# BLOQUE 0 — VERIFICACIÓN DE COBERTURA DE VARIABLES 2016-2023
# ============================================================
message("\n=== BLOQUE 0: Verificación de cobertura de variables ===")

CANDIDATAS <- c(
  "totalMir_total", "altExitus_mIntensiva", "estancias_cirugia",
  "radiodiagnostico_total", "altFinal_total", "camas_funcionamiento",
  "estancias_total", "altFinal_cirugia", "altFinal_mIntensiva",
  "altFinal_uci", "subMedicas_cTotal", "cardiologia_cTotal",
  "neurologia_total", "anestesia_cTotal"
)

rows_early <- df_final$anyo %in% 2010:2015
rows_late  <- df_final$anyo %in% 2016:2023

cobertura_list <- lapply(CANDIDATAS, function(v) {
  if (!(v %in% names(df_final))) {
    return(data.frame(
      variable      = v,
      pct_na_early  = NA_real_,
      pct_na_late   = NA_real_,
      n_early       = sum(rows_early),
      n_late        = sum(rows_late),
      clasificacion = "ausente",
      stringsAsFactors = FALSE
    ))
  }
  pna_e <- pct_na_col(df_final[[v]][rows_early]) * 100
  pna_l <- pct_na_col(df_final[[v]][rows_late])  * 100
  clas <- dplyr::case_when(
    pna_e < 5  & pna_l < 5   ~ "ok",
    pna_e < 5  & pna_l > 20  ~ "degradada",
    pna_e > 20               ~ "alta_na",
    TRUE                     ~ "revisar"
  )
  data.frame(
    variable      = v,
    pct_na_early  = round(pna_e, 2),
    pct_na_late   = round(pna_l, 2),
    n_early       = sum(rows_early),
    n_late        = sum(rows_late),
    clasificacion = clas,
    stringsAsFactors = FALSE
  )
})

cobertura_df <- dplyr::bind_rows(cobertura_list)

message("Clasificación de variables candidatas:")
print(cobertura_df[, c("variable", "pct_na_early", "pct_na_late", "clasificacion")])

write.csv(cobertura_df,
          file.path(INT_DIR, "casemix_var_cobertura.csv"),
          row.names = FALSE, na = "")
message("Guardado en: casemix_var_cobertura.csv")

vars_ok <- cobertura_df$variable[cobertura_df$clasificacion == "ok"]
message("Variables 'ok' para el modelo (", length(vars_ok), "):")
message("  ", paste(vars_ok, collapse = ", "))

if (length(vars_ok) == 0)
  stop("Ninguna variable candidata tiene cobertura suficiente en ambos períodos.",
       call. = FALSE)

# ============================================================
# BLOQUE 1 — PREPARACIÓN DE DATOS
# ============================================================
message("\n=== BLOQUE 1: Preparación de datos ===")

df <- dplyr::left_join(
  df_final,
  pesos[, c("NCODI", "anyo", "peso")],
  by = c("NCODI", "anyo")
)

datos_con_peso <- df %>% dplyr::filter(!is.na(peso))
datos_sin_peso <- df %>% dplyr::filter(is.na(peso))

message("Obs. con peso real: ", nrow(datos_con_peso),
        " | Sin peso (a imputar): ", nrow(datos_sin_peso))

# Conversión a numérico de vars_ok
for (v in vars_ok) {
  if (v %in% names(datos_con_peso))
    datos_con_peso[[v]] <- suppressWarnings(as.numeric(datos_con_peso[[v]]))
  if (v %in% names(datos_sin_peso))
    datos_sin_peso[[v]] <- suppressWarnings(as.numeric(datos_sin_peso[[v]]))
}

# ── Medianas por año calculadas sobre datos_con_peso ──────────
# Para 2016-2023 (fuera del rango de entrenamiento) se usa la
# mediana del año FALLBACK_AÑO (= 2015).
all_peso_years <- sort(unique(datos_con_peso$anyo))
fallback_chr   <- as.character(FALLBACK_AÑO)

median_table <- lapply(vars_ok, function(v) {
  med_vec <- vapply(all_peso_years, function(yr) {
    vals <- datos_con_peso[[v]][datos_con_peso$anyo == yr]
    median(vals, na.rm = TRUE)
  }, numeric(1))
  stats::setNames(med_vec, as.character(all_peso_years))
})
names(median_table) <- vars_ok

# ── Función de imputación ─────────────────────────────────────
impute_df <- function(df_in, vars, med_tbl, fallback_chr) {
  for (v in vars) {
    if (!(v %in% names(df_in))) next
    med_vec  <- med_tbl[[v]]
    yr_chr   <- as.character(df_in$anyo)
    # Índice por año; NA para años sin mediana (2016-2023)
    med_row  <- med_vec[yr_chr]
    # Rellenar años sin mediana con la del año fallback
    med_row[is.na(med_row)] <- med_vec[fallback_chr]
    na_mask  <- is.na(df_in[[v]])
    df_in[[v]][na_mask] <- as.numeric(med_row[na_mask])
  }
  df_in
}

datos_con_peso_imp <- impute_df(datos_con_peso, vars_ok, median_table, fallback_chr)
datos_sin_peso_imp <- impute_df(datos_sin_peso, vars_ok, median_table, fallback_chr)
message("Imputación por medianas por año completada.")

# ============================================================
# BLOQUE 2 — DOS MODELOS ALTERNATIVOS (validación temporal)
# ============================================================
message("\n=== BLOQUE 2: Modelos A y B (train 2010-",
        ANYO_TRAIN_MAX, " / test ", ANYO_TEST_MIN, "-", ANYO_TEST_MAX, ") ===")

train_df <- datos_con_peso_imp %>% dplyr::filter(anyo <= ANYO_TRAIN_MAX)
test_df  <- datos_con_peso_imp %>% dplyr::filter(
  anyo >= ANYO_TEST_MIN & anyo <= ANYO_TEST_MAX
)

message("Train: ", nrow(train_df), " obs. | Test: ", nrow(test_df), " obs.")

if (nrow(train_df) < 30)
  stop("Muy pocas observaciones de entrenamiento: ", nrow(train_df), call. = FALSE)

# ── Correlaciones en training para elegir top-3 cuadráticos ──
cor_train <- vapply(vars_ok, function(v) {
  x <- train_df[[v]]; y <- train_df$peso
  mask <- !is.na(x) & !is.na(y)
  if (sum(mask) < 10) return(NA_real_)
  tryCatch(cor(x[mask], y[mask]), error = function(e) NA_real_)
}, numeric(1))

cor_train_abs <- sort(abs(cor_train), decreasing = TRUE, na.last = TRUE)
top3_vars     <- names(cor_train_abs)[seq_len(min(3, sum(!is.na(cor_train_abs))))]

message("Correlaciones |r| con peso en training:")
print(round(cor_train_abs[!is.na(cor_train_abs)], 4))
message("Top-3 para términos cuadráticos: ", paste(top3_vars, collapse = ", "))

# ── MODELO A: Regresión lineal ────────────────────────────────
message("\n-- Modelo A: Regresión lineal (vars_ok + cuadráticos top-3) --")

quad_terms  <- paste0("I(", top3_vars, "^2)")
formula_A   <- as.formula(
  paste("peso ~", paste(c(vars_ok, quad_terms), collapse = " + "))
)

modelo_A    <- lm(formula_A, data = train_df)
pred_A_test <- predict(modelo_A, newdata = test_df)
metrics_A   <- compute_metrics(test_df$peso, pred_A_test)

message(sprintf("  Modelo A — Test: R²=%.4f  RMSE=%.4f  MAE=%.4f  MAPE=%.2f%%  n=%d",
                metrics_A$R2, metrics_A$RMSE, metrics_A$MAE,
                metrics_A$MAPE, metrics_A$n_test))

# Guardar artefactos modelo A
writeLines(
  c(
    "MODELO A — Regresión Lineal",
    "Train: 2010-2013  |  Test: 2014-2015",
    "============================================================",
    sprintf("R²   : %.4f", metrics_A$R2),
    sprintf("RMSE : %.4f", metrics_A$RMSE),
    sprintf("MAE  : %.4f", metrics_A$MAE),
    sprintf("MAPE : %.2f%%", metrics_A$MAPE),
    sprintf("n_test: %d",   metrics_A$n_test),
    "",
    "Variables lineales:", paste(" ", vars_ok),
    "Términos cuadráticos:", paste(" ", quad_terms)
  ),
  file.path(INT_DIR, "casemix_modeloA_metricas.txt")
)

coef_A <- as.data.frame(summary(modelo_A)$coefficients)
coef_A$variable <- rownames(coef_A)
write.csv(coef_A,
          file.path(INT_DIR, "casemix_modeloA_coeficientes.csv"),
          row.names = FALSE, na = "")

saveRDS(modelo_A, file.path(INT_DIR, "casemix_modelo_A.rds"))
message("  Modelo A guardado.")

# ── MODELO B: LASSO + Random Forest ──────────────────────────
message("\n-- Modelo B: LASSO (lambda.1se) + Random Forest --")

X_train <- as.matrix(train_df[, vars_ok, drop = FALSE])
y_train <- train_df$peso

# Estandarizar (solo para LASSO; RF no lo necesita)
center_v <- colMeans(X_train, na.rm = TRUE)
scale_v  <- apply(X_train, 2, sd, na.rm = TRUE)
scale_v[scale_v == 0] <- 1

X_train_std <- scale(X_train, center = center_v, scale = scale_v)

set.seed(123)
cv_lasso   <- glmnet::cv.glmnet(X_train_std, y_train, alpha = 1, nfolds = 5)
lambda_sel <- cv_lasso$lambda.1se

coefs_lasso <- as.matrix(coef(cv_lasso, s = lambda_sel))
vars_lasso  <- setdiff(rownames(coefs_lasso)[coefs_lasso[, 1] != 0], "(Intercept)")

message("  LASSO (lambda.1se=", round(lambda_sel, 6),
        "): ", length(vars_lasso), " variables seleccionadas")

if (length(vars_lasso) == 0) {
  warning("LASSO no seleccionó variables. Se usan todas las vars_ok.", call. = FALSE)
  vars_lasso <- vars_ok
}
message("  ", paste(vars_lasso, collapse = ", "))

# RF con las variables seleccionadas por LASSO (valores sin estandarizar)
X_train_rf <- train_df[, vars_lasso, drop = FALSE]
mtry_opt   <- max(1L, floor(length(vars_lasso) / 3L))

# CACHÉ: si el modelo ya está guardado, cargarlo en lugar de re-entrenar.
# Esto ahorra ~2-3 min en re-ejecuciones. Para forzar re-entrenamiento,
# borrar casemix_modelo_B_rf.rds de data_intermediate/.
rds_path_B <- file.path(INT_DIR, "casemix_modelo_B_rf.rds")

if (file.exists(rds_path_B)) {
  message("  Cargando modelo RF desde caché (no re-entrenando).")
  modelo_B_rf <- readRDS(rds_path_B)
  # Recuperar vars_lasso desde el modelo guardado (robustez ante cambio de sesión)
  vars_lasso_cached <- rownames(randomForest::importance(modelo_B_rf))
  if (!setequal(vars_lasso, vars_lasso_cached)) {
    message("  AVISO: vars_lasso del caché difiere del LASSO actual. Re-entrenando.")
    modelo_B_rf <- NULL
  }
}

if (!exists("modelo_B_rf") || is.null(modelo_B_rf)) {
  message("  Entrenando modelo RF por primera vez (o caché inválida)...")
  set.seed(123)
  modelo_B_rf <- randomForest::randomForest(
    x          = X_train_rf,
    y          = y_train,
    mtry       = mtry_opt,
    ntree      = 500,
    importance = TRUE
  )
  saveRDS(modelo_B_rf, rds_path_B)
  message("  Modelo B guardado en caché.")
}

message(sprintf("  RF: mtry=%d  R²(OOB)=%.3f  RMSE(OOB)=%.4f",
                mtry_opt,
                1 - modelo_B_rf$mse[500] / var(y_train),
                sqrt(modelo_B_rf$mse[500])))

# Métricas modelo B en test
X_test_rf   <- test_df[, vars_lasso, drop = FALSE]
pred_B_test <- predict(modelo_B_rf, X_test_rf)
metrics_B   <- compute_metrics(test_df$peso, pred_B_test)

message(sprintf("  Modelo B — Test: R²=%.4f  RMSE=%.4f  MAE=%.4f  MAPE=%.2f%%  n=%d",
                metrics_B$R2, metrics_B$RMSE, metrics_B$MAE,
                metrics_B$MAPE, metrics_B$n_test))

# Guardar artefactos modelo B
writeLines(
  c(
    "MODELO B — LASSO + Random Forest",
    "Train: 2010-2013  |  Test: 2014-2015",
    "============================================================",
    sprintf("R²   : %.4f", metrics_B$R2),
    sprintf("RMSE : %.4f", metrics_B$RMSE),
    sprintf("MAE  : %.4f", metrics_B$MAE),
    sprintf("MAPE : %.2f%%", metrics_B$MAPE),
    sprintf("n_test: %d",   metrics_B$n_test),
    "",
    sprintf("LASSO lambda.1se = %.6f", lambda_sel),
    sprintf("Variables LASSO: %d  |  RF mtry: %d  ntree: 500",
            length(vars_lasso), mtry_opt),
    "",
    "Variables seleccionadas:", paste(" ", vars_lasso)
  ),
  file.path(INT_DIR, "casemix_modeloB_metricas.txt")
)

imp_mat  <- randomForest::importance(modelo_B_rf)
imp_col  <- if ("%IncMSE" %in% colnames(imp_mat)) "%IncMSE" else colnames(imp_mat)[1]
imp_vals <- imp_mat[vars_lasso, imp_col]

vars_lasso_df <- data.frame(
  variable      = vars_lasso,
  coef_lasso    = coefs_lasso[vars_lasso, 1],
  importance_rf = imp_vals,
  stringsAsFactors = FALSE
)
write.csv(vars_lasso_df,
          file.path(INT_DIR, "casemix_modeloB_variables_lasso.csv"),
          row.names = FALSE, na = "")

message("  Modelo B listo.")

# ── Elegir mejor modelo para peso_grd_final ───────────────────
rmse_A       <- metrics_A$RMSE
rmse_B       <- metrics_B$RMSE
mejor_modelo <- if (!is.na(rmse_B) && !is.na(rmse_A) && rmse_B < rmse_A) "B" else "A"

message(sprintf(
  "\nModelo elegido para peso_grd_final: Modelo %s  (RMSE_A=%.4f, RMSE_B=%.4f)",
  mejor_modelo, rmse_A, rmse_B
))

# ============================================================
# BLOQUE 3 — PREDICCIÓN Y CONSTRUCCIÓN DE PESO FINAL
# ============================================================
message("\n=== BLOQUE 3: Predicción para todo el panel ===")

# Unir entrenamiento + predicción (mismo orden que df_final)
df_all_imp <- dplyr::bind_rows(datos_con_peso_imp, datos_sin_peso_imp) %>%
  dplyr::arrange(NCODI, anyo)

# Asegurar que vars_ok son numéricos en df_all_imp
for (v in vars_ok) {
  if (v %in% names(df_all_imp))
    df_all_imp[[v]] <- suppressWarnings(as.numeric(df_all_imp[[v]]))
}

# ── Predicción Modelo A ───────────────────────────────────────
pred_A_all <- tryCatch(
  predict(modelo_A, newdata = df_all_imp),
  error = function(e) {
    warning("Modelo A error en predict: ", conditionMessage(e), call. = FALSE)
    rep(NA_real_, nrow(df_all_imp))
  }
)

# ── Predicción Modelo B ───────────────────────────────────────
# Añadir columnas faltantes (imputadas a 0) si LASSO seleccionó
# alguna variable ausente en df_all_imp
miss_lasso <- setdiff(vars_lasso, names(df_all_imp))
if (length(miss_lasso) > 0) {
  for (m in miss_lasso) df_all_imp[[m]] <- 0
  warning("Variables LASSO ausentes en datos (imputadas a 0): ",
          paste(miss_lasso, collapse = ", "), call. = FALSE)
}

X_all_rf   <- df_all_imp[, vars_lasso, drop = FALSE]
pred_B_all <- tryCatch(
  predict(modelo_B_rf, X_all_rf),
  error = function(e) {
    warning("Modelo B error en predict: ", conditionMessage(e), call. = FALSE)
    rep(NA_real_, nrow(df_all_imp))
  }
)

# ── Truncar al rango permitido ────────────────────────────────
pred_A_all <- pmax(RANGO_PESO[1], pmin(RANGO_PESO[2], pred_A_all))
pred_B_all <- pmax(RANGO_PESO[1], pmin(RANGO_PESO[2], pred_B_all))

# ── Construir columnas de peso ────────────────────────────────
peso_grd_real    <- df_all_imp$peso   # NA donde no hay GRD real
pred_mejor_all   <- if (mejor_modelo == "A") pred_A_all else pred_B_all

peso_grd_final   <- dplyr::if_else(!is.na(peso_grd_real), peso_grd_real, pred_mejor_all)

fuente_peso_chr  <- dplyr::case_when(
  !is.na(peso_grd_real) & mejor_modelo == "A" ~ "real",
  !is.na(peso_grd_real) & mejor_modelo == "B" ~ "real",
  mejor_modelo == "A"                         ~ "modeloA",
  TRUE                                        ~ "modeloB"
)

# Añadir columnas de peso a df_all_imp
df_all_imp$peso_grd_real    <- peso_grd_real
df_all_imp$peso_grd_modeloA <- pred_A_all
df_all_imp$peso_grd_modeloB <- pred_B_all
df_all_imp$peso_grd_final   <- peso_grd_final
df_all_imp$fuente_peso      <- factor(
  fuente_peso_chr, levels = c("real", "modeloA", "modeloB")
)

message("Fuente de peso_grd_final:")
print(table(df_all_imp$fuente_peso, useNA = "ifany"))

message(sprintf("  peso_grd_final: min=%.3f  median=%.3f  mean=%.3f  max=%.3f",
                min(peso_grd_final, na.rm = TRUE),
                median(peso_grd_final, na.rm = TRUE),
                mean(peso_grd_final, na.rm = TRUE),
                max(peso_grd_final, na.rm = TRUE)))

# ============================================================
# BLOQUE 4 — OUTPUTS PONDERADOS
# ============================================================
message("\n=== BLOQUE 4: Construcción de outputs ponderados ===")

altFinal_cirugia_v <- get_num_col(df_all_imp, "altFinal_cirugia")
altFinal_trauma_v  <- get_num_col(df_all_imp, "altFinal_trauma")
altFinal_med_v     <- get_num_col(df_all_imp, "altFinal_med")
altFinal_total_v   <- get_num_col(df_all_imp, "altFinal_total")

# Brutos (sin ponderar)
altQ_bruto     <- altFinal_cirugia_v + altFinal_trauma_v
altM_bruto     <- altFinal_med_v
altTotal_bruto <- altFinal_total_v

ShareQ <- dplyr::if_else(
  !is.na(altTotal_bruto) & altTotal_bruto > 0,
  altQ_bruto / altTotal_bruto,
  NA_real_
)

# Ponderados por peso_grd_final
altQ_pond     <- altQ_bruto     * peso_grd_final
altM_pond     <- altM_bruto     * peso_grd_final
altTotal_pond <- altTotal_bruto * peso_grd_final

# Logaritmos (NA si valor <= 0)
ln_altQ_pond     <- safe_log(altQ_pond)
ln_altM_pond     <- safe_log(altM_pond)
ln_altTotal_pond <- safe_log(altTotal_pond)

# Añadir a df_all_imp
df_all_imp$altQ_bruto       <- altQ_bruto
df_all_imp$altM_bruto       <- altM_bruto
df_all_imp$altTotal_bruto   <- altTotal_bruto
df_all_imp$ShareQ           <- ShareQ
df_all_imp$altQ_pond        <- altQ_pond
df_all_imp$altM_pond        <- altM_pond
df_all_imp$altTotal_pond    <- altTotal_pond
df_all_imp$ln_altQ_pond     <- ln_altQ_pond
df_all_imp$ln_altM_pond     <- ln_altM_pond
df_all_imp$ln_altTotal_pond <- ln_altTotal_pond

message("  altTotal_pond: n_nonNA=", sum(!is.na(altTotal_pond)),
        "  rango=[", round(min(altTotal_pond, na.rm = TRUE), 0),
        ", ", round(max(altTotal_pond, na.rm = TRUE), 0), "]")
message("  ln_altTotal_pond: n_nonNA=", sum(!is.na(ln_altTotal_pond)),
        "  rango=[", round(min(ln_altTotal_pond, na.rm = TRUE), 3),
        ", ", round(max(ln_altTotal_pond, na.rm = TRUE), 3), "]")

# ============================================================
# BLOQUE 5 — DIAGNÓSTICO FINAL Y GUARDADO
# ============================================================
message("\n=== BLOQUE 5: Diagnóstico final y guardado ===")

diag_anyo <- df_all_imp %>%
  dplyr::group_by(anyo) %>%
  dplyr::summarise(
    n_real              = sum(fuente_peso == "real",    na.rm = TRUE),
    n_modeloA           = sum(fuente_peso == "modeloA", na.rm = TRUE),
    n_modeloB           = sum(fuente_peso == "modeloB", na.rm = TRUE),
    media_peso          = round(mean(peso_grd_final, na.rm = TRUE), 4),
    sd_peso             = round(sd(peso_grd_final,   na.rm = TRUE), 4),
    media_altTotal_pond = round(mean(altTotal_pond,   na.rm = TRUE), 1),
    pct_NA_altQ_pond    = round(100 * mean(is.na(altQ_pond)), 2),
    .groups = "drop"
  )

message("Diagnóstico por año:")
print(as.data.frame(diag_anyo))

write.csv(diag_anyo,
          file.path(INT_DIR, "casemix_diagnostico_final.csv"),
          row.names = FALSE, na = "")
message("Guardado en: casemix_diagnostico_final.csv")

# ── Integrar variables nuevas en df_final y sobreescribir ────
message("\nIntegrando variables nuevas en df_final...")

COLS_NUEVAS <- c(
  "peso_grd_real", "peso_grd_modeloA", "peso_grd_modeloB",
  "peso_grd_final", "fuente_peso",
  "altQ_bruto", "altM_bruto", "altTotal_bruto", "ShareQ",
  "altQ_pond", "altM_pond", "altTotal_pond",
  "ln_altQ_pond", "ln_altM_pond", "ln_altTotal_pond"
)

# Construir tabla de join: (NCODI, anyo) + nuevas variables
df_vars_nuevas <- df_all_imp[, c("NCODI", "anyo", COLS_NUEVAS)]
df_vars_nuevas$fuente_peso <- as.character(df_vars_nuevas$fuente_peso)

# Eliminar columnas si ya existían en df_final (sobreescribir)
cols_existentes <- intersect(COLS_NUEVAS, names(df_final))
if (length(cols_existentes) > 0) {
  message("  Sobreescribiendo columnas ya existentes: ",
          paste(cols_existentes, collapse = ", "))
  df_final <- df_final %>% dplyr::select(-dplyr::all_of(cols_existentes))
}

df_final <- dplyr::left_join(df_final, df_vars_nuevas, by = c("NCODI", "anyo"))

if (nrow(df_final) != nrow(df_all_imp)) {
  warning("Número de filas distinto tras join: df_final=", nrow(df_final),
          " | df_all_imp=", nrow(df_all_imp), call. = FALSE)
}

# Verificar que PROTECTED_VARS siguen intactas
prot_check <- intersect(PROTECTED_VARS, names(df_final))
message("  Variables protegidas presentes: ", length(prot_check), " / ",
        length(PROTECTED_VARS))

save(df_final, file = DF_FINAL_RDATA_PATH)

message("\n=== Script 9 completado ===")
message("df_final guardado: ", nrow(df_final), " filas x ",
        ncol(df_final), " columnas")
message("Nuevas variables añadidas (", length(COLS_NUEVAS), "):")
message("  ", paste(COLS_NUEVAS, collapse = ", "))
message("\nResumen peso_grd_final:")
print(summary(df_final$peso_grd_final))
