# ============================================================
# _diagnostico_sfa_temp.R
# Diagnóstico de pesos GRD y cobertura de case-mix.
# No modifica df_final. Solo lectura y análisis.
# ============================================================

config_path <- if (file.exists("scripts/00_config.R")) "scripts/00_config.R" else "00_config.R"
source(config_path)
suppressMessages(library(dplyr))

# pesos.txt puede estar en INT_DIR o en LEGACY_BASE_DIR según la versión del pipeline
if (!file.exists(PESOS_TXT_PATH)) {
  alt_path <- file.path(INT_DIR, "pesos.txt")
  if (file.exists(alt_path)) {
    message("NOTA: pesos.txt encontrado en INT_DIR (no en LEGACY_BASE_DIR). Usando: ", alt_path)
    PESOS_TXT_PATH <- alt_path
  }
}

DIAG_OUT <- file.path(INT_DIR, "diagnostico_casemix_recursos.txt")
sink(DIAG_OUT, split = TRUE)   # split=TRUE: también imprime en consola

cat("============================================================\n")
cat("DIAGNÓSTICO CASE-MIX / PESOS GRD — SIAE 2010-2023\n")
cat("Generado:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")
cat("============================================================\n\n")

# ============================================================
# PASO 1 — pesos.txt
# ============================================================

cat("------------------------------------------------------------\n")
cat("PASO 1: Lectura de pesos.txt\n")
cat("------------------------------------------------------------\n")

if (!file.exists(PESOS_TXT_PATH)) {
  cat("ERROR: no se encontró PESOS_TXT_PATH:", PESOS_TXT_PATH, "\n")
  sink()
  stop("Abortando.")
}

pesos <- read.table(PESOS_TXT_PATH, sep = ";", dec = ",", header = TRUE,
                    stringsAsFactors = FALSE)
pesos$NCODI <- trimws(as.character(pesos$NCODI))

cat("Ruta:", PESOS_TXT_PATH, "\n")
cat("Filas:", nrow(pesos), "| Columnas:", ncol(pesos), "\n\n")

cat("-- Nombres de columnas:\n")
print(names(pesos))

cat("\n-- head(pesos):\n")
print(head(pesos, 10))

cat("\n-- str(pesos):\n")
str(pesos)

cat("\n-- Rango de años en pesos:\n")
if ("anyo" %in% names(pesos)) {
  cat("  min:", min(pesos$anyo, na.rm = TRUE),
      "| max:", max(pesos$anyo, na.rm = TRUE), "\n")
  print(table(pesos$anyo, useNA = "ifany"))
} else {
  cat("  Columna 'anyo' NO encontrada.\n")
}

cat("\n-- NCODI únicos:", n_distinct(pesos$NCODI), "\n")

if ("peso" %in% names(pesos)) {
  cat("\n-- Resumen estadístico de 'peso':\n")
  print(summary(pesos$peso))
  cat("  sd:", round(sd(pesos$peso, na.rm = TRUE), 4), "\n")
  cat("  % NA:", round(100 * mean(is.na(pesos$peso)), 2), "%\n")
} else {
  cat("\n  Columna 'peso' NO encontrada. Columnas disponibles:\n")
  print(names(pesos))
}

# ============================================================
# PASO 2 — Scripts legacy de mix
# ============================================================

cat("\n------------------------------------------------------------\n")
cat("PASO 2: Scripts legacy relacionados con mix/pesos/casemix/grd/lasso/imputa\n")
cat("------------------------------------------------------------\n")

script_dirs <- c(
  file.path(BASE_DIR, "scripts"),
  file.path(BASE_DIR, "scripts", "legacy")
)
pattern_mix <- "mix|pesos|casemix|grd|lasso|imputa"

found_scripts <- character(0)
for (d in script_dirs) {
  if (!dir.exists(d)) next
  fls <- list.files(d, pattern = "\\.R$", full.names = TRUE, ignore.case = TRUE)
  hits <- fls[grepl(pattern_mix, basename(fls), ignore.case = TRUE)]
  found_scripts <- c(found_scripts, hits)
}

cat("Archivos encontrados:", length(found_scripts), "\n")
for (f in found_scripts) cat("  -", f, "\n")

for (f in found_scripts) {
  cat("\n\n==============================\n")
  cat("CONTENIDO:", basename(f), "\n")
  cat("==============================\n")
  lns <- readLines(f, warn = FALSE)
  cat(paste(lns, collapse = "\n"), "\n")
}

# ============================================================
# PASO 3 — Cruce pesos x df_final
# ============================================================

cat("\n------------------------------------------------------------\n")
cat("PASO 3: Cruce pesos.txt con df_final\n")
cat("------------------------------------------------------------\n")

if (!file.exists(DF_FINAL_RDATA_PATH)) {
  cat("ERROR: no se encontró df_final en:", DF_FINAL_RDATA_PATH, "\n")
  sink(); stop("Abortando.")
}

load(DF_FINAL_RDATA_PATH)
cat("df_final cargado:", nrow(df_final), "filas x", ncol(df_final), "columnas\n")
cat("Rango anyo:", min(df_final$anyo, na.rm=TRUE), "-", max(df_final$anyo, na.rm=TRUE), "\n")
cat("Hospitales únicos (NCODI):", n_distinct(df_final$NCODI), "\n\n")

# Join
df <- dplyr::left_join(df_final, pesos[, intersect(c("NCODI","anyo","peso"), names(pesos))],
                       by = c("NCODI","anyo"))

# Tabla cobertura por año
cat("-- Cobertura de peso por año:\n")
cob <- df %>%
  group_by(anyo) %>%
  summarise(
    n_total     = n(),
    n_con_peso  = sum(!is.na(peso)),
    n_sin_peso  = sum(is.na(peso)),
    pct_cobertura = round(100 * n_con_peso / n_total, 1),
    .groups = "drop"
  ) %>%
  arrange(anyo)
print(as.data.frame(cob))

# Hospitales sin peso en 2010-2015: ¿son reales?
cat("\n-- Hospitales SIN peso en 2010-2015: ¿tienen datos en variables clave?\n")
check_vars <- c("altFinal_total", "camas_funcionamiento", "subMedicas_cTotal")
check_vars_exist <- intersect(check_vars, names(df))
cat("  Variables disponibles para el check:", paste(check_vars_exist, collapse=", "), "\n")

df_sin_peso_early <- df %>%
  filter(anyo <= 2015, is.na(peso))

cat("  Observaciones sin peso (2010-2015):", nrow(df_sin_peso_early), "\n\n")

if (nrow(df_sin_peso_early) > 0 && length(check_vars_exist) > 0) {
  for (v in check_vars_exist) {
    vals <- df_sin_peso_early[[v]]
    cat(sprintf("  %-30s n_nonNA=%d  %%NA=%.1f%%  mean=%.2f  median=%.2f\n",
                v,
                sum(!is.na(vals)),
                100 * mean(is.na(vals)),
                ifelse(all(is.na(vals)), NA, mean(vals, na.rm=TRUE)),
                ifelse(all(is.na(vals)), NA, median(vals, na.rm=TRUE))
    ))
  }

  # Perfil de NAs en variables clave
  cat("\n  Distribución de NAs simultáneos en variables clave:\n")
  n_na_all <- rowSums(is.na(df_sin_peso_early[, check_vars_exist, drop=FALSE]))
  print(table("n_vars_NA" = n_na_all))

  cat("\n  Hospitales sin peso Y sin todas las variables clave (registros vacíos?):",
      sum(n_na_all == length(check_vars_exist)), "\n")
  cat("  Hospitales sin peso PERO con altFinal_total conocida:",
      if ("altFinal_total" %in% check_vars_exist)
        sum(!is.na(df_sin_peso_early$altFinal_total))
      else NA, "\n")
}

# ============================================================
# PASO 4 — Correlaciones de peso con variables auxiliares
# ============================================================

cat("\n------------------------------------------------------------\n")
cat("PASO 4: Correlaciones de peso con variables auxiliares (2010-2015)\n")
cat("------------------------------------------------------------\n")

df_train <- df %>% filter(!is.na(peso), anyo <= 2015)
cat("Observaciones con peso conocido (2010-2015):", nrow(df_train), "\n\n")

# Construir variables derivadas
compute_ratio <- function(num, den) {
  ifelse(!is.na(num) & !is.na(den) & den > 0, num / den, NA_real_)
}

# Helper: buscar columna por nombre (tolerante a mayúsculas)
find_col <- function(df, candidates) {
  for (nm in candidates) {
    hit <- names(df)[tolower(names(df)) == tolower(nm)]
    if (length(hit) > 0) return(hit[1])
  }
  NULL
}

vars_aux_spec <- list(
  "propCirugia"       = list(num = find_col(df_train, c("altFinal_cirugia")),
                              den = find_col(df_train, c("altFinal_total"))),
  "propIntensiva"     = list(num = find_col(df_train, c("altFinal_mIntensiva","altFinal_med")),
                              den = find_col(df_train, c("altFinal_total"))),
  "propUCI"           = list(num = find_col(df_train, c("altFinal_uci","altFinal_UCI")),
                              den = find_col(df_train, c("altFinal_total"))),
  "estancia_media"    = list(num = find_col(df_train, c("estancias_total")),
                              den = find_col(df_train, c("altFinal_total"))),
  "propUrgencias"     = list(num = find_col(df_train, c("ingreUrge","ingreUrgencias","total_urgente",
                                                         "altFinal_urgente")),
                              den = find_col(df_train, c("altFinal_total","total_ingresosHops")))
)

# Variables directas (no ratio)
vars_directas <- c(
  find_col(df_train, c("camas_funcionamiento")),
  find_col(df_train, c("altFinal_total"))
)
vars_directas <- vars_directas[!is.null(vars_directas)]

# Mostrar qué columnas se encontraron
cat("-- Columnas encontradas para variables ratio:\n")
for (nm in names(vars_aux_spec)) {
  spec <- vars_aux_spec[[nm]]
  cat(sprintf("  %-20s  num=%-30s  den=%s\n",
              nm,
              ifelse(is.null(spec$num), "** NO ENCONTRADA **", spec$num),
              ifelse(is.null(spec$den), "** NO ENCONTRADA **", spec$den)))
}
cat("-- Variables directas:", paste(vars_directas, collapse=", "), "\n\n")

# Construir data.frame de correlaciones
cor_df <- data.frame(variable = character(), fuente = character(),
                     cor_con_peso = numeric(), n_obs = integer(),
                     stringsAsFactors = FALSE)

# Ratios
for (nm in names(vars_aux_spec)) {
  spec <- vars_aux_spec[[nm]]
  if (is.null(spec$num) || is.null(spec$den)) next
  v_ratio <- compute_ratio(df_train[[spec$num]], df_train[[spec$den]])
  mask    <- !is.na(v_ratio) & !is.na(df_train$peso)
  if (sum(mask) < 10) next
  r <- cor(v_ratio[mask], df_train$peso[mask])
  cor_df <- rbind(cor_df, data.frame(
    variable     = nm,
    fuente       = paste0(spec$num, " / ", spec$den),
    cor_con_peso = round(r, 4),
    n_obs        = sum(mask),
    stringsAsFactors = FALSE
  ))
}

# Directas
for (v in vars_directas) {
  vals <- suppressWarnings(as.numeric(df_train[[v]]))
  mask <- !is.na(vals) & !is.na(df_train$peso)
  if (sum(mask) < 10) next
  r <- cor(vals[mask], df_train$peso[mask])
  cor_df <- rbind(cor_df, data.frame(
    variable     = v,
    fuente       = v,
    cor_con_peso = round(r, 4),
    n_obs        = sum(mask),
    stringsAsFactors = FALSE
  ))
}

# Ordenar por |correlación|
cor_df <- cor_df[order(-abs(cor_df$cor_con_peso)), ]

cat("-- Correlaciones con 'peso' (ordenadas por |r|):\n\n")
print(cor_df, row.names = FALSE)

# También: top-20 correlaciones con peso entre todas las numéricas disponibles
cat("\n-- Top-20 variables numéricas con mayor |correlación| con peso (búsqueda amplia):\n")
all_num_vars <- names(df_train)[sapply(df_train, is.numeric)]
all_num_vars <- setdiff(all_num_vars, c("NCODI","anyo","peso",
                                         paste0("u", 1:104), "u900"))
cor_all <- sapply(all_num_vars, function(v) {
  vals <- df_train[[v]]
  mask <- !is.na(vals) & !is.na(df_train$peso)
  if (sum(mask) < 20) return(NA_real_)
  tryCatch(cor(vals[mask], df_train$peso[mask]), error = function(e) NA_real_)
})
cor_all_df <- data.frame(
  variable     = names(cor_all),
  cor_con_peso = round(cor_all, 4),
  stringsAsFactors = FALSE
) %>%
  filter(!is.na(cor_con_peso)) %>%
  arrange(desc(abs(cor_con_peso))) %>%
  head(20)

print(cor_all_df, row.names = FALSE)

# ============================================================
# VALORACIÓN DE ESTRATEGIAS LEGACY
# ============================================================

cat("\n------------------------------------------------------------\n")
cat("VALORACIÓN DE ESTRATEGIAS LEGACY Y RECOMENDACIÓN\n")
cat("------------------------------------------------------------\n")

cat("
# ── ESTRATEGIAS ENCONTRADAS EN LOS SCRIPTS LEGACY ──────────────

# 1. Script legacy/5_variable mix fases.R
#    Enfoque: LASSO sobre datos estandarizados (con imputación por
#    media en entrenamiento, misma media aplicada a sin_peso).
#    Limitaciones:
#      - Imputa NAs con la MEDIA antes de LASSO → contamina la
#        detección de variables relevantes y puede inflar R².
#      - La estandarización escala ambos conjuntos con las medias
#        del conjunto de entrenamiento, lo cual ES correcto; pero
#        la imputación previa lo hace redundante y sesgado.
#      - Lee de rutas absolutas 'h:/' → no portable.
#      - No valida fuera de muestra de forma limpia (usa test_data
#        que también ha sido imputado con medias del conjunto completo).
#    Uso en pipeline actual: ninguno (archivado).

# 2. Script legacy/7_prediccion de variable mix.R
#    Enfoque: LASSO + Random Forest en secuencia.
#    Positivo:
#      - La idea de usar LASSO para selección de variables y luego
#        RF para predicción es metodológicamente sólida.
#      - Separa train/test correctamente.
#      - Aplica preProcess() con center/scale en el conjunto de
#        entrenamiento y lo reutiliza en predicción.
#    Limitaciones:
#      - Lee de data_final.rds (objeto no definido en el pipeline).
#      - Tiene errores de código (x_test, y_test no definidos al
#        calcular métricas; llama predict(lasso_model) y luego
#        predict(lm_model_lasso) de forma inconsistente).
#      - No guarda el modelo → no reproducible.

# 3. Script 9_construccion de variable mix.R (ACTUAL)
#    Enfoque: LASSO (selección) + Random Forest (predicción).
#    Positivo:
#      - Integrado en el pipeline con rutas desde config.
#      - Usa mediana (más robusta que media) para imputación de NAs.
#      - Calcula mtry = p/3 (heurístico estándar para RF en regresión).
#      - Guarda gráficos de diagnóstico.
#      - Evalúa con MAE, RMSE, MAPE, R² in-sample.
#    Limitación principal:
#      - Entrenamiento solo con años 2010-2015 (los que tienen peso).
#        Para 2016-2023 extrapola fuera del dominio de entrenamiento.
#        Si las variables auxiliares cambian de nombre o cobertura
#        en 2016-2023, la predicción puede ser muy imprecisa.

# ── RECOMENDACIÓN PARA EXTENDER A 2016-2023 ─────────────────────

# Dado lo observado en el Paso 4, las variables con mayor
# correlación con peso son habitualmente:
#   - Estancia media (estancias_total / altFinal_total)
#   - Proporción de cirugía (altFinal_cirugia / altFinal_total)
#   - Proporción de UCI/intensiva
#   - Volumen absoluto (altFinal_total, camas_funcionamiento)
#
# Estas variables están disponibles (o son reconstruibles) en todo
# el panel 2010-2023. Por tanto, la estrategia del script 9 es
# la más adecuada SI se corrigen dos puntos:
#
# A) VALIDACIÓN TEMPORAL: en lugar de validación aleatoria,
#    usar 2010-2013 como entrenamiento y 2014-2015 como test
#    temporal. Esto mide mejor la capacidad de extrapolación
#    a años futuros (2016-2023).
#
# B) CONSISTENCIA DE VARIABLES: antes de entrenar, verificar
#    que las variables seleccionadas por LASSO existen en 2016-2023
#    con cobertura suficiente (revisar panel_zero_variance.csv y
#    var_suspects_review.csv generados por scripts 2-3).
#    Si una variable desaparece en 2016, el modelo RF fallará
#    silenciosamente (se imputa 0 o mediana → sesgo).
#
# C) ALTERNATIVA más robusta: usar solo las 3-5 variables con
#    mayor correlación con peso (identificadas en el Paso 4)
#    en una regresión lineal simple (interpretable, sin caja negra).
#    Esto sacrifica algo de R² pero da mucha más garantía de
#    extrapolación estable fuera del rango de entrenamiento.
")

cat("\n============================================================\n")
cat("FIN DEL DIAGNÓSTICO\n")
cat("Guardado en:", DIAG_OUT, "\n")
cat("============================================================\n")

sink()
message("\nDiagnóstico completado. Resultado en:\n  ", DIAG_OUT)
