# ============================================================
# 10_depuracion df_completo.R
# Limpieza estructural de df_completo:
#   - Corrige Depend_agrupada
#   - Elimina hospital NCODI=39 (datos estructuralmente inconsistentes)
#   - Rellena Finalidad_agrupada dentro de cada hospital
#   - Elimina columnas completamente NA
#
# Input:  df_completo (DF_COMPLETO_RDATA_PATH)
# Output: df_completo_actualizado (DF_COMPLETO_ACTUALIZADO_RDATA_PATH)
# ============================================================

config_path <- if (file.exists("scripts/00_config.R")) "scripts/00_config.R" else "00_config.R"
source(config_path)

required_pkgs <- c("dplyr", "tidyr")
missing_pkgs  <- required_pkgs[!vapply(required_pkgs, requireNamespace,
                                       logical(1), quietly = TRUE)]
if (length(missing_pkgs) > 0) {
  stop("Faltan paquetes: ", paste(missing_pkgs, collapse = ", "),
       "\nInstálalos con install.packages().", call. = FALSE)
}

library(dplyr)
library(tidyr)

# ------------------------------------------------------------
# 1. Cargar df_completo
# ------------------------------------------------------------
if (!file.exists(DF_COMPLETO_RDATA_PATH)) {
  stop("No se encontró df_completo en: ", DF_COMPLETO_RDATA_PATH,
       "\nEjecuta el script 9 antes.", call. = FALSE)
}

load(DF_COMPLETO_RDATA_PATH)

if (!exists("df_completo")) {
  stop("El objeto df_completo no existe en ", DF_COMPLETO_RDATA_PATH, call. = FALSE)
}

message("df_completo cargado: ", nrow(df_completo), " filas x ",
        ncol(df_completo), " columnas.")

df <- df_completo

# ------------------------------------------------------------
# 2. Corregir Depend_agrupada
#    Unificar variantes textuales de "Publicos-SNS"
# ------------------------------------------------------------
if ("Depend_agrupada" %in% names(df)) {
  n_antes <- sum(grepl("Publicos.SNS", df$Depend_agrupada, ignore.case = TRUE),
                 na.rm = TRUE)
  df <- df |>
    mutate(Depend_agrupada = gsub(
      "Publicos-SNS|Publicos_SNS", "Publicos",
      Depend_agrupada, ignore.case = FALSE
    ))
  message("Depend_agrupada: ", n_antes, " valores 'Publicos-/SNS' unificados a 'Publicos'.")

  n_na_dep <- sum(is.na(df$Depend_agrupada))
  message("  NA en Depend_agrupada: ", n_na_dep)

  if (n_na_dep > 0) {
    df <- df |> filter(!is.na(Depend_agrupada))
    message("  Eliminadas ", n_na_dep, " filas con Depend_agrupada = NA.")
  }
} else {
  warning("Variable Depend_agrupada no encontrada en df_completo.", call. = FALSE)
}

# ------------------------------------------------------------
# 3. Eliminar NCODI = 39
#    Este hospital presenta datos estructuralmente inconsistentes
#    (módulos con años cruzados y variables de actividad sin sentido).
#    Decisión documentada: excluido en todos los análisis.
# ------------------------------------------------------------
NCODI_EXCLUIR <- "39"

n_antes_excl <- nrow(df)
df <- df |> filter(NCODI != NCODI_EXCLUIR)
n_excl <- n_antes_excl - nrow(df)

message(sprintf("Hospital NCODI=%s excluido: %d filas eliminadas.",
                NCODI_EXCLUIR, n_excl))

# ------------------------------------------------------------
# 4. Rellenar Finalidad_agrupada dentro de cada hospital
#    (hacia adelante y atrás en el tiempo)
# ------------------------------------------------------------
if ("Finalidad_agrupada" %in% names(df)) {
  n_na_fin <- sum(is.na(df$Finalidad_agrupada))
  message("NA en Finalidad_agrupada antes de rellenar: ", n_na_fin)

  df <- df |>
    group_by(NCODI) |>
    fill(Finalidad_agrupada, .direction = "downup") |>
    ungroup()

  n_na_fin_post <- sum(is.na(df$Finalidad_agrupada))
  message("NA en Finalidad_agrupada después de rellenar: ", n_na_fin_post)
} else {
  warning("Variable Finalidad_agrupada no encontrada.", call. = FALSE)
}

# ------------------------------------------------------------
# 5. Eliminar columnas completamente NA
#    (excluir variables protegidas u1..u104, u900)
# ------------------------------------------------------------
PROTECTED_VARS <- c(paste0("u", 1:104), "u900", "NCODI", "anyo")

is_all_na <- function(x) {
  if (is.character(x)) all(is.na(x) | trimws(x) == "")
  else all(is.na(x))
}

cols_all_na <- names(df)[vapply(df, is_all_na, logical(1))]
cols_all_na <- setdiff(cols_all_na, PROTECTED_VARS)

message("\nColumnas completamente NA a eliminar: ", length(cols_all_na))
if (length(cols_all_na) > 0) {
  print(cols_all_na)
  df <- df |> select(-all_of(cols_all_na))
}

# ------------------------------------------------------------
# 6. Verificar unicidad de clave (NCODI, anyo)
# ------------------------------------------------------------
n_dup <- sum(duplicated(df[, c("NCODI", "anyo")]))
if (n_dup > 0) {
  stop("Hay ", n_dup, " filas duplicadas en (NCODI, anyo) tras la depuración.",
       call. = FALSE)
}
message("Unicidad (NCODI, anyo): OK")

# ------------------------------------------------------------
# 7. Guardar df_completo_actualizado
# ------------------------------------------------------------
df_completo_actualizado <- df

save(df_completo_actualizado, file = DF_COMPLETO_ACTUALIZADO_RDATA_PATH)

write.table(df_completo_actualizado, DF_COMPLETO_ACTUALIZADO_TXT_PATH,
            sep = "\t", row.names = FALSE, na = "")

message("\n=== Script 10 completado ===")
message("Filas: ", nrow(df_completo_actualizado),
        " | Columnas: ", ncol(df_completo_actualizado))
message("Guardado en: ", DF_COMPLETO_ACTUALIZADO_RDATA_PATH)
