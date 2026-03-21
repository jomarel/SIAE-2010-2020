# ============================================================
# 12_etiquetas.R
# Reordena columnas de df_depurado y exporta inventario de
# nombres de variable a un fichero de texto para documentación.
# Las etiquetas completas se asignan en el script 13 sobre
# df_seleccion (donde el conjunto de variables es definitivo).
#
# Input:  df_depurado (DF_DEPURADO_RDATA_PATH)
# Output: df_depurado (actualizado, mismo path)
#         NOMBRES_ETIQUETAS_TXT_PATH  — inventario nombre/etiqueta
#         DF_DEPURADO_CSV_PATH        — copia CSV para inspección
# ============================================================

config_path <- if (file.exists("scripts/00_config.R")) "scripts/00_config.R" else "00_config.R"
source(config_path)

required_pkgs <- c("dplyr")
missing_pkgs  <- required_pkgs[!vapply(required_pkgs, requireNamespace,
                                       logical(1), quietly = TRUE)]
if (length(missing_pkgs) > 0) {
  stop("Faltan paquetes: ", paste(missing_pkgs, collapse = ", "),
       "\nInstálalos con install.packages().", call. = FALSE)
}

library(dplyr)

# ------------------------------------------------------------
# 1. Cargar df_depurado
# ------------------------------------------------------------
if (!file.exists(DF_DEPURADO_RDATA_PATH)) {
  stop("No se encontró df_depurado en: ", DF_DEPURADO_RDATA_PATH,
       "\nEjecuta el script 11 antes.", call. = FALSE)
}

load(DF_DEPURADO_RDATA_PATH)

if (!exists("df_depurado")) {
  stop("Objeto df_depurado no encontrado.", call. = FALSE)
}

message("df_depurado cargado: ", nrow(df_depurado), " filas x ",
        ncol(df_depurado), " columnas.")

# ------------------------------------------------------------
# 2. Reordenar columnas — claves primero, luego el resto
# ------------------------------------------------------------
front_cols <- intersect(
  c("NCODI", "anyo", "nombre_hospital", "ccaa", "ccaa_codigo",
    "ccaa_cnh", "provincia_cnh", "cod_centro", "CODCNH",
    "Depend_agrupada", "cod_depend_agrupada",
    "Finalidad_agrupada", "cod_finalidad_agrupada",
    "peso", "mix"),
  names(df_depurado)
)

df_depurado <- df_depurado |>
  select(all_of(front_cols), everything())

message("Columnas reordenadas. Total: ", ncol(df_depurado))

# ------------------------------------------------------------
# 3. Exportar inventario de variables
#    Intenta leer etiquetas con sjlabelled si está disponible;
#    si no, usa solo los nombres de columna.
# ------------------------------------------------------------
nombres  <- names(df_depurado)

if (requireNamespace("sjlabelled", quietly = TRUE)) {
  etiquetas <- sjlabelled::get_label(df_depurado)
  # get_label devuelve "" cuando no hay etiqueta — convertir a NA
  etiquetas[etiquetas == ""] <- NA_character_
} else {
  message("Paquete sjlabelled no disponible. Exportando solo nombres.")
  etiquetas <- rep(NA_character_, length(nombres))
}

df_etiquetas <- data.frame(
  Nombre_Variable = nombres,
  Etiqueta        = etiquetas,
  stringsAsFactors = FALSE
)

write.table(
  df_etiquetas,
  file          = NOMBRES_ETIQUETAS_TXT_PATH,
  sep           = "\t",
  row.names     = FALSE,
  quote         = FALSE,
  fileEncoding  = "UTF-8"
)

message("Inventario de variables guardado en: ", NOMBRES_ETIQUETAS_TXT_PATH)

# ------------------------------------------------------------
# 4. Guardar df_depurado actualizado
# ------------------------------------------------------------
save(df_depurado, file = DF_DEPURADO_RDATA_PATH)

write.table(df_depurado, DF_DEPURADO_CSV_PATH,
            sep = ";", dec = ",", row.names = FALSE, na = "")

message("df_depurado guardado: ", DF_DEPURADO_RDATA_PATH)
message("\n=== Script 12 completado ===")
