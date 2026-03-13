# ================================
# 00_config.R
# Configuración central del proyecto
# ================================

# Carpeta raíz del proyecto (Google Drive)
BASE_DIR <- "G:/Mi unidad/SIAE 2010-2020"

# Subcarpetas
RAW_DIR  <- file.path(BASE_DIR, "data_raw")
INT_DIR  <- file.path(BASE_DIR, "data_intermediate")
OUT_DIR  <- file.path(BASE_DIR, "outputs")
DOCS_DIR <- file.path(BASE_DIR, "docs")
SCRIPTS_DIR <- file.path(BASE_DIR, "scripts")

# Rutas de artefactos legacy del pipeline (se mantienen para no alterar comportamiento)
LEGACY_BASE_DIR <- file.path(BASE_DIR, "data_legacy_outputs")
dir.create(LEGACY_BASE_DIR, recursive = TRUE, showWarnings = FALSE)
PESOS_TXT_PATH <- file.path(LEGACY_BASE_DIR, "pesos.txt")
DF_FINAL_RDATA_PATH <- file.path(LEGACY_BASE_DIR, "df_final.RData")
DF_FINAL_TXT_PATH <- file.path(LEGACY_BASE_DIR, "df_final.txt")
DF_COMPLETO_RDATA_PATH <- file.path(LEGACY_BASE_DIR, "df_completo.RData")
DF_COMPLETO_ACTUALIZADO_RDATA_PATH <- file.path(LEGACY_BASE_DIR, "df_completo_actualizado.RData")
DF_COMPLETO_ACTUALIZADO_TXT_PATH <- file.path(LEGACY_BASE_DIR, "df_completo_actualizado.txt")
DF_FINAL_CON_MIX_TXT_PATH <- file.path(LEGACY_BASE_DIR, "df_final_con_mix.txt")
DF_DEPURADO_RDATA_PATH <- file.path(LEGACY_BASE_DIR, "df_depurado.RData")
DF_DEPURADO_TXT_PATH <- file.path(LEGACY_BASE_DIR, "df_depurado.txt")
DF_DEPURADO_CSV_PATH <- file.path(LEGACY_BASE_DIR, "df_depurado.csv")
NOMBRES_ETIQUETAS_TXT_PATH <- file.path(LEGACY_BASE_DIR, "nombres_y_etiquetas.txt")
DF_CON_ETIQUETAS_CSV_PATH <- file.path(LEGACY_BASE_DIR, "df_con_etiquetas.csv")
DF_SELECCION_CSV_PATH <- file.path(LEGACY_BASE_DIR, "df_seleccion.csv")
DF_SELECCION_RDATA_PATH <- file.path(LEGACY_BASE_DIR, "df_seleccion.RData")

# Mensaje de verificación
message("Configuración cargada correctamente.")
