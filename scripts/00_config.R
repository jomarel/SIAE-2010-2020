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

# Mensaje de verificación
message("Configuración cargada correctamente.")