# ============================================================
# 00_config.R
# Configuración central del pipeline SIAE 2010-2023
# Hacer source() de este archivo al inicio de cada script.
# ============================================================

# ------------------------------------------------------------
# DIRECTORIOS PRINCIPALES
# ------------------------------------------------------------

# Carpeta raíz del proyecto (Google Drive)
BASE_DIR    <- "G:/Mi unidad/SIAE 2010-2020"

# Datos originales anuales (TXT/XML por año y módulo)
RAW_DIR     <- file.path(BASE_DIR, "data_raw")

# Datos intermedios: estandarizados, auditorías, mapeos
INT_DIR     <- file.path(BASE_DIR, "data_intermediate")

# Outputs finales para análisis / publicación
OUT_DIR     <- file.path(BASE_DIR, "outputs")

# Documentación del proyecto
DOCS_DIR    <- file.path(BASE_DIR, "docs")

# Ruta de los scripts
SCRIPTS_DIR <- file.path(BASE_DIR, "scripts")

# ------------------------------------------------------------
# DIRECTORIO INTERMEDIO — DATOS ESTANDARIZADOS (script 1 → script 2)
# ------------------------------------------------------------

# Archivos TXT/XML ya con nombres de variables canónicos,
# organizados por año. Generado por 1_cambiarnombredevariables_2C.R.
STD_RAW_DIR <- file.path(INT_DIR, "standardized_raw")

# ------------------------------------------------------------
# DIRECTORIO LEGACY — ARTEFACTOS DEL PIPELINE
# Mantenido para compatibilidad con scripts 4, 9-14.
# ------------------------------------------------------------

LEGACY_BASE_DIR <- file.path(BASE_DIR, "data_legacy_outputs")
dir.create(LEGACY_BASE_DIR, recursive = TRUE, showWarnings = FALSE)

# Artefactos de scripts intermedios
PESOS_TXT_PATH                        <- file.path(INT_DIR, "pesos.txt")
DF_FINAL_RDATA_PATH                   <- file.path(LEGACY_BASE_DIR, "df_final.RData")
DF_FINAL_TXT_PATH                     <- file.path(LEGACY_BASE_DIR, "df_final.txt")
DF_COMPLETO_RDATA_PATH                <- file.path(LEGACY_BASE_DIR, "df_completo.RData")
DF_COMPLETO_ACTUALIZADO_RDATA_PATH    <- file.path(LEGACY_BASE_DIR, "df_completo_actualizado.RData")
DF_COMPLETO_ACTUALIZADO_TXT_PATH      <- file.path(LEGACY_BASE_DIR, "df_completo_actualizado.txt")
DF_FINAL_CON_MIX_TXT_PATH            <- file.path(LEGACY_BASE_DIR, "df_final_con_mix.txt")
DF_DEPURADO_RDATA_PATH                <- file.path(LEGACY_BASE_DIR, "df_depurado.RData")
DF_DEPURADO_TXT_PATH                  <- file.path(LEGACY_BASE_DIR, "df_depurado.txt")
DF_DEPURADO_CSV_PATH                  <- file.path(LEGACY_BASE_DIR, "df_depurado.csv")
NOMBRES_ETIQUETAS_TXT_PATH            <- file.path(LEGACY_BASE_DIR, "nombres_y_etiquetas.txt")
DF_CON_ETIQUETAS_CSV_PATH             <- file.path(LEGACY_BASE_DIR, "df_con_etiquetas.csv")
DF_SELECCION_CSV_PATH                 <- file.path(LEGACY_BASE_DIR, "df_seleccion.csv")
DF_SELECCION_RDATA_PATH               <- file.path(LEGACY_BASE_DIR, "df_seleccion.RData")

# ------------------------------------------------------------
# RUTAS DE DIAGNÓSTICO (INT_DIR)
# Generados por scripts 2 y 3.
# ------------------------------------------------------------

VAR_MAPPING_EXPLICIT_PATH      <- file.path(INT_DIR, "var_mapping_explicit.csv")

# Mapeo NCODI ↔ cod_centro ↔ nombre_hospital (generado por 0_exportar_mapeo_cnh.R)
NCODI_HOSPITAL_MAP_PATH        <- file.path(INT_DIR, "ncodi_hospital_map.csv")
VAR_DUPLICATES_BETWEEN_YEARS_PATH <- file.path(INT_DIR, "var_duplicates_between_years.csv")
VARS_DROPPED_ALL_NA_PATH       <- file.path(INT_DIR, "vars_dropped_all_na.txt")
PANEL_AUDIT_SUMMARY_PATH       <- file.path(INT_DIR, "panel_audit_summary.txt")
PANEL_TYPE_CONFLICTS_PATH      <- file.path(INT_DIR, "panel_type_conflicts.csv")

# ------------------------------------------------------------
# FUNCIÓN DE VERIFICACIÓN DE DIRECTORIOS
# Emite warning() para los que no existan; no detiene ejecución.
# ------------------------------------------------------------

verify_project_dirs <- function() {
  dirs_criticos <- c(
    BASE_DIR  = BASE_DIR,
    RAW_DIR   = RAW_DIR,
    INT_DIR   = INT_DIR,
    OUT_DIR   = OUT_DIR
  )
  dirs_opcionales <- c(
    STD_RAW_DIR     = STD_RAW_DIR,
    LEGACY_BASE_DIR = LEGACY_BASE_DIR,
    DOCS_DIR        = DOCS_DIR
  )

  for (nm in names(dirs_criticos)) {
    if (!dir.exists(dirs_criticos[[nm]])) {
      warning(
        "Directorio crítico no encontrado: ", nm,
        " -> ", dirs_criticos[[nm]],
        call. = FALSE
      )
    }
  }
  for (nm in names(dirs_opcionales)) {
    if (!dir.exists(dirs_opcionales[[nm]])) {
      message("Directorio opcional aún no creado: ", nm, " -> ", dirs_opcionales[[nm]])
    }
  }
}

verify_project_dirs()
STD_RAW_DIR               <- file.path(INT_DIR, "standardized_raw")
NCODI_HOSPITAL_MAP_PATH   <- file.path(INT_DIR, "ncodi_hospital_map.csv")
VAR_MAPPING_EXPLICIT_PATH <- file.path(INT_DIR, "var_mapping_explicit.csv")
message("Configuración cargada correctamente.")
