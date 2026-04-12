# ============================================================
# 1_exportar_mapeo_cnh.R
# Lee el Excel CNH, limpia nombres y guarda el mapeo
# NCODI→ nombre_hospital + variables descriptivas
# ============== ==============================================

# Ruta a 00_config.R siempre relativa al propio script, sin importar el working dir
config_path <- file.path(
  dirname(normalizePath(sys.frame(1)$ofile)),
  "00_config.R"
)
source(config_path)

if (!requireNamespace("readxl", quietly = TRUE)) install.packages("readxl")
library(readxl)
library(dplyr)

EXCEL_PATH <- file.path(DOCS_DIR, "NCODI_Hospital_DEFINITIVO_2020_2024.xlsx")

if (!file.exists(EXCEL_PATH)) {
  stop("No se encuentra el Excel: ", EXCEL_PATH)
}

# ── Leer hoja de correspondencia ─────────────────────────────
df_raw <- read_excel(EXCEL_PATH, sheet = "🗂 Correspondencia")

cat(sprintf("Leídas %d filas x %d columnas\n", nrow(df_raw), ncol(df_raw)))

# ── Renombrar columnas (eliminar \n y caracteres especiales) ──
df_mapeo <- df_raw %>%
  rename(
    NCODI               = "NCODI",
    cod_centro          = "cod_centro",
    CODCNH              = "CODCNH",
    cod_provincia       = "Cód.\nProv.",
    nombre_hospital     = "Nombre Hospital\n(mejor disponible)",
    ccaa_cnh            = "CCAA",
    provincia_cnh       = "Provincia",
    finalidad_cnh       = "Finalidad\nSIAE",
    pertenencia_sns_cnh = "Pertenencia\nSNS",
    camas_siae          = "Camas\nSIAE",
    quirofanos_siae     = "Quirof.\nSIAE",
    camas_cnh24         = "Camas\nCNH24",
    clase_centro        = "Clase\nCentro",
    dependencia_cnh     = "Dependencia\nFuncional",
    estado_cnh          = "Estado",
    ccn_regcess         = "CCN (REGCESS)"
  ) %>%
  mutate(
    NCODI      = as.character(as.integer(NCODI)),
    cod_centro = as.character(trimws(cod_centro)),
    CODCNH     = as.character(as.integer(CODCNH))
  )

# ── Diagnóstico ───────────────────────────────────────────────
cat(sprintf("Hospitales en el mapeo:         %d\n",  nrow(df_mapeo)))
cat(sprintf("NCODI únicos:                   %d\n",  n_distinct(df_mapeo$NCODI)))
cat(sprintf("cod_centro únicos:              %d\n",  n_distinct(df_mapeo$cod_centro)))
cat(sprintf("Con nombre_hospital:            %d\n",  sum(!is.na(df_mapeo$nombre_hospital))))
cat(sprintf("Sin nombre_hospital:            %d\n",  sum( is.na(df_mapeo$nombre_hospital))))
cat(sprintf("NCODI duplicados:               %d\n",  sum(duplicated(df_mapeo$NCODI))))

cat("\nPrimeras filas:\n")
print(df_mapeo %>% select(NCODI, cod_centro, CODCNH, nombre_hospital, 
                           ccaa_cnh, provincia_cnh) %>% head(6))

# ── Guardar ───────────────────────────────────────────────────
OUT_PATH <- file.path(INT_DIR, "ncodi_hospital_map.csv")
dir.create(INT_DIR, recursive = TRUE, showWarnings = FALSE)

write.csv(df_mapeo, OUT_PATH, row.names = FALSE, fileEncoding = "UTF-8")
cat(sprintf("\n✓ Guardado en: %s\n", OUT_PATH))