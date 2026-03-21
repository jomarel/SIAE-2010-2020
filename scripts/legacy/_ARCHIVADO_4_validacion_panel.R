config_path <- if (file.exists("scripts/00_config.R")) "scripts/00_config.R" else "00_config.R"
source(config_path)

required_pkgs <- c("dplyr", "stringr")
missing_pkgs <- required_pkgs[!vapply(required_pkgs, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing_pkgs) > 0) {
  stop(
    "Faltan paquetes: ",
    paste(missing_pkgs, collapse = ", "),
    "\nInstálalos manualmente con install.packages()."
  )
}

library(dplyr)
library(stringr)

# -----------------------------
# 1. Cargar panel completo
# -----------------------------
load(DF_FINAL_RDATA_PATH)

if (!exists("df_final")) {
  stop("No se encontró el objeto df_final al cargar ", DF_FINAL_RDATA_PATH)
}

df <- df_final

message("Panel cargado: ", nrow(df), " filas y ", ncol(df), " columnas.")

# -----------------------------
# 2. Comprobaciones estructurales básicas
# -----------------------------
message("\n--- VALIDACION ESTRUCTURAL ---")

message("Duplicados (NCODI, anyo): ",
        sum(duplicated(df[, c("NCODI", "anyo")])))

message("\nFilas por año:")
print(table(df$anyo, useNA = "ifany"))

resumen_hospitales <- df %>%
  group_by(anyo) %>%
  summarise(n_hospitales = n_distinct(NCODI), .groups = "drop")

message("\nHospitales únicos por año:")
print(resumen_hospitales)

nombres_duplicados <- names(df)[duplicated(names(df))]
message("\nNúmero de nombres de variable duplicados: ", length(nombres_duplicados))
if (length(nombres_duplicados) > 0) {
  print(nombres_duplicados)
}

xy_names <- names(df)[str_detect(names(df), "\\.(x|y)$")]
message("\nNúmero de variables con sufijos .x / .y: ", length(xy_names))
if (length(xy_names) > 0) {
  print(xy_names)
}

# -----------------------------
# 3. Columnas completamente vacías
# -----------------------------
es_vacia <- function(x) {
  if (is.character(x)) {
    all(is.na(x) | trimws(x) == "")
  } else {
    all(is.na(x))
  }
}

cols_vacias <- names(df)[vapply(df, es_vacia, logical(1))]
message("\nNúmero de columnas completamente vacías: ", length(cols_vacias))
if (length(cols_vacias) > 0) {
  print(cols_vacias)
}

# -----------------------------
# 4. Variables sospechosas relacionadas con año (solo informativo)
# -----------------------------
message("\n--- VARIABLES SOSPECHOSAS DE AÑO (solo informativo) ---")

vars_year_like <- names(df)[
  str_detect(
    string = iconv(names(df), from = "", to = "ASCII//TRANSLIT"),
    pattern = regex("ano|año|anyo|year", ignore_case = TRUE)
  )
]

print(vars_year_like)

# -----------------------------
# 5. Limpieza básica segura
#    SOLO elimina columnas completamente vacías
# -----------------------------
message("\n--- LIMPIEZA BASICA ---")

vars_drop <- cols_vacias

message("Variables candidatas a eliminar automáticamente: ", length(vars_drop))
if (length(vars_drop) > 0) {
  print(vars_drop)
}

df_validado <- df

if (length(vars_drop) > 0) {
  df_validado <- df_validado %>%
    select(-all_of(vars_drop))
}

# -----------------------------
# 6. Guardar versión validada
# -----------------------------
VALIDATED_RDATA_PATH <- file.path(LEGACY_BASE_DIR, "df_final_validado.RData")
VALIDATED_TXT_PATH   <- file.path(LEGACY_BASE_DIR, "df_final_validado.txt")

save(df_validado, file = VALIDATED_RDATA_PATH)

write.table(
  df_validado,
  file = VALIDATED_TXT_PATH,
  row.names = FALSE,
  sep = ";",
  dec = ",",
  na = ""
)

message("\n--- RESULTADO FINAL ---")
message("Dimensión original: ", nrow(df), " x ", ncol(df))
message("Dimensión validada: ", nrow(df_validado), " x ", ncol(df_validado))
message("Guardado en: ", VALIDATED_RDATA_PATH)