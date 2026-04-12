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

# =========================================================
# 6_limpieza_estructural_panel.R
# Limpieza estructural del panel completo:
# - elimina columnas de año corruptas redundantes
# - elimina duplicados exactos ya detectados
# - elimina columnas totalmente vacías
# - elimina columnas todo 0
# - informa de variables que solo existen en un año
# - detecta pares perfectamente colineales / idénticos
# =========================================================

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
# 2. Funciones auxiliares
# -----------------------------
is_blank_or_na <- function(x) {
  if (is.character(x)) {
    is.na(x) | trimws(x) == ""
  } else {
    is.na(x)
  }
}

is_all_empty <- function(x) {
  all(is_blank_or_na(x))
}

is_all_zero <- function(x) {
  if (is.character(x)) {
    x2 <- trimws(x)
    x2[x2 == ""] <- NA
    suppressWarnings(xn <- as.numeric(x2))
    all(!is.na(xn) | is.na(x2)) && all(is.na(xn) | xn == 0)
  } else if (is.numeric(x) || is.integer(x)) {
    all(is.na(x) | x == 0)
  } else {
    FALSE
  }
}

normalize_name <- function(x) {
  x2 <- iconv(x, from = "", to = "ASCII//TRANSLIT")
  x2[is.na(x2)] <- x
  x2 <- tolower(x2)
  x2 <- gsub("_x[0-9a-f]{4}_", "_", x2)
  x2 <- gsub("[^a-z0-9]+", "_", x2)
  x2 <- gsub("_+", "_", x2)
  x2 <- gsub("^_|_$", "", x2)
  x2
}

make_root <- function(x) {
  x <- gsub("(^|_)total($|_)", "_", x)
  x <- gsub("(^|_)codigo($|_)", "_", x)
  x <- gsub("(^|_)cod($|_)", "_", x)
  x <- gsub("(^|_)agrupada($|_)", "_", x)
  x <- gsub("(^|_)anonimizacion($|_)", "_", x)
  x <- gsub("(^|_)anonimizada($|_)", "_", x)
  x <- gsub("(^|_)hospital($|_)", "_", x)
  x <- gsub("(^|_)cep($|_)", "_", x)
  x <- gsub("(^|_)instalada($|_)", "_", x)
  x <- gsub("(^|_)funcionamiento($|_)", "_", x)
  x <- gsub("_+", "_", x)
  x <- gsub("^_|_$", "", x)
  x
}

exact_same <- function(x, y) {
  identical(x, y)
}

perfect_collinear_numeric <- function(x, y) {
  if (!(is.numeric(x) || is.integer(x))) return(FALSE)
  if (!(is.numeric(y) || is.integer(y))) return(FALSE)

  ok <- !is.na(x) & !is.na(y)
  if (sum(ok) < 3) return(FALSE)

  x_ok <- x[ok]
  y_ok <- y[ok]

  if (length(unique(x_ok)) < 2 || length(unique(y_ok)) < 2) return(FALSE)

  suppressWarnings(r <- cor(x_ok, y_ok))
  is.finite(r) && abs(r) == 1
}

# -----------------------------
# 3. Eliminar columnas de año corruptas redundantes
#    Conservamos solo 'anyo'
# -----------------------------
year_like_exact <- intersect(
  names(df),
  c("año", "Año", "Anyo", "AÃ±o", "ano", "year", "a<f1>o", "A<f1>o")
)

year_like_exact <- setdiff(year_like_exact, "anyo")

message("\nVariables redundantes de año a eliminar:")
print(year_like_exact)

df_clean <- df

if (length(year_like_exact) > 0) {
  df_clean <- df_clean %>% select(-all_of(year_like_exact))
}

# -----------------------------
# 4. Eliminar columnas completamente vacías
# -----------------------------
cols_empty <- names(df_clean)[vapply(df_clean, is_all_empty, logical(1))]

message("\nColumnas completamente vacías: ", length(cols_empty))
if (length(cols_empty) > 0) {
  print(cols_empty)
  df_clean <- df_clean %>% select(-all_of(cols_empty))
}

# -----------------------------
# 5. Eliminar columnas todo 0
# -----------------------------
cols_all_zero <- setdiff(
  names(df_clean)[vapply(df_clean, is_all_zero, logical(1))],
  c("anyo", "NCODI")
)

message("\nColumnas todo 0: ", length(cols_all_zero))
if (length(cols_all_zero) > 0) {
  print(cols_all_zero)
  df_clean <- df_clean %>% select(-all_of(cols_all_zero))
}

# -----------------------------
# 6. Detectar duplicados exactos por familias de nombres
# -----------------------------
names_orig <- names(df_clean)
names_norm <- normalize_name(names_orig)
roots <- make_root(names_norm)

dict_vars <- data.frame(
  var_original = names_orig,
  var_norm = names_norm,
  root = roots,
  stringsAsFactors = FALSE
)

familias <- dict_vars %>%
  count(root, name = "n_vars") %>%
  filter(n_vars > 1)

dups_exact <- list()
k <- 1

for (r in familias$root) {
  vars_r <- dict_vars$var_original[dict_vars$root == r]
  if (length(vars_r) < 2) next

  pairs <- combn(vars_r, 2, simplify = FALSE)
  for (p in pairs) {
    v1 <- p[1]
    v2 <- p[2]

    if (exact_same(df_clean[[v1]], df_clean[[v2]])) {
      dups_exact[[k]] <- data.frame(
        root = r,
        keep = v1,
        drop = v2,
        stringsAsFactors = FALSE
      )
      k <- k + 1
    }
  }
}

dups_exact_df <- if (length(dups_exact) > 0) bind_rows(dups_exact) else data.frame()

message("\nDuplicados exactos detectados: ", nrow(dups_exact_df))
if (nrow(dups_exact_df) > 0) {
  print(dups_exact_df)
}

# Eliminar las variables 'drop' de duplicados exactos
vars_drop_dup_exact <- unique(dups_exact_df$drop)

if (length(vars_drop_dup_exact) > 0) {
  df_clean <- df_clean %>% select(-all_of(vars_drop_dup_exact))
}

# -----------------------------
# 7. Variables que solo existen en un año
# -----------------------------
vars_single_year <- lapply(names(df_clean), function(v) {
  x <- df_clean[[v]]
  nonblank <- !is_blank_or_na(x)
  years_present <- sort(unique(df_clean$anyo[nonblank]))

  if (length(years_present) == 1) {
    data.frame(
      variable = v,
      only_year = years_present,
      n_nonblank = sum(nonblank),
      stringsAsFactors = FALSE
    )
  } else {
    NULL
  }
}) %>% bind_rows()

message("\nVariables que solo existen en un año: ", nrow(vars_single_year))
if (nrow(vars_single_year) > 0) {
  print(head(vars_single_year, 50))
}

# No se eliminan automáticamente

# -----------------------------
# 8. Detectar pares perfectamente colineales
#    Solo variables numéricas
# -----------------------------
numeric_vars <- names(df_clean)[vapply(df_clean, function(x) is.numeric(x) || is.integer(x), logical(1))]
numeric_vars <- setdiff(numeric_vars, c("anyo"))

message("\nNúmero de variables numéricas candidatas a revisar colinealidad perfecta: ", length(numeric_vars))

collinear_pairs <- list()
k <- 1

if (length(numeric_vars) >= 2) {
  pairs_num <- combn(numeric_vars, 2, simplify = FALSE)

  for (p in pairs_num) {
    v1 <- p[1]
    v2 <- p[2]

    if (perfect_collinear_numeric(df_clean[[v1]], df_clean[[v2]])) {
      collinear_pairs[[k]] <- data.frame(
        var1 = v1,
        var2 = v2,
        stringsAsFactors = FALSE
      )
      k <- k + 1
    }
  }
}

collinear_df <- if (length(collinear_pairs) > 0) bind_rows(collinear_pairs) else data.frame()

message("\nPares perfectamente colineales detectados: ", nrow(collinear_df))
if (nrow(collinear_df) > 0) {
  print(head(collinear_df, 100))
}

# No se eliminan automáticamente

# -----------------------------
# 9. Comprobaciones finales
# -----------------------------
dup_keys <- sum(duplicated(df_clean[, c("NCODI", "anyo")]))
message("\nDuplicados finales de (NCODI, anyo): ", dup_keys)

if (dup_keys > 0) {
  stop("La limpieza ha introducido duplicados en (NCODI, anyo).")
}

# -----------------------------
# 10. Guardar outputs
# -----------------------------
CLEAN_RDATA_PATH <- file.path(LEGACY_BASE_DIR, "df_final_limpio.RData")
CLEAN_TXT_PATH   <- file.path(LEGACY_BASE_DIR, "df_final_limpio.txt")

REPORT_DUP_PATH  <- file.path(LEGACY_BASE_DIR, "reporte_duplicados_exactos.csv")
REPORT_1YEAR_PATH <- file.path(LEGACY_BASE_DIR, "reporte_variables_un_solo_anio.csv")
REPORT_COLLIN_PATH <- file.path(LEGACY_BASE_DIR, "reporte_colinealidad_perfecta.csv")

save(df_clean, file = CLEAN_RDATA_PATH)

write.table(
  df_clean,
  file = CLEAN_TXT_PATH,
  row.names = FALSE,
  sep = ";",
  dec = ",",
  na = ""
)

write.csv(dups_exact_df, REPORT_DUP_PATH, row.names = FALSE, na = "")
write.csv(vars_single_year, REPORT_1YEAR_PATH, row.names = FALSE, na = "")
write.csv(collinear_df, REPORT_COLLIN_PATH, row.names = FALSE, na = "")

# -----------------------------
# 11. Resumen final
# -----------------------------
message("\n--- RESUMEN FINAL DE LIMPIEZA ---")
message("Dimensión original: ", nrow(df), " x ", ncol(df))
message("Dimensión limpia:   ", nrow(df_clean), " x ", ncol(df_clean))

message("\nGuardado panel limpio en: ", CLEAN_RDATA_PATH)
message("Guardado txt limpio en:   ", CLEAN_TXT_PATH)

message("\nReportes:")
message(" - Duplicados exactos:      ", REPORT_DUP_PATH)
message(" - Variables de un solo año:", REPORT_1YEAR_PATH)
message(" - Colinealidad perfecta:   ", REPORT_COLLIN_PATH)