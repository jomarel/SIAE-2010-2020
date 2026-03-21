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

load(DF_FINAL_RDATA_PATH)
if (!exists("df_final")) {
  stop("No se encontró df_final en ", DF_FINAL_RDATA_PATH)
}

df <- df_final

# -----------------------------
# 1. Normalización de nombres
# -----------------------------
normalize_name <- function(x) {
  x2 <- iconv(x, from = "", to = "ASCII//TRANSLIT")
  x2[is.na(x2)] <- x
  x2 <- tolower(x2)
  x2 <- gsub("_x[0-9a-f]{4}_", "_", x2)   # patrones xml tipo _x0020_
  x2 <- gsub("[^a-z0-9]+", "_", x2)
  x2 <- gsub("_+", "_", x2)
  x2 <- gsub("^_|_$", "", x2)
  x2
}

names_orig <- names(df)
names_norm <- normalize_name(names_orig)

# -----------------------------
# 2. Crear "raíces" aproximadas
# -----------------------------
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

roots <- make_root(names_norm)

dict_vars <- data.frame(
  var_original = names_orig,
  var_norm = names_norm,
  root = roots,
  stringsAsFactors = FALSE
)

# -----------------------------
# 3. Familias con más de una variable
# -----------------------------
familias <- dict_vars %>%
  count(root, name = "n_vars") %>%
  filter(n_vars > 1) %>%
  arrange(desc(n_vars), root)

message("Número de familias con más de una variable: ", nrow(familias))

# -----------------------------
# 4. Funciones de comparación
# -----------------------------
is_blank_or_na <- function(x) {
  if (is.character(x)) {
    is.na(x) | trimws(x) == ""
  } else {
    is.na(x)
  }
}

compare_pair <- function(df, var1, var2) {
  x <- df[[var1]]
  y <- df[[var2]]

  x_blank <- is_blank_or_na(x)
  y_blank <- is_blank_or_na(y)

  both_nonblank <- !x_blank & !y_blank
  only_x <- !x_blank & y_blank
  only_y <- x_blank & !y_blank

  n_both <- sum(both_nonblank)
  n_only_x <- sum(only_x)
  n_only_y <- sum(only_y)

  eq_when_both <- NA_real_
  exact_equal <- FALSE

  if (length(x) == length(y)) {
    exact_equal <- identical(x, y)
  }

  if (n_both > 0) {
    eq_when_both <- mean(as.character(x[both_nonblank]) == as.character(y[both_nonblank]))
  }

  by_year <- df %>%
    transmute(
      anyo = anyo,
      x_nonblank = !x_blank,
      y_nonblank = !y_blank
    ) %>%
    group_by(anyo) %>%
    summarise(
      n_x = sum(x_nonblank),
      n_y = sum(y_nonblank),
      .groups = "drop"
    )

  years_x_only <- paste(by_year$anyo[by_year$n_x > 0 & by_year$n_y == 0], collapse = ",")
  years_y_only <- paste(by_year$anyo[by_year$n_y > 0 & by_year$n_x == 0], collapse = ",")
  years_both   <- paste(by_year$anyo[by_year$n_x > 0 & by_year$n_y > 0], collapse = ",")

  relation <- dplyr::case_when(
    exact_equal ~ "duplicado_exacto",
    n_both == 0 & n_only_x > 0 & n_only_y > 0 ~ "complementarias_sin_solape",
    n_both > 0 & !is.na(eq_when_both) && eq_when_both == 1 & (n_only_x > 0 | n_only_y > 0) ~ "misma_variable_por_periodos",
    n_both > 0 & !is.na(eq_when_both) && eq_when_both == 1 ~ "solape_igual",
    n_both > 0 & !is.na(eq_when_both) && eq_when_both >= 0.9 ~ "solape_muy_parecido",
    TRUE ~ "revisar"
  )

  data.frame(
    var1 = var1,
    var2 = var2,
    n_both = n_both,
    n_only_var1 = n_only_x,
    n_only_var2 = n_only_y,
    eq_when_both = eq_when_both,
    exact_equal = exact_equal,
    years_only_var1 = years_x_only,
    years_only_var2 = years_y_only,
    years_both = years_both,
    relation = relation,
    stringsAsFactors = FALSE
  )
}

# -----------------------------
# 5. Comparar dentro de familias
# -----------------------------
results <- list()
k <- 1

for (r in familias$root) {
  vars_r <- dict_vars$var_original[dict_vars$root == r]

  if (length(vars_r) < 2) next

  pairs <- combn(vars_r, 2, simplify = FALSE)

  for (p in pairs) {
    comp <- compare_pair(df, p[1], p[2])
    comp$root <- r
    results[[k]] <- comp
    k <- k + 1
  }
}

scan_results <- bind_rows(results) %>%
  arrange(root, relation, desc(eq_when_both), desc(n_both))

# -----------------------------
# 6. Guardar resultados
# -----------------------------
SCAN_CSV_PATH <- file.path(LEGACY_BASE_DIR, "scan_variables_parecidas.csv")
DICT_CSV_PATH <- file.path(LEGACY_BASE_DIR, "diccionario_nombres_normalizados.csv")

write.csv(scan_results, SCAN_CSV_PATH, row.names = FALSE, na = "")
write.csv(dict_vars, DICT_CSV_PATH, row.names = FALSE, na = "")

message("Informe guardado en: ", SCAN_CSV_PATH)
message("Diccionario de nombres guardado en: ", DICT_CSV_PATH)

# -----------------------------
# 7. Mostrar resumen útil
# -----------------------------
message("\n--- RESUMEN DE RELACIONES DETECTADAS ---")
print(table(scan_results$relation, useNA = "ifany"))

message("\n--- EJEMPLOS DE POSIBLES DUPLICADOS O VARIABLES UNIFICABLES ---")
print(
  scan_results %>%
    filter(relation %in% c("duplicado_exacto", "misma_variable_por_periodos", "solape_igual", "solape_muy_parecido")) %>%
    select(root, var1, var2, relation, eq_when_both, years_only_var1, years_only_var2, years_both) %>%
    head(50)
)
