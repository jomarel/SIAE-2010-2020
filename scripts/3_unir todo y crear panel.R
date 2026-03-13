config_path <- if (file.exists("scripts/00_config.R")) "scripts/00_config.R" else "00_config.R"
source(config_path)

required_pkgs <- c("dplyr", "readr")
missing_pkgs <- required_pkgs[!vapply(required_pkgs, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing_pkgs) > 0) {
  stop(
    "Faltan paquetes: ",
    paste(missing_pkgs, collapse = ", "),
    "\nInstálalos manualmente con install.packages()."
  )
}

library(dplyr)
library(readr)

list_df <- list()

year_files <- list.files(
  path = LEGACY_BASE_DIR,
  pattern = "^df_[0-9]{4}\\.csv$",
  full.names = TRUE
)

stopifnot(length(year_files) > 0)

years <- as.integer(sub("^df_([0-9]{4})\\.csv$", "\\1", basename(year_files)))
ord <- order(years)
year_files <- year_files[ord]
years <- years[ord]

for (idx in seq_along(year_files)) {
  tmp <- readr::read_delim(
    file = year_files[idx],
    delim = ",",
    col_types = cols(.default = col_character()),
    show_col_types = FALSE,
    progress = FALSE,
    name_repair = "minimal"
  ) %>%
    as.data.frame(stringsAsFactors = FALSE)

  if (!("NCODI" %in% names(tmp))) {
    stop("Falta NCODI en archivo anual: ", basename(year_files[idx]))
  }

  tmp$NCODI <- trimws(as.character(tmp$NCODI))
  tmp$anyo <- as.integer(years[idx])

  tmp <- tmp %>% select(anyo, NCODI, everything())

  dup_n <- sum(duplicated(tmp[, c("NCODI", "anyo")]))
  if (dup_n > 0) {
    stop(
      "Hay duplicados de (NCODI, anyo) en archivo anual ",
      basename(year_files[idx]),
      ": ",
      dup_n
    )
  }

  list_df[[idx]] <- tmp
}

df_final <- dplyr::bind_rows(list_df) %>%
  arrange(anyo, NCODI)

# Convertir tipos: mantener claves fijas y convertir el resto cuando se pueda
cols_to_convert <- setdiff(names(df_final), c("NCODI"))

df_final[cols_to_convert] <- lapply(df_final[cols_to_convert], function(x) {
  x <- trimws(x)
  x[x == ""] <- NA

  # Intentar convertir a numérico si todos los no-NA parecen números
  suppressWarnings(x_num <- as.numeric(x))
  if (all(is.na(x) == is.na(x_num))) {
    return(x_num)
  }

  x
})

dup_panel <- sum(duplicated(df_final[, c("NCODI", "anyo")]))
if (dup_panel > 0) {
  stop("Hay duplicados de (NCODI, anyo) en el panel final: ", dup_panel)
}

save(df_final, file = DF_FINAL_RDATA_PATH)

write.table(
  df_final,
  file = DF_FINAL_TXT_PATH,
  row.names = FALSE,
  sep = ";",
  dec = ",",
  na = ""
)

message(
  "Panel final creado correctamente. Filas: ", nrow(df_final),
  ". Columnas: ", ncol(df_final),
  ". Años: ", min(df_final$anyo), "-", max(df_final$anyo)
)

########################################################################################
# Bloque exploratorio desactivado para ejecución batch del pipeline
if (FALSE) {
  # str(df_final)
  # summary(df_final$anyo)
}