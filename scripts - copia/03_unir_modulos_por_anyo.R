# ============================================================
# 2_unirmodulos_por_anyo.R
# Normaliza nombres de variables (lógica integrada de
# 4_limpieza_nombres_variables_CL.R), luego une los módulos
# de cada año por NCODI → df_YYYY.csv, y ejecuta auditoría
# de variables entre años con merges automáticos.
#
# Protección especial: u1..u104 y u900 nunca se fusionan.
# ============================================================

config_path <- file.path(
  dirname(normalizePath(sys.frame(1)$ofile)),
  "00_config.R"
)
source(config_path)

required_pkgs <- c("dplyr", "tidyr", "fs", "stringr", "readr", "stringdist", "purrr")
missing_pkgs <- required_pkgs[!vapply(required_pkgs, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing_pkgs) > 0) {
  stop(
    "Faltan paquetes: ", paste(missing_pkgs, collapse = ", "),
    "\nInstálalos con install.packages().",
    call. = FALSE
  )
}

library(dplyr)
library(tidyr)
library(fs)
library(stringr)
library(readr)
library(stringdist)
library(purrr)

# ============================================================
# CONSTANTES
# ============================================================

PROTECTED_VARS <- c(paste0("u", 1:104), "u900", "NCODI", "anyo")

is_protected <- function(name) name %in% PROTECTED_VARS

# ============================================================
# BLOQUE 0 — NORMALIZACIÓN DE NOMBRES EN standardized_raw
# (Integrado de _ARCHIVADO_4_limpieza_nombres_variables_CL.R)
# Aplica mapeo explícito + fuzzy automático a los TXT en
# STD_RAW_DIR antes de unir módulos por año.
# ============================================================

message("\n=== Bloque 0: Normalización de nombres de variables ===")

if (!dir.exists(STD_RAW_DIR))
  stop("No existe STD_RAW_DIR: ", STD_RAW_DIR, "\nEjecuta primero el script 1.", call. = FALSE)

# --- Parámetros ---
SIMILARITY_THRESHOLD <- 0.82
MIN_YEARS_FOR_FUZZY  <- 2

KEY_VARS_NORM <- c("NCODI", "anyo", "ccaa", "ccaa_codigo",
                   "Depend_agrupada", "cod_depend_agrupada",
                   "Finalidad_agrupada", "cod_finalidad_agrupada")

# ============================================================
# MAPEO EXPLÍCITO (hardcoded)
#
# Casos conocidos donde el fuzzy no es fiable: nombre XML
# y nombre canónico son muy distintos semánticamente.
# Para añadir nuevos casos: añadir filas a este tribble.
# ============================================================

EXPLICIT_MAP <- tribble(
  ~original,                                                                    ~canonical,

  # Año con encoding corrupto
  "a<f1>o",                                                                     "anyo",
  "A<f1>o",                                                                     "anyo",
  "Año",                                                                        "anyo",
  "año",                                                                        "anyo",
  "AÃ±o",                                                                       "anyo",

  # Identificadores geográficos y clasificación (artefactos XML)
  "Cód_x0020_CCAA_x0020__x0028_Todas_x0029_",                                  "ccaa_codigo",
  "Desc_x0020_CCAA_x0020__x0028_Todas_x0029_",                                 "ccaa",
  "Cód_x0020_Grupo_x0020_Finalidad",                                            "cod_finalidad_agrupada",
  "Cod_finalidad_agrupada",                                                      "cod_finalidad_agrupada",
  "cod_finalidad agrupada",                                                      "cod_finalidad_agrupada",
  "FINALIDAD_x0020_agrupada_x0020_para_x0020_Anonimización",                   "Finalidad_agrupada",
  "FINALIDAD agrupada para Anonimizacion",                                      "Finalidad_agrupada",
  "Cód_x0020_Pertenencia_x0020_SNS",                                            "cod_depend_agrupada",
  "Desc_x0020_Pertenencia_x0020_SNS",                                           "Depend_agrupada",
  "Cód_x0020_Grupo_x0020_Finalidad_x0020__x0028_dejar_x0020_sólo_x0020_1_x0020_y_x0020_2_x0029_",
                                                                                 "cod_finalidad_agrupada",

  # Gastos: serie _x0036_
  "_x0036_0_totalCompra",          "G_totalCompra",
  "_x0036_00_producFarma",         "600_producFarma",
  "_x0036_00_1_FarmaAmb",          "600_1_FarmaAmb",
  "_x0036_00_2_FarmaHosp",         "600_2_FarmaHosp",
  "_x0036_01_materialSani",         "601_materialSani",
  "_x0036_01_1_implantes",          "601_1_implantes",
  "_x0036_01_2_restoMateriaSani",   "601_2_restoMateriaSani",
  "_x0036_06_servContratado",       "606_servContratado",
  "_x0036_07_trabajoContratado",    "607_trabajoContratado",
  "_x0036_0X_restoCompras",         "60X_restoCompras",
  "_x0036_1_variaExistencias",      "G_variaExistencias",
  "_x0036_2_servExteriores",        "G_servExteriores",
  "_x0036_28_sumistro",             "628_sumistro",
  "_x0036_2X_restoServiExter",      "62X_restoServiExter",
  "_x0036_4_gastoPersonal",         "G_gastoPersonal",
  "_x0036_40_sueldos",              "640_sueldos",
  "_x0036_41_indemnizacion",        "641_indemnizacion",
  "_x0036_42_SegSocEmpresa",        "642_SegSocEmpresa",
  "_x0036_43_649_OtrGasSocial",     "643_649_OtrGasSocial",
  "_x0036_8_dotaAmortizacion",      "G_dotaAmortizacion",
  "_x0036_9_perdidaDeterioro",      "G_perdidaDeterioro",
  "_x0036_X_restoGasto",            "G_restoGasto",

  # Ingresos: serie _x0037_
  "_x0037_0_tIngresos",                    "I_totIngresosPS",
  "_x0037_00_particular",                  "I_particular",
  "_x0037_01_AsegPriv",                    "I_AsegPriv",
  "_x0037_01_1_AsistSanitaria",            "I_AsistSanitaria",
  "_x0037_01_2_AccTrafic",                 "I_AccTrafic",
  "_x0037_02_MATEPSS",                     "I_MATEPSS",
  "_x0037_04_SNS",                         "I_SNS",
  "_x0037_05_1_FdirectaSS",               "I_FdirectaSS",
  "_x0037_05_2_FdirectaAPriv_MATEPSS",    "I_FdirectaAPriv_MATEPSS",
  "_x0037_06_OyEntiPublica",              "I_OyEntiPublica",
  "_x0037_08_bonificaciones",             "I_bonificaciones",
  "_x0037_09_Otros_Ips",                  "I_Otros_Ips",
  "_x0037_4_Total_Subvencion",            "I_Total_Subvencion",
  "_x0037_40_subvenciones",               "740_subvenciones",
  "_x0037_41_otraSubvencion",             "741_otraSubvencion",
  "_x0037_X_restoIngresos",               "I_restoIngresos",

  # Desglose ingresos por línea (_x0037_ → 70x_)
  "_x0037_0_Hospital",      "70_Hospital",
  "_x0037_0_consulExter",   "70_consulExter",
  "_x0037_0_CMA",           "70_CMA",
  "_x0037_0_hospDia",       "70_hospDia",
  "_x0037_0_Urgencia",      "70_Urgencia",
  "_x0037_0_hospDom",       "70_hospDom",
  "_x0037_00_Hospital",     "700_Hospital",
  "_x0037_00_consulExter",  "700_consulExter",
  "_x0037_00_CMA",          "700_CMA",
  "_x0037_00_hospDia",      "700_hospDia",
  "_x0037_00_Urgencia",     "700_Urgencia",
  "_x0037_00_hospDom",      "700_hospDom",
  "_x0037_01_Hospital",     "701_Hospital",
  "_x0037_01_consulExter",  "701_consulExter",
  "_x0037_01_CMA",          "701_CMA",
  "_x0037_01_hospDia",      "701_hospDia",
  "_x0037_01_Urgencia",     "701_Urgencia",
  "_x0037_01_hospDom",      "701_hospDom",
  "_x0037_01_1Hospital",    "701_1Hospital",
  "_x0037_01_1consulExter", "701_1consulExter",
  "_x0037_01_1CMA",         "701_1CMA",
  "_x0037_01_1hospDia",     "701_1hospDia",
  "_x0037_01_1Urgencia",    "701_1Urgencia",
  "_x0037_01_1hospDom",     "701_1hospDom",
  "_x0037_01_2Hospital",    "701_2Hospital",
  "_x0037_01_2consulExter", "701_2consulExter",
  "_x0037_01_2CMA",         "701_2CMA",
  "_x0037_01_2hospDia",     "701_2hospDia",
  "_x0037_01_2Urgencia",    "701_2Urgencia",
  "_x0037_01_2hospDom",     "701_2hospDom",
  "_x0037_02_Hospital",     "702_Hospital",
  "_x0037_02_consulExter",  "702_consulExter",
  "_x0037_02_CMA",          "702_CMA",
  "_x0037_02_hospDia",      "702_hospDia",
  "_x0037_02_Urgencia",     "702_Urgencia",
  "_x0037_02_hospDom",      "702_hospDom",
  "_x0037_04_Hospital",     "704_Hospital",
  "_x0037_04_consulExter",  "704_consulExter",
  "_x0037_04_CMA",          "704_CMA",
  "_x0037_04_hospDia",      "704_hospDia",
  "_x0037_04_Urgencia",     "704_Urgencia",
  "_x0037_04_hospDom",      "704_hospDom",
  "_x0037_05_1Hospital",    "705_1Hospital",
  "_x0037_05_1consulExter", "705_1consulExter",
  "_x0037_05_1CMA",         "705_1CMA",
  "_x0037_05_1hospDia",     "705_1hospDia",
  "_x0037_05_1Urgencia",    "705_1Urgencia",
  "_x0037_05_1hospDom",     "705_1hospDom",
  "_x0037_05_2Hospital",    "705_2Hospital",
  "_x0037_05_2consulExter", "705_2consulExter",
  "_x0037_05_2CMA",         "705_2CMA",
  "_x0037_05_2hospDia",     "705_2hospDia",
  "_x0037_05_2Urgencia",    "705_2Urgencia",
  "_x0037_05_2hospDom",     "705_2hospDom",
  "_x0037_06_Hospital",     "706_Hospital",
  "_x0037_06_consulExter",  "706_consulExter",
  "_x0037_06_CMA",          "706_CMA",
  "_x0037_06_hospDia",      "706_hospDia",
  "_x0037_06_Urgencia",     "706_Urgencia",
  "_x0037_06_hospDom",      "706_hospDom",
  "_x0037_08_Hospital",     "708_Hospital",
  "_x0037_08_consulExter",  "708_consulExter",
  "_x0037_08_CMA",          "708_CMA",
  "_x0037_08_hospDia",      "708_hospDia",
  "_x0037_08_Urgencia",     "708_Urgencia",
  "_x0037_08_hospDom",      "708_hospDom"
)

# --- Funciones auxiliares de normalización ---
safe_to_utf8 <- function(x, from = "") {
  y <- tryCatch(iconv(x, from = from, to = "UTF-8", sub = ""),
                error = function(e) rep(NA_character_, length(x)))
  y[is.na(y)] <- ""
  y
}

decode_xml_escapes <- function(x) {
  xml_map <- c(
    "_x0020_" = "_", "_x00e1_" = "a", "_x00e9_" = "e",
    "_x00ed_" = "i", "_x00f3_" = "o", "_x00fa_" = "u",
    "_x00f1_" = "ny","_x00c1_" = "A", "_x00c9_" = "E",
    "_x00cd_" = "I", "_x00d3_" = "O", "_x00da_" = "U",
    "_x00d1_" = "NY","_x0028_" = "",  "_x0029_" = "",
    "_x002e_" = "_", "_x002f_" = "_", "_x002d_" = "_"
  )
  for (pat in names(xml_map)) x <- gsub(pat, xml_map[[pat]], x, fixed = TRUE)
  gsub("_x[0-9a-fA-F]{4}_", "", x)
}

normalize_for_compare <- function(x) {
  x <- safe_to_utf8(x)
  x <- decode_xml_escapes(x)
  x <- iconv(x, from = "UTF-8", to = "ASCII//TRANSLIT", sub = "")
  x[is.na(x)] <- ""
  x <- tolower(trimws(x))
  x <- gsub("[^a-z0-9]+", "_", x)
  x <- gsub("_+", "_", x)
  gsub("^_|_$", "", x)
}

clean_varname <- function(x) {
  x <- safe_to_utf8(x)
  x <- decode_xml_escapes(x)
  x <- iconv(x, from = "UTF-8", to = "ASCII//TRANSLIT", sub = "")
  x[is.na(x)] <- ""
  x <- gsub("[^A-Za-z0-9_]+", "_", x)
  x <- gsub("_+", "_", x)
  gsub("^_|_$", "", x)
}

# --- [0/1] Leer cabeceras ---
message("[0/1] Leyendo cabeceras de standardized_raw...")

year_dirs_norm  <- list.dirs(STD_RAW_DIR, full.names = TRUE, recursive = FALSE)
year_names_norm <- basename(year_dirs_norm)
year_dirs_norm  <- year_dirs_norm[grepl("^[0-9]{4}$", year_names_norm)]

if (length(year_dirs_norm) == 0)
  stop("Sin carpetas de años en: ", STD_RAW_DIR, call. = FALSE)

n_years_total_norm <- length(year_dirs_norm)

read_header_info <- function(year_dir) {
  year  <- as.integer(basename(year_dir))
  files <- list.files(year_dir, pattern = "\\.txt$",
                      full.names = TRUE, ignore.case = TRUE)
  if (length(files) == 0) return(NULL)
  purrr::map_dfr(files, function(f) {
    nm        <- tools::file_path_sans_ext(basename(f))
    mod_match <- regmatches(nm, regexpr("^(0?[0-9]{1,2})", nm))
    module_id <- if (length(mod_match) == 1) as.integer(mod_match) else NA_integer_
    hdr <- tryCatch(readLines(f, n = 1, warn = FALSE, encoding = "UTF-8"),
                    error = function(e) character(0))
    if (length(hdr) == 0 || !nzchar(hdr)) return(NULL)
    hdr  <- gsub('"', "", hdr, fixed = TRUE)
    vars <- trimws(strsplit(hdr, ";", fixed = TRUE)[[1]])
    vars <- vars[nzchar(vars)]
    tibble(year, module_id, module_name = nm, file_path = f, var_original = vars)
  })
}

all_headers_norm <- purrr::map_dfr(year_dirs_norm, read_header_info)
message("  Años leídos: ", paste(sort(unique(all_headers_norm$year)), collapse = ", "))
message("  Registros (variable x año x módulo): ", nrow(all_headers_norm))

# --- [0/2] Diagnóstico variables uN (log) ---
message("[0/2] Diagnosticando variables uN...")

u_vars_diag <- all_headers_norm %>%
  filter(grepl("^u[0-9]+$", var_original)) %>%
  group_by(module_id, module_name, year) %>%
  summarise(n_u_vars = n(),
            u_sample = paste(head(sort(var_original), 5), collapse = ", "),
            .groups  = "drop") %>%
  arrange(module_id, year)

if (nrow(u_vars_diag) > 0) {
  message("  *** ATENCIÓN: variables uN en ", nrow(u_vars_diag),
          " archivo(s). Revisar cabeceras en script 1. ***")
  readr::write_csv(u_vars_diag, file.path(INT_DIR, "vars_u_diagnosis.csv"), na = "")
  message("  Diagnóstico guardado en: vars_u_diagnosis.csv")
} else {
  message("  OK: sin variables uN.")
}

# --- [0/3] Diagnóstico presencia por año (log) ---
all_headers_norm <- all_headers_norm %>%
  mutate(
    var_norm    = normalize_for_compare(var_original),
    var_clean   = clean_varname(var_original),
    has_xml     = grepl("_x[0-9a-fA-F]{4}_", var_original, ignore.case = TRUE),
    is_u_var    = grepl("^u[0-9]+$", var_original),
    in_explicit = var_original %in% EXPLICIT_MAP$original
  )

var_years_diag <- all_headers_norm %>%
  filter(!is_u_var) %>%
  distinct(var_norm, var_original, year) %>%
  group_by(var_norm) %>%
  summarise(
    n_years       = n_distinct(year),
    first_year    = min(year),
    last_year     = max(year),
    years_present = paste(sort(unique(year)), collapse = "|"),
    example_name  = first(var_original),
    .groups = "drop"
  ) %>%
  mutate(
    status = case_when(
      n_years == n_years_total_norm            ~ "completa",
      first_year > min(all_headers_norm$year)  ~ "solo_anios_tardios",
      last_year  < max(all_headers_norm$year)  ~ "solo_anios_tempranos",
      TRUE                                     ~ "parcial_intermedia"
    )
  ) %>%
  arrange(status, first_year, var_norm)

readr::write_csv(var_years_diag, file.path(INT_DIR, "vars_new_by_year.csv"), na = "")
message("[0/3] Presencia por año guardada en: vars_new_by_year.csv")

# --- [0/4] Mapeo explícito activo ---
message("[0/4] Preparando mapeo explícito...")

explicit_active <- EXPLICIT_MAP %>%
  filter(original %in% all_headers_norm$var_original,
         original != canonical,
         !(original %in% KEY_VARS_NORM))

message("  Mapeos explícitos activos: ", nrow(explicit_active))
# PROTECCIÓN STATEFUL: el diagnóstico se escribe en var_mapping_diagnostic.csv,
# NO en var_mapping_explicit.csv. Así, re-ejecutar script 2 no altera las reglas
# de renombrado que usa script 1 (que lee el mapeo congelado FROZEN).
diag_out_path <- if (exists("VAR_MAPPING_DIAGNOSTIC_PATH"))
  VAR_MAPPING_DIAGNOSTIC_PATH else file.path(INT_DIR, "var_mapping_diagnostic.csv")
readr::write_csv(explicit_active, diag_out_path, na = "")
message("  Diagnóstico de mapeo guardado en: var_mapping_diagnostic.csv")
message("  (var_mapping_explicit.csv NO se sobreescribe — usar script 0_exportar_mapeo_cnh.R si necesitas regenerarlo)")

# --- [0/5] Fuzzy matching complementario ---
message("[0/5] Detección fuzzy de alias entre años...")

headers_for_fuzzy <- all_headers_norm %>%
  filter(!is_u_var, !in_explicit, !has_xml,
         !(var_original %in% KEY_VARS_NORM))

detect_fuzzy_aliases <- function(df_mod, mod_id) {
  presence <- df_mod %>%
    distinct(var_norm, var_original, var_clean, year) %>%
    group_by(var_norm) %>%
    summarise(n_years      = n_distinct(year),
              years_list   = paste(sort(unique(year)), collapse = "|"),
              var_clean    = first(var_clean),
              var_original = first(var_original),
              .groups = "drop")

  partial <- presence %>%
    filter(n_years < n_years_total_norm, n_years >= MIN_YEARS_FOR_FUZZY)
  if (nrow(partial) <= 1) return(NULL)

  vl <- partial$var_norm
  n  <- length(vl)

  purrr::map_dfr(seq_len(n - 1), function(i) {
    purrr::map_dfr(seq(i + 1, n), function(j) {
      sim <- 1 - stringdist::stringdist(vl[i], vl[j], method = "jw", p = 0.1)
      if (sim < SIMILARITY_THRESHOLD) return(NULL)
      yi <- as.integer(strsplit(partial$years_list[i], "\\|")[[1]])
      yj <- as.integer(strsplit(partial$years_list[j], "\\|")[[1]])
      tibble(
        module_id  = mod_id,
        var_a      = partial$var_original[i],
        clean_a    = partial$var_clean[i],
        years_a    = partial$years_list[i],
        n_yrs_a    = partial$n_years[i],
        var_b      = partial$var_original[j],
        clean_b    = partial$var_clean[j],
        years_b    = partial$years_list[j],
        n_yrs_b    = partial$n_years[j],
        overlap    = length(intersect(yi, yj)),
        similarity = round(sim, 4),
        proposed   = if (partial$n_years[i] >= partial$n_years[j])
                       partial$var_clean[i] else partial$var_clean[j]
      )
    })
  })
}

fuzzy_aliases <- purrr::map_dfr(
  unique(headers_for_fuzzy$module_id[!is.na(headers_for_fuzzy$module_id)]),
  function(mid) detect_fuzzy_aliases(
    headers_for_fuzzy %>% filter(module_id == mid), mid)
)

message("  Pares fuzzy detectados: ", nrow(fuzzy_aliases))

if (nrow(fuzzy_aliases) > 0) {
  fuzzy_aliases <- fuzzy_aliases %>%
    mutate(priority = case_when(
      overlap == 0 & similarity >= 0.92               ~ "probable_alias",
      overlap == 0 & similarity >= SIMILARITY_THRESHOLD ~ "posible_alias",
      overlap >  0                                     ~ "mismo_periodo_NO_alias",
      TRUE                                             ~ "revisar"
    )) %>%
    arrange(priority, desc(similarity))

  readr::write_delim(fuzzy_aliases,
                     file.path(INT_DIR, "var_suspects_review.csv"),
                     delim = ";", na = "")
  message("  Guardado en: var_suspects_review.csv")

  fuzzy_to_apply <- fuzzy_aliases %>%
    filter(priority == "probable_alias") %>%
    transmute(
      original  = as.character(ifelse(proposed == clean_a, var_b, var_a)),
      canonical = as.character(proposed)
    ) %>%
    filter(original != canonical, !(original %in% KEY_VARS_NORM))

  message("  Alias fuzzy que se aplicarán automáticamente: ", nrow(fuzzy_to_apply))
} else {
  fuzzy_to_apply <- tibble(original = character(), canonical = character())
  message("  No se detectaron pares fuzzy.")
}

readr::write_csv(fuzzy_to_apply, file.path(INT_DIR, "var_mapping_fuzzy.csv"), na = "")

# Mapeo final consolidado
var_mapping_final <- bind_rows(
  explicit_active %>% mutate(source = "explicit"),
  fuzzy_to_apply  %>% mutate(source = "fuzzy_auto")
) %>%
  distinct(original, .keep_all = TRUE) %>%
  filter(original != canonical)

message("  Mapeo final: ", nrow(var_mapping_final), " renombrados",
        " (", sum(var_mapping_final$source == "explicit"), " explícitos, ",
        sum(var_mapping_final$source == "fuzzy_auto"), " fuzzy)")

# --- [0/6] Aplicar mapeo a archivos en STD_RAW_DIR ---
message("[0/6] Aplicando mapeo a archivos...")

apply_mapping_to_file <- function(file_path, mapping) {
  df <- tryCatch(
    readr::read_delim(file_path, delim = ";",
                      locale = readr::locale(decimal_mark = ","),
                      show_col_types = FALSE, progress = FALSE,
                      name_repair = "minimal"),
    error = function(e) {
      message("  ERROR leyendo: ", basename(file_path), " — ", conditionMessage(e))
      NULL
    }
  )
  if (is.null(df)) return(invisible(FALSE))

  nms     <- names(df)
  renamed <- 0

  for (i in seq_along(nms)) {
    hit <- mapping$canonical[mapping$original == nms[i]]
    if (length(hit) != 1 || hit == nms[i]) next
    if (hit %in% nms[-i]) {
      nms[i] <- paste0("__DROP__", nms[i])
    } else {
      nms[i] <- hit
      renamed <- renamed + 1
    }
  }

  names(df) <- nms
  drop_cols  <- grep("^__DROP__", names(df), value = TRUE)
  if (length(drop_cols) > 0) df <- df %>% select(-all_of(drop_cols))

  if (renamed > 0 || length(drop_cols) > 0) {
    readr::write_delim(df, file_path, delim = ";", quote = "all")
    message("  [OK] ", basename(file_path),
            " — ", renamed, " renombrado(s), ",
            length(drop_cols), " duplicado(s) eliminado(s)")
  }
  invisible(TRUE)
}

if (nrow(var_mapping_final) > 0) {
  all_std_files <- list.files(STD_RAW_DIR, pattern = "\\.txt$",
                              full.names = TRUE, recursive = TRUE)
  message("  Procesando ", length(all_std_files), " archivos...")
  purrr::walk(all_std_files, apply_mapping_to_file, mapping = var_mapping_final)
  message("  Mapeo aplicado correctamente.")
} else {
  message("  Sin mapeos que aplicar.")
}

message("=== Bloque 0 completado ===\n")

# ============================================================
# PASO 1: UNIÓN DE MÓDULOS POR AÑO
# ============================================================

year_dirs <- fs::dir_ls(STD_RAW_DIR, type = "directory", recurse = FALSE)
years <- year_dirs %>%
  fs::path_file() %>%
  stringr::str_extract("^\\d{4}$") %>%
  stats::na.omit() %>%
  sort() %>%
  as.integer()

if (length(years) == 0) {
  stop("No se encontraron carpetas de año en STD_RAW_DIR: ", STD_RAW_DIR, call. = FALSE)
}

normalize_key_names <- function(df, year) {
  names(df) <- trimws(names(df))

  # Normalizar nombre de año
  year_aliases <- c("Año", "año", "Anyo", "AÃ±o", "year", "ano", "ANO")
  for (alias in year_aliases) {
    names(df)[names(df) == alias] <- "anyo"
  }

  # Buscar NCODI (case-insensitive)
  if (!("NCODI" %in% names(df))) {
    cand <- grep("^NCODI$", names(df), ignore.case = TRUE, value = TRUE)
    if (length(cand) == 1) names(df)[names(df) == cand] <- "NCODI"
  }

  if (!("NCODI" %in% names(df))) return(NULL)

  df$NCODI <- trimws(as.character(df$NCODI))
  df$anyo  <- as.integer(year)
  df
}

# Log global de años
year_log_rows <- list()
# Log de conflictos entre módulos
conflict_rows <- list()

for (year in years) {
  folder <- file.path(STD_RAW_DIR, as.character(year))
  stopifnot(dir.exists(folder))

  files <- list.files(path = folder, pattern = "\\.txt$", full.names = TRUE)
  if (length(files) == 0) {
    warning("Sin archivos para el año ", year, call. = FALSE)
    next
  }

  # Ordenar por nombre para que el "módulo mayor" sea el de índice más alto
  files <- sort(files)

  df_list       <- list()
  source_module <- character(0)   # nombre base de cada módulo

  for (f in files) {
    tmp <- tryCatch(
      readr::read_delim(
        f,
        delim          = ";",
        locale         = locale(decimal_mark = ","),
        show_col_types = FALSE,
        progress       = FALSE,
        name_repair    = "minimal"
      ) %>% as.data.frame(stringsAsFactors = FALSE),
      error = function(e) {
        warning(sprintf("No se pudo leer %s: %s", basename(f), conditionMessage(e)), call. = FALSE)
        NULL
      }
    )
    if (is.null(tmp)) next

    tmp <- normalize_key_names(tmp, year)
    if (is.null(tmp)) {
      warning(sprintf("Archivo sin NCODI (se omite): %s", basename(f)), call. = FALSE)
      next
    }

    # Eliminar columnas duplicadas dentro del mismo módulo
    tmp <- tmp[, !duplicated(names(tmp)), drop = FALSE]

    # Resolver duplicados de clave dentro del módulo
    if (any(duplicated(tmp[, c("NCODI", "anyo")]))) {
      warning(sprintf(
        "Duplicados (NCODI, anyo) en %s. Se conserva la primera fila.", basename(f)
      ), call. = FALSE)
      tmp <- tmp %>% distinct(NCODI, anyo, .keep_all = TRUE)
    }

    attr(tmp, "source_file") <- basename(f)
    df_list[[length(df_list) + 1]] <- tmp
    source_module <- c(source_module, basename(f))
  }

  if (length(df_list) == 0) {
    warning("No hay archivos válidos para el año ", year, call. = FALSE)
    next
  }

  df <- df_list[[1]]

  if (length(df_list) > 1) {
    for (i in 2:length(df_list)) {
      rhs      <- df_list[[i]]
      rhs_name <- attr(rhs, "source_file")

      common_vars <- setdiff(intersect(names(df), names(rhs)), c("NCODI", "anyo"))

      # Hacer el join primero — dplyr alinea filas por (NCODI, anyo) y añade
      # sufijos .x / .y en las columnas comunes.
      df <- full_join(df, rhs, by = c("NCODI", "anyo"), suffix = c(".x", ".y"))

      # Resolver conflictos .x / .y variable por variable
      for (cv in common_vars) {
        cx <- paste0(cv, ".x")
        cy <- paste0(cv, ".y")

        if (!(cx %in% names(df)) || !(cy %in% names(df))) next

        lhs_vals  <- df[[cx]]
        rhs_vals  <- df[[cy]]

        lhs_blank <- is.na(lhs_vals) | (is.character(lhs_vals) & trimws(lhs_vals) == "")
        rhs_blank <- is.na(rhs_vals) | (is.character(rhs_vals) & trimws(rhs_vals) == "")

        n_conflict <- sum(!lhs_blank & !rhs_blank)

        if (n_conflict > 0) {
          conflict_rows[[length(conflict_rows) + 1]] <- data.frame(
            anyo          = year,
            variable      = cv,
            modulo_lhs    = attr(df_list[[i - 1]], "source_file"),
            modulo_rhs    = rhs_name,
            n_conflictos  = n_conflict,
            resolucion    = "se conserva valor del modulo con numero mayor",
            stringsAsFactors = FALSE
          )
        }

        # Resolución: preferir rhs cuando tiene valor; lhs como fallback
        if (!identical(class(lhs_vals), class(rhs_vals))) {
          lhs_vals <- as.character(lhs_vals)
          rhs_vals <- as.character(rhs_vals)
        }
        resolved <- rhs_vals
        resolved[rhs_blank] <- lhs_vals[rhs_blank]

        df[[cv]]  <- resolved
        df[[cx]]  <- NULL
        df[[cy]]  <- NULL
      }
    }
  }

  df_year <- df %>%
    select(anyo, NCODI, everything()) %>%
    arrange(anyo, NCODI)

  n_hospitales <- n_distinct(df_year$NCODI)
  n_cols       <- ncol(df_year)

  message(sprintf(
    "Año %d: %d hospitales, %d columnas, %d módulos",
    year, n_hospitales, n_cols, length(df_list)
  ))

  write.table(
    df_year,
    file.path(LEGACY_BASE_DIR, paste0("df_", year, ".csv")),
    sep       = ",",
    row.names = FALSE,
    col.names = TRUE,
    na        = ""
  )

  year_log_rows[[length(year_log_rows) + 1]] <- data.frame(
    anyo         = year,
    n_hospitales = n_hospitales,
    n_columnas   = n_cols,
    n_modulos    = length(df_list),
    stringsAsFactors = FALSE
  )
}

# Guardar log de conflictos entre módulos
conflict_df <- if (length(conflict_rows) > 0) dplyr::bind_rows(conflict_rows) else
  data.frame(anyo=integer(), variable=character(), modulo_lhs=character(),
             modulo_rhs=character(), n_conflictos=integer(), resolucion=character(),
             stringsAsFactors=FALSE)

write.csv(
  conflict_df,
  file.path(INT_DIR, "log_conflictos_entre_modulos.csv"),
  row.names = FALSE, na = ""
)
message("Conflictos entre módulos registrados: ", nrow(conflict_df))

# ============================================================
# PASO 2: AUDITORÍA DE VARIABLES ENTRE AÑOS
# audit_variables_between_years()
# ============================================================

audit_variables_between_years <- function() {

  message("\n=== Auditoría de variables entre años ===")

  # --- 2.1 Leer todos los df_YYYY.csv ---
  year_files <- list.files(
    path     = LEGACY_BASE_DIR,
    pattern  = "^df_[0-9]{4}\\.csv$",
    full.names = TRUE
  )
  if (length(year_files) == 0) {
    warning("No se encontraron df_YYYY.csv en LEGACY_BASE_DIR.", call. = FALSE)
    return(invisible(NULL))
  }

  years_avail <- as.integer(sub("^df_([0-9]{4})\\.csv$", "\\1", basename(year_files)))
  ord <- order(years_avail)
  year_files  <- year_files[ord]
  years_avail <- years_avail[ord]

  # --- 2.2 Construir matriz de presencia variable × año ---
  presence_list <- list()

  for (idx in seq_along(year_files)) {
    yy  <- years_avail[idx]
    tmp <- tryCatch(
      readr::read_delim(
        year_files[idx],
        delim          = ",",
        col_types      = cols(.default = col_character()),
        show_col_types = FALSE,
        progress       = FALSE,
        name_repair    = "minimal"
      ) %>% as.data.frame(stringsAsFactors = FALSE),
      error = function(e) { warning(conditionMessage(e), call. = FALSE); NULL }
    )
    if (is.null(tmp) || nrow(tmp) == 0) next

    n_rows <- nrow(tmp)
    for (v in names(tmp)) {
      col <- tmp[[v]]
      blank <- is.na(col) | (is.character(col) & trimws(col) == "")
      pct_nonblank <- round(100 * sum(!blank) / n_rows, 2)
      presence_list[[length(presence_list) + 1]] <- data.frame(
        variable = v, anyo = yy, pct_nonblank = pct_nonblank,
        stringsAsFactors = FALSE
      )
    }
  }

  if (length(presence_list) == 0) {
    message("No se pudo construir matriz de presencia.")
    return(invisible(NULL))
  }

  presence_df <- dplyr::bind_rows(presence_list)

  # Pivot: variable × año
  presence_wide <- presence_df %>%
    tidyr::pivot_wider(
      names_from  = anyo,
      values_from = pct_nonblank,
      values_fill = 0
    )

  # Años con datos por variable
  year_cols <- as.character(years_avail)
  year_cols <- intersect(year_cols, names(presence_wide))

  # --- 2.3 Normalización de nombres para similitud ---
  normalize_for_sim <- function(x) {
    x2 <- iconv(x, from = "", to = "ASCII//TRANSLIT", sub = "byte")
    x2[is.na(x2)] <- x
    x2 <- tolower(x2)
    x2 <- gsub("_x[0-9a-f]{4}_", "_", x2)
    x2 <- gsub("[^a-z0-9]+", "_", x2)
    x2 <- gsub("_+", "_", x2)
    x2 <- gsub("^_|_$", "", x2)
    x2
  }

  token_overlap <- function(a, b) {
    ta <- strsplit(a, "_")[[1]]
    tb <- strsplit(b, "_")[[1]]
    ta <- ta[nchar(ta) > 0]
    tb <- tb[nchar(tb) > 0]
    if (length(ta) == 0 || length(tb) == 0) return(0)
    n_common <- length(intersect(ta, tb))
    n_common / max(length(ta), length(tb))
  }

  all_vars <- unique(presence_wide$variable)
  norm_map  <- stats::setNames(normalize_for_sim(all_vars), all_vars)

  # --- 2.4 Detectar pares candidatos ---
  n_total_years <- length(year_cols)

  candidate_pairs <- list()

  # Pre-calcular años presentes por variable
  years_present_map <- list()
  for (i in seq_len(nrow(presence_wide))) {
    v   <- presence_wide$variable[i]
    row <- unlist(presence_wide[i, year_cols])
    years_present_map[[v]] <- names(row)[row > 0]
  }

  all_vars_list <- all_vars
  n_vars <- length(all_vars_list)

  if (n_vars >= 2) {
    for (i in seq_len(n_vars - 1)) {
      va <- all_vars_list[i]
      if (is_protected(va)) next

      ya <- years_present_map[[va]]
      if (length(ya) == 0) next

      na_name <- norm_map[[va]]

      for (j in (i + 1):n_vars) {
        vb <- all_vars_list[j]
        if (is_protected(vb)) next

        yb <- years_present_map[[vb]]
        if (length(yb) == 0) next

        overlap_years <- intersect(ya, yb)
        total_years   <- length(union(ya, yb))
        overlap_pct   <- if (total_years > 0) length(overlap_years) / total_years else 0

        if (overlap_pct > 0.20) next

        nb_name <- norm_map[[vb]]

        jw_sim  <- 1 - stringdist::stringdist(na_name, nb_name, method = "jw", p = 0.1)
        tok_sim <- token_overlap(na_name, nb_name)

        is_candidate <- jw_sim >= 0.82 || tok_sim >= 0.70

        if (!is_candidate) next

        if (length(ya) >= length(yb)) {
          canonical <- va; other <- vb
        } else {
          canonical <- vb; other <- va
        }

        action <- dplyr::case_when(
          jw_sim >= 0.92 && overlap_pct == 0 ~ "merge_auto",
          jw_sim >= 0.82 || (overlap_pct > 0 && overlap_pct <= 0.20) ~ "merge_review",
          TRUE ~ "keep_separate"
        )

        candidate_pairs[[length(candidate_pairs) + 1]] <- data.frame(
          var_a              = va,
          var_b              = vb,
          years_a            = paste(ya, collapse = ","),
          years_b            = paste(yb, collapse = ","),
          n_years_a          = length(ya),
          n_years_b          = length(yb),
          overlap_pct        = round(overlap_pct, 4),
          similarity         = round(jw_sim, 4),
          tok_similarity     = round(tok_sim, 4),
          proposed_canonical = canonical,
          source_module      = "",
          action             = action,
          applied            = FALSE,
          stringsAsFactors   = FALSE
        )
      }
    }
  }

  pairs_df <- if (length(candidate_pairs) > 0)
    dplyr::bind_rows(candidate_pairs)
  else
    data.frame(
      var_a=character(), var_b=character(), years_a=character(), years_b=character(),
      n_years_a=integer(), n_years_b=integer(), overlap_pct=numeric(),
      similarity=numeric(), tok_similarity=numeric(), proposed_canonical=character(),
      source_module=character(), action=character(), applied=logical(),
      stringsAsFactors=FALSE
    )

  message(sprintf(
    "Pares detectados: %d (merge_auto: %d, merge_review: %d, keep_separate: %d)",
    nrow(pairs_df),
    sum(pairs_df$action == "merge_auto"),
    sum(pairs_df$action == "merge_review"),
    sum(pairs_df$action == "keep_separate")
  ))

  # --- 2.5 Aplicar merge_auto ---
  n_auto_applied <- 0L
  auto_pairs <- pairs_df[pairs_df$action == "merge_auto", ]

  if (nrow(auto_pairs) > 0) {
    message("Aplicando ", nrow(auto_pairs), " merges automáticos...")

    for (idx in seq_len(nrow(auto_pairs))) {
      canonical_name <- auto_pairs$proposed_canonical[idx]
      other_name     <- ifelse(
        auto_pairs$var_a[idx] == canonical_name,
        auto_pairs$var_b[idx],
        auto_pairs$var_a[idx]
      )

      years_other <- strsplit(
        ifelse(auto_pairs$var_a[idx] == other_name,
               auto_pairs$years_a[idx],
               auto_pairs$years_b[idx]), ","
      )[[1]]

      for (yy_str in years_other) {
        csv_path <- file.path(LEGACY_BASE_DIR, paste0("df_", trimws(yy_str), ".csv"))
        if (!file.exists(csv_path)) next

        tmp <- tryCatch(
          read.csv(csv_path, stringsAsFactors = FALSE, na.strings = ""),
          error = function(e) NULL
        )
        if (is.null(tmp)) next
        if (!(other_name %in% names(tmp))) next
        if (canonical_name %in% names(tmp)) next

        names(tmp)[names(tmp) == other_name] <- canonical_name

        write.table(tmp, csv_path, sep = ",", row.names = FALSE, na = "")
        n_auto_applied <- n_auto_applied + 1L
      }

      pairs_df$applied[pairs_df$var_a == auto_pairs$var_a[idx] &
                         pairs_df$var_b == auto_pairs$var_b[idx]] <- TRUE
    }

    message("Merges automáticos aplicados: ", n_auto_applied)
  }

  # --- 2.6 Guardar reporte ---
  write.csv(pairs_df, VAR_DUPLICATES_BETWEEN_YEARS_PATH, row.names = FALSE, na = "")
  message("Reporte guardado en: ", VAR_DUPLICATES_BETWEEN_YEARS_PATH)

  invisible(list(pairs = pairs_df, n_auto_applied = n_auto_applied))
}

audit_result <- audit_variables_between_years()

# Exponer el número de merges para el script 3
N_AUTO_MERGES_APPLIED <- if (!is.null(audit_result)) audit_result$n_auto_applied else 0L

message("\n=== Script 2 completado ===")
message("df_YYYY.csv generados para años: ", paste(years, collapse = ", "))
message("Merges automáticos entre años aplicados: ", N_AUTO_MERGES_APPLIED)
