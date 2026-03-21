# ============================================================
# 4_limpieza_nombres_variables.R
#
# Objetivo:
#   Detectar y corregir inconsistencias en nombres de variables
#   entre años dentro de cada módulo SIAE, operando sobre los
#   archivos ya estandarizados por el script 1.
#
# Ejecutar DESPUÉS de: 1_cambiarnombredevariables_2C.R
# Ejecutar ANTES de:   2_seleccionar_var_bucle.R
#
# Problemas que resuelve:
#   1. Artefactos XML en nombres (_x0020_, _x00e9_, _x003N_, etc.)
#   2. Diferencias de mayúsculas/minúsculas y espacios en nombres
#   3. Variables alias entre años (fuzzy matching por módulo)
#   4. Diagnóstico de variables u1...uN (módulos sin cabecera)
#   5. Diagnóstico de variables nuevas (solo en años tardíos)
#   6. Variables all-NA en el panel final
#
# ── INSTRUCCIONES DE USO ──────────────────────────────────────
#
# PRIMERA VEZ (diagnóstico):
#   1. Deja SOLO_DIAGNOSTICO <- TRUE
#   2. Ejecuta el script completo
#   3. Revisa los archivos generados en INT_DIR:
#        vars_u_diagnosis.csv     -> módulos sin cabecera [CRÍTICO]
#        vars_new_by_year.csv     -> variables nuevas/discontinuadas
#        var_suspects_review.csv  -> alias fuzzy para revisar
#        var_mapping_explicit.csv -> mapeo XML que SE APLICARÁ
#        var_mapping_fuzzy.csv    -> mapeo fuzzy que SE APLICARÁ
#   4. Si hay entradas en vars_u_diagnosis.csv, corrígelas en
#      script 1 antes de continuar.
#   5. Si hay alias fuzzy incorrectos en var_suspects_review.csv,
#      añade los correctos a EXPLICIT_MAP y marca los incorrectos.
#
# SEGUNDA VEZ (aplicar cambios):
#   1. Cambia SOLO_DIAGNOSTICO <- FALSE
#   2. Ejecuta el script completo
#   3. Continúa con script 2 y script 3
#
# Salidas en INT_DIR/:
#   var_mapping_explicit.csv     -> mapeo hardcoded
#   var_mapping_fuzzy.csv        -> alias fuzzy detectados
#   var_suspects_review.csv      -> casos dudosos para revisión manual
#   vars_u_diagnosis.csv         -> diagnóstico de variables uN
#   vars_new_by_year.csv         -> variables nuevas/discontinuadas
#   vars_all_na.txt              -> variables all-NA (si df_final existe)
# ============================================================

# ── MODO DE EJECUCIÓN ─────────────────────────────────────────
# TRUE  = solo diagnóstico, NO modifica ningún archivo
# FALSE = aplica el mapeo y modifica los archivos en standardized_raw
SOLO_DIAGNOSTICO <- TRUE
# ─────────────────────────────────────────────────────────────

config_path <- if (file.exists("scripts/00_config.R")) "scripts/00_config.R" else "00_config.R"
source(config_path)

# ── Paquetes ──────────────────────────────────────────────────────────────────
required_pkgs <- c("dplyr", "readr", "stringr", "stringdist", "tidyr", "purrr")
missing_pkgs  <- required_pkgs[!vapply(required_pkgs, requireNamespace,
                                       logical(1), quietly = TRUE)]
if (length(missing_pkgs) > 0) install.packages(missing_pkgs, dependencies = TRUE)
invisible(lapply(required_pkgs, library, character.only = TRUE))

STD_RAW_DIR <- file.path(INT_DIR, "standardized_raw")
if (!dir.exists(STD_RAW_DIR))
  stop("No existe STD_RAW_DIR: ", STD_RAW_DIR, "\nEjecuta primero el script 1.")
dir.create(INT_DIR, recursive = TRUE, showWarnings = FALSE)

if (SOLO_DIAGNOSTICO) {
  message("\n*** MODO DIAGNÓSTICO activado — ningún archivo será modificado ***\n")
} else {
  message("\n*** MODO APLICAR — los archivos en standardized_raw serán modificados ***\n")
}

# ── Parámetros ────────────────────────────────────────────────────────────────
SIMILARITY_THRESHOLD <- 0.82
MIN_YEARS_FOR_FUZZY  <- 2

KEY_VARS <- c("NCODI", "anyo", "ccaa", "ccaa_codigo",
              "Depend_agrupada", "cod_depend_agrupada",
              "Finalidad_agrupada", "cod_finalidad_agrupada")

# ============================================================
# MAPEO EXPLÍCITO (hardcoded)
#
# Casos conocidos donde el fuzzy no es fiable: nombre XML
# y nombre canónico son muy distintos semánticamente.
# Fuente: análisis del encabezado real de df_final.
#
# Para añadir nuevos casos tras la revisión manual:
# añadir filas a este tribble y volver a ejecutar con
# SOLO_DIAGNOSTICO <- FALSE.
# ============================================================

EXPLICIT_MAP <- tribble(
  ~original,                                                                    ~canonical,

  # ── Año con encoding corrupto ──────────────────────────────────────────────
  "a<f1>o",                                                                     "anyo",
  "A<f1>o",                                                                     "anyo",
  "Año",                                                                        "anyo",
  "año",                                                                        "anyo",
  "AÃ±o",                                                                      "anyo",

  # ── Identificadores geográficos y clasificación (artefactos XML) ───────────
  "Cód_x0020_CCAA_x0020__x0028_Todas_x0029_",                                 "ccaa_codigo",
  "Desc_x0020_CCAA_x0020__x0028_Todas_x0029_",                                "ccaa",
  "Cód_x0020_Grupo_x0020_Finalidad",                                           "cod_finalidad_agrupada",
  "Cod_finalidad_agrupada",                                                     "cod_finalidad_agrupada",
  "cod_finalidad agrupada",                                                     "cod_finalidad_agrupada",
  "FINALIDAD_x0020_agrupada_x0020_para_x0020_Anonimización",                  "Finalidad_agrupada",
  "FINALIDAD agrupada para Anonimizacion",                                     "Finalidad_agrupada",
  "Cód_x0020_Pertenencia_x0020_SNS",                                           "cod_depend_agrupada",
  "Desc_x0020_Pertenencia_x0020_SNS",                                          "Depend_agrupada",
  "Cód_x0020_Grupo_x0020_Finalidad_x0020__x0028_dejar_x0020_sólo_x0020_1_x0020_y_x0020_2_x0029_",
                                                                                "cod_finalidad_agrupada",

  # ── Gastos: serie _x0036_ ─────────────────────────────────────────────────
  "_x0036_0_totalCompra",          "G_totalCompra",
  "_x0036_00_producFarma",         "600_producFarma",
  "_x0036_00_1_FarmaAmb",         "600_1_FarmaAmb",
  "_x0036_00_2_FarmaHosp",        "600_2_FarmaHosp",
  "_x0036_01_materialSani",        "601_materialSani",
  "_x0036_01_1_implantes",         "601_1_implantes",
  "_x0036_01_2_restoMateriaSani",  "601_2_restoMateriaSani",
  "_x0036_06_servContratado",      "606_servContratado",
  "_x0036_07_trabajoContratado",   "607_trabajoContratado",
  "_x0036_0X_restoCompras",        "60X_restoCompras",
  "_x0036_1_variaExistencias",     "G_variaExistencias",
  "_x0036_2_servExteriores",       "G_servExteriores",
  "_x0036_28_sumistro",            "628_sumistro",
  "_x0036_2X_restoServiExter",     "62X_restoServiExter",
  "_x0036_4_gastoPersonal",        "G_gastoPersonal",
  "_x0036_40_sueldos",             "640_sueldos",
  "_x0036_41_indemnizacion",       "641_indemnizacion",
  "_x0036_42_SegSocEmpresa",       "642_SegSocEmpresa",
  "_x0036_43_649_OtrGasSocial",    "643_649_OtrGasSocial",
  "_x0036_8_dotaAmortizacion",     "G_dotaAmortizacion",
  "_x0036_9_perdidaDeterioro",     "G_perdidaDeterioro",
  "_x0036_X_restoGasto",           "G_restoGasto",

  # ── Ingresos: serie _x0037_ ───────────────────────────────────────────────
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

  # ── Desglose ingresos por línea (_x0037_ → 70x_) ─────────────────────────
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

# ── Utilidades ────────────────────────────────────────────────────────────────

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

# ── [1/6] Lectura de cabeceras ────────────────────────────────────────────────

message("[1/6] Leyendo cabeceras de standardized_raw...")

year_dirs  <- list.dirs(STD_RAW_DIR, full.names = TRUE, recursive = FALSE)
year_names <- basename(year_dirs)
year_dirs  <- year_dirs[grepl("^[0-9]{4}$", year_names)]
if (length(year_dirs) == 0) stop("Sin carpetas de años en: ", STD_RAW_DIR)

n_years_total <- length(year_dirs)

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

all_headers <- purrr::map_dfr(year_dirs, read_header_info)
message("  Años leídos:  ", paste(sort(unique(all_headers$year)), collapse = ", "))
message("  Total registros (variable x año x módulo): ", nrow(all_headers))

# ── [2/6] Diagnóstico variables uN ───────────────────────────────────────────

message("\n[2/6] Diagnosticando variables uN (módulos sin cabecera reconocida)...")

u_vars <- all_headers %>%
  filter(grepl("^u[0-9]+$", var_original)) %>%
  group_by(module_id, module_name, year) %>%
  summarise(n_u_vars = n(),
            u_sample = paste(head(sort(var_original), 5), collapse = ", "),
            .groups  = "drop") %>%
  arrange(module_id, year)

if (nrow(u_vars) > 0) {
  message("  *** CRÍTICO: variables uN en ", nrow(u_vars), " archivo(s) ***")
  message("  Estos archivos no tienen cabecera reconocible.")
  message("  Deben corregirse en script 1 antes de continuar.")
  print(u_vars)
  readr::write_csv(u_vars, file.path(INT_DIR, "vars_u_diagnosis.csv"), na = "")
  message("  Diagnóstico guardado en: vars_u_diagnosis.csv")
} else {
  message("  OK: sin variables uN.")
}

# ── [3/6] Diagnóstico de variables por año ────────────────────────────────────

message("\n[3/6] Diagnosticando variables nuevas o discontinuadas por año...")

all_headers <- all_headers %>%
  mutate(
    var_norm    = normalize_for_compare(var_original),
    var_clean   = clean_varname(var_original),
    has_xml     = grepl("_x[0-9a-fA-F]{4}_", var_original, ignore.case = TRUE),
    is_u_var    = grepl("^u[0-9]+$", var_original),
    in_explicit = var_original %in% EXPLICIT_MAP$original
  )

var_years <- all_headers %>%
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
      n_years == n_years_total           ~ "completa",
      first_year > min(all_headers$year) ~ "solo_años_tardios",
      last_year  < max(all_headers$year) ~ "solo_años_tempranos",
      TRUE                               ~ "parcial_intermedia"
    )
  ) %>%
  arrange(status, first_year, var_norm)

readr::write_csv(var_years, file.path(INT_DIR, "vars_new_by_year.csv"), na = "")
message("  Completas (todos los años):   ", sum(var_years$status == "completa"))
message("  Solo años tardíos (nuevas):   ", sum(var_years$status == "solo_años_tardios"))
message("  Solo años tempranos (caídas): ", sum(var_years$status == "solo_años_tempranos"))
message("  Parciales intermedias:        ", sum(var_years$status == "parcial_intermedia"))
message("  Guardado en: vars_new_by_year.csv")

# ── [4/6] Mapeo explícito ─────────────────────────────────────────────────────

message("\n[4/6] Preparando mapeo explícito...")

explicit_active <- EXPLICIT_MAP %>%
  filter(original %in% all_headers$var_original,
         original != canonical,
         !(original %in% KEY_VARS))

message("  Mapeos explícitos activos: ", nrow(explicit_active))
readr::write_csv(explicit_active,
                 file.path(INT_DIR, "var_mapping_explicit.csv"), na = "")
message("  Guardado en: var_mapping_explicit.csv")

# ── [5/6] Fuzzy matching complementario ──────────────────────────────────────

message("\n[5/6] Detección fuzzy de alias entre años...")

headers_for_fuzzy <- all_headers %>%
  filter(!is_u_var, !in_explicit, !has_xml,
         !(var_original %in% KEY_VARS))

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
    filter(n_years < n_years_total, n_years >= MIN_YEARS_FOR_FUZZY)
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
      overlap == 0 & similarity >= 0.92            ~ "probable_alias",
      overlap == 0 & similarity >= SIMILARITY_THRESHOLD ~ "posible_alias",
      overlap >  0                                 ~ "mismo_periodo_NO_alias",
      TRUE                                         ~ "revisar"
    )) %>%
    arrange(priority, desc(similarity))

  readr::write_delim(fuzzy_aliases,
                     file.path(INT_DIR, "var_suspects_review.csv"),
                     delim = ";", na = "")
  message("  Guardado en: var_suspects_review.csv")
  message("  Prioridades:")
  message("    probable_alias       -> se aplicarán automáticamente")
  message("    posible_alias        -> revisar manualmente")
  message("    mismo_periodo_NO_alias -> probablemente variables distintas")

  fuzzy_to_apply <- fuzzy_aliases %>%
    filter(priority == "probable_alias") %>%
    transmute(
      original  = as.character(ifelse(proposed == clean_a, var_b, var_a)),
      canonical = as.character(proposed)
    ) %>%
    filter(original != canonical, !(original %in% KEY_VARS))

  message("  Alias fuzzy que se aplicarán automáticamente: ",
          nrow(fuzzy_to_apply))
} else {
  fuzzy_to_apply <- tibble(original = character(), canonical = character())
  message("  No se detectaron pares fuzzy.")
}

readr::write_csv(fuzzy_to_apply,
                 file.path(INT_DIR, "var_mapping_fuzzy.csv"), na = "")

# ── Mapeo final consolidado ───────────────────────────────────────────────────

var_mapping_final <- bind_rows(
  explicit_active %>% mutate(source = "explicit"),
  fuzzy_to_apply  %>% mutate(source = "fuzzy_auto")
) %>%
  distinct(original, .keep_all = TRUE) %>%
  filter(original != canonical)

message("\n  MAPEO FINAL: ", nrow(var_mapping_final), " renombrados en total")
message("    Explícitos: ", sum(var_mapping_final$source == "explicit"))
message("    Fuzzy auto: ", sum(var_mapping_final$source == "fuzzy_auto"))

# ── [6/6] Aplicar mapeo (solo si SOLO_DIAGNOSTICO = FALSE) ───────────────────

message("\n[6/6] Aplicación del mapeo...")

if (SOLO_DIAGNOSTICO) {

  message("  MODO DIAGNÓSTICO: ningún archivo ha sido modificado.")
  message("")
  message("  PRÓXIMOS PASOS:")
  message("  1. Revisa vars_u_diagnosis.csv  -> corregir cabeceras en script 1")
  message("  2. Revisa var_suspects_review.csv -> añadir alias válidos a EXPLICIT_MAP")
  message("  3. Revisa vars_new_by_year.csv  -> decidir alias vs variable nueva")
  message("  4. Cuando estés conforme, cambia SOLO_DIAGNOSTICO <- FALSE")
  message("     y vuelve a ejecutar este script.")

} else {

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
        # El destino ya existe: marcar el duplicado para eliminar
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

  all_files <- list.files(STD_RAW_DIR, pattern = "\\.txt$",
                          full.names = TRUE, recursive = TRUE)

  message("  Procesando ", length(all_files), " archivos...")
  purrr::walk(all_files, apply_mapping_to_file, mapping = var_mapping_final)
  message("  Mapeo aplicado correctamente.")

  # Variables all-NA en df_final (solo tiene sentido tras scripts 2 y 3)
  message("\n  Buscando variables all-NA en df_final...")
  if (file.exists(DF_FINAL_RDATA_PATH)) {
    load(DF_FINAL_RDATA_PATH)
    if (exists("df_final")) {
      all_na <- names(df_final)[sapply(df_final, function(x) all(is.na(x)))]
      message("  Variables all-NA: ", length(all_na))
      if (length(all_na) > 0) {
        readr::write_lines(all_na, file.path(INT_DIR, "vars_all_na.txt"))
        message("  Listado en: vars_all_na.txt")
        message("  Para eliminarlas del panel ejecuta:")
        message('  df_final <- df_final %>%')
        message('    select(-any_of(readLines("', file.path(INT_DIR, "vars_all_na.txt"), '")))')
      }
    }
  } else {
    message("  df_final no encontrado.")
    message("  Ejecuta scripts 2 y 3, luego vuelve a correr este bloque.")
  }

  message("\n  SIGUIENTE PASO: ejecutar 2_seleccionar_var_bucle.R")
}

# ── Resumen final ─────────────────────────────────────────────────────────────

message("\n", strrep("=", 60))
message("RESUMEN")
message(strrep("=", 60))
message("Modo:                 ",
        if (SOLO_DIAGNOSTICO) "DIAGNÓSTICO (sin cambios)" else "APLICAR (archivos modificados)")
message("Mapeos preparados:    ", nrow(var_mapping_final))
message("  Explícitos:         ", sum(var_mapping_final$source == "explicit"))
message("  Fuzzy automáticos:  ", sum(var_mapping_final$source == "fuzzy_auto"))
message("")
message("Archivos en: ", INT_DIR)
message("  vars_u_diagnosis.csv     -> módulos sin cabecera [CRÍTICO si no está vacío]")
message("  vars_new_by_year.csv     -> variables nuevas/discontinuadas")
message("  var_suspects_review.csv  -> alias fuzzy para revisión manual")
message("  var_mapping_explicit.csv -> mapeo XML preparado")
message("  var_mapping_fuzzy.csv    -> mapeo fuzzy preparado")
message(strrep("=", 60))
