# ============================================================
# 10_depuracion_df_completo.R
# Construye variables de insumo para el análisis SFA sobre df_final.
#
# PASO A — D_desc (estructura organizativa) — TODO pendiente
# PASO B — Pago (proporciones por régimen: sns, privado, mutuas,
#           ingresos monetarios SNS)
# PASO C — L (inputs trabajo): L_total, L_medico, L_quirur
# PASO D — K (inputs capital): K_camas, K_tech_index,
#           K_quirofanos, K_hemo
# PASO E — Log-transformaciones
# PASO F — Diagnóstico por anyo
# PASO G — Guardar df_final
#
# Notas de verificación previa (2025-03-20):
#   - I_FdirectaSS ausente en df_final; pct_ingr_SNS usa solo I_SNS
#   - I_totIngresos es el denominador de ingresos monetarios
#   - total_pMedicos, subMedicas_pTotal, etc. existen (pct_NA<1%)
#   - TAC_hospital, RNM_hospital, PET_hospital confirmados
# ============================================================

config_path <- if (file.exists("scripts/00_config.R")) "scripts/00_config.R" else "00_config.R"
source(config_path)

required_pkgs <- c("dplyr")
missing_pkgs  <- required_pkgs[!vapply(required_pkgs, requireNamespace,
                                       logical(1), quietly = TRUE)]
if (length(missing_pkgs) > 0)
  stop("Faltan paquetes: ", paste(missing_pkgs, collapse = ", "),
       "\nInstálalos con install.packages().", call. = FALSE)

library(dplyr)

PROTECTED_VARS <- c(paste0("u", 1:104), "u900", "NCODI", "anyo")

# ============================================================
# 1. Cargar df_final
# ============================================================

message("\n=== Script 10: Variables SFA (D_desc, Pago, L, K) ===")

if (!file.exists(DF_FINAL_RDATA_PATH))
  stop("No se encontró df_final en: ", DF_FINAL_RDATA_PATH,
       "\nEjecuta el script 9 antes.", call. = FALSE)

load(DF_FINAL_RDATA_PATH)
if (!exists("df_final"))
  stop("Objeto df_final no encontrado en ", DF_FINAL_RDATA_PATH, call. = FALSE)

message("df_final cargado: ", nrow(df_final), " filas x ", ncol(df_final), " columnas")
message("Años disponibles: ", min(df_final$anyo, na.rm = TRUE),
        "-", max(df_final$anyo, na.rm = TRUE))

df <- df_final

# Función auxiliar: coerce a numeric silenciosamente
to_num <- function(x) suppressWarnings(as.numeric(x))

# ============================================================
# PASO A — D_desc (estructura organizativa de dependencia)
# ============================================================

message("\n--- PASO A: D_desc (tabla cod_depend_agrupada) ---")

# Imprimir tabla de frecuencias para revisar los códigos
if ("cod_depend_agrupada" %in% names(df)) {
  message("  Tabla cod_depend_agrupada:")
  tbl_cod <- table(df$cod_depend_agrupada, useNA = "always")
  print(tbl_cod)
} else {
  warning("'cod_depend_agrupada' no encontrada en df_final.", call. = FALSE)
}

if ("Depend_agrupada" %in% names(df)) {
  message("  Tabla Depend_agrupada:")
  tbl_dep <- table(df$Depend_agrupada, useNA = "always")
  print(tbl_dep)
}

# TODO: completar tras revisar tabla de cod_depend_agrupada
# D_desc <- dplyr::case_when(
#   df$cod_depend_agrupada %in% c(????) ~ 0L,  # centralizado SNS
#   df$cod_depend_agrupada %in% c(????) ~ 1L,  # descentralizado / privado
#   TRUE ~ NA_integer_
# )
# df$D_desc <- D_desc

message("  D_desc: pendiente de completar (ver TODO en el código).")

# ============================================================
# PASO B — Variable Pago
# ============================================================

message("\n--- PASO B: Variables de pago (proporciones por régimen) ---")

# Verificar altTotal_bruto
if (!("altTotal_bruto" %in% names(df)))
  stop("'altTotal_bruto' no existe. Ejecuta script 9 antes.", call. = FALSE)

denom_altas <- to_num(df$altTotal_bruto)
denom_altas[!is.na(denom_altas) & denom_altas <= 0] <- NA_real_

# Proporciones de altas por régimen (C1_18)
# Fuente: módulo SIAE C1_18_ACTIVIDAD_POR_REGIMEN
df$pct_sns <- ifelse(!is.na(denom_altas),
                     to_num(df$altas_sns) / denom_altas, NA_real_)

df$pct_privado <- ifelse(
  !is.na(denom_altas),
  (to_num(df$altas_AsegPriv) + to_num(df$altas_particular)) / denom_altas,
  NA_real_
)

df$pct_mutuas <- ifelse(!is.na(denom_altas),
                         to_num(df$altas_MutuaFun) / denom_altas, NA_real_)

message("  pct_sns     — media: ", round(mean(df$pct_sns, na.rm = TRUE), 3))
message("  pct_privado — media: ", round(mean(df$pct_privado, na.rm = TRUE), 3))
message("  pct_mutuas  — media: ", round(mean(df$pct_mutuas, na.rm = TRUE), 3))

# Proporción de ingresos monetarios del SNS (C1_20)
# Fuente: módulo SIAE C1_20_INGRESOS
# Nota: I_FdirectaSS (cuenta 705.1) no encontrada en df_final;
# se usa solo I_SNS (cuenta 704). Ajustar si el módulo 20
# se reconstruye con datos completos.
if ("I_totIngresos" %in% names(df)) {
  denom_ingr <- to_num(df$I_totIngresos)
  denom_ingr[!is.na(denom_ingr) & denom_ingr <= 0] <- NA_real_
} else if ("I_totIngresosPS" %in% names(df)) {
  # Fallback: usar ingresos por prestación de servicios
  denom_ingr <- to_num(df$I_totIngresosPS)
  denom_ingr[!is.na(denom_ingr) & denom_ingr <= 0] <- NA_real_
  message("  AVISO: usando I_totIngresosPS como denominador de ingresos.")
} else {
  denom_ingr <- NA_real_
  warning("Denominador de ingresos monetarios no encontrado.", call. = FALSE)
}

# Numerador: I_SNS + I_FdirectaSS (si existe)
num_sns_ingr <- to_num(df$I_SNS)
if ("I_FdirectaSS" %in% names(df)) {
  num_sns_ingr <- num_sns_ingr + to_num(df$I_FdirectaSS)
  message("  I_FdirectaSS incluida en pct_ingr_SNS.")
} else {
  message("  I_FdirectaSS ausente; pct_ingr_SNS usa solo I_SNS.")
}

df$pct_ingr_SNS <- ifelse(!is.na(denom_ingr),
                            num_sns_ingr / denom_ingr, NA_real_)

message("  pct_ingr_SNS — media: ", round(mean(df$pct_ingr_SNS, na.rm = TRUE), 3),
        " | pct_NA: ", round(100 * mean(is.na(df$pct_ingr_SNS)), 1), "%")

# ============================================================
# PASO C — Input trabajo (L)
# ============================================================

message("\n--- PASO C: Input trabajo (L) ---")

# Excluir _colab (colaboradores externos — no bajo contrato del hospital).
# Fórmula: jornada completa (_cTotal) + 0.5 × jornada parcial (_pTotal)
# Fuente: módulo SIAE C1_10_PERSONAL

check_pTotal <- function(varname) {
  if (varname %in% names(df)) {
    to_num(df[[varname]])
  } else {
    message("  AVISO: '", varname, "' no encontrada. Se usa 0.")
    rep(0, nrow(df))
  }
}

total_cMed <- to_num(df$total_cMedicos)
total_pMed <- check_pTotal("total_pMedicos")
due_cT     <- to_num(df$due_cTotal)
due_pT     <- check_pTotal("due_pTotal")
subMed_cT  <- to_num(df$subMedicas_cTotal)
subMed_pT  <- check_pTotal("subMedicas_pTotal")
subQui_cT  <- to_num(df$subQuirurgicas_cTotal)
subQui_pT  <- check_pTotal("subQuirurgicas_pTotal")

# L_total: todos los médicos + enfermería (jornada completa + 0.5 parcial)
df$L_total <- (total_cMed + 0.5 * total_pMed) +
              (due_cT    + 0.5 * due_pT)

# L_medico: especialistas médicos (excl. quirúrgicos, para Diseño B)
df$L_medico <- subMed_cT + 0.5 * subMed_pT

# L_quirur: especialistas quirúrgicos (para Diseño B frontera quirúrgica)
df$L_quirur <- subQui_cT + 0.5 * subQui_pT

# Marcar NA si TODOS los componentes son NA en esa fila
# (evitar que un hospital con datos 0 se trate como sin datos)
all_na_L <- is.na(to_num(df$total_cMedicos)) &
            is.na(to_num(df$due_cTotal))
df$L_total[all_na_L]  <- NA_real_
df$L_medico[is.na(to_num(df$subMedicas_cTotal))]     <- NA_real_
df$L_quirur[is.na(to_num(df$subQuirurgicas_cTotal))] <- NA_real_

message("  L_total  — media: ", round(mean(df$L_total,  na.rm = TRUE), 1),
        " | pct_NA: ", round(100 * mean(is.na(df$L_total)), 1), "%")
message("  L_medico — media: ", round(mean(df$L_medico, na.rm = TRUE), 1))
message("  L_quirur — media: ", round(mean(df$L_quirur, na.rm = TRUE), 1))

# ============================================================
# PASO D — Input capital (K)
# ============================================================

message("\n--- PASO D: Input capital (K) ---")

# K_camas: camas en funcionamiento (proxy de capital hospitalario)
# Fuente: C1_03_DOTACION_HOSPITAL
df$K_camas <- to_num(df$camas_funcionamiento)

# K_tech_index: índice de dotación tecnológica de alta complejidad
# Fuente: C1_04_DOTACION_TECNOLOGICA
# NA → 0 antes de sumar (hospital no reporta = ausencia de esa tecnología)
# Búsqueda por grep (robustez ante renombrados del pipeline)
find_col <- function(pattern, df_names) {
  hits <- grep(pattern, df_names, ignore.case = TRUE, value = TRUE)
  if (length(hits) == 0) return(NULL)
  hits[1]   # usar la primera coincidencia
}

tech_vars_map <- list(
  TAC          = find_col("^TAC_hospital$", names(df)),
  RNM          = find_col("^RNM_hospital$", names(df)),
  PET          = find_col("^PET_hospital$", names(df)),
  acelerador   = find_col("^acelerador_hospital$", names(df)),
  angiografo   = find_col("^angiografo_hospital$", names(df)),
  gammacamara  = find_col("^gammacamara_hospital$", names(df)),
  spect        = find_col("^spect_hospital$", names(df))
)

tech_vars_found <- Filter(Negate(is.null), tech_vars_map)
tech_vars_missing <- names(tech_vars_map)[vapply(tech_vars_map, is.null, logical(1))]

if (length(tech_vars_missing) > 0)
  message("  Tecnologías no encontradas: ", paste(tech_vars_missing, collapse = ", "))
message("  Tecnologías usadas: ", paste(names(tech_vars_found), collapse = ", "))

if (length(tech_vars_found) > 0) {
  mat_tech <- sapply(unlist(tech_vars_found), function(v) {
    x <- to_num(df[[v]])
    x[is.na(x)] <- 0    # NA → 0: hospital no tiene esa tecnología
    x
  })
  if (!is.matrix(mat_tech)) mat_tech <- matrix(mat_tech, ncol = 1)
  df$K_tech_index <- rowSums(mat_tech)
} else {
  warning("No se encontraron variables de tecnología. K_tech_index = NA.", call. = FALSE)
  df$K_tech_index <- NA_real_
}

# K_quirofanos y K_hemo: capital quirúrgico específico (para Diseño B)
# Fuente: C1_03_DOTACION_HOSPITAL
df$K_quirofanos <- to_num(df$quirofanos_funcionamiento)
df$K_hemo       <- to_num(df$salasHemo_funcionamiento)

message("  K_camas     — media: ", round(mean(df$K_camas,     na.rm = TRUE), 1),
        " | pct_NA: ", round(100 * mean(is.na(df$K_camas)), 1), "%")
message("  K_tech_index— media: ", round(mean(df$K_tech_index, na.rm = TRUE), 1),
        " | pct_NA: ", round(100 * mean(is.na(df$K_tech_index)), 1), "%")
message("  K_quirofanos— media: ", round(mean(df$K_quirofanos, na.rm = TRUE), 1))
message("  K_hemo      — media: ", round(mean(df$K_hemo,       na.rm = TRUE), 1))

# ============================================================
# PASO E — Log-transformaciones
# ============================================================

message("\n--- PASO E: Log-transformaciones ---")

# log(x + 1) para evitar log(0) cuando hay hospitales con 0 trabajadores
df$ln_L_total   <- log(pmax(df$L_total,   0, na.rm = FALSE) + 1)
df$ln_L_medico  <- log(pmax(df$L_medico,  0, na.rm = FALSE) + 1)
df$ln_L_quirur  <- log(pmax(df$L_quirur,  0, na.rm = FALSE) + 1)
df$ln_K_camas   <- log(pmax(df$K_camas,   0, na.rm = FALSE) + 1)
df$ln_K_tech    <- log(pmax(df$K_tech_index, 0, na.rm = FALSE) + 1)
df$ln_K_quirof  <- log(pmax(df$K_quirofanos, 0, na.rm = FALSE) + 1)

# NA se propaga correctamente: log(NA + 1) = NA
for (v in c("ln_L_total","ln_L_medico","ln_L_quirur",
            "ln_K_camas","ln_K_tech","ln_K_quirof")) {
  message(sprintf("  %-14s pct_NA=%.1f%%", v,
                  round(100 * mean(is.na(df[[v]])), 1)))
}

# ============================================================
# PASO F — Diagnóstico
# ============================================================

message("\n--- PASO F: Diagnóstico por anyo ---")

# Variables SFA nuevas
vars_sfa_new <- c("pct_sns","pct_privado","pct_mutuas","pct_ingr_SNS",
                  "L_total","L_medico","L_quirur",
                  "K_camas","K_tech_index","K_quirofanos","K_hemo",
                  "ln_L_total","ln_L_medico","ln_L_quirur",
                  "ln_K_camas","ln_K_tech","ln_K_quirof")

diag_rows <- lapply(sort(unique(df$anyo)), function(yr) {
  d_yr <- df[df$anyo == yr, ]
  dplyr::bind_rows(lapply(vars_sfa_new, function(v) {
    if (!(v %in% names(d_yr))) return(NULL)
    x <- to_num(d_yr[[v]])
    data.frame(
      anyo      = yr,
      variable  = v,
      n_validos = sum(!is.na(x)),
      pct_NA    = round(100 * mean(is.na(x)), 1),
      media     = round(mean(x, na.rm = TRUE), 4),
      sd        = round(sd(x, na.rm = TRUE), 4),
      stringsAsFactors = FALSE
    )
  }))
})

diag_df <- dplyr::bind_rows(diag_rows)
write.csv(diag_df, file.path(INT_DIR, "variables_sfa_diagnostico.csv"),
          row.names = FALSE, na = "")
message("  Diagnóstico guardado en: variables_sfa_diagnostico.csv")

# Tabla pct_sns por anyo (para detectar tendencias temporales)
message("\n  pct_sns por anyo (media):")
pct_sns_yr <- tapply(df$pct_sns, df$anyo, mean, na.rm = TRUE)
print(round(pct_sns_yr, 3))

# ============================================================
# PASO G — Guardar df_final
# ============================================================

message("\n--- PASO G: Guardar ---")

vars_nuevas <- c("pct_sns","pct_privado","pct_mutuas","pct_ingr_SNS",
                 "L_total","L_medico","L_quirur",
                 "K_camas","K_tech_index","K_quirofanos","K_hemo",
                 "ln_L_total","ln_L_medico","ln_L_quirur",
                 "ln_K_camas","ln_K_tech","ln_K_quirof")

# Sobrescribir si ya existen (re-ejecución segura)
df_final[vars_nuevas] <- df[vars_nuevas]

# Verificar unicidad de clave
n_dup <- sum(duplicated(df_final[, c("NCODI", "anyo")]))
if (n_dup > 0)
  stop("Duplicados (NCODI, anyo) en df_final: ", n_dup, call. = FALSE)

save(df_final, file = DF_FINAL_RDATA_PATH)
message("  df_final guardado en: ", DF_FINAL_RDATA_PATH)
message("  Filas: ", nrow(df_final), " | Columnas: ", ncol(df_final))

message("\n=== Script 10 completado ===")
