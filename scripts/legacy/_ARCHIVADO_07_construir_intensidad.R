# ============================================================
# 14_creacion_intensidad.R
# Construye índices de intensidad diagnóstico-terapéutica.
#
# VERSIÓN 3 — Bridges corregidos usando C1_16 (procedimientos)
# y NO variables de C1_04 (dotación de equipos).
#
# Cambio estructural SIAE 2021:
#   C1_16 renombró variables hosp/CEP individuales a totales
#   (e.g. angio_hosp + angio_CEP → totangio).
#   Las endoscopias pasaron de nombre sin sufijo a _hosp/_Amb.
#
# EQUIVALENCIAS (≤2020 vs ≥2021) — fuente: auditoria_c1_16_nombres.csv
#   pet        : pet_hosp + pet_CEP          <->  totpet
#   resonancia : resonancia_hosp + rnm_CEP   <->  totresonancia
#   tac        : tac_hosp + tac_CEP          <->  tottac
#   angio      : angio_hosp + angio_CEP      <->  totangio
#   spect      : spect_hosp + spect_CEP      <->  totspect
#   gamma      : gamma_hosp + gamma_CEP      <->  totgamma
#   densiom    : densiometrias_hosp + _CEP   <->  totdensiometrias
#   mamo       : mamo_hosp + mamo_CEP        <->  totmamo
#   rx         : rx_hosp + rx_CEP            <->  totrx
#   biopsias   : biopsias_hosp + biopsias_CEP <-> totbiopsias
#   colonoscopia: Colonosopia               <->  Col_hosp + Col_Amb
#   broncoscopia: Broncoscopia              <->  Bron_hosp + Bron_Amb
#   ercp       : ERCP                       <->  ERCP_hosp + ERCP_Amb
#
# IMPORTANTE: las variables Tot* con mayúscula (Totpet, Tottac,
#   Totangio, etc.) pertenecen a C1_04 (dotación de equipos,
#   escala 1-5). NO se usan aquí en ningún caso.
# ============================================================

config_path <- if (file.exists("scripts/00_config.R")) "scripts/00_config.R" else "00_config.R"
source(config_path)

required_pkgs <- c("dplyr", "readr")
missing_pkgs  <- required_pkgs[!vapply(required_pkgs, requireNamespace,
                                       logical(1), quietly = TRUE)]
if (length(missing_pkgs) > 0)
  stop("Faltan paquetes: ", paste(missing_pkgs, collapse = ", "),
       "\nInstálalos con install.packages().", call. = FALSE)

library(dplyr)
library(readr)

PROTECTED_VARS <- c(paste0("u", 1:104), "u900", "NCODI", "anyo")

# ============================================================
# 1. Cargar df_final
# ============================================================

message("\n=== Script 14 v3: Creación de variables de intensidad ===")
message("    (bridges corregidos — solo C1_16, sin C1_04)")

if (!file.exists(DF_FINAL_RDATA_PATH))
  stop("No se encontró df_final en: ", DF_FINAL_RDATA_PATH,
       "\nEjecuta el script 9 antes.", call. = FALSE)

load(DF_FINAL_RDATA_PATH)
if (!exists("df_final"))
  stop("Objeto df_final no encontrado.", call. = FALSE)

message("df_final cargado: ", nrow(df_final), " filas x ", ncol(df_final), " columnas")

for (v in c("altTotal_pond", "altTotal_bruto")) {
  if (!(v %in% names(df_final)))
    stop("'", v, "' no existe. Ejecuta el script 9 antes.", call. = FALSE)
}

df <- df_final

# ============================================================
# FUNCIÓN AUXILIAR: existencia y extracción de variables
# ============================================================

# Comprueba existencia con grep (ignore.case) y devuelve
# el nombre real encontrado, o NULL si no existe.
find_var <- function(df, pattern, exact = TRUE) {
  if (exact) {
    if (pattern %in% names(df)) return(pattern)
    return(NULL)
  }
  hits <- grep(pattern, names(df), ignore.case = TRUE, value = TRUE)
  if (length(hits) == 0) return(NULL)
  hits[1]
}

# Extrae columna como numeric; NA si no existe.
get_num <- function(df, varname) {
  if (is.null(varname) || !(varname %in% names(df)))
    return(rep(NA_real_, nrow(df)))
  suppressWarnings(as.numeric(df[[varname]]))
}

# Suma de dos series, tratando NA como 0 si la otra no es NA.
sum_nona <- function(a, b) {
  both_na <- is.na(a) & is.na(b)
  result   <- ifelse(is.na(a), 0, a) + ifelse(is.na(b), 0, b)
  result[both_na] <- NA_real_
  result
}

# ============================================================
# PASO 1 — Construir series armonizadas por procedimiento
# ============================================================

message("\n--- PASO 1: Series armonizadas por procedimiento ---")

# Helper: construye serie armonizada con bridge pre/post 2021.
# pre_vars  : vector de nombres de variables para anyo<=2020 (se suman)
# post_vars : vector de nombres de variables para anyo>=2021 (se suman)
# Documenta con message() si alguna variable no existe.
make_proc <- function(df, label, pre_vars, post_vars) {
  # Verificar existencia
  pre_found  <- Filter(function(v) !is.null(find_var(df, v)), pre_vars)
  post_found <- Filter(function(v) !is.null(find_var(df, v)), post_vars)

  if (length(pre_found) < length(pre_vars)) {
    miss <- setdiff(pre_vars, pre_found)
    message(sprintf("  [%s] AVISO: variable(s) pre-2021 no encontrada(s): %s",
                    label, paste(miss, collapse = ", ")))
  }
  if (length(post_found) < length(post_vars)) {
    miss <- setdiff(post_vars, post_found)
    message(sprintf("  [%s] AVISO: variable(s) post-2020 no encontrada(s): %s",
                    label, paste(miss, collapse = ", ")))
  }

  # Calcular suma pre-2021
  if (length(pre_found) == 0) {
    pre_sum <- rep(NA_real_, nrow(df))
  } else {
    pre_sum <- get_num(df, pre_found[1])
    for (v in pre_found[-1]) pre_sum <- sum_nona(pre_sum, get_num(df, v))
  }

  # Calcular suma post-2020
  if (length(post_found) == 0) {
    post_sum <- rep(NA_real_, nrow(df))
  } else {
    post_sum <- get_num(df, post_found[1])
    for (v in post_found[-1]) post_sum <- sum_nona(post_sum, get_num(df, v))
  }

  # Combinar: pre para <=2020, post para >=2021
  result <- dplyr::case_when(
    df$anyo <= 2020 & !is.na(pre_sum)  ~ pre_sum,
    df$anyo >= 2021 & !is.na(post_sum) ~ post_sum,
    TRUE ~ NA_real_
  )
  result
}

# Construir cada procedimiento
# NOTA: resonancia_CEP puede no existir (búsqueda por exact=FALSE solo
#       para el nombre canónico; si no existe, sum_nona trata como 0).

df$proc_pet          <- make_proc(df, "pet",
                          c("pet_hosp", "pet_CEP"),
                          c("totpet"))

df$proc_resonancia   <- make_proc(df, "resonancia",
                          c("resonancia_hosp", "resonancia_CEP"),
                          c("totresonancia"))

df$proc_ercp         <- make_proc(df, "ercp",
                          c("ERCP"),
                          c("ERCP_hosp", "ERCP_Amb"))

df$proc_tac          <- make_proc(df, "tac",
                          c("tac_hosp", "tac_CEP"),
                          c("tottac"))

df$proc_angio        <- make_proc(df, "angio",
                          c("angio_hosp", "angio_CEP"),
                          c("totangio"))

df$proc_spect        <- make_proc(df, "spect",
                          c("spect_hosp", "spect_CEP"),
                          c("totspect"))

df$proc_gamma        <- make_proc(df, "gamma",
                          c("gamma_hosp", "gamma_CEP"),
                          c("totgamma"))

df$proc_broncoscopia <- make_proc(df, "broncoscopia",
                          c("Broncoscopia"),
                          c("Bron_hosp", "Bron_Amb"))

df$proc_colonoscopia <- make_proc(df, "colonoscopia",
                          c("Colonosopia"),
                          c("Col_hosp", "Col_Amb"))

df$proc_biopsias     <- make_proc(df, "biopsias",
                          c("biopsias_hosp", "biopsias_CEP"),
                          c("totbiopsias"))

df$proc_densiom      <- make_proc(df, "densiometrias",
                          c("densiometrias_hosp", "densiometrias_CEP"),
                          c("totdensiometrias"))

df$proc_mamo         <- make_proc(df, "mamo",
                          c("mamo_hosp", "mamo_CEP"),
                          c("totmamo"))

df$proc_rx           <- make_proc(df, "rx",
                          c("rx_hosp", "rx_CEP"),
                          c("totrx"))

# Cobertura por período
proc_vars <- c("proc_pet","proc_resonancia","proc_ercp","proc_tac",
               "proc_angio","proc_spect","proc_gamma",
               "proc_broncoscopia","proc_colonoscopia",
               "proc_biopsias","proc_densiom","proc_mamo","proc_rx")

message("\n  Cobertura por procedimiento (% no-NA):")
for (v in proc_vars) {
  p1020 <- round(100 * mean(!is.na(df[df$anyo <= 2020, v])), 1)
  p2123 <- round(100 * mean(!is.na(df[df$anyo >= 2021, v])), 1)
  message(sprintf("    %-22s  ≤2020: %5.1f%%   ≥2021: %5.1f%%", v, p1020, p2123))
}

# ============================================================
# PASO 2 — Índice de intensidad diagnóstica (i_diag)
# ============================================================

message("\n--- PASO 2: i_diag ---")

PESOS_DIAG <- c(
  proc_pet          = 4.0,
  proc_resonancia   = 3.5,
  proc_ercp         = 3.0,
  proc_tac          = 2.5,
  proc_angio        = 2.5,
  proc_spect        = 2.0,
  proc_gamma        = 1.5,
  proc_broncoscopia = 1.5,
  proc_colonoscopia = 1.5,
  proc_biopsias     = 1.0,
  proc_densiom      = 0.8,
  proc_mamo         = 0.8,
  proc_rx           = 0.3
)

# Numerador: NA solo si TODOS los componentes son NA.
# Si al menos uno no es NA, los restantes se tratan como 0.
mat_ponderada <- vapply(names(PESOS_DIAG), function(v) {
  df[[v]] * PESOS_DIAG[[v]]
}, numeric(nrow(df)))

n_noNA        <- rowSums(!is.na(mat_ponderada))
df$i_diag_num <- ifelse(n_noNA == 0,
                        NA_real_,
                        rowSums(mat_ponderada, na.rm = TRUE))

df$i_diag <- ifelse(
  !is.na(df$altTotal_pond) & df$altTotal_pond > 0,
  df$i_diag_num / df$altTotal_pond,
  NA_real_
)

df$ln_i_diag <- ifelse(!is.na(df$i_diag) & df$i_diag > 0,
                        log(df$i_diag), NA_real_)

message("  i_diag — resumen:")
print(summary(df$i_diag))

# Verificar correlación mecánica
mask_cor <- !is.na(df$i_diag) & !is.na(df$altFinal_total) & df$altFinal_total > 0
if (sum(mask_cor) >= 30) {
  r_diag <- cor(df$i_diag[mask_cor], df$altFinal_total[mask_cor])
  message(sprintf("  Correlación i_diag ~ altFinal_total: %.3f", r_diag))
  if (abs(r_diag) > 0.85)
    warning(sprintf("Correlación alta (r=%.3f) i_diag ~ altFinal_total.", r_diag),
            call. = FALSE)
  else
    message("  OK: correlación no mecánica.")
}

# ============================================================
# PASO 3 — i_simple (no cambia — total_hosp sigue sin bridge en 2021+)
# ============================================================

message("\n--- PASO 3: i_simple ---")

if ("total_hosp" %in% names(df)) {
  th <- suppressWarnings(as.numeric(df$total_hosp))
  sm <- suppressWarnings(as.numeric(df$sesionTot_med))
  both_na    <- is.na(th) & is.na(sm)
  num_simple <- ifelse(is.na(th), 0, th) + ifelse(is.na(sm), 0, sm)
  num_simple[both_na] <- NA_real_
} else {
  warning("'total_hosp' ausente. i_simple usa solo sesionTot_med.", call. = FALSE)
  num_simple <- suppressWarnings(as.numeric(df$sesionTot_med))
}

df$i_simple    <- ifelse(!is.na(df$altTotal_bruto) & df$altTotal_bruto > 0,
                          num_simple / df$altTotal_bruto, NA_real_)
df$ln_i_simple <- ifelse(!is.na(df$i_simple) & df$i_simple > 0,
                          log(df$i_simple), NA_real_)

# ============================================================
# PASO 4 — Tabla de medias por año (validación de la corrección)
# ============================================================

message("\n--- PASO 4: Tabla de medias por año ---")

all_vars_diag <- c(proc_vars, "i_diag", "i_simple", "ln_i_diag", "ln_i_simple")

diag_rows <- do.call(rbind, lapply(sort(unique(df$anyo)), function(yr) {
  d <- df[df$anyo == yr, ]
  do.call(rbind, lapply(all_vars_diag, function(v) {
    x <- d[[v]]
    data.frame(
      anyo      = yr,
      variable  = v,
      n_validos = sum(!is.na(x)),
      pct_NA    = round(100 * mean(is.na(x)), 1),
      media     = round(mean(x, na.rm = TRUE), 2),
      p50       = round(median(x, na.rm = TRUE), 2),
      stringsAsFactors = FALSE
    )
  }))
}))

message("\n  Media de i_diag por año:")
print(diag_rows[diag_rows$variable == "i_diag",
                c("anyo", "n_validos", "pct_NA", "media", "p50")])

readr::write_csv(diag_rows,
                 file.path(INT_DIR, "intensidad_v2_diagnostico.csv"),
                 na = "")
message("  Tabla guardada en: intensidad_v2_diagnostico.csv")

# ============================================================
# PASO 5 — Trazabilidad NCODI=248
# ============================================================

message("\n--- PASO 5: Trazabilidad NCODI=248 ---")

if (248 %in% df$NCODI) {
  traza_248 <- df[df$NCODI == 248,
                  c("NCODI","anyo","proc_resonancia","proc_tac","proc_pet","i_diag")]
  traza_248 <- traza_248[order(traza_248$anyo), ]
  message("  Hospital NCODI=248:")
  print(traza_248)
} else {
  message("  NCODI=248 no encontrado en df_final.")
}

# ============================================================
# PASO 6 — Actualizar df_final y guardar
# ============================================================

message("\n--- PASO 6: Guardar ---")

# Eliminar variables antiguas si existen
vars_a_eliminar <- c("i_diag_num","i_diag","ln_i_diag",
                     "i_diag_imp","i_diag_final","ln_i_diag_final",
                     "i_simple","ln_i_simple")
vars_a_eliminar <- intersect(vars_a_eliminar, names(df_final))
if (length(vars_a_eliminar) > 0) {
  df_final <- df_final[, setdiff(names(df_final), vars_a_eliminar)]
  message("  Variables antiguas eliminadas: ", paste(vars_a_eliminar, collapse = ", "))
}

# Añadir nuevas variables
vars_nuevas <- c(proc_vars,
                 "i_diag_num", "i_diag", "ln_i_diag",
                 "i_simple", "ln_i_simple")

for (v in vars_nuevas) {
  df_final[[v]] <- df[[v]]
}

n_dup <- sum(duplicated(df_final[, c("NCODI","anyo")]))
if (n_dup > 0) stop("Duplicados (NCODI, anyo): ", n_dup, call. = FALSE)

save(df_final, file = DF_FINAL_RDATA_PATH)
message("  Guardado en: ", DF_FINAL_RDATA_PATH)
message("  Filas: ", nrow(df_final), " | Columnas: ", ncol(df_final))
message("  i_diag válidos   : ", sum(!is.na(df_final$i_diag)), " / ", nrow(df_final))
message("  i_simple válidos : ", sum(!is.na(df_final$i_simple)), " / ", nrow(df_final))

message("\n=== Script 14 v3 completado ===")
