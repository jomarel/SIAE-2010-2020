# ============================================================
# 14_creacion_intensidad.R
# Construye índices de intensidad diagnóstico-terapéutica.
#
# PASO 1 — i_diag  : suma ponderada de procedimientos / altTotal_pond
# PASO 2 — i_simple: (total_hosp + sesionTot_med) / altTotal_bruto
# PASO 3 — Log-transformaciones
# PASO 4 — Diagnóstico por anyo
# PASO 5 — Añade variables a df_final y guarda
#
# Notas de verificación previa (2025-03-20):
#   - altTotal_pond y altTotal_bruto ya existen en df_final
#   - Cambio estructural SIAE 2021-2023: el módulo C1_05 pasó de
#     variables *_hosp a variables Tot* (sin solapamiento temporal).
#     Se usan variables "bridge" vía coalesce para cubrir todo el panel.
#   - ERCP (2010-2020) ↔ ERCP_hosp (2021-2023)
#   - Broncoscopia y Colonosopia: sin bridge en 2021-2023 (ausentes)
#   - total_hosp: 0% cobertura en 2021-2023 (i_simple = NA esos años)
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

message("\n=== Script 14: Creación de variables de intensidad ===")

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
# FUNCIÓN AUXILIAR: coalesce de variables entre períodos
#
# La SIAE cambió nombres en 2021: variables *_hosp → Tot*.
# bridge_coalesce() combina ambas series usando la primera
# disponible (sin solapamiento temporal → resultado limpio).
# ============================================================

bridge_coalesce <- function(df, primary, fallbacks) {
  result <- suppressWarnings(as.numeric(df[[primary]]))
  for (fb in fallbacks) {
    if (!(fb %in% names(df))) next
    alt <- suppressWarnings(as.numeric(df[[fb]]))
    result[is.na(result)] <- alt[is.na(result)]
  }
  result
}

# ============================================================
# PASO 1 — Índice de intensidad diagnóstica (i_diag)
# ============================================================

message("\n--- PASO 1: i_diag ---")

# Pesos fijos: coste relativo por procedimiento (constantes entre
# hospitales y años; evitan distorsión por precios regionales).
PESOS_DIAG <- c(
  pet_hosp           = 4.0,
  resonancia_hosp    = 3.5,
  ERCP               = 3.0,
  tac_hosp           = 2.5,
  angio_hosp         = 2.5,
  spect_hosp         = 2.0,
  gamma_hosp         = 1.5,
  Broncoscopia       = 1.5,
  Colonosopia        = 1.5,
  biopsias_hosp      = 1.0,
  densiometrias_hosp = 0.8,
  mamo_hosp          = 0.8,
  rx_hosp            = 0.3
)

# Mapa de bridges 2021-2023 para cada variable del índice.
# Variables sin bridge: Broncoscopia, Colonosopia (ausentes post-2020).
BRIDGE_MAP <- list(
  pet_hosp           = c("Totpet"),
  resonancia_hosp    = c("Totrnm", "totresonancia"),
  ERCP               = c("ERCP_hosp"),
  tac_hosp           = c("Tottac"),
  angio_hosp         = c("Totangio"),
  spect_hosp         = c("Totspect"),
  gamma_hosp         = c("Totgammaca"),
  Broncoscopia       = character(0),       # sin bridge → NA en 2021-23
  Colonosopia        = character(0),       # sin bridge → NA en 2021-23
  biopsias_hosp      = c("totbiopsias"),
  densiometrias_hosp = c("Totdensi"),
  mamo_hosp          = c("Totmamos"),
  rx_hosp            = c("Totalsalas_rx")
)

# Construir matriz de componentes bridgeados
comp_names <- names(PESOS_DIAG)
mat_weighted <- matrix(NA_real_, nrow = nrow(df), ncol = length(comp_names))
colnames(mat_weighted) <- comp_names

for (v in comp_names) {
  bridges <- BRIDGE_MAP[[v]]
  if (v %in% names(df) || length(bridges) > 0) {
    raw <- if (v %in% names(df)) bridge_coalesce(df, v, bridges) else {
      # variable original ausente, intentar directo desde bridge
      vals <- rep(NA_real_, nrow(df))
      for (fb in bridges) {
        if (fb %in% names(df)) {
          alt <- suppressWarnings(as.numeric(df[[fb]]))
          vals[is.na(vals)] <- alt[is.na(vals)]
        }
      }
      vals
    }
    mat_weighted[, v] <- raw * PESOS_DIAG[[v]]
  }
}

# Cobertura por componente y período
message("  Cobertura por variable (% no-NA):")
for (v in comp_names) {
  p1020 <- round(100 * mean(!is.na(mat_weighted[df$anyo <= 2020, v])), 1)
  p2123 <- round(100 * mean(!is.na(mat_weighted[df$anyo >= 2021, v])), 1)
  message(sprintf("    %-22s  2010-20: %5.1f%%  2021-23: %5.1f%%", v, p1020, p2123))
}

# Numerador: NA solo si TODOS los componentes son NA para ese hospital-año
n_noNA <- rowSums(!is.na(mat_weighted))
df$i_diag_num <- ifelse(n_noNA == 0, NA_real_, rowSums(mat_weighted, na.rm = TRUE))

df$i_diag <- ifelse(
  !is.na(df$altTotal_pond) & df$altTotal_pond > 0,
  df$i_diag_num / df$altTotal_pond,
  NA_real_
)

message("  i_diag_num — resumen:")
print(summary(df$i_diag_num))
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
# PASO 2 — Índice alternativo simple (i_simple)
# ============================================================

message("\n--- PASO 2: i_simple ---")

# i_simple = (consultas_hospital + sesiones_hdia_médico) / altTotal_bruto
# total_hosp tiene 0% cobertura en 2021-2023 → i_simple = NA esos años.
# No existe variable bridge para consultas en 2021-2023.
# (total_consulta existe pero agrega hospital + CEP, distinto concepto)

if ("total_hosp" %in% names(df)) {
  th <- suppressWarnings(as.numeric(df$total_hosp))
  sm <- suppressWarnings(as.numeric(df$sesionTot_med))
  # NA en total_hosp con sesionTot_med presente → usar solo sesionTot_med
  num_simple <- ifelse(!is.na(th) | !is.na(sm),
                       ifelse(is.na(th), 0, th) + ifelse(is.na(sm), 0, sm),
                       NA_real_)
  # Si ambas son NA → NA
  both_na <- is.na(th) & is.na(sm)
  num_simple[both_na] <- NA_real_
} else {
  warning("'total_hosp' ausente. i_simple usa solo sesionTot_med.", call. = FALSE)
  num_simple <- suppressWarnings(as.numeric(df$sesionTot_med))
}

df$i_simple <- ifelse(!is.na(df$altTotal_bruto) & df$altTotal_bruto > 0,
                       num_simple / df$altTotal_bruto, NA_real_)

message("  i_simple — resumen:")
print(summary(df$i_simple))

# ============================================================
# PASO 3 — Log-transformaciones
# ============================================================

message("\n--- PASO 3: Log-transformaciones ---")

df$ln_i_diag   <- ifelse(!is.na(df$i_diag)   & df$i_diag   > 0,
                          log(df$i_diag),   NA_real_)
df$ln_i_simple <- ifelse(!is.na(df$i_simple) & df$i_simple > 0,
                          log(df$i_simple), NA_real_)

message("  ln_i_diag  : ", sum(!is.na(df$ln_i_diag)),  " obs. válidas")
message("  ln_i_simple: ", sum(!is.na(df$ln_i_simple)), " obs. válidas")

# ============================================================
# PASO 4 — Diagnóstico por anyo
# ============================================================

message("\n--- PASO 4: Diagnóstico por anyo ---")

qsafe <- function(x, p) {
  x <- x[!is.na(x)]; if (!length(x)) NA_real_ else quantile(x, p, names = FALSE)
}

diag_rows <- dplyr::bind_rows(lapply(sort(unique(df$anyo)), function(yr) {
  d <- df[df$anyo == yr, ]
  dplyr::bind_rows(lapply(c("i_diag","i_simple","ln_i_diag","ln_i_simple"),
    function(v) {
      x <- d[[v]]
      data.frame(anyo=yr, variable=v, n_validos=sum(!is.na(x)),
                 pct_NA=round(100*mean(is.na(x)),1),
                 media=round(mean(x,na.rm=TRUE),4),
                 sd   =round(sd(x,  na.rm=TRUE),4),
                 p10  =round(qsafe(x,0.10),4),
                 p50  =round(qsafe(x,0.50),4),
                 p90  =round(qsafe(x,0.90),4),
                 stringsAsFactors=FALSE)
    }))
}))

mask_both <- !is.na(df$i_diag) & !is.na(df$i_simple)
if (sum(mask_both) >= 30) {
  r12 <- cor(df$i_diag[mask_both], df$i_simple[mask_both])
  message(sprintf("  Correlación i_diag ~ i_simple: %.3f (n=%d)", r12, sum(mask_both)))
}

write.csv(diag_rows, file.path(INT_DIR, "intensidad_diagnostico.csv"),
          row.names = FALSE, na = "")
message("  Diagnóstico guardado en: intensidad_diagnostico.csv")

cat("\n  i_diag por año (n_validos, pct_NA, media, p50):\n")
print(diag_rows[diag_rows$variable == "i_diag",
                c("anyo","n_validos","pct_NA","media","p50")])

# ============================================================
# PASO 5 — Añadir a df_final y guardar
# ============================================================

message("\n--- PASO 5: Guardar ---")

vars_nuevas <- c("i_diag_num","i_diag","i_simple","ln_i_diag","ln_i_simple")
df_final[vars_nuevas] <- df[vars_nuevas]

n_dup <- sum(duplicated(df_final[, c("NCODI","anyo")]))
if (n_dup > 0) stop("Duplicados (NCODI, anyo): ", n_dup, call. = FALSE)

save(df_final, file = DF_FINAL_RDATA_PATH)
message("  Guardado en: ", DF_FINAL_RDATA_PATH)
message("  Filas: ", nrow(df_final), " | Columnas: ", ncol(df_final))
message("  i_diag   válidos: ", sum(!is.na(df_final$i_diag)),
        " / ", nrow(df_final))
message("  i_simple válidos: ", sum(!is.na(df_final$i_simple)),
        " / ", nrow(df_final))

message("\n=== Script 14 completado ===")
