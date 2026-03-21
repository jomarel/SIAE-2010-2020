# ============================================================
# 14_creacion intensidad.R
# Construye la variable proxy de intensidad diagnóstico-
# terapéutica como suma ponderada de procedimientos por alta.
#
# Fórmula:
#   intensidad_numerador = suma(procedimiento_i * peso_i)
#   intensidad = intensidad_numerador / altFinal_total
#
# Los pesos (en minutos aproximados de tiempo de equipo)
# son: biopsias=60, angio=750, densiometrias=100,
#      gamma=250, mamo=50, pet=1200, resonancia=450,
#      rx=30, spect=400, tac=300.
#
# La variable NO está correlacionada mecánicamente con
# altFinal_total porque el numerador mide intensidad de
# uso de tecnología, no volumen de altas.
#
# Input:  df_seleccion (DF_SELECCION_RDATA_PATH)
# Output: df_seleccion con variable 'intensidad' añadida
#         (sobreescribe DF_SELECCION_RDATA_PATH)
# ============================================================

config_path <- if (file.exists("scripts/00_config.R")) "scripts/00_config.R" else "00_config.R"
source(config_path)

required_pkgs <- c("dplyr")
missing_pkgs  <- required_pkgs[!vapply(required_pkgs, requireNamespace,
                                       logical(1), quietly = TRUE)]
if (length(missing_pkgs) > 0) {
  stop("Faltan paquetes: ", paste(missing_pkgs, collapse = ", "),
       "\nInstálalos con install.packages().", call. = FALSE)
}

library(dplyr)

# ------------------------------------------------------------
# 1. Cargar df_seleccion
# ------------------------------------------------------------
if (!file.exists(DF_SELECCION_RDATA_PATH)) {
  stop("No se encontró df_seleccion en: ", DF_SELECCION_RDATA_PATH,
       "\nEjecuta el script 13 antes.", call. = FALSE)
}

load(DF_SELECCION_RDATA_PATH)

if (!exists("df_seleccion")) {
  stop("Objeto df_seleccion no encontrado.", call. = FALSE)
}

message("df_seleccion cargado: ", nrow(df_seleccion), " filas x ",
        ncol(df_seleccion), " columnas.")

df <- df_seleccion

# ------------------------------------------------------------
# 2. Pesos de procedimientos diagnósticos
#    Unidad: minutos aproximados de equipo por procedimiento
#    Fuente: estimaciones de la literatura de radiología y
#            protocolos de los centros (ajustables).
# ------------------------------------------------------------
PESOS_PROC <- c(
  biopsias_total    =   60,   # biopsia guiada por imagen
  angio_total       =  750,   # angiografía diagnóstica
  densiometrias_total = 100,  # densitometría ósea
  gamma_total       =  250,   # gammagrafía
  mamo_total        =   50,   # mamografía
  pet_total         = 1200,   # PET/TC
  resonancia_total  =  450,   # RMN
  rx_total          =   30,   # radiografía convencional
  spect_total       =  400,   # SPECT
  tac_total         =  300    # TC
)

# Verificar disponibilidad de cada componente
comps_disponibles <- intersect(names(PESOS_PROC), names(df))
comps_faltantes   <- setdiff(names(PESOS_PROC), names(df))

if (length(comps_faltantes) > 0) {
  warning("Componentes de intensidad no disponibles en df_seleccion: ",
          paste(comps_faltantes, collapse = ", "),
          "\nSe calcularán con los componentes disponibles.",
          call. = FALSE)
}

message("Componentes usados para intensidad: ",
        paste(comps_disponibles, collapse = ", "))

# ------------------------------------------------------------
# 3. Calcular intensidad
# ------------------------------------------------------------
if (length(comps_disponibles) == 0) {
  warning("Ningún componente de intensidad disponible. ",
          "Variable 'intensidad' no se creará.", call. = FALSE)
} else {
  # Convertir a numérico y calcular numerador como suma ponderada
  df <- df |>
    mutate(
      intensidad_numerador = {
        mat <- sapply(comps_disponibles, function(v) {
          suppressWarnings(as.numeric(df[[v]])) * PESOS_PROC[[v]]
        })
        # rowSums con na.rm=FALSE: si todos los componentes son NA → NA
        sumas <- rowSums(mat, na.rm = TRUE)
        all_na_rows <- rowSums(!is.na(mat)) == 0
        sumas[all_na_rows] <- NA_real_
        sumas
      },
      intensidad = dplyr::if_else(
        !is.na(altFinal_total) & altFinal_total > 0,
        intensidad_numerador / altFinal_total,
        NA_real_
      )
    )

  message("\nResumen de 'intensidad_numerador':")
  print(summary(df$intensidad_numerador))

  message("\nResumen de 'intensidad':")
  print(summary(df$intensidad))

  # ------------------------------------------------------------
  # 4. Verificar correlación con altFinal_total
  #    Advertir si correlación > 0.85 (sugiere problema de escala)
  # ------------------------------------------------------------
  mask <- !is.na(df$intensidad) & !is.na(df$altFinal_total) &
    df$altFinal_total > 0

  if (sum(mask) >= 30) {
    cor_int_altas <- cor(df$intensidad[mask], df$altFinal_total[mask],
                         use = "complete.obs")
    message(sprintf("\nCorrelación 'intensidad' ~ 'altFinal_total': %.3f",
                    cor_int_altas))

    if (abs(cor_int_altas) > 0.85) {
      warning(
        sprintf(
          "Correlación alta entre 'intensidad' y 'altFinal_total' (r=%.3f). ",
          cor_int_altas
        ),
        "Considera revisar los ponderadores o normalizar diferente.",
        call. = FALSE
      )
    } else {
      message("  OK: correlación no mecánica con altFinal_total.")
    }
  } else {
    message("Pocas observaciones válidas para calcular correlación.")
  }

  # ------------------------------------------------------------
  # 5. Etiqueta para sjlabelled (si está disponible)
  # ------------------------------------------------------------
  if (requireNamespace("sjlabelled", quietly = TRUE)) {
    df$intensidad <- sjlabelled::set_label(
      df$intensidad,
      label = "Intensidad diagnóstico-terapéutica (min. equipo / alta)"
    )
    df$intensidad_numerador <- sjlabelled::set_label(
      df$intensidad_numerador,
      label = "Numerador intensidad (minutos totales de equipo)"
    )
  }
}

# ------------------------------------------------------------
# 6. Verificaciones finales
# ------------------------------------------------------------
n_dup <- sum(duplicated(df[, c("NCODI", "anyo")]))
if (n_dup > 0) {
  stop("Duplicados (NCODI, anyo) en df_seleccion: ", n_dup, call. = FALSE)
}

# ------------------------------------------------------------
# 7. Guardar df_seleccion definitivo
# ------------------------------------------------------------
df_seleccion <- df

save(df_seleccion, file = DF_SELECCION_RDATA_PATH)

write.csv(df_seleccion, DF_SELECCION_CSV_PATH, row.names = FALSE, na = "")

message("\ndf_seleccion final guardado:")
message("  RData: ", DF_SELECCION_RDATA_PATH)
message("  CSV:   ", DF_SELECCION_CSV_PATH)
message("  Filas: ", nrow(df_seleccion),
        " | Columnas: ", ncol(df_seleccion))
message("  Obs. con intensidad: ",
        sum(!is.na(df_seleccion$intensidad)))

message("\n=== Script 14 completado ===")
message("=== Bloque 2 completado. df_seleccion listo para SFA. ===")
