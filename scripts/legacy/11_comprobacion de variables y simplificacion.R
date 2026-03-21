# ============================================================
# 11_comprobacion de variables y simplificacion.R
# Agrega variables tecnológicas y de actividad:
#   - Suma componentes hospital + CEP en variables _total
#   - Elimina columnas desagregadas ya integradas
#   - Guarda df_depurado
#
# Input:  df_completo_actualizado (DF_COMPLETO_ACTUALIZADO_RDATA_PATH)
# Output: df_depurado (DF_DEPURADO_RDATA_PATH + DF_DEPURADO_TXT_PATH)
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
# 1. Cargar df_completo_actualizado
# ------------------------------------------------------------
if (!file.exists(DF_COMPLETO_ACTUALIZADO_RDATA_PATH)) {
  stop("No se encontró df_completo_actualizado en: ",
       DF_COMPLETO_ACTUALIZADO_RDATA_PATH,
       "\nEjecuta el script 10 antes.", call. = FALSE)
}

load(DF_COMPLETO_ACTUALIZADO_RDATA_PATH)

if (!exists("df_completo_actualizado")) {
  stop("Objeto df_completo_actualizado no encontrado.", call. = FALSE)
}

message("df_completo_actualizado: ", nrow(df_completo_actualizado),
        " filas x ", ncol(df_completo_actualizado), " columnas.")

df <- df_completo_actualizado

# ------------------------------------------------------------
# 2. Helper: suma robusta de dos columnas (NA + valor = valor)
#    Si ambas son NA → NA; si una tiene valor → usa ese valor.
# ------------------------------------------------------------
safe_sum2 <- function(a, b) {
  a <- suppressWarnings(as.numeric(a))
  b <- suppressWarnings(as.numeric(b))
  dplyr::case_when(
    !is.na(a) & !is.na(b) ~ a + b,
    !is.na(a)              ~ a,
    !is.na(b)              ~ b,
    TRUE                   ~ NA_real_
  )
}

# Versión flexible: suma cualquier número de columnas si existen
sum_cols <- function(df, ...) {
  cols <- c(...)
  cols_exist <- intersect(cols, names(df))
  if (length(cols_exist) == 0) return(rep(NA_real_, nrow(df)))
  mat <- vapply(df[cols_exist], function(x) suppressWarnings(as.numeric(x)),
                numeric(nrow(df)))
  rowSums(mat, na.rm = FALSE) # NA si TODAS son NA; suma si alguna tiene valor
  # rowSums devuelve 0 cuando na.rm=TRUE y todo es NA — preferimos NA
  # → reemplazar filas donde todas eran NA
  all_na <- rowSums(!is.na(mat)) == 0
  result <- rowSums(mat, na.rm = TRUE)
  result[all_na] <- NA_real_
  result
}

# ------------------------------------------------------------
# 3. Crear variables _total (equipamiento de diagnóstico)
# ------------------------------------------------------------
message("\nCreando variables _total de tecnología...")

df <- df |>
  mutate(
    salas_rx_total     = safe_sum2(salas_rx_Hospital,    salas_rx_CEP),
    acelerador_total   = safe_sum2(acelerador_hospital,  acelerador_CEP),
    angiografo_total   = safe_sum2(angiografo_hospital,  angiografo_CEP),
    bombas_total       = safe_sum2(bombas_hospital,      bombas_CEP),
    densiometros_total = safe_sum2(densiometros_hospital, densiometros_CEP),
    hemodialisis_total = safe_sum2(hemodialisis_hospital, hemodialisis_CEP),
    TAC_total          = safe_sum2(TAC_hospital,         TAC_CEP),
    PET_total          = safe_sum2(PET_hospital,         PET_CEP),
    RNM_total          = safe_sum2(RNM_hospital,         RNM_CEP),
    gammacamara_total  = safe_sum2(gammacamara_hospital, gammacamara_CEP),
    litotriptor_total  = safe_sum2(litotriptor_hospital, litotriptor_CEP),
    mamografos_total   = safe_sum2(mamografos_hospital,  mamografos_CEP),
    # SPECT solo existe en hospital (no hay CEP)
    SPECT_total        = suppressWarnings(as.numeric(spect_hospital))
  )

# Crear variables de actividad diagnóstica _total
df <- df |>
  mutate(
    biopsias_total     = safe_sum2(biopsias_hosp,    biopsias_CEP),
    angio_total        = safe_sum2(angio_hosp,       angio_CEP),
    densiometrias_total = safe_sum2(densiometrias_hosp, densiometrias_CEP),
    gamma_total        = safe_sum2(gamma_hosp,       gamma_CEP),
    mamo_total         = safe_sum2(mamo_hosp,        mamo_CEP),
    pet_total          = safe_sum2(pet_hosp,         pet_CEP),
    resonancia_total   = safe_sum2(resonancia_hosp,  resonancia_CEP),
    rx_total           = safe_sum2(rx_hosp,          rx_CEP),
    spect_total        = suppressWarnings(as.numeric(spect_hosp)),
    tac_total          = safe_sum2(tac_hosp,         tac_CEP)
  )

message("  Variables _total creadas.")

# ------------------------------------------------------------
# 4. Eliminar columnas desagregadas (hospital/CEP por separado)
#    Solo se eliminan si existen en el dataframe.
# ------------------------------------------------------------
cols_drop <- c(
  # Equipamiento
  "salas_rx_Hospital", "salas_rx_CEP",
  "acelerador_hospital", "acelerador_CEP",
  "angiografo_hospital", "angiografo_CEP",
  "bombas_hospital", "bombas_CEP",
  "densiometros_hospital", "densiometros_CEP",
  "hemodialisis_hospital", "hemodialisis_CEP",
  "TAC_hospital", "TAC_CEP",
  "PET_hospital", "PET_CEP",
  "RNM_hospital", "RNM_CEP",
  "gammacamara_hospital", "gammacamara_CEP",
  "litotriptor_hospital", "litotriptor_CEP",
  "mamografos_hospital", "mamografos_CEP",
  "spect_hospital",
  # Actividad diagnóstica
  "biopsias_CEP", "biopsias_hosp",
  "angio_hosp", "angio_CEP",
  "densiometrias_hosp", "densiometrias_CEP",
  "gamma_hosp", "gamma_CEP",
  "mamo_hosp", "mamo_CEP",
  "pet_hosp", "pet_CEP",
  "resonancia_hosp", "resonancia_CEP",
  "rx_hosp", "rx_CEP",
  "spect_hosp",
  "tac_hosp", "tac_CEP"
)

cols_drop_exist <- intersect(cols_drop, names(df))
cols_drop_miss  <- setdiff(cols_drop, names(df))

if (length(cols_drop_miss) > 0) {
  message("  Columnas desagregadas no encontradas (ya eliminadas o renombradas): ",
          length(cols_drop_miss))
}

df_depurado <- df |> select(-all_of(cols_drop_exist))

message("  Columnas desagregadas eliminadas: ", length(cols_drop_exist))

# ------------------------------------------------------------
# 5. Comprobaciones finales
# ------------------------------------------------------------
n_dup <- sum(duplicated(df_depurado[, c("NCODI", "anyo")]))
if (n_dup > 0) {
  stop("Duplicados (NCODI, anyo) en df_depurado: ", n_dup, call. = FALSE)
}

message("\ndf_depurado: ", nrow(df_depurado), " filas x ",
        ncol(df_depurado), " columnas.")

# ------------------------------------------------------------
# 6. Guardar
# ------------------------------------------------------------
save(df_depurado, file = DF_DEPURADO_RDATA_PATH)

write.table(df_depurado, DF_DEPURADO_TXT_PATH,
            sep = ";", dec = ",", row.names = FALSE, na = "")

message("Guardado en: ", DF_DEPURADO_RDATA_PATH)
message("\n=== Script 11 completado ===")
