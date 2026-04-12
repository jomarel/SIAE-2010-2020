# ============================================================
# 11_comprobacion_variables_sfa.R
# Verifica presencia y cobertura de las variables del modelo SFA
# en df_final. NO crea variables nuevas.
#
# Output:
#   data_intermediate/cobertura_variables_sfa.csv
#   data_intermediate/obs_utilizables_sfa.csv
#   data_intermediate/hospitales_problematicos_sfa.csv
# ============================================================

config_path <- if (file.exists("scripts/00_config.R")) "scripts/00_config.R" else "00_config.R"
source(config_path)

required_pkgs <- c("dplyr", "readr")
missing_pkgs  <- required_pkgs[!vapply(required_pkgs, requireNamespace,
                                       logical(1), quietly = TRUE)]
if (length(missing_pkgs) > 0)
  stop("Faltan paquetes: ", paste(missing_pkgs, collapse = ", "), call. = FALSE)

library(dplyr)
library(readr)

PROTECTED_VARS <- c(paste0("u", 1:104), "u900", "NCODI", "anyo")

message("\n=== Script 11: Comprobación de variables SFA ===")

if (!file.exists(DF_FINAL_RDATA_PATH))
  stop("No encontrado df_final: ", DF_FINAL_RDATA_PATH, call. = FALSE)

load(DF_FINAL_RDATA_PATH)
message("df_final cargado: ", nrow(df_final), " x ", ncol(df_final))

df <- df_final

# ============================================================
# PASO 1 — Verificar existencia de variables requeridas
# ============================================================

message("\n--- PASO 1: Verificación de existencia ---")

VAR_GRUPOS <- list(
  identificadores = c("NCODI", "anyo", "nombre_hospital", "ccaa_cnh"),
  outputs = c("altQ_pond", "altM_pond", "altTotal_pond",
              "ln_altQ_pond", "ln_altM_pond", "ln_altTotal_pond"),
  intensidad = c("i_diag", "ln_i_diag", "i_simple", "ln_i_simple"),
  inputs = c("ln_L_total", "ln_L_medico", "ln_L_quirur",
             "ln_K_camas", "ln_K_tech", "ln_K_quirof"),
  determinantes = c("D_desc", "D_desc_siae", "D_desc_cnh",
                    "pct_sns", "pct_privado", "pct_mutuas", "pct_ingr_SNS",
                    "ShareQ", "peso_grd_final", "fuente_peso"),
  brutas = c("altQ_bruto", "altM_bruto", "altTotal_bruto",
             "L_total", "L_medico", "L_quirur",
             "K_camas", "K_tech_index")
)

nms <- names(df)
vars_status <- do.call(rbind, lapply(names(VAR_GRUPOS), function(grp) {
  vars <- VAR_GRUPOS[[grp]]
  do.call(rbind, lapply(vars, function(v) {
    existe <- v %in% nms
    alternativa <- if (!existe) {
      hits <- grep(gsub("_", ".*", v, fixed = TRUE), nms,
                   ignore.case = TRUE, value = TRUE)
      if (length(hits) > 0) paste(head(hits, 3), collapse = " | ") else "no encontrada"
    } else ""
    data.frame(grupo = grp, variable = v,
               existe = existe, alternativa = alternativa,
               stringsAsFactors = FALSE)
  }))
}))

# Añadir proc_* como grupo separado
proc_vars_req <- c("proc_pet","proc_resonancia","proc_tac","proc_angio",
                   "proc_spect","proc_gamma","proc_broncoscopia",
                   "proc_colonoscopia","proc_ercp","proc_biopsias",
                   "proc_densiom","proc_mamo","proc_rx")

proc_status <- do.call(rbind, lapply(proc_vars_req, function(v) {
  existe <- v %in% nms
  data.frame(grupo = "proc_diagnosticos", variable = v,
             existe = existe, alternativa = "",
             stringsAsFactors = FALSE)
}))

vars_status <- rbind(vars_status, proc_status)

message("\n  Variables AUSENTES:")
ausentes <- vars_status[!vars_status$existe, ]
if (nrow(ausentes) == 0) {
  message("  Ninguna — todas las variables requeridas existen en df_final.")
} else {
  for (i in seq_len(nrow(ausentes))) {
    message(sprintf("    [%s] %s  →  alternativa: %s",
                    ausentes$grupo[i], ausentes$variable[i],
                    ausentes$alternativa[i]))
  }
}

message(sprintf("\n  Variables presentes: %d / %d",
                sum(vars_status$existe), nrow(vars_status)))

# ============================================================
# PASO 2 — Tabla de cobertura
# ============================================================

message("\n--- PASO 2: Cobertura por variable ---")

qsafe <- function(x, p) {
  x <- suppressWarnings(as.numeric(x[!is.na(x)]))
  if (length(x) == 0) NA_real_ else quantile(x, p, names = FALSE)
}

vars_para_cobertura <- vars_status$variable[vars_status$existe]
# Excluir string/factor y protegidas para estadísticas numéricas
vars_char <- c("NCODI", "nombre_hospital", "ccaa_cnh", "fuente_peso")

cob_rows <- lapply(vars_para_cobertura, function(v) {
  x_raw <- df[[v]]
  n_val <- sum(!is.na(x_raw))
  pct_NA <- round(100 * mean(is.na(x_raw)), 1)
  x <- if (v %in% vars_char) NULL else suppressWarnings(as.numeric(x_raw))
  data.frame(
    grupo     = vars_status$grupo[vars_status$variable == v][1],
    variable  = v,
    n_validos = n_val,
    pct_NA    = pct_NA,
    min       = if (!is.null(x)) round(min(x, na.rm = TRUE), 3) else NA_real_,
    p25       = if (!is.null(x)) round(qsafe(x, 0.25), 3) else NA_real_,
    p50       = if (!is.null(x)) round(qsafe(x, 0.50), 3) else NA_real_,
    p75       = if (!is.null(x)) round(qsafe(x, 0.75), 3) else NA_real_,
    max       = if (!is.null(x)) round(max(x, na.rm = TRUE), 3) else NA_real_,
    stringsAsFactors = FALSE
  )
})

cob_df <- do.call(rbind, cob_rows)

message("\n  Cobertura variables de modelo:")
print(cob_df[cob_df$grupo %in% c("outputs","intensidad","inputs","determinantes"),
             c("grupo","variable","n_validos","pct_NA","p50")])

write_csv(cob_df, file.path(INT_DIR, "cobertura_variables_sfa.csv"), na = "")
message("  Guardado en: cobertura_variables_sfa.csv")

# ============================================================
# PASO 3 — Observaciones utilizables para SFA
# ============================================================

message("\n--- PASO 3: Observaciones utilizables para SFA ---")

# Variables mínimas para SFA (deben ser no-NA y positivas)
vars_core_sfa <- c("ln_altQ_pond", "ln_altM_pond",
                   "ln_L_total", "ln_K_camas",
                   "D_desc", "pct_sns")

vars_core_presentes <- intersect(vars_core_sfa, nms)
vars_core_ausentes  <- setdiff(vars_core_sfa, nms)
if (length(vars_core_ausentes) > 0)
  message("  AVISO: variables core ausentes: ", paste(vars_core_ausentes, collapse = ", "))

# Completo: todas las core no-NA Y los logs > -Inf (es decir, valores originales > 0)
completo_mask <- rowSums(
  sapply(vars_core_presentes, function(v) {
    x <- suppressWarnings(as.numeric(df[[v]]))
    is.na(x) | (!v %in% c("D_desc","pct_sns") & !is.finite(x))
  })
) == 0

n_obs_completas <- sum(completo_mask)
message(sprintf("  Observaciones completas (core): %d / %d (%.1f%%)",
                n_obs_completas, nrow(df),
                100 * n_obs_completas / nrow(df)))

# Por año
tab_completas <- df %>%
  mutate(.completo = completo_mask) %>%
  group_by(anyo) %>%
  summarise(n_total = n(),
            n_completas = sum(.completo),
            pct_completas = round(100 * sum(.completo) / n(), 1),
            .groups = "drop")
print(tab_completas)

# Hospitales con al menos 3 años completos
hosp_completos <- df %>%
  mutate(.completo = completo_mask) %>%
  group_by(NCODI) %>%
  summarise(n_anios_total = n(),
            n_anios_completos = sum(.completo),
            .groups = "drop") %>%
  filter(n_anios_completos >= 3)

n_hosp_completos <- nrow(hosp_completos)
message(sprintf("  Hospitales con >= 3 años completos: %d", n_hosp_completos))

write_csv(tab_completas, file.path(INT_DIR, "obs_utilizables_sfa.csv"), na = "")
message("  Guardado en: obs_utilizables_sfa.csv")

# ============================================================
# PASO 4 — Hospitales problemáticos
# ============================================================

message("\n--- PASO 4: Hospitales problemáticos ---")

# Variables clave para identificar hospitales problemáticos
vars_clave <- intersect(c("ln_altQ_pond","ln_altM_pond","ln_L_total",
                           "ln_K_camas","pct_sns"), nms)

# Buscar nombre_hospital si existe
tiene_nombre <- "nombre_hospital" %in% nms

# Calcular máscara de NA por fila ANTES del groupby
df$.tiene_na_clave <- rowSums(
  sapply(vars_clave, function(v) {
    x <- suppressWarnings(as.numeric(df[[v]]))
    as.integer(is.na(x) | !is.finite(x))
  })
) > 0

hosp_prob <- df %>%
  group_by(NCODI) %>%
  summarise(
    nombre_hospital = if (tiene_nombre) first(nombre_hospital) else NA_character_,
    n_anios_total   = n(),
    n_anios_NA      = sum(.tiene_na_clave),
    pct_anios_NA    = round(100 * sum(.tiene_na_clave) / n(), 1),
    .groups = "drop"
  ) %>%
  filter(pct_anios_NA > 50) %>%
  arrange(desc(pct_anios_NA))

message(sprintf("  Hospitales con > 50%% años con NAs en variables clave: %d",
                nrow(hosp_prob)))
if (nrow(hosp_prob) > 0) {
  print(head(hosp_prob, 20))
}

write_csv(hosp_prob, file.path(INT_DIR, "hospitales_problematicos_sfa.csv"), na = "")
message("  Guardado en: hospitales_problematicos_sfa.csv")

message("\n=== Script 11 completado ===")
