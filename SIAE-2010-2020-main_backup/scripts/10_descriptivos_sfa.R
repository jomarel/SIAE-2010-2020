# ============================================================
# 13_descriptivos_sfa.R
# Estadísticos descriptivos del dataset SFA para la tesis.
#
# Input:  data_intermediate/df_sfa.RData
# Output:
#   data_intermediate/tabla1_descriptivos.csv
#   data_intermediate/tabla2_evolucion.csv
#   data_intermediate/tabla3_cobertura_intensidad.csv
# ============================================================

config_path <- if (file.exists("scripts/00_config.R")) "scripts/00_config.R" else "00_config.R"
source(config_path)

required_pkgs <- c("dplyr", "readr", "tidyr")
missing_pkgs  <- required_pkgs[!vapply(required_pkgs, requireNamespace,
                                       logical(1), quietly = TRUE)]
if (length(missing_pkgs) > 0)
  stop("Faltan paquetes: ", paste(missing_pkgs, collapse = ", "), call. = FALSE)

library(dplyr)
library(readr)
library(tidyr)

message("\n=== Script 13: Descriptivos SFA ===")

df_sfa_path <- file.path(INT_DIR, "df_sfa.RData")
if (!file.exists(df_sfa_path))
  stop("No encontrado df_sfa. Ejecuta script 12 antes.", call. = FALSE)

load(df_sfa_path)
message("df_sfa cargado: ", nrow(df_sfa), " x ", ncol(df_sfa))

to_num <- function(x) suppressWarnings(as.numeric(x))

qsafe <- function(x, p) {
  x <- x[!is.na(x)]
  if (length(x) == 0) NA_real_ else quantile(x, p, names = FALSE)
}

desc_stats <- function(x) {
  x <- to_num(x)
  data.frame(
    n        = sum(!is.na(x)),
    pct_NA   = round(100 * mean(is.na(x)), 1),
    media    = round(mean(x, na.rm = TRUE), 3),
    sd       = round(sd(x, na.rm = TRUE), 3),
    min      = round(min(x, na.rm = TRUE), 3),
    p10      = round(qsafe(x, 0.10), 3),
    p50      = round(qsafe(x, 0.50), 3),
    p90      = round(qsafe(x, 0.90), 3),
    max      = round(max(x, na.rm = TRUE), 3),
    stringsAsFactors = FALSE
  )
}

# Variables para la tabla descriptiva (excluir identificadores y logs de proc)
VARS_DESC <- c(
  "altTotal_pond", "altQ_pond", "altM_pond",
  "ln_altTotal_pond", "ln_altQ_pond", "ln_altM_pond",
  "i_diag", "ln_i_diag",
  "L_total", "K_camas", "K_tech_index",
  "ln_L_total", "ln_K_camas", "ln_K_tech",
  "pct_sns", "pct_privado", "pct_mutuas", "pct_ingr_SNS",
  "ShareQ", "peso_grd_final",
  "D_desc", "D_desc_siae", "D_desc_cnh"
)

VARS_DESC <- intersect(VARS_DESC, names(df_sfa))

# ============================================================
# TABLA 1 — Descriptivos generales + por D_desc
# ============================================================

message("\n--- TABLA 1: Estadísticos descriptivos ---")

make_desc_table <- function(data, grupo_label) {
  do.call(rbind, lapply(VARS_DESC, function(v) {
    cbind(data.frame(grupo = grupo_label, variable = v,
                     stringsAsFactors = FALSE),
          desc_stats(data[[v]]))
  }))
}

tab1_total <- make_desc_table(df_sfa, "total")

tab1_D0 <- if ("D_desc" %in% names(df_sfa)) {
  make_desc_table(df_sfa[!is.na(df_sfa$D_desc) & df_sfa$D_desc == 0L, ],
                  "centralizado (D=0)")
} else { NULL }

tab1_D1 <- if ("D_desc" %in% names(df_sfa)) {
  make_desc_table(df_sfa[!is.na(df_sfa$D_desc) & df_sfa$D_desc == 1L, ],
                  "descentralizado (D=1)")
} else { NULL }

tabla1 <- do.call(rbind, Filter(Negate(is.null),
                                list(tab1_total, tab1_D0, tab1_D1)))

message("  Variables: ", length(VARS_DESC),
        " | Grupos: ", n_distinct(tabla1$grupo))

# Vista rápida del total
cat("\n  Descriptivos totales (variables principales):\n")
print(tabla1[tabla1$grupo == "total" &
             tabla1$variable %in% c("altTotal_pond","ln_L_total",
                                    "ln_K_camas","pct_sns","i_diag"),
             c("variable","n","media","sd","p10","p50","p90")])

write_csv(tabla1, file.path(INT_DIR, "tabla1_descriptivos.csv"), na = "")
message("\n  Guardado en: tabla1_descriptivos.csv")

# ============================================================
# TABLA 2 — Evolución temporal
# ============================================================

message("\n--- TABLA 2: Evolución temporal ---")

vars_evol <- intersect(
  c("altTotal_pond","i_diag","pct_sns","ShareQ","L_total","K_camas"),
  names(df_sfa)
)

tabla2 <- df_sfa %>%
  group_by(anyo) %>%
  summarise(
    n_obs = n(),
    across(all_of(vars_evol),
           list(media = ~ round(mean(to_num(.), na.rm = TRUE), 2)),
           .names = "{.col}_{.fn}"),
    D_desc_pct1 = if ("D_desc" %in% names(cur_data()))
      round(100 * sum(D_desc == 1L, na.rm = TRUE) / n(), 1)
    else NA_real_,
    .groups = "drop"
  )

print(tabla2)
write_csv(tabla2, file.path(INT_DIR, "tabla2_evolucion.csv"), na = "")
message("  Guardado en: tabla2_evolucion.csv")

# ============================================================
# TABLA 3 — Cobertura i_diag por D_desc × anyo
# ============================================================

message("\n--- TABLA 3: Cobertura i_diag por D_desc y año ---")

if ("i_diag" %in% names(df_sfa) && "D_desc" %in% names(df_sfa)) {
  tabla3 <- df_sfa %>%
    mutate(
      D_desc_label = dplyr::case_when(
        D_desc == 0L ~ "centralizado",
        D_desc == 1L ~ "descentralizado",
        TRUE         ~ "NA_Ddesc"
      )
    ) %>%
    group_by(anyo, D_desc_label) %>%
    summarise(
      n_obs        = n(),
      n_idiag_val  = sum(!is.na(i_diag)),
      pct_idiag    = round(100 * sum(!is.na(i_diag)) / n(), 1),
      .groups      = "drop"
    )

  # Pivot para leer fácil
  tab3_wide <- tidyr::pivot_wider(
    tabla3,
    id_cols    = anyo,
    names_from = D_desc_label,
    values_from = c(n_obs, pct_idiag),
    names_glue = "{D_desc_label}_{.value}"
  )

  print(tab3_wide)
  write_csv(tabla3, file.path(INT_DIR, "tabla3_cobertura_intensidad.csv"), na = "")
  message("  Guardado en: tabla3_cobertura_intensidad.csv")

  # Comparación directa entre grupos
  tab3_comp <- tabla3 %>%
    filter(D_desc_label %in% c("centralizado","descentralizado")) %>%
    select(anyo, D_desc_label, pct_idiag) %>%
    tidyr::pivot_wider(names_from = D_desc_label, values_from = pct_idiag) %>%
    mutate(diferencia = round(centralizado - descentralizado, 1))

  sesgo_detectado <- any(abs(tab3_comp$diferencia) > 10, na.rm = TRUE)
  if (sesgo_detectado) {
    message("\n  *** ADVERTENCIA: diferencia > 10pp en cobertura de i_diag entre grupos D_desc.")
    message("      Posible sesgo de selección en la variable de intensidad.")
    message("      Años con mayor diferencia:")
    print(tab3_comp[abs(tab3_comp$diferencia) > 10, ])
  } else {
    message("  OK: cobertura de i_diag similar entre hospitales centralizados y descentralizados.")
  }

} else {
  message("  i_diag o D_desc no disponibles en df_sfa — TABLA 3 omitida.")
}

message("\n=== Script 13 completado ===")
