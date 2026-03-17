# ============================================================
# 13_reduccion variables.R
# Construye variables compuestas (tecnología, personal),
# verifica coherencia interna de actividad por financiador,
# y selecciona el subconjunto de variables para SFA.
#
# Input:  df_depurado (DF_DEPURADO_RDATA_PATH)
# Output: df_seleccion (DF_SELECCION_RDATA_PATH + DF_SELECCION_CSV_PATH)
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
# 1. Cargar df_depurado
# ------------------------------------------------------------
if (!file.exists(DF_DEPURADO_RDATA_PATH)) {
  stop("No se encontró df_depurado en: ", DF_DEPURADO_RDATA_PATH,
       "\nEjecuta el script 12 antes.", call. = FALSE)
}

load(DF_DEPURADO_RDATA_PATH)

if (!exists("df_depurado")) {
  stop("Objeto df_depurado no encontrado.", call. = FALSE)
}

df <- as.data.frame(df_depurado)

message("df_depurado: ", nrow(df), " filas x ", ncol(df), " columnas.")

# ------------------------------------------------------------
# 2. Construir índices de tecnología
# ------------------------------------------------------------
message("\nConstruyendo índices tecnológicos...")

df <- df |>
  mutate(
    # Alta tecnología diagnóstica y terapéutica
    altatecno = rowSums(
      across(any_of(c("TAC_total", "PET_total", "RNM_total",
                      "gammacamara_total", "acelerador_total",
                      "litotriptor_total", "bombas_total"))),
      na.rm = TRUE
    ),
    # Tecnología básica de diagnóstico por imagen
    tecno_basica = rowSums(
      across(any_of(c("salas_rx_total", "mamografos_total",
                      "densiometros_total", "angiografo_total",
                      "hemodialisis_total"))),
      na.rm = TRUE
    ),
    # Total tecnología
    tecnologia_total = altatecno + tecno_basica
  )

# Poner NA donde todos los componentes eran NA (rowSums devuelve 0)
comps_alta <- c("TAC_total", "PET_total", "RNM_total", "gammacamara_total",
                "acelerador_total", "litotriptor_total", "bombas_total")
comps_alta <- intersect(comps_alta, names(df))
if (length(comps_alta) > 0) {
  all_na_alta <- rowSums(!is.na(df[comps_alta])) == 0
  df$altatecno[all_na_alta] <- NA_real_
}

comps_basica <- c("salas_rx_total", "mamografos_total", "densiometros_total",
                  "angiografo_total", "hemodialisis_total")
comps_basica <- intersect(comps_basica, names(df))
if (length(comps_basica) > 0) {
  all_na_bas <- rowSums(!is.na(df[comps_basica])) == 0
  df$tecno_basica[all_na_bas] <- NA_real_
}

message("  altatecno | tecno_basica construidas.")

# ------------------------------------------------------------
# 3. Construir variables de personal
# ------------------------------------------------------------
message("Construyendo variables de personal...")

df <- df |>
  mutate(
    # Personal médico (jornada completa + parcial + colaboradores + farmacéuticos)
    personal_medico = rowSums(
      across(any_of(c("total_cMedicos", "total_pMedicos", "total_colabMedicos",
                      "farmaceuticos_cTotal", "farmaceuticos_pTotal",
                      "farmaceuticos_colabTotal"))),
      na.rm = TRUE
    ),
    # Personal de enfermería y técnico-sanitario
    personal_aux = rowSums(
      across(any_of(c(
        "due_cTotal", "matronas_cTotal", "oDueEspeciali_cTotal",
        "fisioterapeutas_cTotal", "terapeutas_cTotal", "logopedas_cTotal",
        "oSaniMedio_cTotal", "gMedioSani_cTotal", "gSuperiorSani_cTotal",
        "tecSani_cTotal", "restSani_cTotal2",
        "due_pTotal", "matronas_pTotal", "oDueEspeciali_pTotal",
        "fisioterapeutas_pTotal", "terapeutas_pTotal", "logopedas_pTotal",
        "oSaniMedio_pTotal", "gMedioSani_pTotal", "gSuperiorSani_pTotal",
        "tecSani_pTotal", "restSani_pTotal2"
      ))),
      na.rm = TRUE
    ),
    # Personal no sanitario y administrativo
    personal_resto = rowSums(
      across(any_of(c(
        "administrativos_cTotal", "administrativos_pTotal",
        "administrativos_colabTotal",
        "oNoSani_cTotal", "oNoSani_pTotal", "oNoSani_colabTotal"
      ))),
      na.rm = TRUE
    )
  )

message("  personal_medico | personal_aux | personal_resto construidas.")

# ------------------------------------------------------------
# 4. Variables auxiliares de control
# ------------------------------------------------------------
df <- df |>
  mutate(
    porcentaje_no_defuncion = ifelse(
      !is.na(total_altas) & total_altas > 0,
      (total_altas - altExitus_total) / total_altas,
      NA_real_
    ),
    intensidad_docente = rowSums(
      across(any_of(c("totalMir_total", "totalEir_total"))),
      na.rm = FALSE
    )
  )

# ------------------------------------------------------------
# 5. Verificar coherencia interna (discrepancia total vs sumado)
#    Solo registro — no elimina hospitales sin confirmación
# ------------------------------------------------------------
message("\nVerificando discrepancias actividad total vs sumado por financiador...")

financiadores_sfx <- c("particular", "AsegPriv", "sns", "MutuaFun",
                       "otrEntiPublica", "MATEPSS", "convIntern",
                       "AccTrafic", "otroRegim")

check_discrepancy <- function(df, prefix_items, var_total, label) {
  cols_sum <- paste0(prefix_items, "_", financiadores_sfx)
  # Caso especial: altas_convIntern en un año, altas_conveInter en otro
  if (prefix_items == "altas") {
    cols_sum <- sub("convIntern", "convIntern", cols_sum)
  }
  cols_exist <- intersect(cols_sum, names(df))
  if (length(cols_exist) == 0 || !(var_total %in% names(df))) return(NULL)

  sumado <- rowSums(df[cols_exist], na.rm = TRUE)
  total  <- suppressWarnings(as.numeric(df[[var_total]]))
  pct    <- abs(total - sumado) / ifelse(total > 0, total, NA_real_) * 100

  n_disc <- sum(pct > 5, na.rm = TRUE)
  if (n_disc > 0) {
    message(sprintf("  %s: %d obs. con discrepancia > 5%%", label, n_disc))
  } else {
    message(sprintf("  %s: coherente.", label))
  }
  invisible(NULL)
}

check_discrepancy(df, "altas",       "total_altas",      "Altas")
check_discrepancy(df, "cma",         "total_cma",        "CMA")
check_discrepancy(df, "estancias",   "total_estancias",  "Estancias")
check_discrepancy(df, "urge",        "total_urgencias",  "Urgencias")
check_discrepancy(df, "consulta",    "total_consulta",   "Consultas")
check_discrepancy(df, "sesionHdia",  "total_sesionHdia", "Hosp.Día")

# ------------------------------------------------------------
# 6. Selección de variables para SFA
# ------------------------------------------------------------
message("\nSeleccionando variables para análisis SFA...")

# Variables a incluir (se usa any_of: si no existe, se omite sin error)
vars_sfa <- c(
  # --- Claves y control ---
  "NCODI", "anyo", "nombre_hospital", "ccaa", "ccaa_codigo",
  "ccaa_cnh", "provincia_cnh", "cod_centro", "CODCNH",
  "Depend_agrupada", "cod_depend_agrupada",
  "Finalidad_agrupada", "cod_finalidad_agrupada",

  # --- Variable de case-mix ---
  "peso", "mix",

  # --- Outputs hospitalarios ---
  "altFinal_total", "altFinal_cirugia", "altFinal_med",
  "altFinal_trauma", "altFinal_gine", "altFinal_pediatria",
  "altFinal_neonatologia", "altFinal_restoPedia",
  "aFinales_mIntensiva", "altFinal_uci",
  "altFinal_uCoronarios", "altFinal_uNeonatales",
  "altFinal_uQuemados",
  "altCuracion_total", "altTraslaHosp_total",
  "altExitus_total", "altOtracausa_total",
  "estancias_total", "estancias_medicina", "estancias_cirugia",
  "estancias_trauma", "estancias_gine", "estancias_pediatria",
  "estancias_neonatologia", "estancias_restoPedia",
  "estancias_mIntensiva", "estancias_uci",
  "estancias_uCoronarios", "estancias_uNeonatales",
  "estancias_uQuemados",
  "ingreProgr", "ingreUrge", "total_ingresosHops",
  "primTotal_hosp", "total_hosp", "primTotal_CEP", "total_CEP",
  "Urg_ingresos", "Urg_traslados", "Urg_exitus", "Urg_total",
  "total_altas", "total_estancias", "total_sesionHdia",
  "total_consulta", "total_cma", "total_urgencias", "total_hospDom",

  # --- Inputs capital ---
  "camas_funcionamiento", "quirofanos_funcionamiento",
  "incubadoras_funcionamiento", "paritorios_funcionamiento",
  "salasHemo_funcionamiento",

  # --- Inputs tecnología (equipamiento en propiedad) ---
  "TAC_total", "PET_total", "RNM_total", "gammacamara_total",
  "acelerador_total", "litotriptor_total", "bombas_total",
  "salas_rx_total", "mamografos_total", "densiometros_total",
  "angiografo_total", "hemodialisis_total", "SPECT_total",
  "altatecno", "tecno_basica", "tecnologia_total",

  # Equipamiento concertado (para análisis de sensibilidad)
  "acelerador_concertado", "angiografo_concertado",
  "bombas_concertado", "densiometros_concertado",
  "hemodialisis_concertado", "TAC_concertado", "PET_concertado",
  "RNM_concertado", "gammacamara_concertado",
  "litotriptor_concertado", "mamografos_concertado",
  "spect_concertado",

  # --- Actividad diagnóstica (para intensidad en script 14) ---
  "biopsias_total", "angio_total", "densiometrias_total",
  "gamma_total", "mamo_total", "pet_total",
  "resonancia_total", "rx_total", "spect_total", "tac_total",

  # --- Inputs trabajo ---
  "total_cMedicos", "total_pMedicos", "total_colabMedicos",
  "totalMir_total", "totalEir_total",
  "personal_medico", "personal_aux", "personal_resto",

  # --- Actividad por financiador ---
  "altas_particular", "estancias_particular", "sesionHdia_particular",
  "consulta_particular", "cma_particular", "urge_particular",
  "hospDom_particular",
  "altas_AsegPriv", "estancias_AsegPriv", "sesionHdia_AsegPriv",
  "consulta_AsegPriv", "cma_AsegPriv", "urge_AsegPriv",
  "hospDom_AsegPriv",
  "altas_sns", "estancias_sns", "sesionHdia_sns",
  "consulta_sns", "cma_sns", "urge_sns", "hospDom_sns",
  "altas_MutuaFun", "estancias_MutuaFun", "sesionHdia_MutuaFun",
  "consulta_MutuaFun", "cma_MutuaFun", "urge_MutuaFun",
  "hospDom_MutuaFun",
  "altas_otrEntiPublica", "estancias_otrEntiPublica",
  "sesionHdia_otrEntiPublica", "consulta_otrEntiPublica",
  "cma_otrEntiPublica", "urge_otrEntiPublica",
  "hospDom_otrEntiPublica",
  "altas_MATEPSS", "estancias_MATEPSS", "sesionHdia_MATEPSS",
  "consulta_MATEPSS", "cma_MATEPSS", "urge_MATEPSS",
  "hospDom_MATEPSS",
  "altas_convIntern", "estancias_conveInter",
  "sesionHdia_conveInter", "consulta_conveInter",
  "cma_conveInter", "urge_conveInter", "hospDom_conveInter",
  "altas_AccTrafic", "estancias_AccTrafic", "sesionHdia_AccTrafic",
  "consulta_AccTrafic", "cma_AccTrafic", "urge_AccTrafic",
  "hospDom_AccTrafic",
  "altas_otroRegim", "estancias_otroRegim", "sesionHdia_otroRegim",
  "consulta_otroRegim", "cma_otroRegim", "urge_otroRegim",
  "hospDom_otroRegim",

  # --- Ingresos y gastos (determinantes ineficiencia) ---
  "G_totalCompra", "G_variaExistencias", "G_servExteriores",
  "G_gastoPersonal", "G_dotaAmortizacion", "G_totGastos",
  "I_totIngresosPS", "I_particular", "I_AsegPriv",
  "I_AsistSanitaria", "I_AccTrafic", "I_MATEPSS", "I_SNS",
  "I_FdirectaSS", "I_FdirectaAPriv_MATEPSS", "I_OyEntiPublica",
  "I_bonificaciones", "I_Otros_Ips", "I_Total_Subvencion",
  "I_restoIngresos", "I_totIngresos",

  # --- Variables auxiliares de análisis ---
  "porcentaje_no_defuncion", "intensidad_docente"
)

# Verificar qué variables solicitadas no están en df
vars_faltantes <- setdiff(vars_sfa, names(df))
if (length(vars_faltantes) > 0) {
  message("  Variables solicitadas NO encontradas en df_depurado (",
          length(vars_faltantes), "):")
  print(vars_faltantes)
}

df_seleccion <- df |> select(any_of(vars_sfa))

message("df_seleccion: ", nrow(df_seleccion), " filas x ",
        ncol(df_seleccion), " columnas.")

# ------------------------------------------------------------
# 7. Asignar etiquetas por nombre (robusto, no por posición)
# ------------------------------------------------------------
etiquetas_sfa <- c(
  NCODI                       = "Código de centro anonimizado",
  anyo                        = "Año",
  nombre_hospital             = "Nombre del hospital",
  ccaa                        = "Comunidad autónoma",
  ccaa_codigo                 = "Código INE comunidad autónoma",
  ccaa_cnh                    = "CCAA (CNH)",
  provincia_cnh               = "Provincia (CNH)",
  cod_centro                  = "Código de centro (CNH)",
  CODCNH                      = "Código CNH",
  Depend_agrupada             = "Dependencia funcional agrupada",
  cod_depend_agrupada         = "Código dependencia agrupada",
  Finalidad_agrupada          = "Finalidad asistencial agrupada",
  cod_finalidad_agrupada      = "Código finalidad agrupada",
  peso                        = "Peso medio GRD (real)",
  mix                         = "Proxy case-mix (imputado RF)",
  altFinal_total              = "Altas totales hospital",
  altFinal_cirugia            = "Altas cirugía",
  altFinal_med                = "Altas medicina interna",
  altFinal_trauma             = "Altas traumatología",
  altFinal_gine               = "Altas ginecología",
  altFinal_pediatria          = "Altas pediatría",
  altFinal_neonatologia       = "Altas neonatología",
  altFinal_restoPedia         = "Altas resto pediatría",
  aFinales_mIntensiva         = "Altas medicina intensiva",
  altFinal_uci                = "Altas UCI",
  altFinal_uCoronarios        = "Altas unidad coronaria",
  altFinal_uNeonatales        = "Altas unidad neonatal",
  altFinal_uQuemados          = "Altas unidad quemados",
  altCuracion_total           = "Altas por curación total",
  altTraslaHosp_total         = "Altas por traslado hospital",
  altExitus_total             = "Altas por fallecimiento",
  altOtracausa_total          = "Altas otras causas",
  estancias_total             = "Estancias totales",
  estancias_medicina          = "Estancias medicina interna",
  estancias_cirugia           = "Estancias cirugía",
  estancias_trauma            = "Estancias traumatología",
  estancias_gine              = "Estancias ginecología",
  estancias_pediatria         = "Estancias pediatría",
  estancias_neonatologia      = "Estancias neonatología",
  estancias_mIntensiva        = "Estancias medicina intensiva",
  estancias_uci               = "Estancias UCI",
  estancias_uCoronarios       = "Estancias unidad coronaria",
  estancias_uNeonatales       = "Estancias unidad neonatal",
  camas_funcionamiento        = "Camas en funcionamiento",
  quirofanos_funcionamiento   = "Quirófanos en funcionamiento",
  incubadoras_funcionamiento  = "Incubadoras en funcionamiento",
  paritorios_funcionamiento   = "Paritorios en funcionamiento",
  salasHemo_funcionamiento    = "Salas hemodinámica funcionamiento",
  TAC_total                   = "Equipos TAC (total)",
  PET_total                   = "Equipos PET (total)",
  RNM_total                   = "Equipos RNM (total)",
  gammacamara_total           = "Gammacámaras (total)",
  acelerador_total            = "Aceleradores lineales (total)",
  litotriptor_total           = "Litotriptores (total)",
  bombas_total                = "Bombas de cobalto (total)",
  salas_rx_total              = "Salas de rayos X (total)",
  mamografos_total            = "Mamógrafos (total)",
  densiometros_total          = "Densitómetros (total)",
  angiografo_total            = "Angiógrafos (total)",
  hemodialisis_total          = "Equipos hemodiálisis (total)",
  SPECT_total                 = "SPECT (total)",
  altatecno                   = "Índice alta tecnología",
  tecno_basica                = "Índice tecnología básica",
  tecnologia_total            = "Índice tecnología total",
  total_cMedicos              = "Médicos jornada completa",
  total_pMedicos              = "Médicos jornada parcial",
  total_colabMedicos          = "Médicos colaboradores",
  totalMir_total              = "Total MIR",
  totalEir_total              = "Total EIR",
  personal_medico             = "Personal médico total",
  personal_aux                = "Personal enfermería y técnico-sanitario",
  personal_resto              = "Personal no sanitario y administrativo",
  G_totalCompra               = "Compras",
  G_gastoPersonal             = "Gastos de personal",
  G_totGastos                 = "Total gastos",
  I_totIngresosPS             = "Ingresos por prestación de servicios",
  I_particular                = "Ingresos particulares",
  I_AsegPriv                  = "Ingresos aseguradoras privadas",
  I_AsistSanitaria            = "Ingresos seguros asistencia sanitaria",
  I_AccTrafic                 = "Ingresos accidentes tráfico",
  I_MATEPSS                   = "Ingresos MATEPSS",
  I_SNS                       = "Ingresos del SNS",
  I_FdirectaSS                = "Financiación directa SS",
  I_OyEntiPublica             = "Ingresos otras entidades públicas",
  I_bonificaciones            = "Bonificaciones",
  I_Otros_Ips                 = "Otros ingresos",
  I_Total_Subvencion          = "Subvenciones y donaciones",
  I_restoIngresos             = "Resto de ingresos",
  I_totIngresos               = "Total ventas e ingresos",
  porcentaje_no_defuncion     = "% altas por no defunción",
  intensidad_docente          = "Intensidad docente (MIR + EIR)"
)

if (requireNamespace("sjlabelled", quietly = TRUE)) {
  library(sjlabelled)
  for (v in names(etiquetas_sfa)) {
    if (v %in% names(df_seleccion)) {
      df_seleccion[[v]] <- sjlabelled::set_label(
        df_seleccion[[v]], label = etiquetas_sfa[[v]]
      )
    }
  }
  message("Etiquetas asignadas con sjlabelled.")
} else {
  message("sjlabelled no disponible: etiquetas no asignadas.")
}

# ------------------------------------------------------------
# 8. Verificar unicidad de clave
# ------------------------------------------------------------
n_dup <- sum(duplicated(df_seleccion[, c("NCODI", "anyo")]))
if (n_dup > 0) {
  stop("Duplicados (NCODI, anyo) en df_seleccion: ", n_dup, call. = FALSE)
}

# ------------------------------------------------------------
# 9. Guardar
# ------------------------------------------------------------
save(df_seleccion, file = DF_SELECCION_RDATA_PATH)

write.csv(df_seleccion, DF_SELECCION_CSV_PATH, row.names = FALSE, na = "")

message("df_seleccion guardado en: ", DF_SELECCION_RDATA_PATH)
message("\n=== Script 13 completado ===")
message("Filas: ", nrow(df_seleccion), " | Columnas: ", ncol(df_seleccion))
