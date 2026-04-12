PIPELINE SIAE 2010-2023 — ORDEN DE EJECUCIÓN
=============================================
Proyecto: Eficiencia hospitalaria (SFA) - Tesis doctoral
Dataset:  Panel longitudinal SIAE 2010-2023 (10.614 obs, 884 hospitales)
Última actualización: 2026-03-21

=============================================
Bloque 1 — Construcción del panel longitudinal
=============================================
Ejecutar en orden estricto:

  1. 00_config.R
     - Configuración central: rutas, constantes
     - Source automático al inicio de cada script
     - NO ejecutar manualmente

  2. 01_exportar_mapeo_cnh.R
     - Genera ncodi_hospital_map.csv desde el CNH
     - Ejecutar UNA SOLA VEZ (o si cambia el CNH)

  3. 02_estandarizar_nombres_variables.R
     - Lee TXT/XML de data_raw/, estandariza nombres
     - Escribe data_intermediate/standardized_raw/<anyo>/
     - USA var_mapping_explicit_FROZEN.csv (inmutable)
     - Para regenerar el mapeo: ejecutar primero 03

  4. 03_unir_modulos_por_anyo.R
     - Bloque 0: normalización de nombres con fuzzy matching
     - Paso 1: une módulos por año → data_legacy_outputs/df_YYYY.csv
     - Paso 2: auditoría de variables entre años (Jaro-Winkler)
     - ESCRIBE SOLO var_mapping_diagnostic.csv (no sobreescribe FROZEN)

  5. 04_construir_panel_longitudinal.R
     - Apila df_YYYY.csv → df_final
     - Elimina columnas zero-variance, detecta colinealidad perfecta
     - Output: data_legacy_outputs/df_final.RData (~10.696 obs × 1.537 col)

=============================================
Bloque 2 — Variables para SFA
=============================================
Continuar sobre df_final (requiere Bloque 1 completo):

  6. 05_construir_casemix_outputs.R
     - Construye peso_grd_final (Modelo A: OLS, Modelo B: LASSO+RF)
     - Outputs: altQ_pond, altM_pond, altTotal_pond, ShareQ
     - Log-transformaciones: ln_altQ_pond, ln_altM_pond, ln_altTotal_pond
     - Modelo RF cacheado en casemix_modelo_B_rf.rds
       (borrar el .rds para forzar re-entrenamiento)

  7. 06_construir_Ddesc_pago_inputs.R
     - D_desc: dependencia funcional (cruce SIAE + CNH)
     - Pago: pct_sns, pct_privado, pct_mutuas, pct_ingr_SNS
     - L (trabajo): L_total, L_medico, L_quirur + logs
     - K (capital): K_camas, K_tech_index, K_quirofanos + logs

  8. 07_construir_intensidad.R
     - Índice i_diag: suma ponderada de 13 procedimientos diagnósticos
     - Bridge 2020→2021: biopsias_hosp+CEP ↔ totbiopsias, etc.
     - IMPORTANTE: usa solo variables C1_16, NO C1_04 (dotación)
     - i_simple como variable auxiliar

=============================================
Bloque 3 — Dataset SFA final
=============================================
Requiere Bloque 2 completo:

  9. 08_auditoria_variables_sfa.R
     - Verifica existencia y cobertura de todas las variables del modelo
     - Identifica hospitales problemáticos (>50% NAs en variables clave)
     - Output: cobertura_variables_sfa.csv, obs_utilizables_sfa.csv,
               hospitales_problematicos_sfa.csv

  10. 09_crear_df_sfa.R
      - Selecciona ~64 variables del modelo SFA
      - Filtros: excluye obs sin actividad, sin personal, sin camas
      - Construye términos translog centrados (ln_L_c, ln_K_c, etc.)
      - Interacciones para ecuación de ineficiencia (desc_pago, etc.)
      - Output: data_intermediate/df_sfa.RData, df_sfa.csv

  11. 10_descriptivos_sfa.R
      - Tabla 1: descriptivos por grupo D_desc (centralizado/descentralizado)
      - Tabla 2: evolución temporal de variables clave
      - Tabla 3: cobertura i_diag por D_desc × año
      - Output: tabla1_descriptivos.csv, tabla2_evolucion.csv,
                tabla3_cobertura_intensidad.csv

=============================================
Notas técnicas
=============================================

Archivos clave en data_intermediate/:
  - var_mapping_explicit_FROZEN.csv   → mapeo de nombres INMUTABLE
  - var_mapping_diagnostic.csv        → diagnóstico (regenerable)
  - casemix_modelo_B_rf.rds           → modelo RF cacheado
  - pesos.txt                         → pesos GRD (fuente externa)
  - df_sfa.RData                      → dataset final para sfaR

Variables protegidas (nunca renombrar):
  - u1..u104, u900  (Unidades Asistenciales R.D. 1277/2003)
  - NCODI, anyo     (clave del panel)

Cambio estructural SIAE 2021 (C1_16):
  - Hasta 2020: variables individuales (proc_hosp + proc_CEP)
  - Desde 2021: variables totales (totproc = hosp + CEP integrado)
  - Endoscopias: Broncoscopia/Colonosopia/ERCP → Bron_hosp+Amb, etc.
  - Ver auditoria_c1_16_nombres.csv y propuesta_armonizacion_c1_16.csv

Versiones archivadas:
  - scripts/legacy/   → scripts obsoletos del pipeline anterior
  - data_legacy_outputs/ → df_final_limpio.*, df_final_validado.*
    (versiones intermedias, NO borrar hasta verificar df_final.RData)

Reproducibilidad:
  - Pipeline determinista dado el mismo data_raw/ y los RDS cacheados
  - set.seed(123) en todos los modelos con aleatoriedad
  - Para reproducción completa desde cero: borrar standardized_raw/,
    data_legacy_outputs/df_final.RData, y todos los .rds de INT_DIR

=============================================
