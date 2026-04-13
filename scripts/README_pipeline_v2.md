# Pipeline SIAE 2010-2023 — Documentación técnica
## Proyecto: Eficiencia hospitalaria, estructura organizativa y teoría de agencia
## José María Elena Izquierdo — Universidad de Salamanca, 2026

---

## Estructura de directorios

```
G:/Mi unidad/SIAE 2010-2020/
│
├── data_raw/               # Datos originales anuales (TXT 2010-2015, XML 2016-2023)
│   ├── 2010/               # Módulos por año (01-C1_01, 03-C1_03, etc.)
│   └── ...
│
├── data_intermediate/      # Datos procesados intermedios
│   ├── standardized_raw/   # TXT/XML con nombres de variables canónicos (script 02)
│   ├── ncodi_hospital_map.csv
│   ├── var_mapping_explicit.csv / _FROZEN.csv
│   └── [archivos de auditoría y log]
│
├── data_clean/             # Outputs finales para análisis
│   ├── df_final.RData      # Panel longitudinal completo
│   ├── df_sfa.RData        # Muestra SFA con todas las variables
│   └── sfa_*.rds           # Modelos SFA estimados
│
├── data_legacy_outputs/    # Artefactos legacy (NO MODIFICAR)
│
├── scripts/                # Scripts activos del pipeline
│   ├── legacy/             # Scripts archivados (no ejecutar)
│   └── README_pipeline_v2.md
│
├── outputs/                # Tablas, figuras, robustez
│   └── tablas_tesis/
│
└── docs/                   # Documentos y archivos de referencia
    ├── NCODI_Hospital_DEFINITIVO_2020_2024.xlsx
    └── Textos/capitulo_empirico/
```

---

## Especificación econométrica

El pipeline estima modelos de **Frontera Estocástica (SFA)** según la especificación de Battese & Coelli (1995), implementada con `frontier::sfa()` en R:

```
y_it = f(x_it; β) · exp(v_it - u_it)
u_it ~ N+(μ_it, σ_u²)    con  μ_it = z'_it · δ  (muhet)
v_it ~ N(0, σ_v²)
```

Parámetros clave: `ineffDecrease=TRUE`, `truncNorm=TRUE` (distribución truncada-normal), `maxit=5000`.

**Variables de output:**
- **Cantidad:** `ln_altTotal_pond` = log(altas totales × peso_grd_final)
- **Intensidad:** `ln_i_diag_w` = log(i_diag) winsorizado a p1/p99

**Variables institutionales (z-equation):**
- `D_desc`: 0=público, 1=privado (de `cod_depend_agrupada`)
- `pct_sns`: fracción de altas financiadas por el SNS
- `desc_pago`: interacción D_desc × pct_sns
- `ShareQ`: fracción de altas quirúrgicas sobre total

**Inputs:**
- `ln_L_total_c`: log trabajo total centrado
- `ln_K_camas_c`: log capital camas centrado
- `ln_K_tech_c` / `ln_K_tech_diag_c`: log índice tecnológico centrado

**Muestra base (Diseño A):**
- `es_agudo==1`, excluye 2020-2022, `altTotal_bruto>=200`
- N ≈ 5,332 obs hospital-año

---

## Scripts activos

| Script | Función | Inputs principales | Outputs principales | Notas |
|--------|---------|-------------------|---------------------|-------|
| `00_config.R` | Configuración central | — | Variables de ruta globales | Source() al inicio de cada script |
| `01_exportar_mapeo_cnh.R` | Exporta mapeo NCODI↔hospital desde CNH | `NCODI_Hospital_DEFINITIVO_2020_2024.xlsx` | `data_intermediate/ncodi_hospital_map.csv` | Requiere hoja "🗂 Correspondencia" |
| `02_estandarizar_nombres_variables.R` | Estandariza nombres de variables en TXT/XML | `data_raw/YYYY/*.txt|xml`, `var_mapping_explicit.csv` | `data_intermediate/standardized_raw/YYYY/` | Protege u1-u104, u900, NCODI, anyo |
| `03_unir_modulos_por_anyo.R` | Une módulos del mismo año en un df_YYYY | `data_intermediate/standardized_raw/YYYY/` | `data_legacy_outputs/df_YYYY.csv` | Une por NCODI dentro de cada año |
| `04_construir_panel_longitudinal.R` | Apila df_YYYY en panel longitudinal | `data_legacy_outputs/df_YYYY.csv`, `ncodi_hospital_map.csv` | `data_legacy_outputs/df_final.RData` | Detecta conflictos de tipo, elimina all-NA |
| `05_construir_casemix_outputs.R` | Construye peso_grd y outputs ponderados | `df_final.RData`, `data_intermediate/pesos.txt` | `df_final.RData` enriquecido (altTotal_pond, ShareQ, etc.) | Modelos LASSO+RF; train 2010-2013, test 2014-2015 |
| `07_construir_Ddesc_inputs_v2.R` | Construye D_desc, pct_sns, inputs L/K, tendencia, clusters | `data_clean/df_final.RData` | `data_clean/df_final.RData` + variables nuevas | Integra fixes de CCAA y finalidad (antiguo 07b) |
| `08_construir_intensidad_v2.R` | Construye índice de intensidad i_diag con bridge pre/post 2021 | `data_clean/df_final.RData` | `data_clean/df_final.RData` + i_diag, ln_i_diag_w | Bridge: ≤2020 usa hosp+CEP; ≥2021 usa tot* |
| `09_crear_df_sfa_v2.R` | Auditoría y construcción de df_sfa para estimación | `data_clean/df_final.RData` | `data_clean/df_sfa.RData`, `auditoria_variables.csv` | Verifica cobertura de todas las variables SFA |
| `10_estimar_sfa_A_v2.R` | Estima Diseño A — Frontera de cantidad | `df_sfa.RData`, `df_final.RData` | `sfa_A_total_v2.rds`, `sfa_A_total_withShareQ_v2.rds` | Principal: noShareQ; robustez: withShareQ |
| `10b_estimar_sfa_A_intensidad_v2.R` | Estima Diseño A — Frontera de intensidad | `df_sfa.RData`, `df_final.RData` | `sfa_A_intensity_v2.rds`, `sfa_A_intensity_withShareQ_v2.rds` | Filtro i_diag≥1.0; dep var ln_i_diag_w (winsorizada) |
| `11_estimar_sfa_BCD_v2.R` | Estima Diseños B (quirúrgico/médico), C (panel servicio), D (multioutput ODF) | `df_sfa.RData`, `df_final.RData` | `sfa_B_Q_v3.rds`, `sfa_B_M_v3.rds`, `sfa_C_stacked_service_v3.rds`, `sfa_D_v4_odf.rds`, `sfa_D_v5_cantidad.rds` | Umbral B/C: altQ≥200 y altM≥200 |

---

## Scripts de estimación SFA

### Diseño A — Frontera única de cantidad e intensidad

**Script:** `10_estimar_sfa_A_v2.R` y `10b_estimar_sfa_A_intensidad_v2.R`

**Muestra:** `es_agudo==1`, excluye años 2020-2022, `altTotal_bruto>=200`, sin NA en variables clave, CCAA válida.

**Frontera (Translog):**
```
ln_altTotal_pond ~ ln_L_total_c + ln_K_camas_c + ln_K_tech_c
                 + ln_L_total_c2 + ln_K_camas_c2 + ln_K_tech_c2
                 + ln_L_total_c:ln_K_camas_c + ln_L_total_c:ln_K_tech_c
                 + ln_K_camas_c:ln_K_tech_c
                 + trend + trend2
                 + d_cluster2 + d_cluster3 + d_cluster4 + d_cluster5
```

**Z-equation (muhet):**
- Principal (noShareQ): `~ D_desc + pct_sns + desc_pago`
- Robustez (withShareQ): `~ D_desc + pct_sns + desc_pago + ShareQ`

**Intensidad:** igual estructura pero dep var = `ln_i_diag_w`; filtro adicional `i_diag >= 1.0`.

---

### Diseño B — Fronteras quirúrgica y médica separadas

**Script:** `11_estimar_sfa_BCD_v2.R`

**Muestra:** misma base + `altQ_bruto>=200` (B_Q) o `altM_bruto>=200` (B_M).

**B_Q:** dep var = `ln_altQ_pond`; inputs específicos para servicio quirúrgico (L_quirur, K_quirofanos).
**B_M:** dep var = `ln_altM_pond`; inputs médicos (L_medico, K_camas, K_tech_diag).

**Z-equation B:** `~ d_Priv_Conc + d_Priv_Merc + ShareQ + Conc_shareQ + Merc_shareQ`

---

### Diseño C — Panel hospital × servicio

**Script:** `11_estimar_sfa_BCD_v2.R`

**Estructura:** panel apilado con 2 filas por hospital-año (quirúrgica + médica).
Incluye dummies de servicio `C_s` e interacciones `Priv_Conc_Cs`, `Priv_Merc_Cs`, `ShareQ_Cs`.

---

### Diseño D — Output Distance Function (multioutput)

**Script:** `11_estimar_sfa_BCD_v2.R`

**D_v4:** dep var = `-ln(altQ_pond)` (ODF estándar), con `ln_altM_pond` y `ln_i_diag` como outputs adicionales.
**D_v5:** dep var = `ln_altTotal_pond`, con `ln_i_w` como control de intensidad. Seleccionado (skewness negativa confirmada).

---

## Notas técnicas críticas

- **NCODI siempre como integer antes de joins:** En scripts 07-11, `NCODI` debe ser forzado a entero (`as.integer`) antes de uniones para evitar discrepancias de tipo.
- **Winsorización de `ln_i_diag`:** aplicada a p1/p99 en la muestra de estimación (script 10b) para evitar que hospitales con intensidad extrema hagan `gamma→1`. Guardada como `ln_i_diag_w`.
- **Filtro `i_diag >= 1.0`:** en Diseño A intensidad, excluye hospitales sin actividad diagnóstica mínima. Criterio size-neutral (procedimientos por alta, no total).
- **Umbral `altQ_bruto >= 200` y `altM_bruto >= 200`:** aplicado en Diseños B y C para garantizar tamaño muestral suficiente en cada servicio.
- **Inputs específicos por servicio en B y C:** B_Q usa `K_quirofanos`; B_M usa `K_tech_diag`. Evita errores de medición cruzados.
- **Clusters de tamaño:** 5 grupos basados en `altTotal_bruto`. Dummy de referencia: cluster 1 (hospitales pequeños). Dummies `d_cluster2`–`d_cluster5` en la frontera de producción.
- **Exclusión años COVID 2020-2022:** en Diseño A (cantidad e intensidad) para evitar sesgo por disrupciones pandémicas. Diseño D incluye estos años con dummy de control.
- **Bridge intensidad pre/post 2021:** en ≤2020 `i_diag` = (procedimientos_hosp + procedimientos_CEP) / altTotal_bruto; en ≥2021 = totales* / altTotal_bruto. Implementado en script 08.
- **`ineffDecrease=TRUE`:** los coeficientes z positivos aumentan la ineficiencia. Un coeficiente positivo sobre `D_desc` significa que los privados son MÁS ineficientes (o menos eficientes).
