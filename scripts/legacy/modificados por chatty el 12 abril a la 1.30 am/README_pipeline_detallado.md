# Pipeline de datos y estimación SFA
## Tesis doctoral: Eficiencia hospitalaria, estructura organizativa y teoría de agencia
### José María Elena Izquierdo — Universidad de Salamanca, 2026

---

## Estructura de directorios

```
G:/Mi unidad/SIAE 2010-2020/
├── data_raw/          — Datos originales SIAE (NO modificar)
│   ├── 2010/–2015/    — Módulos TXT separados por punto y coma
│   └── 2016/–2023/    — Módulos XML
├── data_legacy_outputs/ — Datos intermedios del pipeline antiguo (NO modificar)
│   └── df_final.RData — Panel completo con variables originales
├── data_intermediate/ — Resultados intermedios y modelos estimados
├── data_clean/        — Resultados definitivos del análisis actual
└── scripts/           — Pipeline activo (este documento)
```

---

## Estimador y especificación SFA

- **Paquete:** `frontier::sfa()` versión 1.1
- **Especificación:** BC95 — Efficiency Effects Frontier (Battese & Coelli, 1995)
- **Parámetros:** `ineffDecrease=TRUE`, `maxit=5000`
- **Heterogeneidad:** `muhet` — z-variables en la media de u_it (μ_it = z'δ)
- **Nota técnica:** `ln_i_diag` winsorizado a p1/p99 en fronteras de intensidad para evitar patología gamma→1 (skewness OLS = −3.48, distribución apta para SFA de producción)

---

## Scripts del pipeline

---

### 00_config.R
**Configuración central del proyecto.**

Define todas las rutas del proyecto como variables globales:
- `RAW_DIR`: ruta a `data_raw/`
- `INT_DIR`: ruta a `data_intermediate/`
- `CLEAN_DIR`: ruta a `data_clean/`
- `DF_FINAL_RDATA_PATH`: ruta al archivo `df_final.RData`
- `PROTECTED_VARS`: vector con variables que nunca se eliminan (`u1`–`u104`, `u900`, `NCODI`, `anyo`)

Todos los scripts hacen `source("scripts/00_config.R")` al inicio. Modificar rutas aquí afecta a todo el pipeline.

---

### 01_exportar_mapeo_cnh.R
**Construye el crosswalk NCODI ↔ CODCNH.**

Carga el archivo `docs/NCODI_Hospital_DEFINITIVO_2020_2024.xlsx` (Catálogo Nacional de Hospitales) y genera `data_intermediate/ncodi_hospital_map.csv` con columnas:
- `NCODI`: código hospital SIAE (entero)
- `CODCNH`: código CNH de 7 dígitos
- `dependencia_cnh`: categoría institucional CNH (texto)
- `pertenencia_sns_cnh`: "Públicos-SNS" / "Privados"
- `finalidad_cnh`: "Agudos" / "Otros"
- `ccaa_cnh`: comunidad autónoma según CNH

Este mapa cubre ~753 hospitales únicos (no todos los años). Se aplica de forma cross-sectional (mismo valor para todos los años de un hospital).

**Ejecutar una sola vez.** Si el archivo Excel cambia, re-ejecutar.

---

### 02_estandarizar_nombres_variables.R
**Armoniza nombres de variables entre formatos TXT (2010–2015) y XML (2016–2023).**

El SIAE cambió de formato TXT a XML en 2016, introduciendo nombres de variables con caracteres especiales (e.g., `esc_x0020_Pertenencia_x0020_SNS`). Este script:

1. Lee el mapeo canónico congelado desde `var_mapping_explicit_FROZEN.csv`
2. Renombra variables XML a sus equivalentes canónicos
3. Aplica fuzzy matching (Jaro-Winkler, umbral 0.85) para variables sin correspondencia exacta
4. Escribe diagnósticos en `var_mapping_diagnostic.csv` (no sobreescribe el fuente)

**Variables críticas armonizadas:**
- `cod_depend_agrupada` (TXT) ↔ `esc_x0020_Pertenencia_x0020_SNS` (XML)
- Módulos C1_16 de procedimientos diagnósticos (bridges 2021+)

---

### 03_unir_modulos_por_anyo.R
**Une los módulos SIAE de cada año en un único dataframe anual.**

Para cada año 2010–2023:
1. Lee todos los módulos disponibles (C1_01 filiación, C1_03 recursos, C1_04 equipamiento, C1_05/06 personal, C1_12 hospitalización, C1_14 ambulatorio, C1_16 diagnósticos, C1_18 financiación)
2. Une por `NCODI` (clave de hospital)
3. Guarda `data_intermediate/df_YYYY.csv`

Produce un archivo por año con todas las variables del SIAE disponibles para ese año.

---

### 04_construir_panel_longitudinal.R
**Apila los dataframes anuales en un panel longitudinal.**

1. Carga `df_YYYY.csv` para cada año disponible
2. Verifica que las variables clave existen en todos los años
3. Apila en `df_final` con clave `(NCODI, anyo)`
4. Verifica ausencia de duplicados en la clave
5. Guarda en `DF_FINAL_RDATA_PATH` (normalmente `data_legacy_outputs/df_final.RData`)

Panel resultante: ~10.600–11.000 observaciones hospital-año × ~1.500 variables.

**Importante:** `NCODI` debe coercionarse a entero antes de cualquier join posterior:
```r
df$NCODI <- as.integer(df$NCODI)
```

---

### 05_construir_casemix_outputs.R
**Construye los outputs ponderados por case-mix y la variable ShareQ.**

1. **Altas brutas** (desde C1_12):
   - `altQ_bruto` = altas quirúrgicas + traumatología
   - `altM_bruto` = altas médicas
   - `altTotal_bruto` = altas totales
   - `ShareQ` = altQ_bruto / altTotal_bruto (proxy de ψ₁₂)

2. **Pesos GRD-APR** (índice case-mix):
   - Reales para 2010–2015, ~246 hospitales (desde `data_legacy_outputs/pesos.txt`)
   - Imputados con Random Forest (`casemix_modelo_B_rf.rds`) para el resto
   - Si el modelo RF existe: cargarlo (NO re-entrenar)
   - Si no existe: entrenar con y=log(peso_grd_medio), X={ShareQ, pct_uci, pct_urgente, log(camas+1), K_tech, anyo}
   - `peso_grd_final` recortado a [0.40, 2.50]

3. **Outputs ponderados:**
   - `altTotal_pond` = altTotal_bruto × peso_grd_final
   - `altQ_pond` = altQ_bruto × peso_grd_final
   - `altM_pond` = altM_bruto × peso_grd_final
   - `ln_altTotal_pond` = log(altTotal_pond) [NA si ≤ 0]
   - `ln_altQ_pond`, `ln_altM_pond` análogos

---

### 06_construir_panel_xml.R
**Fix crítico: extrae `cod_depend_agrupada` de los XML 2016–2023.**

El pipeline original (scripts 02–04) no extraía correctamente la variable de dependencia institucional de los archivos XML. En los TXT 2010–2015 existe como `cod_depend_agrupada` (valores 1=público, 2=privado). En los XML 2016–2023 se llama `esc_x0020_Pertenencia_x0020_SNS` con valores "Públicos-SNS" / "Privados".

Este script:
1. Lee los XML de cada año 2016–2023
2. Localiza la etiqueta de pertenencia SNS
3. Mapea: "Públicos-SNS" → cod_depend_agrupada=1; "Privados" → cod_depend_agrupada=2
4. Une con `df_final` por (NCODI, anyo)
5. Verifica que `cod_depend_agrupada` es no-NA para todos los años

**Validación:** % con cod=2 debe ser estable ~40–45% en todos los años 2010–2023.

---

### 07_construir_Ddesc_inputs.R
**Construye la variable institucional D_desc y los inputs del modelo SFA.**

**D_desc (variable más crítica del análisis):**
```r
D_desc = case_when(
  cod_depend_agrupada == 1 ~ 0L,  # público SNS (centralizado)
  cod_depend_agrupada == 2 ~ 1L,  # privado (descentralizado)
  TRUE ~ NA_integer_)
```
Target: ~42–48% D_desc=1, estable por año.

**Variable de pago:**
- `pct_sns` = altas_SNS / altTotal_bruto (desde C1_18), recortado a [0,1]

**Inputs trabajo** (desde C1_05/06, sin colaboradores externos `_colab`):
- `L_total` = (total_cMedicos + 0.5×total_pMedicos) + (due_cTotal + 0.5×due_pTotal)
- `L_quirur` = subQuirurgicas_cTotal + 0.5×subQuirurgicas_pTotal
- `L_medico` = subMedicas_cTotal + 0.5×subMedicas_pTotal

**Capital:**
- `K_camas` = camas_funcionamiento (C1_03)
- `K_tech_index` = TAC×1.0 + RNM×2.5 + PET×5.0 + acelerador×4.0 + angiografo×1.5 + gammacamara×1.8 + SPECT×1.8

**Inputs log-centrados** (centrados en media muestral para translog):
- `ln_L_total_c` = log(L_total+1) − media(log(L_total+1))
- `ln_K_camas_c`, `ln_K_tech_c` análogos
- `ln_L_total_c2` = 0.5 × ln_L_total_c² ; `ln_K_camas_c2` análogo
- **Nota:** `ln_LK_c` (cruce L×K) eliminado por multicolinealidad r=0.953

**Tendencia:** `trend` = anyo − 2010; `trend2` = 0.5 × trend²

**Interacciones:**
- `desc_pago` = D_desc × pct_sns
- `desc_shareQ` = D_desc × ShareQ

**Cluster hospitalario** (5 grupos, medianas por hospital):
- Grupo 1: <100 camas y tech≤1 (comarcal)
- Grupo 2: <200 camas y tech≤2
- Grupo 3: <400 camas y tech≤4
- Grupo 4: <700 camas
- Grupo 5: gran hospital terciario
- Dummies `d_cluster2`–`d_cluster5` (referencia: grupo 1)

**Grupos de pago** (para Diseños B, C, D):
- `Pub_Retro`: D_desc=0
- `Priv_Conc`: D_desc=1 y pct_sns≥0.50
- `Priv_Merc`: D_desc=1 y pct_sns<0.50
- Dummies `d_Priv_Conc`, `d_Priv_Merc`
- Interacciones `Conc_shareQ`, `Merc_shareQ`

---

### 07b_fix_esagudo_ccaa.R
**Fix: construye `es_agudo` y dummies de CCAA.**

`es_agudo`:
- 1 si `finalidad_cnh == "Agudos"` (excluye psiquiátricos, larga estancia, monoespecialistas)
- 0 en caso contrario

CCAA (desde SIAE — disponible todos los años, sin necesidad del CNH):
- `ccaa_cod`: entero 1–17 según codificación INE
- Dummies `d_ccaa_1`–`d_ccaa_17`; referencia = Cataluña (ccaa_cod=9)

---

### 08_construir_intensidad.R
**Construye el índice de intensidad diagnóstica `i_diag`.**

Fuente: módulo C1_16 del SIAE (procedimientos diagnósticos).

**Bridges para cambio de denominación en 2021** (versión 3, validada):

| Variable armonizada | Fuente ≤2020 | Fuente ≥2021 |
|---------------------|-------------|-------------|
| biopsias | biopsias_hosp + biopsias_CEP | totbiopsias |
| tac_tot | tac_hosp + tac_CEP | tottac |
| resonancia_tot | resonancia_hosp + resonancia_CEP | totresonancia |
| colonoscopia | Colonosopia (sic) | Col_hosp + Col_Amb |
| pet_tot | pet_hosp + pet_CEP | totpet |
| rx_tot | rx_hosp + rx_CEP | totrx |
| … (spect, angio, gamma, mamo, densio, bronco, ercp) | hosp+CEP | tot* |

**Pesos de procedimientos (wp):**
TAC=1.0, RNM=2.0, PET=5.0, angiografía=2.5, gamma/SPECT=2.0,
colonoscopia=0.8, broncoscopia=0.7, ERCP=1.5, biopsia=0.5,
Rx=0.2, mamografía=0.3, densitometría=0.2

**Construcción:**
```
i_diag   = (Σ wp × proc_count) / altTotal_pond
i_simple = (consultas_ext + sesiones_hdia) / altTotal_bruto
ln_i_diag   = log(i_diag)    [NA si i_diag ≤ 0]
ln_i_simple = log(i_simple+1)
```

**Validación (mediana i_diag por año):**
2010:7.9 → 2015:8.7 → 2019:9.4 → 2023:9.5
Sin salto >15% entre años adyacentes.

---

### 09_crear_df_sfa.R
**Produce el dataset reducido `df_sfa` listo para estimación SFA.**

Selecciona las variables necesarias para la estimación (outputs, inputs, determinantes de ineficiencia, controles) y guarda en `data_clean/df_sfa.RData` (objeto: `df_sfa`).

Variables incluidas: `NCODI`, `anyo`, `ln_altTotal_pond`, `ln_altQ_pond`, `ln_altM_pond`, `ln_i_diag`, `ln_i_simple`, `i_diag`, `altTotal_bruto`, `altQ_bruto`, `altM_bruto`, `ln_L_total_c`, `ln_K_camas_c`, `ln_K_tech_c`, `ln_L_total_c2`, `ln_K_camas_c2`, `trend`, `trend2`, `D_desc`, `pct_sns`, `ShareQ`, `desc_pago`, `desc_shareQ`, `d_Priv_Conc`, `d_Priv_Merc`, `Conc_shareQ`, `Merc_shareQ`, `d_cluster2`–`d_cluster5`, `d_ccaa_1`–`d_ccaa_17`, `ccaa_cod`, `es_agudo`, `grupo_pago`, `cod_depend_agrupada`.

---

### 10_estimar_sfa_A.R
**Diseño A — Frontera de cantidad (ln_altTotal_pond).**

Muestra: `es_agudo==1`, `!anyo %in% 2020:2022`, `altTotal_bruto≥200`, sin NA en variables de modelo.

Especificación:
```r
frontier::sfa(
  ln_altTotal_pond ~
    ln_L_total_c + ln_K_camas_c + ln_K_tech_c +
    ln_L_total_c2 + ln_K_camas_c2 + trend + trend2 +
    d_cluster2 + d_cluster3 + d_cluster4 + d_cluster5 |
    D_desc + pct_sns + desc_pago + ShareQ + desc_shareQ +
    [d_ccaa con ≥30 obs],
  ineffDecrease=TRUE, maxit=5000)
```

Resultado: N=5.332, logLik=−3.030, TE=0.719, gamma=0.916.
D_desc=−7.51 (t=−11.4) — **confirma P1**: descentralización reduce ineficiencia en cantidad.

Guarda: `data_clean/sfa_A_Total.rds`

---

### 10b_estimar_sfa_A_intensidad.R
**Diseño A — Frontera de intensidad (ln_i_diag), versión sin winsorizar.**

Misma muestra y especificación que 10, con `ln_i_diag` como variable dependiente.
Diagnóstico: gamma→0.9999, sigmaSq≈1.060 (patología de borde). Resultados no fiables.
Ver script 10c para la versión corregida.

---

### 10c_estimar_sfa_A_winsorized.R
**Diseño A — Frontera de intensidad con winsorización (versión definitiva).**

Corrige la patología gamma→1 del script 10b mediante winsorización:
```r
p1  <- quantile(df_A_I$ln_i_diag, 0.01)
p99 <- quantile(df_A_I$ln_i_diag, 0.99)
ln_i_diag_w <- pmin(pmax(ln_i_diag, p1), p99)
```

Justificación: skewness OLS de residuos = −3.48 (signo correcto para SFA de producción). La winsorización elimina hospitales con perfiles diagnósticos extremos que causan la patología.

Resultado: N=5.332, logLik=−4.286, TE=0.666, gamma=0.986.
D_desc=+25.88 (t=+5.6) — **confirma P2**: descentralización aumenta ineficiencia en intensidad.

Guarda: `data_clean/sfa_A_I_winsorized.rds`

---

### 11_estimar_sfa_BCD.R
**Diseños B, C y D.**

**Diseño B — Fronteras quirúrgica y médica:**
Muestra: filtros base + altQ_bruto≥200 + altM_bruto≥200 (N=3.866).
Especificación con 3 grupos de pago (d_Priv_Conc, d_Priv_Merc).
- B_Q (quirúrgica): ShareQ=−2.37 (t=−24.0) — **confirma P4 (complementariedad)**
- B_M (médica): ShareQ=+0.91 (t=+7.7) — **confirma P4 (sustitución)**
- P*: LR=4.211, p<0.001

**Diseño C — Panel hospital × servicio:**
Reshape a formato largo (2 filas por hospital-año: C_s=1 quirúrgico, C_s=0 médico).
N=7.732. ShareQ_Cs=−4.09 (t=−30.3) — confirma P4 con variación intra-hospital.

**Diseño D — Función de distancia output (ODF):**
Dep: −ln_altQ_pond. Regresores adicionales: ln_ratio_MQ.
N=3.866, TE=0.585.

Guarda: `data_clean/sfa_B_Q_v2.rds`, `sfa_B_M_v2.rds`, `sfa_C_panel.rds`, `sfa_D_odf.rds`

---

### 11b_estimar_sfa_A3g.R
**Diseño A con especificación 3 grupos de pago (A-3g).**

Misma muestra que Diseño A pero con d_Priv_Conc / d_Priv_Merc en lugar de D_desc binario.

Resultados clave:
- Cantidad: Priv_Merc=−1.75 (t=−3.0), Priv_Conc=+3.93 (t=+3.5)
- Intensidad: Priv_Merc=+22.98 (t=+4.7), Priv_Conc=+18.61 (t=+4.8)

Interpretación: los hospitales de mercado (pct_sns<50%) son más eficientes en volumen que los concertados (pct_sns≥50%), que están sujetos a techos de actividad del concierto SNS. En intensidad, ambos grupos privados son igualmente ineficientes.

Guarda: `data_clean/sfa_A3g_Total.rds`, `data_clean/sfa_A3g_I.rds`

---

### 12_robustez.R
**Análisis de robustez — 5 especificaciones alternativas del Diseño A.**

| Spec | Modificación | D_desc quant | D_desc intens | Sign OK |
|------|-------------|-------------|--------------|---------|
| R0 (base) | — | −7.54 (t=−10.8) | +25.88 (t=+5.6) | ✓ |
| R1 | i_simple en lugar de i_diag | −7.54 (t=−10.8) | +7.22 (t=+4.7) | ✓ |
| R3 | Sin interacciones desc_pago/desc_shareQ | +0.68 (t=+1.7) | +11.06 (t=+4.7) | CHECK |
| R4 | Solo 2010–2019 (sin 2023) | −7.54 (t=−11.1) | +25.93 (t=+4.2) | ✓ |
| R5 | Sin hospitales ONG | −8.26 (t=−9.5) | +15.76 (t=+4.2) | ✓ |

4 de 5 especificaciones confirman el patrón asimétrico. R3 pierde significación en cantidad por omisión del mecanismo de pago (coherente con el modelo teórico).

Guarda: `data_clean/tabla_robustez.csv`

---

### 12b_robustez_fix.R
**Correcciones y extensiones del análisis de robustez.**

Incluye tests de especificación para los 4 modelos principales:
- **Kodde-Palm** (H0: σ²_u=0): valor crítico 2.706 al 5%
- **LR forma funcional** (Cobb-Douglas vs. translog)
- **Correlaciones TE** entre fronteras: Pearson y Spearman TE_Q vs TE_M (esperado: ~−0.14)

---

## Resultados principales

### Predicciones del modelo teórico (Izquierdo, 2019)

| Predicción | Descripción | Resultado | Evidencia |
|-----------|-------------|-----------|-----------|
| **P1** | Descentralización ↓ inef. cantidad | **CONFIRMADA** | D_desc=−7.51 (t=−11.4), 4/5 specs |
| **P2** | Descentralización ↑ inef. intensidad | **CONFIRMADA** | D_desc=+25.88 (t=+5.6), 4/5 specs |
| **P3** | Pago modera asimétricamente | **CONFIRMADA** | z_diff desc_pago=+7.68, p<0.001 |
| **P4** | ψ₁₂ modera asimétricamente | **CONFIRMADA** | z_diff ShareQ B_Q vs B_M=−21.27, p<0.001 |
| **P\*** | Vectores α≠δ (contraste central) | **CONFIRMADA** | LR_A=11.125, LR_B=4.211, p<0.001 |

### Eficiencias técnicas medias

| Diseño | Frontera | TE media |
|--------|---------|---------|
| A | Cantidad | 0.719 |
| A | Intensidad | 0.666 |
| A-3g | Cantidad | 0.718 |
| A-3g | Intensidad | 0.665 |
| B | Quirúrgica | 0.278* |
| B | Médica | 0.598* |
| C | Panel servicio | 0.291 |
| D | ODF | 0.585 |

*Diseños B con gamma corregido.

---

## Notas técnicas importantes

1. **NCODI siempre como entero** antes de cualquier join: `df$NCODI <- as.integer(df$NCODI)`

2. **Winsorización de intensidad**: `ln_i_diag` se winsoriza a p1/p99 en la muestra de estimación. Mencionarlo en nota al pie de la sección 5.3 de la tesis.

3. **uhet vs muhet**: el estimador `frontier::sfa()` con fórmula de dos partes implementa BC95 con z-variables en la media de u_it (muhet). Los modelos de marzo 2026 en `data_intermediate/` usaban `sfaR::sfacross()` con `uhet` (varianza), que es la extensión de Wang-Schmidt (2002) — especificación diferente con coeficientes no directamente comparables.

4. **Crosswalk NCODI-CNH**: el archivo `ncodi_hospital_map.csv` cubre ~753 hospitales de los ~880 del panel. La cobertura CNH es cross-sectional (misma categoría para todos los años). Para el análisis principal se usa solo `cod_depend_agrupada` del SIAE (disponible todos los años tras el fix del script 06).

5. **Años COVID excluidos**: 2020, 2021, 2022. Incluibles como análisis de robustez con dummies `covid=(anyo %in% 2020:2022)`.
