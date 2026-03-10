# Pipeline summary: scripts 1–14

Este README resume los scripts `1` a `14` en la carpeta `R con Chatbot`. Indica el propósito de cada script, los principales archivos de entrada/salida y los conjuntos de datos canonizados que el pipeline produce.

## Resumen por script

1. `1_cambiarnombredevariables_2.R`
   - Propósito: normalizar nombres de variables y codificaciones en los `.txt` originales.
   - Entradas: archivos `.txt` en `h:/Mi unidad/Tesis/Datos con R/SIAE 2010-2020/<año>/` (p.ej. `07_Formacion.txt` para 2015).
   - Salidas: los mismos `.txt` reescritos con encabezados y nombres de variables estandarizados.

2. `2_seleccionar_var_bucle.R`
   - Propósito: para cada año (2010–2020) unir los `.txt` de ese año por `NCODI` y seleccionar un conjunto amplio de variables.
   - Entradas: `.txt` ya renombrados del paso 1.
   - Salidas: `df_<year>.csv` por cada año en `h:/Mi unidad/Tesis/Datos con R/SIAE 2010-2020/`.

3. `3_unir todo y crear panel.R`
   - Propósito: concatenar los `df_<year>.csv` en `df_final` y guardar versión panel.
   - Entradas: `df_2010.csv` … `df_2020.csv`.
   - Salidas: `df_final.RData`, `df_final.txt` (y objetos en memoria como `df_panel` si se usa).

4. `4_crearvariablepeso.R`
   - Propósito: unir `df_final` con `pesos.txt`, etiquetar variables y explorar la variable `peso`.
   - Entradas: `df_final.txt`, `pesos.txt`, y ficheros de nombres/etiquetas (`Nombres_variables.txt`, `variables seleccionadas y descripción.txt`).
   - Salidas: `df.txt`, `df_lab.dta` (Stata con etiquetas), vistas y análisis exploratorio.

5. `5_variable mix fases.R`
   - Propósito: preprocesamiento adicional, separación en datos con/sin `peso`, imputaciones, salida de correlaciones.
   - Entradas: `df_lab.dta` / `df.txt` y `pesos.txt`.
   - Salidas: `correlacion_con_peso.csv` y archivos intermedios en memoria.

6. `6_seleccionar variables relevantes para predecir mix.R`
   - Propósito: selección de variables numéricas relevantes para predecir `peso` (umbral de correlación, p‑valores, VIF, LASSO).
   - Entradas: resultados de imputación / `data` procesado en pasos 4–5.
   - Salidas: `data_final.rds` (con variables seleccionadas para modelado).

7. `7_prediccion de variable mix.R`
   - Propósito: entrenar modelos (lm, LASSO), evaluar y predecir `peso` donde falta.
   - Entradas: `data_final.rds`, conjuntos con/sin `peso` preparados anteriormente.
   - Salidas: objetos de predicción en memoria; `data_completo` combinado (en memoria / intermedio).

8. `8_analisis exploratorio datos completos (con peso).R`
   - Propósito: gráficas comparativas entre `peso` original y `peso` predicho.
   - Entradas: `data_completo` / data combinada con `tipo_peso`.
   - Salidas: visualizaciones (histogramas, densidades, boxplots).

9. `9_construccion de variable mix.R`
   - Propósito: alternativa/flujo paralelo de modelado (LASSO → RandomForest) para generar `mix` (proxy de complejidad).
   - Entradas: `df_final.txt`, `pesos.txt` (merge), y variables numéricas preparadas.
   - Salidas: `df_completo.RData`, `df_final_con_mix.txt` (con columna `mix`) y métricas de evaluación.

10. `10_depuracion df_completo.R`
    - Propósito: limpieza y etiquetado de `df_completo` (correcciones de categorías, eliminación de columnas vacías, aplicación de diccionario de etiquetas).
    - Entradas: `df_completo.RData` (generado en 9).
    - Salidas: `df_completo_actualizado.RData`, `df_completo_actualizado.txt`.

11. `11_comprobacion de variables y simplificacion.R`
    - Propósito: crear variables agregadas (equipos, personal), eliminar duplicados y columnas originales, salvar `df_depurado`.
    - Entradas: `df_completo_actualizado.RData`.
    - Salidas: `df_depurado.RData`, `df_depurado.txt`.

12. `12_etiquetas.R`
    - Propósito: extraer nombres y etiquetas de `df_depurado`, exportar `nombres_y_etiquetas.txt` y crear `df_con_etiquetas.csv`.
    - Entradas: `df_depurado.RData`.
    - Salidas: `nombres_y_etiquetas.txt`, `df_con_etiquetas.csv`, re‑grabado de `df_depurado`.

13. `13_reduccion variables.R`
    - Propósito: crear variables resumen (tecnología, personal), filtrar hospitales con discrepancias en sumas, seleccionar variables finales y clustering (kmeans).
    - Entradas: `df_depurado.RData` / `df_completo_actualizado.RData`.
    - Salidas: `df_seleccion.csv` (y `df_seleccion.RData` si se guarda manualmente), `df_filtrado.csv`/otros intermedios.

14. `14_creacion intensidad.R` (y `14_creación outputs.R`)
    - Propósito: calcular `intensidad` (índice por altas) y otras estadísticas por región/año; `14_creación outputs.R` carga `df_seleccion` para producir outputs.
    - Entradas: `df_seleccion.RData` / `df_seleccion.csv`.
    - Salidas: variables `intensidad_numerador`, `intensidad` añadidas a `df_seleccion`, resúmenes por `ccaa`/`anyo`.

## Conjuntos de datos canonizados (principales archivos de salida del pipeline)

- `df_<year>.csv` — resultados anuales (generados por `2_seleccionar_var_bucle.R`).
- `df_final.RData` / `df_final.txt` — concatenación temporal de años (generado por `3_unir todo y crear panel.R`).
- `df.txt` / `df_lab.dta` — `df_final` unido con `pesos` y etiquetado (generado por `4_crearvariablepeso.R`).
- `data_final.rds` — variables seleccionadas para modelado (generado por `6_seleccionar variables relevantes para predecir mix.R`).
- `df_completo.RData` and `df_final_con_mix.txt` — dataset con la variable `mix` añadida (generado por `9_construccion de variable mix.R`).
- `df_completo_actualizado.RData` / `.txt` — `df_completo` después de limpieza y etiquetado (generado por `10_depuracion df_completo.R`).
- `df_depurado.RData` / `df_depurado.txt` — versiones simplificadas con agregaciones (generado por `11_comprobacion de variables y simplificacion.R`).
- `df_con_etiquetas.csv` and `nombres_y_etiquetas.txt` — listado de variables y etiquetas (generado por `12_etiquetas.R`).
- `df_seleccion.csv` / `df_seleccion.RData` — conjunto final reducido con variables de interés, cluster y `intensidad` (generado por `13_reduccion variables.R` y `14_creacion intensidad.R`).

## Notas rápidas y recomendaciones (no modifica código)

- Rutas: muchos scripts referencian rutas absolutas `h:/Mi unidad/Tesis/Datos con R/SIAE 2010-2020/`. Recomiendo centralizar la ruta base en un archivo de configuración antes de refactorizar.
- Interdependencias: ejecutar los scripts en orden 1→14 es lo esperado; varios pasos sobrescriben archivos origen (por ejemplo el paso 1 reescribe `.txt`) — hacer copias de seguridad si se va a re‑ejecutar.
- Reproducibilidad: convertir el flujo a un driver central (`run_pipeline.R`) que documente entradas/outputs y guarde snapshots intermedios facilitará pruebas y depuración.


## Propuestas de mejora

- Ver `docs/propuestas_mejora_pipeline.md` para un plan priorizado de mejoras (reproducibilidad, rutas, orquestación, validaciones, pruebas y logging).

## ¿Siguiente paso?
Si quieres, puedo:

- Generar un `run_pipeline.R` que llame los scripts en orden y controle rutas.
- Refactorizar renombrados en `1_cambiarnombredevariables_2.R` a una función reutilizable (solo propuesta, no cambio automático).

---
_Documento generado automáticamente por el asistente. Si prefieres que el README esté en inglés o que incluya diagramas/ejemplos de comandos para ejecutar el pipeline, dímelo y lo actualizo._
