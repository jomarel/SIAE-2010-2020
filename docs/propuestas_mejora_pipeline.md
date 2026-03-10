<<<<<<< HEAD
=======
<<<<<<< ours
<<<<<<< ours
>>>>>>> d651e29 (Descripción breve del cambio)
Este documento propone mejoras prácticas sobre el pipeline actual (scripts 1–14), priorizadas por impacto y esfuerzo.

## Diagnóstico rápido

A partir de los scripts actuales se observan estos patrones:

- Instalación de paquetes dentro de scripts de ejecución (`install.packages(...)`), lo que introduce variabilidad y fallos por red/permisos en corridas no interactivas.
- Uso mixto de rutas absolutas (`h:/...`, `H:/...`) y una configuración parcial en `00_config.R`.
- Dependencias implícitas entre scripts mediante objetos en memoria y archivos intermedios no siempre validados.
- Falta de una orquestación única del pipeline (orden, validaciones y logging centralizados).

## Propuestas priorizadas

## 1) Estandarizar entorno y dependencias (prioridad alta)

**Qué hacer**
- Crear un archivo de dependencias reproducibles con `renv` (`renv::init()`, `renv::snapshot()`).
- Eliminar `install.packages(...)` de scripts productivos.
- Dejar un único script de setup (`scripts/00_setup.R`) para instalaciones explícitas cuando sea necesario.

**Beneficio**
- Corridas reproducibles en cualquier equipo (local/CI) sin sorpresas por versiones.

**Esfuerzo estimado**: bajo-medio.

---

## 2) Unificar rutas con configuración central (prioridad alta)

**Qué hacer**
- Convertir **todas** las rutas a `file.path(...)` basadas en `BASE_DIR`.
- Evitar `setwd(...)` salvo en utilidades puntuales.
- Soportar `BASE_DIR` por variable de entorno (`Sys.getenv("SIAE_BASE_DIR")`) con fallback local.

**Beneficio**
- Portabilidad entre máquinas y menor riesgo de escritura/lectura en rutas erróneas.

**Esfuerzo estimado**: medio.

---

## 3) Añadir un orquestador único (`run_pipeline.R`) (prioridad alta)

**Qué hacer**
- Ejecutar scripts en orden definido y parametrizable (ej. `from_step`, `to_step`).
- Validar precondiciones/postcondiciones por paso (archivo de entrada existe, output generado, columnas clave presentes).
- Registrar tiempos y estado por paso (log simple en `outputs/logs/pipeline_YYYYMMDD.log`).

**Beneficio**
- Menor riesgo operativo y depuración más rápida.

**Esfuerzo estimado**: medio.

---

## 4) Definir contratos de datos por etapa (prioridad alta)

**Qué hacer**
- Crear un `docs/data_contracts.md` con:
  - archivo de entrada/salida por paso,
  - columnas obligatorias (ej. `NCODI`, `anyo`),
  - reglas mínimas de calidad (no duplicados por clave, rangos válidos).
- Añadir validaciones automáticas con `stopifnot()` o funciones helper.

**Beneficio**
- Menos errores silenciosos y más confianza en resultados.

**Esfuerzo estimado**: medio.

---

## 5) Persistencia consistente (RDS/Parquet) (prioridad media)

**Qué hacer**
- Estandarizar outputs intermedios en `.rds` (rápido y fiel en R) o `.parquet` (interoperabilidad).
- Reservar `.csv/.txt` para exportación final o intercambio externo.

**Beneficio**
- Mejor rendimiento y menos problemas de tipos/encoding.

**Esfuerzo estimado**: medio.

---

## 6) Separar limpieza, feature engineering y modelado (prioridad media)

**Qué hacer**
- Dividir scripts largos en funciones reusables (`R/` o `scripts/lib/`):
  - `clean_*`, `build_features_*`, `train_*`, `predict_*`.
- Reutilizar funciones entre los flujos de `peso` y `mix`.

**Beneficio**
- Mantenibilidad y menor duplicidad.

**Esfuerzo estimado**: medio-alto.

---

## 7) Trazabilidad de modelos y métricas (prioridad media)

**Qué hacer**
- Guardar artefactos por corrida en carpeta versionada (`outputs/models/<run_id>/`).
- Persistir métricas clave (RMSE, MAE, R²) y parámetros en `metrics.csv`/`json`.

**Beneficio**
- Comparación objetiva entre cambios de metodología.

**Esfuerzo estimado**: medio.

---

## 8) Pruebas mínimas automáticas (prioridad media)

**Qué hacer**
- Añadir `testthat` con pruebas de:
  - renombrado de columnas,
  - joins por `NCODI`,
  - generación de variables derivadas,
  - checks de no pérdida inesperada de filas.
- Ejecutar pruebas en cada cambio relevante.

**Beneficio**
- Menor regresión funcional.

**Esfuerzo estimado**: medio.

---

## 9) Logging y manejo de errores (prioridad media)

**Qué hacer**
- Sustituir `print()` dispersos por un logger simple (niveles `INFO/WARN/ERROR`).
- Capturar errores por paso con contexto (script, archivo, año).

**Beneficio**
- Soporte y diagnóstico más rápidos.

**Esfuerzo estimado**: bajo-medio.

---

## 10) Plan de implementación sugerido (4 semanas)

- **Semana 1**: rutas + setup + retiro de `install.packages` en scripts core.
- **Semana 2**: `run_pipeline.R` con validaciones de entrada/salida.
- **Semana 3**: contratos de datos + pruebas mínimas (`testthat`).
- **Semana 4**: trazabilidad de modelos + logging estructurado.

## Quick wins (puedes aplicar ya)

1. Quitar instalaciones de paquetes dentro de `1`, `4`, `6`, `7`, `12`, `13`.
2. Reemplazar `setwd(...)` por rutas relativas con `BASE_DIR`.
3. Añadir checks de existencia para `df_2010...df_2020` antes del merge.
4. Guardar siempre una versión `*_validated.rds` tras cada etapa crítica.

---

Si quieres, en un siguiente paso te puedo proponer una versión inicial de `run_pipeline.R` con validaciones y logging lista para usar.
=======
=======
>>>>>>> theirs
# Propuestas de mejora del pipeline SIAE (2010–2020)

Este documento resume una revisión técnica del pipeline actual (scripts `1` a `14`) y propone mejoras priorizadas para aumentar **reproducibilidad, mantenibilidad y calidad de datos**.

## 1) Diagnóstico rápido

Observaciones detectadas al revisar la estructura del repositorio y los scripts principales:

- Hay mezcla de rutas absolutas históricas (`h:/...`) y una configuración central (`scripts/00_config.R`) que aún no se usa de forma consistente.
- El flujo depende del orden manual de ejecución de scripts y de objetos en memoria entre pasos.
- Algunos pasos sobrescriben entradas originales o generan salidas intermedias sin versionado.
- No hay validaciones sistemáticas (esquema, claves únicas, NA inesperados, rangos) antes de continuar al siguiente paso.
- Existen scripts con lógica exploratoria y de producción mezclada, lo que dificulta repetir ejecuciones de forma determinista.

## 2) Propuestas priorizadas (alto impacto / bajo esfuerzo)

### P1. Unificar rutas y parámetros en un único archivo de configuración

**Qué hacer**
- Establecer `scripts/00_config.R` como única fuente de verdad para rutas (`BASE_DIR`, `RAW_DIR`, `INT_DIR`, `OUT_DIR`).
- Reemplazar rutas hardcodeadas en todos los scripts por `file.path(...)`.

**Beneficio**
- Portabilidad inmediata entre equipos/entornos.
- Menor riesgo de errores por cambios de carpeta.

**Métrica de éxito**
- `rg "[A-Za-z]:/|h:/|g:/" scripts` devuelve solo referencias justificadas/documentadas.

---

### P2. Añadir un `run_pipeline.R` con ejecución orquestada y logs

**Qué hacer**
- Crear un script lanzador que ejecute pasos en orden, registre inicio/fin de cada etapa y detenga en error con mensaje claro.
- Generar un log en `outputs/logs/pipeline_<fecha>.log`.

**Beneficio**
- Ejecución reproducible con un solo comando.
- Diagnóstico rápido ante fallos.

**Métrica de éxito**
- Pipeline completo ejecutable con un único comando en entorno limpio.

---

### P3. Introducir validaciones de calidad entre etapas

**Qué hacer**
- Tras cada output clave (`df_<year>`, `df_final`, `df_completo`, `df_depurado`, `df_seleccion`), validar:
  - presencia de columnas obligatorias,
  - unicidad de `NCODI + anyo` cuando aplique,
  - porcentaje de NA en variables críticas,
  - rangos esperados (ej. `peso`, `mix`, `intensidad`).
- Guardar reporte simple CSV/MD por ejecución.

**Beneficio**
- Prevención temprana de errores silenciosos.

**Métrica de éxito**
- Cada corrida deja un reporte de checks con estado `OK/FAIL` por dataset.

---

### P4. Separar scripts de producción vs análisis exploratorio

**Qué hacer**
- Mover gráficos exploratorios y pruebas ad hoc a una carpeta `analysis/` o `notebooks/`.
- Dejar en `scripts/` solo pasos deterministas del pipeline.

**Beneficio**
- Menos side effects y más claridad operativa.

**Métrica de éxito**
- Cualquier script en `scripts/` produce salidas definidas y no depende de objetos previos en sesión.

## 3) Propuestas de mediano plazo

### M1. Estandarizar naming y convenciones
- Unificar estilo de nombres de archivos (sin espacios/acentos) y objetos (`snake_case`).
- Definir guía corta en `docs/`.

### M2. Modularizar funciones repetidas
- Extraer utilidades para:
  - normalización de texto/codificación,
  - joins por `NCODI`,
  - exportación y etiquetado,
  - validaciones.
- Colocarlas en `R/utils_*.R`.

### M3. Versionado explícito de datasets intermedios
- Guardar outputs por etapa con sello de fecha/hora o versión (`vYYYYMMDD`).
- Evitar sobreescritura de artefactos críticos sin backup.

### M4. Reducir dependencia de formatos heterogéneos
- Priorizar formatos estables para procesamiento (`.rds`, `.csv` UTF-8).
- Reservar `.dta` solo para interoperabilidad externa.

## 4) Propuestas de largo plazo (si se busca escalabilidad)

### L1. Migrar a un pipeline declarativo (`targets` o `drake`)
- Declarar dependencias entre objetos y ejecutar solo lo que cambió.
- Trazabilidad completa de inputs/outputs por etapa.

### L2. Integración continua (CI) con smoke tests
- Ejecutar checks mínimos en cada cambio:
  - carga de configuración,
  - lectura de un subconjunto de datos,
  - validaciones básicas.

### L3. Diccionario de datos vivo
- Mantener un único diccionario (variable, definición, tipo, rango, origen, script productor).
- Generar documentación automáticamente desde metadatos.

## 5) Plan de implementación sugerido (4 semanas)

### Semana 1
- P1 (rutas) + convención mínima de nombres.
- Entregable: scripts sin rutas absolutas hardcodeadas.

### Semana 2
- P2 (`run_pipeline.R`) + logging.
- Entregable: corrida end-to-end con log.

### Semana 3
- P3 (validaciones) en outputs críticos.
- Entregable: reporte de calidad por corrida.

### Semana 4
- P4 (separación exploratorio/producción) + primeras utilidades reutilizables (M2).
- Entregable: estructura de proyecto más limpia y mantenible.

## 6) Riesgos actuales si no se actúa

- Reproducibilidad frágil (resultados dependen del entorno local y del orden manual).
- Mayor coste de mantenimiento por duplicación de lógica.
- Posibles inconsistencias silenciosas en merges y transformaciones.
- Dificultad para auditar resultados finales frente a cambios de scripts.

## 7) Siguiente paso recomendado

Como primer sprint, aplicar **P1 + P2**. Es la combinación con mejor relación impacto/esfuerzo y habilita el resto de mejoras con menor riesgo.
<<<<<<< ours
>>>>>>> theirs
=======
>>>>>>> theirs
