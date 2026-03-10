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
