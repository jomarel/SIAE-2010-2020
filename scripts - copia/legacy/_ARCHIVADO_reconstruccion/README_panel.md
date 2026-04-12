# Scripts panel SFA

## Qué hay aquí

- `11_estimar_sfa.R`  
  Versión principal con `sfa::psfm()`. Es la opción más flexible si quieres trabajar con panel SFA "real" y dejar abierta la extensión a modelos TRE / GTRE / TFE / FD.

- `11b_estimar_sfa_frontier.R`  
  Versión benchmark con `frontier::sfa()`. Es la más cercana a una especificación clásica Battese-Coelli (1995) con fórmula de dos partes `y ~ x | z`.

## Recomendación práctica

- Si quieres **máxima cercanía al texto actual de la tesis**, empieza por `11b_estimar_sfa_frontier.R`.
- Si quieres **migrar ya a un paquete con soporte panel más amplio**, usa `11_estimar_sfa.R`.

## Uso

1. Sitúa estos scripts en la raíz del proyecto donde ya exista `00_config.R`, o ejecútalos desde un directorio que tenga acceso a `data_intermediate/df_sfa.*`.
2. Ejecuta antes:
   - `06_construir_Ddesc_pago_inputs.R`
   - `07_construir_intensidad.R`
   - `09_crear_df_sfa.R`
3. Luego:
   - `source("11_estimar_sfa.R")`
   - opcionalmente `source("11b_estimar_sfa_frontier.R")`

## Cambio de modelo en el script `sfa`

Por defecto:
- `MODEL_NAME = "TRE"`

Antes de correr el script puedes cambiar:
- `"TRE"` o `"TRE_Z"` para algo relativamente cercano al planteamiento actual
- `"GTRE"` o `"GTRE_Z"` si quieres separar heterogeneidad persistente e ineficiencia transitoria
- `"TFE"` o `"FD"` para variantes alternativas del paquete

También puedes fijarlo con variable de entorno en R:
`Sys.setenv(SFA_MAIN_MODEL = "GTRE")`

## Nota metodológica

La tesis justifica empíricamente el uso de una ineficiencia transitoria con media heterogénea, y deja la extensión tipo Greene como línea natural de trabajo futuro. Por eso tiene sentido conservar **dos scripts**:
- uno clásico (`frontier`)
- otro de extensión panel (`sfa`)
