# Instrucciones para Codex sobre el proyecto SIAE

## Objetivo general
Este proyecto contiene un pipeline en R para construir una base panel de hospitales españoles a partir de archivos SIAE por año, y posteriormente desarrollar análisis econométricos, incluyendo análisis de frontera estocástica (SFA).

## Principios importantes
1. No cambiar el significado económico o institucional de las variables.
2. No eliminar pasos del pipeline sin justificarlo claramente.
3. No sobrescribir archivos raw.
4. No introducir `install.packages()` dentro de scripts productivos.
5. Usar siempre rutas basadas en `00_config.R`.
6. Priorizar reproducibilidad y claridad frente a "optimización" excesiva.
7. Mantener compatibilidad con los datasets finales ya existentes cuando sea posible.

## Prioridades de mejora
1. Identificar redundancias entre scripts.
2. Mejorar el uso de rutas y archivos intermedios.
3. Proponer funciones reutilizables.
4. Separar limpieza, construcción de variables y modelización.
5. Facilitar la futura creación de un dataset final para SFA.

## Lo que NO debe hacer Codex sin aprobación expresa
- Cambiar la lógica de construcción de `peso` o `mix`.
- Borrar scripts antiguos.
- Modificar outputs finales sin advertirlo.
- Cambiar nombres de variables ya consolidados sin justificarlo.