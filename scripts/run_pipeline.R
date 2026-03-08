# Script maestro para ejecutar el pipeline completo

cat("=== Inicio del pipeline SIAE ===\n")

scripts <- c(
  "scripts/1_cambiarnombredevariables_2.R",
  "scripts/2_seleccionar_var_bucle.R",
  "scripts/3_unir todo y crear panel.R",
  "scripts/9_construccion de variable mix.R",
  "scripts/10_depuracion df_completo.R",
  "scripts/11_comprobacion de variables y simplificacion.R",
  "scripts/12_etiquetas.R",
  "scripts/13_reduccion variables.R",
  "scripts/14_creacion intensidad.R"
)

for (s in scripts) {
  cat("Ejecutando:", s, "\n")
  source(s)
}

cat("=== Pipeline finalizado ===\n")
