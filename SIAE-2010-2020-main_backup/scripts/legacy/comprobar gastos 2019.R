library(dplyr)

# ── Añadir número de fila a ambos archivos ────────────────────
df_anon_idx   <- df_anon   %>% mutate(.row = row_number())
df_noAnon_idx <- df_noAnon_filtrado %>% mutate(.row = row_number())

# ── Crosswalk por posición de fila (gold standard para los 6) ─
crosswalk_posicion <- df_anon_idx %>%
  select(NCODI, .row) %>%
  inner_join(
    df_noAnon_idx %>% select(cod_centro, .row),
    by = ".row"
  ) %>%
  select(NCODI, cod_centro)

cat("Crosswalk por posición de fila:\n")
cat(sprintf("  Filas: %d | NCODI únicos: %d | cod_centro únicos: %d\n",
            nrow(crosswalk_posicion),
            n_distinct(crosswalk_posicion$NCODI),
            n_distinct(crosswalk_posicion$cod_centro)))
cat(sprintf("  Duplicados NCODI:      %d\n", sum(duplicated(crosswalk_posicion$NCODI))))
cat(sprintf("  Duplicados cod_centro: %d\n", sum(duplicated(crosswalk_posicion$cod_centro))))

# ── Verificar que para los 747 casos limpios coincide con el join financiero ──
crosswalk_financiero_limpio <- crosswalk_all %>%
  filter(!NCODI %in% dup_ncodi)  # los 747 sin ambigüedad

coincidencias <- crosswalk_posicion %>%
  filter(NCODI %in% crosswalk_financiero_limpio$NCODI) %>%
  inner_join(crosswalk_financiero_limpio,
             by = "NCODI", suffix = c("_pos", "_fin")) %>%
  mutate(coincide = cod_centro_pos == cod_centro_fin)

cat(sprintf("\nVerificación cruzada (747 casos limpios):\n"))
cat(sprintf("  Coinciden posición y join financiero: %d / %d\n",
            sum(coincidencias$coincide), nrow(coincidencias)))

# ── Ver los 6 casos resueltos por posición ────────────────────
cat("\nLos 6 casos ambiguos resueltos por posición de fila:\n")
print(crosswalk_posicion %>%
        filter(NCODI %in% c("241", "6", "761", "116", "927", "962")))

# ── Verificar con CCAA de otro módulo ─────────────────────────
# El código CNH empieza por provincia — comparar con ccaa del anonimizado
cat("\nPrimeros 4 dígitos de cod_centro (provincia CNH) para los 6 casos:\n")
crosswalk_posicion %>%
  filter(NCODI %in% c("241", "6", "761", "116", "927", "962")) %>%
  mutate(provincia_cnh = substr(cod_centro, 1, 4)) %>%
  print()

# ── Si la verificación es correcta, guardar ───────────────────
# (solo ejecutar si sum(coincidencias$coincide) == 747)
if (sum(coincidencias$coincide) == nrow(coincidencias)) {
  cat("\n✓ Posición de fila consistente con join financiero\n")
  cat("  → Usando crosswalk por posición como definitivo\n")
  
  crosswalk_path <- file.path(BASE_DIR, "data_intermediate",
                               "crosswalk_NCODI_cod_centro.csv")
  write.csv(crosswalk_posicion, crosswalk_path, row.names = FALSE)
  cat(sprintf("✓ Guardado: %s\n", crosswalk_path))
  
} else {
  cat("\n✗ Hay discrepancias — revisar manualmente antes de guardar\n")
  cat("Filas discrepantes:\n")
  print(coincidencias %>% filter(!coincide))
}