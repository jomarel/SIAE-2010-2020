## test_sin_ccaa.R — re-estimar Diseño A SIN y CON dummies CCAA
## para identificar la causa de la pérdida de significatividad
suppressPackageStartupMessages(library(sfaR))
suppressPackageStartupMessages(library(dplyr))
set.seed(42)

dir_data <- "G:/Mi unidad/SIAE 2010-2020/data_intermediate"
load(file.path(dir_data, "df_sfa.RData"))

# ── Muestra ACTUAL (script cd2f057) ────────────────────────────
df_actual <- df_sfa %>%
  filter(
    es_agudo == 1,
    !anyo %in% 2020:2022,
    altTotal_bruto > 100,
    !is.na(D_desc), !is.na(pct_sns), !is.na(ShareQ),
    !is.na(ln_L_total_c), !is.na(ln_K_camas_c),
    !is.na(ccaa_cod),
    altTotal_bruto >= 200,
    is.finite(ln_altTotal_pond),
    is.finite(ln_i_diag),
    ln_i_diag > quantile(ln_i_diag, 0.01, na.rm = TRUE)
  )
cat(sprintf("Muestra actual: %d obs, %d hospitales\n", nrow(df_actual), n_distinct(df_actual$NCODI)))

# ── Muestra ORIGINAL (commit 27c859a) ──────────────────────────
df_orig <- df_sfa %>%
  filter(
    !is.na(ln_altQ_pond) & is.finite(ln_altQ_pond),
    !is.na(ln_altM_pond) & is.finite(ln_altM_pond),
    !is.na(ln_altTotal_pond) & is.finite(ln_altTotal_pond),
    !is.na(ln_L_total_c),
    !is.na(ln_K_camas_c),
    !is.na(D_desc),
    !is.na(pct_sns),
    altTotal_bruto > 100,
    !is.na(ln_i_diag) & is.finite(ln_i_diag)
  )
# Filtro outliers como en original (si existe columna)
if ("i_diag_outlier" %in% names(df_orig)) {
  df_orig <- df_orig %>% filter(!i_diag_outlier)
} else {
  df_orig <- df_orig %>%
    filter(ln_i_diag > quantile(ln_i_diag, 0.01, na.rm = TRUE),
           ln_i_diag < quantile(ln_i_diag, 0.99, na.rm = TRUE))
}
cat(sprintf("Muestra original: %d obs, %d hospitales\n", nrow(df_orig), n_distinct(df_orig$NCODI)))

# ── Frontera ──────────────────────────────────────────────────
frontier <- ln_altTotal_pond ~ ln_L_total_c + ln_K_camas_c + ln_K_tech_c +
  ln_L_total_c2 + ln_K_camas_c2 + trend + trend2 +
  d_cluster2 + d_cluster3 + d_cluster4 + d_cluster5

frontier_I <- ln_i_diag ~ ln_L_total_c + ln_K_camas_c + ln_K_tech_c +
  ln_L_total_c2 + ln_K_camas_c2 + trend + trend2 +
  d_cluster2 + d_cluster3 + d_cluster4 + d_cluster5

# Ineficiencia SIN CCAA (como en script original 27c859a)
uhet_sin_ccaa <- ~ D_desc + pct_sns + desc_pago + ShareQ + desc_shareQ

# Ineficiencia CON CCAA (como en script actual cd2f057)
ccaa_vars <- grep("^d_ccaa_[0-9]+$", names(df_actual), value = TRUE)
uhet_con_ccaa <- as.formula(paste(
  "~ D_desc + pct_sns + desc_pago + ShareQ + desc_shareQ +",
  paste(ccaa_vars, collapse = " + ")
))

# ── Función de estimación ─────────────────────────────────────
estimar <- function(frontera, uhet, data, label) {
  cat(sprintf("\nEstimando: %s\n", label))
  m <- tryCatch(
    suppressWarnings(sfaR::sfacross(
      formula = frontera, uhet = uhet, data = data,
      S = 1L, udist = "tnormal", method = "bfgs", hessianType = 1L
    )),
    error = function(e) { cat("  FALLÓ:", e$message, "\n"); NULL }
  )
  if (!is.null(m)) {
    cf <- coef(m)
    se <- tryCatch(sqrt(diag(vcov(m))), error = function(e) rep(NA, length(cf)))
    idx <- grep("^Zu_", names(cf))
    cat(sprintf("  ll = %.2f | udist = %s\n", as.numeric(m$mlLoglik), m$udist))
    for (i in idx) {
      nm <- sub("^Zu_", "", names(cf)[i])
      if (grepl("^d_ccaa", nm)) next
      cat(sprintf("  %-15s  coef=%8.4f  se=%7.4f  t=%7.3f\n", nm, cf[i], se[i], cf[i]/se[i]))
    }
  }
  m
}

cat("\n════════════════════════════════════════════════════════════════")
cat("\n TEST 1: Muestra ACTUAL, SIN dummies CCAA (reproducir original)")
cat("\n════════════════════════════════════════════════════════════════\n")
m1_Q <- estimar(frontier, uhet_sin_ccaa, df_actual, "Total SIN CCAA, muestra actual")
m1_I <- estimar(frontier_I, uhet_sin_ccaa, df_actual, "Intensidad SIN CCAA, muestra actual")

cat("\n════════════════════════════════════════════════════════════════")
cat("\n TEST 2: Muestra ACTUAL, CON dummies CCAA (script Codex)")
cat("\n════════════════════════════════════════════════════════════════\n")
m2_Q <- estimar(frontier, uhet_con_ccaa, df_actual, "Total CON CCAA, muestra actual")
m2_I <- estimar(frontier_I, uhet_con_ccaa, df_actual, "Intensidad CON CCAA, muestra actual")

cat("\n════════════════════════════════════════════════════════════════")
cat("\n TEST 3: Muestra ORIGINAL (27c859a), SIN dummies CCAA")
cat("\n════════════════════════════════════════════════════════════════\n")
m3_Q <- estimar(frontier, uhet_sin_ccaa, df_orig, "Total SIN CCAA, muestra original")
m3_I <- estimar(frontier_I, uhet_sin_ccaa, df_orig, "Intensidad SIN CCAA, muestra original")

cat("\n════════════════════════════════════════════════════════════════")
cat("\n DIAGNÓSTICO: Correlación D_desc con CCAA")
cat("\n════════════════════════════════════════════════════════════════\n")
tab <- df_actual %>% group_by(ccaa_cod) %>%
  summarise(n = n(), pct_desc = mean(D_desc, na.rm = TRUE), .groups = "drop") %>%
  arrange(desc(pct_desc))
print(tab, n = 20)
cat(sprintf("\nCorrelación D_desc ~ CCAA (eta²): %.4f\n",
            summary(aov(D_desc ~ factor(ccaa_cod), data = df_actual))[[1]]["F value"][[1]] *
              summary(aov(D_desc ~ factor(ccaa_cod), data = df_actual))[[1]]["Df"][[1]] /
              nrow(df_actual)))

cat("\nFin test.\n")
