suppressPackageStartupMessages(library(sfaR))
dir_data <- "G:/Mi unidad/SIAE 2010-2020/data_intermediate"

cat("═══ COMPARACIÓN DE COEFICIENTES Zu_ DE TODOS LOS MODELOS ═══\n\n")

archivos <- c(
  "sfa_modeloTotal_disenioA.rds",  # archivado (legacy)
  "sfa_modeloI_disenioA.rds",
  "sfa_modeloTotal_definitivo.rds", # script principal (grupo_pago)
  "sfa_modeloI_definitivo.rds",
  "sfa_mTot_Ddesc_definitivo.rds",  # script principal (D_desc + CCAA)
  "sfa_mI_Ddesc_definitivo.rds",
  "sfa_A_Total_Ddesc_separado.rds", # script A_A1 (D_desc + CCAA)
  "sfa_A_I_Ddesc_separado.rds"
)

for (f in archivos) {
  ruta <- file.path(dir_data, f)
  if (!file.exists(ruta)) {
    cat(sprintf("%-40s NO EXISTE\n", f))
    next
  }
  m <- readRDS(ruta)
  cf <- coef(m)
  se <- tryCatch(sqrt(diag(vcov(m))), error = function(e) rep(NA, length(cf)))
  idx <- grep("^Zu_", names(cf))
  cat(sprintf("\n── %s (udist=%s, ll=%.2f) ──\n", f, m$udist, as.numeric(m$mlLoglik)))
  for (i in idx) {
    nm <- sub("^Zu_", "", names(cf)[i])
    if (grepl("^d_ccaa", nm)) next
    t_val <- cf[i] / se[i]
    cat(sprintf("  %-15s  coef=%8.4f  se=%7.4f  t=%7.3f\n", nm, cf[i], se[i], t_val))
  }
}
