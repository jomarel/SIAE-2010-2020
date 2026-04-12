## Fix es_agudo (cod_finalidad_agrupada) and ccaa_codigo from XML columns
setwd("G:/Mi unidad/SIAE 2010-2020")
library(dplyr)
CLEAN_DIR <- "data_clean"
load(file.path(CLEAN_DIR, "df_final.RData"))
to_num <- function(x) suppressWarnings(as.numeric(x))

cat("=== Fixing cod_finalidad_agrupada ===\n")
# The XML column Cód_Grupo_Finalidad wasn't mapped to cod_finalidad_agrupada
if ("Cód_Grupo_Finalidad" %in% names(df_final)) {
  xml_fin <- to_num(df_final[["Cód_Grupo_Finalidad"]])
  siae_fin <- to_num(df_final$cod_finalidad_agrupada)
  mask <- is.na(siae_fin) & !is.na(xml_fin)
  cat("Filling cod_finalidad_agrupada from Cód_Grupo_Finalidad:", sum(mask), "values\n")
  df_final$cod_finalidad_agrupada[mask] <- xml_fin[mask]
}

# Also check Finalidad_agrupada text column
if ("FINALIDAD_agrupada_para_Anonimización" %in% names(df_final)) {
  cat("FINALIDAD_agrupada_para_Anonimización values:\n")
  print(table(df_final[["FINALIDAD_agrupada_para_Anonimización"]], useNA = "always"))
}

# Also fill from Finalidad_agrupada if it exists
if ("Finalidad_agrupada" %in% names(df_final)) {
  cat("\nFinalidad_agrupada values:\n")
  print(table(df_final$Finalidad_agrupada, useNA = "always"))
}

# Rebuild es_agudo using cod_finalidad_agrupada (1=Agudos) + finalidad_cnh as fallback
df_final$es_agudo <- case_when(
  to_num(df_final$cod_finalidad_agrupada) == 1 ~ 1L,
  !is.na(to_num(df_final$cod_finalidad_agrupada)) ~ 0L,
  toupper(trimws(as.character(df_final$finalidad_cnh))) == "AGUDOS" ~ 1L,
  !is.na(df_final$finalidad_cnh) & nchar(trimws(df_final$finalidad_cnh)) > 0 ~ 0L,
  TRUE ~ NA_integer_
)

cat("\nes_agudo after fix:\n")
print(table(df_final$es_agudo, useNA = "always"))
cat("es_agudo by year:\n")
print(table(df_final$anyo, df_final$es_agudo, useNA = "always"))

cat("\n=== Fixing ccaa_codigo ===\n")
if ("Cód_CCAA_Todas" %in% names(df_final)) {
  xml_ccaa <- to_num(df_final[["Cód_CCAA_Todas"]])
  siae_ccaa <- to_num(df_final$ccaa_codigo)
  mask_ccaa <- is.na(siae_ccaa) & !is.na(xml_ccaa)
  cat("Filling ccaa_codigo from Cód_CCAA_Todas:", sum(mask_ccaa), "values\n")
  df_final$ccaa_codigo[mask_ccaa] <- xml_ccaa[mask_ccaa]
}

cat("ccaa_codigo NA by year:\n")
print(table(df_final$anyo, is.na(df_final$ccaa_codigo)))
cat("Unique ccaa_codigo:", sort(unique(to_num(df_final$ccaa_codigo))), "\n")

save(df_final, file = file.path(CLEAN_DIR, "df_final.RData"))
cat("\nFixes saved. es_agudo NA% =",
    round(100 * mean(is.na(df_final$es_agudo)), 1), "\n")
cat("ccaa_codigo NA% =",
    round(100 * mean(is.na(df_final$ccaa_codigo)), 1), "\n")
