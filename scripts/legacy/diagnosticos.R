source("G:/Mi unidad/SIAE 2010-2020/scripts/00_config.R")
load(DF_FINAL_RDATA_PATH)

# ver todos los valores únicos de la variable texto
table(df_final$Depend_agrupada, useNA="always")