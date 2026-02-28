# Set your working directory to the folder containing df_seleccion.RData
setwd("h:/Mi unidad/Tesis/Datos con R/SIAE 2010-2020/")

# Load the data
load("df_seleccion.RData")
dim(df_seleccion)
library(dplyr)
# Summary by region and year
summary_by_region_year <- df_seleccion %>%
  group_by(ccaa, anyo) %>%
  summarise(
    mean_beds = mean(camas_funcionamiento, na.rm = TRUE),
    total_discharges = sum(altFinal_total, na.rm = TRUE),
    mean_case_mix = mean(mix, na.rm = TRUE)
  )




df_seleccion <- df_seleccion %>%
  distinct()  # Removes exact duplicates across all columns

length(unique(df_seleccion$NCODI))
length(unique(df_seleccion$anyo))
# Check if (NCODI, anyo) is a unique key
any(duplicated(df_seleccion[, c("NCODI", "anyo")]))

library(dplyr)

df_seleccion <- df_seleccion %>%
  mutate(
    intensidad_numerador = 
      (biopsias_total       * 60)   +
      (angio_total          * 750)  +
      (densiometrias_total  * 100)  +
      (gamma_total          * 250)  +
      (mamo_total           * 50)   +
      (pet_total            * 1200) +
      (resonancia_total     * 450)  +
      (rx_total             * 30)   +
      (spect_total          * 400)  +
      (tac_total            * 300),
    
    intensidad = if_else(
      altFinal_total > 0,
      intensidad_numerador / altFinal_total,
      NA_real_
    )
  )
names(df_seleccion)
summary(df_seleccion$intensidad_numerador)
summary(df_seleccion$intensidad)




