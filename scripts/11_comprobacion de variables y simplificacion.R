config_path <- if (file.exists("scripts/00_config.R")) "scripts/00_config.R" else "00_config.R"
source(config_path)
library(dplyr)
# Cargar el archivo .RData
load(DF_COMPLETO_ACTUALIZADO_RDATA_PATH)
#writeLines(names(df_completo_actualizado), "nombres_variables_302.txt")



# creamos variables de equipo tecnologógico y de diagnostico (en minuscula) como la suma de hospital y CEP (centro de especialidades)
df_completo_actualizado <- df_completo_actualizado %>%
  mutate(
    salas_rx_total = salas_rx_Hospital + salas_rx_CEP,
    acelerador_total = acelerador_hospital + acelerador_CEP,
    angiografo_total = angiografo_hospital + angiografo_CEP,
    bombas_total = bombas_hospital + bombas_CEP,
    densiometros_total = densiometros_hospital + densiometros_CEP,
    hemodialisis_total = hemodialisis_hospital + hemodialisis_CEP,
    TAC_total = TAC_hospital + TAC_CEP,
    PET_total = PET_hospital + PET_CEP,
    RNM_total = RNM_hospital + RNM_CEP,
    gammacamara_total = gammacamara_hospital + gammacamara_CEP,
    litotriptor_total = litotriptor_hospital + litotriptor_CEP,
      mamografos_total = mamografos_hospital + mamografos_CEP,
    SPECT_total = spect_hospital,
    biopsias_total=biopsias_CEP+biopsias_hosp,
    angio_total = angio_hosp + angio_CEP,
    densiometrias_total = densiometrias_hosp + densiometrias_CEP,
    gamma_total = gamma_hosp + gamma_CEP,
    mamo_total = mamo_hosp + mamo_CEP,
    pet_total = pet_hosp + pet_CEP,
    resonancia_total = resonancia_hosp + resonancia_CEP,
    rx_total = rx_hosp + rx_CEP,
    spect_total = spect_hosp,
    tac_total = tac_hosp + tac_CEP
  )
df_depurado <- df_completo_actualizado %>%
  select(-salas_rx_Hospital, -salas_rx_CEP,
         -acelerador_hospital, -acelerador_CEP,
         -angiografo_hospital, -angiografo_CEP,
         -bombas_hospital, -bombas_CEP,
         -densiometros_hospital, -densiometros_CEP,
         -hemodialisis_hospital, -hemodialisis_CEP,
         -TAC_hospital, -TAC_CEP,
         -PET_hospital, -PET_CEP,
         -RNM_hospital, -RNM_CEP,
         -gammacamara_hospital, -gammacamara_CEP,
         -litotriptor_hospital, -litotriptor_CEP,
         -mamografos_hospital, -mamografos_CEP,
         -spect_hospital, 
         -biopsias_CEP, -biopsias_hosp,
         -angio_hosp, -angio_CEP,
         -densiometrias_hosp, -densiometrias_CEP,
         -gamma_hosp, -gamma_CEP,
         -mamo_hosp, -mamo_CEP,
         -pet_hosp, -pet_CEP,
         -resonancia_hosp, -resonancia_CEP,
         -rx_hosp, -rx_CEP,
         -spect_hosp,
         -tac_hosp, -tac_CEP)

save(df_depurado,file=DF_DEPURADO_RDATA_PATH)

# Mostrar solo las columnas NCODI, Depend_agrupada, anyo, I_AsegPriv, I_SNS, I_FdirectaSS, I_FdirectaAPriv_MATEPSS

# # Mostrar solo las columnas que comienzan con "I" mayúscula junto con NCODI, Depend_agrupada, y anyo
# View(df_completo_actualizado %>% select(NCODI, Depend_agrupada, anyo, starts_with("I", ignore.case = FALSE)))

write.table(df_completo_actualizado, DF_DEPURADO_TXT_PATH, sep = ";", row.names = FALSE)




