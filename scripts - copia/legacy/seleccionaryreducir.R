{library(reshape2)
  library(dplyr)
  library(tidyverse)}


df_final <- read.csv("h:/Mi unidad/Tesis/Datos con R/SIAE 2010-2020/df_final.csv", sep = ";")

df_final <- df_final %>% filter(!is.na(NCODI))
file <- "h:/Mi unidad/Tesis/Datos con R/SIAE 2010-2020/df_final.csv"
write_delim(df_final, file, delim = ";", quote = "all")

df_grouped <- df_final %>% group_by(ccaa_codigo)

 df_grouped = df_final %>% select(contains("RNM"))

df_sampled %>% filter(TAC_concertado > 20) %>% head(10)


df_tot <- df_final %>%
      mutate(tac=TAC_hospital +TAC_CEP,
             altatecno=tac+RNM_hospital+RNM_CEP+gammacamara_hospital+gammacamara_CEP+angiografo_hospital+ litotriptor_hospital+ bombas_hospital+acelerador_hospital+hemodialisis_hospital+salasHemo_funcionamiento

  
            ) %>%
  select (NCODI, ccaa_codigo, Finalidad_agrupada, tac, altatecno)

install.packages("DataEditR")
library(DataEditR)
data_edit(df_final)
