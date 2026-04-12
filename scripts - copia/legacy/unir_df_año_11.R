# Primero, necesitamos cargar el paquete dplyr
library(dplyr)
library(tidyr)

# Definimos la ruta de la carpeta donde están los archivos
folder <- "h:\\Mi unidad\\Tesis\\Datos con R\\SIAE 2010-2020\\2010\\"

# Obtenemos una lista de todos los archivos txt de la carpeta
files <- list.files(path = folder, pattern = "*.txt", full.names = TRUE)

# Creamos una lista vacía donde guardaremos los dataframes de cada archivo
df_list <- list()

# Leemos cada archivo y guardamos el dataframe en la lista
for (i in 1:length(files)) {
  df_list[[i]] <- read.csv(files[i], sep = ";",colClasses = c("NCODI" = "numeric"), dec=",")
}

# Creamos una copia del primer dataframe de la lista para fusionar con el resto
df <- df_list[[1]] 

# Fusionamos cada dataframe de la lista con el dataframe resultante, eliminando la columna de año
for (i in 2:length(df_list)) {
  df <- full_join(df, df_list[[i]] %>% select(-año), by = "NCODI")
}

NCODI <- as.integer(NCODI)

attach(df)
# df_1<-df %>%  group_by(Depend_agrupada)%>%summarise(media_camas=mean(camas_funcionamiento))

patron=c("ccaa","finali","depend","camas","quirofano","acelerador",  "angiografo","hemodialisis" ,"TAC","RNM","PET", "bombas","densiometros","gamma","litotriptor","mamografo","spect","incubadoras","paritorios","salasHemo","salas_rx","puestos_HDia" )

 # summary_df=df %>%
 #   select(contains("camas"), is.numeric) %>%
 #   summarize_at(vars(contains(patron)), list(mean = mean, median = median, sd = sd))
 # 


vars=c("X70_tIngresos",
       "X700_particular",
       "X708_hospDom")

sapply(df[, vars], mean,na.rm=TRUE)
sapply(df[, vars], max,na.rm=TRUE)
sapply(df[, vars], min,na.rm=TRUE)


patron=c("ccaa","finali","depend","camas","quirofano","acelerador",  "angiografo","hemodialisis" ,"TAC","RNM","PET", "bombas","densiometros","gamma","litotriptor","mamografo","spect","incubadoras","paritorios","salasHemo","salas_rx","puestos_HDia" )

df_1 = df %>% select(año, NCODI, contains(patron), total_cMedicos, farmaceuticos_cTotal,oTituSuperior_cTotal,total_pMedicos,
                     farmaceuticos_pTotal,
                     oTituSuperior_pTotal,
                     total_colabMedicos,
                     farmaceuticos_colabTotal,
                     oTituSuperior_colabTotal,
                      tecSani_cTotal,restSani_cTotal2,
                     tecSani_pTotal,
                     restSani_pTotal2,
                     tecSani_colabTotal,
                     restSani_colabTotal2,
                     administrativos_cTotal,
                     oNoSani_cTotal,
                     administrativos_pTotal,
                     oNoSani_pTotal,
                     
                     totalMir_total,
                     totalEir_total,
                     
                     camas_total, 
                     estancias_total, 
                     altFinal_total, 
                     altCuracion_total, 
                     altTraslaHosp_total, 
                     altExitus_total, 
                     altOtracausa_total, 
                     altTraslaInter_total, 
                     ingreProgr,
                     ingreUrge,
                     total_ingresosHops,
                     
                     progAngioVasc_total,
                     urgeAngioVasc_total,
                     progCirCardio_total,
                     urgeCirCardio_total,
                     progCirGeneral_total,
                     urgeCirGeneral_total,
                     progMaxilo_total,
                     urgeMaxilo_total,
                     progCirPediat_total,
                     urgeCirPediat_total,
                     progPlastica_total,
                     urgePlastica_total,
                     progToracica_total,
                     urgeToracica_total,
                     progDerma_total,
                     urgeDerma_total,
                     progGine_total,
                     urgeGine_total,
                     progCirNeuro_total,
                     urgeCirNeuro_total,
                     progOftalmo_total,
                     urgeOftalmo_total,
                     progORL_total,
                     urgeORL_total,
                     progTrauma_total,
                     urgeTrauma_total,
                     progUrolog_total,
                     urgeUrolog_total,
                     progOtrasCirugias_total,
                     urgeOtrasCirugias_total,
                     totalProg_hosp,
                     totalUrge_hosp,
                     totalProg_cma,
                     totalUrge_cma,
                     totalProg_resto,
                     totalUrge_resto,
                     total_programada,
                     total_urgente,
                     
                     partoVaginal,
                     cesareas,
                     rNacVivos,
                     vivos_menor2500,
                     exitusMaternal,
                     
                     pacienteTot_med,
                     sesionTot_med	,
                     
                     pacienteTot_hospDom,
                     visitaTot_hospDom,
                     
                     primTotal_hosp,
                     total_hosp,
                     primTotal_CEP,
                     total_CEP,
                     
                     Urg_altas,
                     Urg_ingresos,
                     Urg_traslados,
                     Urg_exitus,
                     Urg_total,
                     
                     biopsias_hosp,
                     biopsias_CEP,
                     necropsias_hosp,
                     angio_hosp,
                     angio_CEP,
                     densiometrias_hosp,
                     densiometrias_CEP,
                     gamma_hosp,
                     
                     mamo_hosp,
                     mamo_CEP,
                     pet_hosp,
                     
                     resonancia_hosp,
                     resonancia_CEP,
                     rx_hosp,
                     rx_CEP,
                     spect_hosp,
                     tac_hosp,
                     tac_CEP,
                     
                     altas_particular,
                     estancias_particular,
                     sesionHdia_particular,
                     consulta_particular,
                     cma_particular,
                     urge_particular,
                     hospDom_particular,
                     altas_AsegPriv,
                     estancias_AsegPriv,
                     sesionHdia_AsegPriv,
                     consulta_AsegPriv,
                     cma_AsegPriv,
                     urge_AsegPriv,
                     hospDom_AsegPriv,
                     altas_sns,
                     estancias_sns,
                     sesionHdia_sns,
                     consulta_sns,
                     cma_sns,
                     urge_sns,
                     hospDom_sns,
                     altas_MutuaFun,
                     estancias_MutuaFun,
                     sesionHdia_MutuaFun,
                     consulta_MutuaFun,
                     cma_MutuaFun,
                     urge_MutuaFun,
                     hospDom_MutuaFun,
                     altas_otrEntiPublica,
                     estancias_otrEntiPublica,
                     sesionHdia_otrEntiPublica,
                     consulta_otrEntiPublica,
                     cma_otrEntiPublica,
                     urge_otrEntiPublica,
                     hospDom_otrEntiPublica,
                     altas_MATEPSS,
                     estancias_MATEPSS,
                     sesionHdia_MATEPSS,
                     consulta_MATEPSS,
                     cma_MATEPSS,
                     urge_MATEPSS,
                     hospDom_MATEPSS,
                     altas_convIntern,
                     estancias_conveInter,
                     sesionHdia_conveInter,
                     consulta_conveInter,
                     cma_conveInter,
                     urge_conveInter,
                     hospDom_conveInter,
                     altas_AccTrafic,
                     estancias_AccTrafic,
                     sesionHdia_AccTrafic,
                     consulta_AccTrafic,
                     cma_AccTrafic,
                     urge_AccTrafic,
                     hospDom_AccTrafic,
                     otroRegimen,
                     altas_otroRegim,
                     estancias_otroRegim,
                     sesionHdia_otroRegim,
                     consulta_otroRegim,
                     cma_otroRegim,
                     urge_otroRegim,
                     hospDom_otroRegim,
                     total_altas,
                     total_estancias,
                     total_sesionHdia,
                     total_consulta,
                     total_cma,
                     total_urgencias,
                     total_hospDom,
                     
                     X60_totalCompra,
                     X61_variaExistencias,
                     X62_servExteriores,
                     X64_gastoPersonal,
                     X68_dotaAmortizacion,
                     X69_perdidaDeterioro,
                     X6X_restoGasto,
                     tot_compraGasto,
                     
                     X70_tIngresos,
                     X700_particular,
                     X701_AsegPriv,
                     X701_1_AsistSanitaria,
                     X701_2_AccTrafic,
                     
                     X74_Total_Subvencion,
                     
                     total_ventasIngresos,
                     X70_Hospital,
                     X70_consulExter,
                     X70_CMA,
                     X70_hospDia,
                     X70_Urgencia,
                     X70_hospDom,
                     X700_Hospital,
                     X700_consulExter,
                     X700_CMA,
                     X700_hospDia,
                     X700_Urgencia,
                     X700_hospDom,
                     X701_Hospital,
                     X701_consulExter,
                     X701_CMA,
                     X701_hospDia,
                     X701_Urgencia,
                     X701_hospDom,
                     X701_1Hospital,
                     X701_1consulExter,
                     
                     inver_iIntangible,
                     inver_IMaterial,
                     inver_IMterrenos,
                     inver_IMresto,
                     inver_Otras,
                     inver_Total)
                     
grep("spect", names(df))
names(df)[grep("spect", names(df))]
columns_with_spect <- names(df)[grep("spect", names(df))]
rm(spect_CEP)

sapply(df[, columns_with_spect], mean,na.rm=TRUE)
sapply(df[, columns_with_spect], max,na.rm=TRUE)
sapply(df[, columns_with_spect], min,na.rm=TRUE)
