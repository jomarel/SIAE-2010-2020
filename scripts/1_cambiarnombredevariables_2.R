{install.packages("fs")
install.packages("tzdb")
install.packages("bit64")
install.packages("tidyverse")}

# Cargar configuracion central
config_path <- if (file.exists("00_config.R")) "00_config.R" else file.path("scripts", "00_config.R")
source(config_path)

# Instalar y cargar el paquete 'tidyverse'

library(tidyverse)

# Definir la ruta de la carpeta con los archivos
folder <- file.path(RAW_DIR, "2015")


#Al año 2015 hay que añadirle encabezados al archivo Formacion.txt:
# Crea una lista con los nombres de las columnas
encabezado <- c("año","NCODI","alergologia_total","analisis_total","anatomia_total","anestesiologia_total","angiologia_total","aparato_digestivo_total","bioquimica_total","cardiologia_total","Cirug_Cardiovascular_total","Cirug_General_total","Cirug_Maxilof_total","Cirug_Ortopedica_total","Cirug_Pediatrica_total","Cirug_Plastica_total","Cirug_Toracica_total","dermatologia_total","endocrinologia_total","farmacologia_total","geriatria_total","hematologia_total","inmunologia_total","med_Trabajo_total","med_Familiar_total","med_Fisica_total","med_Intensiva_total","med_Interna_total","med_Nuclear_total","med_Preventiva_total","microbiologia_total","nefrologia_total","neumologia_total","neurocirugia_total","neurofisiologia_total","neurologia_total","obstetricia_total","oftalmologia_total","onco_Medica_total","onco_Radioterapica_total","otorrino_total","pediatria_total","psiquiatria_total","radiodiagnostico_total","reumatologia_total","urologia_total","totalMir_total","alergologia_mujeres","analisis_mujeres","anatomia_mujeres","anestesiologia_mujeres","angiologia_mujeres","aparato_digestivo_mujeres","bioquimica_mujeres","cardiologia_mujeres","Cirug_Cardiovascular_mujeres","Cirug_General_mujeres","Cirug_Maxilof_mujeres","Cirug_Ortopedica_mujeres","Cirug_Pediatrica_mujeres","Cirug_Plastica_mujeres","Cirug_Toracica_mujeres","dermatologia_mujeres","endocrinologia_mujeres","farmacologia_mujeres","geriatria_mujeres","hematologia_mujeres","inmunologia_mujeres","med_Trabajo_mujeres","med_Familiar_mujeres","med_Fisica_mujeres","med_Intensiva_mujeres","med_Interna_mujeres","med_Nuclear_mujeres","med_Preventiva_mujeres","microbiologia_mujeres","nefrologia_mujeres","neumologia_mujeres","neurocirugia_mujeres","neurofisiologia_mujeres","neurologia_mujeres","obstetricia_mujeres","oftalmologia_mujeres","onco_Medica_mujeres","onco_Radioterapica_mujeres","otorrino_mujeres","pediatria_mujeres","psiquiatria_mujeres","radiodiagnostico_mujeres","reumatologia_mujeres","urologia_mujeres","totalMir_mujeres","Eir_Salud_total","Eir_MQ_total","Eir_Trabajo_total","Eir_Familiar_total","Eir_Geriatrica_total","Eir_matronas_total","Eir_Pediatrica_total","totalEir_total","Eir_Salud_mujeres","Eir_MQ_mujeres","Eir_Trabajo_mujeres","Eir_Familiar_mujeres","Eir_Geriatrica_mujeres","Eir_matronas_mujeres","Eir_Pediatrica_mujeres","totalEir_mujeres","Otros_Internos_total","Otro_Postgrado_total","Otros_IInternos_mujeres","Otro_Postgrado_mujeres")

# Definir la ruta del archivo
file <- file.path(RAW_DIR, "2015", "07_Formacion.txt")

# Leer el archivo
df <- read_delim(file, delim = ";", locale=locale(decimal_mark = ","))
# Usa write.table() para escribir el archivo de texto con el encabezado
write.table(df, file, row.names = FALSE, col.names = encabezado, sep = ";")


# Obtener una lista de todas las carpetas en la ruta de datos en bruto
folders <- list.dirs(RAW_DIR)

# Iterar sobre cada carpeta
for (folder in folders) {
  
  # Leer todos los archivos de la carpeta
  files <- list.files(path = folder, pattern = "*.txt", full.names = TRUE)
  
  # Iterar sobre cada archivo
  for (file in files) {
    
    df <- read_delim(file, delim = ";", locale=locale(decimal_mark = ","))
    df %>% mutate_if(is.character, ~iconv(., from = "UTF-8", to = "UTF-8"))
    # Cambiar el nombre de la variable "Año" por "anyo"
    names(df)[names(df) == "Anyo"] <- "anyo"
    # Cambiar el nombre de la variable "year" por "anyo"
    names(df)[names(df) == "year"] <- "anyo"
    names(df)[names(df) == "camas_instaladas"] <- "camas_instalada"
    names(df)[names(df) == "camas instaladas"] <- "camas_instalada"
    names(df)[names(df) == "Camas_instaladas"] <- "camas_instalada"
    names(df)[names(df) == "Camas instaladas"] <- "camas_instalada"
    names(df)[names(df)=="Cod CCAA (Todas)"] <- "ccaa_codigo"
    names(df)[names(df)=="ccaa_Codigo"] <- "ccaa_codigo"
    names(df)[names(df)=="Desc CCAA (Todas)"] <- "ccaa"
    names(df)[names(df)=="Cod Grupo Finalidad"] <- "cod_finalidad_agrupada"
    names(df)[names(df)=="FINALIDAD agrupada para Anonimizacion"]<- "Finalidad_agrupada"
    names(df)[names(df)=="Cod Pertenencia SNS"]<- "cod_depend_agrupada"
    names(df)[names(df)=="Desc Pertenencia SNS"]<- "Depend_agrupada"
    names(df)[names(df)=="60_totalCompra"]<- "G_totalCompra"
    names(df)[names(df)=="61_variaExistencias"]<- "G_variaExistencias"
    names(df)[names(df)=="62_servExteriores"]<- "G_servExteriores"
    names(df)[names(df)=="64_gastoPersonal"]<- "G_gastoPersonal"
    names(df)[names(df)=="68_dotaAmortizacion"]<- "G_dotaAmortizacion"
    names(df)[names(df)=="69_perdidaDeterioro"]<- "G_perdidaDeterioro"
    names(df)[names(df)=="6X_restoGasto"]<- "G_restoGasto"
    names(df)[names(df)=="tot_compraGasto"]<- "G_totGastos"
    names(df)[names(df)=="70_tIngresos"]<- "I_totIngresosPS"
    names(df)[names(df)=="700_particular"]<- "I_particular"
    names(df)[names(df)=="701_AsegPriv"]<- "I_AsegPriv"
    names(df)[names(df)=="701_1_AsistSanitaria"]<- "I_AsistSanitaria"
    names(df)[names(df)=="701_2_AccTrafic"]<- "I_AccTrafic"
    names(df)[names(df)=="702_MATEPSS"]<- "I_MATEPSS"
    names(df)[names(df)=="704_SNS"]<- "I_SNS"
    names(df)[names(df)=="705_1_FdirectaSS"]<- "I_FdirectaSS"
    names(df)[names(df)=="705_2_FdirectaAPriv_MATEPSS"]<- "I_FdirectaAPriv_MATEPSS"
    names(df)[names(df)=="706_OyEntiPublica"]<- "I_OyEntiPublica"
    names(df)[names(df)=="708_bonificaciones"]<- "I_bonificaciones"
    names(df)[names(df)=="709_Otros_Ips"]<- "I_Otros_Ips"
    names(df)[names(df)=="74_Total_Subvencion"]<- "I_Total_Subvencion"
    names(df)[names(df)=="7X_restoIngresos"]<- "I_restoIngresos"
    names(df)[names(df)=="total_ventasIngresos"]<- "I_totIngresos"
    
	
    # Guardar el archivo con el nombre original
    write_delim(df, file, delim = ";", quote = "all")
  }
}




  # Definir la ruta del archivo
file <- file.path(RAW_DIR, "2019", "11_Actividadobstetrica.txt")

# Leer el archivo
df <- read_delim(file, delim = ";")

# Añadir una columna de nombre "año" con valor 2019 al principio del dataframe
df <- df %>% mutate(año = 2019) %>% select(-2)



# Guardar el archivo con la columna añadida y manteniendo las comillas en los nombres de variables
write_delim(df, file, delim = ";", quote = "all")
