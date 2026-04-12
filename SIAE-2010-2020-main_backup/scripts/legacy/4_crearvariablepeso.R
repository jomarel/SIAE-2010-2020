# rstudioapi::writeRStudioPreference("data_viewer_max_columns", 300L)
library(dpyr)
library(tidyverse)
setwd("h:/Mi unidad/Tesis/Datos con R/SIAE 2010-2020")
# Cargar las bases de datos
df_final <- read.table("h:\\Mi unidad\\Tesis\\Datos con R\\SIAE 2010-2020\\df_final.txt", sep = ";", dec = ",", header=TRUE)

pesos <- read.table("h:\\Mi unidad\\Tesis\\Datos con R\\SIAE 2010-2020\\pesos.txt",sep = ";", dec = ",", header = TRUE)
# Unir las bases de datos por NCODI y anyo
df <- merge(df_final, pesos, by = c("NCODI", "anyo"), all = TRUE)

# Cargar los nombres de las variables desde el archivo de texto
nombres_variables <- read.table("h:/Mi unidad/Tesis/Datos con R/R con Chatbot/Nombres_variables.txt", 
                                header = FALSE, stringsAsFactors = FALSE)

# Cargar las descripciones de las variables desde el archivo de texto
archivo_descripciones <- read.table("h:/Mi unidad/Tesis/Datos con R/R con Chatbot/variables seleccionadas y descripción.txt", 
                                    header = FALSE, sep = "\t", stringsAsFactors = FALSE)

# Obtener los nombres de las variables del dataframe df
nombres_df <- names(df)
library(Hmisc)

# Añadir las descripciones como etiquetas a las variables del dataframe df
for (i in seq_along(nombres_df)) {
  label(df[[nombres_df[i]]]) <- archivo_descripciones[i, 2]  # Segunda columna con las descripciones
}

# Suponiendo que la variable peso está en la última posición
df <- df %>% select(NCODI, anyo, peso, everything())
write.table(df, "h:/Mi unidad/Tesis/Datos con R/SIAE 2010-2020/df.txt", row.names = FALSE, sep = ";", dec = ",")

# Instalar y cargar el paquete haven
install.packages("haven")
library(haven)

# Guardar el dataframe en formato Stata, conservando las etiquetas
write_dta(df, "df_lab.dta")

# Leer el archivo df_lab.dta
df <- read_dta("h:/Mi unidad/Tesis/Datos con R/SIAE 2010-2020/df_lab.dta")


library(utils)

# Read the dataset
data <- read.table("h:/Mi unidad/Tesis/Datos con R/SIAE 2010-2020/df.txt", sep = ";", dec = ",", header=TRUE)

#view your data frame in a separate window
utils::View(data)


# correlacionar peso con otras variables cuanti





data2=data %>% transmute(
  
  medic_staff = total_cMedicos+farmaceuticos_cTotal+oTituSuperior_cTotal+total_pMedicos+farmaceuticos_pTotal+oTituSuperior_pTotal+total_colabMedicos+farmaceuticos_colabTotal+oTituSuperior_colabTotal,
                
tec=acelerador_hospital+
  acelerador_CEP+
  acelerador_concertado+
  angiografo_hospital+
  angiografo_CEP+
  angiografo_concertado+
  bombas_hospital+
  bombas_CEP+
  bombas_concertado+
  densiometros_hospital+
  densiometros_CEP+
  densiometros_concertado+
  hemodialisis_hospital+
  hemodialisis_CEP+
  hemodialisis_concertado+
  TAC_hospital+
  TAC_CEP+
  TAC_concertado+
  PET_hospital+
  PET_CEP+
  PET_concertado+
  RNM_hospital+
  RNM_CEP+
  RNM_concertado+
  gammacamara_hospital+
  gammacamara_CEP+
  gammacamara_concertado+
  litotriptor_hospital+
  litotriptor_CEP+
  litotriptor_concertado+
  mamografos_hospital+
  mamografos_CEP+
  mamografos_concertado+
  spect_hospital+
  spect_concertado,

capital=
  camas_funcionamiento+
  incubadoras_funcionamiento+
  paritorios_funcionamiento+
  quirofanos_funcionamiento+
  salasHemo_funcionamiento+
  salas_rx_Hospital+
  salas_rx_CEP+
  puesto_HDia_medico+
  puesto_HDia_psiquiatrico+
  puesto_HDia_geriatrico+
  integradasPuestos_HDia_CMA+
  propiasPuestos_HDia_CMA+
  propiasQuirofanos_HDia_CMA,

enf_staff=due_cTotal+
  matronas_cTotal+
  oDueEspeciali_cTotal+
  fisioterapeutas_cTotal+
  terapeutas_cTotal+
  logopedas_cTotal+
  oSaniMedio_cTotal+
  gMedioSani_cTotal+
  gSuperiorSani_cTotal+
  tecSani_cTotal+
  restSani_cTotal2+
  due_pTotal+
  matronas_pTotal+
  oDueEspeciali_pTotal+
  fisioterapeutas_pTotal+
  terapeutas_pTotal+
  logopedas_pTotal+
  oSaniMedio_pTotal+
  gMedioSani_pTotal+
  gSuperiorSani_pTotal+
  tecSani_pTotal+
  restSani_pTotal2+
  administrativos_cTotal+
  oNoSani_cTotal+
  administrativos_pTotal+
  oNoSani_pTotal+
  administrativos_colabTotal+
  oNoSani_colabTotal,

teach_staff=totalMir_total+
  totalEir_total,

actividad=estancias_total+
  altFinal_total+total_ingresosHops+total_programada+
  total_urgente+
  partoVaginal+
  cesareas




)

data %>% 
  
  
  
  data2 <- data2 %>% select(NCODI, anyo, peso, medic_staff, everything())



df_con_peso <- df %>% filter(!is.na(peso))
df_sin_peso <- df %>% filter(is.na(peso))

variables_con_un_nivel <- colnames(df_con_peso)[sapply(df_con_peso, function(x) is.factor(x) && nlevels(x) < 2)]

str(df)


df_con_peso <- df_con_peso %>% select(-one_of(variables_con_un_nivel))
df_sin_peso <- df_sin_peso %>% select(-one_of(variables_con_un_nivel))


cor(df_con_peso)


str(df_con_peso)


# Selecciona las variables independientes (todas las columnas excepto NCODI y peso)
variables_independientes <- colnames(df_con_peso)[!(colnames(df_con_peso) %in% c("NCODI", "peso", "ccaa",
                                                                                 "Finalidad_agrupada",
                                                                                 "Depend_agrupada",
                                                                                 "camas_instalada"))]

# Crea la fórmula para el modelo de regresión
formula_peso <- as.formula(paste("peso ~", paste(variables_independientes, collapse = " + ")))

# Entrena el modelo de regresión múltiple
modelo_peso <- lm(formula_peso, data = df_con_peso)


##################################### mice
install.packages("mice")
# Load required packages
library(mice)

# Read the dataset
df <- read.table("h:/Mi unidad/Tesis/Datos con R/SIAE 2010-2020/df.txt", header = TRUE)




# Convert "anyo" and "NCODI" to factors
df$anyo <- as.factor(df$anyo)
df$NCODI <- as.factor(df$NCODI)

# Create a formula for imputation
formula <- as.formula("peso ~ anyo + NCODI + other_variables")

# Perform multiple imputation
imputed_data <- mice(df, m = 5, method = "pmm", formula = formula)

# Get the completed datasets
completed_data <- complete(imputed_data, "long", include = TRUE)

# Calculate the proxy for "peso"
proxy_peso <- with(completed_data, aggregate(.imp, by = list(anyo, NCODI), FUN = mean, na.rm = TRUE))

# Assign the proxy values to the original dataset
df$proxy_peso <- proxy_peso$x[match(interaction(df$anyo, df$NCODI), interaction(proxy_peso$Group.1, proxy_peso$Group.2))]

# View the resulting dataset with the proxy values
head(df)
############################################################################
install.packages(c("tidyverse", "caret", "randomForest", "missForest"))
library(tidyverse)
library(caret)
library(randomForest)
library(missForest)

data <- read.table("h:/Mi unidad/Tesis/Datos con R/SIAE 2010-2020/df.txt", sep = ";", dec = ",", header=TRUE)

split_data <- function(data, train_ratio = 0.8) {
  train_index <- sample(1:nrow(data), size = round(nrow(data) * train_ratio))
  train_data <- data[train_index, ]
  test_data <- data[-train_index, ]
  list(train_data = train_data, test_data = test_data)
}
data_split <- split_data(data)
train_data <- data_split$train_data
test_data <- data_split$test_data

lm_impute <- function(train_data, test_data, target_var) {
  complete_data <- train_data %>% filter(!is.na(!!sym(target_var)))
  model <- lm(as.formula(paste(target_var, "~ .")), data = complete_data)
  test_data[is.na(test_data[[target_var]]), target_var] <- predict(model, newdata = test_data[is.na(test_data[[target_var]]), ])
  test_data
}

imputed_data_lm <- lm_impute(train_data, test_data, "peso")

#########################################################################
