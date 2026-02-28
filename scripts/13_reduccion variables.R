# Instalar y cargar dplyr si no está instalado
if (!require(dplyr)) install.packages("dplyr")
library(dplyr)

# Cargar el archivo RData
load("h:\\Mi unidad\\Tesis\\Datos con R\\SIAE 2010-2020\\df_depurado.RData")

# Convertir los datos a un dataframe si no lo es
df <- as.data.frame(df_depurado)

# Usar dplyr para crear las variables de dotación tecnológica, manteniendo todas las columnas originales
df <- df %>%
  mutate(
    altatecno = TAC_total + PET_total + RNM_total + 
      gammacamara_total + acelerador_total + 
      litotriptor_total + bombas_total,
    tecno_basica = salas_rx_total + mamografos_total + 
      densiometros_total + angiografo_total + 
      hemodialisis_total
  )

# # Mostrar un resumen de las nuevas variables junto con el resto de las variables
# summary(df)
# 

# Usar dplyr para crear las nuevas variables de personal
df <- df %>%
  mutate(
    # Crear variable para personal médico (médicos a jornada completa, parcial y colaboradores)
    personal_medico = total_cMedicos + total_pMedicos + total_colabMedicos +
      farmaceuticos_cTotal + farmaceuticos_pTotal + farmaceuticos_colabTotal,
    
    # Crear variable para personal de enfermería y auxiliares
    personal_aux = due_cTotal + matronas_cTotal + oDueEspeciali_cTotal +
      fisioterapeutas_cTotal + terapeutas_cTotal + logopedas_cTotal +
      oSaniMedio_cTotal + gMedioSani_cTotal + gSuperiorSani_cTotal + 
      tecSani_cTotal + restSani_cTotal2 +
      due_pTotal + matronas_pTotal + oDueEspeciali_pTotal +
      fisioterapeutas_pTotal + terapeutas_pTotal + logopedas_pTotal + 
      oSaniMedio_pTotal + gMedioSani_pTotal + gSuperiorSani_pTotal + 
      tecSani_pTotal + restSani_pTotal2,
    
    # Crear variable para el resto del personal (administrativos y no sanitarios)
    personal_resto = administrativos_cTotal + administrativos_pTotal + 
      administrativos_colabTotal + oNoSani_cTotal + 
      oNoSani_pTotal + oNoSani_colabTotal
  )




# Usar dplyr para calcular las sumas, los porcentajes de discrepancia y filtrar
df <- df %>%
  mutate(
    # Calcular las sumas por categoría
    altas_sumadas = altas_particular + altas_AsegPriv + altas_sns + altas_MutuaFun + altas_otrEntiPublica + altas_MATEPSS + altas_convIntern + altas_AccTrafic + altas_otroRegim,
    cma_sumadas = cma_particular + cma_AsegPriv + cma_sns + cma_MutuaFun + cma_otrEntiPublica + cma_MATEPSS + cma_conveInter + cma_AccTrafic + cma_otroRegim,
    sesionHdia_sumadas = sesionHdia_particular + sesionHdia_AsegPriv + sesionHdia_sns + sesionHdia_MutuaFun + sesionHdia_otrEntiPublica + sesionHdia_MATEPSS + sesionHdia_conveInter + sesionHdia_AccTrafic + sesionHdia_otroRegim,
    consulta_sumadas = consulta_particular + consulta_AsegPriv + consulta_sns + consulta_MutuaFun + consulta_otrEntiPublica + consulta_MATEPSS + consulta_conveInter + consulta_AccTrafic + consulta_otroRegim,
    urgencias_sumadas = urge_particular + urge_AsegPriv + urge_sns + urge_MutuaFun + urge_otrEntiPublica + urge_MATEPSS + urge_conveInter + urge_AccTrafic + urge_otroRegim,
    estancias_sumadas = estancias_particular + estancias_AsegPriv + estancias_sns + estancias_MutuaFun + estancias_otrEntiPublica + estancias_MATEPSS + estancias_conveInter + estancias_AccTrafic + estancias_otroRegim,
    
    # Calcular el porcentaje de discrepancia para cada categoría
    pct_altas = abs(total_altas - altas_sumadas) / total_altas * 100,
    pct_cma = abs(total_cma - cma_sumadas) / total_cma * 100,
    pct_sesionHdia = abs(total_sesionHdia - sesionHdia_sumadas) / total_sesionHdia * 100,
    pct_consulta = abs(total_consulta - consulta_sumadas) / total_consulta * 100,
    pct_urgencias = abs(total_urgencias - urgencias_sumadas) / total_urgencias * 100,
    pct_estancias = abs(total_estancias - estancias_sumadas) / total_estancias * 100
  )

# Filtrar los hospitales que tienen discrepancias mayores al 5% en cualquiera de las categorías
df_filtrado <- df %>%
  filter(
    is.na(pct_altas) | pct_altas <= 5,
    is.na(pct_cma) | pct_cma <= 5,
    is.na(pct_sesionHdia) | pct_sesionHdia <= 5,
    is.na(pct_consulta) | pct_consulta <= 5,
    is.na(pct_urgencias) | pct_urgencias <= 5,
    is.na(pct_estancias) | pct_estancias <= 5
  )

# Ver el número de hospitales antes y después del filtrado
cat("Número de hospitales antes del filtrado:", nrow(df), "\n")
cat("Número de hospitales después del filtrado:", nrow(df_filtrado), "\n")




# Crear un nuevo dataframe con las variables seleccionadas
df_seleccion <- df_filtrado %>%
  select(
    NCODI, anyo, ccaa, peso, mix, ccaa_codigo, 
    cod_depend_agrupada, Depend_agrupada,
    camas_funcionamiento, incubadoras_funcionamiento, 
    paritorios_funcionamiento, quirofanos_funcionamiento, 
    salasHemo_funcionamiento, acelerador_concertado, angiografo_concertado, 
    bombas_concertado, densiometros_concertado, hemodialisis_concertado, 
    TAC_concertado, PET_concertado, RNM_concertado, gammacamara_concertado, 
    litotriptor_concertado, mamografos_concertado, spect_concertado,
    totalMir_total, totalEir_total, estancias_total, altFinal_total, 
    altCuracion_total, altTraslaHosp_total, altExitus_total, altOtracausa_total, 
    altTraslaInter_total, ingreProgr, ingreUrge, total_ingresosHops, 
    primTotal_hosp, total_hosp, primTotal_CEP, total_CEP,
Urg_ingresos, Urg_traslados, Urg_exitus, Urg_total, estancias_medicina, altFinal_med, altCuracion_med, estancias_cirugia, altFinal_cirugia, altCuracion_cirugia, estancias_trauma, altFinal_trauma, altCuracion_trauma, estancias_gine, altFinal_gine, altCuracion_gine, estancias_pediatria, altFinal_pediatria, altCuracion_pediatria, estancias_neonatologia, altFinal_neonatologia, altCuracion_neonatologia, estancias_restoPedia, altFinal_restoPedia, altCuracion_restoPedia, estancias_mIntensiva, aFinales_mIntensiva, altCuracion_mIntensiva, estancias_uci, altFinal_uci, altCuracion_uci, estancias_uCoronarios, altFinal_uCoronarios, altCuracion_uCoronarios, estancias_uNeonatales, altFinal_uNeonatales, altCuracion_uNeonatales, estancias_uQuemados, altFinal_uQuemados, 
        altas_particular, estancias_particular, sesionHdia_particular, consulta_particular, 
    cma_particular, urge_particular, hospDom_particular,
    altas_AsegPriv, estancias_AsegPriv, sesionHdia_AsegPriv, consulta_AsegPriv, 
    cma_AsegPriv, urge_AsegPriv, hospDom_AsegPriv, 
    altas_sns, estancias_sns, sesionHdia_sns, consulta_sns, cma_sns, urge_sns, hospDom_sns,
    altas_MutuaFun, estancias_MutuaFun, sesionHdia_MutuaFun, consulta_MutuaFun, 
    cma_MutuaFun, urge_MutuaFun, hospDom_MutuaFun,
    altas_otrEntiPublica, estancias_otrEntiPublica, sesionHdia_otrEntiPublica, consulta_otrEntiPublica, 
    cma_otrEntiPublica, urge_otrEntiPublica, hospDom_otrEntiPublica,
    altas_MATEPSS, estancias_MATEPSS, sesionHdia_MATEPSS, consulta_MATEPSS, 
    cma_MATEPSS, urge_MATEPSS, hospDom_MATEPSS,
    altas_convIntern, estancias_conveInter, sesionHdia_conveInter, consulta_conveInter, 
    cma_conveInter, urge_conveInter, hospDom_conveInter,
    altas_AccTrafic, estancias_AccTrafic, sesionHdia_AccTrafic, consulta_AccTrafic, 
    cma_AccTrafic, urge_AccTrafic, hospDom_AccTrafic,
    altas_otroRegim, estancias_otroRegim, sesionHdia_otroRegim, consulta_otroRegim, 
    cma_otroRegim, urge_otroRegim, hospDom_otroRegim,
    total_altas, total_estancias, total_sesionHdia, total_consulta, 
    total_cma, total_urgencias, total_hospDom,
    G_totalCompra, G_gastoPersonal, G_totGastos,
    I_totIngresosPS, I_particular, I_AsegPriv, I_AsistSanitaria, 
    I_AccTrafic, I_MATEPSS, I_SNS, I_FdirectaSS, 
    I_FdirectaAPriv_MATEPSS, I_OyEntiPublica, I_bonificaciones, I_Otros_Ips,
    I_Total_Subvencion, I_restoIngresos, I_totIngresos,
    salas_rx_total, acelerador_total, angiografo_total, bombas_total, 
    densiometros_total, hemodialisis_total, TAC_total, PET_total, RNM_total, 
    gammacamara_total, litotriptor_total, mamografos_total, spect_total,
    altatecno, tecno_basica, personal_medico, personal_aux, personal_resto,
    biopsias_total, angio_total,densiometrias_total,    gamma_total, mamo_total,
    pet_total,    resonancia_total,    rx_total,    spect_total,tac_total
    )

# Verificar el número de filas y columnas en el nuevo dataframe
cat("Número de filas:", nrow(df_seleccion), "\n")
cat("Número de columnas:", ncol(df_seleccion), "\n")

# Guardar el dataframe con las variables seleccionadas  en un archivo CSV
write.csv(df_seleccion, "h:\\Mi unidad\\Tesis\\Datos con R\\SIAE 2010-2020\\df_seleccion.csv", row.names = FALSE)

# # Mostrar una vista previa del nuevo dataframe
# head(df_seleccion)

# 
# # Crear un vector con las etiquetas de las variables seleccionadas
# variable_labels <- c(
#   "código de centro anonimizado", "año", "nombre comunidad autónoma",
#   "peso medio GRD hospital", "proxy de peso case-mix del hospital",
#   "código ine comunidad autónoma", "código de dependencia agrupada",
#   "descripción de la dependencia", "camas en funcionamiento",
#   "incubadoras en funcionamiento", "paritorios en funcionamiento",
#   "quirófanos totales en funcionamiento", "salas de hemodinámica en funcionamiento",
#   "acelerador lineal concertado", "angiografía digital concertada",
#   "bomba de cobalto concertadas", "densitómetros concertados",
#   "equipos de hemodiálisis concertadas", "equipos de TAC concertados",
#   "equipos de PET concertados", "equipos de resonancia magnética concertados",
#   "gammacámaras concertadas", "litotricia renal concertada",
#   "mamógrafos concertados", "equipos de tomografía por emisión de fotones concertados",
#   "total MIR", "total EIR", "estancias total hospital",
#   "altas total hospital", "altas por curación total hospital",
#   "altas por traslado hospital", "altas por fallecimiento hospital",
#   "altas por otras causas total hospital", "altas por traslados interservicio",
#   "ingresos programados", "ingresos urgentes", "total ingresos",
#   "1ª consultas hospital en total hospital", "consultas totales hospital",
#   "1ª consultas centro de especialidades", "consultas totales centro de especialidades",
#   "altas en prestación de servicios a particulares", "estancias en prestación de servicios a particulares",
#   "hospital de día en prestación de servicios a particulares", "consultas totales en prestación de servicios a particulares",
#   "intervenciones de CMA en prestación de servicios a particulares", "urgencias en prestación de servicios a particulares",
#   "hospitalización a domicilio en prestación de servicios a particulares",
#   "altas aseguradoras privadas", "estancias aseguradoras privadas",
#   "hospital de día aseguradoras privadas", "consultas aseguradoras privadas",
#   "intervenciones de CMA aseguradoras privadas", "urgencias aseguradoras privadas",
#   "hospitalización a domicilio aseguradoras privadas", "altas SNS",
#   "estancias SNS", "hospital de día SNS", "consultas SNS",
#   "intervenciones de CMA SNS", "urgencias SNS", "hospitalización a domicilio SNS",
#   "altas mutualidades", "estancias mutualidades", "hospital de día mutualidades",
#   "consultas mutualidades", "intervenciones de CMA mutualidades", "urgencias mutualidades",
#   "hospitalización a domicilio mutualidades", "altas otras entidades públicas",
#   "estancias otras entidades públicas", "hospital de día otras entidades públicas",
#   "consultas otras entidades públicas", "intervenciones de CMA otras entidades públicas",
#   "urgencias otras entidades públicas", "hospitalización a domicilio otras entidades públicas",
#   "altas MATEPSS", "estancias MATEPSS", "hospital de día MATEPSS",
#   "consultas MATEPSS", "intervenciones de CMA MATEPSS", "urgencias MATEPSS",
#   "hospitalización a domicilio MATEPSS", "altas convenios internacionales",
#   "estancias convenios internacionales", "hospital de día convenios internacionales",
#   "consultas convenios internacionales", "intervenciones de CMA convenios internacionales",
#   "urgencias convenios internacionales", "hospitalización a domicilio convenios internacionales",
#   "altas accidentes tráfico", "estancias accidentes tráfico",
#   "hospital de día accidentes tráfico", "consultas accidentes tráfico",
#   "intervenciones de CMA accidentes tráfico", "urgencias accidentes tráfico",
#   "hospitalización a domicilio accidentes tráfico", "altas otros",
#   "estancias otros", "hospital de día otros", "consultas otros",
#   "intervenciones de CMA otros", "urgencias otros",
#   "hospitalización a domicilio otros", "total altas", "total estancias",
#   "total hospital de día", "total consultas", "total intervenciones CMA",
#   "total urgencias", "total hospitalización a domicilio", "compras",
#   "gastos de personal", "total compras y gastos", "ingresos por prestación de servicios",
#   "prestación de servicios particulares", "concertados con entidades aseguradoras privadas",
#   "seguros de asistencia sanitaria y enfermedad", "accidentes de tráfico",
#   "mutuas accidentes trabajo y emp colaboradoras", "concertados con el sistema nacional de salud",
#   "financiación directa servicios de salud y otras entidades públicas",
#   "financiación directa aseguradoras privadas y MATEPSS", "procedente de otras entidades públicas",
#   "bonificaciones", "otros ingresos", "subvenciones, donaciones y legados",
#   "total ventas e ingresos", "equipos alta tecnología hospital",
#   "tecnología básica hospital", "personal médico", "personal auxiliar",
#   "resto de personal"
# )
# 
# # Asignar etiquetas usando un bucle
# for (i in seq_along(variable_labels)) {
#   df_seleccion[[i]] <- set_label(df_seleccion[[i]], label = variable_labels[i])
# }
# 

# 
# # Asegúrate de que tu dataframe se llame correctamente, aquí lo llamo "df_seleccion"
# # Calcula la nueva variable 'porcentaje_no_defuncion'
# df_seleccion$porcentaje_no_defuncion <- (df_seleccion$total_altas - df_seleccion$altExitus_total) / df_seleccion$total_altas

# Calcular la nueva variable 'porcentaje_no_defuncion' utilizando dplyr
df_seleccion <- df_seleccion %>%
  mutate(porcentaje_no_defuncion = (total_altas - altExitus_total) / total_altas)
df_seleccion <- df_seleccion %>%
  mutate(tecnologia_total = altatecno + tecno_basica)
# Crear la variable de intensidad docente
df_seleccion <- df_seleccion %>%
  mutate(intensidad_docente = totalMir_total + totalEir_total)



# 
# # CREACIÓN DE GRUPOS (5) SEGUN EL CRITERIO DE GRUPOS DE CONGLOMERADOS DEL MINISTERIO
# df_seleccion <- df_seleccion %>%
#   mutate(
#     intensidad_docente = totalMir_total + totalEir_total,
#     grupo_hospital = case_when(
#       camas_funcionamiento < 200 & altatecno <= 2 & personal_medico <100 ~ 1 & mix <=0.5,
#       camas_funcionamiento < 500 & altatecno <= 3 & personal_medico < 200 & mix <= 0.7 & intensidad_docente <= 10 ~ 2,
#       camas_funcionamiento < 800 & altatecno <= 5 & personal_medico < 300 & mix <= 1.0 & intensidad_docente <= 50 ~ 3,
#       camas_funcionamiento < 1000 & altatecno <= 15 & personal_medico < 500 & mix <= 1.1 & intensidad_docente <= 100 ~ 4,
#       camas_funcionamiento >= 1000 & altatecno > 15 & personal_medico >= 500 & mix > 1.1 & intensidad_docente > 100 ~ 5,
#       TRUE ~ NA_real_
#     )
#   )
# 

# Cargar el archivo .RData
# Cargar las librerías necesarias
library(dplyr)
library(ggplot2)
# Instalar y cargar dplyr si no está instalado
if (!require(factoextra)) install.packages("factoextra")
 library(factoextra)  # Para visualización de clusters
library(cluster)
# Verificar si el paquete "sjlabelled" está instalado; si no, instalarlo
if (!requireNamespace("sjlabelled", quietly = TRUE)) {
  install.packages("sjlabelled")
}

# Cargar el paquete
library(sjlabelled)

# Seleccionar las variables de interés para la creación de grupos
df_clustering <- df_seleccion %>%
  select(camas_funcionamiento, altatecno, mix, totalMir_total, mix, personal_medico) %>%
  
  na.omit()  # Eliminar filas con valores faltantes

# Normalizar las variables
df_clustering_scaled <- scale(df_clustering)



set.seed(123)  # Para reproducibilidad
kmeans_result <- kmeans(df_clustering_scaled, centers = 4, nstart = 25)

# Añadir el grupo asignado al dataframe original
df_seleccion$grupo_kmeans <- kmeans_result$cluster

# Verificar la cantidad de hospitales en cada grupo
table(df_seleccion$grupo_kmeans)

# Resumen descriptivo por grupo
estadisticas_clusters <- df_seleccion %>%
  group_by(grupo_kmeans) %>%
  summarise(
    camas_media = mean(camas_funcionamiento, na.rm = TRUE),
    altatecno_media = mean(altatecno, na.rm = TRUE),
    personal_medico_media = mean(personal_medico, na.rm = TRUE),
    totalMir_media = mean(totalMir_total, na.rm = TRUE),
    n = n()
  )


# Mostrar el resumen
print(estadisticas_clusters)

# library(cluster)
# library(factoextra)
# 
# # Calcular la silueta utilizando el resultado del k-means y los datos normalizados
# silhouette_kmeans <- silhouette(kmeans_result$cluster, dist(df_clustering_scaled))
# 
# # Visualizar el coeficiente de silueta
# fviz_silhouette(silhouette_kmeans) +
#   labs(title = "Coeficiente de Silueta para k-means")
# 

ggplot(df_seleccion, aes(x = camas_funcionamiento, y = tecnologia_total, color = as.factor(grupo_kmeans))) +
  geom_point(size = 3) +
  labs(
    title = "Camas vs. Tecnología Total por Grupo (k-means)",
    x = "Camas en Funcionamiento",
    y = "Dotación Tecnológica Total",
    color = "Grupo k-means"
  ) +
  theme_minimal()

# ggplot(df_seleccion, aes(x = personal_medico, y = mix, color = as.factor(grupo_kmeans))) +
#   geom_point(size = 3) +
#   labs(
#     title = "Personal Médico vs. Mix por Grupo (k-means)",
#     x = "Personal Médico",
#     y = "Mix (Complejidad Asistencial)",
#     color = "Grupo k-means"
#   ) +
#   theme_minimal()
# ggplot(df_seleccion, aes(x = tecnologia_total, y = intensidad_docente, color = as.factor(grupo_kmeans))) +
#   geom_point(size = 3) +
#   labs(
#     title = "Tecnología vs. Intensidad Docente por Grupo (k-means)",
#     x = "Dotación Tecnológica Total",
#     y = "Intensidad Docente",
#     color = "Grupo k-means"
#   ) +
#   theme_minimal()
# 



# Verifica que la variable se haya creado correctamente
summary(df_seleccion$porcentaje_no_defuncion)




# Crear un vector con las etiquetas de las variables seleccionadas
variable_labels <- c("código de centro anonimizado",
  "año",
  "nombre comunidad autónoma",
  "peso medio GRD hospital",
  "proxy de peso case-mix del hospital",
  "código ine comunidad autónoma",
  "código de dependencia agrupada",
  "descripción de la dependencia",
  "camas en funcionamiento",
  "incubadoras en funcionamiento",
  "paritorios en funcionamiento",
  "quirófanos totales en funcionamiento",
  "salas de hemodinámica en funcionamiento",
  "acelerador lineal concertado",
  "angiografía digital concertada",
  "bomba de cobalto concertadas",
  "densitómetros concertados",
  "equipos de hemodiálisis concertadas",
  "equipos de TAC concertados",
  "equipos de PET concertados",
  "equipos de resonancia magnética concertados",
  "gammacámaras concertadas",
  "litotricia renal concertada",
  "mamógrafos concertados",
  "equipos de tomografía por emisión de fotones concertados",
  "total MIR",
  "total EIR",
  "estancias total hospital",
  "altas total hospital",
  "altas por curación total hospital",
  "altas por traslado hospital",
  "altas por fallecimiento hospital",
  "altas por otras causas total hospital",
  "altas por traslados interservicio",
  "ingresos programados",
  "ingresos urgentes",
  "total ingresos",
  "1ª consultas hospital en total hospital",
  "consultas totales hospital",
  "1ª consultas centro de especialidades",
  "consultas totales centro de especialidades",
  "altas en prestación de servicios a particulares",
  "estancias en prestación de servicios a particulares",
  "hospital de día en prestación de servicios a particulares",
  "consultas totales en prestación de servicios a particulares",
  "intervenciones de CMA en prestación de servicios a particulares",
  "urgencias en prestación de servicios a particulares",
  "hospitalización a domicilio en prestación de servicios a particulares",
  "altas aseguradoras privadas",
  "estancias aseguradoras privadas",
  "hospital de día aseguradoras privadas",
  "consultas aseguradoras privadas",
  "intervenciones de CMA aseguradoras privadas",
  "urgencias aseguradoras privadas",
  "hospitalización a domicilio aseguradoras privadas",
  "altas SNS",
  "estancias SNS",
  "hospital de día SNS",
  "consultas SNS",
  "intervenciones de CMA SNS",
  "urgencias SNS",
  "hospitalización a domicilio SNS",
  "altas mutualidades",
  "estancias mutualidades",
  "hospital de día mutualidades",
  "consultas mutualidades",
  "intervenciones de CMA mutualidades",
  "urgencias mutualidades",
  "hospitalización a domicilio mutualidades",
  "altas otras entidades públicas",
  "estancias otras entidades públicas",
  "hospital de día otras entidades públicas",
  "consultas otras entidades públicas",
  "intervenciones de CMA otras entidades públicas",
  "urgencias otras entidades públicas",
  "hospitalización a domicilio otras entidades públicas",
  "altas MATEPSS",
  "estancias MATEPSS",
  "hospital de día MATEPSS",
  "consultas MATEPSS",
  "intervenciones de CMA MATEPSS",
  "urgencias MATEPSS",
  "hospitalización a domicilio MATEPSS",
  "altas convenios internacionales",
  "estancias convenios internacionales",
  "hospital de día convenios internacionales",
  "consultas convenios internacionales",
  "intervenciones de CMA convenios internacionales",
  "urgencias convenios internacionales",
  "hospitalización a domicilio convenios internacionales",
  "altas accidentes tráfico",
  "estancias accidentes tráfico",
  "hospital de día accidentes tráfico",
  "consultas accidentes tráfico",
  "intervenciones de CMA accidentes tráfico",
  "urgencias accidentes tráfico",
  "hospitalización a domicilio accidentes tráfico",
  "altas otros",
  "estancias otros",
  "hospital de día otros",
  "consultas otros",
  "intervenciones de CMA otros",
  "urgencias otros",
  "hospitalización a domicilio otros",
  "total altas",
  "total estancias",
  "total hospital de día",
  "total consultas",
  "total intervenciones CMA",
  "total urgencias",
  "total hospitalización a domicilio",
  "compras",
  "gastos de personal",
  "total compras y gastos",
  "ingresos por prestación de servicios",
  "prestación de servicios particulares",
  "concertados con entidades aseguradoras privadas",
  "seguros de asistencia sanitaria y enfermedad",
  "accidentes de tráfico",
  "mutuas accidentes trabajo y emp colaboradoras",
  "concertados con el sistema nacional de salud",
  "financiación directa servicios de salud y otras entidades públicas",
  "financiación directa aseguradoras privadas y MATEPSS",
  "procedente de otras entidades públicas",
  "bonificaciones",
  "otros ingresos",
  "subvenciones,
 donaciones y legados",
  "total ventas e ingresos",
  "equipos alta tecnología hospital",
  "tecnología básica hospital",
  "personal médico",
  "personal auxiliar",
  "resto de personal",
  "porcentaje de altas por no defuncion",
  "tecnología total",
  "intensidad docente (MIR y EIR)",
  "grupo de hospital",
  "equipos de resonancia magnética hospital",
  "gammacamaras en hospital",
  "litotricia renal hospital ",
  "mamógrafos en hospital",
  "equipos de tomografia por emisión de fotones en hospital",
  "equipos de tac  hospital ",
  "salas de rayos x en funcionamiento hospital",
  "total personal médico jornada completa",
  "due,
 enfermeros total  jornada completa",
  "administrativos total  jornada completa",
  "porcentaje altas favorables",
  "tecnologia total ",
  "intensidad docente (MIR+EIR)",
  "Grupo (kmean)"
)
# Asignar etiquetas usando un bucle
for (i in seq_along(variable_labels)) {
  df_seleccion[[i]] <- set_label(df_seleccion[[i]], label = variable_labels[i])
}



save(df_seleccion,file="h:\\Mi unidad\\Tesis\\Datos con R\\SIAE 2010-2020\\df_seleccion.RData")

# Guardar el dataframe con las variables seleccionadas  en un archivo CSV
write.csv(df_seleccion, "df_seleccion.csv", row.names = FALSE)




  # Exportar las etiquetas a un archivo .txt
file_path <- "variables_etiquetas_seleccion.txt"
labels <- get_label(df_seleccion)
write.table(data.frame(Variable = names(labels), Etiqueta = labels), 
            file = file_path, col.names=FALSE, row.names = FALSE, sep = "\t", quote = FALSE)

 # cat("Archivo creado exitosamente en:", file_path, "\n")

cat("Archivo creado exitosamente en:", normalizePath(file_path), "\n")









