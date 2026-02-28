Estoy tratando de hacer una modelización mediante técnicas de frontera estocástica con datos de panel la eficiencia de los hospitales públicos españoles utilizando la base de datos SIAE disponible online. Quiero usar un modelo con variables explicativas del término de ineficiencia (u) que me permitan contrastar algunas hipótesis de un modelo teórico en el que la estructura organizativa del hospital (público o privado/concertado) así como el peso que suponen  las altas financiadas mediante pago directo por parte de los pacientes (out-of-pocket) sobre el total de altas, o por ejemplo, si el centro es concertado o no. Además, quiero contrastar que la estructura organizativa o el peso de los pagos directos afectan al término de eficiencia de manera distinta en la dimensión de cantidad de servicios proporcionados por el centro (número de altas, por ejemplo) frente a una dimensión de intensidad de servicios proporcionados por paciente (por ejemplo mediante una variable que agregue distintas pruebas diagnósticas o de tratamiento de mayor intensidad en términos per capita o por paciente) ESta varialbe de intensidad de diagnóstico/tratamiento sería una proxy de la "calidad" de los servicios. Mi modelo teórico predice que en centros privados o con mayor peso de pagos directos por parte de los pacientes, los hospitales son más eficientes en cantidad pero menos en intensidad mientras que en centros públicos o con menos peso de altas financiadas diractemente por pacientes o por compañías privadas de seguros, el término de ineficiencia es mayor en la dimensión cuantitativa (creando listas de espera) pero menor en términos de "calidad" tengo en mente el modelo basttese coelli de frontera estocástica con datos de panel y variables explicativas en el término de error. La implementación empírica no es fácil. Mi base de datos incluye la naturaleza de cada hospital (público, privado) si es concertado, altas financiadas por el sector público o poer el sector privado (compañías de seguro o out of pocket) así como datos de inputs hospitalarios (personal, camas, tecnología,...) y de altas hospitalarias (por curación, por fallecimiento, reingresos,...) Además, no cuento en mi base de datos con una variable de complejidad de casos para cada centro (case-mix) que tendría que tener en cuenta, por ejemplo creando una variable de complejidad usando otras variables de la base (como tamaño, tecnología, tipo de intervenciones realizadas...). De hecho, sí que cuento con el dato de case mix en forma de peso GRD-AP para un año concreto y para hospitales públicos. Quizás pueda usar eso para construir mi proxi de case-mix para todos los centros.  Con toda esta información qeu te he dado, ¿puedes proponerme un plan de trabajo en mi investigación, qué modelo econométrico me recomiendas usar, cómo debería manejar mi base de datos SIAE para realizar dicha estimación de mis hipótesis?
















Tengo una base de datos en un archivo mdb (SIAE_2016anonimizada.mdb en el siguiente directorio h:\Mi unidad\Tesis\Datos con R\Raw\  Quiero usar una rutina de r para importar todas las tablas de datos hospitalarios (21 tablas que empiezan por el c?digo C1). Quiero que R lea cada tabla y la exporte como un archivo con extensi?n txt, cuya primera fila sean los encabezados y cada columna est? separada por ; Quiero que cada tabla en formato txt la grabe en la subcarpeta h:\Mi unidad\Tesis\Datos con R\Raw\2016\

                                           
                                           
Tengo una base de datos de hospitales en Espanya (Sistema de informacion de atencion especializada) que cubre los anyos 2010 a 2020. Para cada anyo tengo 21 archivos de texto separados por ; y con encabezado. Querria primero unir todos los archivos de cada anyo en un solo data frame y luego unir todos los data frame en uno solo para obtener una base en datos de panel donde el codigo de hospital lo da la variable NCODI y el codigo de anyo lo da la variable AÃ±o. Mi directorio raiz es h:\Mi unidad\Tesis\Datos con R\SIAE 2010-2020\,  dentro de esta carpeta hay un subdirectorio para cada aÃ±o que lleva de nombre el numero del aÃ±o (por ejemplo para el aÃ±o 2010: h:\Mi unidad\Tesis\Datos con R\SIAE 2010-2019\2010).  Los nombres de los 21 archivos de datos de cada aÃ±o son los siguientes: "01_Filiacion.txt","02_Ofertaasistencial.txt","03_Dotacionhospital.txt","04_Dotaciontecnologica.txt","05_Personal_I.txt","06_Personal_II.txt","07_Formacion.txt","08_ServiciosCentrales.txt","09_Actividadhospital.txt","10_Actividadquirurgica.txt","11_Actividadobstetrica.txt","12_HospitaldeDia.txt","13_Hospitalizacionadomicilio.txt","14_Actividadconsultas.txt","15_Actividadurgencias.txt","16_Actividaddiagnostica.txt","17_Restoactividad.txt","18_Regimeneconomico.txt","19_Gastos.txt","20_Ingresos.txt" y "21_Inversiones.txt". Quiero unir los 21 archivos para cada aÃ±o utilizando la variable NCODI en la union, y guardar los data frame creados en la carpeta raiz. Quiero un codigo en R que me permita automatizar esta operacion de union de archivos y creacion de data frame asi como el data frame final en formato de panel que una a todos los aÃ±os. Ademas creo que seria bueno usar el paquete dplyr y la funcion merge o alguno similar que permita trabajar con bases de datos grandes. Cada archivo solo tiene en comun dos variables con el resto: NCODI (el codigo que identifica anonimamente a cada hospital) y AÃ±o (o aÃ±o) que nos dice el aÃ±o al que pertenece la informacion de esa base de datos.                                           
                                           
                                           

Mi directorio raiz es h:\Mi unidad\Tesis\Datos con R\SIAE 2010-2020\,  dentro de esta carpeta hay un subdirectorio para cada a?o que lleva de nombre el numero del a?o (por ejemplo: h:\Mi unidad\Tesis\Datos con R\SIAE 2010-2019\2010). En total hay 10 subdirectorioes que van de 2010 a 2019. Dentro de cada subdirectorio de a?o hay 21 archivos de texto a los que les quiero cambiar el nombre actual por la siguiente lista de nombres: "01_Filiacion.txt","02_Ofertaasistencial.txt","03_Dotacionhospital.txt",
"04_Dotaciontecnologica.txt","05_Personal_I.txt","06_Personal_II.txt","07_Formacion.txt",
"08_ServiciosCentrales.txt","09_Actividadhospital.txt","10_Actividadquirurgica.txt",
"11_Actividadobstetrica.txt","12_HospitaldeDia.txt","13_Hospitalizacionadomicilio.txt",
"14_Actividadconsultas.txt","15_Actividadurgencias.txt","16_Actividaddiagnostica.txt",
"17_Restoactividad.txt","18_Regimeneconomico.txt","19_Gastos.txt","20_Ingresos.txt",
"21_Inversiones.txt" 
El nombre de cada archivo en las carpetas es distinto as? que primero tiene que leer los nombres de esos archivos en orden y luego sustituirlos por los nuevos nombres

file_names <- c("01_Filiacion","02_Ofertaasistencial","03_Dotacionhospital",
                "04_Dotaciontecnologica","05_Personal_I","06_Personal_II","07_Formacion","08_ServiciosCentrales","09_Actividadhospital","10_Actividadquirurgica","11_Actividadobstetrica","12_HospitaldeDia","13_Hospitalizacionadomicilio","14_Actividadconsultas","15_Actividadurgencias","16_Actividaddiagnostica","17_Restoactividad","18_Regimeneconomico","19_Gastos","20_Ingresos","21_Inversiones" )


Mi directorio raiz es h:\Mi unidad\Tesis\Datos con R\SIAE 2010-2019\,  dentro de esta carpeta hay un subdirectorio para cada a?o que lleva de nombre el numero del a?o (por ejemplo: h:\Mi unidad\Tesis\Datos con R\SIAE 2010-2019\2010). En total hay 10 subdirectorioes que van de 2010 a 2019. Dentro de cada subdirectorio de a?o hay 21 archivos de texto con los siguientes nombres "01_Filiacion.txt","02_Ofertaasistencial.txt","03_Dotacionhospital.txt",
"04_Dotaciontecnologica.txt","05_Personal_I.txt","06_Personal_II.txt","07_Formacion.txt",
"08_ServiciosCentrales.txt","09_Actividadhospital.txt","10_Actividadquirurgica.txt",
"11_Actividadobstetrica.txt","12_HospitaldeDia.txt","13_Hospitalizacionadomicilio.txt",
"14_Actividadconsultas.txt","15_Actividadurgencias.txt","16_Actividaddiagnostica.txt",
"17_Restoactividad.txt","18_Regimeneconomico.txt","19_Gastos.txt","20_Ingresos.txt",
"21_Inversiones.txt" 


Quiero  programar una rutina en r  que  una los datos de esos 21 archivos para cada a?o usando la variable NCODI (c?digo de hospital). Despu?s quiero unir los datos de cada a?o en un solo data frame De esta forma me queda una gran base de datos en panel para todos los hospitales entre 2010 y 2019. 



I have a root directory (h:\Mi unidad\Tesis\Datos con R\SIAE 2010-2020) and inside there are 11 subfolders each with a year name from 2010 to 2020. Inside each of thes folders there are 21 txt files. In every folder the 21 files have the same name (this is the list of names: "01_Filiacion.txt","02_Ofertaasistencial.txt","03_Dotacionhospital.txt","04_Dotaciontecnologica.txt","05_Personal_I.txt","06_Personal_II.txt","07_Formacion.txt","08_ServiciosCentrales.txt","09_Actividadhospital.txt","10_Actividadquirurgica.txt","11_Actividadobstetrica.txt","12_HospitaldeDia.txt","13_Hospitalizacionadomicilio.txt","14_Actividadconsultas.txt","15_Actividadurgencias.txt","16_Actividaddiagnostica.txt","17_Restoactividad.txt","18_Regimeneconomico.txt","19_Gastos.txt","20_Ingresos.txt" y "21_Inversiones.txt")

I want to merge the 21 files into one data frame and save it in the root directory with the name df_'number of the year'. The data in each file is separated by ; and they have in common the variable NCODI so the merging should be done using this variable
in the root director there are no files. All the files are inside the sub-folders (one by year)


Tengo una base de datos de hospitales españoles ("h:/Mi unidad/Tesis/Datos con R/SIAE 2010-2020/df_final.csv") con datos de hospitales españoles. Cada hospital se identifica con la variable  "NCODI" y la variable "anyo" identifica el año. Tengo también otro archivo con tres variables "NCODI", "anyo" y "peso" ("h:/Mi unidad/Tesis/Datos con R/SIAE 2010-2020/pesos.txt")
Quiero unir ambos archivos en R usando la variable NCODI y anyo como variables de enlace. En ambos archivos las variables van separadas por ; y la posición decimal se marca con ,  En el archivo pesos.txt hay datos faltantes de NCODI y de anyo








Tengo un data frame llamado h:\Mi unidad\Tesis\Datos con R\SIAE 2010-2020\df_final.csv que tiene 279 variables de varios hospitales a lo largo de varios aÃ±os. una de las variables es la variable aÃ±o y otra la variable NCODI que identifica a cada hospital. Otra es la variable peso pero esta variable solo la tengo disponible para algunos hospitales algunos aÃ±os. Quiero usar R para construir una nueva variable para todos los hospitales en todos los aÃ±os que aproxime lo mejor posible el valor de la variable peso. Como puedo hacerlo sabiendo que el resto de variables disponibles algunas son numÃ©ricas y otras son factores, ademÃ¡s hay mucha variables... unas 276
