# Primero, establece la ruta a tu carpeta raíz
root_folder <- "h:\\Mi unidad\\Tesis\\Datos con R\\SIAE 2010-2020\\"

# Establece una lista de nombres para tus archivos
file_names <- c("01_Filiacion", "02_Ofertaasistencial", "03_Dotacionhospital",
                "04_Dotaciontecnologica", "05_Personal_I", "06_Personal_II", "07_Formacion", "08_ServiciosCentrales",
                "09_Actividadhospital", "10_Actividadquirurgica", "11_Actividadobstetrica", "12_HospitaldeDia",
                "13_Hospitalizacionadomicilio", "14_Actividadconsultas", "15_Actividadurgencias",
                "16_Actividaddiagnostica", "17_Restoactividad", "18_Regimeneconomico", "19_Gastos", "20_Ingresos",
                "21_Inversiones")

# Use un bucle for para iterar a través de las subcarpetas de tu carpeta raíz
for (year in 2010:2021) {
  # Construye la ruta completa a la subcarpeta del año actual
  year_folder <- paste0(root_folder, year)
  
  # Obtén una lista de los archivos en la subcarpeta del año actual
  file_list <- list.files(path = year_folder)
  
  # Use un bucle for para iterar a través de la lista de archivos en la subcarpeta del año actual
  for (i in 1:length(file_list)) {
    # Construye la ruta completa al archivo actual
    file_path <- file.path(year_folder, file_list[i])
    
    # Convierte el nombre del archivo a un encoding común que admita acentos
    new_file_name <- iconv(file_names[i], to = "ASCII//TRANSLIT")
    
    # Cambia el nombre del archivo
    file.rename(from = file_path, to = file.path(year_folder, paste0(new_file_name, ".txt")))
  }
}
