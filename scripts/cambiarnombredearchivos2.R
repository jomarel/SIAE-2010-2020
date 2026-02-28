# Set the root folder
root_folder <- "h:\\Mi unidad\\Tesis\\Datos con R\\SIAE 2010-2020\\"

# Set a list of names for your files
file_names <- c("01_Filiacion","02_Ofertaasistencial","03_Dotacionhospital",
                "04_Dotaciontecnologica","05_Personal_I","06_Personal_II","07_Formacion","08_ServiciosCentrales","09_Actividadhospital","10_Actividadquirurgica","11_Actividadobstetrica","12_HospitaldeDia","13_Hospitalizacionadomicilio","14_Actividadconsultas","15_Actividadurgencias","16_Actividaddiagnostica","17_Restoactividad","18_Regimeneconomico","19_Gastos","20_Ingresos","21_Inversiones" )

# Use a for loop to iterate through the subfolders of your root folder
for (year in 2010:2020) {
  
  # Build the full path to the current year's subfolder
  year_folder <- paste0(root_folder, year)
  
  # Get a list of the files in the current year's subfolder
  file_list <- list.files(path = year_folder)
  
  # Use a for loop to iterate through the list of files in the current year's subfolder
  for (i in 1:length(file_list)) {

    
    
    
        
    # Open the file in read-only mode
    f <- file(paste0(year_folder, "\\", file_list[i]), "rb")
    
    # Read the file as a character vector
    lines <- readLines(f)
    
    # Close the file
    close(f)
    
    # Replace á with a
    lines <- gsub("á", "a", lines)
    # Replace é with e
    lines <- gsub("é", "e", lines)
    # Replace í with i
    lines <- gsub("í", "i", lines)
    # Replace ó with o
    lines <- gsub("ó", "o", lines)
    # Replace ú with u
    lines <- gsub("ú", "u", lines)
    # Replace ñ with n
    lines <- gsub("año", "year", lines)
    lines <- gsub("Año", "year", lines)
    
    # Open a new file in write mode
    f <- file(paste0(year_folder, "\\", file_names[i], ".txt"), "w")
    
    # Write the modified lines to the new file
    writeLines(lines, f)
    
    # Close the file
    close(f)
  }
  
}
