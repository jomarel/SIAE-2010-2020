#rutina para convertir todos los archivos a UTF-8 y quitar acentos y ñ
setwd("h:\\Mi unidad\\Tesis\\Datos con R\\SIAE 2010-2020\\")
for (year in 2010:2021) {
  subdirectory <- paste0(year)
  setwd(subdirectory)
  files <- list.files(pattern = "\\.txt$")
  for (file in files) {
    lines <- readLines(file)
    lines <- gsub("Á", "A", lines)
    lines <- gsub("É", "E", lines)
    lines <- gsub("Í", "I", lines)
    lines <- gsub("Ó", "O", lines)
    lines <- gsub("Ú", "U", lines)
    lines <- gsub("á", "a", lines)
    lines <- gsub("é", "e", lines)
    lines <- gsub("í", "i", lines)
    lines <- gsub("ó", "o", lines)
    lines <- gsub("ú", "u", lines)
    lines <- gsub("Ñ", "NY", lines)
    lines <- gsub("ñ", "ny", lines)
    lines <- gsub("ANDALUCÃA", "ANDALUCIA",lines)
    lines <- gsub("CASTILLA Y LEÃN", "CASTILLA Y LEON",lines)
    lines <- gsub("CATALUÃA", "CATALUNYA",lines)
    lines <- gsub("PAÃS VASCO", "PAIS VASCO",lines)
    lines <- gsub("ARAGÃN", "ARAGON",lines)
    lines <- gsub("REGIÃN DE MURCIA", "REGION DE MURCIA",lines)
    

    
    
    
    
    
    
    writeLines(lines, file, useBytes = TRUE)
  }
  setwd("..")
}
