# Read in the file as a character vector
lines <- readLines("h:/Mi unidad/Tesis/Datos con R/SIAE 2010-2020/2010/C1_01_FILIACION.txt")

# Loop through each line in the character vector
for (i in 1:length(lines)) {
  # Replace á with a
  lines[i] <- gsub("á", "a", lines[i])
  # Replace é with e
  lines[i] <- gsub("é", "e", lines[i])
  # Replace í with i
  lines[i] <- gsub("í", "i", lines[i])
  # Replace ó with o
  lines[i] <- gsub("ó", "o", lines[i])
  # Replace ú with u
  lines[i] <- gsub("ú", "u", lines[i])
  # Replace ñ with n
  lines[i] <- gsub("ñ", "n", lines[i])
}

# Write the modified lines to a new file
writeLines(lines, "h:/Mi unidad/Tesis/Datos con R/SIAE 2010-2020/2010/modified_file.txt")



# Read in the first file
data1 <- read.table("h:/Mi unidad/Tesis/Datos con R/SIAE 2010-2020/2010/modified_file.txt", sep = ";", header = TRUE)

# Read in the second file
data2 <- read.table("h:/Mi unidad/Tesis/Datos con R/SIAE 2010-2020/2010/file2.txt")

# Merge the two data frames by NCODI
merged_data <- merge(data1, data2, by = "NCODI")








full.names = TRUE, recursive = FALSE)

# Recorre la lista de subdirectorios
for (directorio in directorios) {
  # Crea una lista con los nombres de todos los archivos en el subdirectorio
  archivos <- list.files(path = directorio)
  
  # Recorre la lista de archivos y cambia su codificación a UTF-8
  for (archivo in archivos) {
    # Copia el archivo con su nombre original y con codificación en UTF-8
    file.copy(archivo, file.path(dirname(archivo), iconv(basename(archivo), from = "ANSI", to = "UTF-8")), overwrite = TRUE)
  }
}



























