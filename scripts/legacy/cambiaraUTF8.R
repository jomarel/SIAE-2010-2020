root <- "h:\\Mi unidad\\Tesis\\Datos con R\\SIAE 2010-2020"
subfolders <- list.dirs(root, recursive = FALSE)
files <- lapply(subfolders, function(x) list.files(x, pattern = "*.txt", full.names = TRUE))
files <- unlist(files)
for (file in files) {
  lines <- readLines(file)
  combined <- paste(lines, collapse = "\n")
  if (Encoding(combined) != "UTF-8") {
    combined <- iconv(combined, "latin1", "UTF-8")
    writeLines(combined, file)
  }
}

