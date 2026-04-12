suppressPackageStartupMessages(library(sfaR))
mA_Q <- readRDS("G:/Mi unidad/SIAE 2010-2020/data_intermediate/sfa_modeloTotal_disenioA.rds")
cat("typeSfa:", mA_Q$typeSfa, "\n")

ef <- sfaR::efficiencies(mA_Q)
cat("class ef:", class(ef), "\n")
cat("names ef:", paste(names(ef), collapse=", "), "\n")
cat("nrow:", nrow(ef), "\n")
cat("rango efficiencies:", paste(round(range(ef$efficiencies, na.rm=TRUE), 4), collapse=" - "), "\n")
cat("media efficiencies:", round(mean(ef$efficiencies, na.rm=TRUE), 4), "\n")
cat("primeras 6:\n"); print(head(ef$efficiencies))

# Ver también qué más hay en el objeto ef
cat("\nResumen de columnas:\n")
print(summary(ef))
