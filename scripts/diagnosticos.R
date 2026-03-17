# Primero localiza el archivo exacto
list.files("data_raw/2010", full.names = TRUE)

# O busca en todos los años a la vez
list.files("data_raw", pattern = "filiacion", 
           recursive = TRUE, full.names = TRUE)

# Ver también en data_intermediate por si ya está procesado
list.files("data_intermediate", pattern = "filiacion",
           recursive = TRUE, full.names = TRUE)