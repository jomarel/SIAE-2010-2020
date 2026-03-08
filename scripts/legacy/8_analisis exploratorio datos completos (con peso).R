# 9.1. Comparar la Distribución de 'peso' Original y Predicho

# Cargar ggplot2 si no está cargado
if (!require(ggplot2)) install.packages("ggplot2")
library(ggplot2)

# Crear un histograma para cada tipo de peso
ggplot(data_completo, aes(x = peso, fill = tipo_peso)) +
  geom_histogram(alpha = 0.5, position = "identity", bins = 30) +
  labs(title = "Distribución de 'peso' Original y Predicho",
       x = "Peso",
       y = "Frecuencia") +
  theme_minimal()

ggplot(data_completo, aes(x = peso, color = tipo_peso)) +
  geom_density(size = 1.2) +
  labs(title = "Densidad de 'peso' Original y Predicho",
       x = "Peso",
       y = "Densidad") +
  theme_minimal()

ggplot(data_completo, aes(x = tipo_peso, y = peso, fill = tipo_peso)) +
  geom_boxplot(alpha = 0.7) +
  labs(title = "Boxplot de 'peso' Original y Predicho",
       x = "Tipo de Peso",
       y = "Peso") +
  theme_minimal()

