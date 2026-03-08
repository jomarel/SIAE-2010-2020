install.packages("reshape2")
{library(reshape2)
library(dplyr)
library(tidyverse)}


df_2010 <- read.csv("h:/Mi unidad/Tesis/Datos con R/SIAE 2010-2020/df_2010.csv")
df_2011 <- read.csv("h:/Mi unidad/Tesis/Datos con R/SIAE 2010-2020/df_2011.csv")
df_2012 <- read.csv("h:/Mi unidad/Tesis/Datos con R/SIAE 2010-2020/df_2012.csv")
df_2013 <- read.csv("h:/Mi unidad/Tesis/Datos con R/SIAE 2010-2020/df_2013.csv")
df_2014 <- read.csv("h:/Mi unidad/Tesis/Datos con R/SIAE 2010-2020/df_2014.csv")
df_2015 <- read.csv("h:/Mi unidad/Tesis/Datos con R/SIAE 2010-2020/df_2015.csv")
df_2016 <- read.csv("h:/Mi unidad/Tesis/Datos con R/SIAE 2010-2020/df_2016.csv")
df_2017 <- read.csv("h:/Mi unidad/Tesis/Datos con R/SIAE 2010-2020/df_2017.csv")
df_2018 <- read.csv("h:/Mi unidad/Tesis/Datos con R/SIAE 2010-2020/df_2018.csv")
df_2019 <- read.csv("h:/Mi unidad/Tesis/Datos con R/SIAE 2010-2020/df_2019.csv")
df_2020 <- read.csv("h:/Mi unidad/Tesis/Datos con R/SIAE 2010-2020/df_2020.csv")

df_wide <- rbind(df_2010, df_2011, df_2012, df_2013, df_2014, df_2015,
                  df_2016, df_2017, df_2018, df_2019, df_2020)

pesos <- read.csv("h:\\Mi unidad\\Tesis\\Datos con R\\SIAE 2010-2020\\pesos.txt", sep=";",dec=",")
pesos$NCODI <- as.integer(pesos$NCODI)

df_final <- left_join(df_wide, pesos, by=c("año", "NCODI"))%>%
          select(año, NCODI, peso, Depend_agrupada, everything() )

file <- "h:/Mi unidad/Tesis/Datos con R/SIAE 2010-2020/df_final.csv"
write_delim(df_final, file, delim = ";", quote = "all")



df_long <- melt(df_final, id.vars = c("año", "NCODI"))


file <- "h:/Mi unidad/Tesis/Datos con R/SIAE 2010-2020/df_long.csv"
write_delim(df_long, file, delim = ";", quote = "all")
