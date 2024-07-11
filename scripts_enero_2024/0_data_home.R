### Revision analisis TFM - Alberto Coll Fernandez
# Lectura y limpieza de datos
# Comenzado: 24/01/2023
# Terminado 24/01/2023

library(tidyverse)
# Importacion ----

setwd("D:/collf/Documents/GitHub/TFM-Anemonia-sulcata")
datos <- read.csv2("./datos/datos_TFM completos.csv", numerals = "warn.loss", encoding = "latin1")%>% 
  mutate(tratamiento = as.factor(tratamiento))

# Traducir nombres de variables
datos$tratamiento <- factor(datos$tratamiento, levels = c("Control", "Oscuro", "Salobre", "IMTA"), labels = c("Control", "Shade", "Low_salinity", "IMTA"))

# Originalmente Shade y Low_salinity
# Cambiar a Limited_sunlight y Brackish_water? o a LS y BW?