### Revision analisis TFM - Alberto Coll Fernandez
# Lectura y limpieza de datos
# Comenzado: 24/01/2023
# Terminado 24/01/2023

library(tidyverse)

# Importacion ----

setwd("C:/Users/Usuario/Documents/GitHub/TFM-Anemonia-sulcata")
datos <- read.csv2("./datos/datos_TFM completos.csv", numerals = "warn.loss", encoding = "latin1")%>% 
  mutate(tratamiento = as.factor(tratamiento))

summary(datos)

# Traducir nombres de variables
datos$tratamiento <- factor(datos$tratamiento, levels = c("Control", "Oscuro", "Salobre", "IMTA"), labels = c("Control", "Shade", "Low_salinity", "IMTA"))
