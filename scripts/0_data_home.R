### TFM - Alberto Coll Fernandez
# Lectura y limpieza de datos
# Comenzado: 05/06/2023
# Terminado: 22/06/23

library(tidyverse)

setwd("D:/collf/Documents/GitHub/TFM-Ortiguilla")
datos <- read.csv2("./datos/TFM_datos.csv", numerals = "warn.loss", encoding = "latin1")%>% 
  mutate(tratamiento = as.factor(tratamiento))

# Reordenar tratamientos: Control, Oscuro, Salobre, IMTA
datos$tratamiento <- factor(datos$tratamiento, levels = c("Control", "Oscuro", "Salobre", "IMTA")) # no cambiar al final
summary(datos)
