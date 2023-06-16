### TFM - Alberto Coll Fernandez
# Lectura y limpieza de datos
# Comenzado: 05/06/2023
# Terminado:

library(tidyverse)

# Importacion ----

setwd("D:/collf/Documents/GitHub/TFM-Ortiguilla")
datos <- read.csv2("./datos/TFM_datos.csv", numerals = "warn.loss", encoding = "latin1")%>% 
  mutate(tratamiento = as.factor(tratamiento))

# Reordenar tratamientos: Control, Oscuro, Salobre, IMTA
datos$tratamiento <- factor(datos$tratamiento, levels = c("Control", "Oscuro", "Salobre", "IMTA"))
summary(datos)