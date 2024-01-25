### Revision Analisis TFM - Alberto Coll Fernandez
# Definicion de graficas
# Comenzado: 24/01/2023
# Terminado 24/01/2023


## SETUP ----
library(tidyverse)
library(ggthemr)

# Directorio en laboratorio: C:/Users/Usuario/Documents/TFM-Ortiguilla
# Directorio en portatil: D:/collf/Documents/GitHub/TFM-Ortiguilla

setwd("C:/Users/Usuario/Documents/GitHub/TFM-Anemonia-sulcata")
ggthemr("fresh")

source(file = "./scripts_enero_2024/0_data_lab.R") # Laboratorio
#source(file = "./scripts_enero_2024/0_data_home.R") # En casa

## Definicion del tema y formato de las graficas ----
theme_tfm <- function(){
  theme(panel.background = element_rect(fill = "gray99"),
        axis.text = element_text(size = 12),
        plot.title = element_text(size = 18),
        strip.text.x = element_text(size = 15, face = "bold", vjust = 0),
        axis.title = element_text(size = 15),
        legend.position = "none")
        #strip.background = element_rect(colour = "black")
}

## Definicion de grafica de barras ----


# Este es el corazon del codigo que genera las graficas. Itera los nombres de las variables, y para cada una te da su media, desviacion estandar y error para usar en las graficas. Una vez tengas las funciones de las graficas hechas, se añaden al bucle. Es necesario ponerlo en otro script, en el de descriptiva o de analisis.

barras_tfm <- function(){
  ggplot() +
    geom_errorbar(data = tabla_summ, aes(x = tratamiento, ymax = media + error, ymin = media- error), width = 0.7, color = "gray55") +
    geom_col(data = tabla_summ, aes(x = tratamiento, y = media, fill = tratamiento, color = tratamiento),  alpha = 0.1, linewidth = 1) +
    geom_point(data = datos, aes(x = tratamiento, y = get(i), color = tratamiento), alpha = 0.8, size = 2) +
    geom_text(data = tabla_summ, aes(x = tratamiento, y = media + error, label = tukey), color = "grey5", vjust = -0.8, size = 4.5, fontface = "bold") +
    ylab(case_when(
      i == "clorofila.total" ~ "μg clorofila /g tejido",
      i == "proteina.tent" | i == "proteina.pie"  ~ " mg proteína / ml",
      i == "MDA.pie.2" | i == "MDA.tent.2" ~ "μM  MDA",
      i == "TEAC.pie" | i == "TEAC.tent"~ "μM equivalentes de Trolox",
      i == "GST.pie" | i == "GST.tent" ~ "mU / mg  de proteína",
      i == "DTD.pie" | i == "DTD.tent" ~ "mU / mg  de proteína",
      TRUE ~ "U / mg  de proteína")) +
    xlab("Tratamiento") +
    scale_color_manual(values = c("#0c8890", "#54B65D","#E56A1C", "#FBBC4C")) +
    ylim(c(0, 1.35*(max(tabla_summ$media) + max(tabla_summ$error)))) +
      # Colores 1
   # ylim(c(0, case_when(i == "CAT.pie" ~  90,i == "DTD.pie" ~  15,i == "GST.pie" ~  450,i == "MDA.pie.2"| i == "MDA.tent.2" ~  90,i == "SOD.pie"| i == "SOD.tent" ~  150,  i == "TEAC.pie"| i == "TEAC.tent" ~  500,i == "CAT.tent" | i == "DTD.tent" | i == "GST.tent" ~ max(tabla_summ$media) * 2, TRUE ~ max(tabla_summ$media) * 1.2))) +
    labs(title = i) +
    theme_tfm()
  
}


