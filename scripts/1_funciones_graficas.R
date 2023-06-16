### TFM - Alberto Coll Fernandez
# Definicion de graficas
# Comenzado: 15/06/2023
# Terminado:


## SETUP ----
library(tidyverse)
library(ggthemr)

# Directorio en laboratorio: C:/Users/Usuario/Documents/TFM-Ortiguilla
# Directorio en portatil: D:/collf/Documents/GitHub/TFM-Ortiguilla

setwd("D:/collf/Documents/GitHub/TFM-Ortiguilla")
ggthemr("fresh")

#source(file = "./scripts/0_data_lab.R") # Laboratorio
source(file = "./scripts/0_data_home.R") # En casa

## TEMA DE GRAFICAS ----
theme_tfm <- function(){
  theme(panel.background = element_rect(fill = "gray99"),
        axis.text = element_text(size = 10),
        plot.title = element_text(size = 18),
        strip.text.x = element_text(size = 15, face = "bold", vjust = 0),
        axis.title = element_text(size = 14),
        legend.position = "none")
        #strip.background = element_rect(colour = "black")
}

## Grafica de barras ----


# Este es el corazon del codigo que genera las graficas. Itera los nombres de las variables, y para cada una te da su media, desviacion estandar y error para usar en las graficas. Una vez tengas las funciones de las graficas hechas, se añaden al bucle. Es necesario ponerlo en otro script, en el de descriptiva o de analisis.

for (n in c(4:23)) {
  i <- colnames(datos[4:23])[[n]]
  tabla_summ <- datos %>%  group_by(tratamiento) %>% 
    summarise(media = mean(get(i), na.rm = T),
              desvest = sd(get(i), na.rm = T),
              error = desvest/sqrt(sum(!is.na(get(i)))))
}

barras_tfm <- function(){
  ggplot(tabla_summ) +
    geom_errorbar(aes(x = tratamiento, ymax = media + error, ymin = media- error), width = 0.7, color = "gray55") +
    geom_col(aes(x = tratamiento, y = media, fill = tratamiento)) +
    ylab(case_when(
      i == "clorofila.total" ~ "μg clorofila /g tejido",
      i == "proteina_t" | i == "proteina_p"  ~ "protein mg / ml",
      i == "MDA_t" | i == "MDA_p" ~ "MDA μM ",
      i == "TEAC_t" | i == "TEAC_p"~ "Trolox equivalent μM",
      i == "GR_t" | i == "GR_p" ~ "mU / mg  of protein",
      i == "GST_t" | i == "GST_p" ~ "mU / mg  of protein",
      i == "DTD_t" | i == "DTD_p" ~ "mU / mg  of protein",
      TRUE ~ "U / mg  of protein")) +
    xlab("Tratamiento") +
    scale_fill_manual(values = c("#0c8890", "#3EB59B", "#FBBC4C", "#E56A1C")) + # Colores 1
    #scale_fill_manual(values = c("#414066", "#69B4AB", "#FBBC4C", "#EF476F")) + # Colores 2
    #scale_fill_manual(values = c("#1E5D56", "#0C8890", "#3EB59B", "#FBBC4C")) + # Colores 3
    theme_tfm()
  
}


