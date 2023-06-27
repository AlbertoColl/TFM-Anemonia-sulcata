### TFM - Alberto Coll Fernandez
# Analisis estadistico: ANOVA
# Comenzado: 27/06/2023
# Terminado:

### SETUP ----
library(tidyverse)
library(car)
library(multcompView)

setwd("D:/collf/Documents/GitHub/TFM-Ortiguilla")
#source(file = "./scripts/0_data_lab.R") # Laboratorio
source(file = "./scripts/0_data_home.R") # En casa

source(file = "./scripts/1_funciones_graficas.R")
ggthemr("fresh")

### Exploracion y outliers ----

# Outliers potenciales:
datos$MDA.tent[17] <- NA # Es claramente un error de medida
datos$MDA.tent[5] # Bastante elevado para su grupo
datos$CAT.tent[20] <- NA # Bastante elevado en general
datos$CAT.pie[8] <- NA # Desorbitado para el grupo y general
datos$CAT.pie[16] <- NA # Desorbitado para el grupo y general
datos$proteina.tent[20] # Valor de proteina muy bajo, igual cambia otras variables
datos$GST.tent[9] <- NA # outlier clarisimo
datos$GST.tent[10] # se desvia bastante del resto de medidas
datos$GST.pie[8] <- NA # outlier clarisimo
datos$clorofila.total[8] <- NA # Bastante desorbitado





ggplot(datos) +
  geom_histogram(aes(x = MDA.tent), bins = 16) +
  theme_tfm()

ggplot(datos) +
  geom_boxplot(aes(y = clorofila.total, x = tratamiento, fill = tratamiento), alpha = 0.6) +
  #geom_jitter(aes(y = , x = tratamiento, color = tratamiento), width = 0.2) +
  theme_tfm() +
  ylab("clorofila") +
  scale_fill_manual(values = c("#0c8890", "#3EB59B", "#FBBC4C", "#E56A1C")) +
  scale_color_manual(values = c("#0c8890", "#3EB59B", "#FBBC4C", "#E56A1C"))

### Ajuste modelo ----

datos <- datos %>% select(n, muestra, tratamiento, peso.total, clorofila.total, CAT.pie, CAT.tent, GST.pie, GST.tent, MDA.pie, MDA.tent, TEAC.pie, TEAC.tent, proteina.pie, proteina.tent)#QUITAR ESTO CUANDO TENGAS TODO


# Crear lista de formulas con sapply()
formulas <- sapply(colnames(datos[c(5:23)]), function(x){as.formula(paste0(x, " ~ tratamiento"))})

modelos <- sapply(colnames(datos[c(5:15)]), function(x){
  aov(formula = as.formula(paste0(x," ~ tratamiento")), datos)})

# Falta: pruebas de normalidad de residuos y homocedasticidad
sapply(modelos, function(x){
  shapiro.test(residuals(x)) 
}) # Todo bien por ahora

sapply(modelos, function(x){
  print(leveneTest(x))
  
}) #  Las graficas por algun motivo no funcionan pero en test de levene esta to bien


### Test de Tukey ----
### Graficas ----

# Bucle de construccion de resumen
for (n in c(4:23)) {
  i <- colnames(datos[4:23])[[n]]
  tabla_summ <- datos %>%  group_by(tratamiento) %>% 
    summarise(media = mean(get(i), na.rm = T),
              desvest = sd(get(i), na.rm = T),
              error = desvest/sqrt(sum(!is.na(get(i)))))
}