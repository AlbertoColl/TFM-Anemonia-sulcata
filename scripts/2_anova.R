### TFM - Alberto Coll Fernandez
# Analisis estadistico, test post-Hoc y graficas
# Comenzado: 27/06/2023
# Terminado: 05/07/2023

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
datos$MDA.tent[5] # Bastante elevado para su grupo QUITAR
datos$CAT.tent[20] <- NA # Bastante elevado en general
datos$CAT.pie[8] <- NA # Desorbitado para el grupo y general
datos$CAT.pie[16] <- NA # Desorbitado para el grupo y general
datos$proteina.tent[20] # Valor de proteina muy bajo, igual cambia otras variables, QUITAR
datos$GST.tent[9] <- NA # outlier clarisimo
datos$GST.tent[10] # se desvia bastante del resto de medidas, QUITAR
datos$GST.pie[8] <- NA # outlier clarisimo
datos$clorofila.total[8] <- NA # Bastante desorbitado
datos$SOD.pie[20] <- NA # outlier, posiblemente afecta al analisis
datos$SOD.tent[16] <- NA #outlier, enmascara diferencias
datos$SOD.tent[5] <- NA #outlier, enmascara diferencias
datos$MDA.pie.2[19] <- NA #outlier, sale mucho error por su culpa



ggplot(datos) +
  geom_histogram(aes(x = TEAC.tent), bins = 16) +
  theme_tfm()

ggplot(datos) +
  geom_boxplot(aes(y = GST.pie, x = tratamiento, fill = tratamiento), alpha = 0.6) +
  #geom_jitter(aes(y = , x = tratamiento, color = tratamiento), width = 0.2) +
  theme_tfm() +
  ylab("GST.pie") +
  scale_fill_manual(values = c("#0c8890", "#3EB59B","#E56A1C",  "#FBBC4C")) +
  scale_color_manual(values = c("#0c8890", "#3EB59B","#E56A1C",  "#FBBC4C"))

### Ajuste modelo ----

# Nos quedamos con la primera o la segunda medida de MDA?
datos <- datos %>% select(-c(MDA.pie, MDA.tent))


# Ajustar modelo con lapply(). sapply() solia funcionar pero ya no no se por que
modelos <- lapply(colnames(datos[c(4:18)]), function(x){
  aov(formula = as.formula(paste0(x, " ~ tratamiento")), datos)})


# Falta: pruebas de normalidad de residuos y homocedasticidad
sapply(modelos, function(x){
  shapiro.test(residuals(x)) 
}) # Todo bien mientras se quiten esos outliers

sapply(modelos, function(x){
  print(leveneTest(x))
  
}) #  Las graficas por algun motivo no funcionan pero en test de levene esta to bien

for (i in c(1:15)) {
  print(colnames(datos[4:18][i]))
  print(summary(modelos[[i]]))
}

#Hay diferencias en SOD.tent, TEAC.pie, y proteina.tent. SOD pie y DTD pie marginalmente significativo.

### Test post-hoc y graficas ----

# Bucle de construccion de resumen y graficas
for (n in c(1:15)) {
  i <- colnames(datos[4:18])[[n]]
  tabla_summ <- datos %>%  group_by(tratamiento) %>% 
    summarise(media = mean(get(i), na.rm = T),
              desvest = sd(get(i), na.rm = T),
              error = desvest/sqrt(sum(!is.na(get(i)))))
  if ((summary(modelos[[n]])[[1]][["Pr(>F)"]][1]) <= 0.05) {
    tukey_loop <- TukeyHSD(modelos[[n]])
    cld.tukey <- multcompLetters4(modelos[[n]], tukey_loop, reversed = T)
    (letras <- rownames_to_column(as.data.frame(cld.tukey$tratamiento$Letters)))
    colnames(letras) <- c("tratamiento", "tukey")
    tabla_summ <- merge(tabla_summ, letras)
  } else {
    tabla_summ$tukey <- c("", "", "", "")
  }
  (p <- barras_tfm())
  ggsave(paste0("./resultados/graficas/", i, ".png"), width = 800, height = 1000, units = "px", #para clorofila 730, 730
         scale = 2, dpi = "retina")
}
