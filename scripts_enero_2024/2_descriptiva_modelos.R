### Revision analisis TFM - Alberto Coll Fernandez
# Descritiva y modelos
# Comenzado: 24/01/2023
# Terminado 24/01/2023

library(tidyverse)
library(car)
library(multcompView)

### Setup----

setwd("C:/Users/Usuario/Documents/GitHub/TFM-Anemonia-sulcata")
source(file = "./scripts_enero_2024/0_data_lab.R") # Laboratorio
#source(file = "./scripts/0_data_home.R") # En casa

source(file = "./scripts_enero_2024/1_funciones_graficas.R")

### Análisis exploratorio de datos ----

# Comprobacion de normalidad de las variables respuestas GLOBALMENTE

hist(datos$SOD.tent)
scale(datos$SOD.pie)
shapiro.test(datos$SOD.pie)

# SOD mas menos bien en el pie y peor en el tentaculo. Shapiro no significativo
# CAT pie tiene algunos valores extremos, Shapiro salta por poco. Tent igual pero no muy sig, Aceptable creo
# GST no es nada normal. Nada nada. transformar? Transformacion arregla en pie, tent no tanto. Comprobar por grupos
# DTD perfecta que bonica
# G6PDH da problemas en tentaculo, pie no. Comprobar por grupos
# MDA da problemas en pie, ver por grupos
# teac perfecto

# Comprobando normalidad por grupos

hist(filter(datos, tratamiento == "Shade")$SOD.pie)
shapiro.test(filter(datos, tratamiento == "Low_salinity")$SOD.pie)

# GST en pie sale un poco rara en el tratamiento sombreado. Hay un valor que es 715 de repente. Yo probaria a quitarlo
# G6PDH, MDA y CAT estan bien por grupos

### Estudio de datos anómalos e influyentes ---- 


ggplot(datos, aes(x = tratamiento, y = SOD.pie, color = tratamiento)) +
  #geom_boxplot(alpha = 0) +
  geom_point(position = position_jitter(height = 0, width = 0.1), size = 2)

# Hay un punto en 20,6, IMTA SOD pie, que esta bstante alejado del resto de su grupo
# [8,8] en catalasa pie, grupo shade esta muy muy desviado. Igual es influyente.
# En GST pie hay otro punto raro de 200 y lgo pero ya he quitado el de 700 que se desviaba mucho mas.
# [9,11] esta muy por encima, quitarlo. Es Shade en GST tent.

# datos[20,6] <- NA # NO HACE FALTA QUITARLO
#datos$clorofila.total[8] <- NA # POTENCIAL OUTLIER EN CLOROFILA
datos$GST.pie[8] <- NA # potencial outlier en GST pie, rompe normalidad
datos$CAT.pie[8] <- NA # potencial outlier en CAT pie, rompe normalidad
datos$GST.tent[9] <- NA # potencial outlier en GST tent, rompe normalidad
datos$SOD.pie[8] <- NA # POTENCIAL OUTLIER, rompe normalidad

datos <- datos %>% select(-c(MDA.pie, MDA.tent)) # Primera medida de MDA no vale

### Ajuste de modelos ----

modelos <- lapply(colnames(datos[c(4:21)]), function(x){
  aov(formula = as.formula(paste0(x, " ~ tratamiento")), datos)})

# NORMALIDAD DE RESIDUOS
sapply(modelos, function(x){
  print(x$terms[[2]])
  shapiro.test(residuals(x))}) 
# peso.total es unica variable que viola esta asuncion, no esta muy normalmente distribuida

# HOMOCEDASTICIDAD
sapply(modelos, function(x){
  print(leveneTest(x))})
# todas las variables cumplen la asuncion de homocedasticidad

# RESULTADOS DEL ANOVA

for (i in c(1:18)) {
  print(colnames(datos[4:21][i]))
  print(summary(modelos[[i]]))}


### Elaboración de gráficas y test post-hoc ----

# Bucle construccion de estadisticos de resumen, grafica y test post hoc Tukey
for (n in c(1:18)) {
  i <- colnames(datos[4:21])[[n]]
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
  saveRDS(p, paste0("./resultados/graficas2/", i, "_RDS"))
  ggsave(paste0("./resultados/graficas2/", i, ".png"), width = 90, height = 112.5, units = "mm", dpi = 1000)
}


### Patchwork para figuras finales ----

# Problema: no salen los puntos de datos
# Solo se conservan en 3 graficas por algun motivo que no entiendo.
library(patchwork)
plots <- list()
for (n in c(1:18)){
  i <- colnames(datos[4:21])[[n]]
  print(c(i,n))
  plots[[i]] <- readRDS(paste0("./resultados/graficas2/", i, "_RDS"))
  
}

plots <- lapply(colnames(datos[4:21]), function(x){readRDS(paste0("./resultados/graficas2/", x, "_RDS"))})

wrap_plots(plots[3:11])
wrap_plots(plots[12:18])

