### Revision analisis TFM - Alberto Coll Fernandez
# Descritiva y modelos
# Comenzado: 24/01/2023
# Terminado 24/01/2023

library(tidyverse)
library(car)
library(multcompView)
library(patchwork)
library(rstatix)
library(ggpubr)


### Setup----

# Directorio en laboratorio: C:/Users/Usuario/Documents/GitHub/TFM-Anemonia-sulcata
# Directorio en portatil: D:/collf/Documents/GitHub/TFM-Anemonia-sulcata

setwd("D:/collf/Documents/GitHub/TFM-Anemonia-sulcata")
#source(file = "./scripts_enero_2024/0_data_lab.R") # Laboratorio
source(file = "./scripts_enero_2024/0_data_home.R") # En casa

source(file = "./scripts_enero_2024/1_funciones_graficas.R")

### Análisis exploratorio de datos ----

# Comprobacion de normalidad de las variables respuestas GLOBALMENTE

hist(datos$GR.pie)
scale(datos$SOD.pie)
shapiro.test(datos$GPx.tent)

# SOD mas menos bien en el pie y peor en el tentaculo. Shapiro no significativo
# CAT pie tiene algunos valores extremos, Shapiro salta por poco. Tent igual pero no muy sig, Aceptable creo
# GST no es nada normal. Nada nada. transformar? Transformacion arregla en pie, tent no tanto. Comprobar por grupos
# DTD perfecta que bonica
# G6PDH da problemas en tentaculo, pie no. Comprobar por grupos
# MDA da problemas en pie, ver por grupos
# teac perfecto
# gr perfecta
# gpx en el limite de ser no normal

# Comprobando normalidad por grupos

hist(filter(datos, tratamiento == "Shade")$GPx.pie)
shapiro.test(filter(datos, tratamiento == "Low_salinity")$GPx.tent)

# GST en pie sale un poco rara en el tratamiento sombreado. Hay un valor que es 715 de repente. Yo probaria a quitarlo
# G6PDH, MDA y CAT estan bien por grupos

### Estudio de datos anómalos e influyentes ---- 


ggplot(datos, aes(x = tratamiento, y = GPx.pie, color = tratamiento)) +
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

modelos <- lapply(colnames(datos[c(4:25)]), function(x){
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

for (i in c(1:22)) {
  print(colnames(datos[4:25][i]))
  print(summary(modelos[[i]]))}


### Elaboración de gráficas y test post-hoc ----

# Bucle construccion de estadisticos de resumen, grafica y test post hoc Tukey
for (n in c(1:22)) {
  i <- colnames(datos[4:25])[[n]]
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
  (p <- barras_tfm() + labs(subtitle = case_when(str_detect(i, "pie") == T  ~ "Column",
                                               str_detect(i, "tent") == T ~ "Tentacle",
                                               TRUE ~ "")))
  saveRDS(p, paste0("./resultados/graficas_upd/", i, "_RDS"))
  ggsave(paste0("./resultados/graficas_upd/", i, ".png"), width = 90, height = 112.5, units = "mm", dpi = 1000)
}

# determinar limite con 1.4*(max(tabla_summ$media) + max(tabla_summ$error))
### Patchwork para figuras finales ----

plots <- lapply(colnames(datos[4:25]), function(x){readRDS(paste0("./resultados/graficas_upd/", x, "_RDS"))})

# SOD + CAT
(p2 <- wrap_plots(plots[3:4]) +
    labs(title = "A.  SOD activity") +
    theme(plot.title = element_text(hjust = -3.07))) # -2.4 left aligned
ggsave(paste0("./resultados/graficas_upd2/SOD.png"), width = 180, height = 112.5, units = "mm", dpi = 1000)

(p3 <- wrap_plots(plots[5:6]) +
    labs(title = "B.  CAT activity") +
    theme(plot.title = element_text(hjust = -2.86))) # -2.35 left aligned
ggsave(paste0("./resultados/graficas_upd2/CAT.png"), width = 180, height = 112.5, units = "mm", dpi = 1000)

(pf_23 <-  p2/p3)
ggsave(paste0("./resultados/graficas_upd2/compuestaSODCAT.png"), width = 180, height = 225, units = "mm", dpi = 1000)

# GST + DTD
(p4 <- wrap_plots(plots[7:8]) +
    labs(title = "A. GST activity") +
    theme(plot.title = element_text(hjust = -2.88))) # -2.45 left aligned
ggsave(paste0("./resultados/graficas_upd2/GST.png"), width = 180, height = 112.5, units = "mm", dpi = 1000)

(p5 <- wrap_plots(plots[9:10]) +
    labs(title = "B.  DTD activity") +
    theme(plot.title = element_text(hjust = -2.87))) # -2.35 left aligned
ggsave(paste0("./resultados/graficas_upd2/DTD.png"), width = 180, height = 112.5, units = "mm", dpi = 1000)

(pf_45 <-  p4/p5)
ggsave(paste0("./resultados/graficas_upd2/compuestaGSTDTD.png"), width = 180, height = 225, units = "mm", dpi = 1000)

# G6PDH gpx y gr
(p6 <- wrap_plots(plots[11:12]) +
    labs(title = "C.  G6PDH activity") +
    theme(plot.title = element_text(hjust = -3.31))) # -2.58 left aligned
ggsave(paste0("./resultados/graficas_upd2/G6PDH.png"), width = 180, height = 112.5, units = "mm", dpi = 1000)

(p9 <- wrap_plots(plots[19:20]) +
    labs(title = "A.  GPx activity") +
    theme(plot.title = element_text(hjust = -2.8))) # -2.44 left aligned
ggsave(paste0("./resultados/graficas_upd2/GPx.png"), width = 180, height = 112.5, units = "mm", dpi = 1000)

(p10 <- wrap_plots(plots[21:22]) +
    labs(title = "B.  GR activity") +
    theme(plot.title = element_text(hjust = -2.63))) # -2.16 left aligned
ggsave(paste0("./resultados/graficas_upd2/GR.png"), width = 180, height = 112.5, units = "mm", dpi = 1000)

(pf_9106 <-  p9/p10/p6)
ggsave(paste0("./resultados/graficas_upd2/compuestaGPxGRG6PDH.png"), width = 180, height = (225+112.5), units = "mm", dpi = 1000)

# TEAC y MDA

(p8 <- wrap_plots(plots[17:18]) +
    labs(title = "A.  Total Antioxidant Capacity (TEAC)") +
    theme(plot.title = element_text(hjust = 5.1))) # 8 left aligned
ggsave(paste0("./resultados/graficas_upd2/TEAC.png"), width = 180, height = 112.5, units = "mm", dpi = 1000)

(p7 <- wrap_plots(plots[13:14]) +
    labs(title = "B.  MDA") +
    theme(plot.title = element_text(hjust =  -1.885))) # -1.62 left aligned
ggsave(paste0("./resultados/graficas_upd2/MDA.png"), width = 180, height = 112.5, units = "mm", dpi = 1000)

(pf_87 <-  p8/p7)
ggsave(paste0("./resultados/graficas_upd2/compuestaTEACMDA.png"), width = 180, height = 225, units = "mm", dpi = 1000)


### Otros test: ----

datos_t <- read.csv2("./datos/datos_t_test.csv", numerals = "warn.loss", encoding = "latin1") %>% 
  mutate(tejido = as.factor(tejido))
  
datos.long <- datos_t %>%
  select(-tratamiento, -n, -muestra, -proteina.pie) %>% 
  pivot_longer(-tejido, names_to = "variables", values_to = "value") %>% 
  mutate(value = as.double(value))

datos.long$variables <- factor(datos.long$variables, levels = c("SOD.pie", "CAT.pie", "GPx.pie", "GR.pie", "G6PDH.pie", "GST.pie", "DTD.pie", "TEAC.pie", "MDA.pie.2"), labels = c("SOD", "CAT", "GPx", "GR", "G6PDH", "GST", "DTD", "TEAC", "MDA"))

sum <- datos.long  %>% 
  group_by(variables, tejido) %>% get_summary_stats(type = "mean_se")
  
  
(paired.t.test <- datos.long  %>% 
  group_by(variables) %>% 
  t_test(value ~ tejido, paired = T) %>% 
  adjust_pvalue(method = "BH") %>% 
  add_significance())


# Create the plot

paired.t.test <- paired.t.test %>% add_xy_position(x = "tejido", scales = "free", fun = "mean_sd")

datos.long %>% 
  group_by(variables, tejido) %>%
  get_summary_stats(type = "mean_se") %>% 
  ggbarplot(x = "tejido", y = "mean",
            fill = "tejido", color = "tejido",
            alpha = 0.2, legend = "none",  palette = "npg", linewidth = 1,
            xlab = FALSE,
            ggtheme = theme_tfm()) %>% 
  facet(facet.by = "variables", nrow = 2, scales = "free", ) +
  geom_errorbar(aes(x = tejido, color = tejido, ymin = mean, max = mean+se),width = 0.3) +
  scale_x_discrete(labels = c("Column", "Tentacle")) +
  stat_pvalue_manual(paired.t.test, label = "p.adj.signif",
                     hide.ns = T)

ggsave(paste0("./resultados/graficas_upd2/T_TEST.png"), width = 180, height = 112.5, units = "mm", dpi = 1000)
ggsave(paste0("./resultados/graficas_upd2/T_TEST.svg"), width = 180, height = 112.5, units = "mm", dpi = 1000)



library(plotrix)
datos_resumen <- group_by(datos_t, tejido) %>% 
  summarise(SODm = mean(SOD),
            SODse = std.error(SOD),
            CATm = mean(CAT),
            CATse = std.error(CAT),
            GSTm = mean(GST),
            GSTse = std.error(GST),
            DTDm = mean(DTD),
            DTDse = std.error(DTD),
            G6PDHm = mean(G6PDH),
            G6PDHse = std.error(G6PDH),
            GPXm = mean(GPX),
            GPXse = std.error(GPX),
            GRm = mean(GR),
            GRse = std.error(GR),
            TEACm = mean(TEAC),
            TEACse = std.error(TEAC),
            MDAm = mean(MDA),
            MDAse = std.error(MDA),)




### Actualizacion graficas ----

datos_t <- read.csv2("./datos/datos_t_test.csv", numerals = "warn.loss", encoding = "latin1", dec = ".") %>% 
  mutate(tejido = as.factor(tejido),
         tratamiento = as.factor(tratamiento))

colnames(datos_t) <- c("n", "muestra", "tratamiento", "tejido", "SOD", "CAT", "GST", "DTD", "G6PDH", "MDA", "proteina", "TEAC", "GPX", "GR")

for (n in c(1:10)) {
  i <- colnames(datos_t[5:14])[[n]]
  tabla_summ <- datos_t %>%  group_by(tejido,tratamiento) %>% 
    summarise(media = mean(get(i), na.rm = T),
              desvest = sd(get(i), na.rm = T),
              error = desvest/sqrt(sum(!is.na(get(i)))))
  if (i == "TEAC" | i == "GPx"| i == "G6PDH") {
    tukey_loop <- TukeyHSD(modelos[[n]])
    cld.tukey <- multcompLetters4(modelos[[n]], tukey_loop, reversed = T)
    (letras <- rownames_to_column(as.data.frame(cld.tukey$tratamiento$Letters)))
    colnames(letras) <- c("tratamiento", "tukey")
    tabla_summ <- merge(tabla_summ, letras)
  } else {
    tabla_summ$tukey <- c("", "", "", "")
  }
  (p <- barras_tfm() + labs(subtitle = case_when(str_detect(i, "pie") == T  ~ "Column",
                                                 str_detect(i, "tent") == T ~ "Tentacle",
                                                 TRUE ~ "")))
  saveRDS(p, paste0("./resultados/graficas4/", i, "_RDS"))
  ggsave(paste0("./resultados/graficas4/", i, ".png"), width = 90, height = 112.5, units = "mm", dpi = 1000)
}
