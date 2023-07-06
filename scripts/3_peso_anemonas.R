### TFM - Alberto Coll Fernandez
# Grafica de peso de anemonas
# Comenzado: 05/07/2023
# Terminado: 05/07/2023

### SETUP ----
library(tidyverse)

setwd("D:/collf/Documents/GitHub/TFM-Ortiguilla")
datos <- read.csv2("./datos/datos_peso.csv", numerals = "warn.loss", encoding = "latin1")%>% 
  mutate(tratamiento = as.factor(tratamiento))
summary(datos)

# Necesito poner inicial y final como otra variable, medida
datos <- datos %>% gather(c(peso.inicial, peso.final), key = medida, value = peso)
datos$medida <- as.factor(datos$medida)
levels(datos$medida) <- c("final", "inicial")
datos$medida <- factor(datos$medida, levels = c("inicial", "final"))
datos$tratamiento <- factor(datos$tratamiento, levels = c("Control", "Oscuro", "Salobre", "IMTA"))
summary(datos)

### Resumen y grafica ----
resumen <- datos %>% 
  group_by(tratamiento, medida) %>% 
  summarise(media = mean(peso, na.rm = T),
            desvest = sd(peso, na.rm = T),
            error = desvest/sqrt(sum(!is.na(peso))),
            n = sum(!is.na(peso)),
            mediana = median(peso, na.rm = T),
            minimo = min(peso, na.rm =T),
            maximo = max(peso, na.rm = T))


(plot <- ggplot(resumen, aes(x = tratamiento)) +
  geom_errorbar(aes(ymax = media + error, ymin = media-error, group = tratamiento:medida), position = position_dodge(width = 0.93), width = 0.7, color = "gray75") +
  geom_col(aes(y = media, fill = tratamiento:medida), position = position_dodge(width = 0.93)) +
  geom_text(aes(y =  1.5 ,label = paste0("n = ", n), group = tratamiento:medida), position = position_dodge(width = 0.93), color = "white") +
    geom_text(aes(y =  3 ,label = medida, group = tratamiento:medida), position = position_dodge(width = 0.93), color = "white") +
  ylab("Peso medio (g)") +
  xlab(NULL) +
  labs(title = "Peso medio inicial y final, por tratamiento") +
  theme_tfm() +
  scale_fill_manual(values = c("#0c8890", "#0C9CA6", "#3EB59B", "#47D2B4", "#E56A1C", "#F37629", "#F9AB24", "#FBBC4C"))
)

ggsave("./resultados/graficas/peso_n.png", width = 1000, height = 750, units = "px",
       scale = 2, dpi = "retina")




ggplot(datos) +
  geom_boxplot(aes(x = tratamiento, y = peso, fill = medida), alpha = 0.2) +
  geom_point(aes(x = tratamiento, y = peso, color = medida), position = position_jitterdodge())
