### TFM - Alberto Coll Fernandez
# Grafica de peso de anemonas
# Comenzado: 05/07/2023
# Terminado: 05/07/2023

### SETUP ----
library(tidyverse)
library(car)
library(multcompView)

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

### Grafica de barras: media y SEM ----
resumen <- datos %>% 
  group_by(tratamiento, medida) %>% 
  summarise(media = mean(peso, na.rm = T),
            desvest = sd(peso, na.rm = T),
            error = desvest/sqrt(sum(!is.na(peso))),
            n = sum(!is.na(peso)),
            mediana = median(peso, na.rm = T),
            minimo = min(peso, na.rm =T),
            maximo = max(peso, na.rm = T))

datos$medida <- factor(datos$medida, levels = c("inicial", "final"))
resumen$medida <- factor(resumen$medida, levels = c("inicial", "final"))

(plot <- ggplot(resumen, aes(x = tratamiento)) +
  geom_errorbar(aes(ymax = media + error, ymin = media-error, group = tratamiento:medida), position = position_dodge(width = 0.93), width = 0.7, color = "gray75") +
  geom_col(aes(y = media, fill = tratamiento:medida), position = position_dodge(width = 0.93)) +
  geom_text(aes(y =  1.5 ,label = paste0("n = ", n), group = tratamiento:medida), position = position_dodge(width = 0.93), color = "white", size = 3) +
    geom_text(aes(y =  3 ,label = medida, group = tratamiento:medida), position = position_dodge(width = 0.93), color = "white", size = 3) +
  ylab("Peso medio (g)") +
  xlab(NULL) +
  #labs(title = "Peso medio inicial y final, por tratamiento") +
  theme_tfm() +
    ylim(c(0, 30)) +
  scale_fill_manual(values = c("#0c8890", "#0C9CA6", "#3EB59B", "#47D2B4", "#E56A1C", "#F37629", "#F9AB24", "#FBBC4C"))
)

ggsave("./resultados/graficas/peso_n_v3.png", width = 1000, height = 750, units = "px",scale = 2, dpi = "retina")


### Grafica dual: n y barras ----

datos$medida <- factor(datos$medida, levels = c("inicial", "final"))
resumen$medida <- factor(resumen$medida, levels = c("inicial", "final"))

(dual_plot <- ggplot(resumen, aes(x = tratamiento)) +
    geom_errorbar(aes(ymax = media + error, ymin = media-error, group = tratamiento:medida), position = position_dodge(width = 0.93), width = 0.7, color = "gray75") +
    geom_col(aes(y = media, fill = tratamiento:medida), position = position_dodge(width = 0.93)) +
    geom_text(aes(y =  1.5 ,label = paste0("n = ", n), group = tratamiento:medida), position = position_dodge(width = 0.93), color = "white", size = 3) +
    geom_text(aes(y =  3 ,label = medida, group = tratamiento:medida), position = position_dodge(width = 0.93), color = "gray99", size = 3) +
    xlab(NULL) +
    #labs(title = "Peso medio inicial y final, por tratamiento") +
    theme_tfm() +
    ylim(c(0, 30)) +
    scale_y_continuous(name = "Peso medio (g)", sec.axis = sec_axis(~.*6, name="número de individuos")) +
    geom_line(aes(x = tratamiento, y =n/6, group = tratamiento), position = position_dodge2(0.93), color = "gray80") +
    geom_point(aes(x = tratamiento, y = n/6, group = tratamiento:medida), color = "gray89", position = position_dodge(width = 0.93)) +
    scale_fill_manual(values = c("#0c8890", "#0C9CA6", "#3EB59B", "#47D2B4", "#E56A1C", "#F37629", "#F9AB24", "#FBBC4C"))
)

ggsave("./resultados/graficas/peso_n_v4.png", width = 1000, height = 750, units = "px",scale = 2, dpi = "retina")

### Grafica de cajas y jitter ----

datos$medida <- factor(datos$medida, levels = c("final", "inicial"))
resumen$medida<- factor(resumen$medida, levels = c("final", "inicial"))

(bp <- ggplot() +
    geom_boxplot(data = datos, aes(x = tratamiento, y = peso, color = tratamiento:medida), alpha = 0, linewidth = 0.5) +
    geom_point(data = datos, aes(x = tratamiento, y = peso, color = tratamiento:medida),  position = position_jitterdodge(jitter.width = 0.8), alpha = 0.4, size = 1.2) +
    geom_text(data = resumen, aes(x =  tratamiento, y =  -7 ,label = paste0(medida, "\nn = ", n), group = tratamiento:medida), position = position_dodge(width = 0.8), color = "gray45", size = 3) +
    #geom_text(data = datos, aes(x = tratamiento, y = peso, color = tratamiento:medida, label = paste0(peso, " g")), check_overlap = F, position = position_jitterdodge(jitter.width = 0.8, jitter.height = 1, dodge.width = 0.7), alpha = ifelse(datos$peso > 50, 0.9, 0), size = 2) +
    xlab("") +
    ylab("Peso (g)") +
    ylim(c(-10, NA)) +
    #labs(title = "") +
    scale_fill_manual(values = c("#0c8890", "#0C9CA6", "#3EB59B", "#47D2B4", "#E56A1C", "#F37629", "#F9AB24", "#FBBC4C")) +
    scale_color_manual(values = c("#0c8890", "#0C9CA6", "#3EB59B", "#47D2B4", "#E56A1C", "#F37629", "#F9AB24", "#FBBC4C")) +
    theme_tfm() +
    coord_flip() +
    scale_x_discrete(limits = rev(levels(datos$tratamiento))) +
    theme(panel.grid.major.y = element_line(linetype = "dotted"))
)


ggsave("./resultados/graficas/peso_box_v2.png", width = 1000, height = 700, units = "px",scale = 2, dpi = "retina")

(combi <- gridExtra::grid.arrange(plot,bp, nrow = 1, ncol = 2,
                                  top = grid::textGrob("Peso medio inicial y final, por tratamiento", gp = grid::gpar(fontsize = 18,font = 2, col = "gray30"), hjust = 0.82)))
ggsave(plot = combi,"./resultados/graficas/peso_combinada.png", width = 1500, height = 700, units = "px",scale = 2, dpi = "retina")



### ANOVA glm() ----

# No siguen distribucion normal, estan sesgados a la derecha por lo que usamos distribucion gamma con glm()
m.peso <- glm(family = Gamma(), peso ~ tratamiento, data = filter(datos, medida == "final"))
summary(m.peso)
plot(m.peso)
# Las graficas de diagnostico salen de maravilla

# Ahora obtenemos tabla anova para el contraste. Importante, está desbalanceado así que necesitamos Anova de tipo 2

(tabla_anova <- Anova(m.peso, type = 2))

tukey <- TukeyHSD(tabla_anova)
cld.tukey <- multcompLetters4(m.peso, tukey, reversed = T)
letras <- rownames_to_column(as.data.frame(cld.tukey$`tratamiento:medida`$Letters))
letras <- letras %>%
  mutate(rowname = str_split(rowname, ":"),
         letras = cld.tukey$`tratamiento:medida`$Letters)
for (n in length(letras$rowname)) {
  letras$tratamiento[[n]] <- letras$rowname[[n]][1]
  letras$medida[[n]] = letras$rowname[[n]][2]
  
}

    #   medida = str_split(rowname, ":")[[1]][2]) %>% 
  #select(tratamiento, medida, letras)

merge(resumen, letras)
