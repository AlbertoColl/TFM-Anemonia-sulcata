### Revision analisis TFM - Alberto Coll Fernandez
# Analisis de crecimiento y reproduccion
# Comenzado: 03/10/2024
# Terminado: 

### SETUP ----
library(tidyverse)
library(rstatix)
library(ggpubr)
library(multcompView)

#setwd("D:/collf/Documents/GitHub/TFM-Anemonia-sulcata") # portatil

setwd("C:/Users/Usuario/Documents/GitHub/TFM-Anemonia-sulcata")  # lab
datos_c <- read.csv2("./datos/datos_peso_REVISION.csv", numerals = "warn.loss", encoding = "latin1")%>% 
  mutate(tratamiento = as.factor(tratamiento))

summary(datos_c)

source(file = "./scripts_enero_2024/1_funciones_graficas.R")

datos_c$tratamiento <- factor(datos_c$tratamiento, levels = c("Control", "Oscuro", "Salobre", "IMTA"), labels = c("Control", "LS", "BW", "IMTA"))

### Exploración de datos ----

# Observamos la distribución de los datos en general y por grupo
hist(datos_c$incremento.n)
datos_c %>% 
  group_by(tratamiento) %>% 
  shapiro_test(incremento.n) 

# Ambas variable respuesta (incrmento en n y ganancia de peso total son normales en los tratamientos salvo la ganancia de peso para el IMTA.

## Comprobacion de outliers
outliers <- datos_c %>%
  group_by(tratamiento) %>%
  identify_outliers(ganancia.de.peso.total..g.)

# Hay 3 potenciales outliers, uno de ellos extremo en el grupo contol. Los otros dos en el grupo Control e IMTA. El dato de IMTA es el que afecta a la normalidad.


### Graficas iniciales----

# Estadisticos de resumen para las graficas
tabla_sum_1 <- datos_c %>% 
  group_by(tratamiento)  %>% 
  get_summary_stats(ganancia.de.peso.total..g., type = "mean_se")
tabla_sum_2 <- datos_c %>% 
  group_by(tratamiento)  %>% 
  get_summary_stats(incremento.n, type = "mean_se")

# Boxplot rapido
(ggboxplot(datos_c, x = "tratamiento", y = "incremento.n",
                  add = "point", color = "tratamiento"))

(ggboxplot(datos_c, x = "tratamiento", y = "ganancia.de.peso.total..g.",
           add = "point", color = "tratamiento"))

# De barras:

# Para ganancia de peso
ggplot(tabla_sum_1) +
  geom_col(aes(x = tratamiento, y = mean, fill = tratamiento), alpha = 1, width = 0.8) +
  geom_errorbar(aes(x = tratamiento, y = mean, ymax = mean + se, ymin = mean - se), width = 0.4, color = "gray45") +
  geom_hline(yintercept=0, linetype="dashed")

# Para incremento de n
ggplot(tabla_sum_2) +
  geom_col(aes(x = tratamiento, y = mean, fill = tratamiento), alpha = 1, width = 0.8) +
  geom_errorbar(aes(x = tratamiento, y = mean, ymax = mean + se, ymin = mean - se), width = 0.4, color = "gray45") +
  geom_hline(yintercept=0, linetype="dashed")


### Anova de una via ----

peso.anova <- anova_test(data = datos_c, formula = ganancia.de.peso.total..g. ~ tratamiento)
get_anova_table(peso.anova)

n.anova <- anova_test(data = datos_c, formula = incremento.n ~ tratamiento)
get_anova_table(n.anova)


# Como el peso sale cercano a 0.05 hay ambiguedad en torno a si existen diferencias significativas entre tratamientos. Como el análisis de outliers reveló puntos potencialmente influyentes, vamos a comprobar si afectan al resultado del análisis.
# No parece que los outliers resuelvan la ambiguedad en el peso. Mejor no quitarlos, salvo igual el 3 solo porque afecta a la normalidad

# Outlier 3: no extremo pero afecta normalidad, baja p valor pero sigue muy amiguo
datos_c$ganancia.de.peso.total..g.[6] <- NA 
datos_c$ganancia.de.peso.total..g.[3] <- NA
peso.anova <- anova_test(data = datos_c, formula = ganancia.de.peso.total..g. ~ tratamiento, type = 3)
get_anova_table(peso.anova)

### Post-hoc test----

# Solo para numero de individuos

anova1 <- aov(data = datos_c, incremento.n ~ tratamiento)

cld = multcompLetters4(anova1, TukeyHSD(anova1), reversed = T)
letras <- rownames_to_column(as.data.frame(cld$tratamiento$Letters))
colnames(letras) <- c("tratamiento", "tukey")
tabla_sum_2 <- merge(tabla_sum_2, letras)


# Graficas final----

# DIFERENCIA NUMERO INDIVIDUOS
ggplot(tabla_sum_2, aes(x = tratamiento)) +
  geom_col(aes(y = mean, fill = tratamiento, color = tratamiento), alpha = 0.1, linewidth = 1) +
  geom_errorbar(aes(ymax = mean + se, ymin = mean-se), width = 0.5, color = "gray35") +
  geom_text(aes(y = (mean+3*se),label = tukey, x = tratamiento), size = 4, fontface = "bold") +
  xlab(NULL) +
  ylim(c(-5, 11)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  theme_tfm() +
  ylab("Difference in total number of individuals") +
  #labs_pubr() +
  guides(fill = "none") +
  scale_fill_manual(values = c("#0c8890", "#54B65D","#E56A1C", "#FBBC4C")) +
  scale_color_manual(values = c("#0c8890", "#54B65D","#E56A1C", "#FBBC4C"))
    


# INCREMENTO DE PESO
ggplot(tabla_sum_1, aes(x = tratamiento)) +
  geom_col(aes(y = mean, fill = tratamiento, color = tratamiento), alpha = 0.1, linewidth = 1) +
  geom_errorbar(aes(ymax = mean + se, ymin = mean-se), width = 0.5, color = "gray35") +
  #geom_text(aes(y = (mean+3*se),label = tukey, x = tratamiento), size = 4, fontface = "bold") +
  xlab(NULL) +
  #ylim(c(-5, 11)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  theme_tfm() +
  ylab("Change in Total Weight (g)") +
  labs_pubr() +
  guides(fill = "none") +
  scale_fill_manual(values = c("#0c8890", "#54B65D","#E56A1C", "#FBBC4C")) +
  scale_color_manual(values = c("#0c8890", "#54B65D","#E56A1C", "#FBBC4C"))





  
