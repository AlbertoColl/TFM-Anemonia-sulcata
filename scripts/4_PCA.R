### TFM - Alberto Coll Fernandez
# Analisis de Componentes Principales
# Comenzado: 15/06/2023
# Terminado: 05/07/2023

# Falta: revisar PCA, excluir clorofila si eso.
# Exportar csv de tabla factor loadings y luego ponerla bonica
# Revisar grafica PCA, poner puntos de color segun tratamiento?

## SETUP ----
library(tidyverse)
library(GGally) # Correlograma
library(ggvegan) # fortify()
library(vegan) #PCA
library(psych)

setwd("D:/collf/Documents/GitHub/TFM-Ortiguilla")
#source(file = "./scripts/0_data_lab.R") # Laboratorio
source(file = "./scripts/0_data_home.R") # En casa

source(file = "./scripts/1_funciones_graficas.R")

ggthemr("fresh")

### Seleccion de variables, outliers y PCA ----

# Seleccionar variables que incluir en el PCA. ¿Probar separando pie y tentaculo?
datos_pca <- scale(select(datos, SOD.pie, SOD.tent, CAT.pie, CAT.tent, GST.pie, GST.tent, MDA.pie.2, MDA.tent.2, TEAC.pie, TEAC.tent))
names(datos_pca)[7:8] <- c("MDA.tent", "MDA.pie")

outlier(datos_pca) # Calcula Distancia de Mahalanobis y hace QQ plot con cuantiles de la chi cuadrado. El unico que parece un outlier mas gordo es el O4, observacion numero 9, desde un punto de vista multivariable.

datos_pca <- datos_pca[-c(9,19), ] # El punto 9 es potencial outlier pero parece que no influencia mucho el resultado del análisis. Repetir y evaluar de nuevo cuando esten todas las variables.


# Hacer matriz de correlacion
# Ordenar luego manualmente variables segun relevancia en PCA para la grafica
ggcorr(datos_pca, label = F, label_alpha = T,
       hjust = 0.75, size = 3, color = "grey50", layout.exp = 1,
       high = "#3EB59B", mid = "white", low = "#E56A1C") +
  labs(title = "Correlacion entre indicadores medidos",
       subtitle = "ordenados segun PC1/PC2") +
  theme_tfm() +
  theme(axis.line = element_blank())

# Ejecutar PCA
PCA <- rda(na.omit(datos_pca), scale = TRUE)
sum((as.vector(PCA$CA$eig)/sum(PCA$CA$eig))[1:3])
# 3 <- 72 %, 4 <- 85 %, 5 <- 90 % sin DTD
# 3 <- 65 %, 4 <- 77 %, 5 <- 84 % con DTD
# 3 <- 65 %, 4 <- 75 %, 5 <- 82 % con DTD y clor

### Graficas y resultados PCA ----
#Extraer eigenvalues y hacer Scree plot bonico con ggplot
eigenvalues <- as.data.frame(PCA$CA$eig) %>%
  rename("eigenvalue"="PCA$CA$eig") %>% 
  mutate(var_per = eigenvalue/sum(eigenvalue)) %>% 
  rownames_to_column(var = "PC")
eigenvalues$PC <- factor(eigenvalues$PC, levels = c("PC1", "PC2", "PC3", "PC4", "PC5", "PC6", "PC7", "PC8", "PC9", "PC10", "PC11", "PC12", "PC13"))

# Scree plot
(scree.plot <- ggplot(eigenvalues, aes(x = PC,y = var_per)) +
  geom_line(group = 1, color = "#3EB59B") +
  geom_point(size = 3, color = "#0c8890") +
  theme_tfm() +
  scale_y_continuous(labels = scales::percent, limits = c(0,0.5)) +
  xlab("Compontenes Principales") +
  ylab("% de variación explicada"))

ggsave("./resultados/graficas/PCA_scree.png", width = 984, height = 722, units = "px",
       scale = 2, dpi = "retina")

# Factor loadings, exportar en csv para hacer tabla
factor_loading <- rownames_to_column(as.data.frame(t(as.data.frame(PCA[["CA"]][["v"]]))))
names(factor_loading)[1] <- "PC"

write.csv2(merge(eigenvalues, factor_loading), "./resultados/PCA_factor_loadings_clor.csv")

#Biplot, ver si puedes cambiar componentes
biplot(PCA, choices = c(1,2), type = c("text", "points"),
       col = c("#0c8890", "#414066"), scaling = 1)

PCA_df <- fortify(PCA) %>% 
  mutate(Score = ifelse(Score == "species", "variable", "observation"))

(pca.plot <- ggplot() +
  geom_hline(yintercept = 0, color = "#096A71", alpha = 0.7, linewidth = 0.05) +
  geom_vline(xintercept = 0, color = "#096A71", alpha = 0.7, linewidth = 0.05) +
  geom_point(data = filter(PCA_df, Score == "observation"),
             aes(x = PC1, y = PC2),
             size = 2.5, alpha = 0.8, shape = 19) +
  geom_segment(data = filter(PCA_df, Score == "variable"),
               aes(x = 0, y = 0, xend = PC1, yend = PC2),
               color = "#E56A1C", alpha = 0.8, linewidth = 0.6,
               arrow = arrow(length=unit(0.3,"cm"))) +
  geom_text(data = filter(PCA_df, Score == "variable"),
            aes(x = PC1, y = PC2, label = Label), alpha = 0.9,
            position =  position_jitter(width = 0.1),
            fontface = "bold", color = "gray20", size = 3.25) +

  xlab(paste0("Componente Principal 1 (", round(eigenvalues$var_per[1]*100, 2) ,"%)")) +
  ylab(paste0("Componente Principal 2 (", round(eigenvalues$var_per[2]*100, 2) ,"%)")) +
  theme_tfm())

ggsave("./resultados/graficas/PCA_biplot.png", width = 1000, height = 770, units = "px",
       scale = 2, dpi = "retina")

      


