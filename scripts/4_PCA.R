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
library(gridExtra)

setwd("D:/collf/Documents/GitHub/TFM-Ortiguilla")
#source(file = "./scripts/0_data_lab.R") # Laboratorio
source(file = "./scripts/0_data_home.R") # En casa

source(file = "./scripts/1_funciones_graficas.R")

ggthemr("fresh")

### Seleccion de variables, outliers y PCA ----

# Seleccionar variables que incluir en el PCA. ¿Probar separando pie y tentaculo?
datos_pca <- scale(select(datos, SOD.pie, SOD.tent, CAT.pie, CAT.tent, GST.pie, DTD.pie, MDA.pie.2, MDA.tent.2, TEAC.pie, TEAC.tent))
colnames(datos_pca)[7:8] <- c("MDA.tent", "MDA.pie")

outlier(datos_pca) # Calcula Distancia de Mahalanobis y hace QQ plot con cuantiles de la chi cuadrado. El unico que parece un outlier mas gordo es el O4, observacion numero 9, desde un punto de vista multivariable.

datos_pca <- datos_pca[-c(9), ]

# Ejecutar PCA
PCA <- rda(na.omit(datos_pca), scale = TRUE)
sum((as.vector(PCA$CA$eig)/sum(PCA$CA$eig))[1:3])
sum((as.vector(PCA$CA$eig)/sum(PCA$CA$eig))[1:4])
sum((as.vector(PCA$CA$eig)/sum(PCA$CA$eig))[1:5])
# 3 <- 72 %, 4 <- 85 %, 5 <- 90 % sin DTD

# 3 <- 65 %, 4 <- 74 %, 5 <- 81 % con DTD, sin quitar
# 3 <- 66.6 %, 4 <- 77.6 %, 5 <- 84.7 % con DTD, quitando 9

# 3 <- 65.1 %, 4 <- 77.9 %, 5 <- 84.3 % con DTD, quitando 8 y 9
# 3 <- 67.0 %, 4 <- 76.9 %, 5 <- 84.3 % con DTD, quitando 9 y 20

# 3 <- 68.0 %, 4 <- 78.5 %, 5 <- 85 % con DTD quitando 8,9 y 20
# 3 <- 69.6 %, 4 <- 81.0 %, 5 <- 87.0 % con DTD quitando 7,8,9 y 20

# 3 <- 70.5 %, 4 <- 81.2 %, 5 <- 86.8 % sin DTD.tent, quitando 9 y 20
# 3 <- 73.8 %, 4 <- 82.59 %, 5 <- 88.35 % sin DTD.tent ni GST tent, quitando 9 EL MEJOR HASTA EL MOMENTO, SE QUITAN ESTAS DOS VARIABLES QUE TIENEN VALORES DE ACTIVIDAD TAN BAJOS QUE SOLO ENTURBIAN




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

write.csv2(merge(eigenvalues, factor_loading), "./resultados/PCA_factor_loadings_sinDTDniGST.csv")

#Biplot, ver si puedes cambiar componentes
biplot(PCA, choices = c(1,2), type = c("text", "points"),
       col = c("#0c8890", "#414066"), scaling = 1)

PCA_df <- fortify(PCA) %>% 
  mutate(Score = ifelse(Score == "species", "variable", "observation"),
         PC1 = ifelse(Score == "variable", PC1 * 3.46472212261711, PC1),
         PC2 = ifelse(Score == "variable", PC2 * 2.00974727259418, PC2),
         PC3 = ifelse(Score == "variable", PC3 * 2.00974727259418, PC3))
         
(pca.plot <- ggplot() +
  geom_hline(yintercept = 0, color = "#096A71", alpha = 0.7, linewidth = 0.05) +
  geom_vline(xintercept = 0, color = "#096A71", alpha = 0.7, linewidth = 0.05) +
  geom_point(data = filter(PCA_df, Score == "observation"),
             aes(x = PC1, y = PC2),
             size = 2.5, alpha = 0.6, shape = 19, color = "#3EB59B") +
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
    xlim(c(-2.5,3.8)) +
    ylim(c(-1.5,2)) +
    labs(title = "Analisis de componentes principales") +
  theme_tfm())

ggsave("./resultados/graficas/PCA_biplot.png", width = 1000, height = 770, units = "px",
       scale = 2, dpi = "retina")

empty <- ggplot() + theme(axis.line = element_blank())
pca_final <- grid.arrange(pca.plot, grid.arrange(scree.plot, empty), ncol = 2)
  
ggsave(plot = pca_final, "./resultados/graficas/PCA_combinado.png", width = 1500, height = 800, units = "px",
       scale = 2, dpi = "retina")

