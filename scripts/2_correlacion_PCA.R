### TFM - Alberto Coll Fernandez
# Analisis de Componentes Principales
# Comenzado: 15/06/2023
# Terminado:

## SETUP ----
library(tidyverse)
library(GGally) # Correlograma
library(ggvegan)# Biplot
library(vegan) #PCA

setwd("D:/collf/Documents/GitHub/TFM-Ortiguilla")
#source(file = "./scripts/0_data_lab.R") # Laboratorio
source(file = "./scripts/0_data_home.R") # En casa

source(file = "./scripts/1_funciones_graficas.R")

ggthemr("fresh")

### Seleccion de variables y PCA ----

# Seleccionar variables que incluir en el PCA. ¿Probar separando pie y tentaculo?
datos_pca <- scale(select(datos, CAT.pie, CAT.tent, GST.pie, GST.tent, MDA.pie, MDA.tent, TEAC.pie, TEAC.tent)) # Clorofila, peso, proteina y TEAC no son muy relevantes parece ser

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
sum((as.vector(PCA$CA$eig)/sum(PCA$CA$eig))[1:4]) # queremos un 80%


### Graficas y resultados PCA ----
#Extraer eigenvalues y hacer Scree plot bonico con ggplot
eigenvalues <- as.data.frame(PCA$CA$eig) %>%
  rename("eigenvalue"="PCA$CA$eig") %>% 
  mutate(var_per = eigenvalue/sum(eigenvalue)) %>% 
  rownames_to_column(var = "PC")

ggplot(eigenvalues, aes(x = PC,y = var_per)) +
  geom_line(group = 1, color = "#3EB59B") +
  geom_point(size = 3, color = "#0c8890") +
  theme_tfm() +
  scale_y_continuous(labels = scales::percent, limits = c(0,0.5)) +
  xlab("Compontenes Principales") +
  ylab("% de variación explicada")


# Factor loadings, exportar en csv para hacer tabla
write.csv2(as.data.frame(PCA[["CA"]][["v"]]), "./resultados/PCA_factor_loadings.csv")

#Biplot, ver si puedes cambiar componentes
biplot(PCA, choices = c(1,2), type = c("text", "points"),
       col = c("#0c8890", "#414066"), scaling = 1)
# Esto con formato ggplot
autoplot(PCA, arrows = T) + 
  xlab(paste0("Componente Principal 1 (", round(eigenvalues$var_per[1]*100, 2) ,"%)")) +
  ylab(paste0("Componente Principal 2 (", round(eigenvalues$var_per[2]*100, 2) ,"%)")) +
  theme_tfm()

PCA_df <- fortify(PCA) %>% 
  mutate(Score = ifelse(Score == "species", "variable", "observation"))

ggplot() +
  geom_point(data = filter(PCA_df, Score == "observation"),
             aes(x = PC1, y = PC2), size = 2.5, alpha = 0.9) +
  geom_segment(data = filter(PCA_df, Score == "variable"),
               aes(x = 0, y = 0, xend = PC1, yend = PC2),
               color = "#E56A1C", arrow  =arrow(length=unit(0.3,"cm"))) +
  geom_text(data = filter(PCA_df, Score == "variable"),
            aes(x = PC1, y = PC2, label = Label), alpha = 0.75,
            position = "jitter" ) +
  ylim(c(-2, 2)) +
  xlim(c(-2.5,2.5)) +
  xlab(paste0("Componente Principal 1 (", round(eigenvalues$var_per[1]*100, 2) ,"%)")) +
  ylab(paste0("Componente Principal 2 (", round(eigenvalues$var_per[2]*100, 2) ,"%)")) +
  theme_tfm()

             