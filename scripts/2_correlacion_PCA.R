### TFM - Alberto Coll Fernandez
# Lectura y limpieza de datos
# Comenzado: 15/06/2023
# Terminado:

## SETUP ----
library(tidyverse)
library(GGally)
library(factoextra)

setwd("D:/collf/Documents/GitHub/TFM-Ortiguilla")
#source(file = "./scripts/0_data_lab.R") # Laboratorio
source(file = "./scripts/0_data_home.R") # En casa
source(file = "./scripts/1_funciones_graficas.R")

ggthemr("fresh")
# Pruebas correlogramas ----


cor_matrix <- cor(datos[4:23]) # Matriz de correlacion para PCA

# Ordenar manualmente variables segun relevancia en PCA para la grafica

ggcorr(datos[4:23], label = F, label_alpha = T,
       hjust = 0.75, size = 3, color = "grey50", layout.exp = 1,
       high = "#3EB59B", mid = "white", low = "#E56A1C") +
  labs(title = "Correlacion entre indicadores medidos",
  subtitle = "ordenados segun PC1/PC2") +
  theme_tfm() +
  theme(axis.line = element_blank())

# Pruebas PCA ----
# Se puede hacer con distintos paquetes, vegan es el que conozco pero esta muy orientado a ecologia, buscar otro metodo mas generalista.

# Primer paso: normalizar datos con la funcion scale() y quitar columnas no numericas. En este analisis dan igual los tratamientos.
datos_pca <- scale(select(datos, CAT.pie, CAT.tent, GST.pie, GST.tent, MDA.pie, MDA.tent))

# Segundo paso: Hacer matriz de correlacion
cor_matrix <- cor(datos_pca) # Matriz de correlacion para PCA

ggcorr(datos_pca, label = F, label_alpha = T,
       hjust = 0.75, size = 3, color = "grey50", layout.exp = 1,
       high = "#3EB59B", mid = "white", low = "#E56A1C") +
  labs(title = "Correlacion entre indicadores medidos",
       subtitle = "ordenados segun PC1/PC2") +
  theme_tfm() +
  theme(axis.line = element_blank())


# Tercer paso: aplicar princomp() sobre la matriz de correlacion para ejecutar el analisis de componentes principales
pca <- prcomp(na.omit(datos_pca), scale = T)


# Cuarto paso: visualizar resultados
# - scree plot: representa la importancia de los distintos componentes principales de la matriz.Lo puedo hacer con fviz_eig() del paquete factoextra

fviz_eig(pca)

barplot(as.vector(pca$rotation)/sum(pca$rotation)) # Lo mismo en cutre

sum((as.vector(pca$sdev)/sum(pca$sdev))[1:3])

# - biplot: representa los dos componentes mayoritarios como los dos ejes, y cada variable analizada como un vector segun su contribucion a cada uno de los dos componentes. Resultado principal del PCA. fviz_pca_var() en factoextra

biplot(pca)
fviz_pca_var(pca)

# - grafica de contribucion: adicionalmente, se puede elaborar una grafica de barras en la que se represente como contribuye cada variable al componer el PC1 o PC2, o incluso ambos simultaneamente


# Intentar hacerlo con vegan, bastante bien tambien pero hay que cambiar el biplot: etiquetas no pueden decir sit, y ajustar escala
library(vegan)
PCA <- rda(na.omit(datos_pca), scale = TRUE)
barplot(as.vector(PCA$CA$eig)/sum(PCA$CA$eig))
sum((as.vector(PCA$CA$eig)/sum(PCA$CA$eig))[1:2]) # 79%, this is ok.
plot(PCA)
plot(PCA, display = "sites", type = "points")
plot(PCA, display = "species", type = "text")
biplot(PCA, choices = c(1,2), type = c("text", "points"), xlim = c(-5,10)) + theme_tfm()
