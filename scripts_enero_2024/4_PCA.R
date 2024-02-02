#### Revision analisis TFM - Alberto Coll Fernandez
# Analisis de componentes principales y cluster
# Comenzado:01/02/2024
# Terminado 


### SETUP ----
library(tidyverse)
library(psych) #correlacion
library(MVN)
library(factoextra)

setwd("C:/Users/Usuario/Documents/GitHub/TFM-Anemonia-sulcata")
source(file = "./scripts_enero_2024/0_data_lab.R") # Laboratorio
#source(file = "./scripts/0_data_home.R") # En casa

source(file = "./scripts_enero_2024/1_funciones_graficas.R")
ggthemr("fresh")

datos <- datos %>% select(-c(MDA.pie, MDA.tent, -GST,tent)) # Primera medida de MDA no vale
### Matriz de correlacion ----
datos_cor <- cor(datos[,6:20])
win.graph()
cor.plot(datos_cor)
det(datos_cor)

### Contrastes de correlacion y KMO ----

mvn(datos[,6:20], mvnTest = "mardia") # normalidad un poco eee

cortest.bartlett(datos[,6:20], nrow(datos))
# La normalidad esta un poco cuestionada, pero el contraste dice que es diferente de la matriz identidad.

KMO(datos_cor) # esta bien sin gst tent

### Modelo PCA y eleccion de numero de componentes ----
cp <- princomp(~., data = datos[,6:20], cor = TRUE)
cp2$loadings
write.csv2(cp2$loadings, file = "./resultados/cargas.factoriales.sing.gst.tent.csv")
summary(cp2)
# 4 componentes ya superan 70 % varianza
win.graph()
screeplot(cp2, type = "lines") # 3 o 4 componentes
cp$sdev^2 # 5 componentes segun regla de Kaiser

# Voy a seleccionar 4 componentes

### Biplot ----

fviz_pca_biplot(cp2, palette = ggthemr_palette("steelblue"))
fviz_pca_var(cp2, col.var = "steelblue", col.circle = "grey40")


 ### Añadimos a los datos ----
datos <- cbind(datos, cp1 = cp$scores[,1], cp2 = cp$scores[,2], cp3 = cp$scores[,3], cp4 = cp$scores[,4])


### CLUSTERING ----

datos_scl <- scale(datos[,6:21])
distancia <- dist(datos_scl)

ac <- hclust(distancia)
win.graph()
plot(ac)


fviz_nbclust(datos_scl, kmeans, method = "wss")
set.seed(999)

ac.2 <- kmeans(as.matrix(datos_scl), 4)
ac.2


win.graph()
fviz_cluster(ac.2, datos_scl, ellipse.type = "norm")
