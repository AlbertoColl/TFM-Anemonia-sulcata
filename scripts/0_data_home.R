### TFM - Alberto Coll Fernandez
# Lectura y limpieza de datos
# Comenzado: 05/06/2023
# Terminado:

library(tidyverse)
library(GGally)
library(corrgram)

# Importacion ----

setwd("D:/collf/Documents/GitHub/TFM-Ortiguilla")
datos <- read.csv2("./datos/TFM_datos.csv", numerals = "warn.loss", encoding = "latin1")%>% 
  mutate(tratamiento = as.factor(tratamiento))

summary(datos)
hist(log(datos$peso.total..g.))



# Pruebas correlogramas ----
cor(datos[4:23])
ggcorr(datos[4:9], label = T, label_alpha = T) +
  labs(title = "Correlacion entre indicadores medidos, ordenados segun PC1/PC2")
  

corrgram(datos, order = T, lower.panel = panel.shade, upper.panel = NULL,
         text.panel = panel.txt, main = "Correlacion entre indicadores medidos, ordenados segun PC1/PC2")

# Me gusta que se ordenen las variables segun el PCA pero me gusta mas la funcion ggcorr, asi que es posible que tenga que odernar las variables manualmente despues de hacer un PCA. 
ggplot(datos, aes(x = peso.total, y = clorofila.total)) +
  geom_point(aes(color = tratamiento), size = 3) +
  geom_smooth(se = F) +
  theme_minimal()



# Pruebas PCA ----
# Se puede hacer con distintos paquetes, vegan es el que conozco pero esta muy orientado a ecologia, buscar otro metodo mas generalista.

# Primer paso: normalizar datos con la funcion scale() y quitar columnas no numericas. En este analisis dan igual los tratamientos.

# Segundo paso: Hacer matriz de correlacion

# Tercer paso: aplicar princomp() sobre la matriz de correlacion para ejecutar el analisis de componentes principales

# Cuarto paso: visualizar resultados
    # - scree plot: representa la importancia de los distintos componentes principales de la matriz.Lo puedo hacer con fviz_eig() del paquete factoextra
    # - biplot: representa los dos componentes mayoritarios como los dos ejes, y cada variable analizada como un vector segun su contribucion a cada uno de los dos componentes. Resultado principal del PCA. fviz_pca_var() en factoextra
    # - grafica de contribucion: adicionalmente, se puede elaborar una grafica de barras en la que se represente como contribuye cada variable al componer el PC1 o PC2, o incluso ambos simultaneamente