#### Revision analisis TFM - Alberto Coll Fernandez
# Analisis de componentes principales y cluster
# Comenzado: 01/02/2024
# Terminado: 09/02/2024


### SETUP ----
library(tidyverse)
library(psych) #correlacion
library(MVN)
library(factoextra)
library(patchwork)


#setwd("C:/Users/Usuario/Documents/GitHub/TFM-Anemonia-sulcata")
setwd("D:/collf/Documents/GitHub/TFM-Anemonia-sulcata") #portatil

#source(file = "./scripts_enero_2024/0_data_lab.R") # Laboratorio
source(file = "./scripts_enero_2024/0_data_home.R") # portatil

source(file = "./scripts_enero_2024/1_funciones_graficas.R")
ggthemr("fresh")

# Primera medida de MDA no vale
# Proteina es linealmente dependiente del resto de enzimas
datos <- datos %>% select(-c(MDA.pie, MDA.tent, proteina.pie, proteina.tent)) %>% 
  rename(MDA.col = MDA.pie.2, MDA.tent = MDA.tent.2,
         SOD.col = SOD.pie, CAT.col = CAT.pie,
         GST.col = GST.pie, DTD.col = DTD.pie,
         G6PDH.col = G6PDH.pie, TEAC.col = TEAC.pie,
         GPx.col = GPx.pie, GR.col = GR.pie)

### Matriz de correlacion ----
datos_cor <- cor(datos[,6:23])
cor.plot(datos_cor)
det(datos_cor)

### Contrastes de correlacion y KMO ----

mvn(datos[,6:23], mvnTest = "mardia") # Sin proteina ni clorofila, el conjunto de datos es normal

cortest.bartlett(datos[,6:23], nrow(datos))
# Se rechaza H0, la matriz de correlacion es diferente a la matriz identidad

KMO(datos_cor) # 0.41 global, aceptable. GST tent, G6PDH tent y TEAC tent tienen poca correlacion parcial con el resto pero aportan informcion importante.

### Modelo PCA y eleccion de numero de componentes ----
cp <- princomp(~., data = as.data.frame(scale(datos[,6:23])), cor = TRUE)

cp$loadings
write.csv2(cp$loadings, file = "./resultados/cargas.factoriales.csv")
summary(cp)
# 4 componentes ya superan 70 % varianza
screeplot(cp, type = "lines") # 4 componentes
cp$sdev^2 # 5 componentes segun regla de Kaiser

# Voy a seleccionar 5 componentes

### Biplot y graficas ----

(biplot <- fviz_pca_biplot(cp, axes = c(1,2), col.ind = "slateblue4",
                           alpha.ind = 0.4, alpha.var = "contrib",
                           col.var = "contrib", 
                           gradient.cols = c("#0c8890","#54B65D", "#D42828"), 
                           repel = TRUE, label = "var", ggtheme = theme_tfm(),
                           select.var = list(contrib = 15), labelsize = 3) + 
   theme(plot.title = element_text(hjust= 0.5)))
ggsave("./resultados/graficas3/PCA_biplot.png", width = 90, height = 90, units = "mm", dpi = 1000, scale = 1.25)


(indplot <- fviz_pca_ind(cp, col.ind = datos$tratamiento,
             palette = c("#0c8890", "#54B65D","#E56A1C", "#FBBC4C"),
             addEllipses = T, legend.title = "Treatment",
             ggtheme = theme_tfm()) +
    theme(legend.position = "right") + labs(title = "B"))

(varplot <- fviz_pca_var(cp, labelsize = 2, col.var = "contrib",
             gradient.cols = c("#0c8890","#54B65D", "#D42828"),
             alpha.var = "contrib", repel = T,
             legend.title = "Contribution", col.circle = "gray75",
             ggtheme = theme_tfm(), select.var = list(contrib = 15)) + 
    theme(legend.position = "right"))

ggsave("./resultados/graficas3/PCA_var.png", width = 90, height = 90, units = "mm", dpi = 1000, scale = 1.25)

### Añadimos las puntuaciones a los datos ----
datos_pca <- datos
datos_pca <- cbind(datos_pca, cp1 = cp$scores[,1], cp2 = cp$scores[,2], cp3 = cp$scores[,3], cp4 = cp$scores[,4])

### PCA por tejido ----
# PIE
datos_p <- datos %>% select(-SOD.tent, -CAT.tent, -GPx.tent, -GR.tent, -GST.tent, -DTD.tent, -G6PDH.tent, -TEAC.tent, -MDA.tent)

datos_p_cor <- cor(datos_p[6:14])
det(datos_p_cor)
mvn(datos_p[,6:14], mvnTest = "mardia")
cortest.bartlett(datos_p[,6:14], nrow(datos))
KMO(datos_p_cor)

cp_p <- princomp(~., data = as.data.frame(scale(datos_p[,6:14])), cor = TRUE)
summary(cp_p) # 3 componentes
screeplot(cp_p, type = "lines") # 2-3 componentes
cp_p$sdev^2 # 3 componentes segun regla de Kaiser

(biplot <- fviz_pca_biplot(cp_p, axes = c(1,2), col.ind = "slateblue4",
                           alpha.ind = 0.4, alpha.var = "contrib",
                           col.var = "contrib", 
                           gradient.cols = c("#0c8890","#54B65D", "#D42828"), 
                           repel = TRUE, label = "var", ggtheme = theme_tfm(),
                           labelsize = 3))

# TENTACULO
datos_t <- datos %>% select(-SOD.col, -CAT.col, -GPx.col, -GR.col, -GST.col, -DTD.col, -G6PDH.col, -TEAC.col, -MDA.col)

datos_t_cor <- cor(datos_t[6:14])
det(datos_t_cor)
mvn(datos_t[,6:14], mvnTest = "mardia")
cortest.bartlett(datos_t[,6:14], nrow(datos))
KMO(datos_t_cor)

cp_t <- princomp(~., data = as.data.frame(scale(datos_t[,6:14])), cor = TRUE)
summary(cp_t) # 4 componentes
screeplot(cp_t, type = "lines") #3-4 componentes
cp_t$sdev^2 # 3 componentes segun regla de Kaiser

(biplot <- fviz_pca_biplot(cp_t, axes = c(1,2), col.ind = "slateblue4",
                           alpha.ind = 0.4, alpha.var = "contrib",
                           col.var = "contrib", 
                           gradient.cols = c("#0c8890","#54B65D", "#D42828"), 
                           repel = TRUE, label = "var", ggtheme = theme_tfm(),
                           labelsize = 3))
