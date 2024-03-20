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


setwd("C:/Users/Usuario/Documents/GitHub/TFM-Anemonia-sulcata")
#setwd("D:/collf/Documents/GitHub/TFM-Anemonia-sulcata") #portatil

source(file = "./scripts_enero_2024/0_data_lab.R") # Laboratorio
#source(file = "./scripts_enero_2024/0_data_home.R") # portatil

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
screeplot(cp, type = "lines") # 4 o 5 componentes
cp$sdev^2 # 6 componentes segun regla de Kaiser

# Voy a seleccionar 5 componentes

### Biplot y graficas ----

(biplot <- fviz_pca_biplot(cp, col.ind = "gray30", alpha.ind = 0.8,
                alpha.var = "contrib", col.var = "contrib",
                gradient.cols = c("#0c8890","#54B65D", "#D42828"),
                repel = TRUE, label = "var", ggtheme = theme_tfm(),
                select.var = list(contrib = 15), labelsize = 2))
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

### Ver componentes principales por grupo ----
i <- "cp3"
tabla_summ <- datos_pca %>%  group_by(tratamiento) %>% 
  summarise(media = mean(get(i), na.rm = T),
            desvest = sd(get(i), na.rm = T),
            error = desvest/sqrt(sum(!is.na(get(i)))))
ggplot(tabla_summ, aes(x = tratamiento, y = media, color = tratamiento)) +
  geom_col(aes(fill = tratamiento), alpha = 0.2) +
  geom_errorbar(aes(ymax = media + error, ymin = media- error), width = 0.7, color = "gray55") +
  geom_point(data = datos_pca, aes(y = cp1), position = position_jitter(height = 0, width = 0.1), size = 2)

### ANOVAS por PC----
m1 <- aov(cp4 ~ tratamiento, datos_pca)
summary(m1)


### CLUSTERING ----

datos_scl <- scale(datos[-8,6:19])
#datos_scl <- datos_scl[-8,]
distancia <- dist(datos_scl)

ac <- hclust(distancia)
win.graph()
plot(ac)


fviz_nbclust(datos_scl, kmeans, method = "wss")
set.seed(999)

ac.2 <- kmeans(as.matrix(datos_scl), 3)
ac.2


win.graph()
fviz_cluster(ac.2, datos_scl, ellipse.type = "norm")


