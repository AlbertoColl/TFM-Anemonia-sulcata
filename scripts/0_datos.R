### TFM Ortiguilla - Lectura y limpieza de datos
# Comienzo: 26/05/2023

library(tidyverse)

# Importacion ----

setwd("D:/collf/Documents/GitHub/TFM-Ortiguilla")
# Aqui pon la carpeta donde vas a trabajar y guardar los archivos
# Ojo con las barritas que es tipico equivocarse y ponerlas asi \

datos <- read.csv2("./datos/datos.csv", numerals = "warn.loss", encoding = "latin1")
# Aqui le pones la direccion exacta del archivo dentro de la carpeta que has puesto antes. Yo tengo mas barras porque lo tengo dentro de una carpeta, si no pon solo el nombre y extension del archivo. Importante que este guardado en csv.
# Si te salen cosas raras prueba con la funcion read.csv() en vez de read.csv2()



# Limpieza de datos ----

summary(datos) # Comprobar que todos las variables estan bien reconocidas
# Ej: que si tienes una variable "Tramo" que toma valores 1,2,3 o 4, no te la ponga como una variable numerica sino como un factor que es lo que es

# Si tienes que cambiar alguna:

# datos$variable <- as.factor(datos$variable)


# Modificar o filtrar las variables que haga falta con mutate()
# Puedes cambiar valores segun un condicion, puedes transformar una variable, etc
# Ejemplos:
datos <- datos %>% 
  mutate(variable = ifelse(variable < 0, NA, variable), #Si es negativo, cambia por 0
         variable = as.factor(variable), # Cambiar un tipo de variable a factor
         variable = sqrt(variable) # transformar con raiz cuadrada
         )

# Ver y marcar posibles outliers

ggplot(datos, aes(x = "variable tratamiento", y = "variable continua")) +
  geom_boxplot() # Esto hace un diagrama de cajitas basico y ves los puntos que quedan fuera de las cajas y lineas.

# Los datos con valores muy extremos y que modifican mucho el analisis, si es necesario, se quitan:
datos[13,8] <- NA # GPX tentacular, n 13
datos[12,8] <- NA
datos[6,8] <- NA




# Ya esta, normalmente toda esta parte del analisis la dejo en un archivo de R aparte. Luego, en otro archivo de R, se puede usar la funcion source(), que ejecuta el archivo de R que le des y en este caso te cargaria los datos directamente. Asi evitas que te salga un archivo muy largo.

# Si no, puedes seguir aqui con el analisis tambien