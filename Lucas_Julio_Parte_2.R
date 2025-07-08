#importamos y observamos los datos
csv <- read.csv("w2011.csv", sep = ";", dec=',', header = TRUE)
summary(csv)
str(csv)
head(csv)

# 3. Estima un modelo de regresión no paramétrico para explicar lCO2 con la variable internet
# seleccionando el parámetro ventana de forma óptima. Dibuja el modelo de regresión obtenido. A la vista de la gráfica, ¿podemos suponer que existe una relación lineal entre ambas
# variables? (1 pto).

# ==================================================================================================================== #

# 4. Estimar un modelo lineal para explicar lCO2 en función del resto de variables numéricas con
# el objetivo de usar el menor número de covariables. (1 pto).

# ==================================================================================================================== #

# 5. Elaborar dos modelos de clasificación (uno basado en Bayes y otro en métodos de regresión)
# para clasificar el continente (Region) en función de las variables numéricas. Para estos modelos
# excluye los países de América (Region==AM) y Oceanía (Region==OC). Comenta
# sobre los países europeos que se clasificarían mal y aplica el modelo de clasificación para
# los países que NO han entrado en el modelo (América y Oceanía) (1 pto).


