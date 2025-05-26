midni = 1413
set.seed(midni)
A1= midni %% 2
A2= sample(1:25,3)
load("KorTemp.RData")
lmues=sample(1:nrow(KorTemp),300)
summary(lmues)
str(KorTemp)
str(lmues)
summary(KorTemp)

# 3
# (1 pto) Calcula un módelo de regresión para explicar (A1=0:FTMax, A1=1:FTmin) con las variables del último período disponible (empiezan por L), con el objetivo de usar el mejor modelo
# con el menor número de covariables. Explica el proceso seguido, las elecciones tomadas y
# los resultados obtenidos.

# 4
# (1 pto) Seleccionando las filas del conjunto de datos dada por lmues: KorTemp[lmues,],
# estimar un modelo de regresión no lineal para explicar (A1=0:FTMax, A1=1:FTmin) usando la
# variable LRadSol. Comenta las opciones elegidas y dibuja el modelo de regresión obtenido
# sobre los datos originales.
datos_lmues <- KorTemp[lmues, ]


# 5
# (1 pto). Para las estaciones seleccionadas en A2, elaborar dos modelos de clasificación (uno
# basado en Regla de Bayes y el otro en técnicas de regresión) con las variables LTMax, LRHmax,
# LWS y LLH que intente clasificar los datos por estación. Justificar las elecciones tomadas y
# estimar el error de mala clasificación que tendríamos si aplicásemos los modelos a nuevos
# datos.