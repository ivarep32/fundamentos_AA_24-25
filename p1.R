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


# 1
# (2.5 ptos) Se desea predecir la temperatura máxima futura (FTMax), en función de variables
# meteorológicas del último periodo. Ajusta por el metodo de mínimos cuadrados el siguiente
# modelo de regresión múltiple: Y = β0 + β1X1 + β2X2 + β3X3 + β4X4 + β5X5 + β6X6 + ε
# donde: X1 = LWS; X2 = LRadSol; X3 = LLH; X4 = LTMax; X5 = LRHmax y X6 = LTmin

# Compara todos los resultados que obtienes con sus versiones manuales, verifica que coinciden e interpeta los resultados

# a
# Obtén los coeficientes estimados del modelo, así como intervalos de confianza al 95 %
# para los coeficientes del modelo. Calcula los valores ajustados del modelo y calcula la
# suma residual de cuadrados
z <- lm(FTMax ~ LWS + LRadSol + LLH + LTMax + LRHmax + LTmin, data = KorTemp)
summary(z)

coef(z) #coeficientes del modelo

fitted(z) #ajustamos el modelo
residuals(z) #obtenemos los residuos del problema
RSS <- sum(residuals(z)^2)
n <- nrow(KorTemp)
p <- length(coef(KorTemp)) - 1
RSE <- sqrt(RSS / (n - p - 1))
RSE

confint(z) #intervalos de confianza a 95%




# b
# Calcula el valor del coeficiente de correlación de Pearson entre la variable respuesta Y
#y el predictor X1.
cor(KorTemp$FTMax, KorTemp$LWS) #coeficiente de correlacion entre FTMAX(y) y LWS(X1)

# c
#  Define y calcula el coeficiente de correlación parcial entre X1 y X2, controlando por el
# resto de las variables explicativas.


# d
# Considera un modelo reducido que contiene solo un subconjunto de las variables explicativas incluidas en el modelo completo. Formula y contrasta, mediante un test t, la
# hipótesis nula de que los coeficientes asociados a las variables excluidas son iguales a
# cero.

z1 <- lm(FTMax ~ LWS + LRadSol + LLH + LTMax + LRHmax, data = KorTemp) # generamos unn nuevo modelo sin la variable LTmin


# e
# Compara el modelo completo con el modelo reducido mediante un test F. Expón claramente la hipótesis nula, calcula el estadístico de contraste, determina el valor crítico
# correspondiente y extrae las conclusiones en función del resultado del test

# f
# Compara el coeficiente de determinación ajustado para ambos modelos


# 2
# (1.5 ptos) Calcula las componentes principales de las variables del último período disponible.
# Dibuja las puntuaciones de las dos primeras componentes respecto al mes (mes). Interpreta
# las componentes que expliquen conjuntamente más del 75 % de la variabilidad.

# 3
# (1 pto) Calcula un módelo de regresión para explicar (A1=0:FTMax, A1=1:FTmin) con las variables del último período disponible (empiezan por L), con el objetivo de usar el mejor modelo
# con el menor número de covariables. Explica el proceso seguido, las elecciones tomadas y
# los resultados obtenidos.


