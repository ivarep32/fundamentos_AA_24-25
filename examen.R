midni = 1413
set.seed(midni)
A1= midni %% 2;A1
A2= sample(1:25,3);A2
load("KorTemp.RData")
lmues=sample(1:nrow(KorTemp),300)
summary(lmues)
str(KorTemp)
str(lmues)
summary(KorTemp)

# ==================================================================================================================== #

# 1
# (2.5 ptos) Se desea predecir la temperatura máxima futura (FTMax), en función de variables
# meteorológicas del último periodo. Ajusta por el metodo de mínimos cuadrados el siguiente
# modelo de regresión múltiple: Y = β0 + β1X1 + β2X2 + β3X3 + β4X4 + β5X5 + β6X6 + ε
# donde: X1 = LWS; X2 = LRadSol; X3 = LLH; X4 = LTMax; X5 = LRHmax y X6 = LTmin

# Compara todos los resultados que obtienes con sus versiones manuales, verifica que coinciden e interpeta los resultados

# -------------------------------------------------------------------------------------------------------------------- #

# a
# Obtén los coeficientes estimados del modelo, así como intervalos de confianza al 95 %
# para los coeficientes del modelo. Calcula los valores ajustados del modelo y calcula la
# suma residual de cuadrados

z <- lm(FTMax ~ LWS + LRadSol + LLH + LTMax + LRHmax + LTmin, data = KorTemp)

# coeficientes automáticos
betahat <- coef(z); betahat

# coeficientes manuales
X <- model.matrix(z)
n <- nrow(X);n
p <- ncol(X);p

XtXi <- solve(t(X) %*% X)
H <- X %*% XtXi %*% t(X)

y <- KorTemp$FTMax
hbeta <- XtXi %*% t(X) %*% y
hbeta

# suma residual de cuadrados automáticos
residuals(z) #obtenemos los residuos del problema
RSS <- sum(residuals(z)^2); RSS #suma residual de cuadrados

# suma residual de cuadrados manuales
RSS <- t(y - X %*% hbeta) %*% (y - X %*% hbeta);RSS

# intervalos confianza automáticos
confint(z,0.95)

# intervalos confianza manuales
sigma2 <- RSS / (n - p)
sigma2

ET <- sqrt(rep(sigma2, length(diag(XtXi))) * diag(XtXi))
niv <- 0.95
t <- qt(1 - (1 - niv)/2, n - p)

betainf <- betahat - t * ET #extremos inferiores de los intervalos de confianza para los coeficientes
betainf

betasup <- betahat + t * ET #extremos superiores
betasup

# valores ajustados automáticos
fitted(z)

# valores ajustados manuales
yhat <- X %*% betahat; yhat

# -------------------------------------------------------------------------------------------------------------------- #

# b
# Calcula el valor del coeficiente de correlación de Pearson entre la variable respuesta Y
#y el predictor X1.


# -------------------------------------------------------------------------------------------------------------------- #

# c
#  Define y calcula el coeficiente de correlación parcial entre X1 y X2, controlando por el
# resto de las variables explicativas.

# -------------------------------------------------------------------------------------------------------------------- #

# d
# Considera un modelo reducido que contiene solo un subconjunto de las variables explicativas incluidas en el modelo completo. Formula y contrasta, mediante un test t, la
# hipótesis nula de que los coeficientes asociados a las variables excluidas son iguales a
# cero.

# -------------------------------------------------------------------------------------------------------------------- #

# e
# Compara el modelo completo con el modelo reducido mediante un test F. Expón claramente la hipótesis nula, calcula el estadístico de contraste, determina el valor crítico
# correspondiente y extrae las conclusiones en función del resultado del test

# -------------------------------------------------------------------------------------------------------------------- #

# f
# Compara el coeficiente de determinación ajustado para ambos modelos

# ==================================================================================================================== #

# 2
# (1.5 ptos) Calcula las componentes principales de las variables del último período disponible.
# Dibuja las puntuaciones de las dos primeras componentes respecto al mes (mes). Interpreta
# las componentes que expliquen conjuntamente más del 75 % de la variabilidad.

# ==================================================================================================================== #

# 3
# (1 pto) Calcula un módelo de regresión para explicar (A1=0:FTMax, A1=1:FTmin) con las variables del último período disponible (empiezan por L), con el objetivo de usar el mejor modelo
# con el menor número de covariables. Explica el proceso seguido, las elecciones tomadas y
# los resultados obtenidos.
# cargamos las variables relevantes (nombre inicia con "L")

# ==================================================================================================================== #

# 4
# (1 pto) Seleccionando las filas del conjunto de datos dada por lmues: KorTemp[lmues,],
# estimar un modelo de regresión no lineal para explicar (A1=0:FTMax, A1=1:FTmin) usando la
# variable LRadSol. Comenta las opciones elegidas y dibuja el modelo de regresión obtenido
# sobre los datos originales.

# ==================================================================================================================== #

# 5
# (1 pto). Para las estaciones seleccionadas en A2, elaborar dos modelos de clasificación (uno
# basado en Regla de Bayes y el otro en técnicas de regresión) con las variables LTMax, LRHmax,
# LWS y LLH que intente clasificar los datos por estación. Justificar las elecciones tomadas y
# estimar el error de mala clasificación que tendríamos si aplicásemos los modelos a nuevos
# datos.


