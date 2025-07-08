#importamos y observamos los datos
csv <- read.csv("w2011.csv", sep = ";", dec=',', header = TRUE)
summary(csv)
str(csv)
head(csv)


# 1. Se desea predecir las emisiones de emisiones CO2 per cápita (lCO2), en función del resto
# de variables. Ajusta por el metodo de mínimos cuadrados el siguiente modelo de regresión
# múltiple: Y = β0 +β1X1 +β2X2 +β3X3 +β4X4 +β5X5 +β6X6 +ε donde: X1 = inflation;
# X2 = lGDPc; X3 = GDP.growth; X4 = internet y X5 = lagv

# a) Obtén los coeficientes estimados del modelo, así como intervalos de confianza al 95 %
# para los coeficientes del modelo. Calcula los valores ajustados del modelo y calcula la
# suma residual de cuadrados.

z <- lm(lCO2 ~ inflation + lGDPc + GDP.growth + internet + lagv, data = csv)

# coeficientes automáticos
betahat <- coef(z); betahat

# coeficientes manuales
X <- model.matrix(z)
n <- nrow(X);n
p <- ncol(X);p

XtXi <- solve(t(X) %*% X)
H <- X %*% XtXi %*% t(X)

y <- csv$lCO2
hbeta <- XtXi %*% t(X) %*% y
hbeta

# suma residual de cuadrados automáticos
residuals(z) #obtenemos los residuos del problema
RSS <- sum(residuals(z)^2); RSS #suma residual de cuadrados

# suma residual de cuadrados manuales
RSS <- t(y - X %*% hbeta) %*% (y - X %*% hbeta);RSS

# intervalos confianza automáticos
confint(z)

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

# b) Calcula el valor del coeficiente de correlación de Pearson entre la variable respuesta Y
# y el predictor X1.

cor(csv$lCO2, csv$inflation) #coeficiente de correlacion entre lCO2(y) y inflation(X1)

# -------------------------------------------------------------------------------------------------------------------- #

# c) Define y calcula el coeficiente de correlación parcial entre X1 y X2, controlando por el
# resto de las variables explicativas.



# -------------------------------------------------------------------------------------------------------------------- #

# d) Considera un modelo reducido que contiene solo un subconjunto de las variables explicativas
# incluidas en el modelo completo. Formula y contrasta, mediante un test t, la
# hipótesis nula de que los coeficientes asociados a las variables excluidas son iguales a
# cero.

# -------------------------------------------------------------------------------------------------------------------- #

# e) Compara el modelo completo con el modelo reducido mediante un test F.
# Expón claramente la hipótesis nula, calcula el estadístico de contraste, determina el valor crítico
# correspondiente y extrae las conclusiones en función del resultado del test.

# -------------------------------------------------------------------------------------------------------------------- #

# f) Compara el coeficiente de determinación ajustado para ambos modelos.
# Compara todos los resultados que obtienes con sus versiones manuales, verifica que coinciden e interpeta los resultados (2.5 ptos).

# ==================================================================================================================== #

# 2. Calcula las componentes principales de las variables numéricas (excepto country y Region)
# e interpreta aquellas que conjuntamente expliquen al menos un 90 % de la variabilidad total.
# Dibuja las dos primeras componentes principales en función del continente (Region) (1.5
# ptos).