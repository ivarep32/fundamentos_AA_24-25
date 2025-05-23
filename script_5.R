#-----------------------------------------------------------
# REGRESIÓN LINEAL GENERAL CON R (USANDO savings)
#-----------------------------------------------------------
#-----------------------------------------------------------
# Sección 1: Ajuste de un modelo de regresión lineal
# Objetivo: Ajustar un modelo de regresión lineal múltiple con R
# sobre la base de datos `savings` del paquete `faraway`.
# El modelo que se propone es:
# Y = β0 + β1*pop15 + β2*pop75 + β3*dpi + β4*ddpi + ε
# donde Y = tasa de ahorro (sr), y el resto son variables explicativas
# socioeconómicas.
#-----------------------------------------------------------
# Cargamos la librería faraway que contiene los datos
library(faraway)
data("savings")
library(ggplot2)
library(GGally)

ggpairs(savings) + theme_bw()

#Ajuste modelo regrsion lineal multiple
modelo <- lm(savings$sr ~ savings$pop15 + savings$pop75 + savings$dpi + savings$ddpi)

summary(modelo)

# El resumen proporciona:
# - Coeficientes estimados (β̂)
# - Error estándar
# - Valor t y p-valor para cada coeficiente
# - R² y RSE (residual standard error)

# Interpretación:
# Por ejemplo, un coeficiente negativo en pop15 indica que a mayor % de
# población joven, menor tasa de ahorro, con significación estadística.

#-----------------------------------------------------------
# Sección 1.1: Estimación de los parámetros del modelo
#-----------------------------------------------------------
beta <- coef(modelo)

#matriz del diseño
X <- model.matrix(z)

# numero de observaciones y parametros
n<-nrow(X)
p <- ncol(X)

#-----------------------------------------------------------
# Sección 1.2: Interpretación geométrica
# En regresión, la predicción se expresa matricialmente como:
# Ŷ = H * Y, donde H = X(X'X)^(-1)X' es la matriz hat.
# Esta proyecta Y en el subespacio generado por las columnas de X.

#-----------------------------------------------------------

XtXi <- solve(t(X) %*% X)
H <- X %*% XtXi %*% t(X)

# Cálculo manual del vector β̂
y <- savings$sr
hbeta <- XtXi %*% t(X) %*% y
hbeta  # debe coincidir con coef(modelo)

# Matriz generadora de residuos: M = I - H
M <- diag(1, n) - H

# Residuos estimados: ε̂ = M * Y
residuos <- M %*% y

# También se pueden obtener con:
residuals(modelo)

#-----------------------------------------------------------
# Sección 2: Estimación de la varianza
# Para estimar la varianza del error σ² se utiliza:
# σ̂² = RSS / (n - p), donde RSS = ∑(ε̂i)² = ε̂'ε̂ = Y'MY
#-----------------------------------------------------------
# Cálculo de RSS (Residual Sum of Squares)
RSS <- t(y - X %*% hbeta) %*% (y - X %*% hbeta)

# Equivalente con función deviance:
deviance(modelo)

# Varianza estimada del error
sigma2 <- RSS / (n - p)
sigma2




