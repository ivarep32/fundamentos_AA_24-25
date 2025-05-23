#-----------------------------------------------------------
# REGRESIÓN LINEAL GENERAL CON R (USANDO savings)
#-----------------------------------------------------------
#-----------------------------------------------------------
# 1. Cargar datos y ajuste inicial del modelo
#-----------------------------------------------------------
# Cargamos la librería faraway que contiene los datos
library(faraway)
data("savings")
library(ggplot2)
library(GGally)

ggpairs(savings) + theme_bw()

#Ajuste modelo regrsion lineal multiple
z <- lm(savings$sr ~ savings$pop15 + savings$pop75 + savings$dpi + savings$ddpi)

summary(z)

#-----------------------------------------------------------
# 2. Estimación de los parametros del modelo
#-----------------------------------------------------------
beta <- coef(z)

#matriz del diseño
X <- model.matrix(z)

# numero de observaciones y parametros
n<-nrow(X)
p <- ncol(X)

#-----------------------------------------------------------
# 3. Interpretación geométrica: matriz hat y predicciones
#-----------------------------------------------------------
#matriz hat H = X(X'X)^(-1)X'

XtXi <- solve(t(X) %*% X)
H <- X %*% XtXi %*% t(X)

#predicciones y_hat= H * Y

Y <- savings$sr
hbeta <- XtXi %*% t(X) %*% Y

# verificamos que coincide con coef(z)
hbeta

