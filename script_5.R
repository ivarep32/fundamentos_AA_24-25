#-----------------------------------------------------------
# REGRESIÓN LINEAL GENERAL CON R (USANDO savings)
#-----------------------------------------------------------
#-----------------------------------------------------------
# 1. Cargar datos y ajuste inicial del modelo
#-----------------------------------------------------------
# Cargamos la librería faraway que contiene los datos
library(faraway)
data("savings")

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