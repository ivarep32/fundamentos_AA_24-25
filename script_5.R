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

#-----------------------------------------------------------
# SECCIÓN 3: Regresión particionada y parcial (Greene.txt)
# Datos de ejemplo manuales como en el PDF
#-----------------------------------------------------------
# Leer el archivo Greene.txt con separador de tabulación y comas como decimales
datos <- read.table("Greene.txt", header = TRUE, sep = "\t", dec = ",")

# Verificamos
str(datos)
datos_real <- datos
names(datos_real) <- c("Anio", "PNB", "IN", "IPC", "TI")

# Convertimos años en índice de 1 a 15
datos_real$Anio <- 1:15

# Convertimos las variables PNB e IN a dólares constantes del año 1972,
# dividiendo por el IPC y multiplicando por 100.
# Esto elimina el efecto del aumento de precios en los valores monetarios.
# Ajustamos PNB e IN a valores constantes usando IPC
datos_real$PNB <- (datos$PNB / datos$IPC) * 100
datos_real$IN  <- (datos$IN / datos$IPC) * 100

# Usamos la serie IPC para calcular la tasa de inflación año a año.
# Para 1968 usamos un IPC externo (79.06 en 1967), luego usamos cocientes sucesivos.
# La tasa de inflación se define como (IPC_t / IPC_{t-1}) - 1.
datos_real$IPC[1] <- (datos$IPC[1] / 79.06 - 1) * 100  # Valor base para 1968
datos_real$IPC[2:15] <- (datos$IPC[2:15] / datos$IPC[1:14] - 1) * 100

# Se ajusta un modelo de regresión simple para estudiar la relación
# entre la inversión (IN) y el año (Anio).
# Este modelo no tiene en cuenta otras variables posibles (como PNB, IPC o TI),
# por lo tanto, puede estar sesgado por omisión de variables relevantes.
m1 <- lm(IN ~ Anio, datos_real)
summary(m1)

# Ahora ajustamos un modelo múltiple que incluye como variables explicativas:
# - Anio (año)
# - PNB ajustado por inflación
# - IPC (tasa de inflación)
# - TI (tipo de interés)
#
# Este modelo controla por otras influencias sobre la inversión, lo que permite
# interpretar el efecto "neto" o "parcial" de cada variable.
# Aquí, el coeficiente del año podría cambiar de signo si hay confusión con PNB.
m2 <- lm(IN ~ Anio + PNB + IPC + TI, datos_real)
summary(m2)

# Separamos el efecto del año sobre la inversión, controlando por las demás variables.
# Esto se hace en tres pasos:
#
# Paso 1: Ajustar la inversión (IN) con PNB, IPC y TI --> obtener residuos r_IN
# Paso 2: Ajustar el año (Anio) con PNB, IPC y TI --> obtener residuos r_Anio
# Paso 3: Regresar r_IN sobre r_Anio. La pendiente es el efecto parcial del año.
#
# Esta técnica demuestra que en regresión múltiple, el coeficiente de una variable
# se puede interpretar como su efecto "añadido" después de haber descontado los efectos de otras.
#-----------------------------------------------------------

r_IN <- residuals(lm(IN ~ PNB + IPC + TI, datos_real))
r_Anio <- residuals(lm(Anio ~ PNB + IPC + TI, datos_real))

modelo_parcial <- lm(r_IN ~ r_Anio)
summary(modelo_parcial)

# La pendiente de esta regresión coincide con el coeficiente de Anio en m2

m0 <- lm(IN ~ Anio - 1, datos_real)
summary(m0)
