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

#-----------------------------------------------------------
# SECCIÓN 4 - Coeficientes de correlación simple, múltiple y parcial
#-----------------------------------------------------------
# Calculamos la matriz de correlación simple entre todas las variables del dataset.
# La correlación simple entre dos variables mide la intensidad y dirección de la relación lineal.
# El valor está en [-1, 1], y su signo coincide con el del coeficiente de regresión simple.
#-----------------------------------------------------------

correlaciones_simples <- cor(datos_real)
print(correlaciones_simples)

# Podemos extraer la fila o columna correspondiente a IN (Inversión)
correlaciones_IN <- correlaciones_simples["IN", ]
print(correlaciones_IN)

#-----------------------------------------------------------
#Correlación múltiple
#-----------------------------------------------------------
# El coeficiente de correlación múltiple entre una variable respuesta (IN)
# y un conjunto de variables explicativas (Anio, PNB, IPC, TI) se define como:
#
# Corr(IN, Ŷ), donde Ŷ = predicciones del modelo de regresión múltiple.
# Su cuadrado coincide con el R² del modelo de regresión múltiple.
#Comentario: Esta medida nos dice qué tan bien se ajustan las predicciones
# del modelo a la inversión observada. Cuanto más cercana a 1, mejor
# explicación conjunta.
#-----------------------------------------------------------

correlacion_multiple <- cor(datos_real$IN, fitted(m2))
print(correlacion_multiple)

# Verifica que el cuadrado de este valor es igual a Multiple R-squared:
summary(m2)$r.squared  # Debe coincidir con correlacion_multiple^2

#-----------------------------------------------------------
# Correlación parcial
#-----------------------------------------------------------
# Calculamos la correlación entre los residuos de IN y cada variable,
# después de eliminar el efecto de las demás. Esto nos da la influencia neta.
#-----------------------------------------------------------

# Residuos de IN y cada variable, controlando por las otras
r_IN_Anio <- residuals(lm(IN ~ PNB + IPC + TI, data = datos_real))
r_Anio    <- residuals(lm(Anio ~ PNB + IPC + TI, data = datos_real))

r_IN_PNB  <- residuals(lm(IN ~ Anio + IPC + TI, data = datos_real))
r_PNB     <- residuals(lm(PNB ~ Anio + IPC + TI, data = datos_real))

r_IN_IPC  <- residuals(lm(IN ~ Anio + PNB + TI, data = datos_real))
r_IPC     <- residuals(lm(IPC ~ Anio + PNB + TI, data = datos_real))

r_IN_TI   <- residuals(lm(IN ~ Anio + PNB + IPC, data = datos_real))
r_TI      <- residuals(lm(TI ~ Anio + PNB + IPC, data = datos_real))

# Correlaciones parciales entre residuos
correlaciones_parciales <- c(
  Anio = cor(r_IN_Anio, r_Anio),
  PNB  = cor(r_IN_PNB,  r_PNB),
  IPC  = cor(r_IN_IPC,  r_IPC),
  TI   = cor(r_IN_TI,   r_TI)
)
# Resultado esperado: El valor de la correlación parcial entre IN y
# Anio debe ser negativo (~ -0.94), a pesar de que su correlación
# simple era positiva. Esto evidencia un cambio de signo típico del
# fenómeno de confusión

#-----------------------------------------------------------
# Tabla resumen: Correlaciones simples vs. parciales
#-----------------------------------------------------------
# Esta tabla permite comparar la relación directa y la neta
# de cada variable con la inversión (IN). El cambio de signo
# indica un posible efecto de confusión entre variables.
#-----------------------------------------------------------

tabla <- data.frame(
  Correlacion_Simple  = correlaciones_IN[c("Anio", "PNB", "IPC", "TI")],
  Correlacion_Parcial = correlaciones_parciales
)

print(round(tabla, 4))

#-----------------------------------------------------------
#Contexto teórico
#-----------------------------------------------------------
# En regresión lineal múltiple, se desea construir intervalos de confianza
# para cada parámetro β̂ᵢ del modelo:
#
#   ( β̂ᵢ ± t_{n-p, α/2} · σ̂ · sqrt[(X'X)^(-1)ᵢᵢ] )
#
# Donde:
# - β̂ᵢ: estimación del parámetro
# - t_{n-p, α/2}: cuantil de la t de Student con (n - p) grados de libertad
# - σ̂²: varianza residual estimada
# - (X'X)^(-1): inversa de la matriz de diseño
#-----------------------------------------------------------
#-----------------------------------------------------------
# Estimación de intervalos de confianza al 95% para los coeficientes
# según el modelo ajustado sobre los datos "savings"
#-----------------------------------------------------------

# Recordamos que z es el modelo ajustado:
# z <- lm(sr ~ pop15 + pop75 + dpi + ddpi, data = savings)

# Extraemos la matriz del diseño del modelo
X <- model.matrix(z)
n <- nrow(X)
p <- ncol(X)

# Obtenemos la inversa de X'X
XtXi <- solve(t(X) %*% X)

# Estimación puntual de los coeficientes
beta <- coef(z)

# Varianza residual (sigma^2)
y <- savings$sr
RSS <- t(y - X %*% beta) %*% (y - X %*% beta)
sigma2 <- RSS / (n - p)

#-----------------------------------------------------------
# Cálculo de errores típicos de los coeficientes
#-----------------------------------------------------------
ET <- sqrt(rep(sigma2, length(diag(XtXi))) * diag(XtXi))

#-----------------------------------------------------------
# Cuantil t para nivel de confianza 95%
#-----------------------------------------------------------
niv <- 0.95
t <- qt(1 - (1 - niv)/2, n - p)

#-----------------------------------------------------------
# Cálculo de los intervalos de confianza
#-----------------------------------------------------------

#--- Extremos inferiores de los intervalos de confianza
betainf <- beta - t * ET

#--- Extremos superiores de los intervalos de confianza
betasup <- beta + t * ET

# Mostramos los resultados
betainf
betasup
#-----------------------------------------------------------
# Verificación con confint()
#-----------------------------------------------------------
confint(z, level = niv)
