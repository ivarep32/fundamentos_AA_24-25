#--------------------------------------------------------
# 1. Ajuste  de  un  modelo  de  regresión  lineal  con  R
#--------------------------------------------------------
# Cargamos el dataset de publicidad
Advertising <- read.csv("Advertising.csv")

# Vista previa de los datos
head(Advertising)

# Representación gráfica de las variables predictoras frente a la variable respuesta (Sales)
pairs(Advertising)

# 📈 Interpretación del gráfico pairs():
# La función pairs() genera una matriz de gráficos de dispersión (scatterplots) para cada
# combinación de variables del dataset.

# - Cada celda muestra la relación entre dos variables distintas.
# - Los puntos alineados en una diagonal creciente indican correlación positiva.
# - Los puntos alineados en una diagonal descendente indican correlación negativa.
# - Si los puntos están dispersos sin forma clara, las variables están poco o nada correlacionadas.
# - La diagonal principal de la matriz suele estar vacía o mostrar los nombres de las variables.

# Este tipo de gráfico es útil para:
# - Detectar relaciones lineales entre variables.
# - Identificar posibles multicolinealidades entre predictores.
# - Observar patrones, agrupaciones o valores atípicos.

# Ajustamos el modelo lineal múltiple con TV, Radio y Newspaper como predictores
z <- lm(Sales ~ TV + Radio + Newspaper, data = Advertising)
class(z)
names(z)
# Vemos el resumen del modelo
summary(z)
# 🔍 Este resumen incluye:
# - Coeficientes estimados para cada variable
# - Contrastes t individuales (para ver si los coeficientes son significativos)
# - Error estándar residual (RSE)
# - Coeficiente de determinación R^2 y su versión ajustada


# ------------------------------------------------
#1.1 Estimación  de  los  parámetros  del  modelo
# 📌 Coeficientes, valores ajustados y residuos

# - coef(z): devuelve los coeficientes estimados del modelo, es decir, los valores β̂
#   que mejor ajustan los datos bajo el supuesto de mínimos cuadrados.
#   Incluye el intercepto (β0) y los coeficientes para cada variable predictora.

# - fitted(z): devuelve los valores ajustados por el modelo, ŷ = β̂0 + β̂1x1 + ... + β̂pXp,
#   es decir, las predicciones hechas para los datos observados.

# - residuals(z): calcula los residuos ε̂i = yi - ŷi.
#   Muestran la diferencia entre el valor real y el valor predicho por el modelo para cada observación.
#   Sirven para evaluar qué tan bien se ajusta el modelo a los datos.

# - El RSS (Residual Sum of Squares) se obtiene al sumar los residuos al cuadrado.
#   A partir de este se calcula el RSE (Residual Standard Error), que representa una estimación
#   de la desviación estándar del término de error ε.

# - Una menor RSE indica mejor ajuste, aunque debe interpretarse en el contexto de la escala de Y.

# ------------------------------------------------
# Coeficientes estimados
coef(z)

# Valores ajustados por el modelo (ŷ)
fitted(z)

# Residuos del modelo (ε̂ = y - ŷ)
residuals(z)

# Verificación del cálculo del error estándar residual manualmente
RSS <- sum(residuals(z)^2)
n <- nrow(Advertising)
p <- length(coef(z)) - 1
RSE <- sqrt(RSS / (n - p - 1))
RSE

# Intervalos de confianza al 90% para los coeficientes
confint(z, level = 0.9)

#-------------------------------------------------------
#1.2 Contrastes  sobre  los  parámetros  del  modelo
#-------------------------------------------------------
# El p-valor del F-test (resumen del modelo) indica si al menos una variable es útil.
# La variable Newspaper no es significativa (p ≈ 0.86), así que ajustamos un nuevo modelo:
z2 <- lm(Sales ~ TV + Radio, data = Advertising)

# Comparamos los R^2
summary(z2)$r.squared      # R^2 sin Newspaper
summary(z)$r.squared       # R^2 con Newspaper
# ✅ Conclusión:
# El modelo sin Newspaper tiene prácticamente el mismo R^2, por lo tanto Newspaper no aporta
# valor explicativo y puede ser eliminado.


# #---------------------------------------------------------------------------------------------
# 1.3 Prediccion
# - Una vez ajustado el modelo, podemos usarlo para predecir el valor de la variable respuesta
#   (Sales) dados nuevos valores de las variables predictoras (TV y Radio).

# - La función predict() permite hacer:
#   a) predicciones puntuales (valor estimado),
#   b) intervalos de confianza para la media de la predicción,
#   c) intervalos de predicción para una nueva observación.

# - predict(z2, newdata): devuelve la predicción puntual ŷ para los valores en newdata.

# - predict(z2, newdata, interval = "confidence"):
#   devuelve un intervalo que contiene, con determinada probabilidad (por defecto 95%),
#   la media de las respuestas para esas condiciones.

# - predict(z2, newdata, interval = "predict"):
#   devuelve un intervalo más amplio que contiene una posible nueva observación de Y
#   para esas condiciones, teniendo en cuenta la variabilidad del error.

#---------------------------------------------------------------------------------------------
# Supongamos que un mercado invierte $100,000 en TV y $20,000 en Radio.

# Creamos el data.frame con estos valores y usamos predict():
newdata <- data.frame(TV = 100, Radio = 20)

# Predicción puntual
predict(z2, newdata)

# Intervalo de confianza (estimación del valor medio esperado)
predict(z2, newdata, interval = "confidence")

# Intervalo de predicción (valor observado nuevo con error)
predict(z2, newdata, interval = "predict")