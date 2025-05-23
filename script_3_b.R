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

#--------------------------------------------------------------
# 2. Ajuste de los parámetros de un modelo de regresión
# lineal mediante  métodos  de  optimización
#--------------------------------------------------------------

# 🎲 Simulación de datos para un modelo de regresión lineal simple

# - Simulamos un conjunto de datos para observar cómo funciona el ajuste mediante
# descenso de gradiente.
# - El modelo es: y = β0 + β1x + ε, donde:
#   - x es generado de forma aleatoria (distribución uniforme)
#   - ε es un término de error aleatorio con distribución normal
#   - β0 y β1 son los parámetros reales del modelo

# - Estos datos simulan una situación en la que conocemos los parámetros reales,
#   lo que nos permite evaluar la precisión del ajuste manual.

set.seed(123)  # Fijamos semilla para reproducibilidad
n <- 100
x <- runif(n, min = 0, max = 5)
beta0 <- 2
beta1 <- 5
epsilon <- rnorm(n, sd = 1)
y <- beta0 + beta1 * x + epsilon

# Visualizamos los datos simulados
plot(x, y, main = "Datos simulados para regresion lineal simple", xlab = "x", ylab = "y")

# ✅ Ajuste del modelo mediante mínimos cuadrados con lm()

# - Esta es la solución exacta calculada por R utilizando álgebra matricial.
# - Nos servirá como referencia para comparar con los métodos iterativos.

modelo_ref <- lm(y ~ x)
coef(modelo_ref)

#---------------------------------------------------------------------------------------
# implementacion del metodo de descenso del gradiente
# - Este metodo consiste en actualizar iterativamente los parámetros β0 y β1
#   para minimizar la función de coste J(β0, β1), basada en el error cuadrático medio.

# - Fórmulas:
#   β0 ← β0 + t * ∑(yᵢ - (β0 + β1 * xᵢ))
#   β1 ← β1 + t * ∑(xᵢ * (yᵢ - (β0 + β1 * xᵢ)))

# - Donde:
#   - t es la tasa de aprendizaje (learning rate)
#   - El proceso se repite un número fijo de iteraciones o hasta converger

# - Es importante elegir bien la tasa de aprendizaje:
#   - Si es muy grande, el algoritmo puede divergir.
#   - Si es muy pequeña, la convergencia puede ser muy lenta.

#---------------------------------------------------------------------------------------

# Inicialización de parámetros
b0 <- 0
b1 <- 0
t <- 0.001       # Tasa de aprendizaje
iter <- 1000     # Número de iteraciones

# Iteraciones del descenso de gradiente (batch)
for (i in 1:iter) {
  y_pred <- b0 + b1 * x
  error <- y - y_pred
  b0 <- b0 + t * sum(error)
  b1 <- b1 + t * sum(x * error)
}

# Coeficientes estimados por el metodo batch
c(b0, b1)

# 🧾 Comparación con el modelo de referencia ajustado con lm()

# - Si el metodo converge correctamente, los coeficientes estimados por gradiente
#   deben estar muy cerca de los obtenidos con lm().
coef(modelo_ref)
#podemos ver que los valores coinciden, lo hemos implementado bien

#------------------------------------------------------------------------------------------------------------
#2.2. Metodo  de  descenso  de  gradiente  estocástico
# ⚙️ Descenso de gradiente estocástico (SGD)

# - A diferencia del batch, el SGD actualiza los parámetros usando **una sola observación a la vez**.
# - Esto lo hace mucho más eficiente en datasets grandes, ya que no necesita calcular la suma total
#   de todos los errores en cada iteración.

# - Algoritmo:
#   Para cada observación i:
#     β0 ← β0 + t * (yᵢ - (β0 + β1 * xᵢ))
#     β1 ← β1 + t * xᵢ * (yᵢ - (β0 + β1 * xᵢ))

# - Aunque SGD es más ruidoso (las actualizaciones varían más), suele converger más rápido en la práctica.
#------------------------------------------------------------------------------------------------------------
# Inicialización
b0 <- 0
b1 <- 0
t <- 0.001
epochs <- 10  # Número de veces que recorremos el dataset entero

# Iteraciones de SGD
for (e in 1:epochs) {
  for (i in 1:n) {
    error_i <- y[i] - (b0 + b1 * x[i])
    b0 <- b0 + t * error_i
    b1 <- b1 + t * x[i] * error_i
  }
}

# Coeficientes estimados por SGD
c(b0, b1)

# Comparar los coeficientes:
coef(modelo_ref)  # Referencia
c(b0, b1)          # SGD estimado

# vemos una vez mas que coinciden, hemos implementado bien
#----------------------------------------------------------------------
# 🔍 Comparación final de métodos:

# - lm()     → solución exacta por mínimos cuadrados
# - batch    → solución iterativa global, lenta pero precisa
# - SGD      → solución iterativa rápida, eficiente en datos grandes
#----------------------------------------------------------------------

