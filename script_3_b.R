#--------------------------------------------------------
# Ajuste  de  un  modelo  de  regresión  lineal  con  R
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

# Vemos el resumen del modelo
summary(z)
# 🔍 Este resumen incluye:
# - Coeficientes estimados para cada variable
# - Contrastes t individuales (para ver si los coeficientes son significativos)
# - Error estándar residual (RSE)
# - Coeficiente de determinación R^2 y su versión ajustada

