library(MASS)
#==============================================================
#Pregunta 1: Análisis Exploratorio de Datos y Estadística Descriptiva
#==============================================================
#----------------------------------------------------
#a) Carga el conjunto de datos Titanic. Calcula y muestra
# la tabla de frecuencias para la variable clase
#----------------------------------------------------------
datos <- read.table("Titanic.txt", sep="", header=TRUE)
freq_clase <- table(datos$clase)
barplot(freq_clase, main = "Frecuencia por Clase", xlab = "Clase", ylab = "Frecuencia")
pie(freq_clase, main = "Distribución por Clase")

#---------------------------------------------------
#b) Repite el apartado anterior para el resto de las
# variables en el conjunto de datos Titanic.
#---------------------------------------------------
names(datos)
freq_sexo <- table(datos$sexo)
barplot(freq_sexo, main = "Frecuencia por Sexo", xlab = "Sexo", ylab = "Frecuencia")
pie(freq_sexo, main = "Distribución por Sexo")

freq_edad <- table(datos$edad)
barplot(freq_edad, main = "Frecuencia por Edad", xlab = "Edad", ylab = "Frecuencia")
pie(freq_edad, main = "Distribución por Edad")

freq_super <- table(datos$superviviente)
barplot(freq_super, main = "Frecuencia por Supervivientes", xlab = "Superviviente", ylab = "Frecuencia")
pie(freq_super, main = "Distribución por Superviviente")

#---------------------------------------------------------
# c) Calcula el número de supervivientes en función de la clase
# y representa gráficamente el porcentaje de supervivientes
# para cada clase. ¿Se observan diferencias significativas?
#---------------------------------------------------------

# Tabla de frecuencia absoluta: filas = clase, columnas = superviviente
library(ggplot2)

# Asegurarse de que las variables sean factores
datos$clase <- factor(datos$clase, levels = c("primera", "segunda", "tercera", "tripulación"))
datos$superviviente <- factor(datos$superviviente, levels = c("no", "si"))

# Crear un gráfico de barras apiladas proporcionales
ggplot(datos, aes(x = clase, fill = superviviente)) +
  geom_bar(position = "fill") +  # "fill" hace que la altura total sea 1 (100%)
  scale_y_continuous(labels = scales::percent_format()) +
  scale_fill_manual(values = c("red", "green"), labels = c("No sobrevivio", "Sobrevivio")) +
  labs(
    title = "Porcentaje de supervivientes por clase",
    x = "Clase",
    y = "Porcentaje",
    fill = "Supervivencia"
  ) +
  theme_minimal(base_size = 14)

#=======================================
#Pregunta 2: Análisis de Componentes Principales (ACP)
#=======================================
#-------------------------------------------
#a) Carga el conjunto de datos aspirantes.txt.
# Realiza un Análisis de Componentes Principales (ACP)
# utilizando la función princomp()
#--------------------------------------------
datos <- read.table("Aspirantes.txt", sep="", header=TRUE)
names(datos)
test.pca <- princomp(datos)
summary(test.pca)
# vemos en la ultima fila que la variable que mas aporta es la primera
names(test.pca)
#-------------------------------------------
#b) Muestra las desviaciones típicas de las componentes principales,
# que corresponden a las raíces cuadradas de los autovalores de la matriz de covarianzas
#--------------------------------------------
test.pca$sdev        # Desviaciones estándar
test.pca$sdev^2      # Varianza (autovalores)
#La desviación estándar (sdev) indica cuánta varianza explica cada componente principal.
# Al elevarla al cuadrado (sdev^2), obtenemos la varianza explicada por cada componente.
#En este caso, la Componente 1 explica la mayor parte de la varianza (≈ 812 unidades), y las siguientes mucho menos.
#Esto sugiere que con las primeras 2 o 3 componentes se puede conservar la mayor parte de la información del conjunto de datos

#-------------------------------------------
#c) Explica cómo determinar el número de componentes a retener,
# mencionando criterios como la varianza explicada acumulada
# y el gráfico de sedimentación (scree plot)
#--------------------------------------------
# ✅ 1. **Varianza explicada acumulada**:
# - Se observa qué porcentaje de la varianza total explican las primeras componentes.
# - Generalmente se busca conservar un porcentaje alto (ej. 80% o 90%).
# - Se consulta el resultado de: summary(test.pca)
#     Ejemplo: Si las 2 primeras componentes explican >90%, se pueden conservar solo esas.

# ✅ 2. **Gráfico de sedimentación (scree plot)**:
# - Representa los autovalores (varianza explicada) en orden decreciente.
# - Se busca el "codo" del gráfico: el punto donde la ganancia adicional de varianza empieza a ser mínima.
# - Es decir, donde la curva empieza a aplanarse.

# Ejemplo de uso:
screeplot(test.pca)                  # Diagrama de barras
screeplot(test.pca, type = "lines") # Diagrama con líneas (más útil para ver el "codo")

#------------------------------------------------------------
# 🎯 Conclusión:
# Se recomienda conservar las componentes hasta el punto donde la varianza acumulada sea suficiente
# (ej. ≥ 90%) o donde se identifica el "codo" en el scree plot. Generalmente esto ocurre con las
# primeras 2 o 3 componentes.

#-------------------------------------------
#d) ¿Cómo afecta el cambio de escala de las variables originales
# a los resultados del ACP? ¿Qué soluciones se proponen para mitigar este problema?
#--------------------------------------------
# El cambio de escala de las variables originales afecta los resultados
# del Análisis de Componentes Principales (ACP), ya que este método es
# sensible a las diferencias en la escala debido a que se basa en la varianza.

# Si una variable tiene una escala mucho mayor que otra, tendrá también
# una varianza mayor y, en consecuencia, dominará las primeras componentes
# principales. Esto puede distorsionar la interpretación del análisis y
# dar más peso a ciertas variables solo por su escala, no por su relevancia real.

#------------------------------------------------------------
# 🛠️ Soluciones recomendadas:
#------------------------------------------------------------

# ✅ Estandarización de los datos:
# Transformar las variables para que tengan media cero y desviación estándar uno.
# Esto se hace con la función scale() en R.
# Así, todas las variables parten con la misma importancia en el análisis,
# independientemente de su escala original.

# ✅ Utilizar la matriz de correlaciones:
# En lugar de usar la matriz de covarianzas (que conserva las escalas originales),
# se puede utilizar la matriz de correlaciones, que se calcula a partir de
# variables estandarizadas. Esto tiene el mismo efecto que estandarizar directamente
# los datos antes del ACP.

#------------------------------------------------------------
# 🎯 Conclusión:
# Antes de realizar un ACP, es altamente recomendable estandarizar los datos.
# Esto evita que las variables con mayor escala dominen el análisis,
# y asegura que todas contribuyan de forma equilibrada a la construcción
# de las componentes principales.

#=======================================
#Pregunta 3: Clasificación y Modelos Lineales Discriminantes
#=======================================
#-------------------------------------------
#a) Genera datos simulados para dos clases 
# (puedes usar los escenarios descritos en el documento 
# "Fundamentos de (1).pdf", como el 
# Escenario 1: Normal con medias diferentes en cada clase)
#--------------------------------------------
# Generar datos para dos clases normales con medias diferentes
set.seed(123) # Para reproducibilidad

# Parámetros
n1 <- 100 # Tamaño muestra clase 1
n2 <- 100 # Tamaño muestra clase 2
mu1 <- c(0, 0) # Media clase 1
mu2 <- c(2, 2) # Media clase 2
sigma <- matrix(c(1, 0, 0, 1), 2, 2) # Matriz covarianza común

# Generar muestras
x1 <- mvrnorm(n1, mu1, sigma)
x2 <- mvrnorm(n2, mu2, sigma)

# Crear etiquetas de clase
y1 <- rep(1, n1)
y2 <- rep(2, n2)

# Combinar datos
X <- rbind(x1, x2)
y <- c(y1, y2)

# Visualizar
plot(X, col = y, pch = 19, main = "Datos simulados")
legend("topright", legend = c("Clase 1", "Clase 2"), col = 1:2, pch = 19)

#-------------------------------------------
#b) Ajusta modelos de Análisis Discriminante Lineal (LDA),
# Análisis Discriminante Cuadrático (QDA) y
# regresión logística (GLM) a los datos simulados
#--------------------------------------------
# 1. Ajuste del modelo LDA (Análisis Discriminante Lineal)
modelo_lda <- lda(y ~ X, data = data.frame(X = X, y = as.factor(y)))

# 2. Ajuste del modelo QDA (Análisis Discriminante Cuadrático)
modelo_qda <- qda(y ~ X, data = data.frame(X = X, y = as.factor(y)))

# 3. Ajuste del modelo GLM (Regresión logística binaria)
# Nota: usamos as.factor(y) para que R sepa que es una variable categórica
modelo_glm <- glm(as.factor(y) ~ X, data = data.frame(X = X, y = y), family = binomial)

# Predicción con LDA
pred_lda <- predict(modelo_lda)$class
acierto_lda <- mean(pred_lda == as.factor(y))

# Predicción con QDA
pred_qda <- predict(modelo_qda)$class
acierto_qda <- mean(pred_qda == as.factor(y))

# Predicción con GLM (umbral de 0.5)
prob_glm <- predict(modelo_glm, type = "response")
pred_glm <- ifelse(prob_glm > 0.5, 2, 1)
acierto_glm <- mean(pred_glm == y)
cat("Tasa de acierto (LDA):", round(acierto_lda * 100, 2), "%\n")
cat("Tasa de acierto (QDA):", round(acierto_qda * 100, 2), "%\n")
cat("Tasa de acierto (GLM):", round(acierto_glm * 100, 2), "%\n")

#------------------------------------------------------------
# Comentario:
# Como los datos fueron generados con la misma matriz de covarianza para ambas clases,
# el modelo LDA suele tener un rendimiento similar o incluso mejor que QDA.
# La regresión logística también puede funcionar bien si la separación es lineal.
#------------------------------------------------------------

#------------------------------------------------------------
# c) Crear tablas de confusión para evaluar el rendimiento de cada modelo
#------------------------------------------------------------

# Aseguramos que las etiquetas y predicciones son factores con los mismos niveles
y_factor <- factor(y, levels = c(1, 2))
pred_lda_factor <- factor(pred_lda, levels = c(1, 2))
pred_qda_factor <- factor(pred_qda, levels = c(1, 2))
pred_glm_factor <- factor(pred_glm, levels = c(1, 2))

# Mostrar tablas de confusión usando table()
cat("Tabla de confusión para LDA:\n")
print(table(real = y_factor, prediccion = pred_lda_factor))

cat("\nTabla de confusión para QDA:\n")
print(table(real = y_factor, prediccion = pred_qda_factor))

cat("\nTabla de confusión para GLM:\n")
print(table(real = y_factor, prediccion = pred_glm_factor))
# Interpretación de las tablas de confusión para LDA, QDA y regresión logística:
#
# - Los tres modelos muestran un rendimiento muy similar en la clasificación de las dos clases simuladas.
#
# Para LDA y QDA:
# - De los 100 casos reales de la clase 1, 94 se clasificaron correctamente y 6 se clasificaron erróneamente como clase 2.
# - De los 100 casos reales de la clase 2, 92 se clasificaron correctamente y 8 se clasificaron erróneamente como clase 1.
#
# Para regresión logística (GLM):
# - De los 100 casos reales de la clase 1, 94 se clasificaron correctamente y 6 erróneamente como clase 2.
# - De los 100 casos reales de la clase 2, 93 se clasificaron correctamente y 7 erróneamente como clase 1.
#
# En conclusión:
# - Los tres modelos presentan tasas de acierto muy parecidas, con pocos errores en ambas clases.
# - Esto es coherente con la buena separación entre las clases en los datos simulados.
# - Dado que las matrices de covarianza son iguales, LDA y regresión logística funcionan bien,
#   y QDA no mejora significativamente el rendimiento.
# - La pequeña diferencia entre QDA y GLM en la clase 2 (8 vs 7 errores) es mínima
#   y no afecta la conclusión general sobre la eficacia de los modelos.
