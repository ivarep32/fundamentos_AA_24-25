# Cargamos las librerías necesarias
library(MASS)
library(ISLR)

# Cargamos los datos
data(Smarket)

# Dividir los datos en conjuntos de entrenamiento y prueba
set.seed(123) # Para reproducibilidad
train <- Smarket$Year < 2005 # Usamos los datos anteriores a 2005 para entrenamiento
Smarket.train <- Smarket[train, ] # Datos de entrenamiento
Smarket.test <- Smarket[!train, ] # Datos de prueba
Direction.test <- Smarket$Direction[!train] # Variable objetivo para los datos de prueba

# Inicializamos la matriz de errores
iteraciones <- 100 # Número de iteraciones
errores <- matrix(NA, nrow = iteraciones, ncol = 3) # Matriz para almacenar los errores de cada modelo

# Simulación de las 100 muestras de entrenamiento
for (i in 1:iteraciones) {

  # Regresión Logística
  modelo_log <- glm(Direction ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume, data = Smarket.train, family = binomial) # Ajustamos el modelo
  pred_log <- predict(modelo_log, newdata = Smarket.test, type = "response") # Hacemos las predicciones
  pred_ifelse <- ifelse(pred_log > 0.5, "Up", "Down") # Clasificamos en base a si la probabilidad supera 0.5
  errores[i, 1] <- mean(pred_ifelse != Direction.test) # Calculamos el error de clasificación

  # LDA
  modelo_lda <- lda(Direction ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume, data = Smarket.train) # Ajustamos el modelo
  pred_lda <- predict(modelo_lda, newdata = Smarket.test)$class # Hacemos las predicciones
  errores[i, 2] <- mean(pred_lda != Direction.test) # Calculamos el error de clasificación

  # QDA
  modelo_qda <- qda(Direction ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume, data = Smarket.train) # Ajustamos el modelo
  pred_qda <- predict(modelo_qda, newdata = Smarket.test)$class # Hacemos las predicciones
  errores[i, 3] <- mean(pred_qda != Direction.test) # Calculamos el error de clasificación
}

# Comparación de errores entre modelos
boxplot(errores, main = "Tasas de error para Regresion Logistica, LDA y QDA",
        names = c("Regresion Logistica", "LDA", "QDA"), ylab = "Tasa de error") # Creamos un boxplot para comparar los errores de los modelos