#==========================================
# Simulacro de examen con preguntas de chatgpt
#============================================

#==================================
#Pregunta 1: Análisis de Regresión con savings
#==================================
#Apartado a)
#Carga el conjunto de datos savings del paquete faraway.
# Ajusta un modelo de regresión lineal múltiple donde la tasa de ahorro (sr)
# se explica por la población menor de 15 años (pop15), la población mayor de 75 años (pop75),
# la renta per cápita (dpi) y la tasa de crecimiento de la renta per cápita (ddpi).
# Muestra un resumen del modelo usando la función summary().
#-------------------------------------
library(faraway)
data("savings") #cargar savings

z<- lm(sr ~ pop15 + pop75 + dpi + ddpi, data=savings)
summary(z)

#--------------------------------
#b) Interpreta los coeficientes del modelo.
# ¿Qué variables tienen un efecto significativo sobre la tasa de ahorro?
#-------------------------------------------
#Son aquellas con un menor pvalor, es decir, en la salida:
#Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# las mas significatiavs son las dos primeras, es decir
#pop15 y pop75

#-------------------------------------
#c) Realiza un F-test para comparar el modelo completo con un modelo
# reducido que solo incluye pop15 y ddpi como predictores.
# Interpreta el resultado del test
#-----------------------------------
modelo_corto <- lm(sr ~ pop15 + ddpi, data=savings)
anova(z,modelo_corto)

rss0 <- deviance(modelo_corto)
rss <- deviance(z)
q <- 2           # nº de restricciones
n <- nrow(model.matrix(z))
p <- ncol(model.matrix(z))
# Diferencia de RSS
rss0 - rss

# Cálculo manual del estadístico F
f <- ((rss0 - rss) / q) / (rss / (n - p))
pvalue <- 1 - pf(f, q, n - p)

#vemos que el pvalor es mayor que 0.1 lo que significa
# que eliminar las variables que hemos eliminado
# no empeora significativamente el modelo
# considerariamos que lo empeoran si el p valor
#fuese menor a 0.5

#--------------------------------------------
#d) Calcula los intervalos de confianza al 95% para
# los parámetros del modelo completo.
# ¿Cómo se interpretan estos intervalos?
#----------------------------------------------
confint(z)
# las variables que contienen no 0 en su intervalo
# son las de mayor significancia, en este caso
# pop15, y ddpi son las mas significativas
# al 95%

#-----------------------------------------
#e) Usando el modelo completo, predice la tasa de ahorro
# para un país con los siguientes valores:
# pop15 = 35, pop75 = 3, dpi = 1500, ddpi = 6.
# Calcula también un intervalo de predicción al 95% para esta predicción
#------------------------------------------------
predict(z,data.frame(pop15=35,pop75=3,dpi=1500, ddpi=6),interval="prediction")
#la prediccion da 9.3

#==================================
#Pregunta 2: Análisis de Regresión con Advertising.csv
#==================================
#------------------------------------
#a) Carga el conjunto de datos Advertising.csv.
# Ajusta un modelo de regresión lineal múltiple
# donde las ventas (Sales) se explican por la
# inversión en publicidad en TV, Radio y Newspaper.
#----------------------------------------------
Advertising <- read.csv("Advertising.csv")
names(Advertising)
z<-lm(Sales ~ TV + Radio + Newspaper, data=Advertising)

#-----------------------------------------
#b) Muestra un resumen del modelo y comenta
# la significancia de cada predictor.
#------------------------------------------
summary(z)
# los de menor pvalor y por tanto los de
#mayor significancia son TV y Radio
# Newspaper tiene bastante pero es menor

#----------------------------------------
#c) Crea un nuevo modelo que excluya la variable
# Newspaper. Compara este modelo con el modelo completo
# utilizando un test de ANOVA.
# ¿Es justificable la exclusión de Newspaper?
#------------------------------------------
acotado <- lm(Sales ~ TV + Radio, data=Advertising)
anova(acotado,z)
#vemos que el p valor es muy grande
#por tanto newspaper no aporta tanto
#como las otras dos y es posible eliminarlo

#--------------------------------------------------
#d) Implementa el metodo de descenso de gradiente para
# estimar los coeficientes del modelo de regresión lineal
# (puedes simplificar y usar solo TV y Radio como predictores
# para facilitar la implementación). Compara los resultados
# obtenidos con los coeficientes estimados por la función lm().
# (Este apartado es más avanzado y requiere conocimientos de optimización).
#-------------------------------------------------------------
# 1. Preparar los datos y escalarlos (sin intercepto)
X_raw <- Advertising[, c("TV", "Radio")]
y <- Advertising$Sales
n <- nrow(Advertising)

# Escalado manual de las variables predictoras
X_scaled <- scale(X_raw)  # centrado y escala automático

# Añadir columna de 1s para el intercepto
X <- cbind(Intercept = 1, X_scaled)

# 2. Inicialización de coeficientes
beta <- rep(0, ncol(X))

# 3. Hiperparámetros
learning_rate <- 0.01  # tasa mayor ahora que escalamos las variables
iterations <- 10000

# 4. Función de coste (MSE)
cost_function <- function(X, y, beta) {
  predictions <- X %*% beta
  mse <- sum((y - predictions)^2) / (2 * length(y))
  return(mse)
}

# Vector para guardar el error y ver la convergencia
cost_history <- numeric(iterations)

# 5. Descenso de gradiente vectorizado
for (i in 1:iterations) {
  predictions <- X %*% beta
  error <- predictions - y

  gradient <- t(X) %*% error / n
  beta <- beta - learning_rate * gradient

  cost_history[i] <- cost_function(X, y, beta)

  if (i %% 1000 == 0) {
    cat("Iteración:", i, "Costo:", cost_history[i], "\n")
  }
}

# 6. Resultados
cat("\nCoeficientes estimados por descenso de gradiente:\n")
print(beta)

# 7. Ajuste con lm() usando las variables escaladas para comparar
Advertising$TV_scaled <- X_scaled[, "TV"]
Advertising$Radio_scaled <- X_scaled[, "Radio"]
model_lm <- lm(Sales ~ TV_scaled + Radio_scaled, data = Advertising)

cat("\nCoeficientes estimados por lm() con variables escaladas:\n")
print(coef(model_lm))

#==================================
#Pregunta 3: Simulación y Programación en R
#==================================
#-----------------------------------------------------
#a) Simula el lanzamiento de un dado 50 veces. 
# Calcula la tabla de frecuencias de los resultados 
# y muestra un gráfico de barras de las frecuencias
#-----------------------------------------------------
lanzamientos <- sample(1:6, 50, replace = TRUE)
tabla_frecuencias <- table(lanzamientos)
barplot(tabla_frecuencias, main = "Frecuencias de lanzamientos del dado",
        xlab = "Valor del dado", ylab = "Frecuencia")
#---------------------------------------------------------
#b) Escribe una función en R que tome como argumento el
# número de lanzamientos de un dado (n) y devuelva la
# tabla de frecuencias y el gráfico de barras correspondiente.
# La función debe tener un valor por defecto de n = 30.
# Asegúrate de que la función incluya un título en el gráfico
# que indique el número de lanzamientos.
#-----------------------------------------------------------
lanzamiento <- function(n = 30) {
  lanzamientos <- sample(1:6, n, replace = TRUE)
  tabla_frec <- table(lanzamientos)
  barplot(tabla_frec, main = paste("Frecuencias con", n, "lanzamientos"))
}

