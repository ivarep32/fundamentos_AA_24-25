# Generamos datos simulados: X uniforme entre 0 y 1, y función seno con ruido normal
X = runif(200)
y = sin(4 * pi * X) + rnorm(200, sd = 0.25)

# Graficamos los datos originales: puntos (X,y)
plot(X, y, main = "Datos simulados con ruido")

#-----------------------------
# 1) Estimación con Kernel Nadaraya-Watson (modelo núcleo)
#-----------------------------

# Calculamos la matriz de diferencias (distancias) entre todos los puntos de X
D = outer(X, X, "-")  # Matriz 200x200 con X_i - X_j

h = 0.1  # Ancho de banda (bandwidth) para el kernel

# Calculamos la matriz de pesos usando kernel gaussiano con ancho h
K = dnorm(D / h)  # Kernel gaussiano evaluado en cada diferencia normalizada

# Normalizamos las filas para que sumen 1 (cada fila corresponde a un punto X_i)
S = sweep(K, 1, apply(K, 1, sum), "/")

# Estimación del valor suavizado y tilde: promedio ponderado de y según kernel
ytilde = S %*% y

# Graficamos los valores estimados contra los valores originales
plot(ytilde, y, main = "y estimado (kernel) vs y original")

#-----------------------------
# 2) Validación cruzada Leave-One-Out (LOOCV)
#-----------------------------

Kcv = K
diag(Kcv) = 0  # Para LOOCV no usamos el punto consigo mismo

# Normalizamos filas sin considerar el punto diagonal
Scv = sweep(Kcv, 1, apply(Kcv, 1, sum), "/")

# Estimación de y para LOOCV (cada punto predicho sin usar su propio valor)
ytcv = Scv %*% y

# Error cuadrático medio LOOCV
sqrt(mean((y - ytcv)^2))

#-----------------------------
# 3) Selección del ancho de banda (h) por LOOCV
#-----------------------------

# Secuencia de valores posibles para h a evaluar
hseq = seq(0.005, 0.05, length.out = 51)

MSE = numeric(length(hseq))  # Vector para guardar MSE para cada h

for (i in 1:length(hseq)) {
  Kcv = dnorm(D / hseq[i])
  diag(Kcv) = 0
  Scv = sweep(Kcv, 1, apply(Kcv, 1, sum), "/")
  ytcv = Scv %*% y
  MSE[i] = mean((y - ytcv)^2)
}

# Graficamos MSE vs h para elegir el óptimo
plot(hseq, MSE, type = "l", main = "Validación LOOCV para seleccionar h",
     xlab = "Ancho de banda h", ylab = "Error cuadrático medio (MSE)")

# Elegimos el h que minimiza el error
h.opt = hseq[which.min(MSE)]

#-----------------------------
# 4) Predicción con el ancho de banda óptimo
#-----------------------------

# Valores nuevos donde predecir (más densos para curva suave)
xpred = seq(0, 1, length.out = 101)

# Calculamos matriz kernel entre puntos nuevos y los datos originales
Kpr = dnorm(outer(xpred, X, "-") / h.opt)

# Normalizamos filas para que sumen 1
Spr = sweep(Kpr, 1, apply(Kpr, 1, sum), "/")

# Predicción de y en los puntos nuevos
ypr = Spr %*% y

# Graficamos datos originales y la curva suavizada estimada
plot(X, y, main = "Estimación Kernel y función real")
lines(xpred, ypr, col = 2, lwd = 2)

# Graficamos la función real sin ruido para comparación
curve(sin(4 * pi * x), from = 0, to = 1, col = 4, lwd = 2, add = TRUE)

#-----------------------------
# 5) Validación cruzada K-Fold (10 particiones)
#-----------------------------

nfold = 10
fold = sample(rep(1:nfold, each = 20))  # Asignamos aleatoriamente cada dato a un fold

ykf = numeric(length(y))  # Vector para almacenar predicciones

MSEkf = numeric(length(hseq))  # Vector para MSE por cada h

for (i in 1:length(hseq)) {
  for (j in 1:nfold) {
    # Kernel entre datos del fold j y el resto
    Kkf = dnorm(D[fold == j, fold != j] / hseq[i])

    # Normalizamos filas
    Skf = sweep(Kkf, 1, apply(Kkf, 1, sum), "/")

    # Predicción para los datos del fold j usando resto de datos
    ykf[fold == j] = Skf %*% y[fold != j]
  }
  # Error cuadrático medio para fold i
  MSEkf[i] = mean((y - ykf)^2)
}

# Graficamos MSE por K-fold para cada ancho h
plot(hseq, MSEkf, type = "l", main = "Validación K-Fold para seleccionar h",
     xlab = "Ancho de banda h", ylab = "Error cuadrático medio (MSE)")

# Elegimos el h óptimo para K-fold
h.opt2 = hseq[which.min(MSEkf)]

#-----------------------------
# 6) Estimación final con el ancho de banda elegido por K-Fold
#-----------------------------

K = dnorm(D / h.opt2)
S = sweep(K, 1, apply(K, 1, sum), "/")

# Suma de los elementos diagonales (influencia propia)
sum(diag(S))

# Estimación suavizada final
ytilde = S %*% y

# Graficamos estimación final vs valores observados
plot(ytilde, y, main = "Estimación Kernel final (K-Fold) vs Observados")

