#----------------------------------------------------------------
#1. Algoritmo  de  descenso  de  gradiente
# 📘 Descenso de gradiente con paso fijo
# Buscamos minimizar la función f(x, y) = 1/2(x^2 + Γy^2)*
# Su gradiente es: ∇f(x, y) = (x, γy)

#* he puesto la gamma mayuscula pa q se diferencie mejor de la y
#-----------------------------------------------------------------

#parametros iniciales
gamma <- 2            # Valor de γ (> 0)
x <- c(gamma, 1)      # Punto inicial x(0) = (γ, 1)^T
eta <- 1e-6           # Criterio de convergencia
max_iter <- 1000      # Iteraciones máximas
t <- 0.1              # Paso fijo (learning rate)
trajectory <- matrix(NA, nrow = max_iter, ncol = 2)  # Guardar trayectoria

# descenso del gradiente
for (k in 1:max_iter) {
  grad <- c(x[1], gamma * x[2])              # Gradiente ∇f(x)
  x_new <- x - t * grad                      # Actualización: x := x - t * ∇f(x)
  trajectory[k, ] <- x_new                   # Guardamos posición
  if (sqrt(sum((x_new - x)^2)) < eta) break  # Criterio de parada
  x <- x_new
}

trajectory <- trajectory[1:k, ]
x  # Solución final

# ?????????????????????????????????????????????????????????????????????????????????????
# 📌 Efecto de t (paso) en la convergencia:
# - Si t es muy pequeño → convergencia lenta.
# - Si t es demasiado grande → el algoritmo puede divergir.
# - Un valor intermedio adecuado depende de la forma de la función (curvatura = γ).
# ?????????????????????????????????????????????????????????????????????????????????????

#---------------------------------------------------------------------------------------
#1.1 Elección  del  paso
# 📘 Descenso de gradiente con búsqueda exacta del paso (exact line search)
# Queremos minimizar f(x, y) = 1/2 (x^2 + γy^2)
# En cada iteración calculamos el paso t que minimiza f(x + s * ∆x)
#---------------------------------------------------------------------------------------

# Definimos el valor de γ
gamma <- 2

# Punto inicial
x <- c(gamma, 1)

# Parámetros de control
eta <- 1e-6            # Criterio de convergencia
max_iter <- 1000       # Iteraciones máximas
trajectory_exact <- matrix(NA, nrow = max_iter, ncol = 2)

# descenso con busqueda exacta del paso
for (k in 1:max_iter) {
  grad <- c(x[1], gamma * x[2])
  direction <- -grad

  #Paso exacto: t = (∇f)^T ∇f / (d^T H d)
  # En nuestro caso, H es diagonal: diag(1, γ^2)
  num <- sum(grad^2)
  denom <- sum(c(1, gamma^2) * direction^2)
  t_exact <- num / denom

  x_new <- x + t_exact * direction
  trajectory_exact[k, ] <- x_new

  if (sqrt(sum((x_new - x)^2)) < eta) break
  x <- x_new
}
# Coordenadas del mínimo
x

# Pregunta 1: Número de iteraciones realizadas
cat("Con γ = 1, iteraciones necesarias:", k, "\n")

#El metodo converge en 1 iteración cuando 𝛾 =1
# ya que los contornos de la función son circulares y la
# dirección del gradiente apunta exactamente hacia el mínimo.

#Pregunta 2: velocidad de convergencia para distintos valores de γ
convergencia_por_gamma <- function(gamma) {
  x <- c(gamma, 1)
  for (k in 1:max_iter) {
    grad <- c(x[1], gamma * x[2])
    direction <- -grad
    t_exact <- sum(grad^2) / sum(c(1, gamma^2) * direction^2)
    x_new <- x + t_exact * direction
    if (sqrt(sum((x_new - x)^2)) < eta) break
    x <- x_new
  }
  return(k)
}

# Valores de γ a comparar
gammas <- c(0.01, 0.1, 0.5, 1, 2, 5, 10, 100)
iters <- sapply(gammas, convergencia_por_gamma)

# Mostrar resultados
comparacion <- data.frame(gamma = gammas, iteraciones = iters)
print(comparacion)

