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
  # Definimos las variables necesarias dentro de la función
  max_iter <- 1000
  eta <- 1e-6
  x <- c(gamma, 1)
  
  for (k in 1:max_iter) {
    grad <- c(x[1], gamma * x[2])
    direction <- -grad
    t_exact <- sum(grad^2) / sum(c(1, gamma^2) * direction^2)
    x_new <- x + t_exact * direction
    
    # Añadimos validación para evitar NAs
    if (is.na(sum((x_new - x)^2))) {
      return(max_iter)
    }
    
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

# 📌 Análisis de la velocidad de convergencia:
# - Con γ = 1 → convergencia ideal: solo 1 iteración.
# - Cuando 1/3 < γ < 3, la convergencia es bastante eficiente.
# - Cuando γ ≪ 1 o γ ≫ 1, la función se vuelve muy elíptica, lo que hace que:
#     - El gradiente apunte en direcciones poco útiles.
#     - Se necesiten muchas más iteraciones para acercarse al mínimo.

# Pregunta 3: Programa   el   algoritmo   utilizando   como   paso   de   cada
# iteración   el   obtenido   por   el   metodo backtracking  line  search

# 📘 Backtracking Line Search para descenso de gradiente
# Objetivo: minimizar f(x, y) = 1/2(x^2 + γ y^2)

# Parámetros del metodo
gamma <- 2
x <- c(gamma, 1)       # Punto inicial x(0)
eta <- 1e-6            # Criterio de convergencia
max_iter <- 1000       # Número máximo de iteraciones

alpha <- 0.3           # Parámetro de control (0, 0.5]
beta <- 0.8            # Factor de reducción (0, 1)
trajectory_bt <- matrix(NA, nrow = max_iter, ncol = 2)

# Función objetivo
f <- function(x) {
  0.5 * (x[1]^2 + gamma * x[2]^2)
}

#Gradiente de la función
grad_f <- function(x) {
  c(x[1], gamma * x[2])
}

# Bucle del algoritmo con backtracking
for (k in 1:max_iter) {
  grad <- grad_f(x)
  direction <- -grad         # Dirección descendente
  t <- 1                     # Paso inicial

  # Backtracking line search
  while (f(x + t * direction) > f(x) + alpha * t * sum(grad * direction)) {
    t <- beta * t
  }

  x_new <- x + t * direction
  trajectory_bt[k, ] <- x_new

  # Criterio de parada
  if (sqrt(sum((x_new - x)^2)) < eta) {
    cat(sprintf("Convergencia con backtracking en %d iteraciones\n", k))
    x <- x_new
    break
  }

  x <- x_new
}

# Solución final
cat(sprintf(" Solución final: x = (%.6f, %.6f)\n", x[1], x[2]))

# 📌 Sobre el metodo de backtracking line search:
# - Se usa cuando no se conoce el paso óptimo exacto.
# - Permite adaptar dinámicamente la longitud del paso para asegurar descenso suficiente.
# - Los parámetros α y β controlan la "agresividad" del metodo:
#     - α: cuánto descenso mínimo se espera (típicamente entre 0.1 y 0.3)
#     - β: cuánto se reduce el paso en cada intento (típicamente entre 0.5 y 0.9)
# - Si α es muy pequeño, se acepta cualquier paso, aunque no sea eficiente.
# - Si β es muy pequeño, se reducen mucho los pasos → lento pero seguro.
# - Puedes probar diferentes combinaciones para ver cómo afecta la convergencia.

#----------------------------------------------------------------------------------------------------------------------
# 1.2 Demostración  del  algoritmo  de  descenso  de  gradiente
# Visualización del descenso de gradiente con 'grad.desc':
# - La función grad.desc() genera una animación paso a paso del algoritmo.
# - Muestra cómo el punto se mueve en la superficie de la función hasta alcanzar el mínimo.
# - Es útil para entender cómo el paso (gamma) afecta a la velocidad y trayectoria de convergencia.
# - init: vector con el punto inicial (x0, y0).
# - gamma: tamaño del paso (learning rate); si es muy grande, puede rebotar; si es muy pequeño, avanza lento.

# Puedes experimentar cambiando 'gamma' o el punto inicial para observar diferentes comportamientos.

#----------------------------------------------------------------------------------------------------------------------

# Cargar el paquete 'animation' para visualización animada
# Si no está instalado, debes ejecutar primero:install.packages("animation")
# a poder ser en los servidores de austria (si Manuel Febrero es tu profesor
# ya sabrás porqué)
library(animation)

# Configuramos los parámetros de la animación
# - interval: tiempo entre fotogramas (en segundos)
# - nmax: número máximo de iteraciones que mostrará la animación
ani.options(interval = 0.3, nmax = 50)

# Definimos la función a minimizar
# f(x, y) = 1/2 (x^2 + 2y^2)
fun_obj <- function(x, y) 0.5 * (x^2 + 2 * y^2)

# Ejecutamos la animación del descenso de gradiente
# - FUN: función objetivo
# - init: punto inicial
# - gamma: tasa de aprendizaje (paso fijo)
result <- grad.desc(FUN = fun_obj, init = c(2, 1), gamma = 0.2)

# Imprime la solución final encontrada
result$par

#Visualiza la trayectoria sobre la superficie 3D de la función
result$persp(col = "lightblue", phi = 30)

