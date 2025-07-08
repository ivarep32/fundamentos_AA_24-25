# Método gradiente descendente para regresión lineal simple (só con variable 'radio')
z3 <- lm(sales ~ radio, data=datos)  # Modelo lineal tradicional (para comparación)
summary(z3)

gradient_descent <- function(y, x, t = 0.002, tol = 1e-6, max_iter = 1000000) {
  
  # Inicialización de parámetros (b0: intercepto, b1: pendente)
  b0 <- 0
  b1 <- 0
  n <- length(y)  # Número de observacións
  
  for (iter in 1:max_iter) {
    # 1. Predicións do modelo actual
    y_pred <- b0 + b1 * x  # Modelo lineal: y = b0 + b1*x
    
    # 2. Cálculo do erro (diferenza entre predicións e valores reais)
    error <- y_pred - y
    
    # 3. Cálculo do gradiente (derivadas parciais da función de custo MSE)
    grad_b0 <- sum(error) / n          # Derivada respecto a b0
    grad_b1 <- sum(error * x) / n      # Derivada respecto a b1
    
    # 4. Actualización dos parámetros (paso do gradiente descendente)
    new_b0 <- b0 - t * grad_b0
    new_b1 <- b1 - t * grad_b1
    
    # 5. Comprobación de converxencia (cambio moi pequeno nos parámetros)
    if (abs(new_b0 - b0) < tol && abs(new_b1 - b1) < tol) {
      break  # Saír do bucle se converxe
    }
    
    # 6. Actualizar parámetros para a seguinte iteración
    b0 <- new_b0
    b1 <- new_b1
  }
  
  # Devolver resultados (coeficientes e número de iteracións)
  return(list(b0 = b0, b1 = b1, iter=iter))
}

# Aplicar o método aos datos
resultado <- gradient_descent(datos$sales, datos$radio)
print(resultado)  # Mostrar os coeficientes estimados e iteracións



############################
#ALGO MÁS AMPLIADO
############################

# =============================================
# MÉTODO DEL GRADIENTE DESCENDENTE
# Implementación para regresión lineal simple
# (solo con una variable predictora 'radio')
# =============================================

# ---------------------------------------------
# 1. Cargar datos y ajustar modelo lineal tradicional (lm)
# ---------------------------------------------

# Ajustamos un modelo lineal tradicional para comparación
modelo_lm <- lm(sales ~ radio, data = datos)

# Resumen del modelo (coeficientes, R-squared, etc.)
summary(modelo_lm)

# ---------------------------------------------
# 2. Implementación del Gradiente Descendente
# ---------------------------------------------

gradient_descent <- function(
    y,                  # Vector de variable respuesta (ventas)
    x,                  # Vector de variable predictora (radio)
    t = 0.002,          # Tasa de aprendizaje (learning rate)
    tol = 1e-6,         # Tolerancia para criterio de parada
    max_iter = 1000000  # Máximo número de iteraciones
) {
  
  # Inicialización de parámetros
  b0 <- 0   # Intercepto (inicializado en 0)
  b1 <- 0   # Pendiente (inicializado en 0)
  n <- length(y)  # Número de observaciones
  
  # Historial de pérdida (opcional, para visualización)
  loss_history <- numeric(max_iter)
  
  # Bucle de optimización
  for (iter in 1:max_iter) {
    
    # -----------------------------------------
    # Paso 1: Calcular predicciones con los parámetros actuales
    # Modelo: y_pred = b0 + b1 * x
    # -----------------------------------------
    y_pred <- b0 + b1 * x
    
    # -----------------------------------------
    # Paso 2: Calcular el error (diferencia entre predicciones y valores reales)
    # -----------------------------------------
    error <- y_pred - y
    
    # -----------------------------------------
    # Paso 3: Calcular el gradiente (derivadas parciales)
    # Gradiente para b0: derivada de MSE respecto a b0
    # Gradiente para b1: derivada de MSE respecto a b1
    # -----------------------------------------
    grad_b0 <- sum(error) / n
    grad_b1 <- sum(error * x) / n
    
    # -----------------------------------------
    # Paso 4: Actualizar parámetros
    # Nuevo parámetro = parámetro actual - tasa_aprendizaje * gradiente
    # -----------------------------------------
    new_b0 <- b0 - t * grad_b0
    new_b1 <- b1 - t * grad_b1
    
    # -----------------------------------------
    # Paso 5: Calcular pérdida (Error Cuadrático Medio) para seguimiento
    # -----------------------------------------
    loss_history[iter] <- mean(error^2)
    
    # -----------------------------------------
    # Paso 6: Verificar condición de parada
    # Si el cambio en los parámetros es muy pequeño, detenemos las iteraciones
    # -----------------------------------------
    if (abs(new_b0 - b0) < tol && abs(new_b1 - b1) < tol) {
      message("\n¡Convergencia alcanzada en la iteración ", iter, "!")
      loss_history <- loss_history[1:iter]  # Recortar historial
      break
    }
    
    # -----------------------------------------
    # Paso 7: Actualizar parámetros para la siguiente iteración
    # -----------------------------------------
    b0 <- new_b0
    b1 <- new_b1
  }
  
  # Mensaje si se alcanzó el máximo de iteraciones
  if (iter == max_iter) {
    warning("Alcanzado el máximo de iteraciones sin convergencia.")
  }
  
  # -----------------------------------------
  # 3. Retornar resultados
  # -----------------------------------------
  return(list(
    intercept = b0,      # Coeficiente b0 (intercepto)
    slope = b1,          # Coeficiente b1 (pendiente)
    iterations = iter,   # Número de iteraciones realizadas
    loss_history = loss_history  # Historial de pérdida (opcional)
  ))
}

# ---------------------------------------------
# 3. Aplicación a los datos
# ---------------------------------------------

# Ejecutar el gradiente descendente
resultado_gd <- gradient_descent(
  y = datos$sales,  # Variable respuesta
  x = datos$radio   # Variable predictora
)

# Mostrar resultados
cat("\nResultados del Gradiente Descendente:\n")
cat("---------------------------------\n")
cat("Intercepto (b0):", resultado_gd$intercept, "\n")
cat("Pendiente (b1):", resultado_gd$slope, "\n")
cat("Iteraciones:", resultado_gd$iterations, "\n")

# ---------------------------------------------
# 4. Comparación con el modelo lineal tradicional
# ---------------------------------------------

cat("\nComparación con lm():\n")
cat("--------------------\n")
cat("Intercepto (lm):", coef(modelo_lm)[1], "\n")
cat("Pendiente (lm):", coef(modelo_lm)[2], "\n")

# ---------------------------------------------
# 5. Visualización opcional (requiere ggplot2)
# ---------------------------------------------

if (require(ggplot2)) {
  # Crear dataframe para gráfico
  plot_data <- data.frame(
    Radio = datos$radio,
    Ventas = datos$sales,
    Predicho_lm = predict(modelo_lm),
    Predicho_gd = resultado_gd$intercept + resultado_gd$slope * datos$radio
  )
  
  # Gráfico comparativo
  ggplot(plot_data, aes(x = Radio)) +
    geom_point(aes(y = Ventas), color = "blue") +
    geom_line(aes(y = Predicho_lm), color = "red", linetype = "dashed") +
    geom_line(aes(y = Predicho_gd), color = "green") +
    labs(
      title = "Comparación: Gradiente Descendente vs lm()",
      subtitle = "Línea verde: Gradiente Descendente\nLínea roja: Modelo lm()",
      y = "Ventas"
    ) +
    theme_minimal()
  
  # Gráfico de convergencia (pérdida por iteración)
  if (length(resultado_gd$loss_history) > 0) {
    ggplot(data.frame(
      Iteration = 1:resultado_gd$iterations,
      Loss = resultado_gd$loss_history
    ), aes(x = Iteration, y = Loss)) +
      geom_line(color = "purple") +
      labs(
        title = "Convergencia del Gradiente Descendente",
        subtitle = "Evolución de la función de pérdida",
        y = "Error Cuadrático Medio"
      ) +
      theme_minimal()
  }
}
