#----------------------------------
# Remuestreo bootstrap
#----------------------------------
# Introducción:
# El metodo bootstrap permite estimar la variabilidad de un estadístico
# generando múltiples muestras con reemplazo desde los datos observados.
# En este boletín, aplicamos bootstrap a la regresión lineal simple
# cuando los errores no siguen una distribución normal.

#-----------------------------------------------------------
# 1. Generación de los datos del modelo
#-----------------------------------------------------------
set.seed(123)  # Para reproducibilidad
# Definimos 15 puntos equiespaciados en [0, 1)
n <- 15
x <- seq(0, 14/15, length.out = n)
# Parámetros del modelo
beta_0 <- 1
beta_1 <- 1

# Generamos errores con distribución t de Student con 3 grados de libertad
epsilon <- rt(n, df = 3)

# Generamos la variable respuesta según el modelo Yi = β0 + β1*xi + εi
y <- beta_0 + beta_1 * x + epsilon

#-----------------------------------------------------------
# 2. Ajuste del modelo y cálculo inicial
#-----------------------------------------------------------

# ajustamos el modelo de regresion lineal
fit <- lm(y~x)

# estimacion de coeficientes β̂0 gorro y β̂1 gorro
beta_0_hat <- coef(fit)[1]
beta_1_hat <- coef(fit)[2]

#calculamos los residuos
residuos <- resid(fit)

#calculamos leverage h_i para ajustar varianzas de los residuos
h <- hatvalues(fit)

# residuos corregidos con varianza constante
residuos_mod <- residuos/sqrt(1-h)

#media de residuos corregidos
r <- mean(residuos_mod)

#centrado de los residuos modificados
residuos_centered <- residuos_mod - r

#-----------------------------------------------------------
# 3. Definimos función de bootstrap
#-----------------------------------------------------------
bootstrap <- function(x, beta_0_hat, beta_1_hat, residuos_centered){
  # Paso 1: remuestreo de residuos centrados
  e_star <- sample(residuos_centered, replace=T)

  #Paso 2: genrar nueva respuesta Y* con residuos
  y_star <- beta_0_hat + beta_1_hat * x + e_star

  #Paso 3: ajustar el nuevo modelo de regresion
  fit_star <- lm(y_star ~x)

  #devolver b_1_hat
  return(coef(fit_star)[2])
}

#-----------------------------------------------------------
# 4. Estimación del intervalo bootstrap para β̂1 (B = 500)
#-----------------------------------------------------------

B<-500
alpha <- 0.05

# generamos B estimaciones bootsrap de beta_1_hat
beta_1_star <- replicate(B,bootstrap(x, beta_0_hat, beta_1_hat, residuos_centered))

#calculamos el cuantil inferior y superior
cu <- quantile(beta_1_star,1-alpha/2)
cl <- quantile(beta_1_star, alpha/2)

# intervalo de confianza para beta1
IC_bootstrap <- c(beta_1_hat - cu, beta_1_hat - cl)
IC_bootstrap

#-----------------------------------------------------------
# 5. Repetimos el experimento M = 500 veces
#-----------------------------------------------------------

M <- 500
coberturas_bootstrap<- numeric(M)

for (m in 1:M){
  #nuevos datos con errores t
  epsilon <- rt(n, df = 3)
  y <- beta_0 + beta_1 * x + epsilon
  fit <- lm(y ~ x)

  beta_0_hat <- coef(fit)[1]
  beta_1_hat <- coef(fit)[2]
  residuos <- resid(fit)
  h <- hatvalues(fit)
  residuos_mod <- residuos / sqrt(1 - h)
  r <- mean(residuos_mod)
  residuos_centered <- residuos_mod - r

  beta_1_star <- replicate(B, bootstrap(x, beta_0_hat, beta_1_hat, residuos_centered))

  cu <- quantile(beta_1_star, 1 - alpha / 2)
  cl <- quantile(beta_1_star, alpha / 2)

  IC <- c(beta_1_hat - cu, beta_1_hat - cl)
  coberturas_bootstrap[m] <- beta_1 >= IC[1] & beta_1 <= IC[2]
}
# Porcentaje de veces que β1 está dentro del intervalo bootstrap
mean(coberturas_bootstrap)

#-----------------------------------------------------------
# 6. Comparación con el metodo clásico (normalidad)
#-----------------------------------------------------------
coberturas_normal <- numeric(M)
for (m in 1:M) {
  epsilon <- rt(n, df = 3)
  y <- beta_0 + beta_1 * x + epsilon
  fit <- lm(y ~ x)

  beta_1_hat <- coef(fit)[2]
  se <- summary(fit)$coefficients[2, 2]

  t_value <- qt(1 - alpha / 2, df = n - 2)

  IC_normal <- beta_1_hat + c(-1, 1) * t_value * se
  coberturas_normal[m] <- beta_1 >= IC_normal[1] & beta_1 <= IC_normal[2]
}
# Porcentaje de veces que β1 está dentro del intervalo clásico
mean(coberturas_normal)