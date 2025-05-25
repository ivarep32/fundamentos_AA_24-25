###
### Simulación de datos multivariados y modelos lineales con regularización
###

# Cargar librería necesaria para generar datos multivariados normales
library(MASS)

# Ayuda sobre mvrnorm: genera muestras de una distribución normal multivariada
?mvrnorm

# Media de cada variable: vector de ceros de longitud 7
mu <- rep(0, 7)

# Ayuda sobre toeplitz: crea una matriz de correlación con estructura de autocorrelación
?toeplitz

# Matriz de covarianzas: correlaciones decrecientes 0.9^|i-j|
Sigma <- toeplitz(0.9 ^ c(0:6))

# Fijar semilla para reproducibilidad
set.seed(20250423)

# Generar una matriz X (100 observaciones x 7 variables correlacionadas)
X <- mvrnorm(n = 100, mu, Sigma)

# Visualización de las relaciones bivariadas entre las variables
pairs(X)

# Añadir columna de 1s para el intercepto
Xe <- cbind(rep(1, 100), X)

# Coeficientes verdaderos: solo las primeras 5 variables son relevantes
beta <- c(3, 2, -1, 3, -2, 0, 0, 0)  # beta_0 (intercepto), X1..X7

# Generar respuesta con error normal (sd = 0.5)
y <- Xe %*% beta + rnorm(100, sd = 0.5)

# Ajustar un modelo lineal múltiple con todas las variables
rlm <- lm(y ~ X)
summary(rlm)

# Calcular el error estándar residual (RSE) manualmente
sqrt(sum(residuals(rlm)^2) / 92)  # grados de libertad = n - p = 100 - 8

# Separar variables relevantes (X1 a X4) y no relevantes (X5 a X7)
lvar1 <- c(1:4)
X1 <- Xe[, lvar1]
X2 <- Xe[, -lvar1]  # incluye intercepto y X5..X7

# Proyección sobre el espacio generado por X1
P1 <- solve(t(X1) %*% X1) %*% t(X1)

# Estimación parcial de los coeficientes de X1
beta1s <- P1 %*% y

# Matriz de proyección H1 = X1 * (X1'X1)^(-1) * X1'
H1 <- X1 %*% P1
I <- diag(100)  # matriz identidad

# Estimación de beta2 (coeficientes de X2 ajustando por X1)
beta2 <- solve(t(X2) %*% (I - H1) %*% X2) %*% t(X2) %*% (I - H1) %*% y

# Estimación de beta1 ajustando por X2
beta1 <- P1 %*% (y - X2 %*% beta2)

######################

# Reformular como data.frame para usar fórmula
df <- data.frame(y = y, X = X)

# Modelo lineal con todas las variables (X1..X7)
rlm <- lm(y ~ ., data = df)
summary(rlm)

# Eliminar una variable (X.6) y volver a ajustar el modelo
rlm2 <- update(rlm, . ~ . - X.6)
summary(rlm2)

# Aplicar selección de variables paso a paso (AIC con k = 2)
?step
rstep <- step(rlm, k = 2)

# Alternativa: selección por BIC (k = log(n))
rstep <- step(rlm, k = log(100))
summary(rstep)

# Influencia de observaciones (valores altos de leverage)
infl <- influence(rstep)
which(infl$hat > 3 * length(coef(rstep)) / 100)  # observaciones influyentes

# Valores estandarizados de los residuos
rs <- rstandard(rstep)
quantile(rs, c(.01, .025, .05, 0.95, .975, .99))  # comparación con normal
qnorm(c(.01, .025, .05, 0.95, .975, .99))

# Residuos studentizados
rst <- rstudent(rstep)
quantile(rst, c(.01, .025, .05, 0.95, .975, .99))
which(abs(rst) > 2.5)  # observaciones atípicas

################

# Instalar y cargar la librería glmnet (regresiones penalizadas)
install.packages("glmnet", dependencies = TRUE)
library(glmnet)
?glmnet

# Ver desviaciones estándar de las columnas de X
apply(X, 2, sd)

# RIDGE REGRESSION (alpha = 0)
rridge <- cv.glmnet(X, y, alpha = 0, standardize = FALSE)  # ridge sin estandarizar
names(rridge)
plot(rridge)

# Coeficientes para lambda óptimo y 1se
coef(rridge, s = rridge$lambda.min)
coef(rridge, s = rridge$lambda.1se)

# Predicciones con lambda.1se
prridge <- predict(rridge, newx = X, s = rridge$lambda.1se)
plot(prridge, y, main = "Ridge: predicciones vs observaciones")

# LASSO REGRESSION (alpha = 1)
rlasso <- cv.glmnet(X, y, alpha = 1, standardize = FALSE)
plot(rlasso)
coef(rlasso, s = rlasso$lambda.min)
coef(rlasso, s = rlasso$lambda.1se)

# Predicciones LASSO
prlasso <- predict(rlasso, newx = X, s = rlasso$lambda.1se)
plot(prlasso, y, main = "LASSO: predicciones vs observaciones")

# ELASTIC NET (alpha = 0.5)
rEN <- cv.glmnet(X, y, alpha = 0.5, standardize = FALSE)
plot(rEN)

# Visualización de trayectorias para LASSO
plot(rlasso$glmnet.fit, xvar = "lambda", main = "Trayectorias LASSO")
