#==============================================================
#Pregunta 1: Regresión Lineal General y Diagnóstico de Colinealidad
#==============================================================
#---------------------------------------------------------
#a) Carga el conjunto de datos savings del paquete faraway.
# Ajusta un modelo de regresión lineal múltiple donde la tasa
# de ahorro (sr) se explica por la población menor de 15 años
# (pop15), la población mayor de 75 años (pop75), la renta per
# cápita (dpi) y la tasa de crecimiento de la renta per cápita (ddpi)
#---------------------------------------------------------
library(faraway)
data("savings")
names(savings)
z <- lm(sr ~ pop15 + pop75 + dpi + ddpi, data=savings)
summary(z)
#---------------------------------------------------------
#b) Calcula el Factor de Inflación de la Varianza (VIF) para
# cada predictor en el modelo. Interpreta los resultados.
# ¿Hay evidencia de colinealidad? ¿Qué implicaciones tiene
# la colinealidad para la interpretación de los coeficientes
# y la estabilidad del modelo?
#---------------------------------------------------------
# VIF_j = 1 / (1 - R^2_j) donde R^2_j es el R^2 de la regresión de X_j sobre las otras
vars <- c("pop15", "pop75", "dpi", "ddpi")
vif_vals <- numeric(length(vars))
names(vif_vals) <- vars

for (v in vars) {
  otras <- setdiff(vars, v)
  form <- as.formula(paste(v, "~", paste(otras, collapse = "+")))
  r2 <- summary(lm(form, data = savings))$r.squared
  vif_vals[v] <- 1 / (1 - r2)
}

# Paso 3: Mostrar resultados
round(vif_vals, 3)

# Paso 4: Interpretación comentada en bloque de código
# Un VIF cercano a 1 indica baja colinealidad. Valores entre 1 y 5 son aceptables.
# Valores > 5 indican colinealidad moderada o severa.
# Si por ejemplo pop75 tiene un VIF > 5, eso sugiere que está fuertemente correlacionado
# con al menos una de las otras variables (por ejemplo pop15).
# Esto puede hacer que:
#  - Los errores estándar de los coeficientes se inflen
#  - Las estimaciones de los coeficientes sean inestables
#  - La interpretación individual de los coeficientes sea poco fiable
# Aun así, el modelo puede seguir siendo válido para predicción.
# Si la colinealidad es un problema, se pueden considerar:
#  - Eliminar variables redundantes
#  - Usar componentes principales o regularización (ridge, lasso)

#---------------------------------------------------------
#c) Ajusta un nuevo modelo que excluya la variable con el VIF
# más alto. Compara los coeficientes y los errores estándar
# de los predictores restantes en el nuevo modelo con los del
# modelo original. ¿Cómo ha cambiado la significancia de los predictores?
#---------------------------------------------------------
z_ajustado <- lm(sr ~ pop15 + dpi + ddpi, data=savings)
vars_ajustadas <- c("pop15", "dpi", "ddpi")
vif_vals_ajustadas <- numeric(length(vars_ajustadas))
names(vif_vals_ajustadas) <- vars_ajustadas

for (v in vars_ajustadas) {
  otras <- setdiff(vars_ajustadas, v)
  form <- as.formula(paste(v, "~", paste(otras, collapse = "+")))
  r2 <- summary(lm(form, data = savings))$r.squared
  vif_vals_ajustadas[v] <- 1 / (1 - r2)
}

# Paso 3: Mostrar resultados
round(vif_vals_ajustadas, 3)

### INTERPRETACIÓN DE LOS RESULTADOS DE VIF
###

# Resultado del modelo completo:
# pop15: 5.938
# pop75: 6.629
# dpi  : 2.884
# ddpi : 1.074

# Resultado del modelo ajustado (sin pop75):
# pop15: 2.460
# dpi  : 2.496
# ddpi : 1.071

# Comentario 1:
# Al eliminar la variable pop75 del modelo, el VIF de pop15 baja de 5.94 a 2.46,
# lo cual indica una reducción significativa de la colinealidad.

# Comentario 2:
# Esto sugiere que pop15 y pop75 estaban fuertemente correlacionadas entre sí,
# provocando que ambas tuvieran VIFs altos en el modelo original.

# Comentario 3:
# En el modelo ajustado, todos los VIFs están claramente por debajo del umbral de 5,
# lo que indica ausencia de colinealidad preocupante entre las variables restantes.

# Implicaciones:
# - La eliminación de pop75 mejora la estabilidad del modelo.
# - Los errores estándar de los coeficientes de pop15 y dpi probablemente disminuyen.
# - La interpretación de los coeficientes de pop15 y dpi es ahora más fiable.

# Conclusión:
# La colinealidad entre pop15 y pop75 en el modelo original distorsionaba las estimaciones.
# Quitar una de las dos variables es una estrategia válida para mejorar la claridad
# y robustez del modelo, aunque se pierde parte de la información original.
#==============================================================
#Pregunta 2: Regresión con Penalización (Ridge, LASSO, Elastic Net)
#==============================================================
#---------------------------------------------------------
#a) Usando los mismos datos savings, implementa Ridge Regression,
# LASSO Regression y Elastic Net. Utiliza validación cruzada
# para seleccionar el valor óptimo del parámetro de penalización
# (lambda) en cada caso. Justifica por qué es importante estandarizar
# los predictores antes de aplicar la regresión con penalización.
#---------------------------------------------------------
library(glmnet)

# Paso 1: Preparar los datos
# - X: matriz de predictores (sin intercepto, pero con nombres)
# - y: variable respuesta (sr)

X <- model.matrix(sr ~ pop15 + pop75 + dpi + ddpi, data = savings)
X <- X[, colnames(X) != "(Intercept)"]  # Quitar solo la columna del intercepto, conservar nombres

y <- savings$sr

#--------------------------------------------------
# RIDGE REGRESSION (alpha = 0)
#--------------------------------------------------
rridge <- cv.glmnet(X, y, alpha = 0, standardize = TRUE)
plot(rridge)

# Coeficientes con lambda óptimo (mínima CV error)
coef(rridge, s = rridge$lambda.min)

# Coeficientes con lambda 1se (más parsimonioso)
coef(rridge, s = rridge$lambda.1se)

# Predicciones y comparación gráfica
prridge <- predict(rridge, newx = X, s = rridge$lambda.1se)
plot(prridge, y, main = "Ridge: predicciones vs. reales")
abline(0, 1, col = "red")

#--------------------------------------------------
# LASSO REGRESSION (alpha = 1)
#--------------------------------------------------
rlasso <- cv.glmnet(X, y, alpha = 1, standardize = TRUE)
plot(rlasso)

# Coeficientes
coef(rlasso, s = rlasso$lambda.min)
coef(rlasso, s = rlasso$lambda.1se)

prlasso <- predict(rlasso, newx = X, s = rlasso$lambda.1se)
plot(prlasso, y, main = "LASSO: predicciones vs. reales")
abline(0, 1, col = "blue")

#--------------------------------------------------
# ELASTIC NET (alpha = 0.5)
#--------------------------------------------------
rEN <- cv.glmnet(X, y, alpha = 0.5, standardize = TRUE)
plot(rEN)

# Visualización adicional
plot(rlasso$glmnet.fit, xvar = "lambda", label = TRUE, main = "Trayectorias LASSO")

#------------------------------------------------------------------
# b) Compara los coeficientes estimados por Ridge, LASSO y Elastic
# Net con los coeficientes del modelo de regresión lineal sin penalización.
# ¿Qué variables son seleccionadas por LASSO (es decir, tienen coeficientes
# distintos de cero)? ¿Cómo afecta el parámetro alpha en Elastic Net a
# la selección de variables y la magnitud de los coeficientes?
#------------------------------------------------------------------

# Asumimos que tienes el modelo OLS ajustado en 'z' (lm)
coef_z <- coef(z)
coef_nombres <- names(coef_z)  # Ej: "(Intercept)", "pop15", "pop75", "dpi", "ddpi"

# Función para alinear coeficientes de glmnet con nombres de OLS, evitando NA
alinear_coef <- function(coef_obj, nombres_referencia) {
  coefs <- as.numeric(coef_obj)
  names(coefs) <- rownames(coef_obj)
  coefs_ajustados <- coefs[nombres_referencia]
  coefs_ajustados[is.na(coefs_ajustados)] <- 0
  return(coefs_ajustados)
}

# Coeficientes estimados por Ridge Regression (penalización L2)
ridge_mat <- coef(rridge, s = rridge$lambda.1se)
coef_ridge <- alinear_coef(ridge_mat, coef_nombres)

# Coeficientes estimados por LASSO (penalización L1)
lasso_mat <- coef(rlasso, s = rlasso$lambda.1se)
coef_lasso <- alinear_coef(lasso_mat, coef_nombres)

# Coeficientes estimados por Elastic Net (penalización combinada L1 y L2)
elastic_mat <- coef(rEN, s = rEN$lambda.1se)
coef_elasticnet <- alinear_coef(elastic_mat, coef_nombres)

# Crear un data.frame para comparar todos los coeficientes
coef_comparison <- data.frame(
  OLS = round(coef_z, 4),
  Ridge = round(coef_ridge, 4),
  LASSO = round(coef_lasso, 4),
  ElasticNet = round(coef_elasticnet, 4)
)

cat("Comparación de coeficientes entre modelos:\n")
print(coef_comparison)
# Interpretación de la comparación de coeficientes entre modelos:

# OLS (Regresión Lineal Ordinaria)
# - Los coeficientes reflejan la relación directa entre cada variable y la respuesta 'sr'.
# - Por ejemplo, 'pop75' tiene un coeficiente negativo importante (-1.6915),
#   indicando que a mayor proporción de personas mayores de 75 años, menor es 'sr'.
# - Todos los coeficientes están presentes (no hay ceros), lo que significa que todas las variables se usan en el modelo.

# Ridge Regression
# - Los coeficientes se han "encogido" mucho hacia cero comparados con OLS.
# - Esto es característico de Ridge, que penaliza coeficientes grandes sin forzarlos exactamente a cero.
# - Por ejemplo, 'pop75' pasa de -1.6915 a un coeficiente cercano a cero (0.0149), mostrando regularización fuerte.
# - Ningún coeficiente es exactamente cero, todas las variables permanecen en el modelo, aunque con menor peso.

# LASSO Regression
# - LASSO realiza selección automática de variables, forzando coeficientes a cero.
# - Aquí, 'pop75' y 'dpi' tienen coeficiente 0, lo que indica que LASSO las excluyó del modelo.
# - Variables como 'pop15' y 'ddpi' tienen coeficientes distintos de cero, consideradas relevantes.
# - Esto ayuda a simplificar el modelo y mejorar interpretabilidad.

# Elastic Net (alpha=0.5)
# - Combina penalización L1 (LASSO) y L2 (Ridge), por eso algunos coeficientes son exactamente cero (como en LASSO), pero otros son reducidos suavemente.
# - 'pop75' y 'dpi' quedan con coeficiente cero, igual que en LASSO.
# - Coeficientes no nulos ('pop15' y 'ddpi') tienen magnitudes intermedias, equilibrando selección y estabilidad.
# - Elastic Net es útil cuando hay variables correlacionadas, ofreciendo un balance entre selección y regularización.

# Resumen:
# - OLS incluye todas las variables sin penalización.
# - Ridge reduce coeficientes pero mantiene todas las variables.
# - LASSO selecciona variables relevantes eliminando algunas (coeficientes cero).
# - Elastic Net combina ambas técnicas, ofreciendo una selección más estable en presencia de correlación entre variables.

#==============================================================
# ¿Qué variables son seleccionadas por LASSO?
#==============================================================

# Excluir intercepto (primer coeficiente)
lasso_selected <- which(coef_lasso[-1] != 0)

cat("\nVariables seleccionadas por LASSO (coef != 0):\n")
print(colnames(X)[lasso_selected])

#==============================================================
# Exploración del efecto del parámetro alpha en Elastic Net
#==============================================================

alphas <- c(0.2, 0.5, 0.8)
elastic_coefs <- list()

for (a in alphas) {
  rEN_temp <- cv.glmnet(X, y, alpha = a, standardize = TRUE)
  elastic_mat_temp <- coef(rEN_temp, s = rEN_temp$lambda.1se)
  elastic_coefs[[as.character(a)]] <- alinear_coef(elastic_mat_temp, coef_nombres)
}

# Crear data.frame con resultados redondeados
elastic_comparison <- data.frame(
  alpha_0.2 = round(elastic_coefs[["0.2"]], 4),
  alpha_0.5 = round(elastic_coefs[["0.5"]], 4),
  alpha_0.8 = round(elastic_coefs[["0.8"]], 4)
)
rownames(elastic_comparison) <- coef_nombres

cat("\nComparación de coeficientes con diferentes valores de alpha en Elastic Net:\n")
print(elastic_comparison)
# Interpretación de la comparación de coeficientes con diferentes valores de alpha en Elastic Net:

# alpha controla el balance entre Ridge (alpha=0) y LASSO (alpha=1).

# alpha = 0.2 (más cercano a Ridge)
# - Los coeficientes están todos cerca de cero, pero ninguno es exactamente cero (excepto algunos).
# - Esto indica que el modelo es más "suave", penalizando pero sin hacer selección fuerte.
# - Variables como 'pop15' y 'ddpi' tienen coeficientes pequeños pero distintos de cero.
# - Elastic Net con alpha bajo mantiene casi todas las variables, reduciendo magnitudes para evitar sobreajuste.

# alpha = 0.5 (balance intermedio)
# - Varios coeficientes se vuelven exactamente cero (por ejemplo, 'pop15', 'pop75', 'dpi', 'ddpi').
# - El modelo selecciona menos variables, haciendo una selección más agresiva.
# - Esto muestra el efecto de la penalización L1 más fuerte, que induce sparsity.

# alpha = 0.8 (más cercano a LASSO)
# - Similar a alpha=0.5, pero los coeficientes no nulos tienden a tener mayor magnitud en valor absoluto.
# - Variables seleccionadas ('pop15' y 'ddpi') tienen coeficientes más marcados, mientras otras quedan en cero.
# - Mayor alpha enfatiza la selección de variables, favoreciendo modelos más parsimoniosos.

# Resumen:
# - Al aumentar alpha, Elastic Net se comporta más como LASSO, forzando más coeficientes a cero (más sparsity).
# - Al disminuir alpha, se acerca a Ridge, manteniendo todos los coeficientes pero más pequeños.
# - El parámetro alpha permite controlar el trade-off entre selección de variables y estabilidad del modelo.

# Comentario final:
cat("
Interpretación:
- LASSO seleccionó las siguientes variables como relevantes para predecir 'sr': ")
cat(paste(colnames(X)[lasso_selected], collapse = ", "))
cat("
- Esto indica que LASSO realiza una selección automática útil cuando se busca simplicidad interpretativa.

- En Elastic Net, al aumentar alpha de 0.2 a 0.8, los coeficientes tienden a ser más dispersos (más ceros).
  Esto muestra cómo alpha controla el grado de sparsity (dispersión) del modelo:
    - alpha bajo: todos los coeficientes son pequeños pero diferentes de cero.
    - alpha alto: algunos coeficientes son forzados a cero (selección de variables).
")

#------------------------------------------------------------------
# c) Calcula el MSE (Error Cuadrático Medio) en una muestra de prueba
# para Ridge, LASSO, Elastic Net y el modelo de regresión lineal
# sin penalización. 
#------------------------------------------------------------------

#==============================================================
# División de datos en conjunto de entrenamiento y prueba
#==============================================================
set.seed(123)  # Para reproducibilidad
n <- nrow(savings)
# Seleccionamos aleatoriamente 70% de los índices para entrenamiento
train_idx <- sample(1:n, size = round(0.7 * n))
# El resto serán los índices para prueba
test_idx <- setdiff(1:n, train_idx)

# Extraemos las matrices de predictores y vectores de respuesta para cada conjunto
X_train <- X[train_idx,]
y_train <- y[train_idx]
X_test <- X[test_idx,]
y_test <- y[test_idx]

#==============================================================
# Ajuste de modelos en datos de entrenamiento
#==============================================================

# OLS (Ordinary Least Squares)
# Modelo lineal clásico sin penalización, minimiza suma de errores al cuadrado.
z_train <- lm(sr ~ pop15 + pop75 + dpi + ddpi, data = savings[train_idx,])

# Ridge Regression (alpha = 0)
# Penalización L2: reduce magnitud de coeficientes para controlar varianza, no realiza selección de variables.
rridge_train <- cv.glmnet(X_train, y_train, alpha = 0, standardize = TRUE)

# LASSO (alpha = 1)
# Penalización L1: puede reducir coeficientes a cero, realizando selección automática de variables.
rlasso_train <- cv.glmnet(X_train, y_train, alpha = 1, standardize = TRUE)

# Elastic Net (alpha = 0.5)
# Combina penalizaciones L1 y L2, balanceando selección y estabilidad.
rEN_train <- cv.glmnet(X_train, y_train, alpha = 0.5, standardize = TRUE)

#==============================================================
# Evaluación de modelos en conjunto de prueba mediante MSE
#==============================================================

# Predicciones OLS sobre test
pred_ols <- predict(z_train, newdata = savings[test_idx,])
# Error cuadrático medio (MSE): métrica para evaluar desempeño predictivo
mse_ols <- mean((y_test - pred_ols)^2)

# Predicciones Ridge sobre test usando lambda 1se para parsimonia
pred_ridge <- predict(rridge_train, newx = X_test, s = "lambda.1se")
mse_ridge <- mean((y_test - pred_ridge)^2)

# Predicciones LASSO sobre test
pred_lasso <- predict(rlasso_train, newx = X_test, s = "lambda.1se")
mse_lasso <- mean((y_test - pred_lasso)^2)

# Predicciones Elastic Net sobre test
pred_en <- predict(rEN_train, newx = X_test, s = "lambda.1se")
mse_en <- mean((y_test - pred_en)^2)

#==============================================================
# Comparación y visualización de resultados
#==============================================================

# Data frame con MSE de cada modelo para comparación cuantitativa
results <- data.frame(
  Modelo = c("OLS", "Ridge", "LASSO", "Elastic Net"),
  MSE = c(mse_ols, mse_ridge, mse_lasso, mse_en)
)
print(results)

# Gráficos de dispersión predicciones vs valores reales
# La línea roja indica igualdad perfecta (predicción = valor real)
par(mfrow = c(2, 2))  # Organizar gráficos en matriz 2x2

plot(pred_ols, y_test, main = "OLS", xlab = "Predicción", ylab = "Real")
abline(0, 1, col = "red")

plot(pred_ridge, y_test, main = "Ridge", xlab = "Predicción", ylab = "Real")
abline(0, 1, col = "red")

plot(pred_lasso, y_test, main = "LASSO", xlab = "Predicción", ylab = "Real")
abline(0, 1, col = "red")

plot(pred_en, y_test, main = "Elastic Net", xlab = "Predicción", ylab = "Real")
abline(0, 1, col = "red")

#--------------------------------------------------------------
# Interpretación:
# - El MSE cuantifica el error promedio de predicción: menor MSE, mejor ajuste.
# - OLS puede sobreajustar si hay multicolinealidad o variables irrelevantes.
# - Ridge mejora al reducir varianza pero no hace selección.
# - LASSO hace selección automática al eliminar coeficientes no útiles.
# - Elastic Net combina ventajas de Ridge y LASSO para mejor rendimiento general.
# - Los gráficos muestran qué tan cerca están las predicciones de los valores reales.

#==============================================================
#Pregunta 3: Modelos Aditivos y No Paramétricos
#==============================================================
#---------------------------------------------------------
#a) Usando el conjunto de datos oecdpanel (si está disponible,
# o simula un conjunto de datos similar con una relación no
# lineal entre predictores y respuesta), ajusta un modelo aditivo
# generalizado (GAM) para predecir la tasa de crecimiento económico
# (growth) en función de la inversión (inv), el crecimiento de la
# población (popgro), el PIB inicial (initgdp) y capital humano
# (humancap) 1. Utiliza splines cúbicos para modelar las relaciones no lineales.
#---------------------------------------------------------
# Un Modelo Aditivo Generalizado (GAM) es una extensión de los modelos lineales que permite
# capturar relaciones no lineales entre la variable respuesta y los predictores.
#
# En lugar de asumir que cada predictor tiene un efecto lineal, GAM permite que cada uno tenga
# una forma funcional flexible, estimada a partir de los datos.
#
# La forma general es:
#   E[Y] = α + f1(X1) + f2(X2) + ... + fn(Xn)
# donde f1, f2, ..., fn son funciones suaves (por ejemplo, splines).
#
# Esto es útil cuando se sospecha que la relación entre X e Y no es estrictamente lineal.

install.packages("oecdpanel")
install.packages("AER")
library(AER)
library(mgcv)      # Para ajustar modelos GAM
library(ggplot2)   # Para visualización opcional
data("OECDGrowth")
head(OECDGrowth)
str(OECDGrowth)
summary(OECDGrowth)
OECDGrowth$growth <- log(OECDGrowth$gdp85) - log(OECDGrowth$gdp60) # crecimiento (es decir mejora del PIB en 1985 respecto a 1960)
gam_model <- gam(growth ~
                   s(invest, bs = "cr") +      # Inversión: spline cúbico
                   s(popgrowth, bs = "cr") +   # Crecimiento poblacional: spline cúbico
                   s(gdp60, bs = "cr") +       # PIB inicial: spline cúbico
                   s(school, bs = "cr"),       # Capital humano: spline cúbico
                 data = OECDGrowth)
summary(gam_model)

# El modelo GAM sugiere que existe una relación no lineal entre 'growth' y todas las variables:
# especialmente 'invest', 'school' y 'gdp60' (por sus edf altos).
# Sin embargo, los p-valores no muestran significancia estadística individual en los splines.
# Aun así, el modelo tiene un ajuste global excelente (R² ajustado ≈ 98.5%, deviance ≈ 99.9%).
# Esto puede deberse a un tamaño de muestra pequeño (n = 22), que reduce el poder estadístico.
# Es recomendable visualizar los gráficos de las funciones suaves para interpretar mejor
# cómo cada predictor afecta el crecimiento económico (no linealmente).

#---------------------------------------------------------
#b) Interpreta las funciones suaves estimadas para inv, popgro e initgdp.
# ¿Qué patrones no lineales observas en la relación entre estos predictores
# y la tasa de crecimiento económico?
#---------------------------------------------------------
plot(gam_model, se = TRUE, col = "blue")
# invest (Inversión): Relación no lineal. El crecimiento económico es alto con baja inversión,
# luego decrece hasta estabilizarse alrededor del valor 0.27 de inversión.

# popgrowth (Crecimiento poblacional): Relación parcialmente no lineal.
# Estable hasta cierto punto (~0.016), luego muestra una tendencia descendente.

# gdp60 (PIB inicial en 1960): Relación no lineal.
# Disminuye con valores bajos de PIB y se estabiliza a partir de ~8000.

# school (Capital humano): Relación suave.
# Ligeramente creciente hasta 0.08, luego se aplana o desciende un poco.

# Elementos comunes en todas las gráficas:
# - La línea azul indica la tendencia estimada.
# - Las líneas punteadas representan el intervalo de confianza.
# - Las marcas en el eje x ("ticks") muestran dónde se concentran los datos observados.

# Conclusión general:
# Cuando una función estimada tiene una forma pseudolineal (es decir, casi una línea recta),
# indica que la relación entre esa variable y la respuesta podría ser bien explicada
# por un modelo lineal. Sin embargo, el GAM permite verificar si existe alguna curvatura
# sutil que un modelo lineal ignoraría. Estas formas también indican que la variable podría
# tener un efecto constante o muy suave sobre la respuesta en el rango observado.

#---------------------------------------------------------
#c) Compara el rendimiento del GAM con un modelo de regresión lineal múltiple estándar
# (sin funciones suaves). ¿El GAM mejora significativamente el ajuste a los datos?
# Utiliza una prueba de hipótesis apropiada (por ejemplo, una prueba F)
# para comparar los modelos.
#---------------------------------------------------------
# Ajustar el modelo lineal estándar (sin funciones suaves)
lm_model <- lm(growth ~ invest + popgrowth + gdp60 + school, data = OECDGrowth)

# Comparar con el modelo GAM usando una prueba F
anova(lm_model, gam_model, test = "F")

# el valor F obtenido:  21.69
# el pvalor:  0.1072
# el pvalor es >0.05 y mayor que 0.1 (los umbrales mas comunes)
#podemos decir que el modelo gam no ofrece una mejora significativa
# y que podriamos usar el modelo lineal sin perder demasiada explicabilidad

#==============================================================
#Pregunta 4: Descenso de Gradiente y Optimización
#==============================================================
#---------------------------------------------------------
#a) Implementa el algoritmo de descenso de gradiente con "backtracking
# line search" para minimizar la función:
# f(x, y) = 1/2 (x^2 + γy^2), donde γ es un parámetro que puedes variar.
#---------------------------------------------------------
gamma <- 10
x <- c(gamma, 1)       # Punto inicial x(0)
eta <- 1e-6            # Criterio de convergencia
max_iter <- 1000       # Número máximo de iteraciones

alpha <- 0.3           # Parámetro de control (0, 0.5]
beta <- 0.8            # Factor de reducción (0, 1)
trajectory_bt <- matrix(NA, nrow = max_iter, ncol = 2)

# Función objetivo: ahora recibe un vector x
f <- function(x) {
  1/2 * (x[1]^2 + gamma * x[2]^2)
}

# Gradiente de la función
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
cat(sprintf("Solución final: x = (%.6f, %.6f)\n", x[1], x[2]))

# - El algoritmo comienza en el punto inicial x = (2, 1).
# - Aplica descenso de gradiente con búsqueda de paso "backtracking" para minimizar la función f(x, y) = 1/2 (x^2 + 2*y^2).
# - En cada iteración ajusta el tamaño del paso para garantizar una disminución suficiente de la función.
# - La condición de parada es que el cambio entre iteraciones sea muy pequeño (menos que 1e-6).
# - El mensaje "Convergencia con backtracking en 14 iteraciones" indica que el algoritmo encontró un mínimo en solo 14 pasos.
# - La solución final es x = (0.000000, 0.000000), que es el mínimo global esperado, ya que la función es convexa y mínima en el origen.
#
# En resumen, el algoritmo funcionó correctamente y encontró el mínimo de la función rápidamente.

#---------------------------------------------------------
#b) Experimenta con diferentes valores de γ (por ejemplo, γ = 0.1, 1, 10)
# y diferentes valores de los parámetros α y β en el
# "backtracking line search".
# ¿Cómo afectan estos parámetros a la velocidad de convergencia del algoritmo?
#---------------------------------------------------------
# con gamma=0.1:
#Convergencia con backtracking en 111 iteraciones
#Solucion final: x = (0.000000, 0.000008)
# vemos que ahora converge mucho mas lento

#con gamma = 1
#Convergencia con backtracking en 2 iteraciones
#Soluci�n final: x = (0.000000, 0.000000)
# converge mucho mas rapido

# con gamma = 10:
#Convergencia con backtracking en 75 iteraciones
#Soluci�n final: x = (0.000002, -0.000000)
#converge mas rapido que con gamma = 0.1 pero mas lento q con gamma=1

#conclusion final:
# El parámetro gamma afecta la "curvatura" o la forma de la función en la dirección y.
# Esto influye directamente en la geometría del problema y, por tanto, en la dificultad del
# descenso de gradiente para encontrar el mínimo.

# - Cuando gamma es pequeño (por ejemplo, 0.1):
#   La curvatura en la dirección y es muy baja (la función es "plana" en esa dirección).
#   Esto genera un "valle alargado" o una región con pendiente muy pequeña en y.
#   Por eso el algoritmo avanza lentamente en esa dirección y tarda más en converger.
#   Se requieren muchas iteraciones para ajustar y acercarse al mínimo, aunque
#   finalmente converge muy cerca del origen.

# - Cuando gamma es 1:
#   La curvatura en ambas direcciones (x y y) es igual.
#   La función es más "simétrica" y el descenso de gradiente se mueve con rapidez y eficiencia.
#   Esto permite una convergencia muy rápida (pocos pasos) hacia el mínimo en el origen.

# - Cuando gamma es grande (por ejemplo, 10):
#   La función tiene una alta curvatura en la dirección y, lo que genera un "valle estrecho".
#   El gradiente en y es mucho más pronunciado que en x, y el algoritmo debe tomar pasos más
#   pequeños para evitar "sobrepasar" el mínimo en esa dirección.
#   Por tanto, la convergencia es más lenta que con gamma=1, pero más rápida que cuando gamma es muy pequeño.

# En resumen:
# - La velocidad de convergencia depende de la relación entre las curvaturas en cada dirección.
# - Cuando la curvatura es equilibrada (gamma cerca de 1), la convergencia es rápida.
# - Cuando la curvatura es muy diferente en las direcciones (gamma muy pequeño o muy grande),
#   la convergencia es más lenta debido a la geometría del problema que dificulta el avance uniforme.
# Esto es un fenómeno clásico en optimización conocido como "condicionamiento" del problema.

# Por eso, variar gamma cambia tanto el número de iteraciones necesarias para converger
# como la precisión final en el mínimo.

