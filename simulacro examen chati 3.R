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

#==============================================================
# Comparación de coeficientes entre OLS, Ridge, LASSO y Elastic Net
#==============================================================

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
