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
# - X: matriz de predictores
# - y: variable respuesta (sr)
X <- model.matrix(sr ~ pop15 + pop75 + dpi + ddpi, data = savings)[, -1]  # Quitamos intercepto
y <- savings$sr
# NOTA IMPORTANTE:
# Standardize = TRUE es RECOMENDADO porque las penalizaciones de Ridge y LASSO
# dependen de la escala de los predictores. Si las variables están en escalas muy distintas
# (por ejemplo, pop15 en %, dpi en miles), esto distorsiona la penalización.
# Estandarizar = centrar y escalar cada columna (media 0, varianza 1).

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

#--------------------------------------------------
# INTERPRETACIÓN DEL PREPROCESAMIENTO
#--------------------------------------------------

# ¿Por qué estandarizar es importante?
# - Ridge y LASSO penalizan los coeficientes según su magnitud.
# - Si una variable tiene escala mayor (ej: 1000s) que otra (ej: 0-1),
#   su coeficiente será más penalizado, no por irrelevancia, sino por escala.
# - Estandarizar permite que todas las variables contribuyan de forma equitativa
#   a la penalización. Si no lo haces, las variables con varianza grande dominan.

# ¿Cuándo no se estandariza?
# - En algunos casos particulares donde ya se ha transformado o centrado manualmente.
# - Pero en la práctica con glmnet y regresión penalizada SIEMPRE SE RECOMIENDA usar standardize=TRUE.