# Cargamos los datos
data(mtcars)

# Seleccionamos las variables de interés
mpg <- mtcars$mpg; mpg
cyl <- mtcars$cyl; cyl
hp <- mtcars$hp; hp
disp <- mtcars$disp; disp
wt <- mtcars$wt; wt
qsec <- mtcars$qsec; qsec
vs <- mtcars$vs; vs
am <- mtcars$am; am
gear <- mtcars$gear; gear
carb <- mtcars$carb; carb

# Ajustamos un modelo lineal múltiple con todas las variables respecto a mpg
modelo <- lm(mpg ~ cyl + hp + disp + wt + qsec + vs + am + gear + carb)

# Calculamos los apalancamientos
hii <- hatvalues(modelo); hii

# Calculamos las distancias de Cook
distancia_cook <- cooks.distance(modelo); distancia_cook

# Calculamos los residuos del modelo
residuos <- residuals(modelo)

# Calculamos el RSE
RSE <- sqrt(sum(residuos^2) / modelo$df.residual); RSE

# Calcumos el resumen del modelo
resumen <- summary(modelo)

# Coeficiente de correlación simple entre mpg y las demás variables
matriz_correlacion_mpg_s <- cor(mtcars, mtcars$mpg); matriz_correlacion_mpg_s

# Coeficiente de correlación múltiple entre mpg y las demás variables
library(ppcor) # Instalamos la librería
matriz_correlacion_p <- pcor(mtcars); matriz_correlacion_p # Calculamos la matriz de correlación parcial
matriz_correlacion_mpg_p <- matriz_correlacion_p$estimate[1,]; matriz_correlacion_mpg_p # Extraemos la fila de mpg estimado

# Ajustamos los modelos
modelo_1 <- lm(mpg ~ qsec + cyl + wt, data = mtcars)
modelo_2 <- lm(mpg ~ cyl + wt, data = mtcars)

# Resumen del modelo_1
resumen_1 <- summary(modelo_1); resumen_1

# Test Anova entre el modelo_1 y el modelo_2
test <- anova(modelo_2, modelo_1); test

# Valor de R^2 ajustado del modelo_1
R2_ajustado_1 <- resumen_1$adj.r.squared; R2_ajustado_1

# Valor de R^2 ajustado del modelo_2
R2_ajustado_2 <- summary(modelo_2)$adj.r.squared; R2_ajustado_2

# Intervalo de confianza del 90% para los parametros del modelo
confianza <- confint(modelo_2, level = 0.90); confianza

# Predicción de mpg con cyl = 5 y wt = 3.45
# Crear un nuevo data frame con los valores de cyl y wt
nuevo_coche <- data.frame(cyl = 5, wt = 3.45)

# Realizar la predicción con el modelo_2
prediccion <- predict(modelo_2, nuevo_coche, interval = "confidence", level = 0.90); prediccion