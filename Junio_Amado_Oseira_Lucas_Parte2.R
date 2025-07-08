midni = 1413
set.seed(midni)
A1= midni %% 2;A1
A2= sample(1:25,3)
load("KorTemp.RData")
lmues=sample(1:nrow(KorTemp),300)
summary(lmues)
str(KorTemp)
str(lmues)
summary(KorTemp)

# 3
# (1 pto) Calcula un módelo de regresión para explicar (A1=0:FTMax, A1=1:FTmin) con las variables del último período disponible (empiezan por L), con el objetivo de usar el mejor modelo
# con el menor número de covariables. Explica el proceso seguido, las elecciones tomadas y
# los resultados obtenidos.
# cargamos las variables relevantes (nombre inicia con "L")
datos_L <- KorTemp[, grep("^L", names(KorTemp))]
names(datos_L)
z <- lm(FTmin ~ LTMax + LTmin + LRHmin + LRHmax + LTMax_D + LTmin_D + LLH + LRadSol, data=KorTemp)
summary(modelo)

# en esta tabla observamos que las variables con menor valor estadistico son:
# LRHmax(0.56613) y LRadSol(0.08025), podriamos eliminar LRHmax por su baja significancia, si quitaramos
# LRadSol muy posiblemente el modelo empeorase.

z1 <- lm(FTmin ~LTMax + LTmin + LRHmin  + LTMax_D + LTmin_D + LLH + LRadSol, data=KorTemp) #sin LRHMax
z2 <-  lm(FTmin ~LTMax + LTmin + LRHmin + LTMax_D + LTmin_D + LLH, data=KorTemp) #sin LRHMax y sin LRadSol

# hacemos un ftest para  ver como cambian los modelos
anova(z1, z)
# el valor no es significativo y la varianza es casi nula podemos eliminar la variable sin empeorar el modelo

anova(z2, z)
# de nuevo el valor no es significativo y la varianza sigue siendo pequeña podríamos eliminar la variable sin empeorar demasiado el modelo

# 4
# (1 pto) Seleccionando las filas del conjunto de datos dada por lmues: KorTemp[lmues,],
# estimar un modelo de regresión no lineal para explicar (A1=0:FTMax, A1=1:FTmin) usando la
# variable LRadSol. Comenta las opciones elegidas y dibuja el modelo de regresión obtenido
# sobre los datos originales.
library(mgcv)

# preparamos los datos
datos <- KorTemp[lmues,]

# aplicamos gam en LRadSol
gam <- as.formula(paste("FTmin", "~ s(LRadSol)"))

# ajustamos
mgam <- gam(gam, data = lmues)

summary(mgam)

library(ggplot2)

# prediccion de valores
datos$pred <- predict(mgam)
datos$LRadSol_pred <- datos$LRadSol

ggplot(lmues, aes(x = LRadSol, y = FTmin)) +
  geom_point(alpha = 0.4, color = "darkgrey") +
  geom_line(aes(y = pred), color = "blue", linewidth = 1.2) +
  labs(    title = paste("Modelo GAM:", "FTmin", "~ s(LRadSol)"),x = "LRadSo)",y = "FTmin")


# 5
# (1 pto). Para las estaciones seleccionadas en A2, elaborar dos modelos de clasificación (uno
# basado en Regla de Bayes y el otro en técnicas de regresión) con las variables LTMax, LRHmax,
# LWS y LLH que intente clasificar los datos por estación. Justificar las elecciones tomadas y
# estimar el error de mala clasificación que tendríamos si aplicásemos los modelos a nuevos
# datos.
# 1. Filtramos las estaciones de interés
datos_estaciones <- KorTemp[KorTemp$Estacion %in% A2, ]
vars_usadas <- c("Estacion", "LTMax", "LRHmax", "LWS", "LLH")

# 2. Convertimos "Estacion" en factor para clasificación
datos_estaciones$Estacion <- factor(datos_estaciones$Estacion)

# 3. Modelo basado en Regla de Bayes
library(e1071)
modelo_bayes <- naiveBayes(Estacion ~ LTMax + LRHmax + LWS + LLH, data = datos_estaciones)
pred_bayes <- predict(modelo_bayes, datos_estaciones)
tabla_bayes <- table(Predicted = pred_bayes, Actual = datos_estaciones$Estacion)
error_bayes <- 1 - sum(diag(tabla_bayes)) / sum(tabla_bayes)

# 4. Modelo de Regresión Logística Multinomial
library(nnet)
modelo_log <- multinom(Estacion ~ LTMax + LRHmax + LWS + LLH, data = datos_estaciones)
pred_log <- predict(modelo_log, datos_estaciones)
tabla_log <- table(Predicted = pred_log, Actual = datos_estaciones$Estacion)
error_log <- 1 - sum(diag(tabla_log)) / sum(tabla_log)

# 5. Reportamos errores de clasificación
error_bayes
error_log