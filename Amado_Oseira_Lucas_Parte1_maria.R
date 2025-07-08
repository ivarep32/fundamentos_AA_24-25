#EXAMEN FINAL PARTE 1

#Extraemos los datos
dat <- read.csv2("w2011.csv", fileEncoding = "UTF-8")

#Ajustamos por el método de mínimos cuadrados el siguiente modelo de regresión múltiple:
#Y = β0 +β1X1 +β2X2 +β3X3 +β4X4 +β5X5 +β6X6 +ε
#donde: Y = lCO2; X1 = inflation; X2 = lGDPc; X3 = GDP.growth; X4 = internet y X5 = lagv

z = lm(dat$lCO2 ~ dat$inflation + dat$lGDPc + dat$GDP.growth + dat$internet + dat$lagv); z

#a) Obtén los coeficientes estimados del modelo, así como intervalos de confianza al 95 %
#para los coeficientes del modelo. Calcula los valores ajustados del modelo y calcula la
#suma residual de cuadrados.

#COEFICIENTES ESTIMADOS DE FORMA AUTOMATICA
coef(z)

#COEFICIENTES ESTIMADOS DE FORMA MANUAL
X <- cbind(1, dat$inflation, dat$lGDPc, dat$GDP.growth, dat$internet, dat$lagv)
y <- dat$lCO2
coefs = solve(t(X)%*%X) %*% t(X) %*% y
coefs

# INTERVALOS DE CONFIANZA
confint(z,level=0.95)

# VALORES AJUSTADOS DE FORMA AUTOMÁTICA
fitted(z)

# VALORES AJUSTADOS DE FORMA MANUAL
vals = X%*%solve(t(X)%*%X)%*%t(X)%*%y
vals

#RSS MANUAL
XtXi <- solve(t(X)%*%X)
H <- X%*%XtXi%*%t(X)
RSS <- sum((y - H %*% y) ^ 2);RSS

#RSS AUTOMÁTICA
deviance(z)

#b) Calcula el valor del coeficiente de correlación de Pearson entre la variable respuesta Y y el predictor X1.
cor(dat$inflation, y = dat$lCO2, method = "pearson")

#c) Define y calcula el coeficiente de correlación parcial entre X1 y X2, controlando por el
#resto de las variables explicativas.

#ajustamos el modelo lineal de inflation sobre las otras variables y calculamos los residuos
r_inf = residuals(lm(dat$inflation ~ dat$lCO2 + dat$lGDPc + dat$GDP.growth + dat$internet + dat$lagv))
# ajustamos el modelo lineal de lGDPc sobre las mismas variables, obteniendo también los residuos
r_lGDPc = residuals(lm(dat$lGDPc ~ dat$lCO2 + dat$inflation + dat$GDP.growth + dat$internet + dat$lagv))
#coeficiente de correlación entre los residuos de ambos ajustes
cor(r_inf, r_lGDPc)

# Vemos que el coeficiente es positivo, con lo que las conclusiones de una regresión múltiple irían en
# sentido contrario a las de una regresión múltiple para estas variables.
# El efecto de la inflación sobre el log emisiones CO2 per cápita (lGDPc) es positivo.

#d) Considera un modelo reducido que contiene solo un subconjunto de las variables explicativas incluidas
#en el modelo completo. Formula y contrasta, mediante un test t, la
#hipótesis nula de que los coeficientes asociados a las variables excluidas son iguales a cero.

# Ajustamos el modelo completo
full_model <- lm(dat$lCO2 ~ dat$inflation + dat$lGDPc + dat$GDP.growth + dat$internet + dat$lagv)

# Resumen del modelo completo
summary(full_model)

# Ajustamos el modelo reducido (Sin X4)
reduced_model <- lm(dat$lCO2 ~ dat$inflation + dat$lGDPc + dat$GDP.growth + dat$lagv)

# Resumen del modelo reducido
summary(reduced_model)

# Formulando la hipótesis de que el modelo podría explicar la misma varianza de
# FTMax siendo su coeficiente X4 igual a 0, observamos que el p-valor mostrado en
# el summary full_model es < 2.2e-16, esto implica que las  probabilidades de que
# la hipótesis nula sea cierta son bajas, por lo que podríamos concluír que la
# variable X4 hace una aportación importante en nuestro modelo. Sin embargo,
# todavía podemos probar a eliminar X3, por ser una variable poco significativa.

# Ajustamos el modelo reducido (Sin X3)
reduced_model2 <- lm(dat$lCO2 ~ dat$inflation + dat$lGDPc + dat$internet + dat$lagv)

# Resumen del modelo reducido
summary(reduced_model2)

#Observamos lo mismo que en el caso anterior.

# Ajustamos el modelo reducido (Sin X3)
reduced_model3 <- lm(dat$lCO2 ~ dat$inflation + dat$lGDPc + dat$lagv)

# Resumen del modelo reducido
summary(reduced_model3)

#Tampoco nos beneficia eliminar ambas variables

#e) Compara el modelo completo con el modelo reducido mediante un test F. Expón claramente la hipótesis nula,
#calcula el estadístico de contraste, determina el valor crítico
#correspondiente y extrae las conclusiones en función del resultado del test.

# Para contrastar la variable excluida X4, usamos la función anova:
anova(reduced_model, full_model)

# Observamos que el p-valor es extremadamente bajo (igual que en el test-t), con
# esto anova nos está indicando que debemos ACEPTAR la hipótesis nula; es decir,
# la variable excluida (X4), resulta irrelevante para el ajuste del modelo.
# Se prefiere el modelo reducido (sin internet) por ser más simple y equivalente en poder explicativo.

#Estadístico de contraste = 7e-04
#Valor crítico = 0.9787

#En este caso el nivel crítico coincide con la significación del coeficiente asociado a X4,
#pues es la única variable que se suprime de un modelo al otro.

#f) Compara el coeficiente de determinación ajustado para ambos modelos.
summary(reduced_model) # R^2 = 0.7999
summary(full_model) # R^2 = 0.7986

# Al comprobar los coeficientes de determinación de ambos modelos, podemos comprobar
# que el modelo modelo reducido explica un poco mejor la varianza de lCO2 (0.7999 > 0.7986),


#2.Calcula las componentes principales de las variables numéricas (excepto country y Region)
#e interpreta aquellas que conjuntamente expliquen al menos un 90 % de la variabilidad total.
#Dibuja las dos primeras componentes principales en función del continente (Region)

# Excluye las variables categóricas
numeric_data <- dat[, !(names(dat) %in% c("country", "Region"))]

# Nos aseguramos de que todo sea numérico
numeric_data <- data.frame(lapply(numeric_data, as.numeric))

# Eliminamos filas con NA
numeric_data <- na.omit(numeric_data)

test.pca <- prcomp(numeric_data, scale. = TRUE)
summary(test.pca)
