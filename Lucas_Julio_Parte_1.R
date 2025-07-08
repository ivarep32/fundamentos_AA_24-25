#importamos y observamos los datos
csv <- read.csv("w2011.csv", sep = ";", dec=',', header = TRUE)
summary(csv)
str(csv)
head(csv)


# 1. Se desea predecir las emisiones de emisiones CO2 per cápita (lCO2), en función del resto
# de variables. Ajusta por el metodo de mínimos cuadrados el siguiente modelo de regresión
# múltiple: Y = β0 +β1X1 +β2X2 +β3X3 +β4X4 +β5X5 +β6X6 +ε donde: X1 = inflation;
# X2 = lGDPc; X3 = GDP.growth; X4 = internet y X5 = lagv

# a) Obtén los coeficientes estimados del modelo, así como intervalos de confianza al 95 %
# para los coeficientes del modelo. Calcula los valores ajustados del modelo y calcula la
# suma residual de cuadrados.

z <- lm(lCO2 ~ inflation + lGDPc + GDP.growth + internet + lagv, data = csv)

# coeficientes automáticos
betahat <- coef(z); betahat

# coeficientes manuales
X <- model.matrix(z)
n <- nrow(X);n
p <- ncol(X);p

XtXi <- solve(t(X) %*% X)
H <- X %*% XtXi %*% t(X)

y <- csv$lCO2
hbeta <- XtXi %*% t(X) %*% y
hbeta

# suma residual de cuadrados automáticos
residuals(z) #obtenemos los residuos del problema
RSS <- sum(residuals(z)^2); RSS #suma residual de cuadrados

# suma residual de cuadrados manuales
RSS <- t(y - X %*% hbeta) %*% (y - X %*% hbeta);RSS

# intervalos confianza automáticos
confint(z)

# intervalos confianza manuales
sigma2 <- RSS / (n - p)
sigma2

ET <- sqrt(rep(sigma2, length(diag(XtXi))) * diag(XtXi))
niv <- 0.95
t <- qt(1 - (1 - niv)/2, n - p)

betainf <- betahat - t * ET #extremos inferiores de los intervalos de confianza para los coeficientes
betainf

betasup <- betahat + t * ET #extremos superiores
betasup

# valores ajustados automáticos
fitted(z)

# valores ajustados manuales
yhat <- X %*% betahat; yhat

# -------------------------------------------------------------------------------------------------------------------- #

# b) Calcula el valor del coeficiente de correlación de Pearson entre la variable respuesta Y
# y el predictor X1.

cor(dat$inflation, y = dat$lCO2, method = "pearson") #coeficiente de correlacion entre lCO2(y) y inflation(X1)

# -------------------------------------------------------------------------------------------------------------------- #

# c) Define y calcula el coeficiente de correlación parcial entre X1 y X2, controlando por el
# resto de las variables explicativas.

r_x1 <- residuals(lm( inflation ~ GDP.growth + internet + lagv, data = csv))
summary(r_x1)
r_x2 <- residuals(lm( lGDPc ~ GDP.growth + internet + lagv, data = csv))
summary(r_x2)
cor(r_x1,r_x2)


install.packages("ppcor", dep=TRUE)
library(ppcor)
corr_parcial_matrix = pcor(cbind(csv$inflation, csv$lGDPc,csv$GDP.growth, csv$internet, csv$lagv))
corr_parcial_matrix$estimate[1,2]

# -------------------------------------------------------------------------------------------------------------------- #

# d) Considera un modelo reducido que contiene solo un subconjunto de las variables explicativas
# incluidas en el modelo completo. Formula y contrasta, mediante un test t, la
# hipótesis nula de que los coeficientes asociados a las variables excluidas son iguales a
# cero.

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


# -------------------------------------------------------------------------------------------------------------------- #

# e) Compara el modelo completo con el modelo reducido mediante un test F.
# Expón claramente la hipótesis nula, calcula el estadístico de contraste, determina el valor crítico
# correspondiente y extrae las conclusiones en función del resultado del test.

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

# -------------------------------------------------------------------------------------------------------------------- #

# f) Compara el coeficiente de determinación ajustado para ambos modelos.

summary(reduced_model) # R^2 = 0.7999
summary(full_model) # R^2 = 0.7986

# Al comprobar los coeficientes de determinación de ambos modelos, podemos comprobar
# que el modelo modelo reducido explica un poco mejor la varianza de inflation (0.7999 > 0.7986),

# Compara todos los resultados que obtienes con sus versiones manuales, verifica que coinciden e interpeta los resultados (2.5 ptos).

# ==================================================================================================================== #

# 2. Calcula las componentes principales de las variables numéricas (excepto country y Region)
# e interpreta aquellas que conjuntamente expliquen al menos un 90 % de la variabilidad total.
# Dibuja las dos primeras componentes principales en función del continente (Region) (1.5ptos).

datos_numericos <- csv[, !names(csv) %in% c("country", "Region")]

# Análisis de componentes principales
pca <- prcomp(datos_numericos, scale. = TRUE)

# Resumen de varianza explicada
summary(pca)
# Obervamos que superamos el 90% de la variabilidad con los tres primeros elementos (93.58%)

screeplot(pca)
screeplot(pca, type="lines")
# Podríamos ver que le codo se forma ya en el paso de de la primera a la segunda componente pero el salto entre la
# tercera y la cuarta también es considerable, se reduce el porcentaje de varianza de un 12,54% a un 3,121%.


pca$rotation[,1:4]
biplot(pca)



