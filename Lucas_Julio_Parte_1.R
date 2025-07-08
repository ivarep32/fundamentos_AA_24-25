#importamos y observamos los csvos
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
coef(z)

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

betainf <- hbeta - t * ET #extremos inferiores de los intervalos de confianza para los coeficientes
betainf

betasup <- hbeta + t * ET #extremos superiores
betasup

# valores ajustados automáticos
fitted(z)

# valores ajustados manuales
yhat <- X %*% hbeta; yhat

# -------------------------------------------------------------------------------------------------------------------- #

# b) Calcula el valor del coeficiente de correlación de Pearson entre la variable respuesta Y
# y el predictor X1.

cor(csv$inflation, y = csv$lCO2, method = "pearson") #coeficiente de correlacion entre lCO2(y) y inflation(X1)

#La correlación es negativa, por lo que cuando la inflación es mayor hay una tendencia a que las emisiones de CO2 per cápita
# sean menores, aunque esta relación es débil.

# ∣r∣=0.215 -->  correlación débil
# Existe cierta asociación lineal negativa, no es fuerte ni dominante.

# La inflación tiene una ligera asociación inversa con las emisiones de CO₂ per cápita, pero es posiblemente
# no significativa o débilmente significativa.

# -------------------------------------------------------------------------------------------------------------------- #

# c) Define y calcula el coeficiente de correlación parcial entre X1 y X2, controlando por el
# resto de las variables explicativas.

#ajustamos el modelo lineal de inflation sobre las otras variables y calculamos los residuos
r_inf = residuals(lm(csv$inflation ~ csv$lCO2 + csv$GDP.growth + csv$internet + csv$lagv))
# ajustamos el modelo lineal de lGDPc sobre las mismas variables, obteniendo también los residuos
r_lGDPc = residuals(lm(csv$lGDPc ~ csv$lCO2 + csv$GDP.growth + csv$internet + csv$lagv))
#coeficiente de correlación entre los residuos de ambos ajustes
cor(r_inf, r_lGDPc) #-0.186524

# Podemos ver que el coeficiente es negativo, con lo que las conclusiones de una regresión múltiple coincidirían con
# las de una regresión múltiple para estas variables.
# El efecto de la inflación sobre el log de emisiones CO2 per cápita (lGDPc) es negativo.
# A mayor inflación menor será el log de emisiones CO2 per cápita.

# -------------------------------------------------------------------------------------------------------------------- #

# d) Considera un modelo reducido que contiene solo un subconjunto de las variables explicativas
# incluidas en el modelo completo. Formula y contrasta, mediante un test t, la
# hipótesis nula de que los coeficientes asociados a las variables excluidas son iguales a
# cero.

summary(z)

# Ajustamos el modelo reducido (Sin X4)
reduced_model <- lm(csv$lCO2 ~ csv$inflation + csv$lGDPc + csv$GDP.growth + csv$lagv)

# Resumen del modelo reducido
summary(reduced_model)

# Formulando la hipótesis de que el modelo podría explicar la misma varianza de FTMax siendo su coeficiente X4 igual a 0,
# observamos que el p-valor mostrado en el summary full_model es < 2.2e-16, esto indica que las  probabilidades de que
# la hipótesis nula sea cierta son bajas, por lo que podríamos concluír que la variable X4 hace una aportación
# importante en nuestro modelo. Sin embargo, todavía podemos probar a eliminar X3, por ser una variable poco significativa.

# Ajustamos el modelo reducido (Sin X3)
reduced_model2 <- lm(csv$lCO2 ~ csv$inflation + csv$lGDPc + csv$internet + csv$lagv)

# Resumen del modelo reducido
summary(reduced_model2)

#Observamos lo mismo que en el caso anterior.

# Ajustamos el modelo reducido (Sin X3)
reduced_model3 <- lm(csv$lCO2 ~ csv$inflation + csv$lGDPc + csv$lagv)

# Resumen del modelo reducido
summary(reduced_model3)

#Tampoco nos beneficia eliminar ambas variables


# -------------------------------------------------------------------------------------------------------------------- #

# e) Compara el modelo completo con el modelo reducido mediante un test F.
# Expón claramente la hipótesis nula, calcula el estadístico de contraste, determina el valor crítico
# correspondiente y extrae las conclusiones en función del resultado del test.

# Para contrastar la variable excluida X4, usamos la función anova:
anova(reduced_model, z)

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
summary(z) # R^2 = 0.7986

# Al comprobar los coeficientes de determinación de ambos modelos, podemos comprobar
# que el modelo modelo reducido explica un poco mejor la varianza de inflation (0.7999 > 0.7986),

# Compara todos los resultados que obtienes con sus versiones manuales, verifica que coinciden e interpeta los resultados (2.5 ptos).

# ==================================================================================================================== #

# 2. Calcula las componentes principales de las variables numéricas (excepto country y Region)
# e interpreta aquellas que conjuntamente expliquen al menos un 90 % de la variabilidad total.
# Dibuja las dos primeras componentes principales en función del continente (Region) (1.5ptos).

# Excluye las variables categóricas
numeric_data <- dat[, !(names(dat) %in% c("country", "Region"))]

# Nos aseguramos de que todo sea numérico
numeric_data <- data.frame(lapply(numeric_data, as.numeric))

# Eliminamos filas con NA
numeric_data <- na.omit(numeric_data)

#Calculamos componentes principales
test.pca <- prcomp(numeric_data, scale. = TRUE)
#estandarizamos las variables con scale = TRUE para que todas pesen igual en el análisis PCA, ya que se
#encuentran en distintas escalas(unidades).
summary(test.pca)

#Empleamos prcomp() porque los datos tienen diferentes unidades de medida.
#Vemos como las primeras 3 componentes tienen la desviación estándar más alta, lo que significa
#que son las que que más variabilidad explican en los datos. Por el contrario, las componentes
#finales, especialmente la 6, tienen la desviación estándar más baja, lo que indica que capturan
#muy poca variabilidad.
#Con las primeras 3 componentes alcanzamos el 93.58% de la variabilidad

screeplot(test.pca)
names(test.pca)

#Seleccionamos solo las componentes que necesitamos
test.pca$rotation[,1:3]

# Observamos como los valores son positivos en el primer componente para lCO2, lGDPc e internet
# Esto indica que están positivamente correlacionados con el primer componente (lCO2), y por lo tanto
# tienden a aumentar en la misma dirección, mientras que el resto de  componentes contribuyen en la dirección
# opuesta a este primer componente.

biplot(test.pca)

#PC1 representa un eje de desarrollo económico. Las variables más alineadas con dicho desarrollo
#son: lGDPc, internet, lCO2, y hay una fuerte correlación entre ellas, siendo muy semejantes.
#Inflación, agricultura y crecimiento están más presentes en países menos desarrollados.
#PC2 en cambio no es tan relevante ni fácil de interpretar.

# Añadimos la región al resultado del PCA
pca_data <- data.frame(test.pca$x[, 1:2], Region = dat$Region)

library(ggplot2)

ggplot(pca_data, aes(x = PC1, y = PC2, color = Region)) +
  geom_point(size = 3) +
  labs(title = "PCA - Primeras dos componentes", x = "PC1", y = "PC2") +
  theme_minimal()

#Este gráfico muestra una representación de los países proyectados en las dos primeras componentes
#principales (PC1 y PC2), coloreados según su continente (Region), lo que nos permite identificar patrones
#regionales y agrupamientos.

#PC1 capta el eje desarrollo económico y separa claramente regiones como Europa y África.
#Europa y Oceanía se agrupan a la derecha, lo que implica un alto desarrollo, por el contrario, África y
#parte de Asia están a la izquierda por contar con un desarrollo inferior.
#Además, América y Asia tienen diversidad interna alta, pues incluyen tanto países ricos como pobres.
