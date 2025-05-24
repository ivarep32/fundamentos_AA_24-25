setwd('C:/Users/marti/Desktop/FUND/Fundamentos/Fundamentos/MaterialesFinal/datos2')

datos1 <- read.csv('OTERO_TOURIS,_MARTIN__statex77.csv')

summary(datos1)

library(ggplot2)

##########################################
#               EJERCICIO 1              #
##########################################

# a) Ajuste de modelo de regresión lineal simple:
modelo_1 <- lm(Life.Exp ~ Murder, data=datos1)

# a.1) Diagrama de dispersión de los datos con la recta estimada:
plot(datos1$Murder, datos1$Life.Exp)
abline(modelo_1, col="green")

# a.2) Gráfico para el análisis de residuos:

plot(modelo_1$residuals) # Plot básico de los residuos

# Residuos VS Valores ajustados
plot(modelo_1, which=1)

# Este gráfico muestra en el eje X los valores predichos por el modelo y en
# el eje Y los valores residuales del modelo.
# Vemos que los valores, aun sin ser 100% homocedásticos, no siguen un claro
# patrón de embudo o curva, por lo que no podemos afirmar que sean heterocedásticos.

# b) Valor e interpretación del coeficiente de determinación.

summary(modelo_1)
# El coeficiente de determinación se corresponde con el R-Squared
# obtenido del summary de nuestro modelo. Podemos comprobar que su
# valor es 0.5601, por lo que afirmamos que podemos explicar el 56,01% de
# la varianza de Life.Exp mediante los valores de Murder.

# c) IC para la predicción para 9 asesinatos.

nuevos_datos <- data.frame(Murder = 9) # Nuevos datos a predecir
predict(modelo_1, nuevos_datos, interval = "prediction")

# El intervalo nos muestra que, para un nº de asesinatos igual a 9, hay un 95%
# de probabilidades de que la esperanza de vida se encuentre entre los 68.59
# y los 72.29 años; siendo 70.44 años la media del intervalo.

##########################################
#               EJERCICIO 2              #
##########################################

# Ajuste del modelo de regresión múltiple:

modelo_2 <- lm(Life.Exp ~ Murder + Population + HS.Grad + Area + Income, data=datos1)

summary(modelo_2)

# a) Obtención de los coeficientes y el p-valor de los coeficientes tanto automática como manualmente:

# a.1) Obtención de los coeficientes manual y automáticamente.

coef(modelo_2)
modelo_2$coefficients # Forma automática

# Extraer coeficientes de forma manual (mínimos cuadrados):

Y <- datos1$Life.Exp  # Variable dependiente
X <- cbind(1, datos1$Murder, datos1$Population, datos1$HS.Grad, datos1$Area, datos1$Income)  # Matriz de diseño (con columna de 1's para el intercepto)
colnames(X) <- c("Intercept", "Murder", "Population", "HS.Grad", "Area", "Income")
XtX <- t(X) %*% X  # X^T * X
XtX_inv <- solve(XtX)  # (X^T X)^-1
XtY <- t(X) %*% Y  # X^T * Y
beta_manual <- XtX_inv %*% XtY
rownames(beta_manual) <- c("Intercept", "Murder", "Population", "HS.Grad", "Area", "Income")
beta_manual

# El p-valor lo podemos observar también en el summary, pues es la columna de Pr(>F)
summary(modelo_2)

# Observamos que variables como "area" o "income" cuentan con un p-valor muy alto,
# lo que significa que es muy probable que el modelo explique la misma varianza
# de Life.Exp, o incluso más, si no contase con esas variables.

# Observamos también que variables como "murder" tienen un p-valor muy bajo,
# lo que, al contrario que con las anteriores, supone que es una variable
# muy significativa para el ajuste del modelo.

# b) Obtención de los valores ajustados manual y automáticamente.

modelo_2$fitted.values
fitted(modelo_2) # Así los obtenemos de forma automática.

# Para la forma manual, debemos recordar que arriba tenemos definida a "X" como
# la matriz de diseño y a "beta_manual" como los coeficientes de nuestro modelo.

fitted_manual <- X %*% beta_manual

head(fitted_manual, 3)
head(modelo_2$fitted.values, 3) # Comprobamos que son los mismos

# c) Obtención de IC al 99% para los parámetros del modelo ajustado.

confint(modelo_2, level = 0.99)

# d) Obtención de la matriz de diseño y la matriz hat del modelo.

# La matriz de diseño la usamos anteriormente para los coeficientes manuales:
X
# La forma rápida de obtenerla con R sería:
model.matrix(modelo_2)

# La matriz hat se calcula de la forma:

H <- X %*% XtX_inv %*% t(X)
head(H)

# De forma automática con R podemos hacer:
hat(model.matrix(modelo_2)) # Pero solo devuelve la diagonal

# e) Obtención del RSE a partir de los Residuals

# RSE: Representa la dispersión promedio de las observaciones alrededor de la línea de regresión

RSS <- sum(residuals(modelo_2)^2)

# Grados de libertad
n <- nrow(datos1) # Número de observaciones
p <- length(coef(modelo_2))  # Número de parámetros
df <- n - p  # Grados de libertad residuales

# Estimación de sigma cuadrado
sigma2 <- RSS / df

# Residual Standard Error (RSE)
RSE <- sqrt(sigma2) ; RSE

# Cuanto mas RSE: más dispersos están los puntos -> Mayor error en predicciones
# Cuanto menos RSE: menos dispersos están los puntos -> Mejor ajuste del modelo

summary(modelo_2) # Dá lo mismo manualmente que en el summary.

# f) Coeficiente de correlación Life.Exp - Income

cor(datos1$Life.Exp, datos1$Income)
# El valor de 0.29 implica que las variables están poco correladas positivamente.

# g) Coef. de correl. parcial entre Life.Exp e Income, teniendo en cuenta todas las demás.

res_lif <- residuals(lm(Life.Exp ~ Murder + Population + HS.Grad + Area, data = datos1))
res_inc <- residuals(lm(Income ~ Murder + Population + HS.Grad + Area, data = datos1))

cor(res_lif, res_inc) 
# El valor de -0.05 implica que la relación entre Life.Exp e Income depende
# altamente del resto de variables del modelo.

# h) Modelo reducido (Sin Income)

modelo_3 <- lm(Life.Exp ~ Murder + Population + HS.Grad + Area, data = datos1)

# h.1) Analizar si modelo_2 se podría simplificar a modelo_3 comprobando
# el valor de su test-t

summary(modelo_2)
# Siendo H0 la hipótesis de que el modelo podría explicar la misma varianza de
# Life.Exp siendo su coeficiente (beta sub income) igual a 0, observamos que el
# p-valor mostrado en el summary del modelo_2 es de 0.7146, esto implica que las
# probabilidades de que la hipótesis nula sea cierta son MUY altas, por lo que
# podríamos concluír que la variable "Income" sobra o no aporta nada a nuestro modelo.

# h.2) Aplicar un test para contrastar si modelo_2 se puede reducir a modelo_3.

anova(modelo_3, modelo_2)

# Observamos que el p-valor es extremadamente alto (igual que en el test-t), con
# esto anova nos está indicando que debemos ACEPTAR la hipótesis nula; es decir,
# ninguna de las variables adicionales del modelo completo (en este caso, sólo Income),
# resulta relevante para el ajuste del modelo.

# h.3) Comparar los coeficientes de determinación múltiples ajustados de cada modelo:

summary(modelo_2) # R^2 = 0.5828

summary(modelo_3) # R^2 = 0.5908

# Al comprobar los coeficientes de determinación de ambos modelos, podemos
# comprobar que el segundo modelo, sin la variable Income, explica mejor
# la varianza de Life.Exp (0.5908 > 0.5828),


##########################################
#               EJERCICIO 3              #
##########################################

datos2 <- read.csv('OTERO_TOURIS,_MARTIN__protein.csv')
summary(datos2)

# Primero, preparamos los datos para sólo tener las 9 variables pedidas:
dat <- cbind(datos2$Red_Meat, datos2$White_Meat, datos2$Eggs, datos2$Milk,
             datos2$Fish, datos2$Cereals, datos2$Starchy_Foods,
             datos2$Pulses_nuts_oilseeds, datos2$Fruits_Vegetables)

# a) Efectuar el PCA y comentar resultados:

# Generamos el PCA con la matriz de correlaciones para evitar sesgos de escalas.
pca <- princomp(dat, cor = TRUE)

summary(pca)

# En el summary, podemos comprobar en la fila de "Cumulative Proportion" que tan
# sólo las dos primeras componentes logran explicar el 63% de la variabilidad. Las
# últimas componentes apenas explican un 1-2% de la variabilidad, por lo que se podrían
# considerar menos relevantes para el estudio.

# b) Interpretación de las dos primeras componentes:

# Como comentamos en el apartado anterior, las dos primeras componentes logran
# explicar un 63% de la variabilidad, es altamente probable que se trate de las
# componentes de tamaño y forma.
# Variabilidad explicada por Comp 1: 45.8%
# Variabilidad explicada por Comp 2: 17.8%

pca$loadings

# La primera componente tiene peso positivo (0.42) de huevos, y también de leche (0.38)
# por lo que esta componente se relaciona más con productos lácteos o proteicos
# En la segunda componente comprobamos pesos negativos fuertes de pescado (-0.68) y frutas y vegetales (-0.48)
# por lo que podríamos considerar que se relaciona más con los cereales

# Obtención de la variabilidad explicada de forma manual:

# Extraer autovalores (varianzas de las componentes)
autovalores <- pca$sdev^2 ; autovalores

# Proporción de varianza explicada
prop_var <- autovalores/sum(autovalores) ; prop_var
# Como podemos comprobar, coinciden con las del summary.

# c) Gráficos y resultados comentados:

# Gráfico de sedimentación (scree plot)
screeplot(pca, type = "lines", main = "Scree Plot")

# Este gráfico muestra la variabilidad explicada por cada componente,
# de mayor a menor

# Debemos buscar el "codo" donde la pendiente se suaviza,
# como este se encuentra en Comp.3-Comp.4, deberíamos retener
# entre las 2 y 3 primeras componentes para nuestro análisis

# Biplot (observaciones y variables)
biplot(pca, cex = 0.7)

# Los ángulos entre las flechas nos muestran la correlación entre las
# variables. Así pues, podemos confirmar que variables como la 2 y la 4
# están correladas positivamente
# La longitud de las flechas nos indican qué variables son más
# influyentes. Debido a esto podemos considerar la variable 6
# como una de las que más influyen en las componentes 1 y 2.
# Además, aquellos puntos que se encuentren cercanos en el
# plano se corresponden con observaciones similares.
