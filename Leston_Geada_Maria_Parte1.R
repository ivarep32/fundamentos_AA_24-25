#EXAMEN FINAL PARTE 1

#Personalización del examen
midni = 1003
set.seed(midni)
A1= midni %% 2
A2= sample(1:25,3)
load("KorTemp.RData")
lmues=sample(1:nrow(KorTemp),300)


#EJERCICIO 1
#Ajustamos por el método de mínimos cuadrados el siguiente modelo de regresión múltiple:
#Y = β0 + β1X1 + β2X2 + β3X3 + β4X4 + β5X5 + β6X6 + ε
#donde: Y = FTMax; X1 = LWS; X2 = LRadSol; X3 = LLH; X4 = LTMax; X5 = LRHmax y X6 = LTmin
z = lm(KorTemp$FTMax ~ KorTemp$LWS + KorTemp$LRadSol + KorTemp$LLH + KorTemp$LTMax + KorTemp$LRHmax + KorTemp$LTmin); z

#a)Obtén los coeficientes estimados del modelo, así como intervalos de confianza al 95 %
#para los coeficientes del modelo. Calcula los valores ajustados del modelo y calcula la
#suma residual de cuadrados

#COEFICIENTES ESTIMADOS DE FORMA AUTOMATICA
coef(z)

#COEFICIENTES ESTIMADOS DE FORMA MANUAL
X <- cbind(1, KorTemp$LWS, KorTemp$LRadSol, KorTemp$LLH, KorTemp$LTMax, KorTemp$LRHmax, KorTemp$LTmin)
y <- KorTemp$FTMax
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

#b)Calcula el valor del coeficiente de correlación de Pearson entre la variable respuesta Y
#y el predictor X1.
cor(KorTemp$LWS, y = KorTemp$FTMax, method = "pearson")

#c)Define y calcula el coeficiente de correlación parcial entre X1 y X2, controlando por el
#resto de las variables explicativas.

#ajustamos el modelo lineal de LWS sobre las otras variables y calculamos los residuos
r_LWS = residuals(lm(KorTemp$LWS ~ KorTemp$FTMax + KorTemp$LLH + KorTemp$LTMax + KorTemp$LRHmax + KorTemp$LTmin))
# ajustamos el modelo lineal de LRadSol sobre las mismas variables, obteniendo también los residuos
r_RadSol = residuals(lm(KorTemp$LRadSol ~ KorTemp$FTMax + KorTemp$LLH + KorTemp$LTMax + KorTemp$LRHmax + KorTemp$LTmin))
#coeficiente de correlación entre los residuos de ambos ajustes
cor(r_LWS, r_RadSol)

# Vemos que el coeficiente es positivo, con lo que las conclusiones de una regresión múltiple irían en
# sentido contrario a las de una regresión múltiple para estas variables.
# El efecto de la Velocidad de viento (LWS) sobre la Radiación solar en positivo.

#d)Considera un modelo reducido que contiene solo un subconjunto de las variables explicativas
#incluidas en el modelo completo. Formula y contrasta, mediante un test t, la hipótesis nula de
#que los coeficientes asociados a las variables excluidas son iguales a cero.

# Ajustamos el modelo completo
full_model <- lm(KorTemp$FTMax ~ KorTemp$LWS + KorTemp$LRadSol + KorTemp$LLH + KorTemp$LTMax + KorTemp$LRHmax + KorTemp$LTmin)

# Resumen del modelo completo
summary(full_model)

# Ajustamos el modelo reducido (Sin X6)
reduced_model <- lm(KorTemp$FTMax ~ KorTemp$LWS + KorTemp$LRadSol + KorTemp$LLH + KorTemp$LTMax + KorTemp$LRHmax)

# Resumen del modelo reducido
summary(reduced_model)

# Formulando la hipótesis de que el modelo podría explicar la misma varianza de
# FTMax siendo su coeficiente X6 igual a 0, observamos que el p-valor mostrado en
# el summary full_model es < 2.2e-16, esto implica que las  probabilidades de que
# la hipótesis nula sea cierta son bajas, por lo que podríamos concluír que la
# variable X6 hace una aportación importante en nuestro modelo.


#e) Compara el modelo completo con el modelo reducido mediante un test F. Expón claramente
# la hipótesis nula, calcula el estadístico de contraste, determina el valor crítico
# correspondiente y extrae las conclusiones en función del resultado del test.

# Para contrastar la variable excluida X6, usamos la función anova:
anova(reduced_model, full_model)

# Observamos que el p-valor es extremadamente bajo (igual que en el test-t), con
# esto anova nos está indicando que debemos RECHAZAR la hipótesis nula; es decir,
# la variable excluida (X6), resulta relevante para el ajuste del modelo.

#Estadístico de contraste = 434.99
#Valor crítico < 2.2e-16

#En este caso el nivel crítico coincide con la significación del coeficiente asociado a X6,
#pues es la única variable que se suprime de un modelo al otro.

#f) Compara el coeficiente de determinación ajustado para ambos modelos.

summary(reduced_model) # R^2 = 0.473
summary(full_model) # R^2 = 0.502

# Al comprobar los coeficientes de determinación de ambos modelos, podemos comprobar
# que el modelo modelo completo explica mejor la varianza de FTMax (0.502 > 0.473),



#EJERCICIO 2
#Calcula las componentes principales de las variables del último período disponible (las que empiezan por L).
#Dibuja las puntuaciones de las dos primeras componentes respecto al mes (mes). Interpreta
#las componentes que expliquen conjuntamente más del 75 % de la variabilidad.

datL <- KorTemp[, grep("^L", names(KorTemp))]
test.pca <- prcomp(na.omit(datL), scale. =T)
summary(test.pca)

#Empleamos prcomp() porque los datos tienen diferentes unidades de medida.
#Vemos como las primeras 4 componentes tienen la desviación estándar más alta, lo que significa
#que son los que que más variabilidad explican en los datos.Por el contrario, las componentes
#finales, especialmente la 9, tienen la desviación estándar más baja, lo que indica que capturan
#muy poca variabilidad.
#Con las primeras 4 componentes alcanzamos el 78.72% de la variabilidad

screeplot(test.pca)
names(test.pca)

test.pca$rotation[,1:4]

# Observamos como los valores son positivos en el primer componente para LTMax, LTMin, LTMax_D y LTmin_D
# Esto indica que están positivamente correlacionados con el primer componente (LTMax), y por lo tanto
# tienden a aumentar en la misma dirección, mientras que el resto de  componentes contribuyen en la dirección
# opuesta a este primer componente.

biplot(test.pca)

#Las temperaturas min y max están relacionadas, lo cual es lógico ya que cuanta más calor menos temperaturas
#bajas habrá.

#Por otra parte, los valores de la humedad relativa min y max también se agrupan, siendo semejantes,
#además de que la velocidad de viento influye en ellas, aumentando y disminuyendo a la vez que estas dos.

pt <- data.frame(test.pca$x[, 1:2], mes = KorTemp$mes)

# Puntuaciones coloreadas de cada mes de las 2 primeras variables
library(ggplot2)

ggplot(pt, aes(x = PC1, y = PC2, color = mes)) +
  geom_point(alpha = 0.5) +
  theme_minimal() +
  labs(title = "Puntuaciones coloreadas de cada mes")


# En el gráfico vemos como se reparten los datos entre los meses de julio (azul) y agosto (rojo).
# En este último mes, la nube es más dispersa y tiende a menores valores de ambas componentes.