#importamos y observamos los datos
csv <- read.csv("w2011.csv", sep = ";", dec=',', header = TRUE)
summary(csv)
str(csv)
head(csv)

# 3. Estima un modelo de regresión no paramétrico para explicar lCO2 con la variable internet
# seleccionando el parámetro ventana de forma óptima. Dibuja el modelo de regresión obtenido. A la vista de la gráfica, ¿podemos suponer que existe una relación lineal entre ambas
# variables? (1 pto).

# paquete necesario
install.packages("np", dep=TRUE)
library(np)

# ajuste no parametrico
modelo_np <- npreg(lCO2 ~ internet, data = csv, regtype = "lc")

# Gráfico del ajuste
plot(csv$internet, csv$lCO2, main = "regresion no parametrica", xlab = "% Internet", ylab = "log(CO2)")
lines(csv$internet[order(csv$internet)],
      fitted(modelo_np)[order(csv$internet)], col = "blue", lwd = 2)

# Este gráfico muestra una relación no lineal entre el el número de usuarios con internet y las emisiones de CO2 per cápita
# dibujando una función logarítmica, de nuevo esto podría reflejar el desarrollo económico, ya que una mayor disponibilidad
# y uso de internet tiende a reflejar un país más desarrollado económicamente, pero el desarrollo económico una vez se alcanza
# cierto nivel, se tiende a prescindir de la industria y centrarse en el sector servicios eso explicaria por que la grafica
# se aplana. En cualquer caso el suo de modelos no paramétricos es más adecuado en este caso por suponer una relación lineal
# entre ambas variables.


# ==================================================================================================================== #

# 4. Estimar un modelo lineal para explicar lCO2 en función del resto de variables numéricas con
# el objetivo de usar el menor número de covariables. (1 pto).

# modelo completo
modelo <- lm(lCO2 ~ inflation + lGDPc + GDP.growth + internet + lagv, data = csv)
summary(modelo)
# Coefficients:
#               Estimate Std. Error t value Pr(>|t|)
# (Intercept) -5.7320783  1.2742646  -4.498 1.33e-05 ***
# inflation    0.0265514  0.0097339   2.728  0.00711 **
# lGDPc        0.7935896  0.1326396   5.983 1.45e-08 ***
# GDP.growth  -0.0093481  0.0156030  -0.599  0.54996
# internet     0.0001399  0.0052317   0.027  0.97870
# lagv        -0.2869058  0.1388375  -2.066  0.04044 *
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
# Residual standard error: 0.7197 on 156 degrees of freedom
# Multiple R-squared:  0.8048,    Adjusted R-squared:  0.7986
# F-statistic: 128.7 on 5 and 156 DF,  p-value: < 2.2e-16

# En esta tabla observamos las variables con menor valor estadístico (más prescindibles), por tener menor pvalor son:
# Internet (0.97) y GDP.growth(0.55).


#modelo sin las variables no significativas
modelo_reducido <- lm(lCO2 ~ inflation + lGDPc + lagv, data = csv)
summary(modelo_reducido)

# anova para comparar ambos modelos
anova(modelo_reducido, modelo)
# Model 1: lCO2 ~ inflation + lGDPc + lagv
# Model 2: lCO2 ~ inflation + lGDPc + GDP.growth + internet + lagv
#   Res.Df    RSS Df Sum of Sq      F Pr(>F)
# 1    158 81.002
# 2    156 80.809  2    0.1929 0.1862 0.8303

# El test F tiene un p-valor de 0.83, no podriamos decir que añadir GDP.growth e internet mejore el modelo.
# Es decir , el modelo reducido es suficiente

# comprobamos tambien el R2
summary(modelo)$adj.r.squared # 0.799
summary(modelo_reducido)$adj.r.squared #0.800

# vemos que el cambio es mínimo, se vuelve a demostrar que el modelo
# reducido es suficiente

# ==================================================================================================================== #

# 5. Elaborar dos modelos de clasificación (uno basado en Bayes y otro en métodos de regresión)
# para clasificar el continente (Region) en función de las variables numéricas. Para estos modelos
# excluye los países de América (Region==AM) y Oceanía (Region==OC). Comenta
# sobre los países europeos que se clasificarían mal y aplica el modelo de clasificación para
# los países que NO han entrado en el modelo (América y Oceanía) (1 pto).

# quitamos america y oceania
datos_filtrados <- subset(csv, !(Region %in% c("AM", "OC")))
table(datos_filtrados$Region) #para ver que no sale ni AM ni OC

# recogemos las variables numericas
vars_num <- c("inflation", "lGDPc", "GDP.growth", "internet", "lagv")

# usamos region como factor
datos_filtrados$Region <- as.factor(datos_filtrados$Region)

# NAIVE BAYES
library(e1071)
modelo_nb <- naiveBayes(Region ~ ., data = datos_filtrados[, c("Region", vars_num)])
pred_nb <- predict(modelo_nb, datos_filtrados)

# LDA
library(MASS)
modelo_lda <- lda(Region ~ ., data = datos_filtrados[, c("Region", vars_num)])
pred_lda <- predict(modelo_lda)$class

# Matrices de confusión
table(Predicho = pred_nb, Real = datos_filtrados$Region)
table(Predicho = pred_lda, Real = datos_filtrados$Region)

# Podemos ver que LDA tiene un rendimiento un poco superior a Naive Bayes.
# En Europa, LDA acierta con 34 y NB acierta 33, pero se equivoca más con 13 fallos.

# En África, LDA acierta 38 veces frente a los 37 de Naive Bayes.

# En Asia , LDA mejora sustancialmente el acierto (LDA - 16 vs NB - 10) y reduce errores frente a Naive Bayes.

# En general, podemos afirmar que comete menos errores LDA y tiene menos casos cruzados

# paises europeos mal clasificados (Region == "EU").
datos_filtrados$Mal_NB <- pred_nb != datos_filtrados$Region
datos_filtrados$Mal_LDA <- pred_lda != datos_filtrados$Region
subset(datos_filtrados, Region == "EU" & (Mal_NB | Mal_LDA))

# En total, 7 países europeos fueron mal clasificados por al menos uno de los modelos.
# Naive Bayes falla en 6, LDA en 5.

# Países como Georgia, Moldova, Montenegro y Ukraine son mal clasificados por ambos modelos,
# lo que sugiere que sus características numéricas
# (por ejemplo, bajo PIB per cápita, alto peso del sector agrícola o bajo acceso a internet)
# se asemejan más a países de Asia o África que al perfil medio europeo
#
# En cambio, países como Serbia o Albania solo fallan en un modelo, lo que indica que están en
# el límite entre regiones desde el punto de vista de sus variables
#
# En conjunto, LDA comete menos errores en Europa y parece ofrecer una clasificación más robusta que Naive
# Bayes para esta región


# hacemos el estudio con AM y OC
datos_test <- subset(csv, Region %in% c("AM", "OC"))
pred_test_nb <- predict(modelo_nb, newdata = datos_test[, vars_num])
pred_test_lda <- predict(modelo_lda, newdata = datos_test[, vars_num])

# predicciones
cbind(Pais = datos_test$country, Region_Real = datos_test$Region,
      Pred_NB = pred_test_nb, Pred_MN = pred_lda)
# Se observa una gran dispersión en las predicciones. Naive Bayes tiende a clasificarlos como europeos (3)
# LDA los reparte entre África (1), Asia (2) y Europa (3)
# Esto indica que los modelos no generalizan bien a regiones no vistas y que las variables usadas
# no capturan con claridad las diferencias regionales en estos casos.
