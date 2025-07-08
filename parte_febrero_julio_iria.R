datos <- read.csv("C:/Users/Iria/Downloads/w2011.csv", header = TRUE, sep=";", dec=',')

# EJERCICIO 3
# paquete necesario
library(np)

# ajuste no parametrico
modelo_np <- npreg(lCO2 ~ internet, data = datos, regtype = "lc")

# Gráfico del ajuste
plot(datos$internet, datos$lCO2, main = "regresion no parametrica", xlab = "% Internet", ylab = "log(CO2)")
lines(datos$internet[order(datos$internet)],
      fitted(modelo_np)[order(datos$internet)], col = "blue", lwd = 2)

# El gráfico muestra una relación claramente no lineal entre el porcentaje de
# usuarios de internet y las emisiones de CO₂ per cápita (logarítmicas).
# En niveles bajos de acceso, el aumento del uso de internet se asocia con un fuerte
# incremento de emisiones, mientras que en niveles altos este efecto se estabiliza.
# Por tanto, no es apropiado suponer una relación lineal entre ambas variables, y el
# uso de modelos no paramétricos es más adecuado en este caso.

#EJERCICIO 4

# modelo completo
modelo <- lm(lCO2 ~ inflation + lGDPc + GDP.growth + internet + lagv, data = datos)
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

# viendo esta tabla, podemos eliminar las variables de menor valor estadistico,
# es decir aquellas de mayor pvalor, siendo estas Internet (0.97) y GDP.growth(0.55).

#modelo sin las variables no significativas
modelo_reducido <- lm(lCO2 ~ inflation + lGDPc + lagv, data = datos)
summary(modelo_reducido)

# anova para comparar ambos modelos
anova(modelo_reducido, modelo)
# Model 1: lCO2 ~ inflation + lGDPc + lagv
# Model 2: lCO2 ~ inflation + lGDPc + GDP.growth + internet + lagv
#   Res.Df    RSS Df Sum of Sq      F Pr(>F)
# 1    158 81.002
# 2    156 80.809  2    0.1929 0.1862 0.8303

#El test F muestra un p-valor de 0.83, no podriamos decir que añadir GDP.growth e
# internet mejore el modelo. Por tanto, el modelo reducido es suficiente

# comprobamos tambien el R2
summary(modelo)$adj.r.squared # 0.799
summary(modelo_reducido)$adj.r.squared #0.800

# vemos que el cambio es mínimo, se vuelve a demostrar que el modelo
# reducido es suficiente

# EJERCICIO 5
# quitamos america y oceania
datos_filtrados <- subset(datos, !(Region %in% c("AM", "OC")))
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
# Observamos que LDA presenta un rendimiento ligeramente superior a Naive Bayes.
# En la clase Europa (EU): LDA clasifica correctamente 34 países, Naive Bayes 33, pero comete más errores
# (mal clasifica 13 países como EU).

# En África (AF), LDA acierta 38 casos frente a los 37 de Naive Bayes.

# En Asia (AS), LDA mejora sustancialmente el acierto (16 vs 10) y reduce errores frente a Naive Bayes.

# En general, LDA comete menos errores cruzados y distribuye mejor las clases.

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
datos_test <- subset(datos, Region %in% c("AM", "OC"))
pred_test_nb <- predict(modelo_nb, newdata = datos_test[, vars_num])
pred_test_lda <- predict(modelo_lda, newdata = datos_test[, vars_num])

# predicciones
cbind(Pais = datos_test$country, Region_Real = datos_test$Region,
      Pred_NB = pred_test_nb, Pred_MN = pred_lda)
#Se observa una gran dispersión en las predicciones. Naive Bayes tiende a clasificarlos como europeos (3)
# LDA los reparte entre África (1), Asia (2) y Europa (3)
# Esto indica que los modelos no generalizan bien a regiones no vistas y que las variables usadas
# no capturan con claridad las diferencias regionales en estos casos.