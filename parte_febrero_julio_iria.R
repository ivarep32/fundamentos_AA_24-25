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

