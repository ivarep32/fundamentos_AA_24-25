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
