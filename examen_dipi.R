# -------------------------------
# Ejercicio 1: Regresión Múltiple
# -------------------------------

# Cargar los datos
datos <- read.csv("v2011.csv")

# 1a) Modelo completo
modelo_completo <- lm(1002 ~ inflation + 10PPc + GDP.growth + internet + lagv, data = datos)

# Resultados del modelo
summary(modelo_completo)
confint(modelo_completo, level = 0.95)

# Valores ajustados y suma residual
valores_ajustados <- fitted(modelo_completo)
src <- sum(resid(modelo_completo)^2)

# 1b) Correlación de Pearson
cor_pearson <- cor(datos$1002, datos$inflation, method = "pearson")
print(paste("Correlación Pearson entre Y y X1:", cor_pearson))

# 1c) Correlación parcial
library(ppcor)
cor_parcial <- pcor.test(datos$inflation, datos$10PPc,
                        cbind(datos$GDP.growth, datos$internet, datos$lagv, datos$1002))
print("Correlación parcial entre X1 y X2:")
print(cor_parcial)

# 1d) Modelo reducido y test t
modelo_reducido <- lm(1002 ~ 10PPc + internet + lagv, data = datos)
coef_excluidos <- summary(modelo_completo)$coefficients[c("inflation", "GDP.growth"),]
print("Coeficientes de variables excluidas:")
print(coef_excluidos)

# 1e) Test F para comparación de modelos
test_f <- anova(modelo_reducido, modelo_completo)
print("Resultado del test F:")
print(test_f)

# 1f) R² ajustado
r2_completo <- summary(modelo_completo)$adj.r.squared
r2_reducido <- summary(modelo_reducido)$adj.r.squared
print(paste("R² ajustado completo:", r2_completo))
print(paste("R² ajustado reducido:", r2_reducido))

# -------------------------------
# Ejercicio 2: Componentes Principales
# -------------------------------

# Seleccionar variables numéricas
datos_numericos <- datos[, !names(datos) %in% c("country", "Region")]

# Análisis de componentes principales
pca <- prcomp(datos_numericos, scale. = TRUE)

# Resumen de varianza explicada
summary_pca <- summary(pca)
print("Varianza explicada por componentes:")
print(summary_pca)

# Gráfico de componentes
library(ggplot2)
pca_data <- data.frame(pca$x, Region = datos$Region)
ggplot(pca_data, aes(PC1, PC2, color = Region)) +
  geom_point() +
  ggtitle("Primeras dos componentes principales") +
  theme_minimal()

# -------------------------------
# Ejercicio 3: Regresión No Paramétrica
# -------------------------------

library(np)
# Estimación del parámetro ventana óptimo
bw <- npregbw(1002 ~ internet, data = datos)
modelo_np <- npreg(bw)

# Gráfico de la regresión
plot(modelo_np,
     main = "Regresión no paramétrica: CO2 vs Internet",
     xlab = "% Usuarios de Internet",
     ylab = "log Emisiones CO2 per cápita",
     plot.errors.method = "bootstrap")

# -------------------------------
# Ejercicio 4: Modelo Lineal Óptimo
# -------------------------------

library(MASS)
# Selección paso a paso
modelo_step <- stepAIC(lm(1002 ~ ., data = datos_numericos), direction = "both")
print("Modelo óptimo por stepAIC:")
summary(modelo_step)

# -------------------------------
# Ejercicio 5: Modelos de Clasificación
# -------------------------------

# Filtrar datos (excluir América y Oceanía)
datos_clas <- subset(datos, !Region %in% c("MV", "OC"))

# Modelo de Bayes
library(e1071)
modelo_bayes <- naiveBayes(Region ~ inflation + 10PPc + GDP.growth + internet + lagv,
                          data = datos_clas)

# Predicciones y evaluación
pred_bayes <- predict(modelo_bayes, datos_clas)
table(pred_bayes, datos_clas$Region)

# Modelo de regresión logística multinomial
library(nnet)
modelo_multinom <- multinom(Region ~ .,
                           data = datos_clas[, !names(datos_clas) %in% c("country")])

# Predicción para países excluidos
paises_excluidos <- subset(datos, Region %in% c("MV", "OC"))
predicciones <- predict(modelo_multinom, newdata = paises_excluidos)

# Resultados de clasificación
print("Predicciones para América y Oceanía:")
table(predicciones, paises_excluidos$Region)

# Países europeos mal clasificados
pred_europa <- predict(modelo_multinom, newdata = subset(datos_clas, Region == "EU"))
errores_europa <- subset(datos_clas, Region == "EU" & pred_europa != Region)
print("Países europeos mal clasificados:")
print(errores_europa[, c("country", "Region")])