# Fundamentos de Aprendizaje Automático - Examen Mayo 2025
# Sofía Vergara

# Inicialización
midni <- xxxx
set.seed(midni)
A1 <- midni %% 2
A2 <- sample(1:25, 3)
load("KorTemp.RData")
lmues <- sample(1:nrow(KorTemp), 300)

# Ejercicio 1
# a) Modelo de regresión múltiple para predecir FTMax
modelo_completo <- lm(FTMax ~ LWS + LRadSol + LLH + LTMax + LRHmax + LTmin, data = KorTemp)
summary(modelo_completo)

# Coeficientes e intervalos de confianza
confint(modelo_completo)

# Valores ajustados y suma residual de cuadrados
valores_ajustados <- fitted(modelo_completo)
SSE <- sum(resid(modelo_completo)^2)

# b) Correlación de Pearson entre FTMax y LWS
cor(KorTemp$FTMax, KorTemp$LWS, use = "complete.obs")

# c) Correlación parcial entre LWS y LRadSol controlando el resto
library(ppcor)
pcor.test(KorTemp$LWS, KorTemp$LRadSol,
          KorTemp[, c("LLH", "LTMax", "LRHmax", "LTmin", "FTMax")])

# d) Modelo reducido eliminando LLH y LRHmax
modelo_reducido <- lm(FTMax ~ LWS + LRadSol + LTMax + LTmin, data = KorTemp)

# Contraste t para coeficientes eliminados
summary(modelo_completo)$coefficients[c("LLH", "LRHmax"), ]

# e) Comparación con test F
anova(modelo_reducido, modelo_completo)

# f) R² ajustado para ambos modelos
summary(modelo_completo)$adj.r.squared
summary(modelo_reducido)$adj.r.squared

# Interpretación: comparar coeficientes, significancia, SSE, R², etc.

# Ejercicio 2
# Componentes principales del último periodo
datos_L <- KorTemp[, grep("^L", names(KorTemp))]
pca <- prcomp(na.omit(datos_L), scale. = TRUE)
summary(pca)

# Puntuaciones de las dos primeras componentes
pca_scores <- data.frame(pca$x[, 1:2], mes = KorTemp$mes)
library(ggplot2)
ggplot(pca_scores, aes(x = PC1, y = PC2, color = mes)) +
  geom_point() + theme_minimal() +
  labs(title = "PCA: Componentes principales por mes")

# Interpretación: ver proporción de varianza explicada

# Ejercicio 3
# Regresión con selección automática (stepwise AIC)
target <- if (A1 == 0) "FTMax" else "FTmin"
formula <- as.formula(paste(target, "~", paste(names(datos_L), collapse = "+")))
modelo_step <- step(lm(formula, data = KorTemp), direction = "both")
summary(modelo_step)

# Comentarios sobre variables seleccionadas, AIC, significancia

# Ejercicio 4
# Regresión no lineal sobre subconjunto lmues
datos_lmues <- KorTemp[lmues, ]
modelo_nl <- nls(as.formula(paste(target, "~ a + b * log(LRadSol)")),
                 data = datos_lmues, start = list(a = 0, b = 1))
summary(modelo_nl)

# Gráfico
ggplot(datos_lmues, aes(x = LRadSol, y = get(target))) +
  geom_point() +
  stat_function(fun = function(x) coef(modelo_nl)["a"] + coef(modelo_nl)["b"] * log(x), color = "blue") +
  theme_minimal() + labs(title = "Modelo no lineal")

# Ejercicio 5
# Filtrado por estaciones A2
datos_A2 <- subset(KorTemp, Est %in% A2)
datos_A2$Est <- droplevels(datos_A2$Est)

# Clasificación - Regla de Bayes (naiveBayes)
library(e1071)
modelo_bayes <- naiveBayes(Est ~ LTMax + LRHmax + LWS + LLH, data = datos_A2)
pred_bayes <- predict(modelo_bayes, datos_A2)
error_bayes <- mean(pred_bayes != datos_A2$Est)

# Clasificación - Regresión logística multinomial
library(nnet)
modelo_multinom <- multinom(Est ~ LTMax + LRHmax + LWS + LLH, data = datos_A2)
pred_multinom <- predict(modelo_multinom, datos_A2)
error_multinom <- mean(pred_multinom != datos_A2$Est)

# Resultados
error_bayes
error_multinom

# Justificación de técnicas y comparación de errores
