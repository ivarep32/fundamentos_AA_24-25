midni=(5190)
set.seed(midni)
A1= midni%% 2
A2= sample(1:25,3)
load("KorTemp.RData")
lmues=sample(1:nrow(KorTemp),300)

#=================================================================
#Calcula un módelo de regresión para explicar FTMax con las variables
# del último período disponible (empiezan por L), con el
# objetivo de usar el mejor modelo con el menor número de covariables.
# Explica el proceso seguido, las elecciones tomadas y los resultados obtenidos
#==================================================================
# seleccionamos las variables
datos_L <- KorTemp[, grep("^L", names(KorTemp))]
names(datos_L) #para ver como se llaman
modelo <- lm(FTMax ~ LTMax + LTmin + LRHmin + LRHmax + LTMax_D + LTmin_D + LLH + LRadSol, data=KorTemp)
summary(modelo)
# salida de summary:
#              Estimate Std. Error t value Pr(>|t|)
#(Intercept)  1.138e+00  4.547e-01   2.502 0.012363 *
#LTMax        1.662e-01  9.666e-03  17.196  < 2e-16 ***
#LTmin       -8.850e-04  1.303e-02  -0.068 0.945844
#LRHmin      -1.242e-03  2.805e-03  -0.443 0.657891
#LRHmax       6.878e-03  3.587e-03   1.917 0.055221 .
#LTMax_D      7.550e-01  1.611e-02  46.876  < 2e-16 ***
#LTmin_D      6.508e-02  2.032e-02   3.203 0.001364 **
#LLH          9.774e-03  6.434e-04  15.191  < 2e-16 ***
#LRadSol     -1.635e-04  4.725e-05  -3.460 0.000544 ***

# viendo esta tabla, podemos eliminar las variables de menor valor estadistico,
# es decir aquellas de mayor pvalor, siendo estas LTmin (0.95), LRHmin(0.66).
# LRHmax (0.055) esta por encima del umbral 0.01 pero es lo suficientemente
# relevante como para que quitarla empeore el modelo (demostracion mas adelante)

modelo_reducido <- lm(FTMax ~ LTMax + LRHmax + LTMax_D + LTmin_D + LLH + LRadSol, data=KorTemp)
modelo_reducido_alt <-  lm(FTMax ~ LTMax + LTMax_D + LTmin_D + LLH + LRadSol, data=KorTemp)

# hacemos un ftest para comprobar que el modelo reducido (sin LTmin y sin LRHmin) es lo suficientemente bueno
anova(modelo_reducido, modelo)
# vemos que aqui el p valor (0.9) es muy grande por tanto eliminar las variables no afecta significativamente al modelo
#completo

# hacemos un ftest para comprobar que el modelo reducido alternativo (sin LTmin, sin LRHmin y  sin LRHmax) es lo suficientemente bueno
anova(modelo_reducido_alt, modelo)
# aqui vemos que el p valor sigue siendo grande (0.27) está por encima del umbral típico (0.01) y por tanto podemos descartar
# tambien LRHmax sin empeorar significativamente nuestro modelo original. Aún así podría justificarse su uso

#==========================================================
#Seleccionando las ﬁlas del conjunto de datos dada por
# lmues: KorTemp[lmues,], estimar un modelo de regresión no lineal
# para explicar FTMax  usando la variable LRadSol. Comenta las
# opciones elegidas y dibuja el modelo de regresión obtenido sobre
# los datos originales.
#============================================================
library(mgcv) #para GAM

# datos 
lmues <- KorTemp[lmues,]

# formula de gam con spline en radsol
fmla_gam <- as.formula(paste("FTMax", "~ s(LRadSol)"))

# ajuste
modelo_gam <- gam(fmla_gam, data = lmues)

summary(modelo_gam)

library(ggplot2)

# prediccion de valores
lmues$pred <- predict(modelo_gam)
lmues$LRadSol_pred <- lmues$LRadSol

ggplot(lmues, aes(x = LRadSol, y = FTMax)) +
  geom_point(alpha = 0.4, color = "darkgrey") +
  geom_line(aes(y = pred), color = "blue", linewidth = 1.2) +
  theme_minimal() +
  labs(
    title = paste("Modelo GAM:", "FTMax", "~ s(LRadSol)"),
    x = "Radiacion Solar (LRadSol)",
    y = "FTMax"
  )

# ========================================================================
#Para las estaciones seleccionadas en A2,
# elaborar dos modelos de clasiﬁcación (uno basado en Regla de Bayes y
# el otro en técnicas de regresión) con las variables LTMax,
# LRHmax, LWS y LLH que intente clasiﬁcar los datos por estación.
# Justiﬁcar las elecciones tomadas y estimar el error de
# mala clasiﬁcación que tendríamos si aplicásemos los modelos a nuevos datos.
#=============================================================================

# incapaz de resolver este ejercicio no funciona
#========================================================================
#filtro A2
datos_A2 <- subset(KorTemp, Est %in% A2)
#datos_A2$Est <- droplevels(datos_A2$Est) #eliminamos niveles innecesarios

# tomamos las variables necesarias 
vars <- c("Est", "LTMax", "LRHmax", "LWS", "LLH")
datos_A2 <- datos_A2[, vars]

# experimento
library(e1071) #para naiveBayes
library(MASS) #para LDA

n <- nrow(datos_A2)
nrep <- 1000
prop <- 0.7
errclassif <- matrix(NA, nrow = nrep, ncol = 2)
colnames(errclassif) <- c("naiveBayes", "LDA")
for (i in 1:nrep) {
  ltrain <- sample(1:n, round(prop * n)) #indices entrenamiento
  test <- setdiff(1:n, ltrain) #indices test

  #naiveBayes
  m_nb <- naiveBayes(Est ~ ., data = datos_A2, subset = ltrain)
  p_nb <- predict(m_nb, newdata = datos_A2[test,])
  errclassif[i, 1] <- mean(datos_A2$Est[test] != p_nb)

  #LDA
  m_lda <- lda(Est ~ ., data = datos_A2, subset = ltrain) #modelo de datos
  p_lda <- predict(m_lda, datos_A2[test,])$class #prediccion
  errclassif[i, 2] <- mean(datos_A2$Est[test] != p_lda) #calculamos y almacenamos el error
}

boxplot(errclassif, ylab = "Error de clasificación", main = "Comparación Naive Bayes vs LDA")
apply(errclassif, 2, quantile)

