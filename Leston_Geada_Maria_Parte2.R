#EXAMEN FINAL PARTE 2

#Personalización del examen
midni = 1003
set.seed(midni)
A1= midni %% 2
A2= sample(1:25,3)
load("KorTemp.RData")
lmues=sample(1:nrow(KorTemp),300)


#EJERCICIO 3
#Calcula un módelo de regresión para explicar (A1=0:FTMax, A1=1:FTmin) con las variables del último
#período disponible (empiezan por L), con el objetivo de usar el mejor modelo con el menor número de
#covariables. Explica el proceso seguido, las elecciones tomadas y los resultados obtenidos.

A1 #A1 = 1:FTmin

# seleccionamos las variables

datL <- KorTemp[, grep("^L", names(KorTemp))]
head(datL)
names(datL)

mod <- lm(FTmin ~ LTMax + LTmin + LRHmin + LRHmax + LTMax_D + LTmin_D + LLH + LRadSol, KorTemp)
summary(mod)

#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept) -2.454e+00  2.970e-01  -8.263  < 2e-16 ***
#  LTMax        4.144e-02  6.313e-03   6.564 5.58e-11 ***
#  LTmin        2.597e-01  8.510e-03  30.520  < 2e-16 ***
#  LRHmin       1.694e-02  1.832e-03   9.246  < 2e-16 ***
#  LRHmax      -1.344e-03  2.343e-03  -0.574  0.56613    
#LTMax_D      1.031e-01  1.052e-02   9.801  < 2e-16 ***
#  LTmin_D      6.129e-01  1.327e-02  46.194  < 2e-16 ***
#  LLH          1.379e-03  4.202e-04   3.281  0.00104 ** 
#  LRadSol     -5.400e-05  3.086e-05  -1.750  0.08025 .  

#La variable LRHmax no es significativa, por lo que la eliminaremos 
#LRadSol (0.08025) si es significativa aunque en menor proporción, pues está por encima del
#umbral (< 2e-16), mas eliminarla podría empeorar el modelo.

#Modelo sin LRHmax
mod2 <- lm(FTmin ~ LTMax + LTmin + LRHmin + LTMax_D + LTmin_D + LLH + LRadSol, KorTemp)

#Modelo sin LRHmax ni LRadSol
mod3 <- lm(FTmin ~ LTMax + LTmin + LRHmin + LTMax_D + LTmin_D + LLH, KorTemp)

#Realizamos el F-Test para ver si es preferible el modelo sin las variables

anova(mod2, mod)
#El p-valor es demasiado alto (0.5661), por lo que podemos suprimir la variable LRHmax sin afectar al modelo

anova(mod3, mod)
#El p-valor (0.1839), aunque en menor proporción, sigue siendo mayor al umbral (< 2e-16), por lo que
#también podemos descartar la variable LRadSol


#EJERCICIO 4
#Seleccionando las filas del conjunto de datos dada por lmues: KorTemp[lmues,],
#estimar un modelo de regresión no lineal para explicar (A1=0:FTMax, A1=1:FTmin) usando la
#variable LRadSol. Comenta las opciones elegidas y dibuja el modelo de regresión obtenido
#sobre los datos originales.

A1
#A1 = 1:FTmin

#MODELO GAM
library(mgcv)
library(ggplot2)

# Establecemos los datos y la fórmula de gam
lmues <- KorTemp[lmues,]
fgam <- as.formula(paste("FTmin", "~ s(LRadSol)"))

#Ajustamos el modelo
mgam <- gam(fgam, data = lmues)
summary(mgam)

#Predecimos los valores
lmues$pred <- predict(mgam)
lmues$LRadSol_pred <- lmues$LRadSol

#Dibujamos los resultados
ggplot(lmues, aes(x = LRadSol, y = FTmin)) +
  geom_point(alpha = 0.5, color = "blue") +
  geom_line(aes(y = pred), color = "red") +
  theme_minimal() +
  labs(
    title = paste("Modelo GAM"),
    x = "LRadSol",
    y = "FTmin"
  )

#Gracias al modelo GAM vemos que las variables no presentan una relación lineal.
#La forma de campanade la curva sugiere un efecto de saturación o decrecimiento en FTmin cuando
#LRadSol es demasiado alto o demasiado bajo.
#El modelo responde más agresivamente a los cambios en los extremos de LRadSol.