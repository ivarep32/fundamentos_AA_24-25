midni=(5190)
set.seed(midni)
A1= midni%% 2
A2= sample(1:25,3)
load("KorTemp.RData")
lmues=sample(1:nrow(KorTemp),300)
#=================================================================================
#  Se desea predecir la temperatura máxima futura (FTMax), en función de variables 
#  meteorológicas del último periodo. Ajusta por el metodo de mínimos cuadrados el siguiente
#  modelo de regresión múltiple: Y = β0 + β1X1 + β2X2 + β3X3 + β4X4 + β5X5 + β6X6 + ε
#  donde: X1 = LWS; X2 = LRadSol; X3 = LLH; X4 = LTMax; X5 = LRHmax y X6 = LTmin
#==================================================================================
#ajustar el modelo
modelo <- lm(FTMax ~ LWS + LRadSol + LLH + LTMax +LRHmax + LTmin, data=KorTemp)

#------------------------------------------------------------------------------------
#Obtén los coeﬁcientes estimados del modelo, así como intervalos de conﬁanza al 95 %
# para los coeﬁcientes del modelo. Calcula los valores ajustados del modelo y
# calcula la suma residual de cuadrados.
#----------------------------------------------------------------------------------------

# Coeficientes estimados
coef(modelo)

# Valores ajustados por el modelo (ŷ)
fitted(modelo)

# Residuos del modelo (ε̂ = y - ŷ)
residuals(modelo)
RSS <- sum(residuals(modelo)^2)
n <- nrow(KorTemp)
p <- length(coef(KorTemp)) - 1
RSE <- sqrt(RSS / (n - p - 1))
RSE

# Intervalos de confianza al 95%
confint(modelo, level = 0.95)

#---------------------------------------------------
#Calcula  el  valor  del  coeﬁciente  de  correlación
# de  Pearson  entre  la  variable  respuesta  Y y  el
# predictor  X1
#-----------------------------------------------------
