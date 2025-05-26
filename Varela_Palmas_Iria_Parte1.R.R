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
cor(KorTemp$FTMax, KorTemp$LWS, use = "complete.obs")
# la correlacion es -0.35 => relacion negativa moderada
# a mas LWS, menor tiende a ser FTMax

#---------------------------------------------------
#Deﬁne  y  calcula  el  coeﬁciente  de  correlación
# parcial  entre  X1   y  X2 ,  controlando  por
# el resto  de  las  variables  explicativas.
#---------------------------------------------------
library(ppcor) #instalamos ppcor para facilidad
pcor.test(KorTemp$LWS, KorTemp$LRadSol,
          KorTemp[, c("LLH", "LTMax", "LRHmax", "LTmin", "FTMax")])
# el resultado es  ~0.14.
# positivo pequeño pero con una significancia alta (p valor muy pequeño)
# hay una pequeña relacion entre ellas

#----------------------------------------------------------
#Considera un modelo reducido que contiene solo un subconjunto
# de las variables ex- plicativas incluidas en el modelo completo.
# Formula y contrasta, mediante un test t, la hipótesis nula de
# que los coeﬁcientes asociados a las variables excluidas son iguales a cero.
#-----------------------------------------------------------

#modelo reducido (quitamos LLH y LRHmax)
modelo_reducido <- lm(FTMax ~ LWS + LRadSol + LTMax + LTmin, data = KorTemp)

#contraste t para los eliminados
summary(modelo)$coefficients[c("LLH", "LRHmax"), ]
# los estimados son muy significativos (pvalor pequeño).
# aumentar LLH aumenta LRHmax de forma pequeña.
# LRHmax disminuye LLH de forma pequeña.


#-----------------------------------------------------------------
#Compara el modelo completo con el modelo reducido mediante un test F .
# Expón cla- ramente la hipótesis nula, calcula el estadístico de contraste,
# determina el valor crítico correspondiente y extrae las conclusiones en
# función del resultado del test.
#-------------------------------------------------------------------
summary(modelo)
#-------------------------------
# calculo manual del test F
#-------------------------------
# RSS de cada modelo
rss0 <- deviance(modelo_reducido)
rss <- deviance(modelo)

# Diferencia de RSS
rss0 - rss

# Cálculo manual del estadístico F
# recordamos que n y p son:
#n <- nrow(KorTemp)
#p <- length(coef(KorTemp)) - 1

df_diff <- df.residual(modelo_reducido) - df.residual(modelo)
f <- ((rss0 - rss) / df_diff) / (rss / df.residual(modelo))
pvalue <- 1 - pf(f, df_diff, df.residual(modelo))


# Mostrar resultados
f
pvalue # no se muestra pq es tan pequeño (p < 2.2e-16) que no se llega a calcular manualmente, pero podemos verlo con anova
# calculo automatico
anova(modelo,modelo_reducido)

# El estadístico F = 275.5 con p < 2.2e-16 dice que la mejora cuando se incluyen estas variables es significativa
# rechazamos la hipótesis nula: los coeficientes de LLH y LRHmax no son cero.
# el modelo completo explica mejor que el reducido

#---------------------------------------------
#Compara  el  coeﬁciente  de  determinación  ajustado  para  ambos  modelos.
#---------------------------------------------
# automatico
summary(modelo)$adj.r.squared
summary(modelo_reducido)$adj.r.squared

# a mano
# valores observados y ajustados del modelo completo
y <- KorTemp$FTMax
y_hat <- predict(modelo)

# media de la variable respuesta
y_bar <- mean(y, na.rm = TRUE)

# suma total de cuadrados
VT <- sum((y - y_bar)^2, na.rm = TRUE)

# suma de cuadrados explicada
VE <- sum((y_hat - y_bar)^2, na.rm = TRUE)

# suma de cuadrados de los residuos
VNE <- sum((y - y_hat)^2, na.rm = TRUE)

# Verificación: VT ~ VE + VNE
all.equal(VT, VE + VNE) # verificamos q es TRUE

# Cálculo de R^2
R2 <- VE / VT
R2 # da el mismo resultado que haciendolo automatico


# El R^2 ajustado del modelo completo es 0.502, el del modelo reducido es 0.465.
# el modelo completo explica un 50.2 % de la variabilidad de FTMax (ajustado por el número de predictores)
# y el reducido un 46.5 %
# Concluimos que el modelo completo tiene mejor capacidad explicativa

#===================================================================
#Calcula las componentes principales de las variables del último
# período disponible. Dibuja las puntuaciones de las dos primeras componentes
# respecto al mes (mes). Interpreta las componentes que expliquen
# conjuntamente más del 75 % de la variabilidad.
#==================================================================