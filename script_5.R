#-----------------------------------------------------------
# REGRESIÓN LINEAL GENERAL CON R (USANDO savings)
#-----------------------------------------------------------
#-----------------------------------------------------------
# Sección 1: Ajuste de un modelo de regresión lineal
# Objetivo: Ajustar un modelo de regresión lineal múltiple con R
# sobre la base de datos `savings` del paquete `faraway`.
# El modelo que se propone es:
# Y = β0 + β1*pop15 + β2*pop75 + β3*dpi + β4*ddpi + ε
# donde Y = tasa de ahorro (sr), y el resto son variables explicativas
# socioeconómicas.
#-----------------------------------------------------------
# Cargamos la librería faraway que contiene los datos
library(faraway)
data("savings")
library(ggplot2)
library(GGally)

ggpairs(savings) + theme_bw()

#Ajuste modelo regrsion lineal multiple
z <- lm(sr ~ pop15 + pop75 + dpi + ddpi, data = savings)
summary(z)

# El resumen proporciona:
# - Coeficientes estimados (β̂)
# - Error estándar
# - Valor t y p-valor para cada coeficiente
# - R² y RSE (residual standard error)

# Interpretación:
# Por ejemplo, un coeficiente negativo en pop15 indica que a mayor % de
# población joven, menor tasa de ahorro, con significación estadística.

#-----------------------------------------------------------
# Sección 1.1: Estimación de los parámetros del modelo
#-----------------------------------------------------------
beta <- coef(z)

#matriz del diseño
X <- model.matrix(z)

# numero de observaciones y parametros
n<-nrow(X)
p <- ncol(X)

#-----------------------------------------------------------
# Sección 1.2: Interpretación geométrica
# En regresión, la predicción se expresa matricialmente como:
# Ŷ = H * Y, donde H = X(X'X)^(-1)X' es la matriz hat.
# Esta proyecta Y en el subespacio generado por las columnas de X.

#-----------------------------------------------------------

XtXi <- solve(t(X) %*% X)
H <- X %*% XtXi %*% t(X)

# Cálculo manual del vector β̂
y <- savings$sr
hbeta <- XtXi %*% t(X) %*% y
hbeta  # debe coincidir con coef(modelo)

# Matriz generadora de residuos: M = I - H
M <- diag(1, n) - H

# Residuos estimados: ε̂ = M * Y
residuos <- M %*% y

# También se pueden obtener con:
residuals(z)

#-----------------------------------------------------------
# Sección 2: Estimación de la varianza
# Para estimar la varianza del error σ² se utiliza:
# σ̂² = RSS / (n - p), donde RSS = ∑(ε̂i)² = ε̂'ε̂ = Y'MY
#-----------------------------------------------------------
# Cálculo de RSS (Residual Sum of Squares)
RSS <- t(y - X %*% hbeta) %*% (y - X %*% hbeta)

# Equivalente con función deviance:
deviance(z)

# Varianza estimada del error
sigma2 <- RSS / (n - p)
sigma2

#-----------------------------------------------------------
# SECCIÓN 3: Regresión particionada y parcial (Greene.txt)
# Datos de ejemplo manuales como en el PDF
#-----------------------------------------------------------
# Leer el archivo Greene.txt con separador de tabulación y comas como decimales
datos <- read.table("Greene.txt", header = TRUE, sep = "\t", dec = ",")

# Verificamos
str(datos)
datos_real <- datos
names(datos_real) <- c("Anio", "PNB", "IN", "IPC", "TI")

# Convertimos años en índice de 1 a 15
datos_real$Anio <- 1:15

# Convertimos las variables PNB e IN a dólares constantes del año 1972,
# dividiendo por el IPC y multiplicando por 100.
# Esto elimina el efecto del aumento de precios en los valores monetarios.
# Ajustamos PNB e IN a valores constantes usando IPC
datos_real$PNB <- (datos$PNB / datos$IPC) * 100
datos_real$IN  <- (datos$IN / datos$IPC) * 100

# Usamos la serie IPC para calcular la tasa de inflación año a año.
# Para 1968 usamos un IPC externo (79.06 en 1967), luego usamos cocientes sucesivos.
# La tasa de inflación se define como (IPC_t / IPC_{t-1}) - 1.
datos_real$IPC[1] <- (datos$IPC[1] / 79.06 - 1) * 100  # Valor base para 1968
datos_real$IPC[2:15] <- (datos$IPC[2:15] / datos$IPC[1:14] - 1) * 100

# Se ajusta un modelo de regresión simple para estudiar la relación
# entre la inversión (IN) y el año (Anio).
# Este modelo no tiene en cuenta otras variables posibles (como PNB, IPC o TI),
# por lo tanto, puede estar sesgado por omisión de variables relevantes.
m1 <- lm(IN ~ Anio, datos_real)
summary(m1)

# Ahora ajustamos un modelo múltiple que incluye como variables explicativas:
# - Anio (año)
# - PNB ajustado por inflación
# - IPC (tasa de inflación)
# - TI (tipo de interés)
#
# Este modelo controla por otras influencias sobre la inversión, lo que permite
# interpretar el efecto "neto" o "parcial" de cada variable.
# Aquí, el coeficiente del año podría cambiar de signo si hay confusión con PNB.
m2 <- lm(IN ~ Anio + PNB + IPC + TI, datos_real)
summary(m2)

# Separamos el efecto del año sobre la inversión, controlando por las demás variables.
# Esto se hace en tres pasos:
#
# Paso 1: Ajustar la inversión (IN) con PNB, IPC y TI --> obtener residuos r_IN
# Paso 2: Ajustar el año (Anio) con PNB, IPC y TI --> obtener residuos r_Anio
# Paso 3: Regresar r_IN sobre r_Anio. La pendiente es el efecto parcial del año.
#
# Esta técnica demuestra que en regresión múltiple, el coeficiente de una variable
# se puede interpretar como su efecto "añadido" después de haber descontado los efectos de otras.
#-----------------------------------------------------------

r_IN <- residuals(lm(IN ~ PNB + IPC + TI, datos_real))
r_Anio <- residuals(lm(Anio ~ PNB + IPC + TI, datos_real))

modelo_parcial <- lm(r_IN ~ r_Anio)
summary(modelo_parcial)

# La pendiente de esta regresión coincide con el coeficiente de Anio en m2

m0 <- lm(IN ~ Anio - 1, datos_real)
summary(m0)

#-----------------------------------------------------------
# SECCIÓN 4 - Coeficientes de correlación simple, múltiple y parcial
#-----------------------------------------------------------
# Calculamos la matriz de correlación simple entre todas las variables del dataset.
# La correlación simple entre dos variables mide la intensidad y dirección de la relación lineal.
# El valor está en [-1, 1], y su signo coincide con el del coeficiente de regresión simple.
#-----------------------------------------------------------

correlaciones_simples <- cor(datos_real)
print(correlaciones_simples)

# Podemos extraer la fila o columna correspondiente a IN (Inversión)
correlaciones_IN <- correlaciones_simples["IN", ]
print(correlaciones_IN)

#-----------------------------------------------------------
#Correlación múltiple
#-----------------------------------------------------------
# El coeficiente de correlación múltiple entre una variable respuesta (IN)
# y un conjunto de variables explicativas (Anio, PNB, IPC, TI) se define como:
#
# Corr(IN, Ŷ), donde Ŷ = predicciones del modelo de regresión múltiple.
# Su cuadrado coincide con el R² del modelo de regresión múltiple.
#Comentario: Esta medida nos dice qué tan bien se ajustan las predicciones
# del modelo a la inversión observada. Cuanto más cercana a 1, mejor
# explicación conjunta.
#-----------------------------------------------------------

correlacion_multiple <- cor(datos_real$IN, fitted(m2))
print(correlacion_multiple)

# Verifica que el cuadrado de este valor es igual a Multiple R-squared:
summary(m2)$r.squared  # Debe coincidir con correlacion_multiple^2

#-----------------------------------------------------------
# Correlación parcial
#-----------------------------------------------------------
# Calculamos la correlación entre los residuos de IN y cada variable,
# después de eliminar el efecto de las demás. Esto nos da la influencia neta.
#-----------------------------------------------------------

# Residuos de IN y cada variable, controlando por las otras
r_IN_Anio <- residuals(lm(IN ~ PNB + IPC + TI, data = datos_real))
r_Anio    <- residuals(lm(Anio ~ PNB + IPC + TI, data = datos_real))

r_IN_PNB  <- residuals(lm(IN ~ Anio + IPC + TI, data = datos_real))
r_PNB     <- residuals(lm(PNB ~ Anio + IPC + TI, data = datos_real))

r_IN_IPC  <- residuals(lm(IN ~ Anio + PNB + TI, data = datos_real))
r_IPC     <- residuals(lm(IPC ~ Anio + PNB + TI, data = datos_real))

r_IN_TI   <- residuals(lm(IN ~ Anio + PNB + IPC, data = datos_real))
r_TI      <- residuals(lm(TI ~ Anio + PNB + IPC, data = datos_real))

# Correlaciones parciales entre residuos
correlaciones_parciales <- c(
  Anio = cor(r_IN_Anio, r_Anio),
  PNB  = cor(r_IN_PNB,  r_PNB),
  IPC  = cor(r_IN_IPC,  r_IPC),
  TI   = cor(r_IN_TI,   r_TI)
)
# Resultado esperado: El valor de la correlación parcial entre IN y
# Anio debe ser negativo (~ -0.94), a pesar de que su correlación
# simple era positiva. Esto evidencia un cambio de signo típico del
# fenómeno de confusión

#-----------------------------------------------------------
# Tabla resumen: Correlaciones simples vs. parciales
#-----------------------------------------------------------
# Esta tabla permite comparar la relación directa y la neta
# de cada variable con la inversión (IN). El cambio de signo
# indica un posible efecto de confusión entre variables.
#-----------------------------------------------------------

tabla <- data.frame(
  Correlacion_Simple  = correlaciones_IN[c("Anio", "PNB", "IPC", "TI")],
  Correlacion_Parcial = correlaciones_parciales
)

print(round(tabla, 4))

#-----------------------------------------------------------
# SECCION 5 - Inferencia  sobre  los  parámetros  con  R
#-----------------------------------------------------------
#Contexto teórico
#-----------------------------------------------------------
# En regresión lineal múltiple, se desea construir intervalos de confianza
# para cada parámetro β̂ᵢ del modelo:
#
#   ( β̂ᵢ ± t_{n-p, α/2} · σ̂ · sqrt[(X'X)^(-1)ᵢᵢ] )
#
# Donde:
# - β̂ᵢ: estimación del parámetro
# - t_{n-p, α/2}: cuantil de la t de Student con (n - p) grados de libertad
# - σ̂²: varianza residual estimada
# - (X'X)^(-1): inversa de la matriz de diseño
#-----------------------------------------------------------
#-----------------------------------------------------------
# Estimación de intervalos de confianza al 95% para los coeficientes
# según el modelo ajustado sobre los datos "savings"
#-----------------------------------------------------------

# Recordamos que z es el modelo ajustado:
# z <- lm(sr ~ pop15 + pop75 + dpi + ddpi, data = savings)

# Extraemos la matriz del diseño del modelo
X <- model.matrix(z)
n <- nrow(X)
p <- ncol(X)

# Obtenemos la inversa de X'X
XtXi <- solve(t(X) %*% X)

# Estimación puntual de los coeficientes
beta <- coef(z)

# Varianza residual (sigma^2)
y <- savings$sr
RSS <- t(y - X %*% beta) %*% (y - X %*% beta)
sigma2 <- RSS / (n - p)

#-----------------------------------------------------------
# Cálculo de errores típicos de los coeficientes
#-----------------------------------------------------------
ET <- sqrt(rep(sigma2, length(diag(XtXi))) * diag(XtXi))

#-----------------------------------------------------------
# Cuantil t para nivel de confianza 95%
#-----------------------------------------------------------
niv <- 0.95
t <- qt(1 - (1 - niv)/2, n - p)

#-----------------------------------------------------------
# Cálculo de los intervalos de confianza
#-----------------------------------------------------------

#--- Extremos inferiores de los intervalos de confianza
betainf <- beta - t * ET

#--- Extremos superiores de los intervalos de confianza
betasup <- beta + t * ET

# Mostramos los resultados
betainf
betasup
#-----------------------------------------------------------
# Verificación con confint()
#-----------------------------------------------------------
confint(z, level = niv)

#-----------------------------------------------------------------
# SECCION 6 - Descomposición  de  la  variabilidad.  El  test  F
#-----------------------------------------------------------------
# Objetivo: comparar dos modelos de regresión anidados
# Modelo completo: sr ~ pop15 + pop75 + dpi + ddpi
# Modelo restringido: sr ~ pop15 + pop75 + dpi  (sin ddpi)
#-----------------------------------------------------------
# Test F para comparar modelos de regresión anidados
#-----------------------------------------------------------
# Compara un modelo completo y otro restringido (con menos variables).
#
# H0: los coeficientes eliminados son cero (modelo restringido es válido)
# H1: al menos uno de ellos es distinto de cero (modelo completo es mejor)
#
# Estadístico:
#   F = [(RSS_restr - RSS_comp) / q] / [RSS_comp / (n - p)]
#   Donde:
#     - RSS: suma de residuos al cuadrado
#     - q: nº de restricciones (coeficientes eliminados)
#     - p: nº de parámetros del modelo completo
#
# Si el p-valor asociado a F es pequeño (ej. < 0.05),
# se rechaza H0 y se concluye que las variables eliminadas son significativas.
#
# En R: anova(modelo_restringido, modelo_completo)
#-----------------------------------------------------------


# Modelo restringido: sin la variable ddpi
z1 <- lm(sr ~ pop15 + pop75 + dpi, data = savings)

# Test F usando la función anova()
anova(z1, z)

#-------------------------------
# calculo manual del test F
#-------------------------------
# RSS de cada modelo
rss0 <- deviance(z1)
rss <- deviance(z)

# Diferencia de RSS
rss0 - rss

# Cálculo manual del estadístico F
f <- ((rss0 - rss) / 1) / (rss / (n - p))
pvalue <- 1 - pf(f, 1, n - p)

# Mostrar resultados
f
pvalue

#-----------------------------------------------------
#Segundo contraste (dos variables a la vez)
#Eliminamos simultáneamente las variables pop75 y dpi
#-----------------------------------------------------
# Modelo completo
z <- lm(sr ~ pop15 + pop75 + dpi + ddpi, data = savings)

# Modelo restringido (eliminamos pop75 y dpi)
z2 <- lm(sr ~ pop15 + ddpi, data = savings)

# Cálculo manual del test F
rss0 <- deviance(z2)  # RSS del modelo restringido
rss  <- deviance(z)   # RSS del modelo completo

q <- 2           # nº de restricciones
n <- nrow(model.matrix(z))
p <- ncol(model.matrix(z))

f <- ((rss0 - rss) / q) / (rss / (n - p))
pvalue <- 1 - pf(f, q, n - p)

f
pvalue
anova(z2, z)

#-----------------------------------------------------------
# SECCIÓN 7: Predicción con el modelo ajustado
#-----------------------------------------------------------
# Obtenemos las predicciones puntuales para los datos de la muestra
#Aplicando  la  función  predict  obtenemos  las  predicciones  de
# la  tasa  de  ahorro  considerando  los valores  observados  en  la  muestra.
#-----------------------------------------------------------
predicciones_muestra <- predict(z)
print(predicciones_muestra)

# Salida:
# Predicciones para los primeros países:
# Australia: 10.57, Austria: 11.45, ..., Malaysia: 7.68
# Esto representa la tasa de ahorro estimada por el modelo para cada observación.
#-----------------------------------------------------------

-----------------------------------------------------------
# Predicción para un nuevo país hipotético:
# - pop15 = 30%, pop75 = 2%, dpi = 1000, ddpi = 5
#-----------------------------------------------------------
predict(z, data.frame(pop15 = 30, pop75 = 2, dpi = 1000, ddpi = 5))
# EJECUTAR ESTA LINEA EN LA LINEA DE COMANDOS NO PREGUNTES POR QUE

#-----------------------------------------------------------
# Intervalos de confianza y predicción
#-----------------------------------------------------------
# Intervalo de confianza para la media condicional (nivel 95%)
predict(z,data.frame(pop15=c(30,40),pop75=c(2,1.5),dpi=c(1000,500), ddpi=c(5,4)),interval="confidence")

# Intervalo de predicción para una observación individual (nivel 95%)
predict(z,data.frame(pop15=c(30,40),pop75=c(2,1.5),dpi=c(1000,500), ddpi=c(5,4)),interval="prediction")

# - El intervalo de confianza (5.06, 21.06) indica que, con un 95% de confianza,
#   la tasa de ahorro media para países con estas características está en este rango.
# - El intervalo de predicción (1.28, 16.82) es más amplio, ya que incluye la variabilidad
#   del error aleatorio ε.

#-----------------------------------------------------------
# SECCIÓN 8: Ejercicios propuestos
#-----------------------------------------------------------
# EJERICIO 1
#-----------------------------------------------------------
# Apartado  a
#-----------------------------------------------------------
# Cargar librería y datos
library(faraway)
data(gala)

# Ajustar modelo (todas las variables excepto "Species" como predictoras)
modelo_gala <- lm(Species ~ ., data = gala)
summary(modelo_gala)

# Intervalos de confianza al 95% para los coeficientes
confint(modelo_gala, level = 0.95)

#Se ajusta un modelo múltiple usando todas las variables excepto Species como predictoras.
# El resumen muestra los coeficientes estimados y su significación.
# Los intervalos de confianza indican el rango plausible para cada parámetro al 95%

#-----------------------------------------------------------
# Apartado  b
# matriz de diseño y matriz hat
#-----------------------------------------------------------
X <- model.matrix(modelo_gala) # matriz de diseño
XtXi <- solve(t(X) %*% X)
H <- X %*% XtXi %*% t(X) #matriz hat

#-----------------------------------------------------------
# Apartado  c
# varianza residual e intervalo de cofianza para σ^2 al 95 %.
#-----------------------------------------------------------
#Extraer dimensiones del problema (n = muestras, p = parámetros)
n <- nrow(X)
p <- ncol(X)
#Obtener el vector de respuesta y las predicciones del modelo
y <- gala$Species
beta <- coef(modelo_gala)
#Calcular los residuos (observado - predicho)
residuos <- y - X %*% beta
# Calcular la suma residual de cuadrados (RSS)
RSS <- sum(residuos^2)
#Calcular la varianza residual estimada (σ^2)
sigma2 <- RSS / (n - p)

# Calcular un intervalo de confianza al 95% para la varianza σ^2
# Utilizamos los cuantiles de la distribución Chi-cuadrado
nivel <- 0.95
alfa <- 1 - nivel
IC_inf <- (n - p) * sigma2 / qchisq(1 - alfa/2, df = n - p)
IC_sup <- (n - p) * sigma2 / qchisq(alfa/2, df = n - p)

# Mostrar resultados de la varianza estimada y su intervalo
sigma2
c(inf = IC_inf, sup = IC_sup)

#-----------------------------------------------------------
# Apartado  d
# coeficientes de correlación simple y parcial del número de
# especies sobre las otras variables
#-----------------------------------------------------------
#Identificamos las variables predictoras
variables <- names(gala)[names(gala) != "Species"]

#Calculamos la correlación simple entre Species y cada predictora
cor_simple <- sapply(variables, function(v) cor(gala$Species, gala[[v]]))

# Calcular correlaciones parciales manualmente usando residuos
# Para cada variable Xj, calculamos:
# - Residuos de Species ~ otras variables
# - Residuos de Xj ~ otras variables
# - Correlación entre ambos residuos = correlación parcial

cor_parcial_manual <- numeric(length(variables))
names(cor_parcial_manual) <- variables

for (var in variables) {
  otras_vars <- setdiff(variables, var)

  # Residuos de Species respecto a las otras variables
  r_species <- residuals(lm(Species ~ ., data = gala[, c("Species", otras_vars)]))

  # Residuos de la variable actual respecto a las otras
  r_x <- residuals(lm(gala[[var]] ~ ., data = gala[, otras_vars]))

  # Correlación parcial = correlación entre residuos
  cor_parcial_manual[var] <- cor(r_species, r_x)
}
#Mostrar tabla comparativa entre correlaciones simples y parciales
tabla <- data.frame(
  Variable = variables,
  Correlacion_Simple  = round(cor_simple[variables], 4),
  Correlacion_Parcial = round(cor_parcial_manual, 4)
)

print(tabla)

#-----------------------------------------------------------
# Apartado  e
# Aplica  el  F-test  y  comenta  los  resultados
#-----------------------------------------------------------
# Modelo completo (todas las variables)
modelo_full <- lm(Species ~ ., data = gala)

# Modelo restringido (sin Endemics y Area)
#viendo el summary son las de mayor significancia (menor p valor)
modelo_restringido <- lm(Species ~ Elevation + Nearest + Scruz + Adjacent, data = gala)

# Cálculo manual del test F
rss0 <- deviance(modelo_restringido)  # RSS del modelo restringido
rss  <- deviance(modelo_full)   # RSS del modelo completo

q <- 2           # nº de restricciones
n <- nrow(model.matrix(modelo_full))
p <- ncol(model.matrix(modelo_full))

f <- ((rss0 - rss) / q) / (rss / (n - p))
pvalue <- 1 - pf(f, q, n - p)

f
pvalue

#hacemos anova
anova(modelo_restringido, modelo_full)

#vamos a probar ahora a quitar la variable menos significativa (Adjacent)
modelo_restringido_2 <- lm(Species ~ Endemics + Area + Elevation + Nearest + Scruz, data = gala)

rss0 <- deviance(modelo_restringido_2)  # RSS del modelo restringido

f_2 <- ((rss0 - rss) / q) / (rss / (n - p))
pvalue_2 <- 1 - pf(f_2, q, n - p)

f_2
pvalue_2

#hacemos anova
anova(modelo_restringido_2, modelo_full)

# Calculamos el estadístico F de forma manual para comparar ambos modelos:
# Un valor de F = 44.2 con un p-valor ≈ 1.3e-08 indica que eliminar 'Endemics' y 'Area'
# reduce significativamente la calidad del modelo. Estas variables son relevantes.

# La función anova(modelo_restringido, modelo_full) confirma este resultado, mostrando que la diferencia
# entre modelos es altamente significativa (p < 0.001). Por tanto, no conviene eliminar esas variables.

# En contraste, construimos un segundo modelo restringido eliminando 'Adjacent', que es la variable

# En este caso, el estadístico F = 0.0116 y el p-valor ≈ 0.988, lo que indica que eliminar 'Adjacent'
# no afecta significativamente al modelo. Su contribución es mínima.

# La anova también lo confirma: la diferencia entre modelos al eliminar 'Adjacent' no es significativa.

# En resumen:
# - Eliminar variables relevantes (como 'Endemics') degrada mucho el modelo (alta F, bajo p).
# - Eliminar variables irrelevantes (como 'Adjacent') no afecta el modelo (baja F, alto p).
# Esto valida el enfoque basado en la significancia individual y el test F global para evaluar qué
# variables conviene eliminar o mantener.

#-----------------------------------------------------------
# EJERICIO 2
#-----------------------------------------------------------
# Apartado  a
#Ajusta un modelo de regresión múltiple que explique la cantidad de grasa
# corporal en función de las otras variables. Prueba considerando las tres
# variables como explicativas o subconjuntos de dos variables.
#-----------------------------------------------------------
# Cargar datos
fat <- read.table("Fat.txt", header = TRUE, sep = "", dec=".")
names(fat)

#modelo con las 3 variables
modelo_full <- lm(Fat ~ Triceps + Thigh + Midarm, data = fat)
summary(modelo_full)

# modelos con parejas de variables
modelo_parcial_1 <- lm(Fat ~ Triceps + Thigh, data = fat)
modelo_parcial_2 <- lm(Fat ~ Triceps + Midarm, data = fat)
modelo_parcial_3 <- lm(Fat ~ Thigh + Midarm, data = fat)

#comparaciones
anova(modelo_full, modelo_parcial_1)
anova(modelo_full, modelo_parcial_2)
anova(modelo_full, modelo_parcial_3)

#vemos que todos los modelos parciales tienen un pvalor por encima
#de 0.1 lo que significa que por si mismas no son tan significativas
# aunque si lo son cuando estan juntas

#-----------------------------------------------------------
# Apartado b
#Obtén  intervalos  de  conﬁanza  para  los  parámetros  del
# modelo  ajustado.
#-----------------------------------------------------------
#un parámetro es estadísticamente significativo al 95% si su intervalo de confianza no incluye el cero.

#intervalos de 95% para el modelo entero
confint(modelo_full)
#Como todos los intervalos contienen el 0, ninguna de las
# variables es significativa individualmente en el modelo completo.
# Esto refuerza lo que veíamos antes con los valores-p altos: hay colinealidad.

# ahora para cada modelo ajustado
confint(modelo_parcial_1)
# aqui hay dos intervalos que no contienen 0
#Thigh se vuelve significativa cuando no está Midarm,
# lo que indica que Midarm y Thigh están compartiendo información.

confint(modelo_parcial_2)
# Triceps y Midarm son significativos aquí, cuando Thigh no está.
# cada variable gana fuerza cuando se eliminan otras que están correlacionadas con ella.

confint(modelo_parcial_3)
#Thigh es significativo cuando Triceps no está.
#Midarm no lo es en este modelo.

#🔎 Conclusión general
#En el modelo completo, nadie es significativo individualmente porque hay colinealidad:
# las variables están correlacionadas y "se pisan".
#Cuando sacas una de las variables, otras se vuelven significativas.
#Por ejemplo, Triceps y Midarm son significativos cuando se elimina Thigh.
#Esto confirma que las variables se "anulan" mutuamente en el modelo completo.

#-----------------------------------------------------------
# Apartado c
#Calcula  los  coeﬁcientes  de  correlación  simple  y
# parcial  de  la  grasa  corporal  con  las  otras variables.
#-----------------------------------------------------------

#Correlaciones simples
#Son las correlaciones de Pearson entre Fat y cada predictor individual, sin ajustar por las otras variables.
cor(fat) # para ver la tabla completa
cor(fat$Fat, fat[, c("Triceps", "Thigh", "Midarm")]) #para ver solo fat contra las variables

# Resultados:
# Triceps:  0.843 → correlación fuerte positiva
# Thigh:    0.878 → correlación muy fuerte positiva
# Midarm:   0.142 → correlación débil positiva
#
# Interpretación:
# Triceps y Thigh están altamente correlacionadas con la grasa corporal cuando se observan por separado.
# Midarm, en cambio, no muestra una relación fuerte por sí sola.



#Correlaciones parciales
#Miden la correlación entre Fat y otra variable, controlando por las demás. Para esto se necesita el paquete ppcor.
library(ppcor)

#calculo de correlaciones parciales
pcor_result <- pcor(fat)
pcor_result$estimate  # muestra las correlaciones parciales
pcor_result$estimate["Fat", ] # muestra solo las de fat

# Resultados:
# Triceps:  0.338 → correlación parcial moderada positiva
# Thigh:   -0.267 → correlación parcial débil negativa
# Midarm:  -0.324 → correlación parcial moderada negativa
#
# Interpretación:
# Triceps mantiene una relación positiva con la grasa incluso controlando por las otras variables,
# lo que indica que su efecto es más independiente.
# Thigh, a pesar de su alta correlación simple, muestra una relación negativa débil cuando se
# controlan las otras variables. Esto sugiere que su efecto estaba parcialmente mediado por Triceps.
# Midarm tiene una correlación simple baja, pero su relación parcial con la grasa es negativa,
# indicando que podría introducir ruido en el modelo si se incluye.

# Conclusión:
# - Triceps parece ser el predictor más confiable.
# - Thigh y Midarm pueden estar aportando colinealidad o redundancia.
# - Esta interpretación explica por qué en el modelo completo ninguna variable fue significativa por sí sola.

