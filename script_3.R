# ANALISIS DE COMPONENTES PRINCIPALES CON PRINCOMP
dat <- read.table("aspirantes.txt",header=TRUE)

#En primer lugar, como salida del objeto se muestran las desviaciones típicas de
# las componentes, que son las raíces cuadradas de los autovalores de S .
test.pca <- princomp(dat)

#Si  hacemos  un  summary  del  objeto,  obtenemos  además  la  proporción  de
# varianza  explicada  y  sus valores  acumulados.
summary(test.pca)

#Además de esta información, el objeto test.pca almacena otra información relevante.
names(test.pca)

#Como  hemos  visto,  podemos  obtener  las  desviaciones  típicas  de  las  componentes.  De  este  modo podemos  obtener  las  varianzas  de  las  componentes,  que  son  los  autovalores.
test.pca$sdev        # Desviaciones estándar
test.pca$sdev^2      # Varianza (autovalores)

#Representa los autovalores en orden decreciente. Se usa para encontrar el "codo" del gráfico y decidir cuántas componentes usar.
screeplot(test.pca)                  # Diagrama de barras
screeplot(test.pca, type = "lines") # Diagrama con líneas

#Además,  podemos  obtener  los  autovectores  asociados  (columnas  de  la  matriz  test.pca$loadings).
test.pca$loadings

#Un gráfico de barras con los coeficientes de cada variable en cada componente. Útil para interpretar
# las componentes principales visualmente.
barplot(loadings(test.pca), beside = TRUE)

test.pca$n.obs     # Número de observaciones (25 alumnos)
test.pca$center    # Media de cada variable
test.pca$scale     # Escala usada (aquí, no hay estandarización: escala = 1)

#Las nuevas coordenadas de los datos originales en el espacio de las componentes principales.
# Es decir, los datos proyectados sobre los nuevos ejes.
#Son  el  resultado  de  X P ,  siendo  P   la  matriz  que  tiene  como columnas  los
# autovectores  de  S
test.pca$scores

# ------------------------------------------------------------------------------------
# Análisis   de   componentes   principales   a   través   de   la   descomposición
# espectral  de  la  matriz  de  covarianzas

#Esta parte te muestra cómo calcular el PCA “desde cero” usando las funciones cov()
# y eigen(), para confirmar y entender mejor los resultados de princomp().
#--------------------------------------------------------------------------------------

# Calcular el número de observaciones y medias de las variables
#Para obtener las medias de cada variable, que luego se usan para
# centrar los datos y calcular la matriz de covarianzas
# .
n <- nrow(dat)  # Número de observaciones
apply(dat, 2, mean)  # Media de cada variable (por columnas)

#Calcular la matriz de covarianzas muestral corregida
#matriz de covarianzas muestral corregida, usada por princomp().
# Se usa para capturar las relaciones lineales entre las variables.
#cov(dat) usa denominador (n−1); para igualar a princomp(), se ajusta con (n−1)/n.

S <- cov(dat) * (n - 1)/n  # Ajuste muestral: multiplica por (n-1)/n

#Obtener autovalores y autovectores
auto <- eigen(S) # Descomposición espectral de S
lambda <- auto$values # Autovalores (varianzas de las componentes)
v <- auto$vectors # Autovectores (direcciones de las componentes)

#Confirmar varianza explicada y su acumulado
lambda / sum(lambda) # Proporción de varianza explicada
cumsum(lambda / sum(lambda)) # Proporción acumulada

#-------------------------------------------------------------------------------
# EL BIPLOT
#Un biplot es una representación en el plano definido por las dos primeras
# componentes principales:
#Cada punto representa una observación (aspirante).
#Cada flecha representa una variable original (asignatura).
#Esto permite: Ver grupos de individuos con perfiles similares.
#Identificar correlaciones entre variables (ángulos pequeños entre flechas).
#Interpretar qué variables explican mejor la variabilidad de los datos
#-------------------------------------------------------------------------------

biplot(test.pca)

# Interpretación del biplot:
# El gráfico muestra los dos primeros componentes principales:
# Comp.1 (eje horizontal) y Comp.2 (eje vertical).

# - Puntos cercanos entre sí: los aspirantes tienen puntuaciones similares en las materias.
# - Puntos alineados con una flecha: ese aspirante tiene puntuaciones altas en esa materia.
# - Flechas con ángulos pequeños: materias positivamente correlacionadas.
# - Flechas en direcciones opuestas (ángulo ~180°): materias negativamente correlacionadas.

# Ejemplo específico:
# - Si prog, ingcom, ingsof, etc., apuntan todas hacia la misma dirección, indica
#   que las asignaturas están positivamente correlacionadas.
# - Si la primera componente tiene todos los coeficientes positivos y explica >80% de la varianza,
#   probablemente representa la “nota global” del alumno.
# - Un punto muy alejado en la dirección de la primera componente puede indicar un alumno
#   sobresaliente o con bajo rendimiento general (según el signo).

#-------------------------------------------------------------
#Las  componentes  principales  y  los  cambios  de  escala
#-------------------------------------------------------------
# ⚠️ Importancia de la escala en PCA:
# El PCA se basa en la varianza de las variables. Si las variables tienen escalas diferentes,
# las de mayor escala dominarán la primera componente principal.

# Para evitar este sesgo, podemos estandarizar las variables, es decir:
# - Restar la media.
# - Dividir por la desviación estándar.
# Esto equivale a usar la matriz de correlaciones en lugar de la de covarianzas.

# EJERCICIOS

# 1. Realiza un Análisis de Componentes Principales con los datos de decatlón.
# Justifica el uso de la matriz de covarianzas o de la matriz de correlaciones
# muestrales para llevar a cabo el análisis.

# Leer los datos del archivo
deca <- read.table("decatlon.txt", header = TRUE)

# Verificar las escalas de las variables
summary(deca)

#Como las variables representan distintas pruebas físicas
# (con unidades diferentes: segundos, metros, puntos), tienen escalas muy distintas.
#Por tanto, debemos usar la matriz de correlaciones → esto se hace con cor = TRUE.
# Análisis PCA con variables estandarizadas
pca.deca <- princomp(deca, cor = TRUE)
summary(pca.deca)

#2.Haz una interpretación de las dos primeras componentes principales.
#¿Cuál es la proporción de variabilidad explicada por las dos primeras componentes
# principales?

summary(pca.deca)

#Interpretación:

#La primera componente (Comp.1) explica el 61.8% de la variabilidad total.
#Esta suele estar asociada a un rendimiento físico general.

#La segunda componente (Comp.2) explica el 23.6% adicional, llegando a un total
# de 85.4% acumulado.

#Para interpretar qué representa cada componente:
# Cargarings (autovectores) de las variables
pca.deca$loadings

# 3.Realiza el biplot correspondiente y comenta la gráfica obtenida.
biplot(pca.deca)

#Los puntos representan los participantes. Si están juntos, tienen perfiles similares.

#Las flechas son las variables. Las variables con flechas en la misma dirección
# están correlacionadas positivamente.

#Si un grupo de variables apunta en la misma dirección que una nube de puntos,
# esos participantes destacan en esas pruebas.

# RESPUESTA ANALISIS: En el biplot observamos que los participantes con altas
# puntuaciones en X100, X400 y X110 están agrupados y alineados con esas variables.
# Esto sugiere que la primera componente principal refleja el rendimiento en velocidad.
# Por otro lado, jave y disq apuntan en dirección perpendicular, indicando un eje
# independiente relacionado con fuerza/lanzamientos.