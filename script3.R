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



