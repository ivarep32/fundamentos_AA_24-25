#----------BOLETIN 1 ------------
library(car) # cargamos el paquete car

#Recupera el nombre de las variables en cars y haz un listado de los 5 primeros valores
names(cars)
head(cars,5)

#Hacer un grafico de dispersion, que tipo de relacion existe entre la velocidad y la distancia?
plot(cars, main="Relacion velocidad-distancia", col="blue")

#vemos que a mayor velocidad mayor distancia de frenado

# ejecutamos la linea:
x <- matrix(1:20, nrow=4,ncol=5, byrow=T)
class(x) # podemos ver el tipo de dato

#¿estrcutura?: x es una matriz 4x5
#¿que hace byrow? rellena por filas
# comparar con:
x1 <- matrix(1:20, nrow=4,ncol=5, byrow=F)

# se puede ver que x ordena los valores por filas y x1 por columnas

# que pasa si hacemos:
x+2
# le suma la constante a todos los elementos de la matriz

#y si hacemos...
x + c(5,6,7)

# el vector no tiene la longitud suficiente para ser sumado

x + c(5,6,7,8) # suma 5,6,7,8 a cada fila de la matriz

#comparamos
x_2=x-x[,2] # resta a cada elemento de x los valores de la segunda columna de x, la operacion se hace fila a fila

x_3=x-x[2,] # resta a cada elemento de x los valores de la segunda fila de x, se hace columna a columna

# creamos la matriz

y <- matrix(rpois(6,20),nrow=2,ncol=3) #poisson parametro 20

# que tipo de ordenacion consiguen:

sort(y) # de menor a mayor
apply(y,2,sort) # ordena cada columna de la matriz
apply(y,1,sort) # ordena cada fila de la matriz 

# que pasa si hacemos...
diag(y) # se obtiene el elemento (1,1) y el (2,2)

# comparamos
a = t(y)*diag(y) # multiplica la traspuuesta de y por un vector fila
b = t(y)%*%diag(y) # multiplica la traspuesta por una matriz diagonal 2x2

#crear un vectores
x1 <- rep(1:3, each=2) #repite los numeros del 1 al 3 2 veces cada uno
x1b <- rep(1:3, times=2) # da 1 2 3 1 2 3
x2 <- seq(12,21,length.out = 10) # 10 numeros equiespaciados entre 12 y 21
x3 <- seq(6,46, by =2) # numeros separados por 2 
x4 <- c(x1,x2,x3) # empata todos los vectores

# creamos el vector d
d <- c(1,-1,1,0,1,-6,0,0,1)
#construimos la matriz D (cuadrada, o sea 3x3 en este caso) con la funcion matrix
# se va a rellenar por filas
D <- matrix(d,nrow=3,byrow=T)

#vector de la primera columna
D[,1]
#vector de la segunda fila
D[2,]

apply(D,1,sum)#suma por filas
apply(D,2,sum) #suma por columnas
colSums(D)
rowSums(D)

t(D) # traspuesta de D

D2 <- solve(D) # inversa de D

I <- D%*%D2 # una matriz por su inversa es la identidad

# carga de datos
data(iris)
datos <- iris # dataframe
head(iris) 
names(iris)
levels(datos$Species) # obtiene los posibles valores de la variable
plot(datos$Petal.Length, datos$Petal.Width) #relacion longitud anchura
plot(datos$Petal.Length, datos$Petal.Width, col=datos$Species) # varia el color diferenciando por especies

# leemos datos de un txt
datos <- read.table("titanic.txt", header=TRUE)
class(datos)
names(datos) #nombres de las columnas
summary(datos) #resumen estadistico
table(datos$edad)[1] # saca la cantidad de adultos
table(datos$edad)[2] # niños

niños_en_primera <- table(datos$edad == 'niño', datos$clase=='primera')[4] #el cuatro nos devuelve el valor T-T de la matriz
niños_tripulacion  <- table(datos$edad == 'niño' & datos$clase=='tripulacion')

supervivientes <- table(datos$superviviente)
prop.table(supervivientes)*100 # nos da la tabla de supervivientes en porcentaje

# tabla de personas que iban en cada clase
clases <-table(datos$clase)

# barplot de las personas que iban en cada clase
barplot(clases)

#pie plot de las personas en cada clase
pie(clases)

# se puede hacer esto para cualquier variable
lapply(datos, table)

# tabla de supervivientes en funcion de la clase
super_clase <- table(datos$superviviente, datos$clase)

super_clase_porcentajes <- round(prop.table(super_clase,2)*100,2)

barplot(super_clase_porcentajes) # muestra en oscuro el porcentaje cubierto y en claro lo que queda por cubrir
barplot(prop.table(super_clase, 2)[2, ] * 100, main = "Supervivencia por clase") # muestra solo lo cubierto

# --------------------- CARGAR EL  NUEVO DATASET ------------------
library(MASS)
data("mcycle")
str(mcycle)
plot(mcycle$times, mcycle$accel, type='p', main="Aceleracion vs tiempo") #p pone los puntos

# -----------OTRO DATASET -----------------------
data("pressure")
str(pressure)

plot(pressure$temperature, pressure$pressure, type="b")
# es una relacion no lineal ya que la presion aumenta cuando la temperatura aumenta pero lo hace en una curva

plot(log(pressure$temperature), log(pressure$pressure), type="b")
# la relacion ahora se aproxima a ser lineal

# --------------- CARGAR UN DATASET LOCAL --------
datos <- read.table("etanol.txt", header=T)
plot(datos$etanol, datos$nox) # grafico de los datos de etanol

