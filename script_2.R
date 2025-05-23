# BOLETIN 2
# funcion que recibe 2 vectores numericos y determina el numero de prodcutos
# >0 obtenidos al multiplicar cada elemento del primer vector por cada elemento
# del segundo vector. La funcion va a devolver este recuento

productos_positivos <- function(v1,v2){
  productos <- outer(v1,v2,'*') #producto cruzado
  sum(productos>0)
}

# recibe un vector numerico x y un numero natural m y realiza la suma de los elementos
# dex de forma incremental hasta alcanzar o superar m. debe devolver la suma obtenida
# y el vector x final que llega a esa suma

suma_hasta_M <- function(x, M) {
  suma <- 0
  elementos <- c()
  i <- 1  # índice para recorrer x repetidamente

  while (suma < M) {
    val <- x[i]
    suma <- suma + val
    elementos <- c(elementos, val)

    i <- i + 1
    if (i > length(x)) i <- 1  # Reinicia si llegamos al final
  }

  return(list(suma = suma, vector = elementos))
}

v <- c(2,3,4)
suma_hasta_M(v, 20)

#recibe un numero y determina si es o no natural
# si es natural devuelve un vector con todos sus divisores
# si no, debe mostrar un error

natural <- function(n){
  if (n>0 && floor(n)==n){
    return(which(n%%1:n==0)) #divisores
  }
  else{
    stop("El número no es natural")
  }
}

# determinar si un año es bisiesto
bisiesto <- function(year){
  if ((year%%4 ==0 && year%%100!=0) || year%%400==0){
  cat(year, "es bisiesto")}
else{
cat(year, "no es bisiesto")
}
}

#Desarrolla un programa en R que simule el funcionamiento de un sistema de cobro en un estacionamiento.
# El programa solicitará al usuario la hora de entrada (hora, minutos y segun- dos), calculará el tiempo
# transcurrido hasta el momento actual y calculará la cantidad a pagar utilizando una tarifa de 0,000277778 euros
# por segundo, con un límite máximo de 24 euros al día. Una vez calculada la cantidad a pagar, el programa solicitará
# al usuario que ingrese la canti- dad en euros. Luego, calculará el cambio a devolver, minimizando la cantidad
# de billetes de 10e y 5e, así como las monedas necesarias.

aparcamiento <- function() {
  # Solicitar hora de entrada
  cat("Ingrese la hora de entrada (formato 24h):\n")
  hora <- as.numeric(readline("Hora (0-23): "))
  minutos <- as.numeric(readline("Minutos (0-59): "))
  segundos <- as.numeric(readline("Segundos (0-59): "))
  
  # Validar entrada
  if (any(is.na(c(hora, minutos, segundos))) || 
      hora < 0 || hora > 23 || 
      minutos < 0 || minutos > 59 || 
      segundos < 0 || segundos > 59) {
    stop("Entrada de tiempo inválida")
  }
  
  # Calcular tiempo transcurrido
  tiempo_entrada <- as.POSIXct(sprintf("%02d:%02d:%02d", hora, minutos, segundos), 
                              format="%H:%M:%S")
  tiempo_actual <- Sys.time()
  tiempo_transcurrido <- as.numeric(difftime(tiempo_actual, tiempo_entrada, units="secs"))
  
  # Calcular importe
  tarifa <- 0.000277778
  importe <- min(tiempo_transcurrido * tarifa, 24)
  
  # Mostrar información
  cat(sprintf("\nTiempo transcurrido: %.2f horas\n", tiempo_transcurrido/3600))
  cat(sprintf("Importe a pagar: %.2f euros\n", importe))
  
  # Solicitar pago
  pago <- as.numeric(readline("\nIngrese la cantidad en euros: "))
  if (is.na(pago) || pago < importe) {
    stop("Cantidad insuficiente")
  }
  
  # Calcular cambio
  cambio <- pago - importe
  
  # Calcular billetes y monedas
  denominaciones <- c(10, 5, 2, 1, 0.50, 0.20, 0.10, 0.05, 0.02, 0.01)
  nombres <- c("10e", "5e", "2e", "1e", "50c", "20c", "10c", "5c", "2c", "1c")
  cambio_desglosado <- numeric(length(denominaciones))
  
  cambio_restante <- round(cambio, 2)
  for (i in 1:length(denominaciones)) {
    cambio_desglosado[i] <- floor(cambio_restante / denominaciones[i])
    cambio_restante <- round(cambio_restante %% denominaciones[i], 2)
  }
  
  # Mostrar cambio
  cat(sprintf("\nCambio a devolver: %.2f euros\n", cambio))
  cat("Desglose:\n")
  for (i in 1:length(denominaciones)) {
    if (cambio_desglosado[i] > 0) {
      cat(sprintf("%s x %d\n", nombres[i], cambio_desglosado[i]))
    }
  }
}

#Desarrolla una función en R, llamada “cinco”, que solicite al usuario ingresar 5 números por teclado.
# La función deberá veriﬁcar si alguno de los números ingresados es mayor o igual que 100.
# Si alguno de los números es mayor o igual que 100, el programa deberá devolver un mensaje de adver tencia.
# En caso contrario, la función calculará la suma de los números ingresados que sean menores que 10 y la
# devolverá como resultado.

cinco <- function(){
  # vector que almacena los numeros
  numeros <- numeric(5)

  #solicitamos los numeros
  cat("Por favor ingrese los 5 numeros\n")
  for (i in 1:5){
    input <- readline(sprintf("Numero %d ", i))
    numeros[i] <- as.numeric(input)

    #verificar validez del input
    if (is.na(numeros[i])){
      stop("Error: ingrese un numero valido")
    }
  }

  # verificar el si los numeros son >= 100
  if(any(numeros >= 100)){
    return("Advertencia: Al menos uno de los numeros es >=100")
  }

  #calcular la suma de los menores a 10
  suma <- sum(numeros[numeros < 10])

  return(paste("La suma de los menores a 10 es ", suma))
}

#Desarrolla un programa en R que encuentre todos los números enteros positivos menores que 1000
# que cumplan la siguiente condición: la suma de las cifras de su cubo debe ser un cuadrado perfecto.
# Un cuadrado perfecto es un número cuya raíz cuadrada es un número natural. El programa deberá:
#a) Iterar  sobre  los  números  enteros  positivos  menores  que  1000.
#b) Para  cada  número,  calcular  su  cubo  y  luego  la  suma  de  las  cifras  de  su  cubo.
#c) Veriﬁcar  si  la  suma  de  las  cifras  del  cubo  es  un  cuadrado  perfecto.
#d) Si  lo  es,  agregar  el  número  a  una  lista  de  números  que  cumplen  la  condición.
#e) Finalmente,  imprimir  la  lista  de  números  encontrados  que  cumplen  con  la  condición.

numeros_especiales <- function(){

}
