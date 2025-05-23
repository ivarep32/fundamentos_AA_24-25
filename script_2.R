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

numeros_especiales <- function() {
  # Vector para almacenar los números que cumplen la condición
  numeros_encontrados <- c()
  
  # Función auxiliar para sumar las cifras de un número
  suma_cifras <- function(n) {
    digitos <- as.character(n)
    digitos <- strsplit(digitos, "")
    digitos <- as.numeric(digitos[[1]])
    return(sum(digitos))
  }
  
  # Función auxiliar para verificar si un número es cuadrado perfecto
  es_cuadrado_perfecto <- function(n) {
    raiz <- sqrt(n)
    return(all.equal(raiz, round(raiz), tolerance = 1e-10) == TRUE)
  }
  
  # Iterar sobre los números del 1 al 999
  for(i in 1:999) {
    tryCatch({
      cubo <- i^3
      suma <- suma_cifras(cubo)
      if(es_cuadrado_perfecto(suma)) {
        numeros_encontrados <- c(numeros_encontrados, i)
      }
    }, error = function(e) {
      cat("Error procesando número:", i, "\n")
    })
  }
  
  # Imprimir resultados
  if(length(numeros_encontrados) > 0) {
    cat("\nNúmeros encontrados que cumplen la condición:\n")
    for(num in numeros_encontrados) {
      cubo <- num^3
      suma <- suma_cifras(cubo)
      raiz <- round(sqrt(suma))
      cat(sprintf("Número: %d\n  Su cubo: %d\n  Suma de cifras: %d\n  Es cuadrado perfecto: %d^2 = %d\n\n",
                  num, cubo, suma, raiz, suma))
    }
  } else {
    cat("No se encontraron números que cumplan la condición.\n")
  }
  
  return(numeros_encontrados)
}

#Desarrolla una función en R llamada suma_par_impar que calcule la suma de todos los números pares y todos los números
# impares comprendidos entre 1 y 200. La función deberá devolver ambas sumas como resultado. El programa deberá:
#a)    Utilizar  bucles  o  funciones  de  secuencia  para  generar  los  números  pares  e  impares.
#b)    Calcular  la  suma  de  los  números  pares  y  la  suma  de  los  números  impares.
#c)    Devolver  ambas  sumas  como  resultado.
#Luego  de  deﬁnir  la  función,  deberás  llamarla  e  imprimir  las  sumas  de  los  números  pares  y  los números  impares.

suma_par_impar <- function(){
  # incializamos lsa variable
  suma_par <- 0
  suma_impar <- 0

  # iteramos
  for (i in 1:200){
    #verificamos si el numero es par o impar
    if(i%%2==0){
      suma_par <- suma_par + i
    }
    else{
      suma_impar <- suma_impar + i
    }
  }
  resultado <-list(
    suma_par = suma_par,
    suma_impar = suma_impar
  )
  cat("Suma pares: ", suma_par, "\n")
  cat("Suma impares ", suma_impar, "\n")
  return(resultado)
}

#Desarrolla  un  programa  en  R  que  permita  analizar  un  número  entero  dado  por  el  usuario.
# El programa  deberá:
#a)    Calcular  el  número  de  cifras  que  tiene  el  número  entero.
#b)    Determinar  cuál  es  la  menor  y  la  mayor  cifra  presente  en el  número.
#c)    Identiﬁcar  la  posición  que  ocupan  la  menor  y  la  mayor  cifra  en  el  número.
#d)    Finalmente,  mostrar  al  usuario  el  número  de  cifras,  el  valor  de  la  menor  y  la
# mayor  cifra, y  la  posición  que  ocupan  cada  una  de  ellas.
#El  programa  deberá  deﬁnir  una  función  llamada  analizar_numeros  que  tome  como  entrada el
# número  entero  a  analizar  y  devuelva  los  resultados  mencionados  anteriormente.

analizar_numeros <- function(n){
  #sacar los digitos
  digitos <- as.numeric(strsplit(as.character(abs(n)),"")[[1]])

  num_cifras <- length(digitos)
  cifra_menor <- min(digitos)
  cifra_mayor <- max(digitos)

  posicion_menor <- which(digitos == cifra_menor)
  pos_mayor <- which(digitos == cifra_mayor)

  cat("Numero de cifras ",num_cifras, "\n")
  cat("Cifra menor ",cifra_menor, "en posicion(es) ",paste(posicion_menor, collapse = ", " ),"\n")
  cat("Cifra mayor ",cifra_mayor, "en posicion(es) ",paste(pos_mayor, collapse = ", " ),"\n")

  return(list(
    num_cifras = num_cifras,
    menor = list(valor=cifra_menor, posiciones = posicion_menor),
    mayor = list(valor=cifra_mayor, posiciones = pos_mayor)

  ))

}

#Escribe una función par.impar en R que realice la suma de todos los números pares
# y también de todos los número impares comprendidos entre 1 y 200.
par_impar <- function(){
  # incializamos
  suma_par <- 0
  suma_impar <- 0
  for(i in 1:200){
    if (i%%2==0){
      suma_par <- suma_par + i
    }
    else{
      suma_impar <- suma_impar + i
    }
  }
  cat("La suma de los pares es: ", suma_par, "\n")
  cat("La suma de los impares es: ", suma_impar)
}

#Desarrolla una función en R llamada suma_producto que solicite al usuario que introduzca cinco
# números por teclado. Además, el programa deberá ofrecer al usuario la posibilidad de elegir entre
# sumar los números o multiplicarlos. Si se elige la opción de suma, la función de- volverá un
# mensaje con el resultado de la suma de los cinco números. Si se elige la opción de producto,
# la función devolverá un mensaje con el resultado del producto de los cinco números.
# Para realizar los cálculos, puedes utilizar las funciones sum y prod. El programa deberá deﬁnir una
# función llamada suma_producto que tome como entrada los cinco números ingresados por el usuario y la
# opción elegida (suma o producto) y devuelva el resultado correspondiente.
suma_producto <- function() {
  # vector para almacenar los números
  numeros <- numeric(5)

  cat("Por favor, introduce cinco números:\n")
  for(i in 1:5) {
    numeros[i] <- as.numeric(readline(sprintf("Número %d: ", i)))
  }
  
  # menu
  cat("\nOperaciones disponibles:\n")
  cat("1. Suma\n")
  cat("2. Producto\n")

  opcion <- as.numeric(readline("Elige una opción (1 o 2): "))

  if(opcion == 1) {
    resultado <- sum(numeros)
    cat("\nLa suma de los números es:", resultado, "\n")
  } else if(opcion == 2) {
    resultado <- prod(numeros)
    cat("\nEl producto de los números es:", resultado, "\n")
  } else {
    cat("\nOpción no válida. Por favor, elige 1 o 2.\n")
    return(NULL)
  }
  
  return(resultado)
}

#Desarrolla un programa en R que, dado un número entero diferente de 0 y 1 proporcionado por el usuario, escriba todos los
# enteros comprendidos entre 1 y N que sean múltiplos de 3 o de 7, pero no múltiplos de los dos a la vez. Además, el programa
# debe manejar correctamente el caso en el que N sea negativo. El programa deberá:
#a)    Solicitar  al  usuario  que  ingrese  un  número  entero  diferente  de  0  y  1.
#b)    Identiﬁcar  todos  los  enteros  entre  1  y  N   (o  N   y  1  si  N   es  negativo)  que  sean  múltiplos de  3  o  de  7,
# pero  no  de  ambos  a  la  vez.
#c)    Imprimir  los  números  encontrados  que  cumplen  con  la  condición  establecida.
#El  programa  debe  ser  capaz  de  manejar  números  negativos  correctamente  y  garantizar  que el  usuario  ingrese  un  número  válido  como  entrada.

multiplos_3_7 <- function(){
  #verficar entrada
  repeat{
    numero <- as.numeric(readline("Introduce el numero (entero y diferente de 0 y 1)"))
    # verificar que es valido
    if(!is.na(numero) && numero !=0 && numero!=1 && numero == round(numero)){
      break
    }
    else{
      cat("Introduce un numero valido por favor")
    }
  }
  #determinar rango
  inicio <- 1
  fin <- numero
  if (numero <0){
    inicio <- numero
    fin <- 1
  }
  #vector que va a almacenar el resultado
  numeros_validos <- c()

  # encontrar los multiplos
  for(i in inicio:fin){
    #verificar si es multiplo de 3 o 7 (pero no a la vez)
    multiplo_3 <- i %% 3 == 0
    multiplo_7 <- i %% 7 == 0

    if((multiplo_3 || multiplo_7) && !(multiplo_3 && multiplo_7)){
      numeros_validos <- c(numeros_validos,i)
    }
  }

  #resultados
  if(length(numeros_validos)>0){
    cat("\nNúmeros múltiplos de 3 o 7 entre ", inicio, " y ", fin, "\n")
} else {
    cat("\nNo se encontraron números que cumplan la condición entre ", inicio, " y ", fin, "\n")
}

  return(numeros_validos)
}