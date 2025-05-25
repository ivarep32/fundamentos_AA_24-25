#----------------
# PASOS PREVIOS (classup)
#----------------
xDNI=5190
set.seed(xDNI)
library(MASS)
A1 = xDNI%%5 # esto es 0

#-----------------
# LECTURA DEL CSV
#-----------------

csv <- read.csv("nbaest.csv", sep = ";", dec=',', header = TRUE)
head(csv)
summary(csv)
filtro = subset(csv, csv$Ptos>6) #filtro de jugadores con mas de 6 ptos
datos <- filtro[, c("Tipo", "Ptos", "Reb", "Asist")]
datos$Tipo <- as.factor(datos$Tipo) #asegurar q la variable es categorica
pairs(datos[,-1], col=datos$Tipo, pch=19)

#------------------------------------------------------------------------
# PRUEBAS BÁSICAS
#(son implementaciones simples para observar el comportamiento general)
#------------------------------------------------------------------------

# -----------------
# LDA
# -----------------
rlda <- lda(Tipo ~., data=datos)
rlda$means
prlda <- predict(rlda, datos[-1]) #aplicamos el modelo de datos
table(real=datos$Tipo , prediccion=prlda$class) #tabla de confusion
err = which(prlda$class !=datos$Tipo) # muestra errores
plot(datos$Ptos, datos$Reb, col=datos$Tipo, pch=19) #graficamos
points(datos[err, c("Ptos","Reb")], col=prlda$class[err], cex=2) #errores segun clase predicha


rlda_cv <- lda(Tipo ~ ., data=datos, CV=TRUE) #validacion cruzada
table(real=datos$Tipo, predict=rlda_cv$class)

#------------------
# QDA
#------------------

rqda <- qda(Tipo~., data=datos)
prqda <- predict(rqda, datos[,-1])
table(real=datos$Tipo, predict=prqda$class) #tabla de confusion

#-----------------
# KERNEL
#-----------------
# En el metodo del núcleo (Kernel), se estima la probabilidad condicional
# P(Y = j | X = x) como una media ponderada de las clases observadas,
# donde los pesos se determinan según una función núcleo K y una distancia d.
#
# Concretamente, se aproxima:
# P(Y = j | X = x) ≈ sum_{i} 1{Yi = j} * K(h^-1 * d(x, xi)) / sum_{i} K(h^-1 * d(x, xi))
# donde h es un parámetro de suavizado (ancho de banda).

# Implementación basada en av
classif_kernel_predict <- function(Xtrain, Ytrain, Xtest, h) {
  pred <- factor(rep(NA, nrow(Xtest)), levels = levels(Ytrain))
  for (i in 1:nrow(Xtest)) {
    # Calculamos distancias entre el punto de test y todos los de entrenamiento
    D <- sqrt(rowSums((t(t(Xtrain) - as.numeric(Xtest[i, ])))^2))

    # Aplicamos kernel gaussiano a las distancias
    K <- dnorm(D/h)

    # Normalizamos los pesos
    if(sum(K) > 0) {
      S <- K / sum(K)
    } else {
      S <- rep(1/length(K), length(K)) # Si todos son cero, damos pesos iguales
    }

    # Calculamos probabilidades por clase
    clases <- levels(Ytrain)
    probas <- sapply(clases, function(cl) {
      sum(S[Ytrain == cl])
    })

    # Asignamos la clase con mayor probabilidad
    pred[i] <- clases[which.max(probas)]
  }
  return(pred)
}

# -------------------------------------------------------------------------------------------------
# COMPARACIONES
# (repetimos lo hecho en las implementaciones anteriores en un bucle para observar el rendimiento)
#--------------------------------------------------------------------------------------------------
n <- nrow(datos)
nrep <- 10000
prop <- 0.7
errclassif <- matrix(NA,nrow=nrep, ncol=3)
colnames(errclassif)<-c("LDA", "QDA", "Kernel")
h_opt <- 0.6
for (i in 1:nrep) {
  ltrain<-sample(1:n,round(prop*n)) #indices entrenamiento
  test<-setdiff(1:n, ltrain) #indices test

  #LDA
  m_lda<-lda(Tipo~., data=datos,subset=ltrain) #modelo de datos
  p_lda<-predict(m_lda,datos[test,-1])$class #prediccion
  errclassif[i,1]<-mean(datos$Tipo[test]!=p_lda) #calculamos y almacenamos el error

  # QDA
  m_qda<-qda(Tipo~.,data=datos,subset=ltrain)
  p_qda<-predict(m_qda,datos[test,-1])$class
  errclassif[i,2]<-mean(datos$Tipo[test]!=p_qda)

  # Kernel
 Xtrain <- datos[ltrain, -1]
  Ytrain <- datos$Tipo[ltrain]
  Xtest <- datos[test, -1]
  Ytest <- datos$Tipo[test]

  p_kernel <- classif_kernel_predict(Xtrain, Ytrain, Xtest, h_opt)
  errclassif[i, 3] <- mean(p_kernel != Ytest)
}


boxplot(errclassif, ylab="Error") #comparacion grafica
apply(errclassif, 2, quantile) #comparacion numerica



