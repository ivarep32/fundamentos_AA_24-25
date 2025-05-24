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

kernel_gauss <- function(d, h) {
  exp(- (d^2) / (2 * h^2))
}

classif_kernel_predict <- function(Xtrain, Ytrain, Xtest, h) {
  pred <- factor(rep(NA, nrow(Xtest)), levels = levels(Ytrain))
  for (i in 1:nrow(Xtest)) {
    xi <- Xtest[i, ]
    dists <- sqrt(rowSums((t(t(Xtrain) - xi))^2))
    pesos <- kernel_gauss(dists, h)
    clases <- levels(Ytrain)
    probas <- sapply(clases, function(cl) {
      sum(pesos[Ytrain == cl]) / sum(pesos)
    })
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

# ------------------------
# discusion de resultados
# ------------------------
"""
          (con 100 tests)
        LDA       QDA        RF
0%   0.1029412 0.1470588 0.1470588
25%  0.1911765 0.1911765 0.2058824
50%  0.2205882 0.2205882 0.2352941
75%  0.2500000 0.2500000 0.2647059
100% 0.2941176 0.3235294 0.3529412

- Para LDA el error minimo es aprox. 0.103 y el máximo 0.294
- Para QDA el minimo es 0.147 y el maximo 0.324
- Para RF el minimo es 0.147 y el maximo 0.353

# --------------------------------------------------------------
          (con 1000 tests)
        LDA        QDA        RF
0%   0.08823529 0.07352941 0.1176471
25%  0.17647059 0.19117647 0.2058824
50%  0.22058824 0.22058824 0.2352941
75%  0.25000000 0.25000000 0.2647059
100% 0.35294118 0.38235294 0.3823529
- Para LDA el error minimo es aprox. 0.088 y el máximo 0.353
- Para QDA el minimo es 0.074 y el maximo 0.382
- Para RF el minimo es 0.118 y el maximo 0.382

#---------------------------------------------------------------

          (con 10000 tests)
        LDA        QDA        RF
0%   0.07352941 0.08823529 0.1029412
25%  0.19117647 0.19117647 0.2058824
50%  0.22058824 0.22058824 0.2352941
75%  0.25000000 0.25000000 0.2647059
100% 0.41176471 0.42647059 0.4117647

- Para LDA el error minimo es aprox. 0.074 y el máximo 0.412
- Para QDA el minimo es 0.088 y el maximo 0.426
- Para RF el minimo es 0.103 y el maximo 0.412

#----------------------------------------------------------------

Observando esto podemos concluir que el mejor método es LDA ya que se mueve
en errores más pequeños. En algunos casos QDA (normalmente en los quantiles
mas pequeños) y RF (en los cuantiles mas grandes) pueden llegar a igualarlo
pero nunca lo mejoran.

Esto podria variar con otros datasets (más o menos lineales o más o menos
voluminosos)


"""

