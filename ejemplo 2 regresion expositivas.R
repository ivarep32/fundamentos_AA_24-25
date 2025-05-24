library(faraway)
data(phbirths) #Ejemplo madres gestantes
reslm=lm(log(grams)~gestate,data=phbirths) #Modelo lineal
summary(reslm)
X=model.matrix(reslm)

reslm2=lm(log(grams)~gestate+smoke,data=phbirths) # Modelo lineal + v. categórica
summary(reslm2)
X2=model.matrix(reslm2)

reslm3=lm(log(grams)~gestate+smoke+gestate:smoke,data=phbirths) #Modelo lineal + v.cat + interaccion
summary(reslm3)
X3=model.matrix(reslm3)

data(savings)
regsav=lm(sr~.,data=savings)
summary(regsav)

regsav2=lm(sr~.-dpi+pop75:ddpi,data=savings) #Mod. lineal quitanto una variable y añadiendo interacción
summary(regsav2)

regsav3=lm(sr~(pop15+pop75)^2,data=savings) # Mod. lineal con dos var. hasta orden 2.
summary(regsav3)

library(ellipse)
ci=confint(regsav)
nomvar=c("pop75","dpi")
el=ellipse(regsav,which=nomvar)
plot(range(ci[nomvar[1],1:2],el[,1]),range(ci[nomvar[2],1:2],el[,2]),type="n",xlab=nomvar[1],ylab=nomvar[2])

lines(el[,1],el[,2],col=2)
points(regsav$coefficients[nomvar[1]],regsav$coefficients[nomvar[2]],pch=19)
abline(v=ci[nomvar[1],1])
abline(v=ci[nomvar[1],2])
abline(h=ci[nomvar[2],1])
abline(h=ci[nomvar[2],2])
points(0,0,pch=19,cex=3)


#Modelos parciales
X=model.matrix(regsav)
y=savings$sr
betao=drop(solve(t(X)%*%X)%*%t(X)%*%y) # Par. originales
lvar=c(2,5)
X1=X[,c(1,lvar)]
X2=X[,-lvar]

beta1o=drop(solve(t(X1)%*%X1)%*%t(X1)%*%y)
beta2o=drop(solve(t(X2)%*%X2)%*%t(X2)%*%y)

P1=solve(t(X1)%*%X1)%*%t(X1)
H1=X1%*%P1
I=diag(nrow(savings))

X2=X[,c(3,4)]  # Reajustamos X2 para que no incluya como X1 el término independiente.
beta2m=drop(solve(t(X2)%*%(I-H1)%*%X2)%*%t(X2)%*%(I-H1)%*%y)
beta1m=drop(P1%*%(y-X2%*%beta2m))

coef(regsav)
c(beta1m,beta2m)  #Par. en dos fases == coeficientes originales.

infl=influence(regsav)
names(infl)
infl$hat   # Valores de influencia

savings[which(infl$hat>3*5/nrow(savings)),]  # Países influyentes
savings[which(rstandard(regsav)>2),] 		# Países raros por Residuos estandarizado
savings[which(rstudent(regsav)>2),]			# Países raros Residuos estudentizados
savings[which(cooks.distance(regsav)>0.09),] # Países raros por distancia de Cook (cambio coeficientes)

#### Stepwise con AIC
#AIC y BIC del modelo original
AIC(regsav)
BIC(regsav)
#Se puede usar MASS::extractAIC para otras penalizaciones

regstep=step(regsav,direction="both") # Busca el mejor modelo usando AIC
# En la última interación se ve que pasaría si añado variables o quito las del modelo
summary(regstep) # Modelo final
AIC(regstep)
BIC(regstep)


MSE=function(y,ytilde){mean((y-ytilde)^2)}
MAE=function(y,ytilde){mean(abs(y-ytilde))}
MAPE=function(y,ytilde){mean(abs(y-ytilde)/y)}
R2=function(y,ytilde){1-MSE(y,ytilde)/mean((y-mean(y))^2)}

# Evaluación numérica predicción
# Particiones aleatorias
nrep=500
n=nrow(savings)
error.crit=matrix(NA,nrow=nrep,ncol=4)
colnames(error.crit)=c("RMSE","MAE","MAPE","R2")
for (i in 1:nrep){
lentren=sample(1:n,size=floor(0.8*n)) #80%-20% Entre-test
mentre=savings[lentren,]
mtest=savings[-lentren,]
mod=lm(sr~.,data=mentre)
pry=predict(mod,mtest)
error.crit[i,]=c(sqrt(MSE(mtest$sr,pry)),MAE(mtest$sr,pry),MAPE(mtest$sr,pry),R2(mtest$sr,pry))
}
par(mfrow=c(2,2))
boxplot(error.crit[,1])
abline(h=sd(regsav$residuals),col="red")
boxplot(error.crit[,2])
boxplot(error.crit[,3])
boxplot(error.crit[,4])

#K-fold CV
nK=5  #
lab=rep(1:nK,ceiling(n/nK))[1:n]
lab=sample(lab) # para que no esten los grupos predefinidos secuencialmente
err.KCV=matrix(NA,nrow=nK,ncol=4)
colnames(err.KCV)=c("RMSE","MAE","MAPE","R2")
for (i in 1:nK){
mod=lm(sr~.,data=savings,subset=(lab!=i))
pry=predict(mod,savings[lab==i,])
yK=savings[lab==i,"sr"]
err.KCV[i,]=c(sqrt(MSE(yK,pry)),MAE(yK,pry),MAPE(yK,pry),R2(yK,pry))
}
err.KCV
cat("Grupo 1:",paste(rownames(savings[lab==1,])),"\n")
cat("Grupo 2:",paste(rownames(savings[lab==2,])),"\n")
cat("Grupo 3:",paste(rownames(savings[lab==3,])),"\n")
cat("Grupo 4:",paste(rownames(savings[lab==4,])),"\n")
cat("Grupo 5:",paste(rownames(savings[lab==5,])),"\n")
