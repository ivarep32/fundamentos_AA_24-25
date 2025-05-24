X=runif(200)
y=sin(4*pi*X)+rnorm(200,sd=.25)
plot(X,y)
#Modelo nucleo
D=outer(X,X,"-")
h=.1
K=dnorm(D/h)
S=sweep(K,1,apply(K,1,sum),"/")
ytilde=S%*%y
plot(ytilde,y)
#LOOCV
Kcv=K
diag(Kcv)=0
Scv=sweep(Kcv,1,apply(Kcv,1,sum),"/")
ytcv=Scv%*%y
sqrt(mean((y-ytcv)^2))

hseq=seq(.005,.05,len=51)
MSE=numeric(length(hseq))
for (i in 1:length(hseq)){
Kcv=dnorm(D/hseq[i])
diag(Kcv)=0
Scv=sweep(Kcv,1,apply(Kcv,1,sum),"/")
ytcv=Scv%*%y
MSE[i]=mean((y-ytcv)^2)
}
plot(hseq,MSE)
h.opt=hseq[which.min(MSE)]
xpred=seq(0,1,len=101)
Kpr=dnorm(outer(xpred,X,"-")/h.opt)
Spr=sweep(Kpr,1,apply(Kpr,1,sum),"/")
ypr=Spr%*%y
plot(X,y)
lines(xpred,ypr,col=2)
curve(sin(4*pi*x),from=0,to=1,lwd=2,col=4,add=TRUE)
#### Kfold
nfold=10
fold=sample(rep(1:nfold,each=20))
ykf=numeric(length(y))

MSEkf=numeric(length(hseq))
for (i in 1:length(hseq)){
for (j in 1:nfold){
Kkf=dnorm(D[fold==j,fold!=j]/hseq[i])
Skf=sweep(Kkf,1,apply(Kkf,1,sum),"/")
ykf[fold==j]=Skf%*%y[fold!=j]
}
MSEkf[i]=mean((y-ykf)^2)
}

plot(hseq,MSEkf)
h.opt2=hseq[which.min(MSEkf)]

K=dnorm(D/h.opt2)
S=sweep(K,1,apply(K,1,sum),"/")
sum(diag(S))
ytilde=S%*%y
plot(ytilde,y)