library(MASS)
?mvrnorm
mu=rep(0,7)
?toeplitz
Sigma=toeplitz(0.9^c(0:6))
set.seed(20250423)
X=mvrnorm(n=100,mu,Sigma)
pairs(X)
Xe=cbind(rep(1,100),X)
beta=c(3,2,-1,3,-2,0,0,0)
y=Xe%*%beta+rnorm(100,sd=.5)

rlm=lm(y~X)
summary(rlm)
sqrt(sum(residuals(rlm)^2)/92)

lvar1=c(1:4)
X1=Xe[,lvar1]
X2=Xe[,-lvar1]

P1=solve(t(X1)%*%X1)%*%t(X1)
beta1s=P1%*%y
H1=X1%*%P1
I=diag(100)
beta2=solve(t(X2)%*%(I-H1)%*%X2)%*%t(X2)%*%(I-H1)%*%y
beta1=P1%*%(y-X2%*%beta2)
######################
df=data.frame(y=y,X=X)
rlm=lm(y~.,data=df)
summary(rlm)

rlm2=update(rlm,.~.-X.6)
summary(rlm2)
?step
rstep=step(rlm,k=2)
rstep=step(rlm,k=log(100))
summary(rstep)
infl=influence(rstep)
which(infl$hat>3*length(coef(rstep))/100)

rs=rstandard(rstep)
quantile(rs,c(.01,.025,.05,0.95,.975,.99))
qnorm(c(.01,.025,.05,0.95,.975,.99))
rst=rstudent(rstep)
quantile(rst,c(.01,.025,.05,0.95,.975,.99))
which(abs(rst)>2.5)
################

install.packages("glmnet",dep=TRUE)
library(glmnet)
?glmnet

apply(X,2,sd)
rridge=cv.glmnet(X,y,alpha=0,standardize=FALSE)
names(rridge)
plot(rridge)
coef(rridge,s=rridge$lambda.min)
coef(rridge,s=rridge$lambda.1se)

prridge=predict(rridge,newx=X,s=rridge$lambda.1se)
plot(prridge,y)

rlasso=cv.glmnet(X,y,alpha=1,standardize=FALSE)
plot(rlasso)
coef(rlasso,s=rlasso$lambda.min)
coef(rlasso,s=rlasso$lambda.1se)

prlasso=predict(rlasso,newx=X,s=rlasso$lambda.1se)
plot(prlasso,y)

rEN=cv.glmnet(X,y,alpha=.5,standardize=FALSE)
plot(rEN)
plot(rlasso$glmnet.fit,xvar="lambda")