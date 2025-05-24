library(MASS)
library(mgcv)
library(GGally)
library(ggdendro)
library(gridExtra)

data(iris)
summary(iris[,1:4])
# K-medias
res.km3=kmeans(iris[,1:4],centers=3)
table(res.km3$cluster,iris$Species)
ggpairs(iris,columns=1:4,aes(colour=Species, shape=factor(res.km3$cluster)))
res.km2=kmeans(iris[,1:4],centers=2)
res.km4=kmeans(iris[,1:4],centers=4)
D=dist(iris[,1:4])
aa2=fpc::cluster.stats(D,res.km2$cluster)
aa3=fpc::cluster.stats(D,res.km3$cluster)
aa4=fpc::cluster.stats(D,res.km4$cluster)
IDX=rbind(c(aa2$dunn,aa2$dunn2,aa2$ch,aa2$avg.silwidth),c(aa3$dunn,aa3$dunn2,aa3$ch,aa3$avg.silwidth),c(aa4$dunn,aa4$dunn2,aa4$ch,aa4$avg.silwidth))
colnames(IDX)=c("Dunn","Dunn2","CH","SI")
rownames(IDX)=c("K=2","K=3","K=4")
IDX
# Cluster jerárquico
res.hc=hclust(D,method="complete")
hc2=cutree(res.hc,2)
hc3=cutree(res.hc,3)
hc4=cutree(res.hc,4)
aa2=fpc::cluster.stats(D,hc2)
aa3=fpc::cluster.stats(D,hc3)
aa4=fpc::cluster.stats(D,hc4)
IDX=rbind(c(aa2$dunn,aa2$dunn2,aa2$ch,aa2$avg.silwidth),c(aa3$dunn,aa3$dunn2,aa3$ch,aa3$avg.silwidth),c(aa4$dunn,aa4$dunn2,aa4$ch,aa4$avg.silwidth))
colnames(IDX)=c("Dunn","Dunn2","CH","SI")
rownames(IDX)=c("K=2","K=3","K=4")
IDX
ddata=dendro_data(res.hc,type="rectangle")
ggplot(segment(ddata))+geom_segment(aes(x=x,y=y,xend=xend,yend=yend))+coord_flip()+scale_y_reverse()
#Mean-Shift
	X2d=iris[,1:4]
	fg=function(x0,datos,h=0.15){
	dd=sweep(datos,2,x0,"-")
	dd=apply(dd,1,function(u){sqrt(sum(u^2))})
	wd=dnorm(dd/quantile(dd,probs=h,na.rm=TRUE))
	wd=wd/sum(wd)
	fx=apply(sweep(datos,1,wd,"*"),2,sum)
	return(fx)
	}

 	Xnew=as.matrix(X2d)

	plot(Xnew[,1:2],type="n",main="Ejemplo Iris")
	for (i in 1:nrow(Xnew)){
		ffold=rep(0,ncol(X2d))
		ff=fg(as.vector(Xnew[i,]),datos=X2d,h=.15)
		arrows(Xnew[i,1],Xnew[i,2],ff[1],ff[2],length=0.05,col=2)
		while(sum((ffold-ff)^2)>0.00001){  #Criterio de parada
			ffold=ff
			ff=fg(as.vector(ffold),datos=X2d,h=.15)
			arrows(ffold[1],ffold[2],ff[1],ff[2],length=0.05,col=2)
			}
		points(ff[1],ff[2],pch=19,cex=2,col=4)
	}
	points(X2d,pch=19)

# Función DBSCAN
DBSCAN=function(fobj,eps,minPts=5,metric=dist,par.metric=list(method="euclidean")){
aaf=formals(metric)[-1]
if (length(par.metric)>0){
nam=names(par.metric)
for (i in 1:length(par.metric)){aaf[[nam[i]]]=par.metric[[i]]}
}
D=as.matrix(do.call(metric,c(alist(fobj),aaf)))
diag(D)=NA
n=nrow(D)
vis=rep(0,n)
cl=0
cluster=rep(0,n)
mark=rep(NA,n)
if (is.null(eps)) eps=quantile(D,probs=0.05,na.rm=TRUE)
vecinos=apply(D,1,function(x,e){sum(x<=e,na.rm=TRUE)},e=eps)

mark[vecinos<1]="N"
mark[vecinos>=minPts]="C"
mark[vecinos<minPts & vecinos>0]="A"

#l=which(mark=="C" & cluster==0)
nl=sum(mark=="C" & cluster==0,na.rm=TRUE)
if (nl==0) stop("No center points. Consider modify minPts or eps")
while(nl>0){
lori=which(mark=="C" & cluster==0)[1]
cl=cl+1
cluster[lori]=cl
#cluster[which(D[l,]<=eps & cluster==0)]=cl
nc0=sum(cluster==0)
if (nc0>0) {
# which(cluster=cl & mark=="C")
#	mDgr=min(apply(D[which(cluster==cl),which(cluster==0),drop=FALSE],2,min,na.rm=TRUE))} else {
mDgr=min(apply(D[which(cluster==cl & mark=="C"),which(cluster==0),drop=FALSE],2,min,na.rm=TRUE))} else {
mDgr=2*eps;break
}
while(mDgr<eps){
nc1=sum(cluster==cl)
if (nc1==1){
lvec=which(D[which(cluster==cl & mark=="C"),]<eps) } else {
#lvec=which(apply(D[which(cluster==cl),,drop=FALSE],2,min,na.rm=TRUE)<eps)
lvec=which(apply(D[which(cluster==cl & mark=="C"),,drop=FALSE],2,min,na.rm=TRUE)<eps)
}
cluster[lvec]=cl
nc0=sum(cluster==0)
if (nc0>0) {
mDgr=min(apply(D[which(cluster==cl & mark=="C"),which(cluster==0),drop=FALSE],2,min,na.rm=TRUE))} else {
mDgr=2*eps;break
}
}
nl=sum(mark=="C" & cluster==0)
#l=which(mark=="C" & cluster==0)
}
#table(cluster)
lA=which(mark=="A")
for (i in lA){
if (sum(D[i,]<=eps & mark=="C")>0) mark[i]="A" else mark[i]="N"
}
return(list(cluster=cluster,mark=mark))
}

	eps=quantile(D,prob=0.09) #Calcular epsilon como cuantil de las distancias
	res.dbscan=DBSCAN(X2d,eps=eps)
	fac=factor(res.dbscan$cluster,levels=c(0,1:max(res.dbscan$cluster)),
								labels=c("Ruido",paste0("Gr.:",1:max(res.dbscan$cluster))))
	marca=factor(res.dbscan$mark,levels=c("C","A","N"),labels=c("Núcleo","Borde","Ruido"))
	table(iris$Species,fac)
	ggplot(data=iris)+geom_point(mapping=aes(x=Sepal.Length,y=Sepal.Width,col=fac,shape=marca,size=.5))+labs(x="Longitud Sépalo",y="Anchura Sépalo")