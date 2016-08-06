# librerias necesarias
library(sn)
require(car)
library(tseries)
library(nlme)
library(lmtest)
library(blme)
library(mvtnorm)
library(mnormt)
library(Hmisc)
library(MuMIn)

library(lattice)  



framin <- read.table(file.choose(),h=T)
attach(framin)
names(framin)
head(framin)

length(framin)
summary(age)
### separo las variables 

source(file="medidas2.R")
source(file="EMMM.R")
source(file="sesgo.R")


medidas(model.1,cholesterol2,5)



model.1<- lme(log(cholesterol2) ~ time2+time2:edad,
random = ~ time2|sujeto,corr=corCompSymm(0.5),
method="ML",data =grouped)

summary(model.1)

medidas(model.1,log(cholesterol2),5)


nsuj=296
nj=rep(5,nsuj)
D1=diag(2,2)
sigmae2=1 #varianza del error aleatorios
y=cholesterol2


md=matrix(cbind(1,time2,time2*edad),1480)#Matriz de dise?o X
x=md
z=cbind(rep(1,1480),time2)
lambda=c(0.5,0.5)

lb=0.01
ls=10

e_fijos=c(2.485,0.095,-0.0013)
qes=EM.Skew(nsuj,nj,y,x,z,betas=e_fijos,beta1=e_fijos,sigmae=sigmae2,D1,lambda,nu=2,
Ind=1,lb,ls,precis?o,loglik=T,informa=T,calcbi=T)

medidasMM(m=qes,y=cholesterol2,k=5)

### estimaa
yy=blmer(cholst~year+(1+year|newid), 
cov.prior=gamma,fixef.prior=normal)

a <- summary(yy)
a$coefficients

medidasbay(yy,cholesterol2,5)

Y=residuals(model.1)
length(Y)
z <- ts(matrix(Y,nrow=43,ncol = 43) )
det(z)
chol

cc=chol2inv(z)
shapiro.test(cc)

i=c(1:10)
resultado=matrix(0,1,length(i))
grouped <- groupedData(cholst~year|newid, 
data=as.data.frame(framin),order.groups = F)
ctrl=lmeControl(opt='optim')
hc=hclust(dist(cholst),"ave")

library(cluster)

##______________AGRUPACIONES___________
for (k in 2:10){
#memb <- cutree(hc, k)
#memb=as.numeric(memb)
hc=as.data.frame(pam(cholst,k)[3])
          memb=as.numeric(hc$clustering)
m<-lme(cholst/100~year+age+memb,random = ~ year|newid,corr=corCompSymm(0.1),
method="ML",data =grouped,control=ctrl)
summary(m)
yest = fitted(m)
y=cholst/100
sse = sum((yest-y)^2)
ssr = sum((y-mean(y))^2) 
R2 = 1 - sse/ssr
resultado[,k]=R2
}

resultado=as.matrix(resultado)
resultado


        x=2:10
        length(x)
        RESUL=resultado[,]
        rr=as.vector(RESUL[-1])
        plot(x,rr,type="b",col=2,lwd=3,ylim=c(0.8,0.99))   

maxx=max(resultado)
maxx
posis=which(resultado[1,]==maxx)
posis
hc=as.data.frame(pam(cholst,posis)[3])
memb=as.factor(hc$clustering)

m<-lme(cholst/100~year+age+memb,random = ~ year|newid,corr=corCompSymm(0.1),
method="ML",data =grouped,control=ctrl)
medidas(m,(cholst/100),5)

summary(m)



clasi=split((cholst),newid)
clasi
head(clasi)

par(mfrow = c(1,1))
  plot(year,cholst,col=memb )

     
     i=1:200
     for(t in 1:100){
     tt= year[ID]==i[t]
     cho=cholst[ID]==i[t]
     lines(tt,cho,lwd=2,col=sex,pch=2)
     }


year2=c(0,2,4,6,8,10)

lines(year2,clasi$`1`,col=1,lwd=2)
lines(year2,clasi$`138`,col=1,lwd=2)
lines(year2,clasi$`11`,col=1,lwd=2)
lines(year2,clasi$`3`,col=1,lwd=2)


hh=data.frame(memb,cholst/100,newid)
hh
hh$memb1

clasi2=split(hh,memb)
clasi2

grupo1=clasi2$`1`
shapiro.test(grupo1$cholst.100)

grupo2=clasi2$`2`
shapiro.test(grupo2$cholst.100)

grupo3=clasi2$`3`
shapiro.test(grupo3$cholst.100)

grupo4=clasi2$`4`





plot(density(grupo1$cholst),xlim=c(1,5),ylim=c(0,3), 
col=1,lwd=5,lty=1,cex.axis=3)
lines(density(grupo2$cholst),col=2,lwd=5,lty=2)
lines(density(grupo3$cholst),col=3,lwd=5,lty=3)
lines(density(grupo4$cholesterol2),col=4,lwd=5,lty=4)
legend("topright",legend = c("Grupo 1", "Grupo 1",
"Grupo 3","Grupo 4"),col=c(3,1,2,4),lty =c(3,1,2,4),cex=2,
title = "Grupos estimados",lwd=5)

g <- function(x)c(N=length(x),MEAN=mean(x,na.rm=TRUE),
SD=sd(x,na.rm=TRUE),MIN=min(x,na.rm=TRUE),
MAX=max(x,na.rm=TRUE))
sujeto=newid


summarize(cholst/100,by=llist(memb),g)







m<-lme((cholst/100)~year+age+sex,random = ~ year|newid,corr=corCompSymm(0.1),
method="REML",data =grouped,control=ctrl)
summary(m)
medidas(m,(cholst/100),5)

library(car)
par(mfrow = c(1,2))
param_aleat=random.effects(m)
inter=prcomp(param_aleat[1], scale. = TRUE)
pen=prcomp(param_aleat[2],scale. = TRUE)

qqPlot(inter$x)
qqPlot(pen$x)


data =c(2.48, 104, 4.25, 219, 0.682, 0.302 ,1.09, 0.586, 90.7, 344, 13.8, 1.17, 305, 2.8, 79.7, 3.18, 109, 0.932, 562, 0.958, 1.87, 0.59, 114, 391, 13.5, 1.41, 208, 2.37, 166, 3.42)
pca1 = prcomp(data, scale. = TRUE)
library(car)
qqPlot(pca1$x)


m<-lme(log(cholst/100)~year+age+as.factor(sex),random = ~ year|newid,corr=corCompSymm(0.1),
method="REML",data =grouped,control=ctrl)
summary(m)

yest = fitted(m)
y=log(cholst/100)
sse = sum((yest-y)^2)
ssr = sum((y-mean(y))^2) 
R2 = 1 - sse/ssr
R2

library(lmec)

length(year)
 md=matrix(cbind(1,year,age,sex),1044)#Matriz de diseño X
 xem=md
 z=cbind(rep(1,1044),year)
 lambda=c(0.5,0.5)
 lb=0.01
 ls=10

 e_fijos=c(1.4,0.028,0.018,-0.06 )
 
y=cholst/100
 nj=ID 
 z=cbind(rep(1,1044),year)
 cens=rep(0,1044)
sujeto=as.numeric(newid)


source(file="medidas2.R")

EMMMM= lmec(y, cens, X=xem, Z=z, cluster=sujeto, maxstep = 100, 
        varstruct = "unstructured", init=e_fijos, method = "ML", 
      epsstop = 0.001, abspmv = 0.001, mcmc0 = 100, sdl = 0.1,
  iter2 = 15, trs = 5, pls = 5, mcmcmax = 1000)

medidasMM(EMMMM,cholst/100,length(e_fijos),xem=xem)
summary(EMMM)

m=	EMMMM

efectosaleatorios=t( as.matrix( m$bi))
      betaa=m$beta
      
      tiempo=year
      nsuj=200
      interr=as.matrix( rep (efectosaleatorios[,1],each=5))
      pen=as.matrix( rep (efectosaleatorios[,2],each=5))
      
      pendiente=pen*tiempo
      #cbind(pendiente,pen,tiempo)
      yest=xem%*%betaa+pendiente+interr
      
      pos=length(betaa)-2
      




yy=blmer(cholst/100~year+as.factor(sex)+age+(1+age|newid), 
cov.prior=gamma,fixef.prior=normal)
summary(yy)
medidasbay(yy,cholst/100,5)



hh=data.frame(memb,cholesterol2,sujeto)
hh
hh$memb1

clasi2=split(hh,memb)
clasi2

grupo1=clasi2$`1`
grupo2=clasi2$`2`
grupo3=clasi2$`3`
grupo4=clasi2$`4`





plot(density(grupo1$cholesterol2),xlim=c(1,5),ylim=c(0,7), 
col=1,lwd=5,lty=1,cex.axis=3)
lines(density(grupo2$cholesterol2),col=2,lwd=5,lty=2)
lines(density(grupo3$cholesterol2),col=3,lwd=5,lty=3)
lines(density(grupo4$cholesterol2),col=4,lwd=5,lty=4)
legend("topright",legend = c("Grupo 1", "Grupo 1",
"Grupo 3","Grupo 4"),col=c(3,1,2,4),lty =c(3,1,2,4),cex=2,
title = "Grupos estimados",lwd=5)

g <- function(x)c(N=length(x),MEAN=mean(x,na.rm=TRUE),
SD=sd(x,na.rm=TRUE),MIN=min(x,na.rm=TRUE),
MAX=max(x,na.rm=TRUE))
sujeto=NIT


summarize(cholesterol2,by=llist(memb),g)



cho=data.frame(cholesterol2,memb)
head(cho)
clasi=split((cho),memb)
clasi

g1=(clasi$`1`$cholesterol2)
shapiro.test(g1)
g2=(clasi$`2`$cholesterol2)
shapiro.test(g2)
g3=(clasi$`3`$cholesterol2)
shapiro.test(g3)
g4=(clasi$`4`$cholesterol2)
shapiro.test(g4)

###prueba de sesgo
x=scale(cholesterol)

## sesgo 
x=scale(g4)
plot(density(x), col=3,lwd=3)

### sesgo y proporcion
bandas_proporcion(x,distri='ST','Precio escalado',nivel=0.51)
bandas_proporcion(x,distri='SN','Precio escalado',nivel=0.51)
bandas_proporcion(x,distri='Normal','Precio escalado',nivel=0.51)

busca(x,simu=100,nivel=1.3,'ST')
busca(x,simu=100,nivel=0.58,'SN')

## gr?fico de densidad
par(mfrow = c(1,2))
hist(Y,freq = FALSE)
##lines(density(Y),col=2,lty = 2, lwd = 2, add = TRUE)
qqnorm(Y)

density(Y)

### se detecta no normalidad con la variable 
### respuesta sin transformar
library(MASS)
par(mfrow=c(1,1))
(resul_bc=boxcox(cholesterol~1, lambda = seq(-2, 2, length=100), plotit = TRUE,
       xlab = expression(lambda),ylab = "log-Verosimilitud"))

###modelo inicial con log respuesta
model.1<- lme(log(cholesterol) ~ time+bmi+edad+cigarrillos+clasi
+time:edad+time:bmi+bmi:edad+clasi:time+clasi:bmi,
random = ~ 1,corr=corCompSymm(0.5),
method="REML",data =grouped)
summary(model.1)




### modelo final 
model.2<- lme(log(cholesterol) ~ time2+time2:edad+
clasi:time,random = ~ 1,corr=corCompSymm(0.5),
method="ML",data =grouped)
summary(model.2)

Y=residuals(model.1)
z <- ts(matrix(Y,nrow=43,ncol = 43) )
det(z)
cc=chol2inv(z)
shapiro.test(cc)





## gr?fico de densidad
par(mfrow = c(1,2))
hist(Y,freq = FALSE)
lines(density(Y),col=2,lty = 2, lwd = 2, add = TRUE)
qqnorm(Y)

density(Y)

library(sn)
m1 <- selm(cholesterol ~ time+time:edad+
clasi:time, family="ST", data=framin,alpha=0)
summary(m1)
plot(m1)


library(sn)
## modelo selm inicial
m1 <- selm(cholesterol ~ time+bmi+edad+cigarrillos+clasi
+time:edad+time:bmi+bmi:edad+clasi:time+clasi:bmi, 
family="ST", data=framin)
summary(m1)
plot(m1)

## modelo selm inicial con ST
m1 <- selm(cholesterol ~ time+bmi+edad+clasi
+time:edad+clasi:bmi, 
family="ST", data=framin)
summary(m1)
plot(m1)

Y=residuals(m1)
z <- ts(matrix(Y,nrow=43,ncol = 43) )
det(z)
cc=chol2inv(z)
shapiro.test(cc)


## modelo selm inicial ahora con SN
m1 <- selm(cholesterol ~ time+bmi+edad+clasi
+time:edad+clasi:bmi, 
family="SN", data=framin)
summary(m1)
plot(m1)


Y=residuals(m1)
z <- ts(matrix(Y,nrow=43,ncol = 43) )
det(z)
cc=chol2inv(z)
shapiro.test(cc)



#Funci?n para bandas de normalidad#

m1 <- selm(cholesterol ~bmi+edad+cigarrillos+clasi
+time:edad+time:bmi+bmi:edad+clasi:time+clasi:bmi, 
family="SN", data=framin)
summary(m1)
plot(m1)




m1 <- selm((cholesterol) ~ time+bmi+edad+clasi
+time:edad+clasi:bmi, 
family="SN", data=framin)
summary(m1)
plot(m1)

Y=residuals(m1)


## gr?fico de densidad
par(mfrow = c(1,2))
hist(Y,freq = FALSE)
lines(density(Y),col=2,lty = 2, lwd = 2, add = TRUE)
qqnorm(Y)

density(Y)

m1 <- selm(cholesterol ~ time+edad+clasi
+time:edad+clasi:bmi, 
family="SN", data=framin)
summary(m1)








bandas_proporcion<-function(x,distri,expression,nivel){
if(distri=='Normal'){
y<-(x)
ps<-seq(0.05,0.95,length=length(y))
xi.obs<-quantile(y,probs=ps)
sd.xis<-sqrt(ps*(1-ps)/(length(x)*dnorm(qnorm(ps))^2))
dif<-xi.obs-qnorm(ps)
LI= -2*sd.xis
LS= -LI
fi=length(which(dif>LS))
fs=length(which(dif<LI))
plot(ps,LI,type='l',ylim=c(-0.25,0.25),col='red',main='Bandas normal',xlab=expression)
points(ps,LS,type='l',col='red')
points(ps,dif)
con=cbind(fi,fs)
}
if(distri=='SN'){
y<-(x)
ps<-seq(0.05,0.95,length=length(y))
xi.obs<-quantile(y,probs=ps)
sd.xis<-sqrt(ps*(1-ps)/(length(x)*dsn(qsn(ps,xi=0,omega=1,alpha=1))^2))
#plot(sd.xis)
dif<-xi.obs-qsn(ps)
LI<- -2*sd.xis
LS<- -LI
fi=length(which(dif>LS))
fs=length(which(dif<LI))
suma=fi+fs
plot(ps,LI,type='l',ylim=c(-0.25,0.25),col='red',main='Bandas normal sesgada',xlab=expression)
points(ps,LS,type='l',col='red')
points(ps,dif)
con=cbind(fi,fs)
}
if(distri=='ST'){
y<-(x)
ps<-seq(0.05,0.95,length=length(y))
xi.obs<-quantile(y,probs=ps)
sd.xis<-sqrt(ps*(1-ps)/(length(x)*dst(qst(ps,xi=0,omega=1,alpha=1))^2))
dif<-xi.obs-qst(ps)
LI<- -2*sd.xis
LS<- -LI
fi=length(which(dif>LS))
fs=length(which(dif<LI))
plot(ps,LI,type='l',ylim=c(-0.25,0.25),col='red',main='Bandas T sesgada',xlab=expression)
points(ps,LS,type='l',col='red')
points(ps,dif)
con=cbind(fi,fs)
}

par(mfrow=c(1,3))
return(con)
}



###modelo final, despues de quitar una a una no significativas
names(framin)
model.1<- lme(cholesterol ~ time+time:edad,
random = ~ 1,corr=corCompSymm(0.5),
method="ML",data =grouped)
summary(model.1)

x=scale(residuals(model.1))
x=scale(Y)

x=scale(cholesterol)
bandas_proporcion(x,distri='ST','VALOR escalado',nivel=9)
bandas_proporcion(x,distri='Normal','Precio escalado',nivel=-0.2)
bandas_proporcion(x,distri='SN','Precio escalado',nivel=-0.2)

comparar_SN_ST=function(variable,nsim=3,expression,nivel){
limites=0
pf1=matrix(0,nsim,1)
pf2=matrix(0,nsim,1)
valorp=matrix(0,nsim,1)
for(i in 1:nsim){
aplito=scale(variable)
fue1=bandas_proporcion(x,distri='SN','Precio escalado',nivel)
#abline(h=0,col='blue')
#win.graph()
fue2=bandas_proporcion(x,distri='ST','Precio escalado',nivel)
abline(h=0,col='blue')
nsuj=length(variable)
p1_SN=(fue1[1]+fue1[2])/nsuj  #Normal Sesgada
p2_ST=(fue2[1]+fue2[2])/nsuj  #TSesgada
pf1[i,]=p1_SN #Sesgada
pf2[i,]=p2_ST #Normal
prup=prop.test(c(fue1[1]+fue1[2],fue2[1]+fue2[2]),c(nsuj,nsuj))
valorp[i,]=prup[[3]]
vec=matrix(prup[[6]])
if(vec[1]<0) di=1 else di=0
if(vec[2]<0) di2=1 else di2=0
dif=di+di2
if(dif==2) cont=1 else cont=0
menores=cont+limites
limites=menores
}
agrup=cbind(p1_SN,p2_ST,mean(pf1),mean(pf2),mean(valorp),limites/nsim)
return(agrup)
}


comparar_SN_ST(x,nsim=3,expression,nivel=5)


#Funciones para detectar nivel de sesgo, para una de las dist sesgadas#

####B?SQUEDA DEL NIVEL####
#ARROJA M?NIMA PROPORCI?N DE PUNTOS CON B?SQUEDA DE NIVEL LAMBDA EN EL QUE SE OBTIENE###
busca=function(x,simu,nivel,distri){
niveln=matrix(0,simu,1);porcentaje=matrix(0,simu,1)
for(i in 1:simu){
niveln[i]=nivel+cos(runif(1,0,2))
if(distri=='SN'){
prop=(bandas_proporcion(x,distri='ST','Precio escalado',nivel=nivel)[[1]]+bandas_proporcion(x,distri='SN','Precio escalado',nivel=nivel)[[2]])/length(x)
}
if(distri=='ST'){
prop=(bandas_proporcion(x,distri='ST','Precio escalado',nivel=nivel)[[1]]+bandas_proporcion(x,distri='ST','Precio escalado',nivel=nivel)[[2]])/length(x)
}
porcentaje[i]=prop
}
posi=which(porcentaje==min(porcentaje),arr.ind=T)
lista=cbind(niveln[posi[2]],porcentaje[posi[2]])
colnames(lista)=c('nivel','Proporci?n por fuera')
return(lista)
}

busca(x,simu=50,nivel=1.16,'SN')

#LISTA TABU. OBJETIVO: NIVEL QUE MINIMIZA EL TOTAL DE PUNTOS POR FUERA DE LAS BANDAS#

listat=function(x,simu,simu2,distri,nivel){
niveln=matrix(0,simu2,1)  ; resul=matrix(0,simu2,2)
niveln[1]=nivel+cos(runif(1,0,1))
resul[1,]=busca(x,simu,niveln[1],distri)
lista1=resul[1,]
fob=resul[1,2]
for(i in 2:simu){
niveln[i]=nivel+cos(runif(1,0,1))
resul[i,]=busca(x,simu,niveln[i],distri)
fob=resul[i,2]
lista=rbind(lista1,t(resul[i,]))
if(fob<resul[(i-1),2]) listan=lista else nivel=rnorm(1,0,1)
lista1=lista
}
posi=which(listan[,2]==min(listan[,2]))
este=listan[posi,]
names(este)=c('nivel','proporci?n por fuera')
return(este)
}

x=res
m?nimo=listat(x,simu=10,simu2=10,'ST',nivel=1)
m?nimo


#####detecci?n sesgo para residuales por producto#####
i=c('LPD','LPE','LPS','Polvo','queso')
resultado=matrix(0,length(i),2)
sesgo_tipo=function(x,simu,simu2,distri){
for(t in 1:5){
aplica=x[Producto==i[t]]
parte=listat(aplica,simu=10,simu2=10,'ST')
if(length(parte)==2) resultado[t,]=parte else resultado[t,]=parte[dim(parte)[1],]
}
rownames(resultado)=i
return(resultado)
}

x=res
