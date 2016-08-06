
data1=read.table(file.choose(), header=T)
attach(data1)

#-------
i=c(2:10)
resultado=matrix(0,1,length(i))
grouped <- groupedData(cholst ~ year|newid, data = data1,
                       order.groups = F)
ctrl=lmeControl(opt='optim')

hc=hclust(dist(data1$cholst),"ave")

#------
##______________AGRUPACIONES___________
for (k in 2:9){
  
   memb <- cutree(hc, k)
   memb=as.factor(memb)
   
   grouped$sex=as.factor(grouped$sex)
   
   m<-lme(cholst ~ sex+year+memb,
          random = ~ year|newid,
          corr=corCompSymm(0.1),method="REML",data =grouped,control=ctrl)
   yest = fitted(m)
   y=grouped$cholst
   sse = sum((yest-y)^2)
   ssr = sum((y-mean(y))^2) 
   R2 = 1 - sse/ssr
   resultado[,k]=R2
}

resultado=as.matrix(resultado)
maxx=max(resultado)
posis=which(resultado[1,]==maxx)
memb <- cutree(hc, posis)
memb=as.factor(memb)


x=2:9
RESUL=resultado[,]
RESUL[-1]
plot(x,RESUL[-1],type="b",col=2,lwd=3) 

m<-lme(cholst ~ year+memb,
       random = ~ year|newid,
       corr=corCompSymm(0.1),method="REML",data =grouped,control=ctrl)



summary(m)
