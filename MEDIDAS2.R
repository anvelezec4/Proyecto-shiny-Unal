### mis propias medidas_________monte carlo______
medidasMM =  function(m,y,k,xem){
      # y = serie real simulada, m = modelo, k = numero parametros
      ### EN ESTE CASO EL MODELO SE LLAMA m=EMMMM
     
      efectosaleatorios=t( as.matrix( m$bi))
      betaa=m$beta
      
      tiempo=as.vector(rep(seq(1,5,length=5),296))
      nsuj=296
      interr=as.matrix( rep (efectosaleatorios[,1],each=5))
      pen=as.matrix( rep (efectosaleatorios[,2],each=5))
      
      pendiente=pen*tiempo
      #cbind(pendiente,pen,tiempo)
      yest=xem%*%betaa+pendiente+interr
      
      pos=length(betaa)-2
      
      T = length(y)
      sse = sum((yest-y)^2)
      ssr = sum((y-mean(y))^2) 
      mse = sse/(T-k)
      R2 = 1 - sse/ssr
      Ra2 = 1 - ((T-1)*(1-R2)/(T-k))
      aic = log((T-k)*exp(2*k/T)*mse/T)
      bic = log(T^(k/T)*(T-k)*mse/T)
      mapes=(abs(yest-y)/y)
      t=tapply(mapes,sujeto,mean)
      mape=mean(t)
      
      nor_inter=as.vector(jarque.bera.test(ts(interr))$p.value)
      nor_pen=as.vector(jarque.bera.test(ts(pen))$p.value)
      
      M = c(R2, aic, bic,mape,mse, nor_pen,nor_inter,betaa,pos)
      names(M) = c("R2","logAIC","logBIC","MAPE","MSE","Norpen","Norinter",
                   "eff1","eff2","eff3","pos")
      return(as.numeric(M))
   }
   
   
   
   
#funci?n para estimar medidas por modelo simulado MODELO LMER
###___________________



#### mis propias medidas_______________
medidas = function(m,y,k){
# y = serie, m = modelo, k = numero parametros
T = length(y)
yest = fitted(m)
sse = sum((yest-y)^2)
ssr = sum((y-mean(y))^2) 
mse = sse/(T-k)
R2 = 1 - sse/ssr
Ra2 = 1 - ((T-1)*(1-R2)/(T-k))

RM=r.squaredGLMM(m)[1]
RC=r.squaredGLMM(m)[2]
RLR=r.squaredLR(m, null.RE = TRUE)[1]


aic = log((T-k)*exp(2*k/T)*mse/T)
bic = log(T^(k/T)*(T-k)*mse/T)
mapes=(abs(yest-y)/y)
t=tapply(mapes,sujeto,mean)
mape=mean(t)
param_aleat=random.effects(m)
nor_inter=jarque.bera.test(ts(param_aleat[1]))[3]
nor_pen=jarque.bera.test(ts(param_aleat[2]))[3]


vpc=Anova(m,type=3)[[3]]
effs=fixed.effects(m)

pos=length(fixed.effects(m))-2
M = c(R2,RM,RC,RLR, aic, bic,mape,mse,nor_pen,nor_inter,vpc,effs,pos)
names(M) = c("R2","RM","RC","RLR","logAIC","logBIC","MAPE","MSE","Norpen",
             "Norinter","vpc","effs","pos")
return(as.numeric(M))
}




###mODELO BAYESIANO
#### mis propias medidas_______________
medidasbay = function(m,y,k){
# y = serie, m = modelo, k = numero parametros
k=5  
T = length(y)
yest = fitted(m)
sse = sum((yest-y)^2)
ssr = sum((y-mean(y))^2) 
mse = sse/(T-k)
R2 = 1 - sse/ssr

RM=as.numeric(r.squaredGLMM(m)[1])
RC=as.numeric(r.squaredGLMM(m)[2])
RLR=r.squaredLR(m)[1]

Ra2 = 1 - ((T-1)*(1-R2)/(T-k))
aic = log((T-k)*exp(2*k/T)*mse/T)
bic = log(T^(k/T)*(T-k)*mse/T)
mapes=(abs(yest-y)/y)
t=tapply(mapes,sujeto,mean)
mape=mean(t)
param_aleat=random.effects(m)
nor_inter=jarque.bera.test(ts(param_aleat[[1]][1]))[3]
nor_pen=jarque.bera.test(ts(param_aleat[[1]][2]))[3]

#m=yy
vpc=Anova(m,type=3)[[3]]
eff=fixed.effects(m)

pos= length(fixed.effects(m))-2

M = c(R2, RM,RC,RLR, aic, bic,mape,mse,nor_pen,nor_inter,vpc,eff,pos)
names(M) = c("R2","RM","RC","RLR","logAIC","logBIC","MAPE","MSE","Norpen",
"Norinter","vpc","eff","pos")
return(as.numeric(M))
}

###FUNCI?N DE MEDIDAS POR SUJETOS
g <- function(x)c(N=length(x),MEAN=mean(x,na.rm=TRUE),
SD=sd(x,na.rm=TRUE),MIN=min(x,na.rm=TRUE),
MAX=max(x,na.rm=TRUE))

#summarize(ye_sim2,by=llist(memb),g)

