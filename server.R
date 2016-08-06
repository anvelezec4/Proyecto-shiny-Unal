library(shiny)
require(nlme)
library(Hmisc)
library(blme)
library(cluster)
library(knitr)

shinyServer(function(input,output,session){
  
  ##selecci?n de variables
  observe({
    # Direccion del archivo    
    inFile<-input$file
    
    if(is.null(inFile)) 
    return(NULL)
    
    # Lectura del archivo con direccion inFile$datapath y almacenamiento en data1
    data1 = read.table(inFile$datapath, header=T)
 
    updateSelectInput(session, "Vrespuesta", choices = names(data1))
    updateSelectInput(session, "tiempo", choices = names(data1))
    updateSelectInput(session, "sujeto", choices = names(data1))
    updateSelectInput(session, "factor", choices = names(data1))
  })
  
    ### tabla de base de datos
    output$contents <- renderTable({
     inFile <- input$file
     if (is.null(inFile))
     return(NULL)
     data1=read.table(inFile$datapath, header=T)
     })

    ### fgrafico de perfles
  output$pred_plot <- renderPlot({
    inFile <- input$file
     if (is.null(inFile))
     return(NULL)
    
    # Lectura del archivo con direccion inFile$datapath y almacenamiento en data1
     data1=read.table(inFile$datapath, header=T)
     
     #dataModelo <- data.frame(sujeto = as.numeric(data1[, input$sujeto]),
      #                        Vrespuesta = as.numeric(data1[, input$Vrespuesta]),
       #                       tiempo = as.numeric(data1[, input$tiempo]))
     #ctrl=lmeControl(opt='optim')
     #i=c(2:10)
     #resultado=matrix(0,1,length(i))     
     ##______AGRUPACIONES___________
     #for (k in 2:9){
       
       #memb <- cutree(hc, k)
       #memb=as.factor(memb)
      # hc=as.data.frame(pam(dataModelo$Vrespuesta,k)[3])
       #memb=hc$clustering
       #data2=data.frame(dataModelo,memb)
       #m<-lme(Vrespuesta ~ memb+tiempo,random = ~ tiempo|sujeto,
        #      data=data2,corr=corCompSymm(0.1),method="REML",
         #     control=ctrl)
       
       #yest = fitted(m)
       #y=data2$Vrespuesta
       #sse = sum((yest-y)^2)
       #ssr = sum((y-mean(y))^2) 
       #R2 = 1 - sse/ssr
       #resultado[,k]=R2
     #}
     
     #resultado=as.matrix(resultado)
     #x=2:9
     #RESUL=resultado[,]
    
     #maxx=max(resultado)
     #posis=which(resultado[1,]==maxx)
     #hc=(pam(dataModelo$Vrespuesta,posis)[3])
     #memb=(hc$clustering)
     plot(data1[, input$tiempo],data1[, input$Vrespuesta] ,xlab = '',ylab = '')

     
     i=1:200
     for(t in 1:input$bins1){
     tt= data1[, input$tiempo][data1[, input$sujeto]==i[t]]
     cho=data1[, input$Vrespuesta][data1[, input$sujeto]==i[t]]
     lines(tt,cho,lwd=2,col=data1[, input$factor],pch=2)
     }
  
     
     #regFormula <- reactive({
       #as.formula(paste('data1[, input$Vrespuesta] ~',data1[, input$tiempo]  ))
     #})
  })
     
  
  ### grafico de agrupaciones
     output$total_plot <- renderPlot({
        
        inFile <- input$file
        if (is.null(inFile))
        return(NULL)
        data1=read.table(inFile$datapath, header=T)
        
        dataModelo <- data.frame(sujeto = as.numeric(data1[, input$sujeto]),
                                 Vrespuesta = as.numeric(data1[, input$Vrespuesta]),
                                 tiempo = as.numeric(data1[, input$tiempo]),
                                 factor = as.numeric(data1[, input$factor]))
        par(mfrow=c(1, 2))
        #-------
        i=c(2:10)
        resultado=matrix(0,1,length(i))
        #grouped <- groupedData( Vrespuesta ~ tiempo|sujeto, data = dataModelo,
         #                      order.groups = F)
        ctrl=lmeControl(opt='optim')
        
        ##hc=hclust(dist(dataModelo$Vrespuesta),"ave")
        
        #------
       
        ##______AGRUPACIONES___________
        for (k in 2:9){
          
          #memb <- cutree(hc, k)
          #memb=as.factor(memb)
          hc=as.data.frame(pam(dataModelo$Vrespuesta,k)[3])
          memb=hc$clustering
          data2=data.frame(dataModelo,memb)
          m<-lme(Vrespuesta ~ memb+tiempo,random = ~ tiempo|sujeto,
                 data=data2,corr=corCompSymm(0.1),method="REML",
                 control=ctrl)
          
          yest = fitted(m)
          y=data2$Vrespuesta
          sse = sum((yest-y)^2)
          ssr = sum((y-mean(y))^2) 
          R2 = 1 - sse/ssr
          resultado[,k]=R2
        }
        
        resultado=as.matrix(resultado)
        x=2:9
        RESUL=resultado[,]
        RESUL[-1]
        plot(x,RESUL[-1],type="b",col=2,lwd=3,ylim=c(0.8,0.99))   
        
        
        
        maxx=max(resultado)
        posis=which(resultado[1,]==maxx)
        hc=(pam(dataModelo$Vrespuesta,posis)[3])
        memb=(hc$clustering)
        plot(data1[, input$tiempo],data1[, input$Vrespuesta],col=memb,xlab = '',ylab = '')
        
        
    })
     
     
     
     
     # Generate a summary of the modelo agrupado
     output$summary3 <- renderPrint({
       
       inFile <- input$file
       if (is.null(inFile))
         return(NULL)
       data1=read.table(inFile$datapath, header=T)
       
       dataModelo <- data.frame(sujeto = as.numeric(data1[, input$sujeto]),
                                Vrespuesta = as.numeric(data1[, input$Vrespuesta]),
                                tiempo = as.numeric(data1[, input$tiempo]),
                                factor = as.numeric(data1[, input$factor]))
       
       #-------
       i=c(2:10)
       resultado=matrix(0,1,length(i))
       #grouped <- groupedData( Vrespuesta ~ tiempo|sujeto, data = dataModelo,
       #                      order.groups = F)
       ctrl=lmeControl(opt='optim')
       
       hc=hclust(dist(dataModelo$Vrespuesta),"ave")
       #hc=as.data.frame(pam(dataModelo$Vrespuesta,posis)[3])
       #------
       
       ##______AGRUPACIONES___________
       for (k in 2:9){
         
         #memb <- cutree(hc, k)
         #memb=as.factor(memb)
         hc=as.data.frame(pam(dataModelo$Vrespuesta,k)[3])
         memb=(hc$clustering)
         
         
         data2=data.frame(dataModelo,memb)
         m<-lme(Vrespuesta ~ (memb)+tiempo,random = ~ tiempo|sujeto,
                data=data2,corr=corCompSymm(0.1),method="REML",
                control=ctrl)
         
         
         yest = fitted(m)
         y=data2$Vrespuesta
         sse = sum((yest-y)^2)
         ssr = sum((y-mean(y))^2) 
         R2 = 1 - sse/ssr
         resultado[,k]=R2
       }
       
       
       
       resultado=as.matrix(resultado)
       maxx=max(resultado)
       posis=which(resultado[1,]==maxx)
       hc=as.data.frame(pam(dataModelo$Vrespuesta,posis)[3])
       memb=as.factor(hc$clustering)
       #xx <- cutree(hc, posis)
       #xxx=as.factor(xx)
       
       data3=data.frame(dataModelo,memb)
       m<-lme(Vrespuesta ~ memb+tiempo,random = ~ tiempo|sujeto,
             data=data3,corr=corCompSymm(0.1),method="REML",
             control=ctrl)
       
       summary(m)
       
     })
     
     
     
     
     
     
     
     # Generate a summary of the dataset modelo lmer
     output$summary <- renderPrint({
       inFile <- input$file
       if (is.null(inFile))
         return(NULL)
       
       
       data1=read.table(inFile$datapath, header=T)

       dataModelo <- data.frame(sujeto = as.numeric(data1[, input$sujeto]),
                                Vrespuesta = as.numeric(data1[, input$Vrespuesta]),
                                tiempo = as.numeric(data1[, input$tiempo]),
                                factor = as.numeric(data1[, input$factor]))
       
       
       
       #grouped <- groupedData(data1[, input$Vrespuesta]~data1[, input$tiempo]|data1[, input$sujeto], data = data1,
        #                      order.groups = F)
       ctrl=lmeControl(opt='optim')
       
       #grouped$sex=as.factor(grouped$sex)
         
         
         
       #m<-lme( data1[, input$Vrespuesta] ~ as.factor(sex)+year,random = ~ year|newid,
       #          corr=corCompSymm(0.1),method="REML",data =grouped,control=ctrl)
       dataModelo <- data.frame(sujeto = as.numeric(data1[, input$sujeto]),
                                Vrespuesta = as.numeric(data1[, input$Vrespuesta]),
                                tiempo = as.numeric(data1[, input$tiempo]),
                                factor = as.numeric(data1[, input$factor]))
       grouped <- groupedData(Vrespuesta~tiempo|sujeto, data = dataModelo,
                             order.groups = F)
       ifelse(input$Slope==TRUE,
     
         m<-lme( Vrespuesta ~ tiempo+factor,random = ~ tiempo|sujeto,
               corr=corCompSymm(0.1),method="REML",data =dataModelo,
               control=ctrl),
         m<-lme( Vrespuesta ~ tiempo,random=~1, corr=corCompSymm(0.1),
                 method="REML",data =grouped,control=ctrl)
       )
        summary(m)
     })
     # Generate a summary of the blmer  dataset
     output$summary2 <- renderPrint({
       inFile <- input$file
       
       if (is.null(inFile))
         return(NULL)
       
       data1=read.table(inFile$datapath, header=T)
       
       dataModelo <- data.frame(sujeto = as.numeric(data1[, input$sujeto]),
                                Vrespuesta = as.numeric(data1[, input$Vrespuesta]),
                                tiempo = as.numeric(data1[, input$tiempo]))
       
      # ifelse(input$Slope==TRUE,
       #  m <- blmer(Vrespuesta~tiempo+(1+tiempo|sujeto), data=dataModelo,
        #     cov.prior=gamma,fixef.prior=normal),
         m <- blmer(Vrespuesta~tiempo+(1+tiempo|sujeto), data=dataModelo,
                    cov.prior=gamma,fixef.prior=normal)
         #)
             
             u=summary(m)
             #estructuraTabla <- 
                kable(u$coefficients,format="latex")
             #HTML(as.character(estructuraTabla))
       
     })
     
   

})