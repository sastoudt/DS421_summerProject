# packages to use
library(ggplot2)


# raw data
load(file = "~/Desktop/sfei/perStation.Rda")
load(file = "~/Desktop/sfei/perStationParsimoniousModels.Rda")
load(file = "~/Desktop/sfei/perStationFullModels.Rda")

# Define server logic required to generate and plot data
shinyServer(function(input, output) {
  
  ##
  # data
  
  # model
  dat <- reactive({
    
    stat <- input$stat
  
    
    out<-perStation[[which(names(perStation)==stat)]]
   
    
    return(out)
    
  })
  
  # for initial date range
  output$daterng <- renderUI({
    
    rngs <- range(dat()$Date)
    
    dateRangeInput("dt_rng",
                   label = h4("Date range"), 
                   start = rngs[1], 
                   end = rngs[2],
                   startview = 'year'
    )
    
  })
  
  ## plots
  

  output$fittedPars<- renderPlot({
    
    # inputs
    dt_rng <- input$dt_rng
    stat <- input$stat
    index=which(names(perStation)==stat)
    
    # data
    data<-dat()
    mod<-perStationParsMod[[index]]
    
    toUse=na.omit(data[,c("doy","date_dec","pheo","tn","do_per","Date")])
    toUse=as.data.frame(cbind.data.frame(toUse,mod$fitted.values))
  names(toUse)[7]="fitted.values"
    ggplot(data,aes(x = Date, y = chl))+geom_point()+
      geom_line(data=toUse,aes(x=Date,y =fitted.values ,col="red"),lwd=1)+
      ggtitle(paste(names(perStation)[index], "Fitted Values Parsimonious Model",sep=" "))+
      theme(legend.position='none')+
      scale_x_date(limits = dt_rng)+ylab("chl a (microgram/L)")+xlab("Date")
   
    
  }, height = 250, width = 1200)
  
  output$fittedFull<- renderPlot({
    
    # inputs
    dt_rng <- input$dt_rng
    stat <- input$stat
    index=which(names(perStation)==stat)
    
    # data
    data<-dat()
    mod<-perStationFullMod[[index]]
    
    if(index %in% c(5,7,13)){
      toUse=na.omit(data[,c("doy","date_dec","pheo","tn","do_per",
                            "sio2","tp","tss","Date")])
      
    }else{
      toUse=na.omit(data[,c("doy","date_dec","pheo","tn","do_per",
                            "sio2","tp","tss","nh4","Date")])
    }
    
    toUse=as.data.frame(cbind.data.frame(toUse,mod$fitted.values))
    names(toUse)[ncol(toUse)]="fitted.values"
    ggplot(data,aes(x = Date, y = chl))+geom_point()+
      geom_line(data=toUse,aes(x=Date,y =fitted.values ,col="red"),lwd=1)+
      ggtitle(paste(names(perStation)[index], "Fitted Values Full Model",sep=" "))+
      theme(legend.position='none')+
      scale_x_date(limits = dt_rng)+ylab("chl a (microgram/L)")+xlab("Date")
    
    
  }, height = 250, width = 1200)
  
  
  output$nestedPlotPars <- renderPlot({
    
    # inputs

    dt_rng <- input$dt_rng
    stat <- input$stat
    index=which(names(perStation)==stat)
    
    # data
    data<-dat()
    mod<-perStationParsMod[[index]]
    
    toUse=na.omit(data[,c("doy","date_dec","pheo","tn","do_per","Date")])
    byTerm=predict(mod,toUse,type="terms")
  
    nestPred=as.data.frame(cbind.data.frame(byTerm,toUse$Date,rep(summary(mod)$p.coeff,nrow(toUse))))
    names(nestPred)=c("doy","date_dec","pheo","tn","do_per","date","intercept")
    terms<-c("ti(pheo)","ti(doy)","ti(date_dec)","ti(tn)","ti(do_per)","intercept")
    ggplot(data,aes(x = Date, y = log(chl)))+geom_point()+
      geom_line(data=nestPred,aes(x=date,y = pheo, color = 'ti(pheo)'),lwd=1)+
      geom_line(data=nestPred,aes(x=date,y = doy, color = 'ti(doy)'), lwd=1)+
      geom_line(data=nestPred,aes(x=date,y=date_dec, color = 'ti(date_dec)'), lwd=1)+
      geom_line(data=nestPred,aes(x=date,y = tn, color = 'ti(tn)'), lwd=1)+
      geom_line(data=nestPred,aes(x=date,y = do_per, color = 'ti(do_per)'), lwd=1)+
      geom_line(data=nestPred,aes(x=date,y = intercept, color = 'intercept'), lwd=1)+
      scale_colour_manual(name = '',
                          labels =c('red'=terms[1],'orange'=terms[2],"dodgerblue"=terms[3],
                                    "forestgreen"=terms[4],"blue"=terms[5],"purple"=terms[6]),values=c("red","orange",
                                                                                     "dodgerblue","forestgreen","blue","purple")
      ) +
      ggtitle(names(perStation)[index])+scale_x_date(limits = dt_rng)+
      ylab("ln(chl a) ")+xlab("Date")
    
    
  }, height = 250, width = 1200)
})