# packages to use
library(ggplot2)
library(sp)
library(gridExtra)

# raw data
load(file = "~/Desktop/sfei/perStation.Rda")
load(file = "~/Desktop/sfei/perStationParsimoniousModels.Rda")
load(file = "~/Desktop/sfei/perStationFullModels.Rda")
load(file="~/Desktop/sfei/perStationInteractionModels.Rda")
load(file="~/Desktop/DS421_summerProject/GAM_weightedReg_comparison/shiny/data/delt_map.RData")
load(file="~/Desktop/sfei/mod1Spatial.RData")
load(file="~/Desktop/sfei/mod2Spatial.RData")
load(file="~/Desktop/sfei/mod3Spatial.RData")
load(file="~/Desktop/sfei/mod4Spatial.RData")
load(file="~/Desktop/sfei/perStationAdd.Rda")
load(file="~/Desktop/sfei/perStationFlowMod.Rda")
load(file="~/Desktop/sfei/perStationFlowTOT.Rda")
full=read.csv("~/Desktop/sfei/sfeiPlusDates.csv")
allData=read.csv("~/Desktop/sfei/allData.csv")
volFlow=read.csv("~/Desktop/sfei/VolFingerPrintsMaster.csv")
# Define server logic required to generate and plot data

makeCompChart=function(data,stationID,month=T,labels=c("AG","East","Jones","MTZ","SAC","SJR")){
  toPlot=as.data.frame(data)
  
  if(month){
    names(toPlot)=c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sept","Oct","Nov","Dec")
  }else{
    names(toPlot)=c(1991:2006)
  }
  
  toPlot$row<-seq_len(nrow(toPlot))
  toPlot2<-melt(toPlot,id.vars="row")
  
  ggplot(toPlot2,aes(x=variable,y=value,fill=as.factor(row)))+geom_bar(stat="identity")+
    scale_fill_discrete("Volumetric Fingerprint",labels=labels)+
    xlab("")+ylab("% contribution")+
    ggtitle(paste("Average Composition of",stationID,sep=" "))
  
}


shinyServer(function(input, output) {
  
  ##
  # data
  
  # model
  dat <- reactive({
    
    stat <- input$stat
    
    
    out<-perStation[[which(names(perStation)==stat)]]
    
    
    return(out)
    
  })
  
  datA <- reactive({
    
    stat <- input$stat
    
    
    out<-perStationAdd[[which(names(perStation)==stat)]]
    
    
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
  
  output$ylim12 <- renderUI({
    
    rngs <- range(dat()$chl,na.rm=T)
    
    numericInput("ylim12", 
                 label = h3("Upper Limit for Y Fitted Value Plots"), 
                 value = round(rngs[2]+10,2)) 
    
  
    
  })
  
  output$ylim34L <- renderUI({
    
    rngs <- range(log(dat()$chl),na.rm=T)
    
    numericInput("ylim34L", 
                 label = h3("Lower Limit for Y Nested Plots"), 
                 value = round(rngs[1]-2,2)) 
    
    
    
  })
  
  output$ylim34U <- renderUI({
    
    rngs <- range(log(dat()$chl),na.rm=T)
    
    numericInput("ylim34U", 
                 label = h3("Upper Limit for Y Nested Plots"), 
                 value = round(rngs[2]+2,2))
    
    
    
  })
  
  output$plot5choice1<- renderUI({
    
    
    index=which(names(perStation)==input$stat)
    
    if(index %in% c(5,7,13)){
      selectInput("plot5choice1","Select a variable in parsimonious model.",c("doy","date_dec","pheo","do_per"),"Intercept will be added to center results")
      
    }else{
      selectInput("plot5choice1","Select a variable in parsimonious model.",c("doy","date_dec","pheo","tn","do_per"),"Intercept will be added to center results")
      
    }
    

  })
  
  output$plot5choice2<- renderUI({
    
    
    index=which(names(perStation)==input$stat)
    
    if(index %in% c(5,7,13)){
      selectInput("plot5choice2","Select a variable in parsimonious model.",c("doy","date_dec","pheo","do_per"))
      
    }else{
      selectInput("plot5choice2","Select a variable in parsimonious model.",c("doy","date_dec","pheo","tn","do_per"))
      
    }
    
    
  })
  
  output$plot6choice1<- renderUI({
    
    
    index=which(names(perStation)==input$stat)
    
    if(index %in% c(5,7)){
      selectInput("plot6choice1","Select a variable in full model.",c("doy","date_dec","pheo","do_per",
                                                                              "sal"))
      
    }else if(index==13){
      selectInput("plot6choice1","No variables to choose from for full model.",c())
                                                                              
    }else if(index %in% c(17, 18, 21, 22, 23)){
      selectInput("plot6choice1","Select a variable in full model.",c("doy","date_dec","pheo","tn","do_per",
                                                                      "sio2","tp","tss","nh4","sal"))
    }else{
      selectInput("plot6choice1","Select a variable in full model.",c("doy","date_dec","pheo","tn","do_per",
                                                                      "sio2","tp","tss","nh4"))
      
    }
    
    
  })
  
  output$plot6choice2<- renderUI({
    
    
    index=which(names(perStation)==input$stat)
    
    if(index %in% c(5,7)){
      selectInput("plot6choice2","Select a variable in full model.",c("doy","date_dec","pheo","do_per",
                                                                      "sal"))
      
    }else if(index==13){
      selectInput("plot6choice2","No variables to choose from for full model.",c())
      
    }else if(index %in% c(17, 18, 21, 22, 23)){
      selectInput("plot6choice2","Select a variable in full model.",c("doy","date_dec","pheo","tn","do_per",
                                                                      "sio2","tp","tss","nh4","sal"))
    }else{
      selectInput("plot6choice2","Select a variable in full model.",c("doy","date_dec","pheo","tn","do_per",
                                                                      "sio2","tp","tss","nh4"))
      
    }
    
    
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
    
    if(index %in% c(5,7,13)){
      toUse=na.omit(data[,c("doy","date_dec","pheo","do_per","Date")])
      
    }else{
      
      toUse=na.omit(data[,c("doy","date_dec","pheo","tn","do_per","Date")])
      
    }
    
    fullPred=predict(mod,toUse,type="response")
    toUse=as.data.frame(cbind.data.frame(toUse,fullPred))
    names(toUse)[ncol(toUse)]="fitted.values"
    ggplot(data,aes(x = Date, y = chl))+geom_point()+
      geom_line(data=toUse,aes(x=Date,y =fitted.values ,col="red"),lwd=1)+
      ggtitle(paste(names(perStation)[index], "Fitted Values Parsimonious Model",sep=" "))+
      theme(legend.position='none')+
      scale_x_date(limits = dt_rng)+ylab("chl a (microgram/L)")+xlab("Date")+ylim(0,input$ylim12)
    
    
  }, height = 250, width = 1200)
  
  output$fittedFull<- renderPlot({
    
    # inputs
    dt_rng <- input$dt_rng
    stat <- input$stat
    index=which(names(perStation)==stat)
    
    # data
    data<-dat()
    mod<-perStationFullMod[[index]]
    
    if(index %in% c(5,7)){
      toUse=na.omit(data[,c("doy","date_dec","pheo","do_per",
                            "sal","Date")])
      fullPred=predict(mod,toUse,type="response")
      toUse=as.data.frame(cbind.data.frame(toUse,fullPred))
      names(toUse)[ncol(toUse)]="fitted.values"
      
    }else if(index==13){
      
    }else if(index %in% c(17, 18, 21, 22, 23)){
      toUse=na.omit(data[,c("doy","date_dec","pheo","tn","do_per",
                            "sio2","tp","tss","nh4","sal","Date")])
      fullPred=predict(mod,toUse,type="response")
      toUse=as.data.frame(cbind.data.frame(toUse,fullPred))
      names(toUse)[ncol(toUse)]="fitted.values"
    }else{
      toUse=na.omit(data[,c("doy","date_dec","pheo","tn","do_per",
                            "sio2","tp","tss","nh4","Date")])
      fullPred=predict(mod,toUse,type="response")
      toUse=as.data.frame(cbind.data.frame(toUse,fullPred))
      names(toUse)[ncol(toUse)]="fitted.values"
    }
    
    
    if(index==13){
      df <- data.frame()
      ggplot(df) + geom_point() +  scale_x_date(limits = dt_rng)+ggtitle("no data for extra variables in full model")
    }else if(index==15){
      ggplot(data,aes(x = Date, y = chl))+geom_point()+
        geom_line(data=toUse,aes(x=Date,y =fitted.values ,col="red"),lwd=1)+
        ggtitle(paste(names(perStation)[index], "Fitted Values Full Model",sep=" "))+
        theme(legend.position='none')+ylim(0,160)+
        scale_x_date(limits = dt_rng)+ylab("chl a (microgram/L)")+xlab("Date")+ylim(0,input$ylim12)
    }else{
      ggplot(data,aes(x = Date, y = chl))+geom_point()+
        geom_line(data=toUse,aes(x=Date,y =fitted.values ,col="red"),lwd=1)+
        ggtitle(paste(names(perStation)[index], "Fitted Values Full Model",sep=" "))+
        theme(legend.position='none')+
        scale_x_date(limits = dt_rng)+ylab("chl a (microgram/L)")+xlab("Date")+ylim(0,input$ylim12)
    }
    
    
    
    
    
  }, height = 250, width = 1200)
  
  
  output$fittedInt<- renderPlot({
    
    # inputs
    dt_rng <- input$dt_rng
    stat <- input$stat
    index=which(names(perStation)==stat)
    
    # data
    data<-dat()
    mod<-perStationIntMod[[index]]
    
   
    toUse=na.omit(data[,c("doy","date_dec","Date")])
    
    fullPred=predict(mod,toUse,type="response")
    toUse=as.data.frame(cbind.data.frame(toUse,fullPred))
    names(toUse)[ncol(toUse)]="fitted.values"
    ggplot(data,aes(x = Date, y = chl))+geom_point()+
      geom_line(data=toUse,aes(x=Date,y =fitted.values ,col="red"),lwd=1)+
      ggtitle(paste(names(perStation)[index], "Fitted Values Interaction Model",sep=" "))+
      theme(legend.position='none')+
      scale_x_date(limits = dt_rng)+ylab("chl a (microgram/L)")+xlab("Date")+ylim(0,input$ylim12)
    
    
  }, height = 250, width = 1200)
  
  output$fittedSpat<- renderPlot({
    
    # inputs
    dt_rng <- input$dt_rng
    stat <- input$stat
    #index=which(names(perStation)==stat)
    
    # data
    data<-subset(allData,Station==stat)
    data$Date=as.Date(data$Date)
    if(input$spatMod=="spatIntercept"){
      index=38
    }else if(input$spatMod=="spatDate_Dec"){
      index=39
    }else if(input$spatMod=="spatDOY"){
      index=40
    }else if(input$spatMod=="spatinteraction"){
      index=41
    }
   
    ggplot(data,aes(x = Date, y = chl))+geom_point()+
      #geom_line(aes(x=Date,y =names(data)[index] ,col="red"),lwd=1)+
      geom_line(data=data,aes_string(x="Date",y = names(data)[index], color = shQuote("red")),lwd=1)+
      
      ggtitle(paste(input$spatMod, "Fitted Values Model",sep=" "))+
      theme(legend.position='none')+
      scale_x_date(limits = dt_rng)+ylab("chl a (microgram/L)")+xlab("Date")+ylim(0,input$ylim12)
    
    
  }, height = 250, width = 1200)
  
  output$fittedChl<- renderPlot({
    
    # inputs
    dt_rng <- input$dt_rng
    stat <- input$stat
    index=which(names(perStation)==stat)
    
    # data
  data=perStationAdd[[index]]
  
    ggplot(data,aes(x = Date, y = chl))+geom_point()+
      #geom_line(aes(x=Date,y =names(data)[index] ,col="red"),lwd=1)+
      geom_line(data=data,aes(x=Date,y = chlPred, color = "red"),lwd=1)+
      
      ggtitle("Chl from Other Station Fitted Values Model")+
      theme(legend.position='none')+
      scale_x_date(limits = dt_rng)+ylab("chl a (microgram/L)")+xlab("Date")+ylim(0,input$ylim12)
    
    
  }, height = 250, width = 1200)
  
  output$fittedFlow<- renderPlot({
    
    # inputs
    dt_rng <- input$dt_rng
    stat <- input$stat
    index=which(names(perStation)==stat)
    
    # data
    data=perStationAdd[[index]]
    
    ggplot(data,aes(x = Date, y = chl))+geom_point()+
      #geom_line(aes(x=Date,y =names(data)[index] ,col="red"),lwd=1)+
      geom_line(data=data,aes(x=Date,y = flowPred, color = "red"),lwd=1)+
      
      ggtitle("Flow Fitted Values Model")+
      theme(legend.position='none')+
      scale_x_date(limits = dt_rng)+ylab("chl a (microgram/L)")+xlab("Date")+ylim(0,input$ylim12)
    
    
  }, height = 250, width = 1200)
  
  
  
  output$nestedPlotPars <- renderPlot({
    
    # inputs
    
    dt_rng <- input$dt_rng
    stat <- input$stat
    index=which(names(perStation)==stat)
    
    # data
    data<-dat()
    mod<-perStationParsMod[[index]]
    
    if(index %in% c(5,7,13)){
      toUse=na.omit(data[,c("doy","date_dec","pheo","do_per","Date")])
      toName=c("doy","date_dec","pheo","do_per","date","intercept")
      terms<-c("ti(pheo)","ti(doy)","ti(date_dec)","ti(do_per)","intercept")
      
    }else{
      toUse=na.omit(data[,c("doy","date_dec","pheo","tn","do_per","Date")])
      toName=c("doy","date_dec","pheo","tn","do_per","date","intercept")
      terms<-c("ti(pheo)","ti(doy)","ti(date_dec)","ti(tn)","ti(do_per)","intercept")
      
    }
    
    byTerm=predict(mod,toUse,type="terms")
    
    nestPred=as.data.frame(cbind.data.frame(byTerm,toUse$Date,rep(summary(mod)$p.coeff,nrow(toUse))))
    names(nestPred)=toName
    if(index %in% c(5,7,13)){
      ggplot(data,aes(x = Date, y = log(chl)))+geom_point()+
        geom_line(data=nestPred,aes(x=date,y = pheo, color = 'ti(pheo)'),lwd=1)+
        geom_line(data=nestPred,aes(x=date,y = doy, color = 'ti(doy)'), lwd=1)+
        geom_line(data=nestPred,aes(x=date,y=date_dec, color = 'ti(date_dec)'), lwd=1)+
        # geom_line(data=nestPred,aes(x=date,y = tn, color = 'ti(tn)'), lwd=1)+
        geom_line(data=nestPred,aes(x=date,y = do_per, color = 'ti(do_per)'), lwd=1)+
        geom_line(data=nestPred,aes(x=date,y = intercept, color = 'intercept'), lwd=1)+
        scale_colour_manual(name = '',
                            labels =c('red'=terms[1],'orange'=terms[2],"dodgerblue"=terms[3],
                                      "blue"=terms[4],"purple"=terms[5]),values=c("red","orange",
                                                                                  "dodgerblue","blue","purple")
        ) +
        ggtitle(paste(names(perStation)[index],"Component-Wise Predictions Parsimonious Model",sep=" "))+scale_x_date(limits = dt_rng)+
        ylab("ln(chl a) ")+xlab("Date")+ylim(input$ylim34L,input$ylim34U)
    }else{
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
        ggtitle(paste(names(perStation)[index],"Component-Wise Predictions Parsimonious Model",sep=" "))+scale_x_date(limits = dt_rng)+
        ylab("ln(chl a) ")+xlab("Date")+ylim(input$ylim34L,input$ylim34U)
    }
    
  }, height = 250, width = 1200)
  
  output$nestedPlotFull <- renderPlot({
    
    # inputs
    
    dt_rng <- input$dt_rng
    stat <- input$stat
    index=which(names(perStation)==stat)
    
    # data
    data<-dat()
    mod<-perStationFullMod[[index]]
    
    if(index %in% c(5,7)){
      toUse=na.omit(data[,c("doy","date_dec","pheo","do_per",
                            "sal","Date")])
      terms<-c("ti(pheo)","ti(doy)","ti(date_dec)","ti(do_per)",
                "ti(sal)",
               "intercept")
      toName=c("doy","date_dec","pheo","do_per","sal","date","intercept")
      
      byTerm=predict(mod,toUse,type="terms")
      
      
      
      nestPred=as.data.frame(cbind.data.frame(byTerm,toUse$Date,rep(summary(mod)$p.coeff,nrow(toUse))))
      names(nestPred)=toName
      
      ggplot(data,aes(x = Date, y = log(chl)))+geom_point()+
        geom_line(data=nestPred,aes(x=date,y = pheo, color = 'ti(pheo)'),lwd=1)+
        geom_line(data=nestPred,aes(x=date,y = doy, color = 'ti(doy)'), lwd=1)+
        geom_line(data=nestPred,aes(x=date,y=date_dec, color = 'ti(date_dec)'), lwd=1)+
       # geom_line(data=nestPred,aes(x=date,y = tn, color = 'ti(tn)'), lwd=1)+
        geom_line(data=nestPred,aes(x=date,y = do_per, color = 'ti(do_per)'), lwd=1)+
        #geom_line(data=nestPred,aes(x=date,y = sio2, color = 'ti(sio2)'), lwd=1)+
        #geom_line(data=nestPred,aes(x=date,y = tp, color = 'ti(tp)'), lwd=1)+
        #geom_line(data=nestPred,aes(x=date,y = tss, color = 'ti(tss)'), lwd=1)+
        geom_line(data=nestPred,aes(x=date,y = sal, color = 'ti(sal)'), lwd=1)+
        
        geom_line(data=nestPred,aes(x=date,y = intercept, color = 'intercept'), lwd=1)+
        scale_colour_manual(name = '',
                            labels =c('red'=terms[1],'orange'=terms[2],"dodgerblue"=terms[3],
                                      "blue"=terms[4],'mediumturquoise'=terms[5],"black"=terms[6]),
                            values=c("red","orange",
                                         "dodgerblue","blue","mediumturquoise","black")
        ) +
        ggtitle(paste(names(perStation)[index],"Component-Wise Predictions Full Model",sep=" "))+scale_x_date(limits = dt_rng)+
        ylab("ln(chl a) ")+xlab("Date")+ylim(input$ylim34L,input$ylim34U)
      
    }else if(index==13){
      df <- data.frame()
      ggplot(df) + geom_point() +  scale_x_date(limits = dt_rng)+ggtitle("no data for extra variables in full model")
    }else if(index %in% c(17,18,21,22,23)){
      toUse=na.omit(data[,c("doy","date_dec","pheo","tn","do_per",
                            "sio2","tp","tss","nh4","sal","Date")])
      
      terms<-c("ti(pheo)","ti(doy)","ti(date_dec)","ti(tn)","ti(do_per)",
               "ti(sio2)","ti(tp)","ti(tss)","ti(nh4)","ti(sal)",
               "intercept")
      toName=c("doy","date_dec","pheo","tn","do_per","sio2","tp","tss","nh4","sal","date","intercept")
      byTerm=predict(mod,toUse,type="terms")

      nestPred=as.data.frame(cbind.data.frame(byTerm,toUse$Date,rep(summary(mod)$p.coeff,nrow(toUse))))
    
      names(nestPred)=toName
      
      ggplot(data,aes(x = Date, y = log(chl)))+geom_point()+
        geom_line(data=nestPred,aes(x=date,y = pheo, color = 'ti(pheo)'),lwd=1)+
        geom_line(data=nestPred,aes(x=date,y = doy, color = 'ti(doy)'), lwd=1)+
        geom_line(data=nestPred,aes(x=date,y=date_dec, color = 'ti(date_dec)'), lwd=1)+
        geom_line(data=nestPred,aes(x=date,y = tn, color = 'ti(tn)'), lwd=1)+
        geom_line(data=nestPred,aes(x=date,y = do_per, color = 'ti(do_per)'), lwd=1)+
        geom_line(data=nestPred,aes(x=date,y = sio2, color = 'ti(sio2)'), lwd=1)+
        geom_line(data=nestPred,aes(x=date,y = tp, color = 'ti(tp)'), lwd=1)+
        geom_line(data=nestPred,aes(x=date,y = tss, color = 'ti(tss)'), lwd=1)+
        geom_line(data=nestPred,aes(x=date,y = nh4, color = 'ti(nh4)'), lwd=1)+
        geom_line(data=nestPred,aes(x=date,y = nh4, color = 'ti(sal)'), lwd=1)+
        
        geom_line(data=nestPred,aes(x=date,y = intercept, color = 'intercept'), lwd=1)+
        scale_colour_manual(name = '',
                            labels =c('red'=terms[1],'orange'=terms[2],"dodgerblue"=terms[3],
                                      "forestgreen"=terms[4],"blue"=terms[5],"purple"=terms[6],
                                      "magenta"=terms[7],'grey'=terms[8],'mediumturquoise'=terms[9],
                                      "chocolate3"=terms[10],"black"=terms[11]),values=c("red","orange",
                                                                       "dodgerblue","forestgreen","blue","purple","magenta",
                                                                       "grey","mediumturquoise","chocolate3","black")
        ) +
        ggtitle(paste(names(perStation)[index],"Component-Wise Predictions Full Model",sep=" "))+scale_x_date(limits = dt_rng)+
        ylab("ln(chl a) ")+xlab("Date")+ylim(input$ylim34L,input$ylim34U)
    }else{
      
      toUse=na.omit(data[,c("doy","date_dec","pheo","tn","do_per",
                            "sio2","tp","tss","nh4","Date")])
      
      terms<-c("ti(pheo)","ti(doy)","ti(date_dec)","ti(tn)","ti(do_per)",
               "ti(sio2)","ti(tp)","ti(tss)","ti(nh4)",
               "intercept")
      toName=c("doy","date_dec","pheo","tn","do_per","sio2","tp","tss","nh4","date","intercept")
      byTerm=predict(mod,toUse,type="terms")
      
      nestPred=as.data.frame(cbind.data.frame(byTerm,toUse$Date,rep(summary(mod)$p.coeff,nrow(toUse))))
   
      names(nestPred)=toName
      
      ggplot(data,aes(x = Date, y = log(chl)))+geom_point()+
        geom_line(data=nestPred,aes(x=date,y = pheo, color = 'ti(pheo)'),lwd=1)+
        geom_line(data=nestPred,aes(x=date,y = doy, color = 'ti(doy)'), lwd=1)+
        geom_line(data=nestPred,aes(x=date,y=date_dec, color = 'ti(date_dec)'), lwd=1)+
        geom_line(data=nestPred,aes(x=date,y = tn, color = 'ti(tn)'), lwd=1)+
        geom_line(data=nestPred,aes(x=date,y = do_per, color = 'ti(do_per)'), lwd=1)+
        geom_line(data=nestPred,aes(x=date,y = sio2, color = 'ti(sio2)'), lwd=1)+
        geom_line(data=nestPred,aes(x=date,y = tp, color = 'ti(tp)'), lwd=1)+
        geom_line(data=nestPred,aes(x=date,y = tss, color = 'ti(tss)'), lwd=1)+
        geom_line(data=nestPred,aes(x=date,y = nh4, color = 'ti(nh4)'), lwd=1)+

        geom_line(data=nestPred,aes(x=date,y = intercept, color = 'intercept'), lwd=1)+
        scale_colour_manual(name = '',
                            labels =c('red'=terms[1],'orange'=terms[2],"dodgerblue"=terms[3],
                                      "forestgreen"=terms[4],"blue"=terms[5],"purple"=terms[6],
                                      "magenta"=terms[7],'grey'=terms[8],'mediumturquoise'=terms[9],
                                      "chocolate3"=terms[10]),values=c("red","orange",
                                                                                         "dodgerblue","forestgreen","blue","purple","magenta",
                                                                                         "grey","mediumturquoise","chocolate3")
        ) +
        ggtitle(paste(names(perStation)[index],"Component-Wise Predictions Full Model",sep=" "))+scale_x_date(limits = dt_rng)+
        ylab("ln(chl a) ")+xlab("Date")+ylim(input$ylim34L,input$ylim34U)
    }
    
 
  }, height = 250, width = 1200)
  
  output$nestedPlotParsJust2 <- renderPlot({
    
    # inputs
    
    dt_rng <- input$dt_rng
    stat <- input$stat
    index=which(names(perStation)==stat)
    
    # data
    data<-dat()
    mod<-perStationParsMod[[index]]
    
    if(index %in% c(5,7,13)){
      toUse=na.omit(data[,c("doy","date_dec","pheo","do_per","Date")])
      toName=c("doy","date_dec","pheo","do_per","date","intercept")
      terms<-c("ti(pheo)","ti(doy)","ti(date_dec)","ti(do_per)","intercept")
      
    }else{
      toUse=na.omit(data[,c("doy","date_dec","pheo","tn","do_per","Date")])
      toName=c("doy","date_dec","pheo","tn","do_per","date","intercept")
      terms<-c("ti(pheo)","ti(doy)","ti(date_dec)","ti(tn)","ti(do_per)","intercept")
      
    }
    
    byTerm=predict(mod,toUse,type="terms")
    byTerm=apply(byTerm,2,function(x){x+summary(mod)$p.coeff})
    nestPred=as.data.frame(cbind.data.frame(byTerm,toUse$Date,rep(summary(mod)$p.coeff,nrow(toUse))))
    names(nestPred)=toName
   
    ggplot(data,aes(x = Date, y = log(chl)))+geom_point()+
      geom_line(data=nestPred,aes_string(x="date",y = input$plot5choice1, color = "as.character(input$plot5choice1)"),lwd=1)+
      geom_line(data=nestPred,aes_string(x="date",y = input$plot5choice2, color = "as.character(input$plot5choice2)"),lwd=1)+
      scale_colour_manual(name = '',
                          labels =c('red'=input$plot5choice1,"dodgerblue"=input$plot5choice2)
                                    ,values=c("red", "dodgerblue")
      ) +
      ggtitle(paste(names(perStation)[index],"Component-Wise Predictions Parsimonious Model \n Intercept added in order to center components",sep=" "))+scale_x_date(limits = dt_rng)+
      ylab("ln(chl a) ")+xlab("Date")+ylim(input$ylim34L,input$ylim34U)

    }, height = 250, width = 1200)
  
  output$nestedPlotFullJust2 <- renderPlot({
    
    # inputs
    
    dt_rng <- input$dt_rng
    stat <- input$stat
    index=which(names(perStation)==stat)
    
    # data
    data<-dat()
    mod<-perStationFullMod[[index]]
    
    if(index %in% c(5,7)){
      toUse=na.omit(data[,c("doy","date_dec","pheo","do_per",
                            "sal","Date")])
      terms<-c("ti(pheo)","ti(doy)","ti(date_dec)","ti(do_per)",
               "ti(sal)",
               "intercept")
      toName=c("doy","date_dec","pheo","do_per","sal","date","intercept")
      
      byTerm=predict(mod,toUse,type="terms")
      
      byTerm=apply(byTerm,2,function(x){x+summary(mod)$p.coeff})
      nestPred=as.data.frame(cbind.data.frame(byTerm,toUse$Date,rep(summary(mod)$p.coeff,nrow(toUse))))
      names(nestPred)=toName
      
      ggplot(data,aes(x = Date, y = log(chl)))+geom_point()+
        geom_line(data=nestPred,aes_string(x="date",y = input$plot6choice1, color = "as.character(input$plot6choice1)"),lwd=1)+
        geom_line(data=nestPred,aes_string(x="date",y = input$plot6choice2, color = "as.character(input$plot6choice2)"),lwd=1)+
        scale_colour_manual(name = '',
                            labels =c('red'=input$plot6choice1,"dodgerblue"=input$plot6choice2)
                            ,values=c("red", "dodgerblue")
        ) +
        ggtitle(paste(names(perStation)[index],"Component-Wise Predictions Full Model \n Intercept added in order to center components",sep=" "))+scale_x_date(limits = dt_rng)+
        ylab("ln(chl a) ")+xlab("Date")+ylim(input$ylim34L,input$ylim34U)
    }else if(index==13){
      df <- data.frame()
      ggplot(df) + geom_point() +  scale_x_date(limits = dt_rng)+ggtitle("no data for extra variables in full model")
    }else if(index %in% c(17,18,21,22,23)){
      toUse=na.omit(data[,c("doy","date_dec","pheo","tn","do_per",
                            "sio2","tp","tss","nh4","sal","Date")])
      
      terms<-c("ti(pheo)","ti(doy)","ti(date_dec)","ti(tn)","ti(do_per)",
               "ti(sio2)","ti(tp)","ti(tss)","ti(nh4)","ti(sal)",
               "intercept")
      toName=c("doy","date_dec","pheo","tn","do_per","sio2","tp","tss","nh4","sal","date","intercept")
      byTerm=predict(mod,toUse,type="terms")
      
      byTerm=apply(byTerm,2,function(x){x+summary(mod)$p.coeff})
      nestPred=as.data.frame(cbind.data.frame(byTerm,toUse$Date,rep(summary(mod)$p.coeff,nrow(toUse))))
      names(nestPred)=toName
      
      ggplot(data,aes(x = Date, y = log(chl)))+geom_point()+
        geom_line(data=nestPred,aes_string(x="date",y = input$plot6choice1, color = "as.character(input$plot6choice1)"),lwd=1)+
        geom_line(data=nestPred,aes_string(x="date",y = input$plot6choice2, color = "as.character(input$plot6choice2)"),lwd=1)+
        scale_colour_manual(name = '',
                            labels =c('red'=input$plot6choice1,"dodgerblue"=input$plot6choice2)
                            ,values=c("red", "dodgerblue")
        ) +
        ggtitle(paste(names(perStation)[index],"Component-Wise Predictions Full Model \n Intercept added in order to center components",sep=" "))+scale_x_date(limits = dt_rng)+
        ylab("ln(chl a) ")+xlab("Date")+ylim(input$ylim34L,input$ylim34U)
    }else{
      toUse=na.omit(data[,c("doy","date_dec","pheo","tn","do_per",
                            "sio2","tp","tss","nh4","Date")])
      
      terms<-c("ti(pheo)","ti(doy)","ti(date_dec)","ti(tn)","ti(do_per)",
               "ti(sio2)","ti(tp)","ti(tss)","ti(nh4)",
               "intercept")
      toName=c("doy","date_dec","pheo","tn","do_per","sio2","tp","tss","nh4","date","intercept")
      byTerm=predict(mod,toUse,type="terms")

  
    byTerm=apply(byTerm,2,function(x){x+summary(mod)$p.coeff})
    nestPred=as.data.frame(cbind.data.frame(byTerm,toUse$Date,rep(summary(mod)$p.coeff,nrow(toUse))))
    names(nestPred)=toName
    
    ggplot(data,aes(x = Date, y = log(chl)))+geom_point()+
      geom_line(data=nestPred,aes_string(x="date",y = input$plot6choice1, color = "as.character(input$plot6choice1)"),lwd=1)+
      geom_line(data=nestPred,aes_string(x="date",y = input$plot6choice2, color = "as.character(input$plot6choice2)"),lwd=1)+
      scale_colour_manual(name = '',
                          labels =c('red'=input$plot6choice1,"dodgerblue"=input$plot6choice2)
                          ,values=c("red", "dodgerblue")
      ) +
      ggtitle(paste(names(perStation)[index],"Component-Wise Predictions Full Model \n Intercept added in order to center components",sep=" "))+scale_x_date(limits = dt_rng)+
      ylab("ln(chl a) ")+xlab("Date")+ylim(input$ylim34L,input$ylim34U)
    }
    
    
  }, height = 250, width = 1200)
  
  output$mapPlot <- renderPlot({
    wholeSeries<-c(1, 2, 5, 7, 11, 13, 15, 16, 17, 18, 21, 22, 23, 29, 40)
    
    allData<- do.call("rbind", perStation[wholeSeries])
    
plot(full$Longitude,full$Latitude,pch=19,main="Location of Station",xlab="longitude",ylab="latitude")
    points(dat()$Longitude,dat()$Latitude,col="red",pch=19)
   # plot(allData$Longitude,allData$Latitude,pch=19,main="Location of Station",xlab="longitude",ylab="latitude")
    #points(dat()$Longitude,dat()$Latitude,col="red",pch=19)
  },height=300,width=300)
  
  
  output$nestedPlotInt <- renderPlot({
    
    # inputs
    
    dt_rng <- input$dt_rng
    stat <- input$stat
    index=which(names(perStation)==stat)
    
    # data
    data<-dat()
    mod<-perStationIntMod[[index]]
    toUse=na.omit(data[,c("doy","date_dec","Date")])
    byTerm=predict(mod,toUse,type="terms")
    toName=c("doy","date_dec","interaction","date","intercept")
    nestPred=as.data.frame(cbind.data.frame(byTerm,toUse$Date,rep(summary(mod)$p.coeff,nrow(toUse))))
    names(nestPred)=toName
   terms<-c("ti(doy)","ti(date_dec)","ti(doy,date_dec)","intercept")
      ggplot(data,aes(x = Date, y = log(chl)))+geom_point()+
        geom_line(data=nestPred,aes(x=date,y = doy, color = 'ti(doy)'), lwd=1)+
        geom_line(data=nestPred,aes(x=date,y=date_dec, color = 'ti(date_dec)'), lwd=1)+
        geom_line(data=nestPred,aes(x=date,y = interaction, color = 'ti(doy,date_dec)'), lwd=1)+
        geom_line(data=nestPred,aes(x=date,y = intercept, color = 'intercept'), lwd=1)+
        scale_colour_manual(name = '',
                            labels =c('red'=terms[1],"dodgerblue"=terms[2],
                                      "forestgreen"=terms[3],"purple"=terms[4]),values=c("red",
                                                                                  "dodgerblue","forestgreen","purple")
        ) +
        ggtitle(paste(names(perStation)[index],"Component-Wise Predictions Interaction Model",sep=" "))+scale_x_date(limits = dt_rng)+
        ylab("ln(chl a) ")+xlab("Date")+ylim(input$ylim34L,input$ylim34U)
    
    
  }, height = 250, width = 1200)
  
  output$nestedPlotIntJust2 <- renderPlot({
    
    # inputs
    
    dt_rng <- input$dt_rng
    stat <- input$stat
    index=which(names(perStation)==stat)
    
    # data
    data<-dat()
    mod<-perStationIntMod[[index]]
    toUse=na.omit(data[,c("doy","date_dec","Date")])
    toName=c("doy","date_dec","interaction","date","intercept")
    terms<-c("ti(doy)","ti(date_dec)","ti(doy,date_dec)","intercept")
    
    byTerm=predict(mod,toUse,type="terms")
    byTerm=apply(byTerm,2,function(x){x+summary(mod)$p.coeff})
    nestPred=as.data.frame(cbind.data.frame(byTerm,toUse$Date,rep(summary(mod)$p.coeff,nrow(toUse))))
    names(nestPred)=toName
    
    ggplot(data,aes(x = Date, y = log(chl)))+geom_point()+
      geom_line(data=nestPred,aes_string(x="date",y = input$intVar1, color = "as.character(input$intVar1)"),lwd=1)+
      geom_line(data=nestPred,aes_string(x="date",y = input$intVar2, color = "as.character(input$intVar2)"),lwd=1)+
      scale_colour_manual(name = '',
                          labels =c('red'=input$intVar1,"dodgerblue"=input$intVar2)
                          ,values=c("red", "dodgerblue")
      ) +
      ggtitle(paste(names(perStation)[index],"Component-Wise Predictions Interaction Model \n Intercept added in order to center components",sep=" "))+scale_x_date(limits = dt_rng)+
      ylab("ln(chl a) ")+xlab("Date")+ylim(input$ylim34L,input$ylim34U)
    
  }, height = 250, width = 1200)
  
  output$nestedPlotSpat <- renderPlot({
    
    # inputs
    
    dt_rng <- input$dt_rng
    stat <- input$stat
    index=which(names(perStation)==stat)
    
    data<-subset(allData,Station==stat)
    indices<-which(allData$Station==stat)
    data$Date=as.Date(data$Date)
    
    if(input$spatMod=="spatIntercept"){
      ## maybe just boost everything by the intercept so that it lines up, no need to do two plot?
      toPlot=as.data.frame(cbind.data.frame(predict(mod1,data,type="terms"),data$Date))
      names(toPlot)=c("station","doy","date_dec","date")
      if(stat=="C10"){
      toPlot$station=toPlot$station+mod1$coefficients[1]
      toPlot$doy=toPlot$doy+mod1$coefficients[1]
      toPlot$date_dec=toPlot$date_dec+mod1$coefficients[1]
      }else{
       findS= unlist(lapply(names(mod1$coefficients),function(x){y<-strsplit(x,"Station)");unlist(y)[2]}))
       indexS=which(findS==stat) 
       toPlot$station=toPlot$station+mod1$coefficients[1]+mod1$coefficients[indexS]
        toPlot$doy=toPlot$doy+mod1$coefficients[1]+mod1$coefficients[indexS]
        toPlot$date_dec=toPlot$date_dec+mod1$coefficients[1]+mod1$coefficients[indexS]
      }
      ggplot(data,aes(x = Date, y = log(chl)))+geom_point()+
        geom_line(data=toPlot,aes(x=date,y = station, color = 'station'), lwd=1)+
        geom_line(data=toPlot,aes(x=date,y = doy, color = 'ti(doy)'), lwd=1)+
        geom_line(data=toPlot,aes(x=date,y=date_dec, color = 'ti(date_dec)'), lwd=1)+
        
        scale_colour_manual(name = '',
                            labels =c('red'="station","dodgerblue"="ti(doy)",
                                      "forestgreen"="ti(date_dec)"),values=c("red","dodgerblue","forestgreen")
        ) +
        ggtitle(paste(stat,"Component-Wise Predictions",input$spatMod ,"Model \n Intercept added to each piece to center. \n Intercept =",round(toPlot$station[1],2),sep=" "))+scale_x_date(limits = dt_rng)+
        ylab("ln(chl a) ")+xlab("Date")+ylim(input$ylim34L,input$ylim34U)
      
    }else if(input$spatMod=="spatDate_Dec"){
   
      
      toPlot=as.data.frame(cbind.data.frame(predict(mod2,data,type="terms"),data$Date))
     findS=unlist(lapply( names(toPlot)[-c(1,2,ncol(toPlot))], function(x){y<-strsplit(x,"Station)");unlist(y)[2]}))
      index=which(findS==stat)+2
      toPlot=toPlot[,c(1,2,index,ncol(toPlot))]
      
      names(toPlot)=c("station","doy","date_decStation","date")
      
      if(stat=="C10"){
      toPlot$station=toPlot$station+mod2$coefficients[1]
      toPlot$doy=toPlot$doy+mod2$coefficients[1]
      toPlot$date_decStation=toPlot$date_decStation+mod2$coefficients[1]
      }else{
        findS= unlist(lapply(names(mod2$coefficients),function(x){y<-strsplit(x,"Station)");unlist(y)[2]}))
        indexS=which(findS==stat) 
        toPlot$station=toPlot$station+mod2$coefficients[1]+mod2$coefficients[indexS]
        toPlot$doy=toPlot$doy+mod2$coefficients[1]+mod2$coefficients[indexS]
        toPlot$date_decStation=toPlot$date_decStation+mod2$coefficients[1]+mod2$coefficients[indexS]
      }
      ggplot(data,aes(x = Date, y = log(chl)))+geom_point()+
        geom_line(data=toPlot,aes(x=date,y = station, color = 'station'), lwd=1)+
        geom_line(data=toPlot,aes(x=date,y = doy, color = 'ti(doy)'), lwd=1)+
        geom_line(data=toPlot,aes(x=date,y=date_decStation, color = 'ti(date_dec,Station)'), lwd=1)+
        
        scale_colour_manual(name = '',
                            labels =c('red'="station","dodgerblue"="ti(doy)",
                                      "forestgreen"="ti(date_dec,Station)"),values=c("red","dodgerblue","forestgreen")
        ) +
        ggtitle(paste(stat,"Component-Wise Predictions",input$spatMod ,"Model \n Intercept added to each piece to center. \n Intercept =",round(toPlot$station[1],2),sep=" "))+scale_x_date(limits = dt_rng)+
        ylab("ln(chl a) ")+xlab("Date")+ylim(input$ylim34L,input$ylim34U)
      
    }else if(input$spatMod=="spatDOY"){
      toPlot=as.data.frame(cbind.data.frame(predict(mod3,data,type="terms"),data$Date))
      findS=unlist(lapply( names(toPlot)[-c(1,ncol(toPlot))], function(x){y<-strsplit(x,"Station)");unlist(y)[2]}))
      index=which(findS==stat)+1
      toPlot=toPlot[,c(1,index,ncol(toPlot))]
      
      names(toPlot)=c("station","doy_Station","date_decStation","date")
      
      if(stat=="C10"){
      toPlot$station=toPlot$station+mod3$coefficients[1]
      toPlot$doy_Station=toPlot$doy_Station+mod3$coefficients[1]
      toPlot$date_decStation=toPlot$date_decStation+mod3$coefficients[1]
      }else{
        findS= unlist(lapply(names(mod3$coefficients),function(x){y<-strsplit(x,"Station)");unlist(y)[2]}))
        indexS=which(findS==stat) 
        toPlot$station=toPlot$station+mod3$coefficients[1]+mod3$coefficients[indexS]
        toPlot$doy_Station=toPlot$doy_Station+mod3$coefficients[1]+mod3$coefficients[indexS]
        toPlot$date_decStation=toPlot$date_decStation+mod3$coefficients[1]+mod3$coefficients[indexS]
      }
      ggplot(data,aes(x = Date, y = log(chl)))+geom_point()+
        geom_line(data=toPlot,aes(x=date,y = station, color = 'station'), lwd=1)+
        geom_line(data=toPlot,aes(x=date,y = doy_Station, color = 'ti(doy_Station)'), lwd=1)+
        geom_line(data=toPlot,aes(x=date,y=date_decStation, color = 'ti(date_dec,Station)'), lwd=1)+
        
        scale_colour_manual(name = '',
                            labels =c('red'="station","dodgerblue"="ti(doy_Station)",
                                      "forestgreen"="ti(date_dec,Station)"),values=c("red","dodgerblue","forestgreen")
        ) +
        ggtitle(paste(stat,"Component-Wise Predictions",input$spatMod ,"Model \n Intercept added to each piece to center. \n Intercept =",round(toPlot$station[1],2),sep=" "))+scale_x_date(limits = dt_rng)+
        ylab("ln(chl a) ")+xlab("Date")+ylim(input$ylim34L,input$ylim34U)
      
    }else if(input$spatMod=="spatinteraction"){
      toPlot=as.data.frame(cbind.data.frame(predict(mod4,data,type="terms"),data$Date))
      findS=unlist(lapply( names(toPlot)[-c(1,2,ncol(toPlot))], function(x){y<-strsplit(x,"Station)");unlist(y)[2]}))
      index=which(findS==stat)+2
      toPlot=toPlot[,c(1,2,index,ncol(toPlot))]
      
      names(toPlot)=c("station","doy","date_decStation","interactionStation","date")
      
      if(stat=="C10"){
      toPlot$station=toPlot$station+mod4$coefficients[1]
      toPlot$doy=toPlot$doy+mod4$coefficients[1]
      toPlot$date_decStation=toPlot$date_decStation+mod4$coefficients[1]
      toPlot$interactionStation=toPlot$interactionStation+mod4$coefficients[1]
      }else{
        findS= unlist(lapply(names(mod4$coefficients),function(x){y<-strsplit(x,"Station)");unlist(y)[2]}))
        indexS=which(findS==stat) 
        toPlot$station=toPlot$station+mod4$coefficients[1]+mod4$coefficients[indexS]
        toPlot$doy=toPlot$doy+mod4$coefficients[1]+mod4$coefficients[indexS]
        toPlot$date_decStation=toPlot$date_decStation+mod4$coefficients[1]+mod4$coefficients[indexS]
        toPlot$interactionStation=toPlot$interactionStation+mod4$coefficients[1]+mod4$coefficients[indexS]
      }
      ggplot(data,aes(x = Date, y = log(chl)))+geom_point()+
        geom_line(data=toPlot,aes(x=date,y = station, color = 'station'), lwd=1)+
        geom_line(data=toPlot,aes(x=date,y = doy, color = 'ti(doy)'), lwd=1)+
        geom_line(data=toPlot,aes(x=date,y=date_decStation, color = 'ti(date_dec,Station)'), lwd=1)+
        geom_line(data=toPlot,aes(x=date,y=interactionStation, color = 'ti(date_dec,doy_Station)'), lwd=1)+
        scale_colour_manual(name = '',
                            labels =c('red'="station","dodgerblue"="ti(doy)",
                                      "forestgreen"="ti(date_dec,Station)",
                                      "purple"="ti(date_dec,doy_Station)"),values=c("red","dodgerblue",
                                                  "forestgreen","purple")
        ) +
        ggtitle(paste(stat,"Component-Wise Predictions",input$spatMod ,"Model  \n Intercept added to each piece to center. \n Intercept =",round(toPlot$station[1],2),sep=" "))+scale_x_date(limits = dt_rng)+
        ylab("ln(chl a) ")+xlab("Date")+ylim(input$ylim34L,input$ylim34U)
      
    }
    
  }, height = 250, width = 1200)
  
  output$nestedPlotSpat2 <- renderPlot({
    
    # inputs
    
    dt_rng <- input$dt_rng
    stat <- input$stat
    index=which(names(perStation)==stat)
    
    data<-subset(allData,Station==stat)
    indices<-which(allData$Station==stat)
    data$Date=as.Date(data$Date)
    
    if(input$spatMod2=="spatIntercept"){
      ## maybe just boost everything by the intercept so that it lines up, no need to do two plot?
      toPlot=as.data.frame(cbind.data.frame(predict(mod1,data,type="terms"),data$Date))
      names(toPlot)=c("station","doy","date_dec","date")
      if(stat=="C10"){
        toPlot$station=toPlot$station+mod1$coefficients[1]
        toPlot$doy=toPlot$doy+mod1$coefficients[1]
        toPlot$date_dec=toPlot$date_dec+mod1$coefficients[1]
      }else{
        findS= unlist(lapply(names(mod1$coefficients),function(x){y<-strsplit(x,"Station)");unlist(y)[2]}))
        indexS=which(findS==stat) 
        toPlot$station=toPlot$station+mod1$coefficients[1]+mod1$coefficients[indexS]
        toPlot$doy=toPlot$doy+mod1$coefficients[1]+mod1$coefficients[indexS]
        toPlot$date_dec=toPlot$date_dec+mod1$coefficients[1]+mod1$coefficients[indexS]
      }
      ggplot(data,aes(x = Date, y = log(chl)))+geom_point()+
        geom_line(data=toPlot,aes(x=date,y = station, color = 'station'), lwd=1)+
        geom_line(data=toPlot,aes(x=date,y = doy, color = 'ti(doy)'), lwd=1)+
        geom_line(data=toPlot,aes(x=date,y=date_dec, color = 'ti(date_dec)'), lwd=1)+
        
        scale_colour_manual(name = '',
                            labels =c('red'="station","dodgerblue"="ti(doy)",
                                      "forestgreen"="ti(date_dec)"),values=c("red","dodgerblue","forestgreen")
        ) +
        ggtitle(paste(stat,"Component-Wise Predictions",input$spatMod2 ,"Model \n Intercept added to each piece to center. \n Intercept =",round(toPlot$station[1],2),sep=" "))+scale_x_date(limits = dt_rng)+
        ylab("ln(chl a) ")+xlab("Date")+ylim(input$ylim34L,input$ylim34U)
      
    }else if(input$spatMod2=="spatDate_Dec"){
      
      
      toPlot=as.data.frame(cbind.data.frame(predict(mod2,data,type="terms"),data$Date))
      findS=unlist(lapply( names(toPlot)[-c(1,2,ncol(toPlot))], function(x){y<-strsplit(x,"Station)");unlist(y)[2]}))
      index=which(findS==stat)+2
      toPlot=toPlot[,c(1,2,index,ncol(toPlot))]
      
      names(toPlot)=c("station","doy","date_decStation","date")
      
      if(stat=="C10"){
        toPlot$station=toPlot$station+mod2$coefficients[1]
        toPlot$doy=toPlot$doy+mod2$coefficients[1]
        toPlot$date_decStation=toPlot$date_decStation+mod2$coefficients[1]
      }else{
        findS= unlist(lapply(names(mod2$coefficients),function(x){y<-strsplit(x,"Station)");unlist(y)[2]}))
        indexS=which(findS==stat) 
        toPlot$station=toPlot$station+mod2$coefficients[1]+mod2$coefficients[indexS]
        toPlot$doy=toPlot$doy+mod2$coefficients[1]+mod2$coefficients[indexS]
        toPlot$date_decStation=toPlot$date_decStation+mod2$coefficients[1]+mod2$coefficients[indexS]
      }
      ggplot(data,aes(x = Date, y = log(chl)))+geom_point()+
        geom_line(data=toPlot,aes(x=date,y = station, color = 'station'), lwd=1)+
        geom_line(data=toPlot,aes(x=date,y = doy, color = 'ti(doy)'), lwd=1)+
        geom_line(data=toPlot,aes(x=date,y=date_decStation, color = 'ti(date_dec,Station)'), lwd=1)+
        
        scale_colour_manual(name = '',
                            labels =c('red'="station","dodgerblue"="ti(doy)",
                                      "forestgreen"="ti(date_dec,Station)"),values=c("red","dodgerblue","forestgreen")
        ) +
        ggtitle(paste(stat,"Component-Wise Predictions",input$spatMod2 ,"Model \n Intercept added to each piece to center. \n Intercept =",round(toPlot$station[1],2),sep=" "))+scale_x_date(limits = dt_rng)+
        ylab("ln(chl a) ")+xlab("Date")+ylim(input$ylim34L,input$ylim34U)
      
    }else if(input$spatMod2=="spatDOY"){
      toPlot=as.data.frame(cbind.data.frame(predict(mod3,data,type="terms"),data$Date))
      findS=unlist(lapply( names(toPlot)[-c(1,ncol(toPlot))], function(x){y<-strsplit(x,"Station)");unlist(y)[2]}))
      index=which(findS==stat)+1
      toPlot=toPlot[,c(1,index,ncol(toPlot))]
      
      names(toPlot)=c("station","doy_Station","date_decStation","date")
      
      if(stat=="C10"){
        toPlot$station=toPlot$station+mod3$coefficients[1]
        toPlot$doy_Station=toPlot$doy_Station+mod3$coefficients[1]
        toPlot$date_decStation=toPlot$date_decStation+mod3$coefficients[1]
      }else{
        findS= unlist(lapply(names(mod3$coefficients),function(x){y<-strsplit(x,"Station)");unlist(y)[2]}))
        indexS=which(findS==stat) 
        toPlot$station=toPlot$station+mod3$coefficients[1]+mod3$coefficients[indexS]
        toPlot$doy_Station=toPlot$doy_Station+mod3$coefficients[1]+mod3$coefficients[indexS]
        toPlot$date_decStation=toPlot$date_decStation+mod3$coefficients[1]+mod3$coefficients[indexS]
      }
      ggplot(data,aes(x = Date, y = log(chl)))+geom_point()+
        geom_line(data=toPlot,aes(x=date,y = station, color = 'station'), lwd=1)+
        geom_line(data=toPlot,aes(x=date,y = doy_Station, color = 'ti(doy_Station)'), lwd=1)+
        geom_line(data=toPlot,aes(x=date,y=date_decStation, color = 'ti(date_dec,Station)'), lwd=1)+
        
        scale_colour_manual(name = '',
                            labels =c('red'="station","dodgerblue"="ti(doy_Station)",
                                      "forestgreen"="ti(date_dec,Station)"),values=c("red","dodgerblue","forestgreen")
        ) +
        ggtitle(paste(stat,"Component-Wise Predictions",input$spatMod2 ,"Model \n Intercept added to each piece to center. \n Intercept =",round(toPlot$station[1],2),sep=" "))+scale_x_date(limits = dt_rng)+
        ylab("ln(chl a) ")+xlab("Date")+ylim(input$ylim34L,input$ylim34U)
      
    }else if(input$spatMod2=="spatinteraction"){
      toPlot=as.data.frame(cbind.data.frame(predict(mod4,data,type="terms"),data$Date))
      findS=unlist(lapply( names(toPlot)[-c(1,2,ncol(toPlot))], function(x){y<-strsplit(x,"Station)");unlist(y)[2]}))
      index=which(findS==stat)+2
      toPlot=toPlot[,c(1,2,index,ncol(toPlot))]
      
      names(toPlot)=c("station","doy","date_decStation","interactionStation","date")
      
      if(stat=="C10"){
        toPlot$station=toPlot$station+mod4$coefficients[1]
        toPlot$doy=toPlot$doy+mod4$coefficients[1]
        toPlot$date_decStation=toPlot$date_decStation+mod4$coefficients[1]
        toPlot$interactionStation=toPlot$interactionStation+mod4$coefficients[1]
      }else{
        findS= unlist(lapply(names(mod4$coefficients),function(x){y<-strsplit(x,"Station)");unlist(y)[2]}))
        indexS=which(findS==stat) 
        toPlot$station=toPlot$station+mod4$coefficients[1]+mod4$coefficients[indexS]
        toPlot$doy=toPlot$doy+mod4$coefficients[1]+mod4$coefficients[indexS]
        toPlot$date_decStation=toPlot$date_decStation+mod4$coefficients[1]+mod4$coefficients[indexS]
        toPlot$interactionStation=toPlot$interactionStation+mod4$coefficients[1]+mod4$coefficients[indexS]
      }
      ggplot(data,aes(x = Date, y = log(chl)))+geom_point()+
        geom_line(data=toPlot,aes(x=date,y = station, color = 'station'), lwd=1)+
        geom_line(data=toPlot,aes(x=date,y = doy, color = 'ti(doy)'), lwd=1)+
        geom_line(data=toPlot,aes(x=date,y=date_decStation, color = 'ti(date_dec,Station)'), lwd=1)+
        geom_line(data=toPlot,aes(x=date,y=interactionStation, color = 'ti(date_dec,doy_Station)'), lwd=1)+
        scale_colour_manual(name = '',
                            labels =c('red'="station","dodgerblue"="ti(doy)",
                                      "forestgreen"="ti(date_dec,Station)",
                                      "purple"="ti(date_dec,doy_Station)"),values=c("red","dodgerblue",
                                                                                    "forestgreen","purple")
        ) +
        ggtitle(paste(stat,"Component-Wise Predictions",input$spatMod2 ,"Model  \n Intercept added to each piece to center. \n Intercept =",round(toPlot$station[1],2),sep=" "))+scale_x_date(limits = dt_rng)+
        ylab("ln(chl a) ")+xlab("Date")+ylim(input$ylim34L,input$ylim34U)
      
    }
    
  }, height = 250, width = 1200)
  
  output$nestedPlotSpat3 <- renderPlot({
    
    # inputs
    
    dt_rng <- input$dt_rng
    stat <- input$stat
    index=which(names(perStation)==stat)
    
    data<-subset(allData,Station==stat)
    indices<-which(allData$Station==stat)
    data$Date=as.Date(data$Date)
    
    if(input$spatMod3=="spatIntercept"){
      ## maybe just boost everything by the intercept so that it lines up, no need to do two plot?
      toPlot=as.data.frame(cbind.data.frame(predict(mod1,data,type="terms"),data$Date))
      names(toPlot)=c("station","doy","date_dec","date")
      if(stat=="C10"){
        toPlot$station=toPlot$station+mod1$coefficients[1]
        toPlot$doy=toPlot$doy+mod1$coefficients[1]
        toPlot$date_dec=toPlot$date_dec+mod1$coefficients[1]
      }else{
        findS= unlist(lapply(names(mod1$coefficients),function(x){y<-strsplit(x,"Station)");unlist(y)[2]}))
        indexS=which(findS==stat) 
        toPlot$station=toPlot$station+mod1$coefficients[1]+mod1$coefficients[indexS]
        toPlot$doy=toPlot$doy+mod1$coefficients[1]+mod1$coefficients[indexS]
        toPlot$date_dec=toPlot$date_dec+mod1$coefficients[1]+mod1$coefficients[indexS]
      }
      ggplot(data,aes(x = Date, y = log(chl)))+geom_point()+
        geom_line(data=toPlot,aes(x=date,y = station, color = 'station'), lwd=1)+
        geom_line(data=toPlot,aes(x=date,y = doy, color = 'ti(doy)'), lwd=1)+
        geom_line(data=toPlot,aes(x=date,y=date_dec, color = 'ti(date_dec)'), lwd=1)+
        
        scale_colour_manual(name = '',
                            labels =c('red'="station","dodgerblue"="ti(doy)",
                                      "forestgreen"="ti(date_dec)"),values=c("red","dodgerblue","forestgreen")
        ) +
        ggtitle(paste(stat,"Component-Wise Predictions",input$spatMod3 ,"Model \n Intercept added to each piece to center. \n Intercept =",round(toPlot$station[1],2),sep=" "))+scale_x_date(limits = dt_rng)+
        ylab("ln(chl a) ")+xlab("Date")+ylim(input$ylim34L,input$ylim34U)
      
    }else if(input$spatMod3=="spatDate_Dec"){
      
      
      toPlot=as.data.frame(cbind.data.frame(predict(mod2,data,type="terms"),data$Date))
      findS=unlist(lapply( names(toPlot)[-c(1,2,ncol(toPlot))], function(x){y<-strsplit(x,"Station)");unlist(y)[2]}))
      index=which(findS==stat)+2
      toPlot=toPlot[,c(1,2,index,ncol(toPlot))]
      
      names(toPlot)=c("station","doy","date_decStation","date")
      
      if(stat=="C10"){
        toPlot$station=toPlot$station+mod2$coefficients[1]
        toPlot$doy=toPlot$doy+mod2$coefficients[1]
        toPlot$date_decStation=toPlot$date_decStation+mod2$coefficients[1]
      }else{
        findS= unlist(lapply(names(mod2$coefficients),function(x){y<-strsplit(x,"Station)");unlist(y)[2]}))
        indexS=which(findS==stat) 
        toPlot$station=toPlot$station+mod2$coefficients[1]+mod2$coefficients[indexS]
        toPlot$doy=toPlot$doy+mod2$coefficients[1]+mod2$coefficients[indexS]
        toPlot$date_decStation=toPlot$date_decStation+mod2$coefficients[1]+mod2$coefficients[indexS]
      }
      ggplot(data,aes(x = Date, y = log(chl)))+geom_point()+
        geom_line(data=toPlot,aes(x=date,y = station, color = 'station'), lwd=1)+
        geom_line(data=toPlot,aes(x=date,y = doy, color = 'ti(doy)'), lwd=1)+
        geom_line(data=toPlot,aes(x=date,y=date_decStation, color = 'ti(date_dec,Station)'), lwd=1)+
        
        scale_colour_manual(name = '',
                            labels =c('red'="station","dodgerblue"="ti(doy)",
                                      "forestgreen"="ti(date_dec,Station)"),values=c("red","dodgerblue","forestgreen")
        ) +
        ggtitle(paste(stat,"Component-Wise Predictions",input$spatMod3 ,"Model \n Intercept added to each piece to center. \n Intercept =",round(toPlot$station[1],2),sep=" "))+scale_x_date(limits = dt_rng)+
        ylab("ln(chl a) ")+xlab("Date")+ylim(input$ylim34L,input$ylim34U)
      
    }else if(input$spatMod3=="spatDOY"){
      toPlot=as.data.frame(cbind.data.frame(predict(mod3,data,type="terms"),data$Date))
      findS=unlist(lapply( names(toPlot)[-c(1,ncol(toPlot))], function(x){y<-strsplit(x,"Station)");unlist(y)[2]}))
      index=which(findS==stat)+1
      toPlot=toPlot[,c(1,index,ncol(toPlot))]
      
      names(toPlot)=c("station","doy_Station","date_decStation","date")
      
      if(stat=="C10"){
        toPlot$station=toPlot$station+mod3$coefficients[1]
        toPlot$doy_Station=toPlot$doy_Station+mod3$coefficients[1]
        toPlot$date_decStation=toPlot$date_decStation+mod3$coefficients[1]
      }else{
        findS= unlist(lapply(names(mod3$coefficients),function(x){y<-strsplit(x,"Station)");unlist(y)[2]}))
        indexS=which(findS==stat) 
        toPlot$station=toPlot$station+mod3$coefficients[1]+mod3$coefficients[indexS]
        toPlot$doy_Station=toPlot$doy_Station+mod3$coefficients[1]+mod3$coefficients[indexS]
        toPlot$date_decStation=toPlot$date_decStation+mod3$coefficients[1]+mod3$coefficients[indexS]
      }
      ggplot(data,aes(x = Date, y = log(chl)))+geom_point()+
        geom_line(data=toPlot,aes(x=date,y = station, color = 'station'), lwd=1)+
        geom_line(data=toPlot,aes(x=date,y = doy_Station, color = 'ti(doy_Station)'), lwd=1)+
        geom_line(data=toPlot,aes(x=date,y=date_decStation, color = 'ti(date_dec,Station)'), lwd=1)+
        
        scale_colour_manual(name = '',
                            labels =c('red'="station","dodgerblue"="ti(doy_Station)",
                                      "forestgreen"="ti(date_dec,Station)"),values=c("red","dodgerblue","forestgreen")
        ) +
        ggtitle(paste(stat,"Component-Wise Predictions",input$spatMod3 ,"Model \n Intercept added to each piece to center. \n Intercept =",round(toPlot$station[1],2),sep=" "))+scale_x_date(limits = dt_rng)+
        ylab("ln(chl a) ")+xlab("Date")+ylim(input$ylim34L,input$ylim34U)
      
    }else if(input$spatMod3=="spatinteraction"){
      toPlot=as.data.frame(cbind.data.frame(predict(mod4,data,type="terms"),data$Date))
      findS=unlist(lapply( names(toPlot)[-c(1,2,ncol(toPlot))], function(x){y<-strsplit(x,"Station)");unlist(y)[2]}))
      index=which(findS==stat)+2
      toPlot=toPlot[,c(1,2,index,ncol(toPlot))]
      
      names(toPlot)=c("station","doy","date_decStation","interactionStation","date")
      
      if(stat=="C10"){
        toPlot$station=toPlot$station+mod4$coefficients[1]
        toPlot$doy=toPlot$doy+mod4$coefficients[1]
        toPlot$date_decStation=toPlot$date_decStation+mod4$coefficients[1]
        toPlot$interactionStation=toPlot$interactionStation+mod4$coefficients[1]
      }else{
        findS= unlist(lapply(names(mod4$coefficients),function(x){y<-strsplit(x,"Station)");unlist(y)[2]}))
        indexS=which(findS==stat) 
        toPlot$station=toPlot$station+mod4$coefficients[1]+mod4$coefficients[indexS]
        toPlot$doy=toPlot$doy+mod4$coefficients[1]+mod4$coefficients[indexS]
        toPlot$date_decStation=toPlot$date_decStation+mod4$coefficients[1]+mod4$coefficients[indexS]
        toPlot$interactionStation=toPlot$interactionStation+mod4$coefficients[1]+mod4$coefficients[indexS]
      }
      ggplot(data,aes(x = Date, y = log(chl)))+geom_point()+
        geom_line(data=toPlot,aes(x=date,y = station, color = 'station'), lwd=1)+
        geom_line(data=toPlot,aes(x=date,y = doy, color = 'ti(doy)'), lwd=1)+
        geom_line(data=toPlot,aes(x=date,y=date_decStation, color = 'ti(date_dec,Station)'), lwd=1)+
        geom_line(data=toPlot,aes(x=date,y=interactionStation, color = 'ti(date_dec,doy_Station)'), lwd=1)+
        scale_colour_manual(name = '',
                            labels =c('red'="station","dodgerblue"="ti(doy)",
                                      "forestgreen"="ti(date_dec,Station)",
                                      "purple"="ti(date_dec,doy_Station)"),values=c("red","dodgerblue",
                                                                                    "forestgreen","purple")
        ) +
        ggtitle(paste(stat,"Component-Wise Predictions",input$spatMod3 ,"Model  \n Intercept added to each piece to center. \n Intercept =",round(toPlot$station[1],2),sep=" "))+scale_x_date(limits = dt_rng)+
        ylab("ln(chl a) ")+xlab("Date")+ylim(input$ylim34L,input$ylim34U)
      
    }
    
  }, height = 250, width = 1200)
  
  
  output$nestedPlotSpat4 <- renderPlot({
    
    # inputs
    
    dt_rng <- input$dt_rng
    stat <- input$stat
    index=which(names(perStation)==stat)
    
    data<-subset(allData,Station==stat)
    indices<-which(allData$Station==stat)
    data$Date=as.Date(data$Date)
    
    if(input$spatMod4=="spatIntercept"){
      ## maybe just boost everything by the intercept so that it lines up, no need to do two plot?
      toPlot=as.data.frame(cbind.data.frame(predict(mod1,data,type="terms"),data$Date))
      names(toPlot)=c("station","doy","date_dec","date")
      if(stat=="C10"){
        toPlot$station=toPlot$station+mod1$coefficients[1]
        toPlot$doy=toPlot$doy+mod1$coefficients[1]
        toPlot$date_dec=toPlot$date_dec+mod1$coefficients[1]
      }else{
        findS= unlist(lapply(names(mod1$coefficients),function(x){y<-strsplit(x,"Station)");unlist(y)[2]}))
        indexS=which(findS==stat) 
        toPlot$station=toPlot$station+mod1$coefficients[1]+mod1$coefficients[indexS]
        toPlot$doy=toPlot$doy+mod1$coefficients[1]+mod1$coefficients[indexS]
        toPlot$date_dec=toPlot$date_dec+mod1$coefficients[1]+mod1$coefficients[indexS]
      }
      ggplot(data,aes(x = Date, y = log(chl)))+geom_point()+
        geom_line(data=toPlot,aes(x=date,y = station, color = 'station'), lwd=1)+
        geom_line(data=toPlot,aes(x=date,y = doy, color = 'ti(doy)'), lwd=1)+
        geom_line(data=toPlot,aes(x=date,y=date_dec, color = 'ti(date_dec)'), lwd=1)+
        
        scale_colour_manual(name = '',
                            labels =c('red'="station","dodgerblue"="ti(doy)",
                                      "forestgreen"="ti(date_dec)"),values=c("red","dodgerblue","forestgreen")
        ) +
        ggtitle(paste(stat,"Component-Wise Predictions",input$spatMod4 ,"Model \n Intercept added to each piece to center. \n Intercept =",round(toPlot$station[1],2),sep=" "))+scale_x_date(limits = dt_rng)+
        ylab("ln(chl a) ")+xlab("Date")+ylim(input$ylim34L,input$ylim34U)
      
    }else if(input$spatMod4=="spatDate_Dec"){
      
      
      toPlot=as.data.frame(cbind.data.frame(predict(mod2,data,type="terms"),data$Date))
      findS=unlist(lapply( names(toPlot)[-c(1,2,ncol(toPlot))], function(x){y<-strsplit(x,"Station)");unlist(y)[2]}))
      index=which(findS==stat)+2
      toPlot=toPlot[,c(1,2,index,ncol(toPlot))]
      
      names(toPlot)=c("station","doy","date_decStation","date")
      
      if(stat=="C10"){
        toPlot$station=toPlot$station+mod2$coefficients[1]
        toPlot$doy=toPlot$doy+mod2$coefficients[1]
        toPlot$date_decStation=toPlot$date_decStation+mod2$coefficients[1]
      }else{
        findS= unlist(lapply(names(mod2$coefficients),function(x){y<-strsplit(x,"Station)");unlist(y)[2]}))
        indexS=which(findS==stat) 
        toPlot$station=toPlot$station+mod2$coefficients[1]+mod2$coefficients[indexS]
        toPlot$doy=toPlot$doy+mod2$coefficients[1]+mod2$coefficients[indexS]
        toPlot$date_decStation=toPlot$date_decStation+mod2$coefficients[1]+mod2$coefficients[indexS]
      }
      ggplot(data,aes(x = Date, y = log(chl)))+geom_point()+
        geom_line(data=toPlot,aes(x=date,y = station, color = 'station'), lwd=1)+
        geom_line(data=toPlot,aes(x=date,y = doy, color = 'ti(doy)'), lwd=1)+
        geom_line(data=toPlot,aes(x=date,y=date_decStation, color = 'ti(date_dec,Station)'), lwd=1)+
        
        scale_colour_manual(name = '',
                            labels =c('red'="station","dodgerblue"="ti(doy)",
                                      "forestgreen"="ti(date_dec,Station)"),values=c("red","dodgerblue","forestgreen")
        ) +
        ggtitle(paste(stat,"Component-Wise Predictions",input$spatMod4 ,"Model \n Intercept added to each piece to center. \n Intercept =",round(toPlot$station[1],2),sep=" "))+scale_x_date(limits = dt_rng)+
        ylab("ln(chl a) ")+xlab("Date")+ylim(input$ylim34L,input$ylim34U)
      
    }else if(input$spatMod4=="spatDOY"){
      toPlot=as.data.frame(cbind.data.frame(predict(mod3,data,type="terms"),data$Date))
      findS=unlist(lapply( names(toPlot)[-c(1,ncol(toPlot))], function(x){y<-strsplit(x,"Station)");unlist(y)[2]}))
      index=which(findS==stat)+1
      toPlot=toPlot[,c(1,index,ncol(toPlot))]
      
      names(toPlot)=c("station","doy_Station","date_decStation","date")
      
      if(stat=="C10"){
        toPlot$station=toPlot$station+mod3$coefficients[1]
        toPlot$doy_Station=toPlot$doy_Station+mod3$coefficients[1]
        toPlot$date_decStation=toPlot$date_decStation+mod3$coefficients[1]
      }else{
        findS= unlist(lapply(names(mod3$coefficients),function(x){y<-strsplit(x,"Station)");unlist(y)[2]}))
        indexS=which(findS==stat) 
        toPlot$station=toPlot$station+mod3$coefficients[1]+mod3$coefficients[indexS]
        toPlot$doy_Station=toPlot$doy_Station+mod3$coefficients[1]+mod3$coefficients[indexS]
        toPlot$date_decStation=toPlot$date_decStation+mod3$coefficients[1]+mod3$coefficients[indexS]
      }
      ggplot(data,aes(x = Date, y = log(chl)))+geom_point()+
        geom_line(data=toPlot,aes(x=date,y = station, color = 'station'), lwd=1)+
        geom_line(data=toPlot,aes(x=date,y = doy_Station, color = 'ti(doy_Station)'), lwd=1)+
        geom_line(data=toPlot,aes(x=date,y=date_decStation, color = 'ti(date_dec,Station)'), lwd=1)+
        
        scale_colour_manual(name = '',
                            labels =c('red'="station","dodgerblue"="ti(doy_Station)",
                                      "forestgreen"="ti(date_dec,Station)"),values=c("red","dodgerblue","forestgreen")
        ) +
        ggtitle(paste(stat,"Component-Wise Predictions",input$spatMod4 ,"Model \n Intercept added to each piece to center. \n Intercept =",round(toPlot$station[1],2),sep=" "))+scale_x_date(limits = dt_rng)+
        ylab("ln(chl a) ")+xlab("Date")+ylim(input$ylim34L,input$ylim34U)
      
    }else if(input$spatMod4=="spatinteraction"){
      toPlot=as.data.frame(cbind.data.frame(predict(mod4,data,type="terms"),data$Date))
      findS=unlist(lapply( names(toPlot)[-c(1,2,ncol(toPlot))], function(x){y<-strsplit(x,"Station)");unlist(y)[2]}))
      index=which(findS==stat)+2
      toPlot=toPlot[,c(1,2,index,ncol(toPlot))]
      
      names(toPlot)=c("station","doy","date_decStation","interactionStation","date")
      
      if(stat=="C10"){
        toPlot$station=toPlot$station+mod4$coefficients[1]
        toPlot$doy=toPlot$doy+mod4$coefficients[1]
        toPlot$date_decStation=toPlot$date_decStation+mod4$coefficients[1]
        toPlot$interactionStation=toPlot$interactionStation+mod4$coefficients[1]
      }else{
        findS= unlist(lapply(names(mod4$coefficients),function(x){y<-strsplit(x,"Station)");unlist(y)[2]}))
        indexS=which(findS==stat) 
        toPlot$station=toPlot$station+mod4$coefficients[1]+mod4$coefficients[indexS]
        toPlot$doy=toPlot$doy+mod4$coefficients[1]+mod4$coefficients[indexS]
        toPlot$date_decStation=toPlot$date_decStation+mod4$coefficients[1]+mod4$coefficients[indexS]
        toPlot$interactionStation=toPlot$interactionStation+mod4$coefficients[1]+mod4$coefficients[indexS]
      }
      ggplot(data,aes(x = Date, y = log(chl)))+geom_point()+
        geom_line(data=toPlot,aes(x=date,y = station, color = 'station'), lwd=1)+
        geom_line(data=toPlot,aes(x=date,y = doy, color = 'ti(doy)'), lwd=1)+
        geom_line(data=toPlot,aes(x=date,y=date_decStation, color = 'ti(date_dec,Station)'), lwd=1)+
        geom_line(data=toPlot,aes(x=date,y=interactionStation, color = 'ti(date_dec,doy_Station)'), lwd=1)+
        scale_colour_manual(name = '',
                            labels =c('red'="station","dodgerblue"="ti(doy)",
                                      "forestgreen"="ti(date_dec,Station)",
                                      "purple"="ti(date_dec,doy_Station)"),values=c("red","dodgerblue",
                                                                                    "forestgreen","purple")
        ) +
        ggtitle(paste(stat,"Component-Wise Predictions",input$spatMod4 ,"Model  \n Intercept added to each piece to center. \n Intercept =",round(toPlot$station[1],2),sep=" "))+scale_x_date(limits = dt_rng)+
        ylab("ln(chl a) ")+xlab("Date")+ylim(input$ylim34L,input$ylim34U)
      
    }
    
  }, height = 250, width = 1200)
  
  
  output$flowInput <- renderPlot({
    
    stat <- input$stat
    
    if(stat %in% c("D6","D7","D8","D10","D4","D12","D22","D26","D28A",
                   "MD10","P8")){
     
      
      if(stat=="D28A"){
        
        aggdata <-aggregate(volFlow[,grepl(stat,names(volFlow))], by=list(volFlow$year), 
                            FUN=mean, na.rm=TRUE)
        gY<-makeCompChart(t(aggdata[,-1]),stat,F,c("AG","East","Jones","SAC","SJR"))
        
        aggdata <-aggregate(volFlow[,grepl(stat,names(volFlow))], by=list(volFlow$month), 
                            FUN=mean, na.rm=TRUE)
        gM<-makeCompChart(t(aggdata[,-1]),stat,T,c("AG","East","Jones","SAC","SJR"))
        
        grid.arrange(gY,gM)
      }else if(stat=="MD10"){
        
        aggdata <-aggregate(volFlow[,grepl(stat,names(volFlow))], by=list(volFlow$year), 
                            FUN=mean, na.rm=TRUE)
        gY<-makeCompChart(t(aggdata[,-1]),stat,F,c("AG","East","MTZ","SAC","SJR"))
        
        aggdata <-aggregate(volFlow[,grepl(stat,names(volFlow))], by=list(volFlow$month), 
                            FUN=mean, na.rm=TRUE)
        gM<-makeCompChart(t(aggdata[,-1]),stat,T,c("AG","East","MTZ","SAC","SJR"))
        
        grid.arrange(gY,gM)
        
      }else if(stat=="D10"){
        aggdata <-aggregate(volFlow[,grepl(paste(stat,"\\.",sep=""),names(volFlow))], by=list(volFlow$year), 
                            FUN=mean, na.rm=TRUE)
        gY<-makeCompChart(t(aggdata[,-1]),stat,F)
        
        aggdata <-aggregate(volFlow[,grepl(paste(stat,"\\.",sep=""),names(volFlow))], by=list(volFlow$month), 
                            FUN=mean, na.rm=TRUE)
        gM<-makeCompChart(t(aggdata[,-1]),stat)
        
        grid.arrange(gY,gM)
      }else{
      aggdata <-aggregate(volFlow[,grepl(stat,names(volFlow))], by=list(volFlow$year), 
                          FUN=mean, na.rm=TRUE)
      gY<-makeCompChart(t(aggdata[,-1]),stat,F)
      
      aggdata <-aggregate(volFlow[,grepl(stat,names(volFlow))], by=list(volFlow$month), 
                          FUN=mean, na.rm=TRUE)
      gM<-makeCompChart(t(aggdata[,-1]),stat)
      
      grid.arrange(gY,gM)
      }
    }else{
      df <- data.frame()
      ggplot(df) + geom_point() +ggtitle("no volumetric flow data for this station")
    }
    
    
  }, height = 800, width = 800)
  
  
  output$nestedPlotChl <- renderPlot({
    
    # inputs
    
    dt_rng <- input$dt_rng
    stat <- input$stat
    index=which(names(perStation)==stat)
    
    # data
    data<- datA()
    
  
    
    
    mod<-perStationFlowMod[[index]]
    toUse=na.omit(data[,c("doy","date_dec","Date","chl","chl.1")])
    byTerm=predict(mod,toUse,type="terms")
    toName=c("doy","date_dec","chl","date","intercept")
    nestPred=as.data.frame(cbind.data.frame(byTerm,toUse$Date,rep(summary(mod)$p.coeff,nrow(toUse))))
    names(nestPred)=toName
    terms<-c("ti(doy)","ti(date_dec)","ti(chl)","intercept")
    ggplot(data,aes(x = Date, y = log(chl)))+geom_point()+
      geom_line(data=nestPred,aes(x=date,y = doy, color = 'ti(doy)'), lwd=1)+
      geom_line(data=nestPred,aes(x=date,y=date_dec, color = 'ti(date_dec)'), lwd=1)+
      geom_line(data=nestPred,aes(x=date,y = chl, color = 'ti(chl)'), lwd=1)+
      geom_line(data=nestPred,aes(x=date,y = intercept, color = 'intercept'), lwd=1)+
      scale_colour_manual(name = '',
                          labels =c('red'=terms[1],"dodgerblue"=terms[2],
                                    "forestgreen"=terms[3],"purple"=terms[4]),values=c("red",
                                                                                       "dodgerblue","forestgreen","purple")
      ) +
      ggtitle(paste(names(perStation)[index],"Component-Wise Predictions Chl Flow Model",sep=" "))+scale_x_date(limits = dt_rng)+
      ylab("ln(chl a) ")+xlab("Date")+ylim(input$ylim34L,input$ylim34U)
    
    
  }, height = 250, width = 1200)
  
  output$nestedPlotFlow <- renderPlot({
    
    # inputs
    
    dt_rng <- input$dt_rng
    stat <- input$stat
    index=which(names(perStation)==stat)
    
    # data
    data<- datA()
    data$TOT=0.028316847*data$TOT
    mod<-perStationFlowTOT[[index]]
    toUse=na.omit(data[,c("doy","date_dec","Date","TOT")])
    byTerm=predict(mod,toUse,type="terms")
    toName=c("doy","date_dec","tot","date","intercept")
   
    nestPred=as.data.frame(cbind.data.frame(byTerm,toUse$Date,rep(summary(mod)$p.coeff,nrow(toUse))))
    names(nestPred)=toName
    terms<-c("ti(doy)","ti(date_dec)","ti(tot)","intercept")
    ggplot(data,aes(x = Date, y = log(chl)))+geom_point()+
      geom_line(data=nestPred,aes(x=date,y = doy, color = 'ti(doy)'), lwd=1)+
      geom_line(data=nestPred,aes(x=date,y=date_dec, color = 'ti(date_dec)'), lwd=1)+
      geom_line(data=nestPred,aes(x=date,y = tot, color = 'ti(tot)'), lwd=1)+
      geom_line(data=nestPred,aes(x=date,y = intercept, color = 'intercept'), lwd=1)+
      scale_colour_manual(name = '',
                          labels =c('red'=terms[1],"dodgerblue"=terms[2],
                                    "forestgreen"=terms[3],"purple"=terms[4]),values=c("red",
                                                                                       "dodgerblue","forestgreen","purple")
      ) +
      ggtitle(paste(names(perStation)[index],"Component-Wise Predictions Total Flow Model",sep=" "))+scale_x_date(limits = dt_rng)+
      ylab("ln(chl a) ")+xlab("Date")+ylim(input$ylim34L,input$ylim34U)
    
    
  }, height = 250, width = 1200)
  
})