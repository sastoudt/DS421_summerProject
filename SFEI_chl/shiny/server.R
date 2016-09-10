## bugs in fittedSpat and all of the nested plots
## discrete value supplied to continuous scale (mostly)
## data of class uneval, fixed now discrete value issue
## $ operator invalid



# packages to use
library(ggplot2)
library(sp)
library(gridExtra)
library(reshape)
library(mgcv)
library(lubridate)

## all bugs worked out with data locally, same data as on Dropbox
# load(file = "~/Desktop/sfei/perStationParsimoniousModels.Rda")
# load(file = "~/Desktop/sfei/perStationFullModels.Rda")
# load(file="~/Desktop/sfei/perStationInteractionModels.Rda")
# load(file="~/Desktop/sfei/mod1Spatial.RData")
# load(file="~/Desktop/sfei/mod2Spatial.RData")
# load(file="~/Desktop/sfei/mod3Spatial.RData")
# load(file="~/Desktop/sfei/mod4Spatial.RData")
# load(file="~/Desktop/sfei/perStationAdd.Rda")
# load(file="~/Desktop/sfei/perStationFlowMod.Rda")
# load(file="~/Desktop/sfei/perStationFlowTOT.Rda")
# load(file="~/Desktop/sfei/perStationPredVal.Rda")

# raw data
#load(file = "perStation.Rda")
# load(file = "perStationParsimoniousModels.Rda")
# load(file = "perStationFullModels.Rda")
# load(file="perStationInteractionModels.Rda")
# #load(file="~/Desktop/DS421_summerProject/GAM_weightedReg_comparison/shiny/data/delt_map.RData")
# load(file="mod1Spatial.RData")
# load(file="mod2Spatial.RData")
# load(file="mod3Spatial.RData")
# load(file="mod4Spatial.RData")
# load(file="perStationAdd.Rda")
# load(file="perStationFlowMod.Rda")
# load(file="perStationFlowTOT.Rda")
# load(file="perStationPredVal.Rda")
#full=read.csv("sfeiPlusDates.csv")
#allData=read.csv("allData.csv")
volFlow=read.csv("VolFingerPrintsMaster.csv")
require(httr)

# response<- GET(url="https://www.dropbox.com/s/nqvmugpezqb5uxu/mod1Spatial.RData?dl=0")
# load(rawConnection(response$content))
# 
# response<- GET(url="https://www.dropbox.com/s/7ldafvwffwer8e3/mod2Spatial.RData?dl=0")
# load(rawConnection(response$content))
# 
# response<- GET(url="https://www.dropbox.com/s/wkmn4a16i8ikdxl/mod3Spatial.RData?dl=0")
# load(rawConnection(response$content))
# 
# response<- GET(url="https://www.dropbox.com/s/lg27vg3a9wddexp/mod4Spatial.RData?dl=0")
# load(rawConnection(response$content))
# 
# 
# response<- GET(url="https://www.dropbox.com/s/4colbyhrto2jxwl/perStationAdd.Rda?dl=0")
# load(rawConnection(response$content))
# 
# response<- GET(url="https://www.dropbox.com/s/5ab5if0hbjour78/perStationFlowModels.Rda?dl=0")
# load(rawConnection(response$content))
# 
# response<- GET(url="https://www.dropbox.com/s/ea6oskjwyktxisk/perStationFlowSpecific.Rda?dl=0")
# load(rawConnection(response$content))
# 
# response<- GET(url="https://www.dropbox.com/s/3tppvhwojkl0nu6/perStationFlowTOT.Rda?dl=0")
# load(rawConnection(response$content))
# 
# response<- GET(url="https://www.dropbox.com/s/mo8kob5m8f3yykd/perStationFullModels.Rda?dl=0")
# load(rawConnection(response$content))
# 
# response<- GET(url="https://www.dropbox.com/s/8z5l8hnssfuvh5f/perStationInteractionModels.Rda?dl=0")
# load(rawConnection(response$content))
# 
# response<- GET(url="https://www.dropbox.com/s/mj7b9crkyaxaxjd/perStationParsimoniousModels.Rda?dl=0")
# load(rawConnection(response$content))

response<- GET(url="https://www.dropbox.com/s/vqolh5rnp2w1iai/perStationPredVal.Rda?dl=0")
load(rawConnection(response$content))

#response<- GET(url="https://www.dropbox.com/s/3ql5nihqjt1pxak/perStationTOTModels.Rda?dl=0")
#load(rawConnection(response$content))
# Define server logic required to generate and plot data
wholeSeries<-c(1, 2, 5, 7, 11, 13, 15, 16, 17, 18, 21, 22, 23, 29, 40)
allData<- do.call("rbind", perStationAdd[wholeSeries])
forMap=allData[,c("Longitude","Latitude","Station")]
forMap=unique(forMap)

dt_rng=c(NA,NA)

stationNames= c("C10", "C3", "C7" , "C9","D10","D11","D12","D14A","D15","D16","D19",    
"D2","D22", "D24","D26","D28A", "D4","D41","D41A","D42", "D6","D7", "D8","D9","EZ2","EZ2-SJR", "EZ6","EZ6-SJR",
"MD10","MD6", "MD7","NZ002","NZ004","NZ032","NZ325","NZS42","P10" ,"P12","P2", "P8","S42")

longitude=c(-121.2647, -121.5205, -121.9183, -121.8063, -121.6148, -121.7391, -121.5669, -121.5730, -121.8205,
            -122.3729, -122.1177,-122.0397, -121.9900, -121.4199, -121.3823)
latitude=c(37.67934, 38.36771, 38.04631, 38.02161, 38.04376, 38.08453, 38.07664, 37.97048, 38.06248, 38.03022,
           38.04436, 38.11714, 38.05992, 38.04226, 37.97817)

getSummaryRMSE<-function(data,namePred){
  
  trueVal=data$chl 
  
  predVal=data[,namePred]
  
  rmse=sqrt(sum((trueVal-predVal)^2,na.rm=T)/sum(!is.na((trueVal-predVal)^2)))
  
  data$month=as.numeric(strftime(data$Date, '%m'))
  data$year=as.numeric(strftime(data$Date, '%Y'))
  
  annual1=subset(data,year<1982 & year>=1975)
  annual1I=which(data$year<1982 & data$year>=1975)
  annual1P=predVal[annual1I]
  
  
  annual2=subset(data,year<1989 & year>=1982)
  annual2I=which(data$year<1989 & data$year>=1982)
  annual2P=predVal[annual2I]
  
  
  annual3=subset(data,year<1996 & year>=1989)
  annual3I=which(data$year<1996 & data$year>=1989)
  annual3P=predVal[annual3I]
  
  annual4=subset(data,year<2003 & year>=1996)
  annual4I=which(data$year<2003 & data$year>=1996)
  annual4P=predVal[annual4I]
  
  annual5=subset(data,year<2010 & year>=2003)
  annual5I=which(data$year<2010 & data$year>=2003)
  annual5P=predVal[annual5I]
  
  
  annual6=subset(data,year>=2010) ## has 2 fewer years
  annual6I=which( data$year>=2010)
  annual6P=predVal[annual6I]
  
  
  rmseA1=sqrt(sum((annual1$chl-annual1P)^2,na.rm=T)/sum(!is.na((annual1$chl-annual1P)^2)))
  rmseA2=sqrt(sum((annual2$chl-annual2P)^2,na.rm=T)/sum(!is.na((annual2$chl-annual2P)^2)))
  rmseA3=sqrt(sum((annual3$chl-annual3P)^2,na.rm=T)/sum(!is.na((annual3$chl-annual3P)^2)))
  rmseA4=sqrt(sum((annual4$chl-annual4P)^2,na.rm=T)/sum(!is.na((annual4$chl-annual4P)^2)))
  rmseA5=sqrt(sum((annual5$chl-annual5P)^2,na.rm=T)/sum(!is.na((annual5$chl-annual5P)^2)))
  rmseA6=sqrt(sum((annual6$chl-annual6P)^2,na.rm=T)/sum(!is.na((annual6$chl-annual6P)^2)))
  
  seasonal1=subset(data,month %in% c(1:3))
  seasonal1I=which(data$month %in% c(1:3))
  seasonal1P=predVal[seasonal1I]
  
  seasonal2=subset(data,month %in% c(4:6))
  seasonal2I=which(data$month %in% c(4:6))
  seasonal2P=predVal[seasonal2I]
  
  seasonal3=subset(data,month %in% c(7:9))
  seasonal3I=which(data$month %in% c(7:9))
  seasonal3P=predVal[seasonal3I]
  
  seasonal4=subset(data,month %in% c(10:12))
  seasonal4I=which(data$month %in% c(10:12))
  seasonal4P=predVal[seasonal4I]
  
  
  rmseS1=sqrt(sum((seasonal1$chl-seasonal1P)^2,na.rm=T)/sum(!is.na((seasonal1$chl-seasonal1P)^2)))
  rmseS2=sqrt(sum((seasonal2$chl-seasonal2P)^2,na.rm=T)/sum(!is.na((seasonal2$chl-seasonal2P)^2)))
  rmseS3=sqrt(sum((seasonal3$chl-seasonal3P)^2,na.rm=T)/sum(!is.na((seasonal3$chl-seasonal3P)^2)))
  rmseS4=sqrt(sum((seasonal4$chl-seasonal4P)^2,na.rm=T)/sum(!is.na((seasonal4$chl-seasonal4P)^2)))
  
  
  
  rmse=rbind(rmse,rmseA1,rmseA2,rmseA3,rmseA4,rmseA5,rmseA6,rmseS1,rmseS2,rmseS3,rmseS4)
  
  return(rmse)
  # return(list(all=rmse,annual1=rmseA1,annual2=rmseA2,annual3=rmseA3,annual4=rmseA4,
  #             seasonal1=rmseS1, seasonal2=rmseS2, seasonal3=rmseS3, seasonal4=rmseS4,
  #             flow1=rmseF1,flow2=rmseF2,flow3=rmseF3,flow4=rmseF4))
}

spatialPlotRMSE_pieces=function(data,breakdownPieces,modName,removeOutlier=F){
  testMerge=cbind(forMap,data)
  
  if(removeOutlier){
    testMerge=testMerge[-1,]
  }
  
  if(length(breakdownPieces)==4){
    
    g1<-ggplot(testMerge,aes_string(x = "Longitude", y = "Latitude",colour=breakdownPieces[1],cex=2))+geom_point()+
      ggtitle(paste(modName,breakdownPieces[1]))+scale_size(guide=F)
    
    g2<-ggplot(testMerge,aes_string(x = "Longitude", y = "Latitude",colour=breakdownPieces[2],cex=2))+geom_point()+
      ggtitle(paste(modName,breakdownPieces[2]))+scale_size(guide=F)
    
    g3<-ggplot(testMerge,aes_string(x = "Longitude", y = "Latitude",colour=breakdownPieces[3],cex=2))+geom_point()+
      ggtitle(paste(modName,breakdownPieces[2]))+scale_size(guide=F)
    
    g4<-ggplot(testMerge,aes_string(x = "Longitude", y = "Latitude",colour=breakdownPieces[4],cex=2))+geom_point()+
      ggtitle(paste(modName,breakdownPieces[4]))+scale_size(guide=F)
    
    grid.arrange(g1,g2,g3,g4)
  }else{
    
    g1<-ggplot(testMerge,aes_string(x = "Longitude", y = "Latitude",colour=breakdownPieces[1],cex=2))+geom_point()+
      ggtitle(paste(modName,breakdownPieces[1]))+scale_size(guide=F)
    
    g2<-ggplot(testMerge,aes_string(x = "Longitude", y = "Latitude",colour=breakdownPieces[2],cex=2))+geom_point()+
      ggtitle(paste(modName,breakdownPieces[2]))+scale_size(guide=F)
    
    g3<-ggplot(testMerge,aes_string(x = "Longitude", y = "Latitude",colour=breakdownPieces[3],cex=2))+geom_point()+
      ggtitle(paste(modName,breakdownPieces[3]))+scale_size(guide=F)
    
    g4<-ggplot(testMerge,aes_string(x = "Longitude", y = "Latitude",colour=breakdownPieces[4],cex=2))+geom_point()+
      ggtitle(paste(modName,breakdownPieces[4]))+scale_size(guide=F)
    
    g5<-ggplot(testMerge,aes_string(x = "Longitude", y = "Latitude",colour=breakdownPieces[5],cex=2))+geom_point()+
      ggtitle(paste(modName,breakdownPieces[5]))+scale_size(guide=F)
    
    g6<-ggplot(testMerge,aes_string(x = "Longitude", y = "Latitude",colour=breakdownPieces[6],cex=2))+geom_point()+
      ggtitle(paste(modName,breakdownPieces[6]))+scale_size(guide=F)
    
    grid.arrange(g1,g2,g3,g4,g5,g6)
  }
}



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
    
    
    out<-perStationPredVal[[which(stationNames==stat)]]
    
    
    return(out)
    
  })
  
  # datA <- reactive({
  #   
  #   stat <- input$stat
  #   
  #   
  #   out<-perStationAdd[[which(names(perStationAdd)==stat)]]
  #   
  #   
  #   return(out)
  #   
  # })
  
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
    
    
    index=which(names(perStationAdd)==input$stat)
    
    if(index %in% c(5,7,13)){
      selectInput("plot5choice1","Select a variable in parsimonious model.",c("doy","date_dec","pheo","do_per"),"Intercept will be added to center results")
      
    }else{
      selectInput("plot5choice1","Select a variable in parsimonious model.",c("doy","date_dec","pheo","tn","do_per"),"Intercept will be added to center results")
      
    }
    
    
  })
  
  output$plot5choice2<- renderUI({
    
    
    index=which(names(perStationAdd)==input$stat)
    
    if(index %in% c(5,7,13)){
      selectInput("plot5choice2","Select a variable in parsimonious model.",c("doy","date_dec","pheo","do_per"))
      
    }else{
      selectInput("plot5choice2","Select a variable in parsimonious model.",c("doy","date_dec","pheo","tn","do_per"))
      
    }
    
    
  })
  
  output$plot6choice1<- renderUI({
    
    
    index=which(names(perStationAdd)==input$stat)
    
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
    
    
    index=which(names(perStationAdd)==input$stat)
    
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
    index=which(stationNames==stat)
    
    # data
    #data<-dat()
    #mod<-perStationParsMod[[index]]
    
    #if(index %in% c(5,7,13)){
     # toUse=na.omit(data[,c("doy","date_dec","pheo","do_per","Date")])
    #   
    # }else{
    #   
    #   toUse=na.omit(data[,c("doy","date_dec","pheo","tn","do_per","Date")])
    #   
    # }
    # 
    # fullPred=predict(mod,toUse,type="response")
    # toUse=as.data.frame(cbind.data.frame(toUse,fullPred))
    # names(toUse)[ncol(toUse)]="fitted.values"
    data=perStationPredVal[[index]]
    ggplot(data,aes(x = Date, y = chl))+geom_point()+
      geom_line(aes(x=Date,y =predPars ,col="red"),lwd=1)+
      ggtitle(paste(stationNames[index], "Fitted Values Parsimonious Model",sep=" "))+
      theme(legend.position='none')+
      scale_x_date(limits = dt_rng)+ylab("chl a (microgram/L)")+xlab("Date")+ylim(0,input$ylim12)
    
    
  }, height = 250, width = 1200)
  
  output$fittedFull<- renderPlot({
    
    # inputs
    dt_rng <- input$dt_rng
    stat <- input$stat
    index=which(stationNames==stat)
    
    # data
    # data<-dat()
    # mod<-perStationFullMod[[index]]
    # 
    # if(index %in% c(5,7)){
    #   toUse=na.omit(data[,c("doy","date_dec","pheo","do_per",
    #                         "sal","Date")])
    #   fullPred=predict(mod,toUse,type="response")
    #   toUse=as.data.frame(cbind.data.frame(toUse,fullPred))
    #   names(toUse)[ncol(toUse)]="fitted.values"
    #   
    # }else if(index==13){
    #   
    # }else if(index %in% c(17, 18, 21, 22, 23)){
    #   toUse=na.omit(data[,c("doy","date_dec","pheo","tn","do_per",
    #                         "sio2","tp","tss","nh4","sal","Date")])
    #   fullPred=predict(mod,toUse,type="response")
    #   toUse=as.data.frame(cbind.data.frame(toUse,fullPred))
    #   names(toUse)[ncol(toUse)]="fitted.values"
    # }else{
    #   toUse=na.omit(data[,c("doy","date_dec","pheo","tn","do_per",
    #                         "sio2","tp","tss","nh4","Date")])
    #   fullPred=predict(mod,toUse,type="response")
    #   toUse=as.data.frame(cbind.data.frame(toUse,fullPred))
    #   names(toUse)[ncol(toUse)]="fitted.values"
    # }
    # 
    
   data=perStationPredVal[[index]]
      ggplot(data,aes(x = Date, y = chl))+geom_point()+
        geom_line(aes(x=Date,y =predFull ,col="red"),lwd=1)+
        ggtitle(paste(stationNames[index], "Fitted Values Full Model",sep=" "))+
        theme(legend.position='none')+ylim(0,160)+
        scale_x_date(limits = dt_rng)+ylab("chl a (microgram/L)")+xlab("Date")+ylim(0,input$ylim12)
   
    
    
    
    
    
  }, height = 250, width = 1200)
  
  
  output$fittedInt<- renderPlot({
    
    # inputs
    dt_rng <- input$dt_rng
    stat <- input$stat
    index=which(stationNames==stat)
    
    # data
    # data<-dat()
    # mod<-perStationIntMod[[index]]
    # 
    # 
    # toUse=na.omit(data[,c("doy","date_dec","Date")])
    # 
    # fullPred=predict(mod,toUse,type="response")
    # toUse=as.data.frame(cbind.data.frame(toUse,fullPred))
    # names(toUse)[ncol(toUse)]="fitted.values"
    data=perStationPredVal[[index]]
    ggplot(data,aes(x = Date, y = chl))+geom_point()+
      geom_line(aes(x=Date,y =predInt ,col="red"),lwd=1)+
      ggtitle(paste(stationNames[index], "Fitted Values Interaction Model",sep=" "))+
      theme(legend.position='none')+
      scale_x_date(limits = dt_rng)+ylab("chl a (microgram/L)")+xlab("Date")+ylim(0,input$ylim12)
    
    
  }, height = 250, width = 1200)
  
  output$fittedSpat<- renderPlot({
    
    # inputs
    dt_rng <- input$dt_rng
    stat <- input$stat
  index=which(stationNames==stat)
    
    # data
   # data<-subset(allData,Station==stat)
    #data$Date=as.Date(as.character(data$Date))
  data=perStationPredVal[[index]]
    
    if(input$spatMod=="spatIntercept"){
      indexCol=79
    }else if(input$spatMod=="spatDate_Dec"){
      indexCol=80
    }else if(input$spatMod=="spatDOY"){
      indexCol=81
    }else if(input$spatMod=="spatinteraction"){
      indexCol=82
    }
   
    
    ggplot(data,aes(x = Date, y = chl))+geom_point()+
      #geom_line(aes(x=Date,y =names(data)[index] ,col="red"),lwd=1)+
      geom_line(aes_string(x="Date",y = names(data)[indexCol], color = shQuote("red")),lwd=1)+
      ggtitle(paste(input$spatMod,stationNames[index], "Fitted Values Model",sep=" "))+
      theme(legend.position='none')+
      scale_x_date(limits = dt_rng)+ylab("chl a (microgram/L)")+xlab("Date")+ylim(0,input$ylim12)
    
    
  }, height = 250, width = 1200)
  
  output$fittedChl<- renderPlot({
    
    # inputs
    dt_rng <- input$dt_rng
    stat <- input$stat
    index=which(stationNames==stat)
    
    # data
    #data=perStationAdd[[index]]
    data=perStationPredVal[[index]]
    ggplot(data,aes(x = Date, y = chl))+geom_point()+
      #geom_line(aes(x=Date,y =names(data)[index] ,col="red"),lwd=1)+
      geom_line(aes(x=Date,y = chlPred, color = "red"),lwd=1)+
      
      ggtitle(paste(stationNames[index],"Chl from Other Station Fitted Values Model",sep=" "))+
      theme(legend.position='none')+
      scale_x_date(limits = dt_rng)+ylab("chl a (microgram/L)")+xlab("Date")+ylim(0,input$ylim12)
    
    
  }, height = 250, width = 1200)
  
  output$fittedFlow<- renderPlot({
    
    # inputs
    dt_rng <- input$dt_rng
    stat <- input$stat
    index=which(stationNames==stat)
    
    # data
    #data=perStationAdd[[index]]
    data=perStationPredVal[[index]]
    
    ggplot(data,aes(x = Date, y = chl))+geom_point()+
      #geom_line(aes(x=Date,y =names(data)[index] ,col="red"),lwd=1)+
      geom_line(aes(x=Date,y = flowPred, color = "red"),lwd=1)+
      
      ggtitle(paste(stationNames[index],"Flow Fitted Values Model",sep=""))+
      theme(legend.position='none')+
      scale_x_date(limits = dt_rng)+ylab("chl a (microgram/L)")+xlab("Date")+ylim(0,input$ylim12)
    
    
  }, height = 250, width = 1200)
  
  
  
  output$nestedPlotPars <- renderPlot({
    
    # inputs
    
    dt_rng <- input$dt_rng
    stat <- input$stat
    index=which(stationNames==stat)
    
    # data
   # data<-dat()
    #mod<-perStationParsMod[[index]]
    data=perStationPredVal[[index]]
    
    if(index %in% c(5,7,13)){
      #toUse=na.omit(data[,c("doy","date_dec","pheo","do_per","Date")])
      #toName=c("doy","date_dec","pheo","do_per","date","intercept")
      terms<-c("ti(pheo)","ti(doy)","ti(date_dec)","ti(do_per)","intercept")
      toPlot=which(grepl("parsMod_",names(data)))
      ggplot(data,aes(x = Date, y = log(chl)))+geom_point()+
        geom_line(aes_string(x="Date",y =names(data)[toPlot[1]] , color = shQuote('ti(doy)')), lwd=1)+
        geom_line(aes_string(x="Date",y=names(data)[toPlot[2]], color = shQuote('ti(date_dec)')), lwd=1)+
        geom_line(aes_string(x="Date",y = names(data)[toPlot[3]], color = shQuote('ti(pheo)')),lwd=1)+
        geom_line(aes_string(x="Date",y = names(data)[toPlot[4]], color = shQuote('ti(do_per)')), lwd=1)+
        geom_line(aes_string(x="Date",y = names(data)[toPlot[5]], color = shQuote('intercept')), lwd=1)+
        scale_colour_manual(name = '',
                            labels =c('red'=terms[1],'orange'=terms[2],"dodgerblue"=terms[3],
                                      "blue"=terms[4],"purple"=terms[5]),values=c("red","orange",
                                                                                  "dodgerblue","blue","purple")
        ) +
        ggtitle(paste(stationNames[index],"Component-Wise Predictions Parsimonious Model",sep=" "))+scale_x_date(limits = dt_rng)+
        ylab("ln(chl a) ")+xlab("Date")+ylim(input$ylim34L,input$ylim34U)
      
    }else{
      #toUse=na.omit(data[,c("doy","date_dec","pheo","tn","do_per","Date")])
      #toName=c("doy","date_dec","pheo","tn","do_per","date","intercept")
      terms<-c("ti(pheo)","ti(doy)","ti(date_dec)","ti(tn)","ti(do_per)","intercept")
      toPlot=which(grepl("parsMod_",names(data)))
      ggplot(data,aes(x = Date, y = log(chl)))+geom_point()+
        
        geom_line(aes_string(x="Date",y =names(data)[toPlot[1]] , color = shQuote('ti(doy)')), lwd=1)+
        geom_line(aes_string(x="Date",y=names(data)[toPlot[2]], color = shQuote('ti(date_dec)')), lwd=1)+
        geom_line(aes_string(x="Date",y = names(data)[toPlot[3]], color = shQuote('ti(pheo)')),lwd=1)+
        geom_line(aes_string(x="Date",y = names(data)[toPlot[4]], color = shQuote('ti(tn)')),lwd=1)+
        geom_line(aes_string(x="Date",y = names(data)[toPlot[5]], color = shQuote('ti(do_per)')), lwd=1)+
        geom_line(aes_string(x="Date",y = names(data)[toPlot[6]], color = shQuote('intercept')), lwd=1)+
        scale_colour_manual(name = '',
                            labels =c('red'=terms[1],'orange'=terms[2],"dodgerblue"=terms[3],
                                      "blue"=terms[4],"purple"=terms[5],"forestgreen"=terms[6]),values=c("red","orange",
                                                                                  "dodgerblue","blue","purple","forestgreen")
        ) +
        ggtitle(paste(stationNames[index],"Component-Wise Predictions Parsimonious Model",sep=" "))+scale_x_date(limits = dt_rng)+
        ylab("ln(chl a) ")+xlab("Date")+ylim(input$ylim34L,input$ylim34U)
      
    }
    
    
    
  }, height = 250, width = 1200)
  
  output$nestedPlotFull <- renderPlot({
    
    # inputs
    
    dt_rng <- input$dt_rng
    stat <- input$stat
    index=which(stationNames==stat)
    
    # data
   data=perStationPredVal[[index]]
    
    if(index %in% c(5,7)){
      #toUse=na.omit(data[,c("doy","date_dec","pheo","do_per",
                           # "sal","Date")])
      terms<-c("ti(pheo)","ti(doy)","ti(date_dec)","ti(do_per)",
               "ti(sal)",
               "intercept")
      toPlot=which(grepl("parsFull_",names(data)))
      
      
      ggplot(data,aes(x = Date, y = log(chl)))+geom_point()+
        
        geom_line(aes_string(x="Date",y = names(data)[toPlot[1]], color = shQuote('ti(doy)')), lwd=1)+
        geom_line(aes_string(x="Date",y=names(data)[toPlot[2]], color = shQuote('ti(date_dec)')), lwd=1)+
        geom_line(aes_string(x="Date",y = names(data)[toPlot[3]], color = shQuote('ti(pheo)')),lwd=1)+
        geom_line(aes_string(x="Date",y = names(data)[toPlot[4]], color = shQuote('ti(do_per)')), lwd=1)+
        
        geom_line(aes_string(x="Date",y = names(data)[toPlot[5]], color = shQuote('ti(sal)')), lwd=1)+
        
        geom_line(aes_string(x="Date",y = names(data)[toPlot[6]], color = shQuote('intercept')), lwd=1)+
        scale_colour_manual(name = '',
                            labels =c('red'=terms[1],'orange'=terms[2],"dodgerblue"=terms[3],
                                      "blue"=terms[4],'mediumturquoise'=terms[5],"black"=terms[6]),
                            values=c("red","orange",
                                     "dodgerblue","blue","mediumturquoise","black")
        ) +
        ggtitle(paste(stationNames[index],"Component-Wise Predictions Full Model",sep=" "))+scale_x_date(limits = dt_rng)+
        ylab("ln(chl a) ")+xlab("Date")+ylim(input$ylim34L,input$ylim34U)
      
    }else if(index==13){
      df <- data.frame()
      ggplot(df) + geom_point() +  scale_x_date(limits = dt_rng)+ggtitle("no data for extra variables in full model")
    }else if(index %in% c(17,18,21,22,23)){
      
      terms<-c("ti(pheo)","ti(doy)","ti(date_dec)","ti(tn)","ti(do_per)",
               "ti(sio2)","ti(tp)","ti(tss)","ti(nh4)","ti(sal)",
               "intercept")
      toPlot=which(grepl("parsFull_",names(data)))
      
      ggplot(data,aes(x = Date, y = log(chl)))+geom_point()+
       
        geom_line(aes_string(x="Date",y =  names(data)[toPlot[1]], color = shQuote('ti(doy)')), lwd=1)+
        geom_line(aes_string(x="Date",y= names(data)[toPlot[2]], color = shQuote('ti(date_dec)')), lwd=1)+
        geom_line(aes_string(x="Date",y =  names(data)[toPlot[3]], color = shQuote('ti(pheo)')),lwd=1)+
        geom_line(aes_string(x="Date",y =  names(data)[toPlot[4]], color = shQuote('ti(tn)')), lwd=1)+
        geom_line(aes_string(x="Date",y =  names(data)[toPlot[5]], color = shQuote('ti(do_per)')), lwd=1)+
        geom_line(aes_string(x="Date",y =  names(data)[toPlot[6]], color = shQuote('ti(sio2)')), lwd=1)+
        geom_line(aes_string(x="Date",y =  names(data)[toPlot[7]], color = shQuote('ti(tp)')), lwd=1)+
        geom_line(aes_string(x="Date",y =  names(data)[toPlot[8]], color = shQuote('ti(tss)')), lwd=1)+
        geom_line(aes_string(x="Date",y =  names(data)[toPlot[9]], color = shQuote('ti(nh4)')), lwd=1)+
        geom_line(aes_string(x="Date",y =  names(data)[toPlot[10]], color = shQuote('ti(sal)')), lwd=1)+
        
        geom_line(aes(x="Date",y =  names(data)[toPlot[11]], color = shQuote('intercept')), lwd=1)+
        scale_colour_manual(name = '',
                            labels =c('red'=terms[1],'orange'=terms[2],"dodgerblue"=terms[3],
                                      "forestgreen"=terms[4],"blue"=terms[5],"purple"=terms[6],
                                      "magenta"=terms[7],'grey'=terms[8],'mediumturquoise'=terms[9],
                                      "chocolate3"=terms[10],"black"=terms[11]),values=c("red","orange",
                                                                                         "dodgerblue","forestgreen","blue","purple","magenta",
                                                                                         "grey","mediumturquoise","chocolate3","black")
        ) +
        ggtitle(paste(stationNames[index],"Component-Wise Predictions Full Model",sep=" "))+scale_x_date(limits = dt_rng)+
        ylab("ln(chl a) ")+xlab("Date")+ylim(input$ylim34L,input$ylim34U)
    }else{

      terms<-c("ti(pheo)","ti(doy)","ti(date_dec)","ti(tn)","ti(do_per)",
               "ti(sio2)","ti(tp)","ti(tss)","ti(nh4)",
               "intercept")
      toPlot=which(grepl("parsFull_",names(data)))
      
      
      ggplot(data,aes(x = Date, y = log(chl)))+geom_point()+
       
        geom_line(aes_string(x="Date",y = names(data)[toPlot[1]], color = shQuote('ti(doy)')), lwd=1)+
        geom_line(aes_string(x="Date",y=names(data)[toPlot[2]], color = shQuote('ti(date_dec)')), lwd=1)+
        geom_line(aes_string(x="Date",y = names(data)[toPlot[3]], color = shQuote('ti(pheo)')),lwd=1)+
        geom_line(aes_string(x="Date",y = names(data)[toPlot[4]], color = shQuote('ti(tn)')), lwd=1)+
        geom_line(aes_string(x="Date",y = names(data)[toPlot[5]], color = shQuote('ti(do_per)')), lwd=1)+
        geom_line(aes_string(x="Date",y = names(data)[toPlot[6]], color = shQuote('ti(sio2)')), lwd=1)+
        geom_line(aes_string(x="Date",y = names(data)[toPlot[7]], color = shQuote('ti(tp)')), lwd=1)+
        geom_line(aes_string(x="Date",y = names(data)[toPlot[8]], color = shQuote('ti(tss)')), lwd=1)+
        geom_line(aes_string(x="Date",y = names(data)[toPlot[9]], color = shQuote('ti(nh4)')), lwd=1)+
        geom_line(aes_string(x="Date",y = names(data)[toPlot[10]], color = shQuote('intercept')), lwd=1)+
        scale_colour_manual(name = '',
                            labels =c('red'=terms[1],'orange'=terms[2],"dodgerblue"=terms[3],
                                      "forestgreen"=terms[4],"blue"=terms[5],"purple"=terms[6],
                                      "magenta"=terms[7],'grey'=terms[8],'mediumturquoise'=terms[9],
                                      "chocolate3"=terms[10]),values=c("red","orange",
                                                                       "dodgerblue","forestgreen","blue","purple","magenta",
                                                                       "grey","mediumturquoise","chocolate3")
        ) +
        ggtitle(paste(stationNames[index],"Component-Wise Predictions Full Model",sep=" "))+scale_x_date(limits = dt_rng)+
        ylab("ln(chl a) ")+xlab("Date")+ylim(input$ylim34L,input$ylim34U)
    }
    
    
  }, height = 250, width = 1200)
  
  output$nestedPlotParsJust2 <- renderPlot({
    
    # inputs
    
    dt_rng <- input$dt_rng
    stat <- input$stat
    index=which(stationNames==stat)
    
    # data
    data<-perStationPredVal[[index]]
   
    
    if(index %in% c(5,7,13)){
    
      terms<-c("ti(pheo)","ti(doy)","ti(date_dec)","ti(do_per)","intercept")
      getID=c("doy","date_dec","pheo","do_per")  
      
    }else{
     
      terms<-c("ti(pheo)","ti(doy)","ti(date_dec)","ti(tn)","ti(do_per)","intercept")
      getID=c("doy","date_dec","pheo","tn","do_per")
      
    }
    
   
    
    id1=which(getID==input$plot5choice1)
    id2=which(getID==input$plot5choice2)
    toPlot=which(grepl("parsMod_",names(data)))
    

    ggplot(data,aes(x = Date, y = log(chl)))+geom_point()+
      geom_line(aes_string(x="Date",y = names(data)[toPlot[id1]], color = "input$plot5choice1"),lwd=1)+
      geom_line(aes_string(x="Date",y = names(data)[toPlot[id2]], color = "input$plot5choice2"),lwd=1)+
      scale_colour_manual(name = '',
                          labels =c('red'=input$plot5choice1,"dodgerblue"=input$plot5choice2)
                          ,values=c("red", "dodgerblue")
      ) +
      ggtitle(paste(stationNames[index],"Component-Wise Predictions Parsimonious Model \n Intercept added in order to center components",sep=" "))+scale_x_date(limits = dt_rng)+
      ylab("ln(chl a) ")+xlab("Date")+ylim(input$ylim34L,input$ylim34U)
    
  }, height = 250, width = 1200)
  
  output$nestedPlotFullJust2 <- renderPlot({
    
    # inputs
    
    dt_rng <- input$dt_rng
    stat <- input$stat
    index=which(stationNames==stat)
    
    # data
    
    data=perStationPredVal[[index]]

    if(index %in% c(5,7)){
      getID=c("doy","date_dec","pheo","do_per","sal")
      id1=which(getID==input$plot6choice1)
      id2=which(getID==input$plot6choice2)
      toPlot=which(grepl("parsFull_",names(data)))
      
      ggplot(data,aes(x = Date, y = log(chl)))+geom_point()+
        geom_line(aes_string(x="Date",y = names(data)[toPlot[id1]], color = "input$plot6choice1"),lwd=1)+
        geom_line(aes_string(x="Date",y = names(data)[toPlot[id2]], color = "input$plot6choice2"),lwd=1)+
        scale_colour_manual(name = '',
                            labels =c('red'=input$plot6choice1,"dodgerblue"=input$plot6choice2)
                            ,values=c("red", "dodgerblue")
        ) +
        ggtitle(paste(stationNames[index],"Component-Wise Predictions Full Model \n Intercept added in order to center components",sep=" "))+scale_x_date(limits = dt_rng)+
        ylab("ln(chl a) ")+xlab("Date")+ylim(input$ylim34L,input$ylim34U)
      
    }else if(index==13){
      df <- data.frame()
      ggplot(df) + geom_point() +  scale_x_date(limits = dt_rng)+ggtitle("no data for extra variables in full model")
      
    }else if(index %in% c(17, 18, 21, 22, 23)){
      getID=c("doy","date_dec","pheo","tn","do_per", "sio2","tp","tss","nh4","sal")
      id1=which(getID==input$plot6choice1)
      id2=which(getID==input$plot6choice2)
      toPlot=which(grepl("parsFull_",names(data)))
      
      ggplot(data,aes(x = Date, y = log(chl)))+geom_point()+
        geom_line(aes_string(x="Date",y = names(data)[toPlot[id1]], color = "input$plot6choice1"),lwd=1)+
        geom_line(aes_string(x="Date",y = names(data)[toPlot[id2]], color = "input$plot6choice2"),lwd=1)+
        scale_colour_manual(name = '',
                            labels =c('red'=input$plot6choice1,"dodgerblue"=input$plot6choice2)
                            ,values=c("red", "dodgerblue")
        ) +
        ggtitle(paste(stationNames[index],"Component-Wise Predictions Full Model \n Intercept added in order to center components",sep=" "))+scale_x_date(limits = dt_rng)+
        ylab("ln(chl a) ")+xlab("Date")+ylim(input$ylim34L,input$ylim34U)
    }else{
      getID=c("doy","date_dec","pheo","tn","do_per","sio2","tp","tss","nh4")
      id1=which(getID==input$plot6choice1)
      id2=which(getID==input$plot6choice2)
      ggplot(data,aes(x = Date, y = log(chl)))+geom_point()+
        geom_line(aes_string(x="Date",y = names(data)[toPlot[id1]], color = "input$plot6choice1"),lwd=1)+
        geom_line(aes_string(x="Date",y = names(data)[toPlot[id2]], color = "input$plot6choice2"),lwd=1)+
        scale_colour_manual(name = '',
                            labels =c('red'=input$plot6choice1,"dodgerblue"=input$plot6choice2)
                            ,values=c("red", "dodgerblue")
        ) +
        ggtitle(paste(stationNames[index],"Component-Wise Predictions Full Model \n Intercept added in order to center components",sep=" "))+scale_x_date(limits = dt_rng)+
        ylab("ln(chl a) ")+xlab("Date")+ylim(input$ylim34L,input$ylim34U)
      
    }
    
    
    
   
  
    
    
  }, height = 250, width = 1200)
  
  output$mapPlot <- renderPlot({
    wholeSeries<-c(1, 2, 5, 7, 11, 13, 15, 16, 17, 18, 21, 22, 23, 29, 40)
    
    index=which(stationNames==input$stat)
    
    plot(longitude,latitude,pch=19,main="Location of Station",xlab="longitude",ylab="latitude")
    
    
    points(perStationPredVal[[index]]$Longitude[1],perStationPredVal[[index]]$Latitude[1],col="red",pch=19)
    # plot(allData$Longitude,allData$Latitude,pch=19,main="Location of Station",xlab="longitude",ylab="latitude")
    #points(dat()$Longitude,dat()$Latitude,col="red",pch=19)
  },height=300,width=300)
  
  
  output$nestedPlotInt <- renderPlot({
    
    # inputs
    
    dt_rng <- input$dt_rng
    stat <- input$stat
    index=which(stationNames==stat)
    data=perStationPredVal[[index]]
    # data
    # data<-dat()
    # mod<-perStationIntMod[[index]]
    # toUse=na.omit(data[,c("doy","date_dec","Date")])
    # byTerm=predict(mod,toUse,type="terms")
    # toName=c("doy","date_dec","interaction","date","intercept")
    # nestPred=as.data.frame(cbind.data.frame(byTerm,toUse$Date,rep(summary(mod)$p.coeff,nrow(toUse))))
    # names(nestPred)=toName
    terms<-c("ti(doy)","ti(date_dec)","ti(doy,date_dec)","intercept")
    toPlot=which(grepl("parsInt_",names(data)))
    
    ggplot(data,aes(x = Date, y = log(chl)))+geom_point()+
      geom_line(aes_string(x="Date",y = names(data)[toPlot[1]], color = shQuote('ti(doy)')), lwd=1)+
      geom_line(aes_string(x="Date",y=names(data)[toPlot[2]], color = shQuote('ti(date_dec)')), lwd=1)+
      geom_line(aes_string(x="Date",y = names(data)[toPlot[3]], color = shQuote('ti(doy,date_dec)')), lwd=1)+
      geom_line(aes_string(x="Date",y = names(data)[toPlot[4]], color = shQuote('intercept')), lwd=1)+
      scale_colour_manual(name = '',
                          labels =c('red'=terms[1],"dodgerblue"=terms[2],
                                    "forestgreen"=terms[3],"purple"=terms[4]),values=c("red",
                                                                                       "dodgerblue","forestgreen","purple")
      ) +
      ggtitle(paste(stationNames[index],"Component-Wise Predictions Interaction Model",sep=" "))+scale_x_date(limits = dt_rng)+
      ylab("ln(chl a) ")+xlab("Date")+ylim(input$ylim34L,input$ylim34U)
    
    
  }, height = 250, width = 1200)
  
  output$nestedPlotIntJust2 <- renderPlot({
    
    # inputs
    
    dt_rng <- input$dt_rng
    stat <- input$stat
    index=which(stationNames==stat)
    
    # data
    data=perStationPredVal[[index]]
   
    terms<-c("ti(doy)","ti(date_dec)","ti(doy,date_dec)","intercept")
    
    getID=c('doy', 'date_dec', 'interaction')
    toPlot=which(grepl("parsInt_",names(data)))
    id1=which(getID==input$intVar1)
    id2=which(getID==input$intVar2)
    ggplot(data,aes(x = Date, y = log(chl)))+geom_point()+
      geom_line(aes_string(x="Date",y = names(data)[toPlot[id1]], color = "input$intVar1"),lwd=1)+
      geom_line(aes_string(x="Date",y = names(data)[toPlot[id2]], color = "input$intVar2"),lwd=1)+
      scale_colour_manual(name = '',
                          labels =c('red'=input$intVar1,"dodgerblue"=input$intVar2)
                          ,values=c("red", "dodgerblue")
      ) +
      ggtitle(paste(stationNames[index],"Component-Wise Predictions Interaction Model \n Intercept added in order to center components",sep=" "))+scale_x_date(limits = dt_rng)+
      ylab("ln(chl a) ")+xlab("Date")+ylim(input$ylim34L,input$ylim34U)
    
  }, height = 250, width = 1200)
  
  output$nestedPlotSpat <- renderPlot({
    
    # inputs
    
    dt_rng <- input$dt_rng
    stat <- input$stat
    index=which(stationNames==stat)
    data=perStationPredVal[[index]]
    # data<-subset(allData,Station==stat)
    # indices<-which(allData$Station==stat)
    # data$Date=as.Date(as.character(data$Date))
    
    if(input$spatMod=="spatIntercept"){
      ## maybe just boost everything by the intercept so that it lines up, no need to do two plot?
      toPlot=which(grepl("spatM1_",names(data)))
      int=data[,toPlot[3]][1]
      ggplot(data,aes(x = Date, y = log(chl)))+geom_point()+
   
        geom_line(aes_string(x="Date",y = names(data)[toPlot[1]], color = shQuote('ti(doy)')), lwd=1)+
        geom_line(aes_string(x="Date",y=names(data)[toPlot[2]], color = shQuote('ti(date_dec)')), lwd=1)+
        geom_line(aes_string(x="Date",y = names(data)[toPlot[3]], color = shQuote('station')), lwd=1)+
        
        scale_colour_manual(name = '',
                            labels =c('red'="station","dodgerblue"="ti(doy)",
                                      "forestgreen"="ti(date_dec)"),values=c("red","dodgerblue","forestgreen")
        ) +
        ggtitle(paste(stat,"Component-Wise Predictions",input$spatMod ,"Model \n Intercept added to each piece to center. \n Intercept =",round(int,2),sep=" "))+scale_x_date(limits = dt_rng)+
        ylab("ln(chl a) ")+xlab("Date")+ylim(input$ylim34L,input$ylim34U)
      
    }else if(input$spatMod=="spatDate_Dec"){
      
      toPlot=which(grepl("spatM2_",names(data)))
      
      ggplot(data,aes(x = Date, y = log(chl)))+geom_point()+
      
        geom_line(aes_string(x="Date",y = names(data)[toPlot[1]], color = shQuote('ti(doy)')), lwd=1)+
        geom_line(aes_string(x="Date",y=names(data)[toPlot[2]], color = shQuote('ti(date_dec,Station)')), lwd=1)+
        geom_line(aes_string(x="Date",y = names(data)[toPlot[3]], color = shQuote('station')), lwd=1)+
        
        scale_colour_manual(name = '',
                            labels =c('red'="station","dodgerblue"="ti(doy)",
                                      "forestgreen"="ti(date_dec,Station)"),values=c("red","dodgerblue","forestgreen")
        ) +
        ggtitle(paste(stat,"Component-Wise Predictions",input$spatMod ,"Model \n Intercept added to each piece to center. \n Intercept =",round(int,2),sep=" "))+scale_x_date(limits = dt_rng)+
        ylab("ln(chl a) ")+xlab("Date")+ylim(input$ylim34L,input$ylim34U)
      
    }else if(input$spatMod=="spatDOY"){
      toPlot=which(grepl("spatM3_",names(data)))
      
      ggplot(data,aes(x = Date, y = log(chl)))+geom_point()+
        geom_line(aes_string(x="Date",y =  names(data)[toPlot[1]], color = shQuote('ti(doy_Station)')), lwd=1)+
        geom_line(aes_string(x="Date",y= names(data)[toPlot[2]], color = shQuote('ti(date_dec,Station)')), lwd=1)+
        geom_line(aes_string(x="Date",y =  names(data)[toPlot[3]], color = shQuote('station')), lwd=1)+
        
        scale_colour_manual(name = '',
                            labels =c('red'="station","dodgerblue"="ti(doy_Station)",
                                      "forestgreen"="ti(date_dec,Station)"),values=c("red","dodgerblue","forestgreen")
        ) +
        ggtitle(paste(stat,"Component-Wise Predictions",input$spatMod ,"Model \n Intercept added to each piece to center. \n Intercept =",round(int,2),sep=" "))+scale_x_date(limits = dt_rng)+
        ylab("ln(chl a) ")+xlab("Date")+ylim(input$ylim34L,input$ylim34U)
      
    }else if(input$spatMod=="spatinteraction"){
      toPlot=which(grepl("spatM4_",names(data)))
      
      ggplot(data,aes(x = Date, y = log(chl)))+geom_point()+
        geom_line(aes_string(x="Date",y = names(data)[toPlot[1]], color = shQuote('ti(doy)')), lwd=1)+
        geom_line(aes_string(x="Date",y=names(data)[toPlot[2]], color = shQuote('ti(date_dec,Station)')), lwd=1)+
        geom_line(aes_string(x="Date",y=names(data)[toPlot[3]], color = shQuote('ti(date_dec,doy_Station)')), lwd=1)+
        geom_line(aes_string(x="Date",y = names(data)[toPlot[4]], color = shQuote('station')), lwd=1)+
        
        scale_colour_manual(name = '',
                            labels =c('red'="station","dodgerblue"="ti(doy)",
                                      "forestgreen"="ti(date_dec,Station)",
                                      "purple"="ti(date_dec,doy_Station)"),values=c("red","dodgerblue",
                                                                                    "forestgreen","purple")
        ) +
        ggtitle(paste(stat,"Component-Wise Predictions",input$spatMod ,"Model  \n Intercept added to each piece to center. \n Intercept =",round(int,2)))+scale_x_date(limits = dt_rng)+
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
    index=which(stationNames==stat)
    
    # data
    data=perStationPredVal[[index]]
    
    
    terms<-c("ti(doy)","ti(date_dec)","ti(chl)","intercept")
    toPlot=which(grepl("chlFlow_",names(data)))
    
    ggplot(data,aes(x = Date, y = log(chl)))+geom_point()+
      geom_line(aes_string(x="Date",y = names(data)[toPlot[1]], color = shQuote('ti(doy)')), lwd=1)+
      geom_line(aes_string(x="Date",y=names(data)[toPlot[2]], color = shQuote('ti(date_dec)')), lwd=1)+
      geom_line(aes_string(x="Date",y = names(data)[toPlot[3]], color = shQuote('ti(chl)')), lwd=1)+
      geom_line(aes_string(x="Date",y = names(data)[toPlot[4]], color = shQuote('intercept')), lwd=1)+
      scale_colour_manual(name = '',
                          labels =c('red'=terms[1],"dodgerblue"=terms[2],
                                    "forestgreen"=terms[3],"purple"=terms[4]),values=c("red",
                                                                                       "dodgerblue","forestgreen","purple")
      ) +
      ggtitle(paste(names(perStationAdd)[index],"Component-Wise Predictions Chl Flow Model",sep=" "))+scale_x_date(limits = dt_rng)+
      ylab("ln(chl a) ")+xlab("Date")+ylim(input$ylim34L,input$ylim34U)
    
    
  }, height = 250, width = 1200)
  
  output$nestedPlotFlow <- renderPlot({
    
    # inputs
    
    dt_rng <- input$dt_rng
    stat <- input$stat
    index=which(stationNames==stat)
    
    # data
    data=perStationPredVal[[index]]
    data$TOT=0.028316847*data$TOT
   
    
   
    terms<-c("ti(doy)","ti(date_dec)","ti(tot)","intercept")
    toPlot=which(grepl("totFlow_",names(data)))
    
    ggplot(data,aes(x = Date, y = log(chl)))+geom_point()+
      geom_line(aes_string(x="Date",y = names(data)[toPlot[1]], color = shQuote('ti(doy)')), lwd=1)+
      geom_line(aes_string(x="Date",y=names(data)[toPlot[2]], color = shQuote('ti(date_dec)')), lwd=1)+
      geom_line(aes_string(x="Date",y = names(data)[toPlot[3]], color = shQuote('ti(tot)')), lwd=1)+
      geom_line(aes_string(x="Date",y = names(data)[toPlot[4]], color = shQuote('intercept')), lwd=1)+
      scale_colour_manual(name = '',
                          labels =c('red'=terms[1],"dodgerblue"=terms[2],
                                    "forestgreen"=terms[3],"purple"=terms[4]),values=c("red",
                                                                                       "dodgerblue","forestgreen","purple")
      ) +
      ggtitle(paste(stationNames[index],"Component-Wise Predictions Total Flow Model",sep=" "))+scale_x_date(limits = dt_rng)+
      ylab("ln(chl a) ")+xlab("Date")+ylim(input$ylim34L,input$ylim34U)
    
    
  }, height = 250, width = 1200)
  
  output$rmseSpatPlotA <- renderPlot({
    
    RMSE=lapply(perStationPredVal[wholeSeries],getSummaryRMSE,input$model)
    RMSE=as.data.frame(do.call(cbind,RMSE))
    
    test=as.data.frame(t(RMSE))
    spatialPlotRMSE_pieces(test,names(test)[2:7],input$model,input$outlier)
  }, height = 800, width = 800)
  
  output$rmseSpatPlotS <- renderPlot({
    
    RMSE=lapply(perStationPredVal[wholeSeries],getSummaryRMSE,input$model)
    RMSE=as.data.frame(do.call(cbind,RMSE))
    
    test=as.data.frame(t(RMSE))
    spatialPlotRMSE_pieces(test,names(test)[8:11],input$model,input$outlier)
  }, height = 800, width = 800)
  
})