setwd("~/Desktop/sfei")

allData<-read.csv("allData.csv",stringsAsFactors=F)

forMap=allData[,c("Longitude","Latitude","Station")]
forMap=unique(forMap)

require(gridExtra)
require(ggplot2)


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


i=8
RMSE=lapply(perStationPredVal[wholeSeries],getSummaryRMSE,models[i])
RMSE=as.data.frame(do.call(cbind,RMSE))

test=as.data.frame(t(RMSE))

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
  
  g4<-ggplot(testMerge,aes(x = "Longitude", y = "Latitude",colour=breakdownPieces[4],cex=2))+geom_point()+
    ggtitle(paste(modName,breakdownPieces[4]))+scale_size(guide=F)
  
  grid.arrange(g1,g2,g3,g4)
}else{
  g1<-ggplot(testMerge,aes_string(x = "Longitude", y = "Latitude",colour=breakdownPieces[1],cex=2))+geom_point()+
    ggtitle(paste(modName,breakdownPieces[1]))+scale_size(guide=F)
  
  g2<-ggplot(testMerge,aes_string(x = "Longitude", y = "Latitude",colour=breakdownPieces[2],cex=2))+geom_point()+
    ggtitle(paste(modName,breakdownPieces[2]))+scale_size(guide=F)
  
  g3<-ggplot(testMerge,aes_string(x = "Longitude", y = "Latitude",colour=breakdownPieces[3],cex=2))+geom_point()+
    ggtitle(paste(modName,breakdownPieces[3]))+scale_size(guide=F)
  
  g4<-ggplot(testMerge,aes(x = "Longitude", y = "Latitude",colour=breakdownPieces[4],cex=2))+geom_point()+
    ggtitle(paste(modName,breakdownPieces[4]))+scale_size(guide=F)
  
  g5<-ggplot(testMerge,aes_string(x = "Longitude", y = "Latitude",colour=breakdownPieces[5],cex=2))+geom_point()+
    ggtitle(paste(modName,breakdownPieces[5]))+scale_size(guide=F)
  
  g6<-ggplot(testMerge,aes(x = "Longitude", y = "Latitude",colour=breakdownPieces[6],cex=2))+geom_point()+
    ggtitle(paste(modName,breakdownPieces[6]))+scale_size(guide=F)
  
  grid.arrange(g1,g2,g3,g4,g5,g6)
}
}

spatialPlotRMSE_pieces(test,names(test)[2:7],models[i])
