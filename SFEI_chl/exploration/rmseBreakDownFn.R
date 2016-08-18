setwd("~/Desktop/sfei")
load("perStationPredVal.Rda")
load("perStation.Rda") # has names
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



models=c("predPars","predFull","predInt","predSpat1","predSpat2","predSpat3","predSpat4",
         "predSpat3Pheo","predSpat3Tn","chlPred","flowPred")
setwd("~/Desktop/DS421_summerProject/SFEI_chl/compareModels")
require(xtable)

for( i in models){
  
  RMSE=lapply(perStationPredVal[wholeSeries],getSummaryRMSE,i)
  RMSE=as.data.frame(do.call(cbind,RMSE))
  
  names(RMSE)=names(perStation)[wholeSeries]
  print(xtable(RMSE,caption=i),float=T,type="latex",floating.environment="table",table.placement="H",file="compareRMSEbreakdown.tex",append=T)
  print(i)
}

i=11
RMSE=lapply(perStationPredVal[wholeSeries],getSummaryRMSE,models[i])
RMSE=as.data.frame(do.call(cbind,RMSE))

table(row.names(RMSE)[unlist(apply(RMSE,2,which.min))])

## ok this is gross but I'm just going to switch the order of the parameters so that I can do 
## what I want to do

getSummaryRMSE2<-function(namePred,data){
  
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


RMSE=lapply(models,getSummaryRMSE2,perStationPredVal[[1]])

setwd("~/Desktop/DS421_summerProject/SFEI_chl/compareModels")
for( i in wholeSeries){
  
  RMSE=lapply(models,getSummaryRMSE2,perStationPredVal[[i]])
  RMSE=as.data.frame(do.call(cbind,RMSE))
  
  names(RMSE)=models
  print(xtable(RMSE,caption=names(perStation)[i]),float=T,type="latex",floating.environment="table",table.placement="H",file="compareRMSEbreakdown2.tex",append=T,size="\\fontsize{2.5pt}{4pt}\\selectfont")
  print(i)
} ## more intutive way to compare, keep the other one for later
