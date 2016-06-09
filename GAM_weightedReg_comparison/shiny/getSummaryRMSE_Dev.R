## stay on log scale
## like table 2 Beck_and_Murphy_EMA
getSummaryRMSE<-function(data,model){
  data=data[!is.na(data$res),]
  data=data[!is.na(data$flo),]
  trueVal=data$res
  predVal=model$fitted.values
  rmse=sqrt(sum((trueVal-predVal)^2))
  data$month=as.numeric(strftime(data$date, '%m'))
  data$year=as.numeric(strftime(data$date, '%Y'))
  
  annual1=subset(data,year<1983 & year>=1976)
  annual1I=which(data$year<1983 & data$year>=1976)
  annual1P=predVal[annual1I]
  
  annual2=subset(data,year<1990 & year>=1983)
  annual2I=which(data$year<1990 & data$year>=1983)
  annual2P=predVal[annual2I]
  
  annual3=subset(data,year<1997 & year>=1990)
  annual3I=which(data$year<1997 & data$year>=1990)
  annual3P=predVal[annual3I]
  
  annual4=subset(data,year<2004 & year>=1997)
  annual4I=which(data$year<2004 & data$year>=1997)
  annual4P=predVal[annual4I]
  
  annual5=subset(data,year>=2004) ## has 2 extra years
  annual5I=which( data$year>=2004)
  annual5P=predVal[annual5I]
  
  rmseA1=sqrt(sum((annual1$res-annual1P)^2))
  rmseA2=sqrt(sum((annual2$res-annual2P)^2))
  rmseA3=sqrt(sum((annual3$res-annual3P)^2))
  rmseA4=sqrt(sum((annual4$res-annual4P)^2))
  rmseA5=sqrt(sum((annual5$res-annual5P)^2))
  
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
  
  rmseS1=sqrt(sum((seasonal1$res-seasonal1P)^2))
  rmseS2=sqrt(sum((seasonal2$res-seasonal2P)^2))
  rmseS3=sqrt(sum((seasonal3$res-seasonal3P)^2))
  rmseS4=sqrt(sum((seasonal4$res-seasonal4P)^2))
  
  flow1=subset(data,flo<quantile(data$flo,.25))
  flow1I=which(data$flo<quantile(data$flo,0.25))
  flow1P=predVal[flow1I]
  
  flow2=subset(data,flo>=quantile(data$flo,.25) & flow<quantile(data$flo,0.5))
  flow2I=which(data$flo>=quantile(data$flo,0.25)& data$flow<quantile(data$flo,0.5))
  flow2P=predVal[flow2I]
  
  flow3=subset(data,flo>=quantile(data$flo,.5) & flow<quantile(data$flo,0.75))
  flow3I=which(data$flo>=quantile(data$flo,0.5)& data$flow<quantile(data$flo,0.75))
  flow3P=predVal[flow3I]
  
  flow4=subset(data,flo>=quantile(data$flo,.75) )
  flow4I=which(data$flo>=quantile(data$flo,0.75))
  flow4P=predVal[flow4I]
  
  rmseF1=sqrt(sum((flow1$res-flow1P)^2))
  rmseF2=sqrt(sum((flow2$res-flow2P)^2))
  rmseF3=sqrt(sum((flow3$res-flow3P)^2))
  rmseF4=sqrt(sum((flow4$res-flow4P)^2))
  
  return(list(all=rmse,annual1=rmseA1,annual2=rmseA2,annual3=rmseA3,annual4=rmseA4,
              seasonal1=rmseS1, seasonal2=rmseS2, seasonal3=rmseS3, seasonal4=rmseS4,
              flow1=rmseF1,flow2=rmseF2,flow3=rmseF3,flow4=rmseF4))
}

## stay on log scale
getSummaryDeviance<-function(data,model){
  data=data[!is.na(data$res),]
  data=data[!is.na(data$flo),]
  dev=sum(model$residuals^2)
  data$month=as.numeric(strftime(data$date, '%m'))
  data$year=as.numeric(strftime(data$date, '%Y'))
  
  
  annual1I=which(data$year<1983 & data$year>=1976)
  annual1R=model$residuals[annual1I]
  
  annual2I=which(data$year<1990 & data$year>=1983)
  annual2R=model$residuals[annual2I]
  
  annual3I=which(data$year<1997 & data$year>=1990)
  annual3R=model$residuals[annual3I]
  
  annual4I=which(data$year<2004 & data$year>=1997)
  annual4R=model$residuals[annual4I]
  
  ## has 2 extra years
  annual5I=which( data$year>=2004)
  annual5R=model$residuals[annual5I]
  
  annualDev1=sum(annual1R^2)
  annualDev2=sum(annual2R^2)
  annualDev3=sum(annual3R^2)
  annualDev4=sum(annual4R^2)
  
  seasonal1I=which(data$month %in% c(1:3))
  seasonal1R=model$residuals[seasonal1I]
  
  seasonal2I=which(data$month %in% c(4:6))
  seasonal2R=model$residuals[seasonal2I]
  
  seasonal3I=which(data$month %in% c(7:9))
  seasonal3R=model$residuals[seasonal3I]
  
  seasonal4I=which(data$month %in% c(10:12))
  seasonal4R=model$residuals[seasonal4I]
  
  seasonalDev1=sum(seasonal1R^2)
  seasonalDev2=sum(seasonal2R^2)
  seasonalDev3=sum(seasonal3R^2)
  seasonalDev4=sum(seasonal4R^2)
  
  flow1I=which(data$flo<quantile(data$flo,0.25))
  flow1R=model$residuals[flow1I]
  
  flow2I=which(data$flo>=quantile(data$flo,0.25)& data$flow<quantile(data$flo,0.5))
  flow2R=model$residuals[flow2I]
  
  flow3I=which(data$flo>=quantile(data$flo,0.5)& data$flow<quantile(data$flo,0.75))
  flow3R=model$residuals[flow3I]
   
  flow4I=which(data$flo>=quantile(data$flo,0.75))
  flow4R=model$residuals[flow4I]
  
  flowDev1=sum(flow1R^2)
  flowDev2=sum(flow2R^2)
  flowDev3=sum(flow3R^2)
  flowDev4=sum(flow4R^2)
  
  return(list(all=dev,annual1=annualDev1,annual2=annualDev2,annual3=annualDev3,annual4=annualDev4,
              seasonal1=seasonalDev1, seasonal2=seasonalDev2, seasonal3=seasonalDev3, seasonal4=seasonalDev4,
              flow1=flowDev1,flow2=flowDev2,flow3=flowDev3,flow4=flowDev4))
}