## stay on log scale
## like table 2 Beck_and_Murphy_EMA
getSummaryRMSE<-function(data,model){
  data=data[!is.nan(data$res),]
  data=data[!is.nan(data$flo),]
  trueVal=data$res ## need to pass this in
  predValGAM=data$gamPred
  predValWRTDS=data$wrtdsPred
  
  rmseGAM=sqrt(sum((trueVal-predValGAM)^2))
  rmseWRTDS=sqrt(sum((trueVal-predValWRTDS)^2,na.rm=T))
  
  data$month=as.numeric(strftime(data$date, '%m'))
  data$year=as.numeric(strftime(data$date, '%Y'))
  
  annual1=subset(data,year<1983 & year>=1976)
  annual1I=which(data$year<1983 & data$year>=1976)
  annual1PG=predValGAM[annual1I]
  annual1PW=predValWRTDS[annual1I]
  
  
  annual2=subset(data,year<1990 & year>=1983)
  annual2I=which(data$year<1990 & data$year>=1983)
  annual2PG=predValGAM[annual2I]
  annual2PW=predValWRTDS[annual2I]
  
  
  annual3=subset(data,year<1997 & year>=1990)
  annual3I=which(data$year<1997 & data$year>=1990)
  annual3PG=predValGAM[annual3I]
  annual3PW=predValWRTDS[annual3I]
  
  
  annual4=subset(data,year<2004 & year>=1997)
  annual4I=which(data$year<2004 & data$year>=1997)
  annual4PG=predValGAM[annual4I]
  annual4PW=predValWRTDS[annual4I]
  
  
  annual5=subset(data,year>=2004) ## has 2 extra years
  annual5I=which( data$year>=2004)
  annual5PG=predValGAM[annual5I]
  annual5PW=predValWRTDS[annual5I]
  
  
  rmseA1G=sqrt(sum((annual1$res-annual1PG)^2))
  rmseA2G=sqrt(sum((annual2$res-annual2PG)^2))
  rmseA3G=sqrt(sum((annual3$res-annual3PG)^2))
  rmseA4G=sqrt(sum((annual4$res-annual4PG)^2))
  rmseA5G=sqrt(sum((annual5$res-annual5PG)^2))
  
  rmseA1W=sqrt(sum((annual1$res-annual1PW)^2,na.rm=T))
  rmseA2W=sqrt(sum((annual2$res-annual2PW)^2,na.rm=T))
  rmseA3W=sqrt(sum((annual3$res-annual3PW)^2,na.rm=T))
  rmseA4W=sqrt(sum((annual4$res-annual4PW)^2,na.rm=T))
  rmseA5W=sqrt(sum((annual5$res-annual5PW)^2,na.rm=T))
  
  seasonal1=subset(data,month %in% c(1:3))
  seasonal1I=which(data$month %in% c(1:3))
  seasonal1PG=predValGAM[seasonal1I]
  seasonal1PW=predValWRTDS[seasonal1I]
  
  
  seasonal2=subset(data,month %in% c(4:6))
  seasonal2I=which(data$month %in% c(4:6))
  seasonal2PG=predValGAM[seasonal2I]
  seasonal2PW=predValWRTDS[seasonal2I]
  
  
  seasonal3=subset(data,month %in% c(7:9))
  seasonal3I=which(data$month %in% c(7:9))
  seasonal3PG=predValGAM[seasonal3I]
  seasonal3PW=predValWRTDS[seasonal3I]
  
  
  seasonal4=subset(data,month %in% c(10:12))
  seasonal4I=which(data$month %in% c(10:12))
  seasonal4PG=predValGAM[seasonal4I]
  seasonal4PW=predValWRTDS[seasonal4I]
  
  rmseS1G=sqrt(sum((seasonal1$res-seasonal1PG)^2))
  rmseS2G=sqrt(sum((seasonal2$res-seasonal2PG)^2))
  rmseS3G=sqrt(sum((seasonal3$res-seasonal3PG)^2))
  rmseS4G=sqrt(sum((seasonal4$res-seasonal4PG)^2))
  
  rmseS1W=sqrt(sum((seasonal1$res-seasonal1PW)^2,na.rm=T))
  rmseS2W=sqrt(sum((seasonal2$res-seasonal2PW)^2,na.rm=T))
  rmseS3W=sqrt(sum((seasonal3$res-seasonal3PW)^2,na.rm=T))
  rmseS4W=sqrt(sum((seasonal4$res-seasonal4PW)^2,na.rm=T))
  
  flow1=subset(data,flo<quantile(data$flo,.25))
  flow1I=which(data$flo<quantile(data$flo,0.25))
  flow1PG=predValGAM[flow1I]
  flow1PW=predValWRTDS[flow1I]
  
  flow2=subset(data,flo>=quantile(data$flo,.25) & flo<quantile(data$flo,0.5))
  flow2I=which(data$flo>=quantile(data$flo,0.25)& data$flo<quantile(data$flo,0.5))
  flow2PG=predValGAM[flow2I]
  flow2PW=predValWRTDS[flow2I]
  
  flow3=subset(data,flo>=quantile(data$flo,.5) & flo<quantile(data$flo,0.75))
  flow3I=which(data$flo>=quantile(data$flo,0.5)& data$flo<quantile(data$flo,0.75))
  flow3PG=predValGAM[flow3I]
  flow3PW=predValWRTDS[flow3I]
  
  flow4=subset(data,flo>=quantile(data$flo,.75) )
  flow4I=which(data$flo>=quantile(data$flo,0.75))
  flow4PG=predValGAM[flow4I]
  flow4PW=predValWRTDS[flow4I]
  
  rmseF1G=sqrt(sum((flow1$res-flow1PG)^2))
  rmseF2G=sqrt(sum((flow2$res-flow2PG)^2))
  rmseF3G=sqrt(sum((flow3$res-flow3PG)^2))
  rmseF4G=sqrt(sum((flow4$res-flow4PG)^2))
  
  rmseF1W=sqrt(sum((flow1$res-flow1PW)^2,na.rm=T))
  rmseF2W=sqrt(sum((flow2$res-flow2PW)^2,na.rm=T))
  rmseF3W=sqrt(sum((flow3$res-flow3PW)^2,na.rm=T))
  rmseF4W=sqrt(sum((flow4$res-flow4PW)^2,na.rm=T))
  
  rmseG=rbind(rmseGAM,rmseA1G,rmseA2G,rmseA3G,rmseA4G,rmseA5G,rmseS1G,rmseS2G,rmseS3G,rmseS4G,
             rmseF1G,rmseF2G,rmseF3G,rmseF4G)
  
  rmseW=rbind(rmseWRTDS,rmseA1W,rmseA2W,rmseA3W,rmseA4W,rmseA5W,rmseS1W,rmseS2W,rmseS3W,rmseS4W,
              rmseF1W,rmseF2W,rmseF3W,rmseF4W)
  return(cbind(rmseG,rmseW))
  # return(list(all=rmse,annual1=rmseA1,annual2=rmseA2,annual3=rmseA3,annual4=rmseA4,
  #             seasonal1=rmseS1, seasonal2=rmseS2, seasonal3=rmseS3, seasonal4=rmseS4,
  #             flow1=rmseF1,flow2=rmseF2,flow3=rmseF3,flow4=rmseF4))
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
  annualDev5=sum(annual5R^2)
  
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
  
  dev=rbind(dev,annualDev1,annualDev2,annualDev3,annualDev4,annualDev5,
            seasonalDev1,seasonalDev2,seasonalDev3,seasonalDev4,
            flowDev1,flowDev2,flowDev3,flowDev4)
  return(dev)
  
  # return(list(all=dev,annual1=annualDev1,annual2=annualDev2,annual3=annualDev3,annual4=annualDev4,annual5=annualDev5,
  #             seasonal1=seasonalDev1, seasonal2=seasonalDev2, seasonal3=seasonalDev3, seasonal4=seasonalDev4,
  #             flow1=flowDev1,flow2=flowDev2,flow3=flowDev3,flow4=flowDev4))
}