## flow normalized comparisons
## like Tables 3 and 4 in Beck_and_Murphy_EMA

getFlowNormalizedSummary<-function(data,model){
  data=data[!is.na(data$res),]
  data=data[!is.na(data$flo),]
  
  avgOverall=mean(model$fitted.values)
  minDate=which.min(data$date)
  maxDate=which.max(data$date)
  percentChange=(model$fitted.values[maxDate]-model$fitted.values[minDate])/model$fitted.values[minDate]
  
  data$month=as.numeric(strftime(data$date, '%m'))
  data$year=as.numeric(strftime(data$date, '%Y'))
  
  annual1=subset(data,year<1983 & year>=1976)
  annual1I=which(data$year<1983 & data$year>=1976)
  minDate=which.min(annual1$date)
  maxDate=which.max(annual1$date)
  annual1M=mean(model$fitted.values[annual1I])
  annual1PC=(model$fitted.values[which(data$date==maxDate)]-
               model$fitted.values[which(data$date==minDate)])/
                model$fitted.values[which(data$date==minDate)]
  
  annual2=subset(data,year<1990 & year>=1983)
  annual2I=which(data$year<1990 & data$year>=1983)
  minDate=which.min(annual2$date)
  maxDate=which.max(annual2$date)
  annual2M=mean(model$fitted.values[annual2I])
  annual2PC=(model$fitted.values[which(data$date==maxDate)]-
               model$fitted.values[which(data$date==minDate)])/
    model$fitted.values[which(data$date==minDate)]
  
  annual3=subset(data,year<1997 & year>=1990)
  annual3I=which(data$year<1997 & data$year>=1990)
  minDate=which.min(annual3$date)
  maxDate=which.max(annual3$date)
  annual3M=mean(model$fitted.values[annual3I])
  annual3PC=(model$fitted.values[which(data$date==maxDate)]-
               model$fitted.values[which(data$date==minDate)])/
    model$fitted.values[which(data$date==minDate)]
  
  annual4=subset(data,year<2004 & year>=1997)
  annual4I=which(data$year<2004 & data$year>=1997)
  minDate=which.min(annual4$date)
  maxDate=which.max(annual4$date)
  annual4M=mean(model$fitted.values[annual4I])
  annual4PC=(model$fitted.values[which(data$date==maxDate)]-
               model$fitted.values[which(data$date==minDate)])/
    model$fitted.values[which(data$date==minDate)]
  
  annual5=subset(data,year>=2004) ## has 2 extra years
  annual5I=which( data$year>=2004)
  minDate=which.min(annual1$date)
  maxDate=which.max(annual1$date)
  annual5M=mean(model$fitted.values[annual5I])
  annual5PC=(model$fitted.values[which(data$date==maxDate)]-
               model$fitted.values[which(data$date==minDate)])/
    model$fitted.values[which(data$date==minDate)]
  
  #start here
  
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
