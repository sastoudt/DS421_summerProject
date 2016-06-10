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
  
  seasonal1=subset(data,month %in% c(1:3))
  seasonal1I=which(data$month %in% c(1:3))
  minDate=which.min(seasonal1$date)
  maxDate=which.max(seasonal1$date)
  seasonal1M=mean(model$fitted.values[seasonal1I])
  seasonal1PC=(model$fitted.values[which(data$date==maxDate)]-
               model$fitted.values[which(data$date==minDate)])/
    model$fitted.values[which(data$date==minDate)]
  
  seasonal2=subset(data,month %in% c(4:6))
  seasonal2I=which(data$month %in% c(4:6))
  minDate=which.min(seasonal2$date)
  maxDate=which.max(seasonal2$date)
  seasonal2M=mean(model$fitted.values[seasonal2I])
  seasonal2PC=(model$fitted.values[which(data$date==maxDate)]-
                 model$fitted.values[which(data$date==minDate)])/
    model$fitted.values[which(data$date==minDate)]
  
  seasonal3=subset(data,month %in% c(7:9))
  seasonal3I=which(data$month %in% c(7:9))
  minDate=which.min(seasonal3$date)
  maxDate=which.max(seasonal3$date)
  seasonal3M=mean(model$fitted.values[seasonal3I])
  seasonal3PC=(model$fitted.values[which(data$date==maxDate)]-
                 model$fitted.values[which(data$date==minDate)])/
    model$fitted.values[which(data$date==minDate)]
  
  seasonal4=subset(data,month %in% c(10:12))
  seasonal4I=which(data$month %in% c(10:12))
  minDate=which.min(seasonal4$date)
  maxDate=which.max(seasonal4$date)
  seasonal4M=mean(model$fitted.values[seasonal4I])
  seasonal4PC=(model$fitted.values[which(data$date==maxDate)]-
                 model$fitted.values[which(data$date==minDate)])/
    model$fitted.values[which(data$date==minDate)]
  
  flow1=subset(data,flo<quantile(data$flo,.25))
  flow1I=which(data$flo<quantile(data$flo,0.25))
  minDate=which.min(flow1$date)
  maxDate=which.max(flow1$date)
  flow1M=mean(model$fitted.values[flow1I])
  flow1PC=(model$fitted.values[which(data$date==maxDate)]-
                 model$fitted.values[which(data$date==minDate)])/
    model$fitted.values[which(data$date==minDate)]
  
  flow2=subset(data,flo>=quantile(data$flo,.25) & flow<quantile(data$flo,0.5))
  flow2I=which(data$flo>=quantile(data$flo,0.25)& data$flow<quantile(data$flo,0.5))
  minDate=which.min(flow2$date)
  maxDate=which.max(flow2$date)
  flow2M=mean(model$fitted.values[flow2I])
  flow2PC=(model$fitted.values[which(data$date==maxDate)]-
              model$fitted.values[which(data$date==minDate)])/
    model$fitted.values[which(data$date==minDate)]
  
  flow3=subset(data,flo>=quantile(data$flo,.5) & flow<quantile(data$flo,0.75))
  flow3I=which(data$flo>=quantile(data$flo,0.5)& data$flow<quantile(data$flo,0.75))
  minDate=which.min(flow3$date)
  maxDate=which.max(flow3$date)
  flow3M=mean(model$fitted.values[flow3I])
  flow3PC=(model$fitted.values[which(data$date==maxDate)]-
              model$fitted.values[which(data$date==minDate)])/
    model$fitted.values[which(data$date==minDate)]
  
  flow4=subset(data,flo>=quantile(data$flo,.75) )
  flow4I=which(data$flo>=quantile(data$flo,0.75))
  minDate=which.min(flow4$date)
  maxDate=which.max(flow4$date)
  flow4M=mean(model$fitted.values[flow4I])
  flow4PC=(model$fitted.values[which(data$date==maxDate)]-
              model$fitted.values[which(data$date==minDate)])/
    model$fitted.values[which(data$date==minDate)]
  
  avg=rbind(avgOverall,annual1M,annual2M,annual3M,annual4M,annual5M,
            seasonal1M,seasonal2M,seasonal3M,seasonal4M,
            flow1M,flow2M,flow3M,flow4M)
  pc=rbind(percentChange,annual1PC,annual2PC,annual3PC,annual4PC,annual5PC,
           seasonal1PC,seasonal2PC,seasonal3PC,seasonal4PC,
           flow1PC,flow2PC,flow3PC,flow4PC)
  return(list(avg=avg,pc=pc))
  
  # return(list(allM=avgOverall,allPC=percentChange,annual1M=annual1M,annual1PC=annual1PC,
  #             annual2M=annual2M,annual2PC=annual2PC, annual3M=annual3M, 
  #             annual3PC=annual3PC, annual4M=annual4M, annual4PC=annual4PC, annual5M=annual5M,
  #             annual5PC=annual5PC,
  #             seasonal1M=seasonal1M,seasonal1PC=seasonal1PC, seasonal2M=seasonal2M,
  #             seasonal2PC=seasonal2PC,
  #             seasonal3M=seasonal3M, seasonal3PC=seasonal3PC, seasonal4M=seasonal4M,
  #             seasonal4PC=seasonal4PC,
  #             flow1M=flow1M, flow1PC=flow1PC, flow2M=flow2M, flow2PC=flow2PC, 
  #             flow3M=flow3M, flow3PC=flow3PC,
  #             flow4M=flow4M, flow4PC=flow4PC))
}
