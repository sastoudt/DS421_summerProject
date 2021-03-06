## flow normalized comparisons
## like Tables 3 and 4 in Beck_and_Murphy_EMA

getFlowNormalizedSummary<-function(data,index){
  data=data[!is.nan(data$res),]
  data=data[!is.nan(data$flo),]
  
  #avgOverall=mean(model$fitted.values)
  avgOverallG=mean(data$gamPred)
  avgOverallW=mean(data$wrtdsPred,na.rm=T)
  
  data$month=as.numeric(strftime(data$date, '%m'))
  data$year=as.numeric(strftime(data$date, '%Y'))
  
  minDate=which.min(data$date)
  maxDate=which.max(data$date)
  
  firstG=mean(subset(data,year==min(year,na.rm=T))$gamPred,na.rm=T)
  secondG=mean(subset(data,year==(min(year,na.rm=T)+1))$gamPred,na.rm=T)
  thirdG=mean(subset(data,year==(min(year,na.rm=T)+2))$gamPred,na.rm=T)
  
  lastG=mean(subset(data,year==max(year,na.rm=T))$gamPred,na.rm=T)
  last2G=mean(subset(data,year==(max(year,na.rm=T)-1))$gamPred,na.rm=T)
  last3G=mean(subset(data,year==(max(year,na.rm=T)-2))$gamPred,na.rm=T)
  
  meanLG=mean(na.omit(c(firstG,secondG,thirdG)))
  meanUG=mean(na.omit(c(lastG,last2G,last3G)))
  
  
  firstW=mean(subset(data,year==min(year,na.rm=T))$wrtdsPred,na.rm=T)
  secondW=mean(subset(data,year==(min(year,na.rm=T)+1))$wrtdsPred,na.rm=T)
  thirdW=mean(subset(data,year==(min(year,na.rm=T)+2))$wrtdsPred,na.rm=T)
  
  lastW=mean(subset(data,year==max(year,na.rm=T))$wrtdsPred,na.rm=T)
  last2W=mean(subset(data,year==(max(year,na.rm=T)-1))$wrtdsPred,na.rm=T)
  last3W=mean(subset(data,year==(max(year,na.rm=T)-2))$wrtdsPred,na.rm=T)
  
  meanLW=mean(na.omit(c(firstW,secondW,thirdW)))
  meanUW=mean(na.omit(c(lastW,last2W,last3W)))
  
  #percentChange=(model$fitted.values[maxDate]-model$fitted.values[minDate])/model$fitted.values[minDate]
  #percentChangeG=(data$gamPred[maxDate]-data$gamPred[minDate])/data$gamPred[minDate]
  #percentChangeW=(data$wrtdsPred[maxDate]-data$wrtdsPred[minDate])/data$wrtdsPred[minDate]
  percentChangeG=(meanUG-meanLG)/meanLG
  percentChangeW=(meanUW-meanLW)/meanLW
  
  annual1=subset(data,year<1983 & year>=1976)
  annual1I=which(data$year<1983 & data$year>=1976)
  minDate=which.min(annual1$date)
  maxDate=which.max(annual1$date)
  
  #annual1M=mean(model$fitted.values[annual1I])
  #annual1PC=(model$fitted.values[which(data$date==maxDate)]-
  #model$fitted.values[which(data$date==minDate)])/
  #model$fitted.values[which(data$date==minDate)]
  annual1MG=mean(data$gamPred[annual1I])
  annual1MW=mean(data$wrtdsPred[annual1I],na.rm=T)
  
  
  firstG=mean(subset(data[annual1I,],year==min(year,na.rm=T))$gamPred,na.rm=T)
  secondG=mean(subset(data[annual1I,],year==(min(year,na.rm=T)+1))$gamPred,na.rm=T)
  thirdG=mean(subset(data[annual1I,],year==(min(year,na.rm=T)+2))$gamPred,na.rm=T)
  
  lastG=mean(subset(data[annual1I,],year==max(year,na.rm=T))$gamPred,na.rm=T)
  last2G=mean(subset(data[annual1I,],year==(max(year,na.rm=T)-1))$gamPred,na.rm=T)
  last3G=mean(subset(data[annual1I,],year==(max(year,na.rm=T)-2))$gamPred,na.rm=T)
  
  meanLG=mean(na.omit(c(firstG,secondG,thirdG)))
  meanUG=mean(na.omit(c(lastG,last2G,last3G)))
  
  
  firstW=mean(subset(data,year==min(year,na.rm=T))$wrtdsPred,na.rm=T)
  secondW=mean(subset(data,year==(min(year,na.rm=T)+1))$wrtdsPred,na.rm=T)
  thirdW=mean(subset(data,year==(min(year,na.rm=T)+2))$wrtdsPred,na.rm=T)
  
  lastW=mean(subset(data,year==max(year,na.rm=T))$wrtdsPred,na.rm=T)
  last2W=mean(subset(data,year==(max(year,na.rm=T)-1))$wrtdsPred,na.rm=T)
  last3W=mean(subset(data,year==(max(year,na.rm=T)-2))$wrtdsPred,na.rm=T)
  
  meanLW=mean(na.omit(c(firstW,secondW,thirdW)))
  meanUW=mean(na.omit(c(lastW,last2W,last3W)))
  
  
  annual1PCG=(meanUG-meanLG)/meanLG
  annual1PCW=(meanUW-meanLW)/meanLW
  
 
  # annual1PCG=(data$gamPred[maxDate]-
  #              data$gamPred[minDate])/
  #   data$gamPred[minDate]
  # annual1PCW=(data$wrtdsPred[maxDate]-
  #               data$wrtdsPred[minDate])/
  #   data$wrtdsPred[minDate]
  
  annual2=subset(data,year<1990 & year>=1983)
  annual2I=which(data$year<1990 & data$year>=1983)
  minDate=which.min(annual2$date)
  maxDate=which.max(annual2$date)
  annual2MG=mean(data$gamPred[annual2I])
  annual2MW=mean(data$wrtdsPred[annual2I],na.rm=T)
  
  firstG=mean(subset(data[annual2I,],year==min(year,na.rm=T))$gamPred,na.rm=T)
  secondG=mean(subset(data[annual2I,],year==(min(year,na.rm=T)+1))$gamPred,na.rm=T)
  thirdG=mean(subset(data[annual2I,],year==(min(year,na.rm=T)+2))$gamPred,na.rm=T)
  
  lastG=mean(subset(data[annual2I,],year==max(year,na.rm=T))$gamPred,na.rm=T)
  last2G=mean(subset(data[annual2I,],year==(max(year,na.rm=T)-1))$gamPred,na.rm=T)
  last3G=mean(subset(data[annual2I,],year==(max(year,na.rm=T)-2))$gamPred,na.rm=T)
  
  meanLG=mean(firstG,secondG,thirdG,na.rm=T)
  meanUG=mean(lastG,last2G,last3G,na.rm=T)
  
  
  firstW=mean(subset(data[annual2I,],year==min(year,na.rm=T))$wrtdsPred,na.rm=T)
  secondW=mean(subset(data[annual2I,],year==(min(year,na.rm=T)+1))$wrtdsPred,na.rm=T)
  thirdW=mean(subset(data[annual2I,],year==(min(year,na.rm=T)+2))$wrtdsPred,na.rm=T)
  
  lastW=mean(subset(data[annual2I,],year==max(year,na.rm=T))$wrtdsPred,na.rm=T)
  last2W=mean(subset(data[annual2I,],year==(max(year,na.rm=T)-1))$wrtdsPred,na.rm=T)
  last3W=mean(subset(data[annual2I,],year==(max(year,na.rm=T)-2))$wrtdsPred,na.rm=T)
  
  meanLW=mean(na.omit(c(firstW,secondW,thirdW)))
  meanUW=mean(na.omit(c(lastW,last2W,last3W)))
  
  
  annual2PCG=(meanUG-meanLG)/meanLG
  annual2PCW=(meanUW-meanLW)/meanLW
  
  
  # annual2PCG=(data$gamPred[maxDate]-
  #               data$gamPred[minDate])/
  #   data$gamPred[minDate]
  # annual2PCW=(data$wrtdsPred[maxDate]-
  #               data$wrtdsPred[minDate])/
  #   data$wrtdsPred[minDate]
  
  annual3=subset(data,year<1997 & year>=1990)
  annual3I=which(data$year<1997 & data$year>=1990)
  minDate=which.min(annual3$date)
  maxDate=which.max(annual3$date)
  annual3MG=mean(data$gamPred[annual3I])
  annual3MW=mean(data$wrtdsPred[annual3I],na.rm=T)
  
  firstG=mean(subset(data[annual3I,],year==min(year,na.rm=T))$gamPred,na.rm=T)
  secondG=mean(subset(data[annual3I,],year==(min(year,na.rm=T)+1))$gamPred,na.rm=T)
  thirdG=mean(subset(data[annual3I,],year==(min(year,na.rm=T)+2))$gamPred,na.rm=T)
  
  lastG=mean(subset(data[annual3I,],year==max(year,na.rm=T))$gamPred,na.rm=T)
  last2G=mean(subset(data[annual3I,],year==(max(year,na.rm=T)-1))$gamPred,na.rm=T)
  last3G=mean(subset(data[annual3I,],year==(max(year,na.rm=T)-2))$gamPred,na.rm=T)
  
  meanLG=mean(na.omit(c(firstG,secondG,thirdG)))
  meanUG=mean(na.omit(c(lastG,last2G,last3G)))
  
  
  firstW=mean(subset(data[annual3I,],year==min(year,na.rm=T))$wrtdsPred,na.rm=T)
  secondW=mean(subset(data[annual3I,],year==(min(year,na.rm=T)+1))$wrtdsPred,na.rm=T)
  thirdW=mean(subset(data[annual3I,],year==(min(year,na.rm=T)+2))$wrtdsPred,na.rm=T)
  
  lastW=mean(subset(data[annual3I,],year==max(year,na.rm=T))$wrtdsPred,na.rm=T)
  last2W=mean(subset(data[annual3I,],year==(max(year,na.rm=T)-1))$wrtdsPred,na.rm=T)
  last3W=mean(subset(data[annual3I,],year==(max(year,na.rm=T)-2))$wrtdsPred,na.rm=T)
  
  meanLW=mean(na.omit(c(firstW,secondW,thirdW)))
  meanUW=mean(na.omit(c(lastW,last2W,last3W)))
  
  
  annual3PCG=(meanUG-meanLG)/meanLG
  annual3PCW=(meanUW-meanLW)/meanLW
  
  
  # annual3PCG=(data$gamPred[maxDate]-
  #               data$gamPred[minDate])/
  #   data$gamPred[minDate]
  # annual3PCW=(data$wrtdsPred[maxDate]-
  #               data$wrtdsPred[minDate])/
  #   data$wrtdsPred[minDate]
  if(index %in% c(19,20,21)){ ## data missing in this period
    annual4MG=NA
    annual4MW=NA
    annual4PCG=NA
    annual4PCW=NA
  }else{
  annual4=subset(data,year<2004 & year>=1997)
  annual4I=which(data$year<2004 & data$year>=1997)
  minDate=which.min(annual4$date)
  maxDate=which.max(annual4$date)
  annual4MG=mean(data$gamPred[annual4I])
  annual4MW=mean(data$wrtdsPred[annual4I],na.rm=T)
  
  firstG=mean(subset(data[annual4I,],year==min(year,na.rm=T))$gamPred,na.rm=T)
  secondG=mean(subset(data[annual4I,],year==(min(year,na.rm=T)+1))$gamPred,na.rm=T)
  thirdG=mean(subset(data[annual4I,],year==(min(year,na.rm=T)+2))$gamPred,na.rm=T)
  
  lastG=mean(subset(data[annual4I,],year==max(year,na.rm=T))$gamPred,na.rm=T)
  last2G=mean(subset(data[annual4I,],year==(max(year,na.rm=T)-1))$gamPred,na.rm=T)
  last3G=mean(subset(data[annual4I,],year==(max(year,na.rm=T)-2))$gamPred,na.rm=T)
  
  meanLG=mean(na.omit(c(firstG,secondG,thirdG)))
  meanUG=mean(na.omit(c(lastG,last2G,last3G)))
  
  
  firstW=mean(subset(data[annual4I,],year==min(year,na.rm=T))$wrtdsPred,na.rm=T)
  secondW=mean(subset(data[annual4I,],year==(min(year,na.rm=T)+1))$wrtdsPred,na.rm=T)
  thirdW=mean(subset(data[annual4I,],year==(min(year,na.rm=T)+2))$wrtdsPred,na.rm=T)
  
  lastW=mean(subset(data[annual4I,],year==max(year,na.rm=T))$wrtdsPred,na.rm=T)
  last2W=mean(subset(data[annual4I,],year==(max(year,na.rm=T)-1))$wrtdsPred,na.rm=T)
  last3W=mean(subset(data[annual4I,],year==(max(year,na.rm=T)-2))$wrtdsPred,na.rm=T)
  
  meanLW=mean(na.omit(c(firstW,secondW,thirdW)))
  meanUW=mean(na.omit(c(lastW,last2W,last3W)))
  
  
 annual4PCG=(meanUG-meanLG)/meanLG
  annual4PCW=(meanUW-meanLW)/meanLW
  
  
  # annual4PCG=(data$gamPred[maxDate]-
  #               data$gamPred[minDate])/
  #   data$gamPred[minDate]
  # annual4PCW=(data$wrtdsPred[maxDate]-
  #               data$wrtdsPred[minDate])/
  #   data$wrtdsPred[minDate]
   }
  annual5=subset(data,year>=2004) ## has 2 extra years
  annual5I=which( data$year>=2004)
  minDate=which.min(annual1$date)
  maxDate=which.max(annual1$date)
  annual5MG=mean(data$gamPred[annual5I])
  annual5MW=mean(data$wrtdsPred[annual5I],na.rm=T)
  
  firstG=mean(subset(data[annual5I,],year==min(year,na.rm=T))$gamPred,na.rm=T)
  secondG=mean(subset(data[annual5I,],year==(min(year,na.rm=T)+1))$gamPred,na.rm=T)
  thirdG=mean(subset(data[annual5I,],year==(min(year,na.rm=T)+2))$gamPred,na.rm=T)
  
  lastG=mean(subset(data[annual5I,],year==max(year,na.rm=T))$gamPred,na.rm=T)
  last2G=mean(subset(data[annual5I,],year==(max(year,na.rm=T)-1))$gamPred,na.rm=T)
  last3G=mean(subset(data[annual5I,],year==(max(year,na.rm=T)-2))$gamPred,na.rm=T)
  
  meanLG=mean(na.omit(c(firstG,secondG,thirdG)))
  meanUG=mean(na.omit(c(lastG,last2G,last3G)))
  
  
  firstW=mean(subset(data[annual5I,],year==min(year,na.rm=T))$wrtdsPred,na.rm=T)
  secondW=mean(subset(data[annual5I,],year==(min(year,na.rm=T)+1))$wrtdsPred,na.rm=T)
  thirdW=mean(subset(data[annual5I,],year==(min(year,na.rm=T)+2))$wrtdsPred,na.rm=T)
  
  lastW=mean(subset(data[annual5I,],year==max(year,na.rm=T))$wrtdsPred,na.rm=T)
  last2W=mean(subset(data[annual5I,],year==(max(year,na.rm=T)-1))$wrtdsPred,na.rm=T)
  last3W=mean(subset(data[annual5I,],year==(max(year,na.rm=T)-2))$wrtdsPred,na.rm=T)
  
  meanLW=mean(na.omit(c(firstW,secondW,thirdW)))
  meanUW=mean(na.omit(c(lastW,last2W,last3W)))
  
  
  annual5PCG=(meanUG-meanLG)/meanLG
  annual5PCW=(meanUW-meanLW)/meanLW
  
  # annual5PCG=(data$gamPred[maxDate]-
  #               data$gamPred[minDate])/
  #   data$gamPred[minDate]
  # annual5PCW=(data$wrtdsPred[maxDate]-
  #               data$wrtdsPred[minDate])/
  #   data$wrtdsPred[minDate]
  # 
  seasonal1=subset(data,month %in% c(1:3))
  seasonal1I=which(data$month %in% c(1:3))
  minDate=which.min(seasonal1$date)
  maxDate=which.max(seasonal1$date)
  #seasonal1M=mean(model$fitted.values[seasonal1I])
  # seasonal1PC=(model$fitted.values[which(data$date==maxDate)]-
  #              model$fitted.values[which(data$date==minDate)])/
  #   model$fitted.values[which(data$date==minDate)]
  
  seasonal1MG=mean(data$gamPred[seasonal1I])
  seasonal1MW=mean(data$gamPred[seasonal1I],na.rm=T)
  
  firstG=mean(subset(data[seasonal1I,],year==min(year,na.rm=T))$gamPred,na.rm=T)
  secondG=mean(subset(data[seasonal1I,],year==(min(year,na.rm=T)+1))$gamPred,na.rm=T)
  thirdG=mean(subset(data[seasonal1I,],year==(min(year,na.rm=T)+2))$gamPred,na.rm=T)
  
  lastG=mean(subset(data[seasonal1I,],year==max(year,na.rm=T))$gamPred,na.rm=T)
  last2G=mean(subset(data[seasonal1I,],year==(max(year,na.rm=T)-1))$gamPred,na.rm=T)
  last3G=mean(subset(data[seasonal1I,],year==(max(year,na.rm=T)-2))$gamPred,na.rm=T)
  
  meanLG=mean(na.omit(c(firstG,secondG,thirdG)))
  meanUG=mean(na.omit(c(lastG,last2G,last3G)))
  
  firstW=mean(subset(data[seasonal1I,],year==min(year,na.rm=T))$wrtdsPred,na.rm=T)
  secondW=mean(subset(data[seasonal1I,],year==(min(year,na.rm=T)+1))$wrtdsPred,na.rm=T)
  thirdW=mean(subset(data[seasonal1I,],year==(min(year,na.rm=T)+2))$wrtdsPred,na.rm=T)
  
  lastW=mean(subset(data[seasonal1I,],year==max(year,na.rm=T))$wrtdsPred,na.rm=T)
  last2W=mean(subset(data[seasonal1I,],year==(max(year,na.rm=T)-1))$wrtdsPred,na.rm=T)
  last3W=mean(subset(data[seasonal1I,],year==(max(year,na.rm=T)-2))$wrtdsPred,na.rm=T)
  
  meanLW=mean(na.omit(c(firstW,secondW,thirdW)))
  meanUW=mean(na.omit(c(lastW,last2W,last3W)))
  
  
  seasonal1PCG=(meanUG-meanLG)/meanLG
  seasonal1PCW=(meanUW-meanLW)/meanLW
  
  
  # seasonal1PCG=(data$gamPred[maxDate]-
  #                 data$gamPred[minDate])/
  #   data$gamPred[minDate]
  # seasonal1PCW=(data$wrtdsPred[maxDate]-
  #                 data$wrtdsPred[minDate])/
  #   data$wrtdsPred[minDate]
  
  seasonal2=subset(data,month %in% c(4:6))
  seasonal2I=which(data$month %in% c(4:6))
  minDate=which.min(seasonal2$date)
  maxDate=which.max(seasonal2$date)
  seasonal2MG=mean(data$gamPred[seasonal2I])
  seasonal2MW=mean(data$gamPred[seasonal2I],na.rm=T)
  
  firstG=mean(subset(data[seasonal2I,],year==min(year,na.rm=T))$gamPred,na.rm=T)
  secondG=mean(subset(data[seasonal2I,],year==(min(year,na.rm=T)+1))$gamPred,na.rm=T)
  thirdG=mean(subset(data[seasonal2I,],year==(min(year,na.rm=T)+2))$gamPred,na.rm=T)
  
  lastG=mean(subset(data[seasonal2I,],year==max(year,na.rm=T))$gamPred,na.rm=T)
  last2G=mean(subset(data[seasonal2I,],year==(max(year,na.rm=T)-1))$gamPred,na.rm=T)
  last3G=mean(subset(data[seasonal2I,],year==(max(year,na.rm=T)-2))$gamPred,na.rm=T)
  
  meanLG=mean(na.omit(c(firstG,secondG,thirdG)))
  meanUG=mean(na.omit(c(lastG,last2G,last3G)))
  
  
  firstW=mean(subset(data[seasonal2I,],year==min(year,na.rm=T))$wrtdsPred,na.rm=T)
  secondW=mean(subset(data[seasonal2I,],year==(min(year,na.rm=T)+1))$wrtdsPred,na.rm=T)
  thirdW=mean(subset(data[seasonal2I,],year==(min(year,na.rm=T)+2))$wrtdsPred,na.rm=T)
  
  lastW=mean(subset(data[seasonal2I,],year==max(year,na.rm=T))$wrtdsPred,na.rm=T)
  last2W=mean(subset(data[seasonal2I,],year==(max(year,na.rm=T)-1))$wrtdsPred,na.rm=T)
  last3W=mean(subset(data[seasonal2I,],year==(max(year,na.rm=T)-2))$wrtdsPred,na.rm=T)
  
  meanLW=mean(na.omit(c(firstW,secondW,thirdW)))
  meanUW=mean(na.omit(c(lastW,last2W,last3W)))
  
  
  seasonal2PCG=(meanUG-meanLG)/meanLG
  seasonal2PCW=(meanUW-meanLW)/meanLW
  
  
  # seasonal2PCG=(data$gamPred[maxDate]-
  #                 data$gamPred[minDate])/
  #   data$gamPred[minDate]
  # seasonal2PCW=(data$wrtdsPred[maxDate]-
  #                 data$wrtdsPred[minDate])/
  #   data$wrtdsPred[minDate]
  
  seasonal3=subset(data,month %in% c(7:9))
  seasonal3I=which(data$month %in% c(7:9))
  minDate=which.min(seasonal3$date)
  maxDate=which.max(seasonal3$date)
  seasonal3MG=mean(data$gamPred[seasonal3I])
  seasonal3MW=mean(data$gamPred[seasonal3I],na.rm=T)
  
  firstG=mean(subset(data[seasonal3I,],year==min(year,na.rm=T))$gamPred,na.rm=T)
  secondG=mean(subset(data[seasonal3I,],year==(min(year,na.rm=T)+1))$gamPred,na.rm=T)
  thirdG=mean(subset(data[seasonal3I,],year==(min(year,na.rm=T)+2))$gamPred,na.rm=T)
  
  lastG=mean(subset(data[seasonal3I,],year==max(year,na.rm=T))$gamPred,na.rm=T)
  last2G=mean(subset(data[seasonal3I,],year==(max(year,na.rm=T)-1))$gamPred,na.rm=T)
  last3G=mean(subset(data[seasonal3I,],year==(max(year,na.rm=T)-2))$gamPred,na.rm=T)
  
  meanLG=mean(na.omit(c(firstG,secondG,thirdG)))
  meanUG=mean(na.omit(c(lastG,last2G,last3G)))
  
  
  firstW=mean(subset(data[seasonal3I,],year==min(year,na.rm=T))$wrtdsPred,na.rm=T)
  secondW=mean(subset(data[seasonal3I,],year==(min(year,na.rm=T)+1))$wrtdsPred,na.rm=T)
  thirdW=mean(subset(data[seasonal3I,],year==(min(year,na.rm=T)+2))$wrtdsPred,na.rm=T)
  
  lastW=mean(subset(data[seasonal3I,],year==max(year,na.rm=T))$wrtdsPred,na.rm=T)
  last2W=mean(subset(data[seasonal3I,],year==(max(year,na.rm=T)-1))$wrtdsPred,na.rm=T)
  last3W=mean(subset(data[seasonal3I,],year==(max(year,na.rm=T)-2))$wrtdsPred,na.rm=T)
  
  meanLW=mean(na.omit(c(firstW,secondW,thirdW)))
  meanUW=mean(na.omit(c(lastW,last2W,last3W)))
  
  
  seasonal3PCG=(meanUG-meanLG)/meanLG
  seasonal3PCW=(meanUW-meanLW)/meanLW
  
  
  
  # seasonal3PCG=(data$gamPred[maxDate]-
  #                 data$gamPred[minDate])/
  #   data$gamPred[minDate]
  # seasonal3PCW=(data$wrtdsPred[maxDate]-
  #                 data$wrtdsPred[minDate])/
  #   data$wrtdsPred[minDate]
  
  seasonal4=subset(data,month %in% c(10:12))
  seasonal4I=which(data$month %in% c(10:12))
  minDate=which.min(seasonal4$date)
  maxDate=which.max(seasonal4$date)
  seasonal4MG=mean(data$gamPred[seasonal4I])
  seasonal4MW=mean(data$gamPred[seasonal4I],na.rm=T)
 
  firstG=mean(subset(data[seasonal4I,],year==min(year,na.rm=T))$gamPred,na.rm=T)
  secondG=mean(subset(data[seasonal4I,],year==(min(year,na.rm=T)+1))$gamPred,na.rm=T)
  thirdG=mean(subset(data[seasonal4I,],year==(min(year,na.rm=T)+2))$gamPred,na.rm=T)
  
  lastG=mean(subset(data[seasonal4I,],year==max(year,na.rm=T))$gamPred,na.rm=T)
  last2G=mean(subset(data[seasonal4I,],year==(max(year,na.rm=T)-1))$gamPred,na.rm=T)
  last3G=mean(subset(data[seasonal4I,],year==(max(year,na.rm=T)-2))$gamPred,na.rm=T)
  
  meanLG=mean(na.omit(c(firstG,secondG,thirdG)))
  meanUG=mean(na.omit(c(lastG,last2G,last3G)))
  
  
  firstW=mean(subset(data[seasonal4I,],year==min(year,na.rm=T))$wrtdsPred,na.rm=T)
  secondW=mean(subset(data[seasonal4I,],year==(min(year,na.rm=T)+1))$wrtdsPred,na.rm=T)
  thirdW=mean(subset(data[seasonal4I,],year==(min(year,na.rm=T)+2))$wrtdsPred,na.rm=T)
  
  lastW=mean(subset(data[seasonal4I,],year==max(year,na.rm=T))$wrtdsPred,na.rm=T)
  last2W=mean(subset(data[seasonal4I,],year==(max(year,na.rm=T)-1))$wrtdsPred,na.rm=T)
  last3W=mean(subset(data[seasonal4I,],year==(max(year,na.rm=T)-2))$wrtdsPred,na.rm=T)
  
  meanLW=mean(na.omit(c(firstW,secondW,thirdW)))
  meanUW=mean(na.omit(c(lastW,last2W,last3W)))
  
  
  seasonal4PCG=(meanUG-meanLG)/meanLG
  seasonal4PCW=(meanUW-meanLW)/meanLW
  
  
  #  seasonal4PCG=(data$gamPred[maxDate]-
  #                 data$gamPred[minDate])/
  #   data$gamPred[minDate]
  # seasonal4PCW=(data$wrtdsPred[maxDate]-
  #                 data$wrtdsPred[minDate])/
  #   data$wrtdsPred[minDate]
  
  flow1=subset(data,flo<quantile(data$flo,.25))
  flow1I=which(data$flo<quantile(data$flo,0.25))
  minDate=which.min(flow1$date)
  maxDate=which.max(flow1$date)
 # flow1M=mean(model$fitted.values[flow1I])
#  flow1PC=(model$fitted.values[which(data$date==maxDate)]-
 #                model$fitted.values[which(data$date==minDate)])/
  #  model$fitted.values[which(data$date==minDate)]
  
  flow1MG=mean(data$gamPred[flow1I],na.rm=T)
  flow1MW=mean(data$wrtdsPred[flow1I],na.rm=T)
  
  ## does this still make sense for flow?
  
  firstG=mean(subset(data[flow1I,],year==min(year,na.rm=T))$gamPred,na.rm=T)
  secondG=mean(subset(data[flow1I,],year==(min(year,na.rm=T)+1))$gamPred,na.rm=T)
  thirdG=mean(subset(data[flow1I,],year==(min(year,na.rm=T)+2))$gamPred,na.rm=T)
  
  lastG=mean(subset(data[flow1I,],year==max(year,na.rm=T))$gamPred,na.rm=T)
  last2G=mean(subset(data[flow1I,],year==(max(year,na.rm=T)-1))$gamPred,na.rm=T)
  last3G=mean(subset(data[flow1I,],year==(max(year,na.rm=T)-2))$gamPred,na.rm=T)
  
  meanLG=mean(na.omit(c(firstG,secondG,thirdG)))
  meanUG=mean(na.omit(c(lastG,last2G,last3G)))
  
  
  firstW=mean(subset(data[flow1I,],year==min(year,na.rm=T))$wrtdsPred,na.rm=T)
  secondW=mean(subset(data[flow1I,],year==(min(year,na.rm=T)+1))$wrtdsPred,na.rm=T)
  thirdW=mean(subset(data[flow1I,],year==(min(year,na.rm=T)+2))$wrtdsPred,na.rm=T)
  
  lastW=mean(subset(data[flow1I,],year==max(year,na.rm=T))$wrtdsPred,na.rm=T)
  last2W=mean(subset(data[flow1I,],year==(max(year,na.rm=T)-1))$wrtdsPred,na.rm=T)
  last3W=mean(subset(data[flow1I,],year==(max(year,na.rm=T)-2))$wrtdsPred,na.rm=T)
  
  meanLW=mean(na.omit(c(firstW,secondW,thirdW)))
  meanUW=mean(na.omit(c(lastW,last2W,last3W)))
  
  
  flow1PCG=(meanUG-meanLG)/meanLG
  flow1PCW=(meanUW-meanLW)/meanLW
  
  # flow1PCG=(data$gamPred[maxDate]-
  #             data$gamPred[minDate])/
  #   data$gamPred[minDate]
  # flow1PCW=(data$wrtdsPred[maxDate]-
  #             data$wrtdsPred[minDate])/
  #   data$wrtdsPred[minDate]
  
  flow2=subset(data,flo>=quantile(data$flo,.25) & data$flo<quantile(data$flo,0.5))
  flow2I=which(data$flo>=quantile(data$flo,0.25)& data$flo<quantile(data$flo,0.5))
  minDate=which.min(flow2$date)
  maxDate=which.max(flow2$date)
  flow2MG=mean(data$gamPred[flow2I])
  flow2MW=mean(data$wrtdsPred[flow2I])
  
  
  firstG=mean(subset(data[flow2I,],year==min(year,na.rm=T))$gamPred,na.rm=T)
  secondG=mean(subset(data[flow2I,],year==(min(year,na.rm=T)+1))$gamPred,na.rm=T)
  thirdG=mean(subset(data[flow2I,],year==(min(year,na.rm=T)+2))$gamPred,na.rm=T)
  
  lastG=mean(subset(data[flow2I,],year==max(year,na.rm=T))$gamPred,na.rm=T)
  last2G=mean(subset(data[flow2I,],year==(max(year,na.rm=T)-1))$gamPred,na.rm=T)
  last3G=mean(subset(data[flow2I,],year==(max(year,na.rm=T)-2))$gamPred,na.rm=T)
  
  meanLG=mean(na.omit(c(firstG,secondG,thirdG)))
  meanUG=mean(na.omit(c(lastG,last2G,last3G)))
  
  
  firstW=mean(subset(data[flow2I,],year==min(year,na.rm=T))$wrtdsPred,na.rm=T)
  secondW=mean(subset(data[flow2I,],year==(min(year,na.rm=T)+1))$wrtdsPred,na.rm=T)
  thirdW=mean(subset(data[flow2I,],year==(min(year,na.rm=T)+2))$wrtdsPred,na.rm=T)
  
  lastW=mean(subset(data[flow2I,],year==max(year,na.rm=T))$wrtdsPred,na.rm=T)
  last2W=mean(subset(data[flow2I,],year==(max(year,na.rm=T)-1))$wrtdsPred,na.rm=T)
  last3W=mean(subset(data[flow2I,],year==(max(year,na.rm=T)-2))$wrtdsPred,na.rm=T)
  
  meanLW=mean(na.omit(c(firstW,secondW,thirdW)))
  meanUW=mean(na.omit(c(lastW,last2W,last3W)))
  
  
  flow2PCG=(meanUG-meanLG)/meanLG
  flow2PCW=(meanUW-meanLW)/meanLW
  
  
  # flow2PCG=(data$gamPred[maxDate]-
  #             data$gamPred[minDate])/
  #   data$gamPred[minDate]
  # flow2PCW=(data$wrtdsPred[maxDate]-
  #             data$wrtdsPred[minDate])/
  #   data$wrtdsPred[minDate]
  
  flow3=subset(data,flo>=quantile(data$flo,.5) & data$flo<quantile(data$flo,0.75))
  flow3I=which(data$flo>=quantile(data$flo,0.5)& data$flo<quantile(data$flo,0.75))
  minDate=which.min(flow3$date)
  maxDate=which.max(flow3$date)
  flow3MG=mean(data$gamPred[flow3I])
  flow3MW=mean(data$wrtdsPred[flow3I])
  
  firstG=mean(subset(data[flow3I,],year==min(year,na.rm=T))$gamPred,na.rm=T)
  secondG=mean(subset(data[flow3I,],year==(min(year,na.rm=T)+1))$gamPred,na.rm=T)
  thirdG=mean(subset(data[flow3I,],year==(min(year,na.rm=T)+2))$gamPred,na.rm=T)
  
  lastG=mean(subset(data[flow3I,],year==max(year,na.rm=T))$gamPred,na.rm=T)
  last2G=mean(subset(data[flow3I,],year==(max(year,na.rm=T)-1))$gamPred,na.rm=T)
  last3G=mean(subset(data[flow3I,],year==(max(year,na.rm=T)-2))$gamPred,na.rm=T)
  
  meanLG=mean(na.omit(c(firstG,secondG,thirdG)))
  meanUG=mean(na.omit(c(lastG,last2G,last3G)))
  
  
  firstW=mean(subset(data[flow3I,],year==min(year,na.rm=T))$wrtdsPred,na.rm=T)
  secondW=mean(subset(data[flow3I,],year==(min(year,na.rm=T)+1))$wrtdsPred,na.rm=T)
  thirdW=mean(subset(data[flow3I,],year==(min(year,na.rm=T)+2))$wrtdsPred,na.rm=T)
  
  lastW=mean(subset(data[flow3I,],year==max(year,na.rm=T))$wrtdsPred,na.rm=T)
  last2W=mean(subset(data[flow3I,],year==(max(year,na.rm=T)-1))$wrtdsPred,na.rm=T)
  last3W=mean(subset(data[flow3I,],year==(max(year,na.rm=T)-2))$wrtdsPred,na.rm=T)
  
  meanLW=mean(na.omit(c(firstW,secondW,thirdW)))
  meanUW=mean(na.omit(c(lastW,last2W,last3W)))
  
  
  flow3PCG=(meanUG-meanLG)/meanLG
  flow3PCW=(meanUW-meanLW)/meanLW
  
  # flow3PCG=(data$gamPred[maxDate]-
  #             data$gamPred[minDate])/
  #   data$gamPred[minDate]
  # flow3PCW=(data$wrtdsPred[maxDate]-
  #             data$wrtdsPred[minDate])/
  #   data$wrtdsPred[minDate]
  
  flow4=subset(data,flo>=quantile(data$flo,.75) )
  flow4I=which(data$flo>=quantile(data$flo,0.75))
  minDate=which.min(flow4$date)
  maxDate=which.max(flow4$date)
  flow4MG=mean(data$gamPred[flow4I])
  flow4MW=mean(data$wrtdsPred[flow4I])
  
  firstG=mean(subset(data[flow4I,],year==min(year,na.rm=T))$gamPred,na.rm=T)
  secondG=mean(subset(data[flow4I,],year==(min(year,na.rm=T)+1))$gamPred,na.rm=T)
  thirdG=mean(subset(data[flow4I,],year==(min(year,na.rm=T)+2))$gamPred,na.rm=T)
  
  lastG=mean(subset(data[flow4I,],year==max(year,na.rm=T))$gamPred,na.rm=T)
  last2G=mean(subset(data[flow4I,],year==(max(year,na.rm=T)-1))$gamPred,na.rm=T)
  last3G=mean(subset(data[flow4I,],year==(max(year,na.rm=T)-2))$gamPred,na.rm=T)
  
  meanLG=mean(na.omit(c(firstG,secondG,thirdG)))
  meanUG=mean(na.omit(c(lastG,last2G,last3G)))
  
  
  firstW=mean(subset(data[flow4I,],year==min(year,na.rm=T))$wrtdsPred,na.rm=T)
  secondW=mean(subset(data[flow4I,],year==(min(year,na.rm=T)+1))$wrtdsPred,na.rm=T)
  thirdW=mean(subset(data[flow4I,],year==(min(year,na.rm=T)+2))$wrtdsPred,na.rm=T)
  
  lastW=mean(subset(data[flow4I,],year==max(year,na.rm=T))$wrtdsPred,na.rm=T)
  last2W=mean(subset(data[flow4I,],year==(max(year,na.rm=T)-1))$wrtdsPred,na.rm=T)
  last3W=mean(subset(data[flow4I,],year==(max(year,na.rm=T)-2))$wrtdsPred,na.rm=T)
  
  meanLW=mean(na.omit(c(firstW,secondW,thirdW)))
  meanUW=mean(na.omit(c(lastW,last2W,last3W)))
  
  
  flow4PCG=(meanUG-meanLG)/meanLG
  flow4PCW=(meanUW-meanLW)/meanLW
  
  # flow4PCG=(data$gamPred[maxDate]-
  #             data$gamPred[minDate])/
  #   data$gamPred[minDate]
  # flow4PCW=(data$wrtdsPred[maxDate]-
  #             data$wrtdsPred[minDate])/
  #   data$wrtdsPred[minDate]
  
  avgG=rbind(avgOverallG,annual1MG,annual2MG,annual3MG,annual4MG,annual5MG,
            seasonal1MG,seasonal2MG,seasonal3MG,seasonal4MG,
            flow1MG,flow2MG,flow3MG,flow4MG)
  pcG=rbind(percentChangeG,annual1PCG,annual2PCG,annual3PCG,annual4PCG,annual5PCG,
           seasonal1PCG,seasonal2PCG,seasonal3PCG,seasonal4PCG,
           flow1PCG,flow2PCG,flow3PCG,flow4PCG)
  avgW=rbind(avgOverallW,annual1MW,annual2MW,annual3MW,annual4MW,annual5MW,
            seasonal1MW,seasonal2MW,seasonal3MW,seasonal4MW,
            flow1MW,flow2MW,flow3MW,flow4MW)
  pcW=rbind(percentChangeW,annual1PCW,annual2PCW,annual3PCW,annual4PCW,annual5PCW,
           seasonal1PCW,seasonal2PCW,seasonal3PCW,seasonal4PCW,
           flow1PCW,flow2PCW,flow3PCW,flow4PCW)
  return(list(avg=cbind(avgG,avgW),pc=cbind(pcG,pcW)))
  
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
