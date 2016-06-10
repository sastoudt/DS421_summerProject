## like Table 6 Beck_and_Murphy_EMA
##WRTDS response, GAM predictor

## assume that model has gone through modfit and has an attribute $fits or $norms
## assume the predictions match up: order by date before making predictions
getRegressionResults<-function(modGAM,modWRTDS,sigLevel){

  toUse=ifelse("norms" %in% names(modWRTDS),"norms","fits")
  
  mod.lm=lm(modWRTDS[,toUse]~modGAM$fitted.values)
  
  betas=unname(coefficients(mod.lm))
  pVals=unname(summary(mod.lm)$coefficients[,4])
  isSignificant=ifelse(pVals<sigLevel,1,0)
  
  predValGAM=modelGAM$fitted.values
  predValWRTDS=modelWRTDS[,toUse]
  
  data$month=as.numeric(strftime(data$date, '%m'))
  data$year=as.numeric(strftime(data$date, '%Y'))
  
  annual1=subset(data,year<1983 & year>=1976)
  annual1I=which(data$year<1983 & data$year>=1976)
  annual1PW=predValWRTDS[annual1I]
  annual1PG=predValGAM[annual1I]
  mod.annual1=lm(annual1PW~annual1PG)
  annualBetas1=unname(coefficients(mod.annual1))
  annualPVals1=unname(summary(mod.annual1)$coefficients[,4])
  isSignificantAnnual1=ifelse(annualPVals1<sigLevel,1,0)
  
  annual2=subset(data,year<1990 & year>=1983)
  annual2I=which(data$year<1990 & data$year>=1983)
  annual2PW=predValWRTDS[annual2I]
  annual2PG=predValGAM[annual2I]
  mod.annual2=lm(annual2PW~annual2PG)
  annualBetas2=unname(coefficients(mod.annual2))
  annualPVals2=unname(summary(mod.annual2)$coefficients[,4])
  isSignificantAnnual2=ifelse(annualPVals2<sigLevel,1,0)
  
  annual3=subset(data,year<1997 & year>=1990)
  annual3I=which(data$year<1997 & data$year>=1990)
  annual3PW=predValWRTDS[annual3I]
  annual3PG=predValGAM[annual3I]
  mod.annual3=lm(annual3PW~annual3PG)
  annualBetas3=unname(coefficients(mod.annual3))
  annualPVals3=unname(summary(mod.annual3)$coefficients[,4])
  isSignificantAnnual3=ifelse(annualPVals3<sigLevel,1,0)
  
  annual4=subset(data,year<2004 & year>=1997)
  annual4I=which(data$year<2004 & data$year>=1997)
  annual4PW=predValWRTDS[annual4I]
  annual4PG=predValGAM[annual4I]
  mod.annual4=lm(annual4PW~annual4PG)
  annualBetas4=unname(coefficients(mod.annual4))
  annualPVals4=unname(summary(mod.annual4)$coefficients[,4])
  isSignificantAnnual4=ifelse(annualPVals4<sigLevel,1,0)
  
  annual5=subset(data,year>=2004) ## has 2 extra years
  annual5I=which( data$year>=2004)
  annual5PW=predValWRTDS[annual5I]
  annual5PG=predValGAM[annual5I]
  mod.annual5=lm(annual5PW~annual5PG)
  annualBetas5=unname(coefficients(mod.annual5))
  annualPVals5=unname(summary(mod.annual5)$coefficients[,4])
  isSignificantAnnual5=ifelse(annualPVals5<sigLevel,1,0)
  
  
  seasonal1=subset(data,month %in% c(1:3))
  seasonal1I=which(data$month %in% c(1:3))
  seasonal1PW=predValWRTDS[seasonal1I]
  seasonal1PG=predValGAM[seasonal1I]
  mod.seasonal1=lm(seasonal1PW~seasonal1PG)
  seasonalBetas1=unname(coefficients(mod.seasonal1))
  seasonalPVals1=unname(summary(mod.seasonal1)$coefficients[,4])
  isSignificantSeasonal1=ifelse(seasonalPVals1<sigLevel,1,0)
  
  seasonal2=subset(data,month %in% c(4:6))
  seasonal2I=which(data$month %in% c(4:6))
  seasonal2PW=predValWRTDS[seasonal2I]
  seasonal2PG=predValGAM[seasonal2I]
  mod.seasonal2=lm(seasonal2PW~seasonal2PG)
  seasonalBetas2=unname(coefficients(mod.seasonal2))
  seasonalPVals2=unname(summary(mod.seasonal2)$coefficients[,4])
  isSignificantSeasonal2=ifelse(seasonalPVals2<sigLevel,1,0)
  
  seasonal3=subset(data,month %in% c(7:9))
  seasonal3I=which(data$month %in% c(7:9))
  seasonal3PW=predValWRTDS[seasonal3I]
  seasonal3PG=predValGAM[seasonal3I]
  mod.seasonal3=lm(seasonal3PW~seasonal3PG)
  seasonalBetas3=unname(coefficients(mod.seasonal3))
  seasonalPVals3=unname(summary(mod.seasonal3)$coefficients[,4])
  isSignificantSeasonal3=ifelse(seasonalPVals3<sigLevel,1,0)
  
  seasonal4=subset(data,month %in% c(10:12))
  seasonal4I=which(data$month %in% c(10:12))
  seasonal4PW=predValWRTDS[seasonal4I]
  seasonal4PG=predValGAM[seasonal4I]
  mod.seasonal4=lm(seasonal4PW~seasonal4PG)
  seasonalBetas4=unname(coefficients(mod.seasonal4))
  seasonalPVals4=unname(summary(mod.seasonal4)$coefficients[,4])
  isSignificantSeasonal4=ifelse(seasonalPVals4<sigLevel,1,0)
  
  flow1=subset(data,flo<quantile(data$flo,.25))
  flow1I=which(data$flo<quantile(data$flo,0.25))
  flow1PW=predValWRTDS[flow1I]
  flow1PG=predValGAM[flow1I]
  mod.flow1=lm(flow1PW~flow1PG)
  flowBetas1=unname(coefficients(mod.flow1))
  flowPVals1=unname(summary(mod.flow1)$coefficients[,4])
  isSignificantFlow1=ifelse(flowPVals1<sigLevel,1,0)
  
  flow2=subset(data,flo>=quantile(data$flo,.25) & flow<quantile(data$flo,0.5))
  flow2I=which(data$flo>=quantile(data$flo,0.25)& data$flow<quantile(data$flo,0.5))
  flow2PW=predValWRTDS[flow2I]
  flow2PG=predValGAM[flow2I]
  mod.flow2=lm(flow2PW~flow2PG)
  flowBetas2=unname(coefficients(mod.flow2))
  flowPVals2=unname(summary(mod.flow2)$coefficients[,4])
  isSignificantFlow2=ifelse(flowPVals2<sigLevel,1,0)
  
  flow3=subset(data,flo>=quantile(data$flo,.5) & flow<quantile(data$flo,0.75))
  flow3I=which(data$flo>=quantile(data$flo,0.5)& data$flow<quantile(data$flo,0.75))
  flow3PW=predValWRTDS[flow3I]
  flow3PG=predValGAM[flow3I]
  mod.flow3=lm(flow3PW~flow3PG)
  flowBetas3=unname(coefficients(mod.flow3))
  flowPVals3=unname(summary(mod.flow3)$coefficients[,4])
  isSignificantFlow3=ifelse(flowPVals3<sigLevel,1,0)
  
  flow4=subset(data,flo>=quantile(data$flo,.75) )
  flow4I=which(data$flo>=quantile(data$flo,0.75))
  flow4PW=predValWRTDS[flow4I]
  flow4PG=predValGAM[flow4I]
  mod.flow4=lm(flow4PW~flow4PG)
  flowBetas4=unname(coefficients(mod.flow4))
  flowPVals4=unname(summary(mod.flow4)$coefficients[,4])
  isSignificantFlow4=ifelse(flowPVals4<sigLevel,1,0)
  
betas=rbind(betas,annualBetas1,annualBetas2,annualBetas3,annualBetas4,annualBetas5,
            seasonalBetas1,seasonalBetas2,seasonalBetas3,seasonalBetas4,
            flowBetas1,flowBetas2,flowBetas3,flowBetas4)
pVals=rbind(pVals,annualPVals1,annualPVals2,annualPVals3,annualPVals4,annualPVals5,
            seasonalPVals1,seasonalPVals2,seasonalPVals3,seasonalPVals4,
            flowPVals1,flowPVals2,flowPVals3,flowPVals4)
isSignificant=rbind(isSignificant,isSignificantAnnual1,isSignificantAnnual2,
                    isSignificantAnnual3,isSignificantAnnual4,isSignificantAnnual5,
                    isSignificantSeasonal1,isSignificantSeasonal2,isSignificantSeasonal3,
                    isSignificantSeasonal4,isSignificantFlow1,isSignificantFlow2,
                    isSignificantFlow3,isSignificantFlow4)
  
  
  return(list(betas=betas,pVals=pVals,isSignificant=isSignificant))
}