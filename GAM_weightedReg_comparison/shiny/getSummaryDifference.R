## like Table 5 Beck_and_Murphy_EMA
## assume that model has gone through modfit and has an attribute $fits
## assume the predictions match up: order by date before making predictions
getSummaryDifference<-function(data,modelGAM,modelWRTDS){
  data=data[!is.na(data$res),]
  data=data[!is.na(data$flo),]
  data=data[order(data$date),]
  
  predValGAM=modelGAM$fitted.values
  predValWRTDS=modelWRTDS$fits
  
 avgDiff=(sum(predValWRTDS)-sum(predValGAM))/sum(predValGAM)*100
 rmse=sqrt(sum((predValWRTDS-predValGAM)^2))

  data$month=as.numeric(strftime(data$date, '%m'))
  data$year=as.numeric(strftime(data$date, '%Y'))
  
  annual1=subset(data,year<1983 & year>=1976)
  annual1I=which(data$year<1983 & data$year>=1976)
  annual1PW=predValWRTDS[annual1I]
  annual1PG=predValGAM[annual1I]
  annualAvgDiff1=(sum(annual1PW)-sum(annual1PG))/sum(annual1PG)*100
  annualRMSE1=sqrt(sum((annual1PW-annual1PG)^2))
  
  annual2=subset(data,year<1990 & year>=1983)
  annual2I=which(data$year<1990 & data$year>=1983)
  annual2PW=predValWRTDS[annual2I]
  annual2PG=predValGAM[annual2I]
  annualAvgDiff2=(sum(annual2PW)-sum(annual2PG))/sum(annual2PG)*100
  annualRMSE2=sqrt(sum((annual2PW-annual2PG)^2))
  
  annual3=subset(data,year<1997 & year>=1990)
  annual3I=which(data$year<1997 & data$year>=1990)
  annual3PW=predValWRTDS[annual3I]
  annual3PG=predValGAM[annual3I]
  annualAvgDiff3=(sum(annual3PW)-sum(annual3PG))/sum(annual3PG)*100
  annualRMSE3=sqrt(sum((annual3PW-annual3PG)^2))
  
  annual4=subset(data,year<2004 & year>=1997)
  annual4I=which(data$year<2004 & data$year>=1997)
  annual4PW=predValWRTDS[annual4I]
  annual4PG=predValGAM[annual4I]
  annualAvgDiff4=(sum(annual4PW)-sum(annual4PG))/sum(annual4PG)*100
  annualRMSE4=sqrt(sum((annual4PW-annual4PG)^2))
  
  annual5=subset(data,year>=2004) ## has 2 extra years
  annual5I=which( data$year>=2004)
  annual5PW=predValWRTDS[annual5I]
  annual5PG=predValGAM[annual5I]
  annualAvgDiff5=(sum(annual5PW)-sum(annual5PG))/sum(annual5PG)*100
  annualRMSE5=sqrt(sum((annual5PW-annual5PG)^2))

  
  seasonal1=subset(data,month %in% c(1:3))
  seasonal1I=which(data$month %in% c(1:3))
  seasonal1PW=predValWRTDS[seasonal1I]
  seasonal1PG=predValGAM[seasonal1I]
  seasonalAvgDiff1=(sum(seasonal1PW)-sum(seasonal1PG))/sum(seasonal1PG)*100
  seasonalRMSE1=sqrt(sum((seasonal1PW-seasonal1PG)^2))
  
  seasonal2=subset(data,month %in% c(4:6))
  seasonal2I=which(data$month %in% c(4:6))
  seasonal2PW=predValWRTDS[seasonal2I]
  seasonal2PG=predValGAM[seasonal2I]
  seasonalAvgDiff2=(sum(seasonal2PW)-sum(seasonal2PG))/sum(seasonal2PG)*100
  seasonalRMSE2=sqrt(sum((seasonal2PW-seasonal2PG)^2))
  
  seasonal3=subset(data,month %in% c(7:9))
  seasonal3I=which(data$month %in% c(7:9))
  seasonal3PW=predValWRTDS[seasonal3I]
  seasonal3PG=predValGAM[seasonal3I]
  seasonalAvgDiff3=(sum(seasonal3PW)-sum(seasonal3PG))/sum(seasonal3PG)*100
  seasonalRMSE3=sqrt(sum((seasonal3PW-seasonal3PG)^2))
  
  seasonal4=subset(data,month %in% c(10:12))
  seasonal4I=which(data$month %in% c(10:12))
  seasonal4PW=predValWRTDS[seasonal4I]
  seasonal4PG=predValGAM[seasonal4I]
  seasonalAvgDiff4=(sum(seasonal4PW)-sum(seasonal4PG))/sum(seasonal4PG)*100
  seasonalRMSE4=sqrt(sum((seasonal4PW-seasonal4PG)^2))
  
  flow1=subset(data,flo<quantile(data$flo,.25))
  flow1I=which(data$flo<quantile(data$flo,0.25))
  flow1PW=predValWRTDS[flow1I]
  flow1PG=predValGAM[flow1I]
  flowAvgDiff1=(sum(flow1PW)-sum(flow1PG))/sum(flow1PG)*100
  flowRMSE1=sqrt(sum((flow1PW-flow1PG)^2))
  
  flow2=subset(data,flo>=quantile(data$flo,.25) & flo<quantile(data$flo,0.5))
  flow2I=which(data$flo>=quantile(data$flo,0.25)& data$flo<quantile(data$flo,0.5))
  flow2PW=predValWRTDS[flow2I]
  flow2PG=predValGAM[flow2I]
  flowAvgDiff2=(sum(flow2PW)-sum(flow2PG))/sum(flow2PG)*100
  flowRMSE2=sqrt(sum((flow2PW-flow2PG)^2))
  
  flow3=subset(data,flo>=quantile(data$flo,.5) & flo<quantile(data$flo,0.75))
  flow3I=which(data$flo>=quantile(data$flo,0.5)& data$flo<quantile(data$flo,0.75))
  flow3PW=predValWRTDS[flow3I]
  flow3PG=predValGAM[flow3I]
  flowAvgDiff3=(sum(flow3PW)-sum(flow3PG))/sum(flow3PG)*100
  flowRMSE3=sqrt(sum((flow3PW-flow3PG)^2))
  
  flow4=subset(data,flo>=quantile(data$flo,.75) )
  flow4I=which(data$flo>=quantile(data$flo,0.75))
  flow4PW=predValWRTDS[flow4I]
  flow4PG=predValGAM[flow4I]
  flowAvgDiff4=(sum(flow4PW)-sum(flow4PG))/sum(flow4PG)*100
  flowRMSE4=sqrt(sum((flow4PW-flow4PG)^2))

  
  avgDiff=rbind(avgDiff,annualAvgDiff1,annualAvgDiff2,annualAvgDiff3,annualAvgDiff4,
                annualAvgDiff5,seasonalAvgDiff1,seasonalAvgDiff2,seasonalAvgDiff3,
                seasonalAvgDiff4,flowAvgDiff1,flowAvgDiff2,flowAvgDiff3,flowAvgDiff4)
  rmse=rbind(rmse,annualRMSE1,annualRMSE2,annualRMSE3,annualRMSE4,annualRMSE5,
             seasonalRMSE1,seasonalRMSE2,seasonalRMSE3,seasonalRMSE4,
             flowRMSE1,flowRMSE2,flowRMSE3,flowRMSE4)
  
  return(list(avgDiff=avgDiff,rmse=rmse))
  
  # return(list(avgDiff=avgDiff,rmse=rmse,annualAvgDiff1=annualAvgDiff1,annualRMSE1=annualRMSE1,
  #             annualAvgDiff2=annualAvgDiff2,annualRMSE2=annualRMSE2,
  #             annualAvgDiff3=annualAvgDiff3, annualRMSE3=annualRMSE3,
  #             annualAvgDiff4=annualAvgDiff4, annualRMSE4=annualRMSE4,
  #             annualAvgDiff5=annualAvgDiff5, annualRMSE5=annualRMSE5,
  #             seasonalAvgDiff1=seasonalAvgDiff1,seasonalRMSE1=seasonalRMSE1,
  #             seasonalAvgDiff2=seasonalAvgDiff2, seasonalRMSE2=seasonalRMSE2,
  #             seasonalAvgDiff3=seasonalAvgDiff3,seasonalRMSE3=seasonalRMSE3,
  #             seasonalAvgDiff4=seasonalAvgDiff4,seasonalRMSE4=seasonalRMSE4,
  #             flowAvgDiff1=flowAvgDiff1,flowRMSE1=flowRMSE1,
  #             flowAvgDiff2=flowAvgDiff2, flowRMSE2=flowRMSE2,
  #             flowAvgDiff3=flowAvgDiff3,flowRMSE3=flowRMSE3,
  #             flowAvgDiff4=flowAvgDiff4,flowRMSE4=flowRMSE4))
}

### SCRATCH ####

# setwd("~/Desktop/EPA_GAMS/sf_trends-master/data")
# load("mods_nolag.RData")
# names(mods_nolag)
# class(mods_nolag$mod[[1]])
# names(mods_nolag$mod[[1]])
# dim(mods_nolag$mod[[1]])
# 
# test=respred.tidalmean(mods_nolag$mod[[1]])
# head(test)
# dim(test)
# sum(!is.na(test$res))
# dim(mods_nolag$data[[1]])
# sum(is.na(mods_nolag$data[[1]]$resval) | is.na(mods_nolag$data[[1]]$flolag))
# 456-434
# 
# class(test)
# head(test)
# test[!is.na(test$res),]