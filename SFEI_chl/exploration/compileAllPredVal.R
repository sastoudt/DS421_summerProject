

### compile predicted values from all models so I can do a more depth analysis of rmse
### like in the GAM/WRTDS comparison, finer breakdown of rmse

load("perStationParsimoniousModels.Rda")
load("perStationFullModels.Rda")
load("perStationInteractionModels.Rda")
load("mod1Spatial.RData")
load("mod2Spatial.RData")
load("mod3Spatial.RData")
load("mod4Spatial.RData")
load("perStationMod3Compare.RData")
load("perStationFlowMod.Rda")
load("perStationFlowTOT.Rda")

load("perStationAdd.Rda")

perStationPredVal=perStationAdd

wholeSeries<-c(1, 2, 5, 7, 11, 13, 15, 16, 17, 18, 21, 22, 23, 29, 40)

for(i in wholeSeries){
  data=perStationAdd[[i]]
  mod=perStationParsMod[[i]]
  if(i %in% c(5,7,13)){
    toUse=na.omit(data[,c("doy","date_dec","pheo","do_per","Date")])
    toPut=setdiff(1:nrow(data),which(is.na(data$doy) | is.na(data$date_dec)|is.na(data$pheo)|
                                       is.na(data$do_per) | is.na(data$Date)))
    
  }else{
    
    toUse=na.omit(data[,c("doy","date_dec","pheo","tn","do_per","Date")])
    toPut=setdiff(1:nrow(data),which(is.na(data$doy) | is.na(data$date_dec)|is.na(data$pheo)|
                                     is.na(data$tn)|  is.na(data$do_per) | is.na(data$Date)))
  }
  
  fullPred=predict(mod,toUse,type="response")
  perStationPredVal[[i]]$predPars[toPut]=fullPred
  print(i)
}

View(perStationPredVal[[5]])

for(i in wholeSeries){
  data=perStationAdd[[i]]
  mod=perStationFullMod[[i]]
  if(i %in% c(5,7)){
    toUse=na.omit(data[,c("doy","date_dec","pheo","do_per",
                          "sal","Date")])
    fullPred=predict(mod,toUse,type="response")
    
    toPut=setdiff(1:nrow(data),which(is.na(data$doy) | is.na(data$date_dec)|is.na(data$pheo)|
                                       is.na(data$do_per) | is.na(data$sal) |is.na(data$Date)))
    
    
  }else if(i==13){
    fullPred=rep(NA,nrow(data))
    toPut=1:nrow(data)
  }else if(i %in% c(17, 18, 21, 22, 23)){
    toUse=na.omit(data[,c("doy","date_dec","pheo","tn","do_per",
                          "sio2","tp","tss","nh4","sal","Date")])
    fullPred=predict(mod,toUse,type="response")
    
    toPut=setdiff(1:nrow(data),which(is.na(data$doy) | is.na(data$date_dec)|is.na(data$pheo)|
              is.na(data$tn)| is.na(data$do_per) |is.na(data$sio2)|is.na(data$tp)|
                is.na(data$tss) | is.na(data$nh4) | is.na(data$sal) |is.na(data$Date)))
    
  }else{
    toUse=na.omit(data[,c("doy","date_dec","pheo","tn","do_per",
                          "sio2","tp","tss","nh4","Date")])
    fullPred=predict(mod,toUse,type="response")
    
    toPut=setdiff(1:nrow(data),which(is.na(data$doy) | is.na(data$date_dec)|is.na(data$pheo)|
                                       is.na(data$tn)| is.na(data$do_per) |is.na(data$sio2)|is.na(data$tp)|
                                       is.na(data$tss) | is.na(data$nh4)  |is.na(data$Date)))
    
  }
  
  
  perStationPredVal[[i]]$predFull[toPut]=fullPred
  print(i)
  
}

for(i in wholeSeries){
  
  data<-perStationAdd[[i]]
  mod<-perStationIntMod[[i]]
  
  toUse=na.omit(data[,c("doy","date_dec","Date")])
  toPut=setdiff(1:nrow(data),which(is.na(data$doy) | is.na(data$date_dec)| is.na(data$Date)))
  fullPred=predict(mod,toUse,type="response")
  
  perStationPredVal[[i]]$predInt[toPut]=fullPred
  print(i)
  
}

for(i in wholeSeries){
  
  data<-perStationAdd[[i]]
  mod<-mod1
  
  toUse=na.omit(data[,c("doy","date_dec","Date","Station")])
  toPut=setdiff(1:nrow(data),which(is.na(data$doy) | is.na(data$date_dec)| is.na(data$Date)| 
                                     is.na(data$Station)))
  fullPred=predict(mod,toUse,type="response")
  
  perStationPredVal[[i]]$predSpat1[toPut]=fullPred
  print(i)
  
  
}

for(i in wholeSeries){
  
  data<-perStationAdd[[i]]
  mod<-mod2
  
  toUse=na.omit(data[,c("doy","date_dec","Date","Station")])
  toPut=setdiff(1:nrow(data),which(is.na(data$doy) | is.na(data$date_dec)| is.na(data$Date)| 
                                     is.na(data$Station)))
  fullPred=predict(mod,toUse,type="response")
  
  perStationPredVal[[i]]$predSpat2[toPut]=fullPred
  print(i)
  
  
}

for(i in wholeSeries){
  
  data<-perStationAdd[[i]]
  mod<-mod3
  
  toUse=na.omit(data[,c("doy","date_dec","Date","Station")])
  toPut=setdiff(1:nrow(data),which(is.na(data$doy) | is.na(data$date_dec)| is.na(data$Date)| 
                                     is.na(data$Station)))
  fullPred=predict(mod,toUse,type="response")
  
  perStationPredVal[[i]]$predSpat3[toPut]=fullPred
  print(i)
  
  
}

for(i in wholeSeries){
  
  data<-perStationAdd[[i]]
  mod<-mod4
  
  toUse=na.omit(data[,c("doy","date_dec","Date","Station")])
  toPut=setdiff(1:nrow(data),which(is.na(data$doy) | is.na(data$date_dec)| is.na(data$Date)| 
                                     is.na(data$Station)))
  fullPred=predict(mod,toUse,type="response")
  
  perStationPredVal[[i]]$predSpat4[toPut]=fullPred
  print(i)
  
  
}

names(perStationPredVal[[1]]) ## already has chlPred and flowPred


load("spatialMod3Tn.RData")
load("spatialMod3Pheo.RData")


for(i in wholeSeries){
  
  data<-perStationAdd[[i]]
  mod<-spatialMod3Pheo
  
  toUse=na.omit(data[,c("doy","date_dec","Date","Station","pheo")])
  toPut=setdiff(1:nrow(data),which(is.na(data$doy) | is.na(data$date_dec)| is.na(data$Date)| 
                        is.na(data$pheo)|             is.na(data$Station)))
  fullPred=predict(mod,toUse,type="response")
  
  perStationPredVal[[i]]$predSpat3Pheo[toPut]=fullPred
  print(i)
  
  
}

for(i in wholeSeries){
  
  data<-perStationAdd[[i]]
  mod<-spatialMod3Tn
  
  toUse=na.omit(data[,c("doy","date_dec","Date","Station","tn")])
  toPut=setdiff(1:nrow(data),which(is.na(data$doy) | is.na(data$date_dec)| is.na(data$Date)| 
                                     is.na(data$tn)|             is.na(data$Station)))
  fullPred=as.vector(predict(mod,toUse,type="response"))
  
  perStationPredVal[[i]]$predSpat3Tn=rep(NA,nrow(data)) ## being weird without this
  perStationPredVal[[i]]$predSpat3Tn[toPut]=fullPred 
  print(i)
  
  
}

save(perStationPredVal,file="perStationPredVal.Rda")
