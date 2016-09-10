#### fitted value plots


setwd("~/Desktop/sfei")

 load(file = "perStationParsimoniousModels.Rda")
 load(file = "perStationFullModels.Rda")
 load(file="perStationInteractionModels.Rda")

 load(file="mod1Spatial.RData")
# load(file="mod2Spatial.RData")
# load(file="mod3Spatial.RData")
# load(file="mod4Spatial.RData")
# load(file="perStationAdd.Rda")
load(file="perStationFlowMod.Rda")
# load(file="perStationFlowTOT.Rda")
 load(file="perStationPredVal.Rda")
 load(file="perStationAdd.Rda")
head(perStationPredVal[[1]])
wholeSeries<-c(1, 2, 5, 7, 11, 13, 15, 16, 17, 18, 21, 22, 23, 29, 40)

for(index in wholeSeries){

  data=perStationPredVal[[index]]
  
  byTerm=predict(perStationParsMod[[index]],data,type="terms")

nestPred=as.data.frame(cbind.data.frame(byTerm,rep(summary(perStationParsMod[[index]])$p.coeff)))

if(index %in% c(5,7,13)){
  toName=c("doy","date_dec","pheo","do_per","intercept")
  terms<-c("ti(pheo)","ti(doy)","ti(date_dec)","ti(do_per)","intercept")
  
}else{
  toName=c("doy","date_dec","pheo","tn","do_per","intercept")
  terms<-c("ti(pheo)","ti(doy)","ti(date_dec)","ti(tn)","ti(do_per)","intercept")
 
}


names(nestPred)=paste("parsMod",toName,sep="_")

perStationPredVal[[index]]=cbind.data.frame(data,nestPred)

}
save(perStationPredVal,file="perStationPredVal.Rda")

for(index in wholeSeries){
  
  data=perStationPredVal[[index]]
  
  if(index!=13){
  byTerm=predict(perStationFullMod[[index]],data,type="terms")
  nestPred=as.data.frame(cbind.data.frame(byTerm,rep(summary(perStationFullMod[[index]])$p.coeff)))
  
  }
  

  if(index %in% c(5,7)){
   
    toName=c("doy","date_dec","pheo","do_per","sal","intercept")
    
  }else if(index %in% c(17,18,21,22,23)){
    
    toName=c("doy","date_dec","pheo","tn","do_per","sio2","tp","tss","nh4","sal","intercept")
    
  }else{
    
   
    toName=c("doy","date_dec","pheo","tn","do_per","sio2","tp","tss","nh4","intercept")

    
    
  }
  
  
  if(index!=13){
  names(nestPred)=paste("parsFull",toName,sep="_")
  
  perStationPredVal[[index]]=cbind.data.frame(data,nestPred)
}
}
save(perStationPredVal,file="perStationPredVal.Rda")



for(index in wholeSeries){
  data=perStationPredVal[[index]]
  
  toName=c("doy","date_dec","interaction","intercept")
  byTerm=predict(perStationIntMod[[index]],data,type="terms")
  nestPred=as.data.frame(cbind.data.frame(byTerm,rep(summary(perStationIntMod[[index]])$p.coeff)))
  names(nestPred)=paste("parsInt",toName,sep="_")
  perStationPredVal[[index]]=cbind.data.frame(data,nestPred)
  
}

save(perStationPredVal,file="perStationPredVal.Rda")


####

summary(mod1)

stationNames[wholeSeries]

stationEffect=unname(mod1$coefficients[1:length(wholeSeries)])
stationEffect[2:length(stationEffect)]=stationEffect[2:length(stationEffect)]+stationEffect[1]

for(index in wholeSeries){
data=perStationPredVal[[index]]
byTerm=predict(mod1,data,type="terms")
nestPred=as.data.frame(cbind.data.frame(byTerm[,2:3],stationEffect[index]))
toName=c("doy","date_dec","intercept")
names(nestPred)=paste("spatM1",toName,sep="_")

perStationPredVal[[index]]=cbind.data.frame(data,nestPred)
}

save(perStationPredVal,file="perStationPredVal.Rda")

###


####

summary(mod2)

stationEffect=unname(mod2$coefficients[1:length(wholeSeries)])
stationEffect[2:length(stationEffect)]=stationEffect[2:length(stationEffect)]+stationEffect[1]

for(index in wholeSeries){
  data=perStationPredVal[[index]]
  byTerm=predict(mod2,data,type="terms")
  dateDecStat=apply(byTerm[,3:ncol(byTerm)],1,sum)
  nestPred=as.data.frame(cbind.data.frame(byTerm[,2],dateDecStat,stationEffect[index]))
  toName=c("doy","date_decByStation","intercept")
  names(nestPred)=paste("spatM2",toName,sep="_")
  
  perStationPredVal[[index]]=cbind.data.frame(data,nestPred)
}

save(perStationPredVal,file="perStationPredVal.Rda")

####
summary(mod3)

stationEffect=unname(mod3$coefficients[1:length(wholeSeries)])
stationEffect[2:length(stationEffect)]=stationEffect[2:length(stationEffect)]+stationEffect[1]

for(index in wholeSeries){
  data=perStationPredVal[[index]]
  byTerm=predict(mod3,data,type="terms")
  dateDecStat=apply(byTerm[,2:16],1,sum)
  doyStat=apply(byTerm[,17:31],1,sum)
  nestPred=as.data.frame(cbind.data.frame(dateDecStat,doyStat,stationEffect[index]))
  toName=c("doyByStation","date_decByStation","intercept")
  names(nestPred)=paste("spatM3",toName,sep="_")
  
  perStationPredVal[[index]]=cbind.data.frame(data,nestPred)
}



save(perStationPredVal,file="perStationPredVal.Rda")

####

summary(mod4)

stationEffect=unname(mod4$coefficients[1:length(wholeSeries)])
stationEffect[2:length(stationEffect)]=stationEffect[2:length(stationEffect)]+stationEffect[1]

for(index in wholeSeries){
  data=perStationPredVal[[index]]
  byTerm=predict(mod4,data,type="terms")
  dateDecStat=apply(byTerm[,3:17],1,sum)
  intStat=apply(byTerm[,18:32],1,sum)
  nestPred=as.data.frame(cbind.data.frame(byTerm[,2],dateDecStat,intStat,stationEffect[index]))
  toName=c("doy","date_decByStation","intByStation","intercept")
  names(nestPred)=paste("spatM3",toName,sep="_")
  
  perStationPredVal[[index]]=cbind.data.frame(data,nestPred)
}



save(perStationPredVal,file="perStationPredVal.Rda")
