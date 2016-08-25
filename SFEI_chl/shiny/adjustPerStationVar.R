setwd("~/Desktop/sfei")

allData<-read.csv("allData.csv")

head(allData[,c("Date","Station","chl")])
head( do.call("rbind", perStationAdd[wholeSeries])[,c("Date","Station","chl")])

tail(allData[,c("Date","Station","chl")])
tail( do.call("rbind", perStationAdd[wholeSeries])[,c("Date","Station","chl")])

for(i in wholeSeries){
  sub=subset(allData,Station==names(perStationAdd)[i])
  perStationAdd[[i]]=cbind(perStationAdd[[i]],sub[,c("mod1Pred","mod2Pred","mod3Pred","mod4Pred")])
}
head(perStationAdd[[1]])

## 68,69,70,71

for(i in setdiff(1:41,wholeSeries)){
  perStationAdd[[i]]=NA
}
save(perStationAdd,file="perStationAdd.Rda")


