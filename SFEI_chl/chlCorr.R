## investigate chl correlation between stations and any potential lags

## merge date, chl from each station

setwd("~/Desktop/sfei")
load(file="perStation.Rda")

wholeSeries<-c(1, 2, 5, 7, 11, 13, 15, 16, 17, 18, 21, 22, 23, 29, 40)

testMerge=merge(perStation[[1]][,c("station","chl","Date")],perStation[[2]][,c("station","chl","Date")],
                all.x=T,all.y=T)
for(i in wholeSeries[3:length(wholeSeries)]){
  testMerge=merge(testMerge,perStation[[i]][,c("station","chl","Date")],all.x=T,all.y=T)
}

require(Hmisc)
result<-rcorr()
## will want to order these by location to have a sense of what is going on

plot(allData$Longitude,allData$Latitude,type="n")
text(allData$Longitude,allData$Latitude, allData$Station)