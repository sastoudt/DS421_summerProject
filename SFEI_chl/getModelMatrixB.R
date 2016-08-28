## make B design matrix

## n x p (p: number of stream units)
## i th row has value 1 in column corresponding to the stream unit of y_i and 0 elsewhere
## 

p=nrow(adjacency)

setwd("~/Desktop/sfei")

allData<-read.csv("allData.csv",stringsAsFactors=F)
allData<-allData[-which(allData$Station=="C10"),]
allData<-allData[-which(allData$Station=="D41"),]
## pick one year, ignore over time for now
require(lubridate)
test=subset(allData,year(allData$Date)==2015)
nrow(test)

y=test$chl
sum(is.na(y)) ## good, no NA

seg=test$Station
class(seg)

B=matrix(0,nrow=length(y),ncol=p)

lookUp=as.data.frame(cbind(unique(test$Station),1:length(unique(test$Station))))
names(lookUp)=c("station","p")
lookUp$station=as.character(lookUp$station)
lookUp$p=as.numeric(as.character(lookUp$p))
## do without a loop later, just get a quick sense
for(i in 1:nrow(B)){
  index=which(lookUp$station==seg[i])
  B[i,index]=1
  print(i)
}

