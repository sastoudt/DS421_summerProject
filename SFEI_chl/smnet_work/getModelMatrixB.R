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

B=as.spam(B)

t(B)%*%B

lambda=1

t(D)%*%D

inv=solve(t(B)%*%B+lambda*t(D)%*%D) ## fast, might have to worry as we scale up

betaHat=inv%*%t(B)%*%y

betaHat

## everything ran, does this make even a little bit of sense?

forMap=allData[,c("Longitude","Latitude","Station")]
forMap=unique(forMap)
dim(forMap)

require(ggplot2)
testMerge=cbind(forMap,betaHat)

ggplot(testMerge,aes(x = Longitude, y = Latitude,colour=betaHat,cex=2))+geom_point()+
  ggtitle("smnet betas by Station")+scale_size(guide=F)

load(file="mod1Spatial.RData")
mod1$coefficients
statNam=unlist(lapply(names(mod1$coefficients)[1:15],function(x){y=strsplit(x,"Station)");unlist(y)[2]}))
statNam[1]="C10"

statNam
intercept1=as.data.frame(cbind(statNam,mod1$coefficients[1:15]))
intercept1

intercept1=intercept1[-c(1,10),]
intercept1

require(gridExtra)

g1<-ggplot(testMerge,aes(x = Longitude, y = Latitude,colour=betaHat,cex=2))+geom_point()+
  ggtitle("smnet betas by Station")+scale_size(guide=F)

testMerge2=cbind(forMap,intercept1)
names(testMerge2)[5]="int"
testMerge2$int=as.numeric(as.character(testMerge2$int))
g2<-ggplot(testMerge2,aes(x = Longitude, y = Latitude,colour=int,cex=2))+geom_point()+
  ggtitle("Spatial Model 1 Intercept by Station")+scale_size(guide=F)

grid.arrange(g1,g2)

## get mean per station

require(dplyr)

by_station <- group_by(test, Station)
chlAvg <- summarise(by_station,
                   count = n(),
                   avg = mean(chl, na.rm = TRUE))

chlAvg

cbind.data.frame(chlAvg$Station,chlAvg$avg,betaHat)

order(chlAvg$avg)
order(betaHat)
order(testMerge2$int)
