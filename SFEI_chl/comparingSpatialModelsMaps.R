require(ggplot2)

setwd("~/Desktop/sfei")

allData=read.csv("allData.csv")
load(file="mod1Spatial.RData")
load(file="mod2Spatial.RData")
load(file="mod3Spatial.RData")
load(file="mod4Spatial.RData")


forMap=allData[,c("Longitude","Latitude","Station")]
forMap=unique(forMap)

statNam=unlist(lapply(names(mod1$coefficients)[1:15],function(x){y=strsplit(x,"Station)");unlist(y)[2]}))
statNam[1]="C10"

intercept1=as.data.frame(cbind(statNam,mod1$coefficients[1:15]))
row.names(intercept1)<-NULL
names(intercept1)[2]="intercept1"

intercept1=as.data.frame(cbind(statNam,mod1$coefficients[1:15]))
row.names(intercept1)<-NULL
names(intercept1)[2]="intercept1"

intercept1[,2]=as.numeric(as.character(intercept1[,2]))

intercepts=as.data.frame(cbind(statNam,mod1$coefficients[1:15],mod2$coefficients[1:15],mod3$coefficients[1:15],
                               mod4$coefficients[1:15]))

row.names(intercepts)=NULL
names(intercepts)[2:5]=c("int1","int2","int3","int4")
intercepts$int1=as.numeric(as.character(intercepts$int1))
intercepts$int2=as.numeric(as.character(intercepts$int2))
intercepts$int3=as.numeric(as.character(intercepts$int3))
intercepts$int4=as.numeric(as.character(intercepts$int4))

testMerge=merge(forMap,intercepts,by.x="Station",by.y="statNam")

require(gridExtra)

g1<-ggplot(testMerge,aes(x = Longitude, y = Latitude,colour=int1,cex=2))+geom_point()+
  ggtitle("Spatial Model 1 Intercepts by Station")+scale_size(guide=F)

g2<-ggplot(testMerge,aes(x = Longitude, y = Latitude,colour=int2,cex=2))+geom_point()+
  ggtitle("Spatial Model 2 Intercepts by Station")+scale_size(guide=F)

g3<-ggplot(testMerge,aes(x = Longitude, y = Latitude,colour=int3,cex=2))+geom_point()+
  ggtitle("Spatial Model 3 Intercepts by Station")+scale_size(guide=F)

g4<-ggplot(testMerge,aes(x = Longitude, y = Latitude,colour=int4,cex=2))+geom_point()+
  ggtitle("Spatial Model 4 Intercepts by Station")+scale_size(guide=F)
  
grid.arrange(g1,g2,g3,g4) ## can't see what is going on

which(testMerge$Latitude<37.8)

testMerge2=testMerge[-1,]

g1<-ggplot(testMerge2,aes(x = Longitude, y = Latitude,colour=int1,cex=2))+geom_point()+
  ggtitle("Spatial Model 1 Intercepts by Station")+scale_size(guide=F)

g2<-ggplot(testMerge2,aes(x = Longitude, y = Latitude,colour=int2,cex=2))+geom_point()+
  ggtitle("Spatial Model 2 Intercepts by Station")+scale_size(guide=F)

g3<-ggplot(testMerge2,aes(x = Longitude, y = Latitude,colour=int3,cex=2))+geom_point()+
  ggtitle("Spatial Model 3 Intercepts by Station")+scale_size(guide=F)

g4<-ggplot(testMerge2,aes(x = Longitude, y = Latitude,colour=int4,cex=2))+geom_point()+
  ggtitle("Spatial Model 4 Intercepts by Station")+scale_size(guide=F)

grid.arrange(g1,g2,g3,g4)

summary(mod1)
summary(mod2)
summary(mod3)
summary(mod4)

## deviance explained goes down as you get more complicated
mod1
mod2
mod3
mod4

## REML scores go up which makes sense, adding more complexity to each model

### RMSE per station plot

require(dplyr)

####
sum(is.na(allData$resid1)) ## 490
sum(is.na(allData$resid2))
sum(is.na(allData$resid3))
sum(is.na(allData$resid4))

View(allData[which(is.na(allData$resid1)),]) ## places where chl is missing, fine


allData<-allData[!is.na(allData$chl),]

allData$resid1=(allData$chl-allData$mod1Pred)^2
allData$resid2=(allData$chl-allData$mod2Pred)^2
allData$resid3=(allData$chl-allData$mod3Pred)^2
allData$resid4=(allData$chl-allData$mod4Pred)^2


byStation<-group_by(allData,Station)
rmsePerStation<-summarise(byStation,stp1=sum(resid1,na.rm=T),count=n())
rmse1=sqrt(as.vector(rmsePerStation$stp1)/rmsePerStation$count)

rmsePerStation2<-summarise(byStation,stp1=sum(resid2,na.rm=T),count=n(),station=Station[1])
rmse2=sqrt(as.vector(rmsePerStation2$stp1)/rmsePerStation2$count)

rmsePerStation3<-summarise(byStation,stp1=sum(resid3,na.rm=T),count=n(),station=Station[1])
rmse3=sqrt(as.vector(rmsePerStation3$stp1)/rmsePerStation3$count)

rmsePerStation4<-summarise(byStation,stp1=sum(resid4,na.rm=T),count=n())
rmse4=sqrt(as.vector(rmsePerStation4$stp1)/rmsePerStation4$count)

## get matching station names
RMSE=as.data.frame(cbind.data.frame(rmsePerStation2$station, rmse1, rmse2, rmse3,rmse4))
names(RMSE)[1]="station"

testMerge$Station=as.character(testMerge$Station)
testMerge3=merge(testMerge,RMSE,by.x="Station",by.y="station")

testMerge3
class(testMerge3$rmse1)

testMerge3$rmse1=as.numeric(as.character(testMerge3$rmse1))
testMerge3$rmse2=as.numeric(as.character(testMerge3$rmse2))
testMerge3$rmse3=as.numeric(as.character(testMerge3$rmse3))
testMerge3$rmse4=as.numeric(as.character(testMerge3$rmse4))


g1<-ggplot(testMerge3,aes(x = Longitude, y = Latitude,colour=rmse1,cex=2))+geom_point()+
  ggtitle("Spatial Model 1 RMSE by Station")+scale_size(guide=F)

g2<-ggplot(testMerge3,aes(x = Longitude, y = Latitude,colour=rmse2,cex=2))+geom_point()+
  ggtitle("Spatial Model 2 RMSE by Station")+scale_size(guide=F)

g3<-ggplot(testMerge3,aes(x = Longitude, y = Latitude,colour=rmse3,cex=2))+geom_point()+
  ggtitle("Spatial Model 3 RMSE by Station")+scale_size(guide=F)

g4<-ggplot(testMerge3,aes(x = Longitude, y = Latitude,colour=rmse4,cex=2))+geom_point()+
  ggtitle("Spatial Model 4 RMSE by Station")+scale_size(guide=F)

grid.arrange(g1,g2,g3,g4)

which(testMerge3$Latitude<37.8)

testMerge4=testMerge3[-1,]

g1<-ggplot(testMerge4,aes(x = Longitude, y = Latitude,colour=rmse1,cex=2))+geom_point()+
  ggtitle("Spatial Model 1 RMSE by Station")+scale_size(guide=F)

g2<-ggplot(testMerge4,aes(x = Longitude, y = Latitude,colour=rmse2,cex=2))+geom_point()+
  ggtitle("Spatial Model 2 RMSE by Station")+scale_size(guide=F)

g3<-ggplot(testMerge4,aes(x = Longitude, y = Latitude,colour=rmse3,cex=2))+geom_point()+
  ggtitle("Spatial Model 3 RMSE by Station")+scale_size(guide=F)

g4<-ggplot(testMerge4,aes(x = Longitude, y = Latitude,colour=rmse4,cex=2))+geom_point()+
  ggtitle("Spatial Model 4 RMSE by Station")+scale_size(guide=F)

grid.arrange(g1,g2,g3,g4)

## so that one station is really the cause of all the problems with the other models
## otherwise very similar across the models

setwd("~/Desktop/DS421_summerProject/GAM_weightedReg_comparison/shiny/data")
load("delt_map.RData")
plot(delt_map)
points(allData$Longitude,allData$Latitude,pch=19)

### rmse per station in individual models

setwd("~/Desktop/sfei")
load("perStationParsimoniousModels.Rda")
load("perStation.Rda")
load("perStationFullModels.Rda")
dim(perStation[[1]])
length(perStationParsMod[[1]]$fitted.values)
wholeSeries<-c(1, 2, 5, 7, 11, 13, 15, 16, 17, 18, 21, 22, 23, 29, 40)

rmsePerStationSep<-c()
for(i in wholeSeries){
  true=perStation[[i]]$chl
  fitted=predict(perStationParsMod[[i]],perStation[[i]],type="response")
 rmsePerStationSep=c(rmsePerStationSep, sqrt(sum((true-fitted)^2,na.rm=T)/sum(is.na(!fitted))))
} ## although not really a fair comparison, other covariates in there

rmsePerStationSep

load("perStationInteractionModels.Rda")
rmsePerStationSepPlain<-c()
for(i in wholeSeries){
  true=perStation[[i]]$chl
  fitted=as.vector(predict(perStationIntMod[[i]],perStation[[i]],type="response"))
  rmsePerStationSepPlain=c(rmsePerStationSepPlain, sqrt(sum((true-fitted)^2,na.rm=T)/sum(!is.na(fitted))))
}

rmsePerStationSepPlain


rmsePerStationFull<-c()
for(i in wholeSeries){
  true=perStation[[i]]$chl
  fitted=predict(perStationFullMod[[i]],perStation[[i]],type="response")
  rmsePerStationFull=c(rmsePerStationFull, sqrt(sum((true-fitted)^2,na.rm=T)/sum(is.na(!fitted))))
} ## although not really a fair comparison, other covariates in there

rmsePerStationFull

names(perStation)[wholeSeries]

rmseSep=as.data.frame(cbind(names(perStation)[wholeSeries],rmsePerStationFull,rmsePerStationSepPlain,rmsePerStationSep))
names(rmseSep)=c("station","rmseFull","rmseInt","rmsePars")
rmseSep$rmseFull=as.numeric(as.character(rmseSep$rmseFull))
rmseSep$rmseInt=as.numeric(as.character(rmseSep$rmseInt))
rmseSep$rmsePars=as.numeric(as.character(rmseSep$rmsePars))

rmseSep

testMerge5=merge(testMerge3,rmseSep,by.x="Station",by.y="station")

g1<-ggplot(testMerge5,aes(x = Longitude, y = Latitude,colour=rmse1,cex=2))+geom_point()+
  ggtitle("Spatial Model 1 RMSE by Station")+scale_size(guide=F)

g2<-ggplot(testMerge5,aes(x = Longitude, y = Latitude,colour=rmse2,cex=2))+geom_point()+
  ggtitle("Spatial Model 2 RMSE by Station")+scale_size(guide=F)

g3<-ggplot(testMerge5,aes(x = Longitude, y = Latitude,colour=rmseInt,cex=2))+geom_point()+
  ggtitle("Interaction Model RMSE by Station")+scale_size(guide=F)

g4<-ggplot(testMerge5,aes(x = Longitude, y = Latitude,colour=rmsePars,cex=2))+geom_point()+
  ggtitle("Parsimonious Model RMSE Intercepts by Station")+scale_size(guide=F)

grid.arrange(g1,g2,g3,g4)

which(testMerge$Latitude<37.8)

testMerge6=testMerge5[-1,]

g1<-ggplot(testMerge6,aes(x = Longitude, y = Latitude,colour=rmse1,cex=2))+geom_point()+
  ggtitle("Spatial Model 1 RMSE by Station")+scale_size(guide=F)

g2<-ggplot(testMerge6,aes(x = Longitude, y = Latitude,colour=rmse2,cex=2))+geom_point()+
  ggtitle("Spatial Model 2 RMSE by Station")+scale_size(guide=F)

g3<-ggplot(testMerge6,aes(x = Longitude, y = Latitude,colour=rmseInt,cex=2))+geom_point()+
  ggtitle("Interaction Model RMSE by Station")+scale_size(guide=F)

g4<-ggplot(testMerge6,aes(x = Longitude, y = Latitude,colour=rmsePars,cex=2))+geom_point()+
  ggtitle("Parsimonious Model RMSE Intercepts by Station")+scale_size(guide=F)

grid.arrange(g1,g2,g3,g4)
