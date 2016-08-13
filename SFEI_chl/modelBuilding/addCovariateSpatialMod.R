## spatial models with bam

setwd("~/Desktop/sfei")
require(mgcv)
load("perStation.Rda")

names(perStation[[1]])
wholeSeries<-c(1, 2, 5, 7, 11, 13, 15, 16, 17, 18, 21, 22, 23, 29, 40)

allData<- do.call("rbind", perStation[wholeSeries])
names(allData)

class(allData$Station) ## Need to make a factor

ctrl <- list(nthreads=4)

system.time(gamP<-bam(chl~as.factor(Station)+ti(doy,bs="cc",by=as.factor(Station))+ti(date_dec,bs="tp",by=as.factor(Station))+ti(pheo,bs="tp"),data=allData,family=gaussian(link="log"),control=ctrl))
gam.check(gamP) ## 30 seconds

system.time(gamP<-bam(chl~as.factor(Station)+ti(doy,bs="cc",by=as.factor(Station))+ti(date_dec,bs="tp",by=as.factor(Station))+ti(pheo,bs="tp",k=10),data=allData,family=gaussian(link="log"),control=ctrl))
gam.check(gamP) 

system.time(gamP<-bam(chl~as.factor(Station)+ti(doy,bs="cc",by=as.factor(Station))+ti(date_dec,bs="tp",by=as.factor(Station))+ti(pheo,bs="tp",k=20),data=allData,family=gaussian(link="log"),control=ctrl))
gam.check(gamP)  ## warning

system.time(gamP<-bam(chl~as.factor(Station)+ti(doy,bs="cc",by=as.factor(Station))+ti(date_dec,bs="tp",by=as.factor(Station))+ti(pheo,bs="tp",k=15),data=allData,family=gaussian(link="log"),control=ctrl))
gam.check(gamP) ## warning

system.time(gamP<-bam(chl~as.factor(Station)+ti(doy,bs="cc",by=as.factor(Station))+ti(date_dec,bs="tp",by=as.factor(Station),k=10)+ti(pheo,bs="tp",k=10),data=allData,family=gaussian(link="log"),control=ctrl))
gam.check(gamP) 

system.time(gamP<-bam(chl~as.factor(Station)+ti(doy,bs="cc",by=as.factor(Station),k=10)+ti(date_dec,bs="tp",by=as.factor(Station),k=10)+ti(pheo,bs="tp",k=10),data=allData,family=gaussian(link="log"),control=ctrl))
gam.check(gamP)  ## Just under a minute

## pheo helps make this look pretty decent without getting too big

## pheo by station
system.time(gamP2<-bam(chl~as.factor(Station)+ti(doy,bs="cc",by=as.factor(Station))+ti(date_dec,bs="tp",by=as.factor(Station))+ti(pheo,bs="tp",by=as.factor(Station)),data=allData,family=gaussian(link="log"),control=ctrl))
gam.check(gamP2)  ## stopped after 5 minutes


## try tn
system.time(gamP3<-bam(chl~as.factor(Station)+ti(doy,bs="cc",by=as.factor(Station),k=10)+ti(date_dec,bs="tp",by=as.factor(Station),k=10)+ti(tn,bs="tp",k=10),data=allData,family=gaussian(link="log"),control=ctrl))
gam.check(gamP3) ## under 2 minutes

##
require(dplyr)
pheoUse=na.omit(allData[,c("Station","chl","pheo","doy","date_dec")])
pheoPred=predict(gamP,pheoUse,type="response")
pheoUse$resid=(pheoUse$chl-pheoPred)^2

byStation<-group_by(pheoUse,Station)
rmsePerStation<-summarise(byStation,stp1=sum(resid),count=n())
rmsePheo=sqrt(as.vector(rmsePerStation$stp1)/rmsePerStation$count)

tnUse=na.omit(allData[,c("Station","chl","tn","doy","date_dec")])
tnPred=predict(gamP3,tnUse,type="response")
tnUse$resid=(tnUse$chl-tnPred)^2

byStation<-group_by(tnUse,Station)
rmsePerStation<-summarise(byStation,stp1=sum(resid),count=n())
rmseTn=sqrt(as.vector(rmsePerStation$stp1)/rmsePerStation$count)

cbind(rmsePheo,rmseTn)
