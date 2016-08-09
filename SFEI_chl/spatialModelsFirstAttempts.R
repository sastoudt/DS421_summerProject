setwd("~/Desktop/sfei")
require(mgcv)
load("perStation.Rda")

names(perStation[[1]])
wholeSeries<-c(1, 2, 5, 7, 11, 13, 15, 16, 17, 18, 21, 22, 23, 29, 40)


allData<- do.call("rbind", perStation[wholeSeries])
names(allData)

class(allData$Station) ## Need to make a factor

## simplest way to add in station info
## all smooth terms the same, parametric component of station allows a different intercept per station
gamP<-gam(chl~as.factor(Station)+ti(doy,bs="cc")+ti(date_dec,bs="tp"),data=allData,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~as.factor(Station)+ti(doy,bs="cc",k=20)+ti(date_dec,bs="tp",k=20),data=allData,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~as.factor(Station)+ti(doy,bs="cc",k=25)+ti(date_dec,bs="tp",k=30),data=allData,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~as.factor(Station)+ti(doy,bs="cc",k=40)+ti(date_dec,bs="tp",k=40),data=allData,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~as.factor(Station)+ti(doy,bs="cc",k=60)+ti(date_dec,bs="tp",k=60),data=allData,family=gaussian(link="log"))
gam.check(gamP)

ptm <- proc.time()
gamP<-gam(chl~as.factor(Station)+ti(doy,bs="cc",k=80)+ti(date_dec,bs="tp",k=80),data=allData,family=gaussian(link="log"))
proc.time() - ptm ## almost 4 minutes
gam.check(gamP)

ptm <- proc.time()
gamP<-gam(chl~as.factor(Station)+ti(doy,bs="cc",k=100)+ti(date_dec,bs="tp",k=100),data=allData,family=gaussian(link="log"))
proc.time() - ptm ## ## had to stop this
gam.check(gamP)


####
## allows the trend over time to differ by station
gamP<-gam(chl~as.factor(Station)+ti(doy,bs="cc")+ti(date_dec,bs="tp",by=as.factor(Station)),data=allData,family=gaussian(link="log"))
gam.check(gamP)

ptm <- proc.time()
gamP<-gam(chl~as.factor(Station)+ti(doy,bs="cc",k=10)+ti(date_dec,bs="tp",by=as.factor(Station),k=7),data=allData,family=gaussian(link="log"))
proc.time() - ptm #2.5 minutes
gam.check(gamP)


####
## allow yearly seasonal trend to differ by station
gamP<-gam(chl~as.factor(Station)+ti(doy,bs="cc",by=as.factor(Station))+ti(date_dec,bs="tp",by=as.factor(Station)),data=allData,family=gaussian(link="log"))
gam.check(gamP)

ptm <- proc.time()
gamP<-gam(chl~as.factor(Station)+ti(doy,bs="cc",by=as.factor(Station),k=7)+ti(date_dec,bs="tp",by=as.factor(Station),k=7),data=allData,family=gaussian(link="log"))
proc.time() - ptm ## over 7 minutes
gam.check(gamP)

####
## dial back the doy but add interaction term that can vary by station
gamP<-gam(chl~as.factor(Station)+ti(doy,bs="cc")+ti(date_dec,bs="tp",by=as.factor(Station))+
            ti(doy,date_dec,by=as.factor(Station)),data=allData,family=gaussian(link="log"))
gam.check(gamP) ## had to stop for time issues

## going to try to remember how to do stuff on the cluster, 
## but until then let's try to break things up and see if that helps

setwd("~/Desktop/DS421_summerProject/GAM_weightedReg_comparison/shiny/data")
load("delt_map.RData")

require(sp)
plot(delt_map)
points(allData$Longitude,allData$Latitude,pch=19)

plot(allData$Longitude,allData$Latitude,pch=19)

## based on visual inspection "natural" groupings
g1<-subset(allData,Station %in% c("D10","D6","D7","D8"))
g2<-subset(allData, Station %in% c("D12","D22","D4"))
g3<-subset(allData,Station %in% c("D19","D26","D28A"))

## do C10, C3, D41 seperately

gamP<-gam(chl~as.factor(Station)+ti(doy,bs="cc",k=60)+ti(date_dec,bs="tp",k=60),data=g1,family=gaussian(link="log"))
gam.check(gamP)

ptm <- proc.time()
gamP<-gam(chl~as.factor(Station)+ti(doy,bs="cc",k=100)+ti(date_dec,bs="tp",k=100),data=g1,family=gaussian(link="log"))
proc.time() - ptm ## a minute
gam.check(gamP)

ptm <- proc.time()
gamP<-gam(chl~as.factor(Station)+ti(doy,bs="cc",k=100)+ti(date_dec,bs="tp",k=150),data=g1,family=gaussian(link="log"))
proc.time() - ptm ## warning, 3.5ish minutes
gam.check(gamP)

## this seems to be more feasible, check for more complicated models

## allows the trend over time to differ by station
gamP<-gam(chl~as.factor(Station)+ti(doy,bs="cc")+ti(date_dec,bs="tp",by=as.factor(Station)),data=g1,family=gaussian(link="log"))
gam.check(gamP)

ptm <- proc.time()
gamP<-gam(chl~as.factor(Station)+ti(doy,bs="cc",k=50)+ti(date_dec,bs="tp",by=as.factor(Station),k=50),data=g1,family=gaussian(link="log"))
proc.time() - ptm ## <2 minutes
gam.check(gamP)

## this is feasible, keep checking more complicated models
gamP<-gam(chl~as.factor(Station)+ti(doy,bs="cc",by=as.factor(Station))+ti(date_dec,bs="tp",by=as.factor(Station)),data=g1,family=gaussian(link="log"))
gam.check(gamP)

ptm <- proc.time()
gamP<-gam(chl~as.factor(Station)+ti(doy,bs="cc",by=as.factor(Station),k=50)+ti(date_dec,bs="tp",by=as.factor(Station),k=50),data=g1,family=gaussian(link="log"))
proc.time() - ptm ## stopped, this is getting too long
gam.check(gamP)

gamP<-gam(chl~as.factor(Station)+ti(doy,bs="cc")+ti(date_dec,bs="tp",by=as.factor(Station))+
            ti(doy,date_dec,by=as.factor(Station)),data=g1,family=gaussian(link="log"))
gam.check(gamP) 

ptm <- proc.time()
gamP<-gam(chl~as.factor(Station)+ti(doy,bs="cc",k=10)+ti(date_dec,bs="tp",by=as.factor(Station))+
            ti(doy,date_dec,by=as.factor(Station),k=7),data=g1,family=gaussian(link="log"))
proc.time() - ptm #<1min
gam.check(gamP) 


ptm <- proc.time()
gamP<-gam(chl~as.factor(Station)+ti(doy,bs="cc",k=30)+ti(date_dec,bs="tp",by=as.factor(Station),k=10)+
            ti(doy,date_dec,by=as.factor(Station),k=7),data=g1,family=gaussian(link="log"))
proc.time() - ptm #<1min
gam.check(gamP) 

ptm <- proc.time()
gamP<-gam(chl~as.factor(Station)+ti(doy,bs="cc",k=40)+ti(date_dec,bs="tp",by=as.factor(Station),k=20)+
            ti(doy,date_dec,by=as.factor(Station),k=7),data=g1,family=gaussian(link="log"))
proc.time() - ptm ## stopped, taking too long
gam.check(gamP) 

## so here we have the constraints on my laptop, going to run really big models on the cluster, save them,
## and then move them back to check, hopefully won't have to do this too many times. 
