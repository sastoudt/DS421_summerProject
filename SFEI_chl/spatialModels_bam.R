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

## simplest way to add in station info
## all smooth terms the same, parametric component of station allows a different intercept per station
system.time(gamP<-bam(chl~as.factor(Station)+ti(doy,bs="cc")+ti(date_dec,bs="tp"),data=allData,family=gaussian(link="log"),control=ctrl))
gam.check(gamP)

system.time(gamP<-bam(chl~as.factor(Station)+ti(doy,bs="cc",k=150)+ti(date_dec,bs="tp",k=150),data=allData,family=gaussian(link="log"),control=ctrl))
gam.check(gamP) ## warnings

system.time(gamP<-bam(chl~as.factor(Station)+ti(doy,bs="cc",k=120)+ti(date_dec,bs="tp",k=120),data=allData,family=gaussian(link="log"),control=ctrl))
gam.check(gamP)  ## <1 minute, this is awesome

system.time(gamP<-bam(chl~as.factor(Station)+ti(doy,bs="cc",k=130)+ti(date_dec,bs="tp",k=130),data=allData,family=gaussian(link="log"),control=ctrl))
gam.check(gamP)  ## tightens up resids v. linear pred a bit

system.time(gamP<-bam(chl~as.factor(Station)+ti(doy,bs="cc",k=140)+ti(date_dec,bs="tp",k=140),data=allData,family=gaussian(link="log"),control=ctrl))
gam.check(gamP) ## warning, stick with the previous

system.time(gamP<-bam(chl~as.factor(Station)+ti(doy,bs="cc",k=130)+ti(date_dec,bs="tp",k=130),data=allData,family=gaussian(link="log"),control=ctrl))
gam.check(gamP) 

mod1=gamP
setwd("~/Desktop/sfei")
save(mod1,file="mod1Spatial.RData")

####
## allows the trend over time to differ by station
system.time(gamP<-bam(chl~as.factor(Station)+ti(doy,bs="cc")+ti(date_dec,bs="tp",by=as.factor(Station)),data=allData,family=gaussian(link="log"),control=ctrl))
gam.check(gamP)

system.time(gamP<-bam(chl~as.factor(Station)+ti(doy,bs="cc",k=100)+ti(date_dec,bs="tp",by=as.factor(Station),k=50),data=allData,family=gaussian(link="log"),control=ctrl))
gam.check(gamP) ## stopeed at 7  minutes

system.time(gamP<-bam(chl~as.factor(Station)+ti(doy,bs="cc",k=30)+ti(date_dec,bs="tp",by=as.factor(Station),k=30),data=allData,family=gaussian(link="log"),control=ctrl))
gam.check(gamP) #5.5 min

## save this for now, might have to put next iteration on cluster

mod2=gamP
save(mod2,file="mod2Spatial.RData")

####
## allow yearly seasonal trend to differ by station
system.time(gamP<-bam(chl~as.factor(Station)+ti(doy,bs="cc",by=as.factor(Station))+ti(date_dec,bs="tp",by=as.factor(Station)),data=allData,family=gaussian(link="log"),control=ctrl))
gam.check(gamP) #1.5 min

system.time(gamP<-bam(chl~as.factor(Station)+ti(doy,bs="cc",by=as.factor(Station),k=10)+ti(date_dec,bs="tp",by=as.factor(Station),k=10),data=allData,family=gaussian(link="log"),control=ctrl))
gam.check(gamP) # a little over a minute

system.time(gamP<-bam(chl~as.factor(Station)+ti(doy,bs="cc",by=as.factor(Station),k=15)+ti(date_dec,bs="tp",by=as.factor(Station),k=15),data=allData,family=gaussian(link="log"),control=ctrl))
gam.check(gamP) ## a little over 7 minutes

mod3=gamP
save(mod3,file="mod3Spatial.RData")

####
## dial back the doy but add interaction term that can vary by station
system.time(gamP<-bam(chl~as.factor(Station)+ti(doy,bs="cc")+ti(date_dec,bs="tp",by=as.factor(Station))+
            ti(doy,date_dec,by=as.factor(Station)),data=allData,family=gaussian(link="log"),control=ctrl))
gam.check(gamP) ## a little under 3 minutes

system.time(gamP2<-bam(chl~as.factor(Station)+ti(doy,bs="cc",k=10)+ti(date_dec,bs="tp",by=as.factor(Station),k=10)+
                        ti(doy,date_dec,by=as.factor(Station)),data=allData,family=gaussian(link="log"),control=ctrl))
gam.check(gamP2) ## a little over 8 min

## save this for now
mod4=gamP
save(mod4,file="mod4Spatial.RData")

load("mod1Spatial.RData")
load("mod2Spatial.RData")
load("mod3Spatial.RData")
load("mod4Spatial.RData")
allData$mod1Pred=predict(mod1,allData,type="response")
allData$mod2Pred=predict(mod2,allData,type="response")
allData$mod3Pred=predict(mod3,allData,type="response")
allData$mod4Pred=predict(mod4,allData,type="response")

length(mod1$fitted.values)
nrow(allData)

getwd()
write.csv(allData,"allData.csv",row.names=F)
