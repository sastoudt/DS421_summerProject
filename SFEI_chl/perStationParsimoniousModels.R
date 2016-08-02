setwd("~/Desktop/sfei")
load(file="perStation.Rda")
require(mgcv)

### "parsimonious model"

### look promising
## doy 
## date_dec
## pheo
## tn (monthly structure) 
## do_per (check context of the "per")

### Add these for "full model"

### of interest
## sio2
## tp
## tss
## nh4 (when data available)

perStationParsMod=vector("list",length(perStation))


data=perStation[[1]]

gamP<-gam(chl~ti(doy,bs="cc")+ti(date_dec,bs="tp")+ti(pheo,bs="tp")+ti(tn,bs="tp")+ti(do_per,bs="tp"),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=5)+ti(date_dec,bs="tp",k=10)+ti(pheo,bs="tp")+ti(tn,bs="tp",k=10)+ti(do_per,bs="tp",k=10),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=5)+ti(date_dec,bs="tp",k=10)+ti(pheo,bs="tp")+ti(tn,bs="tp",k=5)+ti(do_per,bs="tp",k=10),data=data,family=gaussian(link="log"))
gam.check(gamP)

perStationParsMod[[1]]=gamP

####

data=perStation[[2]]

gamP<-gam(chl~ti(doy,bs="cc")+ti(date_dec,bs="tp")+ti(pheo,bs="tp")+ti(tn,bs="tp")+ti(do_per,bs="tp"),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc")+ti(date_dec,bs="tp",k=10)+ti(pheo,bs="tp")+ti(tn,bs="tp")+ti(do_per,bs="tp"),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=10)+ti(date_dec,bs="tp",k=10)+ti(pheo,bs="tp",k=10)+ti(tn,bs="tp")+ti(do_per,bs="tp"),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=10)+ti(date_dec,bs="tp",k=20)+ti(pheo,bs="tp",k=10)+ti(tn,bs="tp")+ti(do_per,bs="tp"),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=10)+ti(date_dec,bs="tp",k=25)+ti(pheo,bs="tp",k=10)+ti(tn,bs="tp")+ti(do_per,bs="tp"),data=data,family=gaussian(link="log"))
gam.check(gamP)

perStationParsMod[[2]]=gamP ## some constant variance issues

####

data=perStation[[3]]

gamP<-gam(chl~ti(doy,bs="cc")+ti(date_dec,bs="tp")+ti(pheo,bs="tp")+ti(tn,bs="tp")+ti(do_per,bs="tp"),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=5)+ti(date_dec,bs="tp")+ti(pheo,bs="tp")+ti(tn,bs="tp")+ti(do_per,bs="tp",k=5),data=data,family=gaussian(link="log"))
gam.check(gamP) ## same

gamP<-gam(chl~ti(doy,bs="cc",k=10)+ti(date_dec,bs="tp")+ti(pheo,bs="tp")+ti(tn,bs="tp")+ti(do_per,bs="tp",k=10),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=15)+ti(date_dec,bs="tp",k=10)+ti(pheo,bs="tp",k=10)+ti(tn,bs="tp")+ti(do_per,bs="tp",k=15),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=15)+ti(date_dec,bs="tp",k=15)+ti(pheo,bs="tp",k=10)+ti(tn,bs="tp")+ti(do_per,bs="tp",k=15),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=15)+ti(date_dec,bs="tp",k=15)+ti(pheo,bs="tp",k=10)+ti(tn,bs="tp",k=10)+ti(do_per,bs="tp",k=15),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=15)+ti(date_dec,bs="tp",k=15)+ti(pheo,bs="tp",k=15)+ti(tn,bs="tp",k=15)+ti(do_per,bs="tp",k=20),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=20)+ti(date_dec,bs="tp",k=20)+ti(pheo,bs="tp",k=20)+ti(tn,bs="tp",k=20)+ti(do_per,bs="tp",k=25),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=20)+ti(date_dec,bs="tp",k=25)+ti(pheo,bs="tp",k=20)+ti(tn,bs="tp",k=20)+ti(do_per,bs="tp",k=30),data=data,family=gaussian(link="log"))
gam.check(gamP)

perStationParsMod[[3]]=gamP 
