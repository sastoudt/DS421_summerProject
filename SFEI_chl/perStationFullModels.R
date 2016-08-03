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

wholeSeries<-c(1, 2, 5, 7, 9, 11, 13, 15, 16, 17, 18, 21, 22, 23, 29, 40)

perStationFullMod=vector("list",length(perStation))

data=perStation[[1]]
sum(is.na(data$nh4))/nrow(data) ## go ahead and use nh4

gamP<-gam(chl~ti(doy,bs="cc")+ti(date_dec,bs="tp")+ti(pheo,bs="tp")+ti(tn,bs="tp")+ti(do_per,bs="tp")+
            ti(sio2,bs="tp")+ti(tp,bs="tp")+ti(tss,bs="tp")+ti(nh4,bs="tp"),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc")+ti(date_dec,bs="tp",k=10)+ti(pheo,bs="tp")+ti(tn,bs="tp")+ti(do_per,bs="tp",k=10)+
            ti(sio2,bs="tp")+ti(tp,bs="tp")+ti(tss,bs="tp")+ti(nh4,bs="tp"),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=10)+ti(date_dec,bs="tp",k=15)+ti(pheo,bs="tp")+ti(tn,bs="tp")+ti(do_per,bs="tp",k=10)+
            ti(sio2,bs="tp")+ti(tp,bs="tp")+ti(tss,bs="tp")+ti(nh4,bs="tp"),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=15)+ti(date_dec,bs="tp",k=20)+ti(pheo,bs="tp")+ti(tn,bs="tp")+ti(do_per,bs="tp",k=10)+
            ti(sio2,bs="tp")+ti(tp,bs="tp")+ti(tss,bs="tp")+ti(nh4,bs="tp"),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=15)+ti(date_dec,bs="tp",k=20)+ti(pheo,bs="tp",k=10)+ti(tn,bs="tp")+ti(do_per,bs="tp",k=15)+
            ti(sio2,bs="tp")+ti(tp,bs="tp")+ti(tss,bs="tp")+ti(nh4,bs="tp"),data=data,family=gaussian(link="log"))
gam.check(gamP) ## starting to get slow

gamP<-gam(chl~ti(doy,bs="cc",k=20)+ti(date_dec,bs="tp",k=25)+ti(pheo,bs="tp",k=15)+ti(tn,bs="tp",k=10)+ti(do_per,bs="tp",k=20)+
            ti(sio2,bs="tp",k=10)+ti(tp,bs="tp",k=10)+ti(tss,bs="tp",k=10)+ti(nh4,bs="tp",k=10),data=data,family=gaussian(link="log"))
gam.check(gamP) ## warnings

gamP<-gam(chl~ti(doy,bs="cc",k=20)+ti(date_dec,bs="tp",k=20)+ti(pheo,bs="tp",k=10)+ti(tn,bs="tp")+ti(do_per,bs="tp",k=15)+
            ti(sio2,bs="tp")+ti(tp,bs="tp")+ti(tss,bs="tp")+ti(nh4,bs="tp"),data=data,family=gaussian(link="log"))
gam.check(gamP) 

gamP<-gam(chl~ti(doy,bs="cc",k=20)+ti(date_dec,bs="tp",k=25)+ti(pheo,bs="tp",k=10)+ti(tn,bs="tp")+ti(do_per,bs="tp",k=15)+
            ti(sio2,bs="tp")+ti(tp,bs="tp")+ti(tss,bs="tp")+ti(nh4,bs="tp"),data=data,family=gaussian(link="log"))
gam.check(gamP) 


gamP<-gam(chl~ti(doy,bs="cc",k=20)+ti(date_dec,bs="tp",k=25)+ti(pheo,bs="tp",k=10)+ti(tn,bs="tp")+ti(do_per,bs="tp",k=15)+
            ti(sio2,bs="tp")+ti(tp,bs="tp")+ti(tss,bs="tp")+ti(nh4,bs="tp",k=10),data=data,family=gaussian(link="log"))
gam.check(gamP)  ## werid clustering thing that I don't quite trust, and everything was fine before

gamP<-gam(chl~ti(doy,bs="cc",k=20)+ti(date_dec,bs="tp",k=25)+ti(pheo,bs="tp",k=10)+ti(tn,bs="tp")+ti(do_per,bs="tp",k=15)+
            ti(sio2,bs="tp",k=10)+ti(tp,bs="tp")+ti(tss,bs="tp")+ti(nh4,bs="tp",k=10),data=data,family=gaussian(link="log"))
gam.check(gamP)  

gamP<-gam(chl~ti(doy,bs="cc",k=20)+ti(date_dec,bs="tp",k=25)+ti(pheo,bs="tp",k=10)+ti(tn,bs="tp",k=10)+ti(do_per,bs="tp",k=15)+
            ti(sio2,bs="tp",k=10)+ti(tp,bs="tp")+ti(tss,bs="tp")+ti(nh4,bs="tp",k=10),data=data,family=gaussian(link="log"))
gam.check(gamP)  ## yeah, going back to before

gamP<-gam(chl~ti(doy,bs="cc",k=20)+ti(date_dec,bs="tp",k=25)+ti(pheo,bs="tp",k=10)+ti(tn,bs="tp")+ti(do_per,bs="tp",k=15)+
            ti(sio2,bs="tp")+ti(tp,bs="tp")+ti(tss,bs="tp")+ti(nh4,bs="tp"),data=data,family=gaussian(link="log"))
gam.check(gamP) 


perStationFullMod[[1]]=gamP

####


