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

data=perStation[[2]]
sum(is.na(data$nh4))/nrow(data) ## go ahead and use nh4

gamP<-gam(chl~ti(doy,bs="cc")+ti(date_dec,bs="tp")+ti(pheo,bs="tp")+ti(tn,bs="tp")+ti(do_per,bs="tp")+
            ti(sio2,bs="tp")+ti(tp,bs="tp")+ti(tss,bs="tp")+ti(nh4,bs="tp"),data=data,family=gaussian(link="log"))
gam.check(gamP)


gamP<-gam(chl~ti(doy,bs="cc")+ti(date_dec,bs="tp",k=10)+ti(pheo,bs="tp")+ti(tn,bs="tp")+ti(do_per,bs="tp")+
            ti(sio2,bs="tp")+ti(tp,bs="tp")+ti(tss,bs="tp")+ti(nh4,bs="tp"),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=10)+ti(date_dec,bs="tp",k=15)+ti(pheo,bs="tp")+ti(tn,bs="tp")+ti(do_per,bs="tp")+
            ti(sio2,bs="tp")+ti(tp,bs="tp")+ti(tss,bs="tp")+ti(nh4,bs="tp"),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=10)+ti(date_dec,bs="tp",k=15)+ti(pheo,bs="tp")+ti(tn,bs="tp")+ti(do_per,bs="tp")+
            ti(sio2,bs="tp")+ti(tp,bs="tp",k=10)+ti(tss,bs="tp")+ti(nh4,bs="tp"),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=10)+ti(date_dec,bs="tp",k=15)+ti(pheo,bs="tp")+ti(tn,bs="tp",k=10)+ti(do_per,bs="tp")+
            ti(sio2,bs="tp")+ti(tp,bs="tp",k=10)+ti(tss,bs="tp")+ti(nh4,bs="tp"),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=10)+ti(date_dec,bs="tp",k=15)+ti(pheo,bs="tp")+ti(tn,bs="tp",k=15)+ti(do_per,bs="tp",k=10)+
            ti(sio2,bs="tp")+ti(tp,bs="tp",k=10)+ti(tss,bs="tp")+ti(nh4,bs="tp"),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=10)+ti(date_dec,bs="tp",k=15)+ti(pheo,bs="tp",k=10)+ti(tn,bs="tp",k=15)+ti(do_per,bs="tp",k=10)+
            ti(sio2,bs="tp")+ti(tp,bs="tp",k=15)+ti(tss,bs="tp")+ti(nh4,bs="tp"),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=10)+ti(date_dec,bs="tp",k=15)+ti(pheo,bs="tp",k=10)+ti(tn,bs="tp",k=15)+ti(do_per,bs="tp",k=15)+
            ti(sio2,bs="tp")+ti(tp,bs="tp",k=15)+ti(tss,bs="tp",k=10)+ti(nh4,bs="tp"),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=10)+ti(date_dec,bs="tp",k=15)+ti(pheo,bs="tp",k=15)+ti(tn,bs="tp",k=15)+ti(do_per,bs="tp",k=15)+
            ti(sio2,bs="tp")+ti(tp,bs="tp",k=15)+ti(tss,bs="tp",k=10)+ti(nh4,bs="tp"),data=data,family=gaussian(link="log"))
gam.check(gamP) ## this looks good, try one more expansion

gamP<-gam(chl~ti(doy,bs="cc",k=10)+ti(date_dec,bs="tp",k=15)+ti(pheo,bs="tp",k=15)+ti(tn,bs="tp",k=15)+ti(do_per,bs="tp",k=15)+
            ti(sio2,bs="tp")+ti(tp,bs="tp",k=15)+ti(tss,bs="tp",k=10)+ti(nh4,bs="tp",k=10),data=data,family=gaussian(link="log"))
gam.check(gamP) ## didn't make any real change

gamP<-gam(chl~ti(doy,bs="cc",k=10)+ti(date_dec,bs="tp",k=15)+ti(pheo,bs="tp",k=15)+ti(tn,bs="tp",k=15)+ti(do_per,bs="tp",k=15)+
            ti(sio2,bs="tp")+ti(tp,bs="tp",k=15)+ti(tss,bs="tp",k=10)+ti(nh4,bs="tp"),data=data,family=gaussian(link="log"))
gam.check(gamP) ## go with this

perStationFullMod[[2]]=gamP

#####

data=perStation[[5]]
sum(is.na(data$nh4))/nrow(data) ## don't use nh4, over half missing

gamP<-gam(chl~ti(doy,bs="cc")+ti(date_dec,bs="tp")+ti(pheo,bs="tp")+ti(tn,bs="tp")+ti(do_per,bs="tp")+
            ti(sio2,bs="tp")+ti(tp,bs="tp")+ti(tss,bs="tp"),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc")+ti(date_dec,bs="tp",k=10)+ti(pheo,bs="tp",k=10)+ti(tn,bs="tp")+ti(do_per,bs="tp")+
            ti(sio2,bs="tp",k=10)+ti(tp,bs="tp",k=10)+ti(tss,bs="tp",k=10),data=data,family=gaussian(link="log"))
gam.check(gamP) ## warnings

gamP<-gam(chl~ti(doy,bs="cc")+ti(date_dec,bs="tp",k=10)+ti(pheo,bs="tp")+ti(tn,bs="tp")+ti(do_per,bs="tp")+
            ti(sio2,bs="tp")+ti(tp,bs="tp")+ti(tss,bs="tp"),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc")+ti(date_dec,bs="tp",k=15)+ti(pheo,bs="tp")+ti(tn,bs="tp")+ti(do_per,bs="tp")+
            ti(sio2,bs="tp")+ti(tp,bs="tp")+ti(tss,bs="tp"),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc")+ti(date_dec,bs="tp",k=15)+ti(pheo,bs="tp")+ti(tn,bs="tp")+ti(do_per,bs="tp")+
            ti(sio2,bs="tp",k=10)+ti(tp,bs="tp")+ti(tss,bs="tp"),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc")+ti(date_dec,bs="tp",k=15)+ti(pheo,bs="tp")+ti(tn,bs="tp")+ti(do_per,bs="tp")+
            ti(sio2,bs="tp",k=15)+ti(tp,bs="tp")+ti(tss,bs="tp"),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc")+ti(date_dec,bs="tp",k=15)+ti(pheo,bs="tp",k=10)+ti(tn,bs="tp")+ti(do_per,bs="tp")+
            ti(sio2,bs="tp",k=15)+ti(tp,bs="tp")+ti(tss,bs="tp"),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc")+ti(date_dec,bs="tp",k=15)+ti(pheo,bs="tp",k=15)+ti(tn,bs="tp")+ti(do_per,bs="tp")+
            ti(sio2,bs="tp",k=15)+ti(tp,bs="tp")+ti(tss,bs="tp"),data=data,family=gaussian(link="log"))
gam.check(gamP) ## warnings

gamP<-gam(chl~ti(doy,bs="cc")+ti(date_dec,bs="tp",k=15)+ti(pheo,bs="tp",k=12)+ti(tn,bs="tp")+ti(do_per,bs="tp")+
            ti(sio2,bs="tp",k=15)+ti(tp,bs="tp")+ti(tss,bs="tp"),data=data,family=gaussian(link="log"))
gam.check(gamP) ## warnings

gamP<-gam(chl~ti(doy,bs="cc")+ti(date_dec,bs="tp",k=15)+ti(pheo,bs="tp",k=11)+ti(tn,bs="tp")+ti(do_per,bs="tp")+
            ti(sio2,bs="tp",k=15)+ti(tp,bs="tp")+ti(tss,bs="tp"),data=data,family=gaussian(link="log"))
gam.check(gamP) 

gamP<-gam(chl~ti(doy,bs="cc")+ti(date_dec,bs="tp",k=15)+ti(pheo,bs="tp",k=11)+ti(tn,bs="tp",k=10)+ti(do_per,bs="tp")+
            ti(sio2,bs="tp",k=15)+ti(tp,bs="tp")+ti(tss,bs="tp"),data=data,family=gaussian(link="log"))
gam.check(gamP) 

gamP<-gam(chl~ti(doy,bs="cc")+ti(date_dec,bs="tp",k=15)+ti(pheo,bs="tp",k=11)+ti(tn,bs="tp",k=10)+ti(do_per,bs="tp")+
            ti(sio2,bs="tp",k=15)+ti(tp,bs="tp")+ti(tss,bs="tp",k=10),data=data,family=gaussian(link="log"))
gam.check(gamP)  ## warnings

gamP<-gam(chl~ti(doy,bs="cc")+ti(date_dec,bs="tp",k=15)+ti(pheo,bs="tp",k=11)+ti(tn,bs="tp",k=10)+ti(do_per,bs="tp")+
            ti(sio2,bs="tp",k=15)+ti(tp,bs="tp")+ti(tss,bs="tp",k=8),data=data,family=gaussian(link="log"))
gam.check(gamP) ## warnings

gamP<-gam(chl~ti(doy,bs="cc")+ti(date_dec,bs="tp",k=15)+ti(pheo,bs="tp",k=11)+ti(tn,bs="tp",k=10)+ti(do_per,bs="tp")+
            ti(sio2,bs="tp",k=15)+ti(tp,bs="tp")+ti(tss,bs="tp",k=7),data=data,family=gaussian(link="log"))
gam.check(gamP) ## warnings

gamP<-gam(chl~ti(doy,bs="cc")+ti(date_dec,bs="tp",k=15)+ti(pheo,bs="tp",k=11)+ti(tn,bs="tp",k=10)+ti(do_per,bs="tp")+
            ti(sio2,bs="tp",k=15)+ti(tp,bs="tp")+ti(tss,bs="tp",k=6),data=data,family=gaussian(link="log"))
gam.check(gamP) ## stuck at k=6 for tss 

perStationFullMod[[5]]=gamP

#### 
data=perStation[[7]]
sum(is.na(data$nh4))/nrow(data) ## don't use nh4, over half missing

gamP<-gam(chl~ti(doy,bs="cc")+ti(date_dec,bs="tp")+ti(pheo,bs="tp")+ti(tn,bs="tp")+ti(do_per,bs="tp")+
            ti(sio2,bs="tp")+ti(tp,bs="tp")+ti(tss,bs="tp"),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc")+ti(date_dec,bs="tp",k=10)+ti(pheo,bs="tp")+ti(tn,bs="tp")+ti(do_per,bs="tp")+
            ti(sio2,bs="tp")+ti(tp,bs="tp")+ti(tss,bs="tp"),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc")+ti(date_dec,bs="tp",k=15)+ti(pheo,bs="tp")+ti(tn,bs="tp")+ti(do_per,bs="tp")+
            ti(sio2,bs="tp")+ti(tp,bs="tp")+ti(tss,bs="tp"),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc")+ti(date_dec,bs="tp",k=15)+ti(pheo,bs="tp")+ti(tn,bs="tp")+ti(do_per,bs="tp")+
            ti(sio2,bs="tp",k=10)+ti(tp,bs="tp",k=10)+ti(tss,bs="tp"),data=data,family=gaussian(link="log"))
gam.check(gamP) ## warnings

gamP<-gam(chl~ti(doy,bs="cc")+ti(date_dec,bs="tp",k=15)+ti(pheo,bs="tp")+ti(tn,bs="tp")+ti(do_per,bs="tp")+
            ti(sio2,bs="tp")+ti(tp,bs="tp",k=10)+ti(tss,bs="tp"),data=data,family=gaussian(link="log"))
gam.check(gamP) ## warnings

gamP<-gam(chl~ti(doy,bs="cc")+ti(date_dec,bs="tp",k=15)+ti(pheo,bs="tp")+ti(tn,bs="tp")+ti(do_per,bs="tp")+
            ti(sio2,bs="tp",k=10)+ti(tp,bs="tp")+ti(tss,bs="tp"),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc")+ti(date_dec,bs="tp",k=15)+ti(pheo,bs="tp")+ti(tn,bs="tp")+ti(do_per,bs="tp")+
            ti(sio2,bs="tp",k=15)+ti(tp,bs="tp")+ti(tss,bs="tp"),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc")+ti(date_dec,bs="tp",k=15)+ti(pheo,bs="tp")+ti(tn,bs="tp")+ti(do_per,bs="tp")+
            ti(sio2,bs="tp",k=15)+ti(tp,bs="tp",k=8)+ti(tss,bs="tp"),data=data,family=gaussian(link="log"))
gam.check(gamP) ## warnings

gamP<-gam(chl~ti(doy,bs="cc")+ti(date_dec,bs="tp",k=15)+ti(pheo,bs="tp")+ti(tn,bs="tp")+ti(do_per,bs="tp")+
            ti(sio2,bs="tp",k=15)+ti(tp,bs="tp",k=7)+ti(tss,bs="tp"),data=data,family=gaussian(link="log"))
gam.check(gamP)


gamP<-gam(chl~ti(doy,bs="cc")+ti(date_dec,bs="tp",k=15)+ti(pheo,bs="tp")+ti(tn,bs="tp")+ti(do_per,bs="tp",k=10)+
            ti(sio2,bs="tp",k=15)+ti(tp,bs="tp",k=7)+ti(tss,bs="tp"),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=10)+ti(date_dec,bs="tp",k=15)+ti(pheo,bs="tp")+ti(tn,bs="tp")+ti(do_per,bs="tp",k=10)+
            ti(sio2,bs="tp",k=15)+ti(tp,bs="tp",k=7)+ti(tss,bs="tp"),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=15)+ti(date_dec,bs="tp",k=15)+ti(pheo,bs="tp")+ti(tn,bs="tp")+ti(do_per,bs="tp",k=10)+
            ti(sio2,bs="tp",k=15)+ti(tp,bs="tp",k=7)+ti(tss,bs="tp"),data=data,family=gaussian(link="log"))
gam.check(gamP)


gamP<-gam(chl~ti(doy,bs="cc",k=15)+ti(date_dec,bs="tp",k=20)+ti(pheo,bs="tp")+ti(tn,bs="tp")+ti(do_per,bs="tp",k=10)+
            ti(sio2,bs="tp",k=15)+ti(tp,bs="tp",k=7)+ti(tss,bs="tp"),data=data,family=gaussian(link="log"))
gam.check(gamP)

perStationFullMod[[7]]=gamP

####
data=perStation[[9]]
sum(is.na(data$nh4))/nrow(data) ## use, about 14% missing

gamP<-gam(chl~ti(doy,bs="cc")+ti(date_dec,bs="tp")+ti(pheo,bs="tp")+ti(tn,bs="tp")+ti(do_per,bs="tp")+
            ti(sio2,bs="tp")+ti(tp,bs="tp")+ti(tss,bs="tp")+ti(nh4,bs="tp"),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc")+ti(date_dec,bs="tp",k=10)+ti(pheo,bs="tp")+ti(tn,bs="tp")+ti(do_per,bs="tp")+
            ti(sio2,bs="tp")+ti(tp,bs="tp")+ti(tss,bs="tp")+ti(nh4,bs="tp"),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc")+ti(date_dec,bs="tp",k=15)+ti(pheo,bs="tp")+ti(tn,bs="tp")+ti(do_per,bs="tp")+
            ti(sio2,bs="tp")+ti(tp,bs="tp")+ti(tss,bs="tp")+ti(nh4,bs="tp"),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc")+ti(date_dec,bs="tp",k=20)+ti(pheo,bs="tp")+ti(tn,bs="tp")+ti(do_per,bs="tp")+
            ti(sio2,bs="tp")+ti(tp,bs="tp")+ti(tss,bs="tp")+ti(nh4,bs="tp"),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc")+ti(date_dec,bs="tp",k=20)+ti(pheo,bs="tp")+ti(tn,bs="tp")+ti(do_per,bs="tp",k=10)+
            ti(sio2,bs="tp")+ti(tp,bs="tp")+ti(tss,bs="tp")+ti(nh4,bs="tp"),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc")+ti(date_dec,bs="tp",k=20)+ti(pheo,bs="tp")+ti(tn,bs="tp")+ti(do_per,bs="tp",k=10)+
            ti(sio2,bs="tp")+ti(tp,bs="tp",k=10)+ti(tss,bs="tp")+ti(nh4,bs="tp"),data=data,family=gaussian(link="log"))
gam.check(gamP) ## warning

gamP<-gam(chl~ti(doy,bs="cc")+ti(date_dec,bs="tp",k=20)+ti(pheo,bs="tp")+ti(tn,bs="tp")+ti(do_per,bs="tp",k=10)+
            ti(sio2,bs="tp")+ti(tp,bs="tp",k=8)+ti(tss,bs="tp")+ti(nh4,bs="tp"),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc")+ti(date_dec,bs="tp",k=20)+ti(pheo,bs="tp")+ti(tn,bs="tp")+ti(do_per,bs="tp",k=10)+
            ti(sio2,bs="tp")+ti(tp,bs="tp",k=9)+ti(tss,bs="tp")+ti(nh4,bs="tp"),data=data,family=gaussian(link="log"))
gam.check(gamP) ## warning

gamP<-gam(chl~ti(doy,bs="cc")+ti(date_dec,bs="tp",k=20)+ti(pheo,bs="tp")+ti(tn,bs="tp")+ti(do_per,bs="tp",k=10)+
            ti(sio2,bs="tp",k=10)+ti(tp,bs="tp",k=8)+ti(tss,bs="tp")+ti(nh4,bs="tp"),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc")+ti(date_dec,bs="tp",k=20)+ti(pheo,bs="tp",k=10)+ti(tn,bs="tp")+ti(do_per,bs="tp",k=10)+
            ti(sio2,bs="tp",k=10)+ti(tp,bs="tp",k=8)+ti(tss,bs="tp")+ti(nh4,bs="tp"),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc")+ti(date_dec,bs="tp",k=20)+ti(pheo,bs="tp",k=15)+ti(tn,bs="tp")+ti(do_per,bs="tp",k=10)+
            ti(sio2,bs="tp",k=10)+ti(tp,bs="tp",k=8)+ti(tss,bs="tp")+ti(nh4,bs="tp"),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc")+ti(date_dec,bs="tp",k=20)+ti(pheo,bs="tp",k=15)+ti(tn,bs="tp")+ti(do_per,bs="tp",k=10)+
            ti(sio2,bs="tp",k=10)+ti(tp,bs="tp",k=8)+ti(tss,bs="tp")+ti(nh4,bs="tp",k=10),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc")+ti(date_dec,bs="tp",k=25)+ti(pheo,bs="tp",k=15)+ti(tn,bs="tp")+ti(do_per,bs="tp",k=10)+
            ti(sio2,bs="tp",k=10)+ti(tp,bs="tp",k=8)+ti(tss,bs="tp")+ti(nh4,bs="tp",k=10),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc")+ti(date_dec,bs="tp",k=25)+ti(pheo,bs="tp",k=15)+ti(tn,bs="tp")+ti(do_per,bs="tp",k=10)+
            ti(sio2,bs="tp",k=15)+ti(tp,bs="tp",k=8)+ti(tss,bs="tp")+ti(nh4,bs="tp",k=10),data=data,family=gaussian(link="log"))
gam.check(gamP)

perStationFullMod[[9]]=gamP

####
data=perStation[[11]]
sum(is.na(data$nh4))/nrow(data) ## use

gamP<-gam(chl~ti(doy,bs="cc")+ti(date_dec,bs="tp")+ti(pheo,bs="tp")+ti(tn,bs="tp")+ti(do_per,bs="tp")+
            ti(sio2,bs="tp")+ti(tp,bs="tp")+ti(tss,bs="tp")+ti(nh4,bs="tp"),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc")+ti(date_dec,bs="tp",k=10)+ti(pheo,bs="tp")+ti(tn,bs="tp")+ti(do_per,bs="tp")+
            ti(sio2,bs="tp")+ti(tp,bs="tp")+ti(tss,bs="tp")+ti(nh4,bs="tp"),data=data,family=gaussian(link="log"))
gam.check(gamP) ## uh oh, weird plots

gamP<-gam(chl~ti(doy,bs="cc")+ti(date_dec,bs="tp",k=15)+ti(pheo,bs="tp")+ti(tn,bs="tp",k=10)+ti(do_per,bs="tp")+
            ti(sio2,bs="tp")+ti(tp,bs="tp")+ti(tss,bs="tp")+ti(nh4,bs="tp"),data=data,family=gaussian(link="log"))
gam.check(gamP) ## good, back to normal

gamP<-gam(chl~ti(doy,bs="cc",k=10)+ti(date_dec,bs="tp",k=15)+ti(pheo,bs="tp")+ti(tn,bs="tp",k=10)+ti(do_per,bs="tp")+
            ti(sio2,bs="tp")+ti(tp,bs="tp")+ti(tss,bs="tp")+ti(nh4,bs="tp"),data=data,family=gaussian(link="log"))
gam.check(gamP) 

gamP<-gam(chl~ti(doy,bs="cc",k=15)+ti(date_dec,bs="tp",k=20)+ti(pheo,bs="tp")+ti(tn,bs="tp",k=10)+ti(do_per,bs="tp")+
            ti(sio2,bs="tp")+ti(tp,bs="tp")+ti(tss,bs="tp")+ti(nh4,bs="tp"),data=data,family=gaussian(link="log"))
gam.check(gamP) 

gamP<-gam(chl~ti(doy,bs="cc",k=15)+ti(date_dec,bs="tp",k=20)+ti(pheo,bs="tp",k=10)+ti(tn,bs="tp",k=10)+ti(do_per,bs="tp")+
            ti(sio2,bs="tp")+ti(tp,bs="tp")+ti(tss,bs="tp")+ti(nh4,bs="tp"),data=data,family=gaussian(link="log"))
gam.check(gamP)  ## warning

gamP<-gam(chl~ti(doy,bs="cc",k=15)+ti(date_dec,bs="tp",k=20)+ti(pheo,bs="tp",k=8)+ti(tn,bs="tp",k=10)+ti(do_per,bs="tp")+
            ti(sio2,bs="tp")+ti(tp,bs="tp")+ti(tss,bs="tp")+ti(nh4,bs="tp"),data=data,family=gaussian(link="log"))
gam.check(gamP) ## warning

gamP<-gam(chl~ti(doy,bs="cc",k=15)+ti(date_dec,bs="tp",k=20)+ti(pheo,bs="tp",k=7)+ti(tn,bs="tp",k=10)+ti(do_per,bs="tp")+
            ti(sio2,bs="tp")+ti(tp,bs="tp")+ti(tss,bs="tp")+ti(nh4,bs="tp"),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=15)+ti(date_dec,bs="tp",k=20)+ti(pheo,bs="tp",k=7)+ti(tn,bs="tp",k=10)+ti(do_per,bs="tp",k=10)+
            ti(sio2,bs="tp")+ti(tp,bs="tp")+ti(tss,bs="tp")+ti(nh4,bs="tp"),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=15)+ti(date_dec,bs="tp",k=20)+ti(pheo,bs="tp",k=7)+ti(tn,bs="tp",k=10)+ti(do_per,bs="tp",k=15)+
            ti(sio2,bs="tp")+ti(tp,bs="tp")+ti(tss,bs="tp")+ti(nh4,bs="tp"),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=15)+ti(date_dec,bs="tp",k=20)+ti(pheo,bs="tp",k=7)+ti(tn,bs="tp",k=10)+ti(do_per,bs="tp",k=15)+
            ti(sio2,bs="tp",k=10)+ti(tp,bs="tp")+ti(tss,bs="tp")+ti(nh4,bs="tp"),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=15)+ti(date_dec,bs="tp",k=20)+ti(pheo,bs="tp",k=7)+ti(tn,bs="tp",k=10)+ti(do_per,bs="tp",k=15)+
            ti(sio2,bs="tp",k=15)+ti(tp,bs="tp")+ti(tss,bs="tp")+ti(nh4,bs="tp"),data=data,family=gaussian(link="log"))
gam.check(gamP) ## getting slow

gamP<-gam(chl~ti(doy,bs="cc",k=15)+ti(date_dec,bs="tp",k=20)+ti(pheo,bs="tp",k=7)+ti(tn,bs="tp",k=10)+ti(do_per,bs="tp",k=15)+
            ti(sio2,bs="tp",k=15)+ti(tp,bs="tp")+ti(tss,bs="tp",k=10)+ti(nh4,bs="tp"),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=15)+ti(date_dec,bs="tp",k=20)+ti(pheo,bs="tp",k=7)+ti(tn,bs="tp",k=10)+ti(do_per,bs="tp",k=15)+
            ti(sio2,bs="tp",k=20)+ti(tp,bs="tp")+ti(tss,bs="tp",k=10)+ti(nh4,bs="tp"),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=15)+ti(date_dec,bs="tp",k=25)+ti(pheo,bs="tp",k=7)+ti(tn,bs="tp",k=10)+ti(do_per,bs="tp",k=15)+
            ti(sio2,bs="tp",k=20)+ti(tp,bs="tp")+ti(tss,bs="tp",k=10)+ti(nh4,bs="tp"),data=data,family=gaussian(link="log"))
gam.check(gamP) ## warning

gamP<-gam(chl~ti(doy,bs="cc",k=15)+ti(date_dec,bs="tp",k=23)+ti(pheo,bs="tp",k=7)+ti(tn,bs="tp",k=10)+ti(do_per,bs="tp",k=15)+
            ti(sio2,bs="tp",k=20)+ti(tp,bs="tp")+ti(tss,bs="tp",k=10)+ti(nh4,bs="tp"),data=data,family=gaussian(link="log"))
gam.check(gamP) ## warning

gamP<-gam(chl~ti(doy,bs="cc",k=15)+ti(date_dec,bs="tp",k=22)+ti(pheo,bs="tp",k=7)+ti(tn,bs="tp",k=10)+ti(do_per,bs="tp",k=15)+
            ti(sio2,bs="tp",k=20)+ti(tp,bs="tp")+ti(tss,bs="tp",k=10)+ti(nh4,bs="tp"),data=data,family=gaussian(link="log"))
gam.check(gamP) 

gamP<-gam(chl~ti(doy,bs="cc",k=15)+ti(date_dec,bs="tp",k=22)+ti(pheo,bs="tp",k=7)+ti(tn,bs="tp",k=10)+ti(do_per,bs="tp",k=15)+
            ti(sio2,bs="tp",k=25)+ti(tp,bs="tp")+ti(tss,bs="tp",k=10)+ti(nh4,bs="tp"),data=data,family=gaussian(link="log"))
gam.check(gamP) 

gamP<-gam(chl~ti(doy,bs="cc",k=15)+ti(date_dec,bs="tp",k=22)+ti(pheo,bs="tp",k=7)+ti(tn,bs="tp",k=10)+ti(do_per,bs="tp",k=15)+
            ti(sio2,bs="tp",k=25)+ti(tp,bs="tp")+ti(tss,bs="tp",k=10)+ti(nh4,bs="tp",k=10),data=data,family=gaussian(link="log"))
gam.check(gamP) 

perStationFullMod[[11]]=gamP

####
data=perStation[[13]]
sum(is.na(data$nh4))/nrow(data) ## 45% missing, don't use

gamP<-gam(chl~ti(doy,bs="cc")+ti(date_dec,bs="tp")+ti(pheo,bs="tp")+ti(tn,bs="tp")+ti(do_per,bs="tp")+
            ti(sio2,bs="tp")+ti(tp,bs="tp")+ti(tss,bs="tp"),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc")+ti(date_dec,bs="tp")+ti(pheo,bs="tp",k=10)+ti(tn,bs="tp")+ti(do_per,bs="tp")+
            ti(sio2,bs="tp")+ti(tp,bs="tp")+ti(tss,bs="tp"),data=data,family=gaussian(link="log"))
gam.check(gamP) ## warning

gamP<-gam(chl~ti(doy,bs="cc")+ti(date_dec,bs="tp")+ti(pheo,bs="tp",k=8)+ti(tn,bs="tp")+ti(do_per,bs="tp")+
            ti(sio2,bs="tp")+ti(tp,bs="tp")+ti(tss,bs="tp"),data=data,family=gaussian(link="log"))
gam.check(gamP) ## warning

gamP<-gam(chl~ti(doy,bs="cc")+ti(date_dec,bs="tp")+ti(pheo,bs="tp",k=7)+ti(tn,bs="tp")+ti(do_per,bs="tp")+
            ti(sio2,bs="tp")+ti(tp,bs="tp")+ti(tss,bs="tp"),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=10)+ti(date_dec,bs="tp")+ti(pheo,bs="tp",k=7)+ti(tn,bs="tp")+ti(do_per,bs="tp")+
            ti(sio2,bs="tp")+ti(tp,bs="tp")+ti(tss,bs="tp"),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=10)+ti(date_dec,bs="tp",k=10)+ti(pheo,bs="tp",k=7)+ti(tn,bs="tp")+ti(do_per,bs="tp")+
            ti(sio2,bs="tp")+ti(tp,bs="tp")+ti(tss,bs="tp"),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=10)+ti(date_dec,bs="tp",k=15)+ti(pheo,bs="tp",k=7)+ti(tn,bs="tp")+ti(do_per,bs="tp")+
            ti(sio2,bs="tp")+ti(tp,bs="tp")+ti(tss,bs="tp"),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=10)+ti(date_dec,bs="tp",k=20)+ti(pheo,bs="tp",k=7)+ti(tn,bs="tp")+ti(do_per,bs="tp")+
            ti(sio2,bs="tp")+ti(tp,bs="tp")+ti(tss,bs="tp"),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=10)+ti(date_dec,bs="tp",k=20)+ti(pheo,bs="tp",k=7)+ti(tn,bs="tp",k=10)+ti(do_per,bs="tp")+
            ti(sio2,bs="tp")+ti(tp,bs="tp")+ti(tss,bs="tp"),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=10)+ti(date_dec,bs="tp",k=20)+ti(pheo,bs="tp",k=7)+ti(tn,bs="tp",k=10)+ti(do_per,bs="tp")+
            ti(sio2,bs="tp")+ti(tp,bs="tp",k=10)+ti(tss,bs="tp"),data=data,family=gaussian(link="log"))
gam.check(gamP) ## warning

gamP<-gam(chl~ti(doy,bs="cc",k=10)+ti(date_dec,bs="tp",k=20)+ti(pheo,bs="tp",k=7)+ti(tn,bs="tp",k=10)+ti(do_per,bs="tp")+
            ti(sio2,bs="tp")+ti(tp,bs="tp",k=8)+ti(tss,bs="tp"),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=10)+ti(date_dec,bs="tp",k=20)+ti(pheo,bs="tp",k=7)+ti(tn,bs="tp",k=10)+ti(do_per,bs="tp",k=10)+
            ti(sio2,bs="tp")+ti(tp,bs="tp",k=8)+ti(tss,bs="tp"),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=10)+ti(date_dec,bs="tp",k=25)+ti(pheo,bs="tp",k=7)+ti(tn,bs="tp",k=10)+ti(do_per,bs="tp",k=10)+
            ti(sio2,bs="tp")+ti(tp,bs="tp",k=8)+ti(tss,bs="tp"),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=10)+ti(date_dec,bs="tp",k=30)+ti(pheo,bs="tp",k=7)+ti(tn,bs="tp",k=10)+ti(do_per,bs="tp",k=10)+
            ti(sio2,bs="tp")+ti(tp,bs="tp",k=8)+ti(tss,bs="tp"),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=15)+ti(date_dec,bs="tp",k=30)+ti(pheo,bs="tp",k=7)+ti(tn,bs="tp",k=10)+ti(do_per,bs="tp",k=10)+
            ti(sio2,bs="tp")+ti(tp,bs="tp",k=8)+ti(tss,bs="tp"),data=data,family=gaussian(link="log"))
gam.check(gamP)

perStationFullMod[[13]]=gamP

####

data=perStation[[15]]
sum(is.na(data$nh4))/nrow(data) ## use

gamP<-gam(chl~ti(doy,bs="cc")+ti(date_dec,bs="tp")+ti(pheo,bs="tp")+ti(tn,bs="tp")+ti(do_per,bs="tp")+
            ti(sio2,bs="tp")+ti(tp,bs="tp")+ti(tss,bs="tp")+ti(nh4,bs="tp"),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc")+ti(date_dec,bs="tp")+ti(pheo,bs="tp")+ti(tn,bs="tp",k=10)+ti(do_per,bs="tp")+
            ti(sio2,bs="tp")+ti(tp,bs="tp")+ti(tss,bs="tp")+ti(nh4,bs="tp"),data=data,family=gaussian(link="log"))
gam.check(gamP)


gamP<-gam(chl~ti(doy,bs="cc")+ti(date_dec,bs="tp",k=10)+ti(pheo,bs="tp")+ti(tn,bs="tp",k=10)+ti(do_per,bs="tp")+
            ti(sio2,bs="tp")+ti(tp,bs="tp")+ti(tss,bs="tp")+ti(nh4,bs="tp"),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc")+ti(date_dec,bs="tp",k=15)+ti(pheo,bs="tp",k=10)+ti(tn,bs="tp",k=10)+ti(do_per,bs="tp")+
            ti(sio2,bs="tp")+ti(tp,bs="tp")+ti(tss,bs="tp")+ti(nh4,bs="tp"),data=data,family=gaussian(link="log"))
gam.check(gamP) ## getting slow

gamP<-gam(chl~ti(doy,bs="cc")+ti(date_dec,bs="tp",k=15)+ti(pheo,bs="tp",k=10)+ti(tn,bs="tp",k=10)+ti(do_per,bs="tp",k=10)+
            ti(sio2,bs="tp")+ti(tp,bs="tp")+ti(tss,bs="tp")+ti(nh4,bs="tp"),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc")+ti(date_dec,bs="tp",k=15)+ti(pheo,bs="tp",k=15)+ti(tn,bs="tp",k=10)+ti(do_per,bs="tp",k=10)+
            ti(sio2,bs="tp")+ti(tp,bs="tp")+ti(tss,bs="tp")+ti(nh4,bs="tp"),data=data,family=gaussian(link="log"))
gam.check(gamP) ## warning

gamP<-gam(chl~ti(doy,bs="cc")+ti(date_dec,bs="tp",k=15)+ti(pheo,bs="tp",k=12)+ti(tn,bs="tp",k=10)+ti(do_per,bs="tp",k=10)+
            ti(sio2,bs="tp")+ti(tp,bs="tp")+ti(tss,bs="tp")+ti(nh4,bs="tp"),data=data,family=gaussian(link="log"))
gam.check(gamP) ## warning

gamP<-gam(chl~ti(doy,bs="cc")+ti(date_dec,bs="tp",k=15)+ti(pheo,bs="tp",k=11)+ti(tn,bs="tp",k=10)+ti(do_per,bs="tp",k=10)+
            ti(sio2,bs="tp")+ti(tp,bs="tp")+ti(tss,bs="tp")+ti(nh4,bs="tp"),data=data,family=gaussian(link="log"))
gam.check(gamP) ## warning, stuck at 10 for pheo

gamP<-gam(chl~ti(doy,bs="cc")+ti(date_dec,bs="tp",k=20)+ti(pheo,bs="tp",k=10)+ti(tn,bs="tp",k=10)+ti(do_per,bs="tp",k=10)+
            ti(sio2,bs="tp")+ti(tp,bs="tp")+ti(tss,bs="tp")+ti(nh4,bs="tp"),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=10)+ti(date_dec,bs="tp",k=25)+ti(pheo,bs="tp",k=10)+ti(tn,bs="tp",k=10)+ti(do_per,bs="tp",k=10)+
            ti(sio2,bs="tp")+ti(tp,bs="tp")+ti(tss,bs="tp")+ti(nh4,bs="tp"),data=data,family=gaussian(link="log"))
gam.check(gamP) 

gamP<-gam(chl~ti(doy,bs="cc",k=15)+ti(date_dec,bs="tp",k=30)+ti(pheo,bs="tp",k=10)+ti(tn,bs="tp",k=10)+ti(do_per,bs="tp",k=10)+
            ti(sio2,bs="tp")+ti(tp,bs="tp")+ti(tss,bs="tp")+ti(nh4,bs="tp"),data=data,family=gaussian(link="log"))
gam.check(gamP) 

gamP<-gam(chl~ti(doy,bs="cc",k=15)+ti(date_dec,bs="tp",k=30)+ti(pheo,bs="tp",k=10)+ti(tn,bs="tp",k=10)+ti(do_per,bs="tp",k=10)+
            ti(sio2,bs="tp")+ti(tp,bs="tp")+ti(tss,bs="tp")+ti(nh4,bs="tp",k=10),data=data,family=gaussian(link="log"))
gam.check(gamP) 

gamP<-gam(chl~ti(doy,bs="cc",k=15)+ti(date_dec,bs="tp",k=30)+ti(pheo,bs="tp",k=10)+ti(tn,bs="tp",k=10)+ti(do_per,bs="tp",k=10)+
            ti(sio2,bs="tp",k=10)+ti(tp,bs="tp")+ti(tss,bs="tp")+ti(nh4,bs="tp",k=10),data=data,family=gaussian(link="log"))
gam.check(gamP) 

gamP<-gam(chl~ti(doy,bs="cc",k=15)+ti(date_dec,bs="tp",k=35)+ti(pheo,bs="tp",k=10)+ti(tn,bs="tp",k=10)+ti(do_per,bs="tp",k=10)+
            ti(sio2,bs="tp",k=10)+ti(tp,bs="tp")+ti(tss,bs="tp")+ti(nh4,bs="tp",k=10),data=data,family=gaussian(link="log"))
gam.check(gamP) 

gamP<-gam(chl~ti(doy,bs="cc",k=20)+ti(date_dec,bs="tp",k=35)+ti(pheo,bs="tp",k=10)+ti(tn,bs="tp",k=10)+ti(do_per,bs="tp",k=10)+
            ti(sio2,bs="tp",k=10)+ti(tp,bs="tp")+ti(tss,bs="tp")+ti(nh4,bs="tp",k=10),data=data,family=gaussian(link="log"))
gam.check(gamP) 

perStationFullMod[[15]]=gamP

####

data=perStation[[16]]
sum(is.na(data$nh4))/nrow(data) ## use

gamP<-gam(chl~ti(doy,bs="cc")+ti(date_dec,bs="tp")+ti(pheo,bs="tp")+ti(tn,bs="tp")+ti(do_per,bs="tp")+
            ti(sio2,bs="tp")+ti(tp,bs="tp")+ti(tss,bs="tp")+ti(nh4,bs="tp"),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc")+ti(date_dec,bs="tp")+ti(pheo,bs="tp")+ti(tn,bs="tp")+ti(do_per,bs="tp")+
            ti(sio2,bs="tp")+ti(tp,bs="tp")+ti(tss,bs="tp",k=10)+ti(nh4,bs="tp"),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc")+ti(date_dec,bs="tp")+ti(pheo,bs="tp")+ti(tn,bs="tp")+ti(do_per,bs="tp")+
            ti(sio2,bs="tp")+ti(tp,bs="tp")+ti(tss,bs="tp",k=15)+ti(nh4,bs="tp"),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc")+ti(date_dec,bs="tp",k=10)+ti(pheo,bs="tp")+ti(tn,bs="tp")+ti(do_per,bs="tp")+
            ti(sio2,bs="tp")+ti(tp,bs="tp")+ti(tss,bs="tp",k=15)+ti(nh4,bs="tp"),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc")+ti(date_dec,bs="tp",k=20)+ti(pheo,bs="tp")+ti(tn,bs="tp")+ti(do_per,bs="tp")+
            ti(sio2,bs="tp")+ti(tp,bs="tp")+ti(tss,bs="tp",k=15)+ti(nh4,bs="tp"),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc")+ti(date_dec,bs="tp",k=20)+ti(pheo,bs="tp")+ti(tn,bs="tp")+ti(do_per,bs="tp")+
            ti(sio2,bs="tp")+ti(tp,bs="tp")+ti(tss,bs="tp",k=15)+ti(nh4,bs="tp",k=10),data=data,family=gaussian(link="log"))
gam.check(gamP) ## warning

gamP<-gam(chl~ti(doy,bs="cc")+ti(date_dec,bs="tp",k=20)+ti(pheo,bs="tp")+ti(tn,bs="tp")+ti(do_per,bs="tp")+
            ti(sio2,bs="tp")+ti(tp,bs="tp")+ti(tss,bs="tp",k=15)+ti(nh4,bs="tp",k=8),data=data,family=gaussian(link="log"))
gam.check(gamP) ## warning

gamP<-gam(chl~ti(doy,bs="cc")+ti(date_dec,bs="tp",k=20)+ti(pheo,bs="tp")+ti(tn,bs="tp")+ti(do_per,bs="tp")+
            ti(sio2,bs="tp")+ti(tp,bs="tp")+ti(tss,bs="tp",k=15)+ti(nh4,bs="tp",k=7),data=data,family=gaussian(link="log"))
gam.check(gamP) ## warning

gamP<-gam(chl~ti(doy,bs="cc")+ti(date_dec,bs="tp",k=20)+ti(pheo,bs="tp")+ti(tn,bs="tp")+ti(do_per,bs="tp")+
            ti(sio2,bs="tp")+ti(tp,bs="tp")+ti(tss,bs="tp",k=15)+ti(nh4,bs="tp",k=6),data=data,family=gaussian(link="log"))
gam.check(gamP) ## warning, stuck there

gamP<-gam(chl~ti(doy,bs="cc")+ti(date_dec,bs="tp",k=20)+ti(pheo,bs="tp")+ti(tn,bs="tp",k=10)+ti(do_per,bs="tp")+
            ti(sio2,bs="tp")+ti(tp,bs="tp")+ti(tss,bs="tp",k=15)+ti(nh4,bs="tp"),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc")+ti(date_dec,bs="tp",k=20)+ti(pheo,bs="tp")+ti(tn,bs="tp",k=10)+ti(do_per,bs="tp",k=10)+
            ti(sio2,bs="tp")+ti(tp,bs="tp")+ti(tss,bs="tp",k=15)+ti(nh4,bs="tp"),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc")+ti(date_dec,bs="tp",k=20)+ti(pheo,bs="tp")+ti(tn,bs="tp",k=10)+ti(do_per,bs="tp",k=15)+
            ti(sio2,bs="tp")+ti(tp,bs="tp")+ti(tss,bs="tp",k=15)+ti(nh4,bs="tp"),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=10)+ti(date_dec,bs="tp",k=20)+ti(pheo,bs="tp")+ti(tn,bs="tp",k=10)+ti(do_per,bs="tp",k=15)+
            ti(sio2,bs="tp")+ti(tp,bs="tp")+ti(tss,bs="tp",k=15)+ti(nh4,bs="tp"),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=15)+ti(date_dec,bs="tp",k=20)+ti(pheo,bs="tp")+ti(tn,bs="tp",k=10)+ti(do_per,bs="tp",k=15)+
            ti(sio2,bs="tp")+ti(tp,bs="tp")+ti(tss,bs="tp",k=15)+ti(nh4,bs="tp"),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=15)+ti(date_dec,bs="tp",k=20)+ti(pheo,bs="tp")+ti(tn,bs="tp",k=10)+ti(do_per,bs="tp",k=15)+
            ti(sio2,bs="tp",k=10)+ti(tp,bs="tp")+ti(tss,bs="tp",k=15)+ti(nh4,bs="tp"),data=data,family=gaussian(link="log"))
gam.check(gamP) ## warning

gamP<-gam(chl~ti(doy,bs="cc",k=15)+ti(date_dec,bs="tp",k=20)+ti(pheo,bs="tp")+ti(tn,bs="tp",k=10)+ti(do_per,bs="tp",k=15)+
            ti(sio2,bs="tp",k=8)+ti(tp,bs="tp")+ti(tss,bs="tp",k=15)+ti(nh4,bs="tp"),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=15)+ti(date_dec,bs="tp",k=20)+ti(pheo,bs="tp")+ti(tn,bs="tp",k=10)+ti(do_per,bs="tp",k=15)+
            ti(sio2,bs="tp",k=9)+ti(tp,bs="tp")+ti(tss,bs="tp",k=15)+ti(nh4,bs="tp"),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=15)+ti(date_dec,bs="tp",k=20)+ti(pheo,bs="tp",k=10)+ti(tn,bs="tp",k=10)+ti(do_per,bs="tp",k=15)+
            ti(sio2,bs="tp",k=9)+ti(tp,bs="tp")+ti(tss,bs="tp",k=15)+ti(nh4,bs="tp"),data=data,family=gaussian(link="log"))
gam.check(gamP) ## getting slow now

gamP<-gam(chl~ti(doy,bs="cc",k=15)+ti(date_dec,bs="tp",k=20)+ti(pheo,bs="tp",k=15)+ti(tn,bs="tp",k=10)+ti(do_per,bs="tp",k=15)+
            ti(sio2,bs="tp",k=9)+ti(tp,bs="tp")+ti(tss,bs="tp",k=15)+ti(nh4,bs="tp"),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=15)+ti(date_dec,bs="tp",k=20)+ti(pheo,bs="tp",k=15)+ti(tn,bs="tp",k=10)+ti(do_per,bs="tp",k=15)+
            ti(sio2,bs="tp",k=9)+ti(tp,bs="tp",k=10)+ti(tss,bs="tp",k=15)+ti(nh4,bs="tp"),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=15)+ti(date_dec,bs="tp",k=20)+ti(pheo,bs="tp",k=20)+ti(tn,bs="tp",k=10)+ti(do_per,bs="tp",k=15)+
            ti(sio2,bs="tp",k=9)+ti(tp,bs="tp",k=10)+ti(tss,bs="tp",k=15)+ti(nh4,bs="tp"),data=data,family=gaussian(link="log"))
gam.check(gamP) ## warning

gamP<-gam(chl~ti(doy,bs="cc",k=15)+ti(date_dec,bs="tp",k=20)+ti(pheo,bs="tp",k=18)+ti(tn,bs="tp",k=10)+ti(do_per,bs="tp",k=15)+
            ti(sio2,bs="tp",k=9)+ti(tp,bs="tp",k=10)+ti(tss,bs="tp",k=15)+ti(nh4,bs="tp"),data=data,family=gaussian(link="log"))
gam.check(gamP)  ## slower, not great but go with this

perStationFullMod[[16]]=gamP

####

data=perStation[[17]]
sum(is.na(data$nh4))/nrow(data) ## use

gamP<-gam(chl~ti(doy,bs="cc")+ti(date_dec,bs="tp")+ti(pheo,bs="tp")+ti(tn,bs="tp")+ti(do_per,bs="tp")+
            ti(sio2,bs="tp")+ti(tp,bs="tp")+ti(tss,bs="tp")+ti(nh4,bs="tp"),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc")+ti(date_dec,bs="tp",k=10)+ti(pheo,bs="tp")+ti(tn,bs="tp")+ti(do_per,bs="tp")+
            ti(sio2,bs="tp")+ti(tp,bs="tp")+ti(tss,bs="tp")+ti(nh4,bs="tp"),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc")+ti(date_dec,bs="tp",k=10)+ti(pheo,bs="tp")+ti(tn,bs="tp")+ti(do_per,bs="tp")+
            ti(sio2,bs="tp")+ti(tp,bs="tp")+ti(tss,bs="tp")+ti(nh4,bs="tp",k=10),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc")+ti(date_dec,bs="tp",k=10)+ti(pheo,bs="tp")+ti(tn,bs="tp")+ti(do_per,bs="tp")+
            ti(sio2,bs="tp")+ti(tp,bs="tp")+ti(tss,bs="tp")+ti(nh4,bs="tp",k=15),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc")+ti(date_dec,bs="tp",k=10)+ti(pheo,bs="tp")+ti(tn,bs="tp")+ti(do_per,bs="tp")+
            ti(sio2,bs="tp",k=10)+ti(tp,bs="tp")+ti(tss,bs="tp")+ti(nh4,bs="tp",k=15),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc")+ti(date_dec,bs="tp",k=10)+ti(pheo,bs="tp")+ti(tn,bs="tp")+ti(do_per,bs="tp")+
            ti(sio2,bs="tp",k=15)+ti(tp,bs="tp")+ti(tss,bs="tp")+ti(nh4,bs="tp",k=15),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc")+ti(date_dec,bs="tp",k=10)+ti(pheo,bs="tp")+ti(tn,bs="tp",k=10)+ti(do_per,bs="tp",k=10)+
            ti(sio2,bs="tp",k=15)+ti(tp,bs="tp")+ti(tss,bs="tp")+ti(nh4,bs="tp",k=15),data=data,family=gaussian(link="log"))
gam.check(gamP) ## warning

gamP<-gam(chl~ti(doy,bs="cc")+ti(date_dec,bs="tp",k=10)+ti(pheo,bs="tp")+ti(tn,bs="tp",k=10)+ti(do_per,bs="tp")+
            ti(sio2,bs="tp",k=15)+ti(tp,bs="tp")+ti(tss,bs="tp")+ti(nh4,bs="tp",k=15),data=data,family=gaussian(link="log"))
gam.check(gamP) ## warning

gamP<-gam(chl~ti(doy,bs="cc")+ti(date_dec,bs="tp",k=10)+ti(pheo,bs="tp")+ti(tn,bs="tp",k=8)+ti(do_per,bs="tp")+
            ti(sio2,bs="tp",k=15)+ti(tp,bs="tp")+ti(tss,bs="tp")+ti(nh4,bs="tp",k=15),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc")+ti(date_dec,bs="tp",k=10)+ti(pheo,bs="tp")+ti(tn,bs="tp",k=8)+ti(do_per,bs="tp",k=8)+
            ti(sio2,bs="tp",k=15)+ti(tp,bs="tp")+ti(tss,bs="tp")+ti(nh4,bs="tp",k=15),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc")+ti(date_dec,bs="tp",k=10)+ti(pheo,bs="tp",k=10)+ti(tn,bs="tp",k=8)+ti(do_per,bs="tp",k=8)+
            ti(sio2,bs="tp",k=15)+ti(tp,bs="tp")+ti(tss,bs="tp")+ti(nh4,bs="tp",k=15),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=10)+ti(date_dec,bs="tp",k=15)+ti(pheo,bs="tp",k=10)+ti(tn,bs="tp",k=8)+ti(do_per,bs="tp",k=8)+
            ti(sio2,bs="tp",k=15)+ti(tp,bs="tp")+ti(tss,bs="tp")+ti(nh4,bs="tp",k=15),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=10)+ti(date_dec,bs="tp",k=15)+ti(pheo,bs="tp",k=10)+ti(tn,bs="tp",k=8)+ti(do_per,bs="tp",k=8)+
            ti(sio2,bs="tp",k=15)+ti(tp,bs="tp")+ti(tss,bs="tp",k=10)+ti(nh4,bs="tp",k=15),data=data,family=gaussian(link="log"))
gam.check(gamP) ## warning

gamP<-gam(chl~ti(doy,bs="cc",k=10)+ti(date_dec,bs="tp",k=15)+ti(pheo,bs="tp",k=10)+ti(tn,bs="tp",k=8)+ti(do_per,bs="tp",k=8)+
            ti(sio2,bs="tp",k=15)+ti(tp,bs="tp")+ti(tss,bs="tp",k=8)+ti(nh4,bs="tp",k=15),data=data,family=gaussian(link="log"))
gam.check(gamP) ## warning

gamP<-gam(chl~ti(doy,bs="cc",k=10)+ti(date_dec,bs="tp",k=15)+ti(pheo,bs="tp",k=10)+ti(tn,bs="tp",k=8)+ti(do_per,bs="tp",k=8)+
            ti(sio2,bs="tp",k=15)+ti(tp,bs="tp")+ti(tss,bs="tp",k=7)+ti(nh4,bs="tp",k=15),data=data,family=gaussian(link="log"))
gam.check(gamP) ## good, got better plot

gamP<-gam(chl~ti(doy,bs="cc",k=10)+ti(date_dec,bs="tp",k=20)+ti(pheo,bs="tp",k=10)+ti(tn,bs="tp",k=8)+ti(do_per,bs="tp",k=8)+
            ti(sio2,bs="tp",k=15)+ti(tp,bs="tp")+ti(tss,bs="tp",k=7)+ti(nh4,bs="tp",k=15),data=data,family=gaussian(link="log"))
gam.check(gamP) ## plot still fine


gamP<-gam(chl~ti(doy,bs="cc",k=10)+ti(date_dec,bs="tp",k=25)+ti(pheo,bs="tp",k=10)+ti(tn,bs="tp",k=8)+ti(do_per,bs="tp",k=8)+
            ti(sio2,bs="tp",k=15)+ti(tp,bs="tp")+ti(tss,bs="tp",k=7)+ti(nh4,bs="tp",k=15),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=10)+ti(date_dec,bs="tp",k=30)+ti(pheo,bs="tp",k=10)+ti(tn,bs="tp",k=8)+ti(do_per,bs="tp",k=8)+
            ti(sio2,bs="tp",k=15)+ti(tp,bs="tp")+ti(tss,bs="tp",k=7)+ti(nh4,bs="tp",k=15),data=data,family=gaussian(link="log"))
gam.check(gamP) ## going with this
## detour
gamP<-gam(chl~ti(doy,bs="cc",k=10)+ti(date_dec,bs="tp",k=20)+ti(pheo,bs="tp",k=10)+ti(tn,bs="tp",k=8)+ti(do_per,bs="tp",k=8)+
            ti(sio2,bs="tp",k=20)+ti(tp,bs="tp")+ti(tss,bs="tp",k=7)+ti(nh4,bs="tp",k=15),data=data,family=gaussian(link="log"))
gam.check(gamP) ## warning

gamP<-gam(chl~ti(doy,bs="cc",k=10)+ti(date_dec,bs="tp",k=20)+ti(pheo,bs="tp",k=10)+ti(tn,bs="tp",k=8)+ti(do_per,bs="tp",k=8)+
            ti(sio2,bs="tp",k=18)+ti(tp,bs="tp")+ti(tss,bs="tp",k=7)+ti(nh4,bs="tp",k=15),data=data,family=gaussian(link="log"))
gam.check(gamP) ## warning

gamP<-gam(chl~ti(doy,bs="cc",k=10)+ti(date_dec,bs="tp",k=20)+ti(pheo,bs="tp",k=10)+ti(tn,bs="tp",k=8)+ti(do_per,bs="tp",k=8)+
            ti(sio2,bs="tp",k=17)+ti(tp,bs="tp")+ti(tss,bs="tp",k=7)+ti(nh4,bs="tp",k=15),data=data,family=gaussian(link="log"))
gam.check(gamP) ## warning

gamP<-gam(chl~ti(doy,bs="cc",k=10)+ti(date_dec,bs="tp",k=20)+ti(pheo,bs="tp",k=10)+ti(tn,bs="tp",k=8)+ti(do_per,bs="tp",k=8)+
            ti(sio2,bs="tp",k=16)+ti(tp,bs="tp")+ti(tss,bs="tp",k=7)+ti(nh4,bs="tp",k=15),data=data,family=gaussian(link="log"))
gam.check(gamP) ## warning, stuck there
##

gamP<-gam(chl~ti(doy,bs="cc",k=15)+ti(date_dec,bs="tp",k=20)+ti(pheo,bs="tp",k=10)+ti(tn,bs="tp",k=8)+ti(do_per,bs="tp",k=8)+
            ti(sio2,bs="tp",k=15)+ti(tp,bs="tp")+ti(tss,bs="tp",k=7)+ti(nh4,bs="tp",k=15),data=data,family=gaussian(link="log"))
gam.check(gamP)  ## that made it worse plot wise

gamP<-gam(chl~ti(doy,bs="cc",k=15)+ti(date_dec,bs="tp",k=20)+ti(pheo,bs="tp",k=10)+ti(tn,bs="tp",k=8)+ti(do_per,bs="tp",k=8)+
            ti(sio2,bs="tp",k=15)+ti(tp,bs="tp",k=10)+ti(tss,bs="tp",k=7)+ti(nh4,bs="tp",k=15),data=data,family=gaussian(link="log"))
gam.check(gamP) ## warning

gamP<-gam(chl~ti(doy,bs="cc",k=15)+ti(date_dec,bs="tp",k=20)+ti(pheo,bs="tp",k=10)+ti(tn,bs="tp",k=8)+ti(do_per,bs="tp",k=8)+
            ti(sio2,bs="tp",k=15)+ti(tp,bs="tp",k=8)+ti(tss,bs="tp",k=7)+ti(nh4,bs="tp",k=15),data=data,family=gaussian(link="log"))
gam.check(gamP) 

gamP<-gam(chl~ti(doy,bs="cc",k=20)+ti(date_dec,bs="tp",k=20)+ti(pheo,bs="tp",k=10)+ti(tn,bs="tp",k=8)+ti(do_per,bs="tp",k=8)+
            ti(sio2,bs="tp",k=15)+ti(tp,bs="tp",k=8)+ti(tss,bs="tp",k=7)+ti(nh4,bs="tp",k=15),data=data,family=gaussian(link="log"))
gam.check(gamP) 

gamP<-gam(chl~ti(doy,bs="cc",k=10)+ti(date_dec,bs="tp",k=30)+ti(pheo,bs="tp",k=10)+ti(tn,bs="tp",k=8)+ti(do_per,bs="tp",k=8)+
            ti(sio2,bs="tp",k=15)+ti(tp,bs="tp")+ti(tss,bs="tp",k=7)+ti(nh4,bs="tp",k=15),data=data,family=gaussian(link="log"))
gam.check(gamP) ## going with this

perStationFullMod[[17]]=gamP

####

