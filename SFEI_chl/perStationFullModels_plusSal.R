setwd("~/Desktop/sfei")
load(file="perStation.Rda")
require(mgcv)

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
## forgot sal!!!!

perStationFullMod_Sal=vector("list",length(perStation))

data=perStation[[1]]

gamP<-gam(chl~ti(doy,bs="cc")+ti(date_dec,bs="tp")+ti(pheo,bs="tp")+ti(tn,bs="tp")+ti(do_per,bs="tp")+
            ti(sio2,bs="tp")+ti(tp,bs="tp")+ti(tss,bs="tp")+ti(nh4,bs="tp")+ti(sal,bs="tp"),
          data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc")+ti(date_dec,bs="tp")+ti(pheo,bs="tp")+ti(tn,bs="tp")+ti(do_per,bs="tp",k=10)+
            ti(sio2,bs="tp",k=10)+ti(tp,bs="tp")+ti(tss,bs="tp")+ti(nh4,bs="tp")+ti(sal,bs="tp",k=10),
          data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc")+ti(date_dec,bs="tp")+ti(pheo,bs="tp")+ti(tn,bs="tp")+ti(do_per,bs="tp",k=15)+
            ti(sio2,bs="tp",k=15)+ti(tp,bs="tp")+ti(tss,bs="tp")+ti(nh4,bs="tp")+ti(sal,bs="tp",k=15),
          data=data,family=gaussian(link="log"))
gam.check(gamP) ## error

gamP<-gam(chl~ti(doy,bs="cc")+ti(date_dec,bs="tp")+ti(pheo,bs="tp",k=10)+ti(tn,bs="tp")+ti(do_per,bs="tp",k=10)+
            ti(sio2,bs="tp",k=10)+ti(tp,bs="tp")+ti(tss,bs="tp")+ti(nh4,bs="tp")+ti(sal,bs="tp",k=10),
          data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc")+ti(date_dec,bs="tp")+ti(pheo,bs="tp",k=10)+ti(tn,bs="tp")+ti(do_per,bs="tp",k=10)+
            ti(sio2,bs="tp",k=10)+ti(tp,bs="tp")+ti(tss,bs="tp",k=10)+ti(nh4,bs="tp")+ti(sal,bs="tp",k=10),
          data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc")+ti(date_dec,bs="tp")+ti(pheo,bs="tp",k=10)+ti(tn,bs="tp")+ti(do_per,bs="tp",k=10)+
            ti(sio2,bs="tp",k=15)+ti(tp,bs="tp")+ti(tss,bs="tp",k=10)+ti(nh4,bs="tp")+ti(sal,bs="tp",k=10),
          data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc")+ti(date_dec,bs="tp",k=15)+ti(pheo,bs="tp",k=10)+ti(tn,bs="tp")+ti(do_per,bs="tp",k=10)+
            ti(sio2,bs="tp",k=15)+ti(tp,bs="tp")+ti(tss,bs="tp",k=10)+ti(nh4,bs="tp")+ti(sal,bs="tp",k=10),
          data=data,family=gaussian(link="log"))
gam.check(gamP) ## getting slow

gamP<-gam(chl~ti(doy,bs="cc",k=10)+ti(date_dec,bs="tp",k=15)+ti(pheo,bs="tp",k=10)+ti(tn,bs="tp")+ti(do_per,bs="tp",k=10)+
            ti(sio2,bs="tp",k=15)+ti(tp,bs="tp")+ti(tss,bs="tp",k=10)+ti(nh4,bs="tp")+ti(sal,bs="tp",k=10),
          data=data,family=gaussian(link="log"))
gam.check(gamP) 

gamP<-gam(chl~ti(doy,bs="cc",k=10)+ti(date_dec,bs="tp",k=15)+ti(pheo,bs="tp",k=10)+ti(tn,bs="tp")+ti(do_per,bs="tp",k=10)+
            ti(sio2,bs="tp",k=15)+ti(tp,bs="tp",k=10)+ti(tss,bs="tp",k=10)+ti(nh4,bs="tp")+ti(sal,bs="tp",k=10),
          data=data,family=gaussian(link="log"))
gam.check(gamP) 

gamP<-gam(chl~ti(doy,bs="cc",k=10)+ti(date_dec,bs="tp",k=15)+ti(pheo,bs="tp",k=10)+ti(tn,bs="tp")+ti(do_per,bs="tp",k=10)+
            ti(sio2,bs="tp",k=15)+ti(tp,bs="tp",k=10)+ti(tss,bs="tp",k=15)+ti(nh4,bs="tp")+ti(sal,bs="tp",k=10),
          data=data,family=gaussian(link="log"))
gam.check(gamP)  ## warning

gamP<-gam(chl~ti(doy,bs="cc",k=10)+ti(date_dec,bs="tp",k=15)+ti(pheo,bs="tp",k=10)+ti(tn,bs="tp")+ti(do_per,bs="tp",k=10)+
            ti(sio2,bs="tp",k=15)+ti(tp,bs="tp",k=10)+ti(tss,bs="tp",k=11)+ti(nh4,bs="tp")+ti(sal,bs="tp",k=10),
          data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=10)+ti(date_dec,bs="tp",k=15)+ti(pheo,bs="tp",k=10)+ti(tn,bs="tp")+ti(do_per,bs="tp",k=10)+
            ti(sio2,bs="tp",k=15)+ti(tp,bs="tp",k=10)+ti(tss,bs="tp",k=12)+ti(nh4,bs="tp")+ti(sal,bs="tp",k=10),
          data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=10)+ti(date_dec,bs="tp",k=15)+ti(pheo,bs="tp",k=10)+ti(tn,bs="tp",k=10)+ti(do_per,bs="tp",k=10)+
            ti(sio2,bs="tp",k=15)+ti(tp,bs="tp",k=10)+ti(tss,bs="tp",k=12)+ti(nh4,bs="tp")+ti(sal,bs="tp",k=10),
          data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=10)+ti(date_dec,bs="tp",k=15)+ti(pheo,bs="tp",k=10)+ti(tn,bs="tp",k=10)+ti(do_per,bs="tp",k=10)+
            ti(sio2,bs="tp",k=15)+ti(tp,bs="tp",k=10)+ti(tss,bs="tp",k=12)+ti(nh4,bs="tp",k=10)+ti(sal,bs="tp",k=10),
          data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=10)+ti(date_dec,bs="tp",k=15)+ti(pheo,bs="tp",k=10)+ti(tn,bs="tp",k=10)+ti(do_per,bs="tp",k=10)+
            ti(sio2,bs="tp",k=15)+ti(tp,bs="tp",k=10)+ti(tss,bs="tp",k=12)+ti(nh4,bs="tp",k=10)+ti(sal,bs="tp",k=15),
          data=data,family=gaussian(link="log"))
gam.check(gamP) ## error, not enough unique covariate combinations

gamP<-gam(chl~ti(doy,bs="cc",k=10)+ti(date_dec,bs="tp",k=15)+ti(pheo,bs="tp",k=10)+ti(tn,bs="tp",k=10)+ti(do_per,bs="tp",k=10)+
            ti(sio2,bs="tp",k=15)+ti(tp,bs="tp",k=10)+ti(tss,bs="tp",k=12)+ti(nh4,bs="tp",k=10)+ti(sal,bs="tp",k=12),
          data=data,family=gaussian(link="log"))
gam.check(gamP) 

gamP<-gam(chl~ti(doy,bs="cc",k=10)+ti(date_dec,bs="tp",k=15)+ti(pheo,bs="tp",k=10)+ti(tn,bs="tp",k=10)+ti(do_per,bs="tp",k=10)+
            ti(sio2,bs="tp",k=15)+ti(tp,bs="tp",k=10)+ti(tss,bs="tp",k=12)+ti(nh4,bs="tp",k=15)+ti(sal,bs="tp",k=12),
          data=data,family=gaussian(link="log"))
gam.check(gamP) 

gamP<-gam(chl~ti(doy,bs="cc",k=10)+ti(date_dec,bs="tp",k=20)+ti(pheo,bs="tp",k=10)+ti(tn,bs="tp",k=10)+ti(do_per,bs="tp",k=10)+
            ti(sio2,bs="tp",k=15)+ti(tp,bs="tp",k=10)+ti(tss,bs="tp",k=12)+ti(nh4,bs="tp",k=15)+ti(sal,bs="tp",k=12),
          data=data,family=gaussian(link="log"))
gam.check(gamP) 

gamP<-gam(chl~ti(doy,bs="cc",k=10)+ti(date_dec,bs="tp",k=30)+ti(pheo,bs="tp",k=10)+ti(tn,bs="tp",k=10)+ti(do_per,bs="tp",k=10)+
            ti(sio2,bs="tp",k=15)+ti(tp,bs="tp",k=10)+ti(tss,bs="tp",k=12)+ti(nh4,bs="tp",k=15)+ti(sal,bs="tp",k=12),
          data=data,family=gaussian(link="log"))
gam.check(gamP) 

gamP<-gam(chl~ti(doy,bs="cc",k=10)+ti(date_dec,bs="tp",k=30)+ti(pheo,bs="tp",k=10)+ti(tn,bs="tp",k=10)+ti(do_per,bs="tp",k=10)+
            ti(sio2,bs="tp",k=15)+ti(tp,bs="tp",k=15)+ti(tss,bs="tp",k=12)+ti(nh4,bs="tp",k=15)+ti(sal,bs="tp",k=12),
          data=data,family=gaussian(link="log"))
gam.check(gamP)  ## too slow, stopped it

gamP<-gam(chl~ti(doy,bs="cc",k=10)+ti(date_dec,bs="tp",k=30)+ti(pheo,bs="tp",k=15)+ti(tn,bs="tp",k=10)+ti(do_per,bs="tp",k=10)+
            ti(sio2,bs="tp",k=15)+ti(tp,bs="tp",k=10)+ti(tss,bs="tp",k=12)+ti(nh4,bs="tp",k=15)+ti(sal,bs="tp",k=12),
          data=data,family=gaussian(link="log"))
gam.check(gamP)  ## warning

gamP<-gam(chl~ti(doy,bs="cc",k=10)+ti(date_dec,bs="tp",k=30)+ti(pheo,bs="tp",k=10)+ti(tn,bs="tp",k=10)+ti(do_per,bs="tp",k=10)+
            ti(sio2,bs="tp",k=15)+ti(tp,bs="tp",k=10)+ti(tss,bs="tp",k=12)+ti(nh4,bs="tp",k=15)+ti(sal,bs="tp",k=12),
          data=data,family=gaussian(link="log"))
gam.check(gamP)  ## just stick with this

perStationFullMod_Sal[[1]]=gamP

####

data=perStation[[2]]

gamP<-gam(chl~ti(doy,bs="cc")+ti(date_dec,bs="tp")+ti(pheo,bs="tp")+ti(tn,bs="tp")+ti(do_per,bs="tp")+
            ti(sio2,bs="tp")+ti(tp,bs="tp")+ti(tss,bs="tp")+ti(nh4,bs="tp")+ti(sal,bs="tp"),
          data=data,family=gaussian(link="log"))
gam.check(gamP)

length(unique(data$sal)) ##5

data=perStation[[15]]
length(unique(data$sal)) ##8

data=perStation[[21]]
length(unique(data$sal)) ## 251

wholeSeries<-c(1, 2, 5, 7, 9, 11, 13, 15, 16, 17, 18, 21, 22, 23, 29, 40)

salVal<-c()
for(i in wholeSeries){
  salVal<-c(salVal,length(unique(perStation[[i]]$sal)))
}
salVal

pheoVal<-c()
for(i in wholeSeries){
  pheoVal<-c(pheoVal,length(unique(perStation[[i]]$pheo)))
}
pheoVal

wholeSeries[which(salVal>75)]
# 5  7 17 18 21 22 23

## Ok, add salinity to the full models for these
## drop all but sal (if it is in) and tn for 5, 7,13

data=perStation[[5]]

gamP<-gam(chl~ti(doy,bs="cc")+ti(date_dec,bs="tp")+ti(pheo,bs="tp")+ti(do_per,bs="tp")+
            ti(sal,bs="tp"),
          data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc")+ti(date_dec,bs="tp")+ti(pheo,bs="tp",k=10)+ti(do_per,bs="tp")+
            ti(sal,bs="tp"),
          data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc")+ti(date_dec,bs="tp")+ti(pheo,bs="tp",k=15)+ti(do_per,bs="tp")+
            ti(sal,bs="tp"),
          data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc")+ti(date_dec,bs="tp")+ti(pheo,bs="tp",k=15)+ti(do_per,bs="tp")+
            ti(sal,bs="tp",k=10),
          data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc")+ti(date_dec,bs="tp",k=20)+ti(pheo,bs="tp",k=15)+ti(do_per,bs="tp")+
            ti(sal,bs="tp",k=10),
          data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=10)+ti(date_dec,bs="tp",k=20)+ti(pheo,bs="tp",k=15)+ti(do_per,bs="tp")+
            ti(sal,bs="tp",k=10),
          data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=10)+ti(date_dec,bs="tp",k=20)+ti(pheo,bs="tp",k=20)+ti(do_per,bs="tp")+
            ti(sal,bs="tp",k=10),
          data=data,family=gaussian(link="log"))
gam.check(gamP) ## warning

gamP<-gam(chl~ti(doy,bs="cc",k=10)+ti(date_dec,bs="tp",k=20)+ti(pheo,bs="tp",k=18)+ti(do_per,bs="tp")+
            ti(sal,bs="tp",k=10),
          data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=15)+ti(date_dec,bs="tp",k=20)+ti(pheo,bs="tp",k=18)+ti(do_per,bs="tp")+
            ti(sal,bs="tp",k=10),
          data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=15)+ti(date_dec,bs="tp",k=20)+ti(pheo,bs="tp",k=18)+ti(do_per,bs="tp",k=10)+
            ti(sal,bs="tp",k=10),
          data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=15)+ti(date_dec,bs="tp",k=20)+ti(pheo,bs="tp",k=18)+ti(do_per,bs="tp",k=10)+
            ti(sal,bs="tp",k=15),
          data=data,family=gaussian(link="log"))
gam.check(gamP)

perStationFullMod[[5]]=gamP

####

data=perStation[[7]]

gamP<-gam(chl~ti(doy,bs="cc")+ti(date_dec,bs="tp")+ti(pheo,bs="tp")+ti(do_per,bs="tp")+
            ti(sal,bs="tp"),
          data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc")+ti(date_dec,bs="tp",k=10)+ti(pheo,bs="tp")+ti(do_per,bs="tp")+
            ti(sal,bs="tp"),
          data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc")+ti(date_dec,bs="tp",k=10)+ti(pheo,bs="tp",k=10)+ti(do_per,bs="tp")+
            ti(sal,bs="tp"),
          data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc")+ti(date_dec,bs="tp",k=20)+ti(pheo,bs="tp",k=10)+ti(do_per,bs="tp")+
            ti(sal,bs="tp"),
          data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=10)+ti(date_dec,bs="tp",k=20)+ti(pheo,bs="tp",k=10)+ti(do_per,bs="tp")+
            ti(sal,bs="tp"),
          data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=20)+ti(date_dec,bs="tp",k=20)+ti(pheo,bs="tp",k=10)+ti(do_per,bs="tp")+
            ti(sal,bs="tp"),
          data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=20)+ti(date_dec,bs="tp",k=20)+ti(pheo,bs="tp",k=15)+ti(do_per,bs="tp")+
            ti(sal,bs="tp"),
          data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=20)+ti(date_dec,bs="tp",k=20)+ti(pheo,bs="tp",k=15)+ti(do_per,bs="tp",k=10)+
            ti(sal,bs="tp"),
          data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=20)+ti(date_dec,bs="tp",k=20)+ti(pheo,bs="tp",k=15)+ti(do_per,bs="tp",k=10)+
            ti(sal,bs="tp",k=10),
          data=data,family=gaussian(link="log"))
gam.check(gamP) ## warning

gamP<-gam(chl~ti(doy,bs="cc",k=20)+ti(date_dec,bs="tp",k=20)+ti(pheo,bs="tp",k=15)+ti(do_per,bs="tp",k=10)+
            ti(sal,bs="tp",k=8),
          data=data,family=gaussian(link="log"))
gam.check(gamP) ## warning

gamP<-gam(chl~ti(doy,bs="cc",k=20)+ti(date_dec,bs="tp",k=20)+ti(pheo,bs="tp",k=15)+ti(do_per,bs="tp",k=10)+
            ti(sal,bs="tp",k=7),
          data=data,family=gaussian(link="log"))
gam.check(gamP)  ## warning

gamP<-gam(chl~ti(doy,bs="cc",k=20)+ti(date_dec,bs="tp",k=20)+ti(pheo,bs="tp",k=15)+ti(do_per,bs="tp",k=10)+
            ti(sal,bs="tp",k=6),
          data=data,family=gaussian(link="log"))
gam.check(gamP)


gamP<-gam(chl~ti(doy,bs="cc",k=20)+ti(date_dec,bs="tp",k=20)+ti(pheo,bs="tp",k=20)+ti(do_per,bs="tp",k=10)+
            ti(sal,bs="tp",k=6),
          data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=20)+ti(date_dec,bs="tp",k=30)+ti(pheo,bs="tp",k=20)+ti(do_per,bs="tp",k=10)+
            ti(sal,bs="tp",k=6),
          data=data,family=gaussian(link="log"))
gam.check(gamP)

perStationFullMod[[7]]=gamP

####

## remove all from full model, no salinity, so reduces to parsimonious model
data=perStation[[13]]

perStationFullMod[[13]]=NULL

####

data=perStation[[17]]

gamP<-gam(chl~ti(doy,bs="cc")+ti(date_dec,bs="tp")+ti(pheo,bs="tp")+ti(tn,bs="tp")+ti(do_per,bs="tp")+
            ti(sio2,bs="tp")+ti(tp,bs="tp")+ti(tss,bs="tp")+ti(nh4,bs="tp")+ti(sal,bs="tp"),data=data,family=gaussian(link="log"))
gam.check(gamP)


gamP<-gam(chl~ti(doy,bs="cc",k=10)+ti(date_dec,bs="tp")+ti(pheo,bs="tp")+ti(tn,bs="tp")+ti(do_per,bs="tp")+
            ti(sio2,bs="tp")+ti(tp,bs="tp")+ti(tss,bs="tp")+ti(nh4,bs="tp")+ti(sal,bs="tp",k=10),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=10)+ti(date_dec,bs="tp")+ti(pheo,bs="tp")+ti(tn,bs="tp",k=10)+ti(do_per,bs="tp")+
            ti(sio2,bs="tp")+ti(tp,bs="tp")+ti(tss,bs="tp")+ti(nh4,bs="tp")+ti(sal,bs="tp",k=10),data=data,family=gaussian(link="log"))
gam.check(gamP) ## warning
 
gamP<-gam(chl~ti(doy,bs="cc",k=10)+ti(date_dec,bs="tp")+ti(pheo,bs="tp")+ti(tn,bs="tp",k=8)+ti(do_per,bs="tp")+
            ti(sio2,bs="tp")+ti(tp,bs="tp")+ti(tss,bs="tp")+ti(nh4,bs="tp")+ti(sal,bs="tp",k=10),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=10)+ti(date_dec,bs="tp")+ti(pheo,bs="tp")+ti(tn,bs="tp",k=8)+ti(do_per,bs="tp",k=8)+
            ti(sio2,bs="tp")+ti(tp,bs="tp")+ti(tss,bs="tp")+ti(nh4,bs="tp")+ti(sal,bs="tp",k=10),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=10)+ti(date_dec,bs="tp",k=8)+ti(pheo,bs="tp")+ti(tn,bs="tp",k=8)+ti(do_per,bs="tp",k=8)+
            ti(sio2,bs="tp")+ti(tp,bs="tp")+ti(tss,bs="tp")+ti(nh4,bs="tp")+ti(sal,bs="tp",k=10),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=10)+ti(date_dec,bs="tp",k=8)+ti(pheo,bs="tp")+ti(tn,bs="tp",k=8)+ti(do_per,bs="tp",k=8)+
            ti(sio2,bs="tp")+ti(tp,bs="tp")+ti(tss,bs="tp")+ti(nh4,bs="tp",k=8)+ti(sal,bs="tp",k=10),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=10)+ti(date_dec,bs="tp",k=8)+ti(pheo,bs="tp",k=8)+ti(tn,bs="tp",k=8)+ti(do_per,bs="tp",k=8)+
            ti(sio2,bs="tp")+ti(tp,bs="tp")+ti(tss,bs="tp")+ti(nh4,bs="tp",k=8)+ti(sal,bs="tp",k=10),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=10)+ti(date_dec,bs="tp",k=8)+ti(pheo,bs="tp",k=8)+ti(tn,bs="tp",k=8)+ti(do_per,bs="tp",k=8)+
            ti(sio2,bs="tp")+ti(tp,bs="tp")+ti(tss,bs="tp")+ti(nh4,bs="tp",k=8)+ti(sal,bs="tp",k=15),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=10)+ti(date_dec,bs="tp",k=8)+ti(pheo,bs="tp",k=8)+ti(tn,bs="tp",k=8)+ti(do_per,bs="tp",k=8)+
            ti(sio2,bs="tp")+ti(tp,bs="tp")+ti(tss,bs="tp",k=8)+ti(nh4,bs="tp",k=8)+ti(sal,bs="tp",k=15),data=data,family=gaussian(link="log"))
gam.check(gamP) ## warning

gamP<-gam(chl~ti(doy,bs="cc",k=10)+ti(date_dec,bs="tp",k=8)+ti(pheo,bs="tp",k=8)+ti(tn,bs="tp",k=8)+ti(do_per,bs="tp",k=8)+
            ti(sio2,bs="tp")+ti(tp,bs="tp")+ti(tss,bs="tp")+ti(nh4,bs="tp",k=8)+ti(sal,bs="tp",k=15),data=data,family=gaussian(link="log"))
gam.check(gamP)  ## go with this


gamP<-gam(chl~ti(doy,bs="cc",k=10)+ti(date_dec,bs="tp")+ti(pheo,bs="tp")+ti(tn,bs="tp",k=10)+ti(do_per,bs="tp")+
            ti(sio2,bs="tp")+ti(tp,bs="tp")+ti(tss,bs="tp")+ti(nh4,bs="tp")+ti(sal,bs="tp",k=10),data=data,family=gaussian(link="log"))
gam.check(gamP) ## warning

gamP<-gam(chl~ti(doy,bs="cc",k=10)+ti(date_dec,bs="tp")+ti(pheo,bs="tp")+ti(tn,bs="tp",k=10)+ti(do_per,bs="tp",k=10)+
            ti(sio2,bs="tp")+ti(tp,bs="tp")+ti(tss,bs="tp")+ti(nh4,bs="tp")+ti(sal,bs="tp",k=10),data=data,family=gaussian(link="log"))
gam.check(gamP) ## warning

gamP<-gam(chl~ti(doy,bs="cc",k=10)+ti(date_dec,bs="tp")+ti(pheo,bs="tp")+ti(tn,bs="tp",k=10)+ti(do_per,bs="tp",k=8)+
            ti(sio2,bs="tp")+ti(tp,bs="tp")+ti(tss,bs="tp")+ti(nh4,bs="tp")+ti(sal,bs="tp",k=10),data=data,family=gaussian(link="log"))
gam.check(gamP) ## warning

gamP<-gam(chl~ti(doy,bs="cc",k=10)+ti(date_dec,bs="tp")+ti(pheo,bs="tp")+ti(tn,bs="tp",k=10)+ti(do_per,bs="tp",k=7)+
            ti(sio2,bs="tp")+ti(tp,bs="tp")+ti(tss,bs="tp")+ti(nh4,bs="tp")+ti(sal,bs="tp",k=10),data=data,family=gaussian(link="log"))
gam.check(gamP) ## warning

gamP<-gam(chl~ti(doy,bs="cc",k=10)+ti(date_dec,bs="tp")+ti(pheo,bs="tp")+ti(tn,bs="tp",k=15)+ti(do_per,bs="tp",k=7)+
            ti(sio2,bs="tp")+ti(tp,bs="tp")+ti(tss,bs="tp")+ti(nh4,bs="tp")+ti(sal,bs="tp",k=10),data=data,family=gaussian(link="log"))
gam.check(gamP) ## warning

gamP<-gam(chl~ti(doy,bs="cc",k=10)+ti(date_dec,bs="tp")+ti(pheo,bs="tp")+ti(tn,bs="tp",k=12)+ti(do_per,bs="tp",k=7)+
            ti(sio2,bs="tp")+ti(tp,bs="tp")+ti(tss,bs="tp")+ti(nh4,bs="tp")+ti(sal,bs="tp",k=10),data=data,family=gaussian(link="log"))
gam.check(gamP) ## warning

gamP<-gam(chl~ti(doy,bs="cc",k=10)+ti(date_dec,bs="tp")+ti(pheo,bs="tp")+ti(tn,bs="tp",k=11)+ti(do_per,bs="tp",k=7)+
            ti(sio2,bs="tp")+ti(tp,bs="tp")+ti(tss,bs="tp")+ti(nh4,bs="tp")+ti(sal,bs="tp",k=10),data=data,family=gaussian(link="log"))
gam.check(gamP)  ## warning, stuck here for tn

gamP<-gam(chl~ti(doy,bs="cc",k=10)+ti(date_dec,bs="tp",k=10)+ti(pheo,bs="tp")+ti(tn,bs="tp",k=10)+ti(do_per,bs="tp",k=7)+
            ti(sio2,bs="tp")+ti(tp,bs="tp")+ti(tss,bs="tp")+ti(nh4,bs="tp")+ti(sal,bs="tp",k=10),data=data,family=gaussian(link="log"))
gam.check(gamP) ## warning

gamP<-gam(chl~ti(doy,bs="cc",k=10)+ti(date_dec,bs="tp",k=8)+ti(pheo,bs="tp")+ti(tn,bs="tp",k=10)+ti(do_per,bs="tp",k=7)+
            ti(sio2,bs="tp")+ti(tp,bs="tp")+ti(tss,bs="tp")+ti(nh4,bs="tp")+ti(sal,bs="tp",k=10),data=data,family=gaussian(link="log"))
gam.check(gamP) ## warning

gamP<-gam(chl~ti(doy,bs="cc",k=10)+ti(date_dec,bs="tp",k=7)+ti(pheo,bs="tp")+ti(tn,bs="tp",k=10)+ti(do_per,bs="tp",k=7)+
            ti(sio2,bs="tp")+ti(tp,bs="tp")+ti(tss,bs="tp")+ti(nh4,bs="tp")+ti(sal,bs="tp",k=10),data=data,family=gaussian(link="log"))
gam.check(gamP) ## warning

gamP<-gam(chl~ti(doy,bs="cc",k=10)+ti(date_dec,bs="tp",k=6)+ti(pheo,bs="tp")+ti(tn,bs="tp",k=10)+ti(do_per,bs="tp",k=7)+
            ti(sio2,bs="tp")+ti(tp,bs="tp")+ti(tss,bs="tp")+ti(nh4,bs="tp")+ti(sal,bs="tp",k=10),data=data,family=gaussian(link="log"))
gam.check(gamP)  ## warning, stop here

gamP<-gam(chl~ti(doy,bs="cc",k=10)+ti(date_dec,bs="tp")+ti(pheo,bs="tp",k=10)+ti(tn,bs="tp",k=10)+ti(do_per,bs="tp",k=7)+
            ti(sio2,bs="tp")+ti(tp,bs="tp")+ti(tss,bs="tp")+ti(nh4,bs="tp")+ti(sal,bs="tp",k=10),data=data,family=gaussian(link="log"))
gam.check(gamP) ## warning

gamP<-gam(chl~ti(doy,bs="cc",k=10)+ti(date_dec,bs="tp")+ti(pheo,bs="tp",k=8)+ti(tn,bs="tp",k=10)+ti(do_per,bs="tp",k=7)+
            ti(sio2,bs="tp")+ti(tp,bs="tp")+ti(tss,bs="tp")+ti(nh4,bs="tp")+ti(sal,bs="tp",k=10),data=data,family=gaussian(link="log"))
gam.check(gamP)  ## warning


gamP<-gam(chl~ti(doy,bs="cc",k=10)+ti(date_dec,bs="tp")+ti(pheo,bs="tp",k=7)+ti(tn,bs="tp",k=10)+ti(do_per,bs="tp",k=7)+
            ti(sio2,bs="tp")+ti(tp,bs="tp")+ti(tss,bs="tp")+ti(nh4,bs="tp")+ti(sal,bs="tp",k=10),data=data,family=gaussian(link="log"))
gam.check(gamP) ## warning


gamP<-gam(chl~ti(doy,bs="cc",k=10)+ti(date_dec,bs="tp")+ti(pheo,bs="tp",k=6)+ti(tn,bs="tp",k=10)+ti(do_per,bs="tp",k=7)+
            ti(sio2,bs="tp")+ti(tp,bs="tp")+ti(tss,bs="tp")+ti(nh4,bs="tp")+ti(sal,bs="tp",k=10),data=data,family=gaussian(link="log"))
gam.check(gamP) ## warning stuck here

gamP<-gam(chl~ti(doy,bs="cc",k=10)+ti(date_dec,bs="tp")+ti(pheo,bs="tp")+ti(tn,bs="tp",k=15)+ti(do_per,bs="tp",k=7)+
            ti(sio2,bs="tp")+ti(tp,bs="tp")+ti(tss,bs="tp")+ti(nh4,bs="tp")+ti(sal,bs="tp",k=10),data=data,family=gaussian(link="log"))
gam.check(gamP) ## warning

gamP<-gam(chl~ti(doy,bs="cc",k=10)+ti(date_dec,bs="tp")+ti(pheo,bs="tp")+ti(tn,bs="tp",k=12)+ti(do_per,bs="tp",k=7)+
            ti(sio2,bs="tp")+ti(tp,bs="tp")+ti(tss,bs="tp")+ti(nh4,bs="tp")+ti(sal,bs="tp",k=10),data=data,family=gaussian(link="log"))
gam.check(gamP)  ## warning

gamP<-gam(chl~ti(doy,bs="cc",k=10)+ti(date_dec,bs="tp",k=8)+ti(pheo,bs="tp",k=8)+ti(tn,bs="tp",k=8)+ti(do_per,bs="tp",k=8)+
            ti(sio2,bs="tp")+ti(tp,bs="tp")+ti(tss,bs="tp")+ti(nh4,bs="tp",k=8)+ti(sal,bs="tp",k=15),data=data,family=gaussian(link="log"))
gam.check(gamP)  ## go with this

perStationFullMod[[17]]=gamP
####

data=perStation[[18]]

gamP<-gam(chl~ti(doy,bs="cc")+ti(date_dec,bs="tp")+ti(pheo,bs="tp")+ti(tn,bs="tp")+ti(do_per,bs="tp")+
            ti(sio2,bs="tp")+ti(tp,bs="tp")+ti(tss,bs="tp")+ti(nh4,bs="tp")+ti(sal,bs="tp"),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc")+ti(date_dec,bs="tp")+ti(pheo,bs="tp")+ti(tn,bs="tp")+ti(do_per,bs="tp")+
            ti(sio2,bs="tp")+ti(tp,bs="tp")+ti(tss,bs="tp")+ti(nh4,bs="tp")+ti(sal,bs="tp",k=10),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc")+ti(date_dec,bs="tp")+ti(pheo,bs="tp")+ti(tn,bs="tp")+ti(do_per,bs="tp")+
            ti(sio2,bs="tp")+ti(tp,bs="tp",k=10)+ti(tss,bs="tp")+ti(nh4,bs="tp")+ti(sal,bs="tp",k=10),data=data,family=gaussian(link="log"))
gam.check(gamP) ## warning

gamP<-gam(chl~ti(doy,bs="cc")+ti(date_dec,bs="tp")+ti(pheo,bs="tp")+ti(tn,bs="tp")+ti(do_per,bs="tp")+
            ti(sio2,bs="tp")+ti(tp,bs="tp",k=8)+ti(tss,bs="tp")+ti(nh4,bs="tp")+ti(sal,bs="tp",k=10),data=data,family=gaussian(link="log"))
gam.check(gamP) ## warning

gamP<-gam(chl~ti(doy,bs="cc")+ti(date_dec,bs="tp")+ti(pheo,bs="tp")+ti(tn,bs="tp")+ti(do_per,bs="tp")+
            ti(sio2,bs="tp")+ti(tp,bs="tp",k=7)+ti(tss,bs="tp")+ti(nh4,bs="tp")+ti(sal,bs="tp",k=10),data=data,family=gaussian(link="log"))
gam.check(gamP) ## warning

gamP<-gam(chl~ti(doy,bs="cc")+ti(date_dec,bs="tp")+ti(pheo,bs="tp")+ti(tn,bs="tp")+ti(do_per,bs="tp")+
            ti(sio2,bs="tp")+ti(tp,bs="tp")+ti(tss,bs="tp")+ti(nh4,bs="tp",k=10)+ti(sal,bs="tp",k=10),data=data,family=gaussian(link="log"))
gam.check(gamP) 

gamP<-gam(chl~ti(doy,bs="cc")+ti(date_dec,bs="tp")+ti(pheo,bs="tp")+ti(tn,bs="tp")+ti(do_per,bs="tp")+
            ti(sio2,bs="tp",k=10)+ti(tp,bs="tp")+ti(tss,bs="tp")+ti(nh4,bs="tp",k=10)+ti(sal,bs="tp",k=10),data=data,family=gaussian(link="log"))
gam.check(gamP)  ## warning

gamP<-gam(chl~ti(doy,bs="cc")+ti(date_dec,bs="tp")+ti(pheo,bs="tp")+ti(tn,bs="tp")+ti(do_per,bs="tp")+
            ti(sio2,bs="tp",k=8)+ti(tp,bs="tp")+ti(tss,bs="tp")+ti(nh4,bs="tp",k=10)+ti(sal,bs="tp",k=10),data=data,family=gaussian(link="log"))
gam.check(gamP)  ## warning

gamP<-gam(chl~ti(doy,bs="cc")+ti(date_dec,bs="tp")+ti(pheo,bs="tp")+ti(tn,bs="tp")+ti(do_per,bs="tp")+
            ti(sio2,bs="tp",k=7)+ti(tp,bs="tp")+ti(tss,bs="tp")+ti(nh4,bs="tp",k=10)+ti(sal,bs="tp",k=10),data=data,family=gaussian(link="log"))
gam.check(gamP) ## warning

gamP<-gam(chl~ti(doy,bs="cc",k=10)+ti(date_dec,bs="tp",k=10)+ti(pheo,bs="tp")+ti(tn,bs="tp")+ti(do_per,bs="tp")+
            ti(sio2,bs="tp")+ti(tp,bs="tp")+ti(tss,bs="tp")+ti(nh4,bs="tp",k=10)+ti(sal,bs="tp",k=10),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=10)+ti(date_dec,bs="tp",k=10)+ti(pheo,bs="tp",k=10)+ti(tn,bs="tp")+ti(do_per,bs="tp")+
            ti(sio2,bs="tp")+ti(tp,bs="tp")+ti(tss,bs="tp")+ti(nh4,bs="tp",k=10)+ti(sal,bs="tp",k=10),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=10)+ti(date_dec,bs="tp",k=10)+ti(pheo,bs="tp",k=10)+ti(tn,bs="tp")+ti(do_per,bs="tp",k=10)+
            ti(sio2,bs="tp")+ti(tp,bs="tp")+ti(tss,bs="tp")+ti(nh4,bs="tp",k=10)+ti(sal,bs="tp",k=10),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=10)+ti(date_dec,bs="tp",k=10)+ti(pheo,bs="tp",k=10)+ti(tn,bs="tp",k=10)+ti(do_per,bs="tp",k=10)+
            ti(sio2,bs="tp")+ti(tp,bs="tp")+ti(tss,bs="tp")+ti(nh4,bs="tp",k=10)+ti(sal,bs="tp",k=10),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=20)+ti(date_dec,bs="tp",k=20)+ti(pheo,bs="tp",k=10)+ti(tn,bs="tp",k=10)+ti(do_per,bs="tp",k=10)+
            ti(sio2,bs="tp")+ti(tp,bs="tp")+ti(tss,bs="tp")+ti(nh4,bs="tp",k=10)+ti(sal,bs="tp",k=10),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=20)+ti(date_dec,bs="tp",k=20)+ti(pheo,bs="tp",k=15)+ti(tn,bs="tp",k=10)+ti(do_per,bs="tp",k=10)+
            ti(sio2,bs="tp")+ti(tp,bs="tp")+ti(tss,bs="tp")+ti(nh4,bs="tp",k=10)+ti(sal,bs="tp",k=10),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=20)+ti(date_dec,bs="tp",k=20)+ti(pheo,bs="tp",k=15)+ti(tn,bs="tp",k=10)+ti(do_per,bs="tp",k=10)+
            ti(sio2,bs="tp")+ti(tp,bs="tp")+ti(tss,bs="tp")+ti(nh4,bs="tp",k=15)+ti(sal,bs="tp",k=10),data=data,family=gaussian(link="log"))
gam.check(gamP) ## warning

gamP<-gam(chl~ti(doy,bs="cc",k=20)+ti(date_dec,bs="tp",k=20)+ti(pheo,bs="tp",k=15)+ti(tn,bs="tp",k=10)+ti(do_per,bs="tp",k=10)+
            ti(sio2,bs="tp")+ti(tp,bs="tp")+ti(tss,bs="tp")+ti(nh4,bs="tp",k=12)+ti(sal,bs="tp",k=10),data=data,family=gaussian(link="log"))
gam.check(gamP) ## warning

gamP<-gam(chl~ti(doy,bs="cc",k=20)+ti(date_dec,bs="tp",k=20)+ti(pheo,bs="tp",k=15)+ti(tn,bs="tp",k=10)+ti(do_per,bs="tp",k=10)+
            ti(sio2,bs="tp")+ti(tp,bs="tp")+ti(tss,bs="tp")+ti(nh4,bs="tp")+ti(sal,bs="tp",k=10),data=data,family=gaussian(link="log"))
gam.check(gamP) ## go with this

perStationFullMod[[18]]=gamP

####

data=perStation[[21]]

gamP<-gam(chl~ti(doy,bs="cc")+ti(date_dec,bs="tp")+ti(pheo,bs="tp")+ti(tn,bs="tp")+ti(do_per,bs="tp")+
            ti(sio2,bs="tp")+ti(tp,bs="tp")+ti(tss,bs="tp")+ti(nh4,bs="tp")+ti(sal,bs="tp"),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc")+ti(date_dec,bs="tp")+ti(pheo,bs="tp",k=10)+ti(tn,bs="tp")+ti(do_per,bs="tp")+
            ti(sio2,bs="tp")+ti(tp,bs="tp")+ti(tss,bs="tp")+ti(nh4,bs="tp")+ti(sal,bs="tp"),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc")+ti(date_dec,bs="tp")+ti(pheo,bs="tp",k=15)+ti(tn,bs="tp")+ti(do_per,bs="tp")+
            ti(sio2,bs="tp")+ti(tp,bs="tp")+ti(tss,bs="tp")+ti(nh4,bs="tp")+ti(sal,bs="tp"),data=data,family=gaussian(link="log"))
gam.check(gamP) ## warning

gamP<-gam(chl~ti(doy,bs="cc")+ti(date_dec,bs="tp")+ti(pheo,bs="tp",k=12)+ti(tn,bs="tp")+ti(do_per,bs="tp")+
            ti(sio2,bs="tp")+ti(tp,bs="tp")+ti(tss,bs="tp")+ti(nh4,bs="tp")+ti(sal,bs="tp"),data=data,family=gaussian(link="log"))
gam.check(gamP) ## warning

gamP<-gam(chl~ti(doy,bs="cc",k=10)+ti(date_dec,bs="tp",k=10)+ti(pheo,bs="tp",k=10)+ti(tn,bs="tp")+ti(do_per,bs="tp")+
            ti(sio2,bs="tp")+ti(tp,bs="tp")+ti(tss,bs="tp")+ti(nh4,bs="tp")+ti(sal,bs="tp"),data=data,family=gaussian(link="log"))
gam.check(gamP) 

gamP<-gam(chl~ti(doy,bs="cc",k=20)+ti(date_dec,bs="tp",k=20)+ti(pheo,bs="tp",k=10)+ti(tn,bs="tp")+ti(do_per,bs="tp")+
            ti(sio2,bs="tp")+ti(tp,bs="tp")+ti(tss,bs="tp")+ti(nh4,bs="tp")+ti(sal,bs="tp"),data=data,family=gaussian(link="log"))
gam.check(gamP) 

gamP<-gam(chl~ti(doy,bs="cc",k=20)+ti(date_dec,bs="tp",k=20)+ti(pheo,bs="tp",k=10)+ti(tn,bs="tp")+ti(do_per,bs="tp",k=10)+
            ti(sio2,bs="tp")+ti(tp,bs="tp")+ti(tss,bs="tp")+ti(nh4,bs="tp")+ti(sal,bs="tp"),data=data,family=gaussian(link="log"))
gam.check(gamP) 

gamP<-gam(chl~ti(doy,bs="cc",k=20)+ti(date_dec,bs="tp",k=20)+ti(pheo,bs="tp",k=10)+ti(tn,bs="tp")+ti(do_per,bs="tp",k=10)+
            ti(sio2,bs="tp",k=10)+ti(tp,bs="tp")+ti(tss,bs="tp")+ti(nh4,bs="tp")+ti(sal,bs="tp"),data=data,family=gaussian(link="log"))
gam.check(gamP)  ## warning


gamP<-gam(chl~ti(doy,bs="cc",k=20)+ti(date_dec,bs="tp",k=20)+ti(pheo,bs="tp",k=10)+ti(tn,bs="tp")+ti(do_per,bs="tp",k=10)+
            ti(sio2,bs="tp",k=8)+ti(tp,bs="tp")+ti(tss,bs="tp")+ti(nh4,bs="tp")+ti(sal,bs="tp"),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=20)+ti(date_dec,bs="tp",k=20)+ti(pheo,bs="tp",k=10)+ti(tn,bs="tp")+ti(do_per,bs="tp",k=10)+
            ti(sio2,bs="tp",k=8)+ti(tp,bs="tp",k=10)+ti(tss,bs="tp")+ti(nh4,bs="tp")+ti(sal,bs="tp"),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=20)+ti(date_dec,bs="tp",k=20)+ti(pheo,bs="tp",k=10)+ti(tn,bs="tp")+ti(do_per,bs="tp",k=10)+
            ti(sio2,bs="tp",k=8)+ti(tp,bs="tp",k=10)+ti(tss,bs="tp")+ti(nh4,bs="tp")+ti(sal,bs="tp",k=10),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=20)+ti(date_dec,bs="tp",k=20)+ti(pheo,bs="tp",k=10)+ti(tn,bs="tp",k=10)+ti(do_per,bs="tp",k=10)+
            ti(sio2,bs="tp",k=8)+ti(tp,bs="tp",k=10)+ti(tss,bs="tp")+ti(nh4,bs="tp")+ti(sal,bs="tp",k=10),data=data,family=gaussian(link="log"))
gam.check(gamP) ## warning

gamP<-gam(chl~ti(doy,bs="cc",k=20)+ti(date_dec,bs="tp",k=20)+ti(pheo,bs="tp",k=10)+ti(tn,bs="tp",k=8)+ti(do_per,bs="tp",k=10)+
            ti(sio2,bs="tp",k=8)+ti(tp,bs="tp",k=10)+ti(tss,bs="tp")+ti(nh4,bs="tp")+ti(sal,bs="tp",k=10),data=data,family=gaussian(link="log"))
gam.check(gamP) 

gamP<-gam(chl~ti(doy,bs="cc",k=20)+ti(date_dec,bs="tp",k=20)+ti(pheo,bs="tp",k=10)+ti(tn,bs="tp",k=8)+ti(do_per,bs="tp",k=10)+
            ti(sio2,bs="tp",k=8)+ti(tp,bs="tp",k=10)+ti(tss,bs="tp")+ti(nh4,bs="tp",k=10)+ti(sal,bs="tp",k=10),data=data,family=gaussian(link="log"))
gam.check(gamP) 

gamP<-gam(chl~ti(doy,bs="cc",k=20)+ti(date_dec,bs="tp",k=25)+ti(pheo,bs="tp",k=10)+ti(tn,bs="tp",k=8)+ti(do_per,bs="tp",k=10)+
            ti(sio2,bs="tp",k=8)+ti(tp,bs="tp",k=10)+ti(tss,bs="tp")+ti(nh4,bs="tp",k=10)+ti(sal,bs="tp",k=10),data=data,family=gaussian(link="log"))
gam.check(gamP)  ## worse

gamP<-gam(chl~ti(doy,bs="cc",k=20)+ti(date_dec,bs="tp",k=20)+ti(pheo,bs="tp",k=10)+ti(tn,bs="tp",k=8)+ti(do_per,bs="tp",k=10)+
            ti(sio2,bs="tp",k=8)+ti(tp,bs="tp",k=10)+ti(tss,bs="tp")+ti(nh4,bs="tp",k=10)+ti(sal,bs="tp",k=10),data=data,family=gaussian(link="log"))
gam.check(gamP) ## go with this

perStationFullMod[[21]]=gamP

####

data=perStation[[22]]

gamP<-gam(chl~ti(doy,bs="cc")+ti(date_dec,bs="tp")+ti(pheo,bs="tp")+ti(tn,bs="tp")+ti(do_per,bs="tp")+
            ti(sio2,bs="tp")+ti(tp,bs="tp")+ti(tss,bs="tp")+ti(nh4,bs="tp")+ti(sal,bs="tp"),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc")+ti(date_dec,bs="tp")+ti(pheo,bs="tp")+ti(tn,bs="tp")+ti(do_per,bs="tp")+
            ti(sio2,bs="tp",k=10)+ti(tp,bs="tp")+ti(tss,bs="tp")+ti(nh4,bs="tp")+ti(sal,bs="tp"),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc")+ti(date_dec,bs="tp")+ti(pheo,bs="tp")+ti(tn,bs="tp")+ti(do_per,bs="tp")+
            ti(sio2,bs="tp",k=10)+ti(tp,bs="tp")+ti(tss,bs="tp",k=10)+ti(nh4,bs="tp")+ti(sal,bs="tp"),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc")+ti(date_dec,bs="tp")+ti(pheo,bs="tp")+ti(tn,bs="tp")+ti(do_per,bs="tp")+
            ti(sio2,bs="tp",k=10)+ti(tp,bs="tp")+ti(tss,bs="tp",k=15)+ti(nh4,bs="tp")+ti(sal,bs="tp"),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc")+ti(date_dec,bs="tp")+ti(pheo,bs="tp")+ti(tn,bs="tp")+ti(do_per,bs="tp")+
            ti(sio2,bs="tp",k=10)+ti(tp,bs="tp")+ti(tss,bs="tp",k=20)+ti(nh4,bs="tp")+ti(sal,bs="tp"),data=data,family=gaussian(link="log"))
gam.check(gamP) ## warning


gamP<-gam(chl~ti(doy,bs="cc")+ti(date_dec,bs="tp")+ti(pheo,bs="tp")+ti(tn,bs="tp")+ti(do_per,bs="tp")+
            ti(sio2,bs="tp",k=10)+ti(tp,bs="tp")+ti(tss,bs="tp",k=18)+ti(nh4,bs="tp")+ti(sal,bs="tp"),data=data,family=gaussian(link="log"))
gam.check(gamP) ## getting slow

gamP<-gam(chl~ti(doy,bs="cc")+ti(date_dec,bs="tp")+ti(pheo,bs="tp",k=10)+ti(tn,bs="tp")+ti(do_per,bs="tp")+
            ti(sio2,bs="tp",k=10)+ti(tp,bs="tp")+ti(tss,bs="tp",k=18)+ti(nh4,bs="tp")+ti(sal,bs="tp"),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc")+ti(date_dec,bs="tp")+ti(pheo,bs="tp",k=10)+ti(tn,bs="tp",k=8)+ti(do_per,bs="tp")+
            ti(sio2,bs="tp",k=10)+ti(tp,bs="tp")+ti(tss,bs="tp",k=18)+ti(nh4,bs="tp")+ti(sal,bs="tp"),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=10)+ti(date_dec,bs="tp",k=10)+ti(pheo,bs="tp",k=10)+ti(tn,bs="tp",k=8)+ti(do_per,bs="tp")+
            ti(sio2,bs="tp",k=10)+ti(tp,bs="tp")+ti(tss,bs="tp",k=18)+ti(nh4,bs="tp")+ti(sal,bs="tp"),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=10)+ti(date_dec,bs="tp",k=10)+ti(pheo,bs="tp",k=10)+ti(tn,bs="tp",k=8)+ti(do_per,bs="tp",k=10)+
            ti(sio2,bs="tp",k=10)+ti(tp,bs="tp")+ti(tss,bs="tp",k=18)+ti(nh4,bs="tp")+ti(sal,bs="tp"),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=10)+ti(date_dec,bs="tp",k=10)+ti(pheo,bs="tp",k=10)+ti(tn,bs="tp",k=8)+ti(do_per,bs="tp",k=10)+
            ti(sio2,bs="tp",k=10)+ti(tp,bs="tp",k=8)+ti(tss,bs="tp",k=18)+ti(nh4,bs="tp")+ti(sal,bs="tp"),data=data,family=gaussian(link="log"))
gam.check(gamP)


## detour

gamP<-gam(chl~ti(doy,bs="cc",k=20)+ti(date_dec,bs="tp",k=20)+ti(pheo,bs="tp",k=10)+ti(tn,bs="tp",k=8)+ti(do_per,bs="tp",k=10)+
            ti(sio2,bs="tp",k=10)+ti(tp,bs="tp",k=8)+ti(tss,bs="tp",k=18)+ti(nh4,bs="tp")+ti(sal,bs="tp"),data=data,family=gaussian(link="log"))
gam.check(gamP) ## slow

gamP<-gam(chl~ti(doy,bs="cc",k=20)+ti(date_dec,bs="tp",k=20)+ti(pheo,bs="tp",k=10)+ti(tn,bs="tp",k=8)+ti(do_per,bs="tp",k=10)+
            ti(sio2,bs="tp",k=10)+ti(tp,bs="tp",k=8)+ti(tss,bs="tp",k=18)+ti(nh4,bs="tp",k=8)+ti(sal,bs="tp"),data=data,family=gaussian(link="log"))
gam.check(gamP) ## go with this



gamP<-gam(chl~ti(doy,bs="cc",k=10)+ti(date_dec,bs="tp",k=10)+ti(pheo,bs="tp",k=10)+ti(tn,bs="tp",k=8)+ti(do_per,bs="tp",k=10)+
            ti(sio2,bs="tp",k=10)+ti(tp,bs="tp",k=8)+ti(tss,bs="tp",k=18)+ti(nh4,bs="tp")+ti(sal,bs="tp",k=8),data=data,family=gaussian(link="log"))
gam.check(gamP) #### resids v. linear pred gets condensed

gamP<-gam(chl~ti(doy,bs="cc",k=10)+ti(date_dec,bs="tp",k=10)+ti(pheo,bs="tp",k=10)+ti(tn,bs="tp",k=8)+ti(do_per,bs="tp",k=10)+
            ti(sio2,bs="tp",k=10)+ti(tp,bs="tp",k=8)+ti(tss,bs="tp",k=18)+ti(nh4,bs="tp",k=8)+ti(sal,bs="tp",k=8),data=data,family=gaussian(link="log"))
gam.check(gamP) ## resids v. linear pred gets condensed

gamP<-gam(chl~ti(doy,bs="cc",k=20)+ti(date_dec,bs="tp",k=20)+ti(pheo,bs="tp",k=10)+ti(tn,bs="tp",k=8)+ti(do_per,bs="tp",k=10)+
            ti(sio2,bs="tp",k=10)+ti(tp,bs="tp",k=8)+ti(tss,bs="tp",k=18)+ti(nh4,bs="tp",k=8)+ti(sal,bs="tp",k=8),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=20)+ti(date_dec,bs="tp",k=20)+ti(pheo,bs="tp",k=10)+ti(tn,bs="tp",k=8)+ti(do_per,bs="tp",k=10)+
            ti(sio2,bs="tp",k=10)+ti(tp,bs="tp",k=8)+ti(tss,bs="tp",k=18)+ti(nh4,bs="tp",k=8)+ti(sal,bs="tp",k=10),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=20)+ti(date_dec,bs="tp",k=20)+ti(pheo,bs="tp",k=10)+ti(tn,bs="tp",k=8)+ti(do_per,bs="tp",k=10)+
            ti(sio2,bs="tp",k=10)+ti(tp,bs="tp",k=10)+ti(tss,bs="tp",k=18)+ti(nh4,bs="tp",k=8)+ti(sal,bs="tp",k=10),data=data,family=gaussian(link="log"))
gam.check(gamP) ## warning

gamP<-gam(chl~ti(doy,bs="cc",k=25)+ti(date_dec,bs="tp",k=25)+ti(pheo,bs="tp",k=10)+ti(tn,bs="tp",k=8)+ti(do_per,bs="tp",k=10)+
            ti(sio2,bs="tp",k=10)+ti(tp,bs="tp",k=8)+ti(tss,bs="tp",k=18)+ti(nh4,bs="tp",k=8)+ti(sal,bs="tp",k=10),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=25)+ti(date_dec,bs="tp",k=25)+ti(pheo,bs="tp",k=10)+ti(tn,bs="tp",k=9)+ti(do_per,bs="tp",k=10)+
            ti(sio2,bs="tp",k=10)+ti(tp,bs="tp",k=8)+ti(tss,bs="tp",k=18)+ti(nh4,bs="tp",k=8)+ti(sal,bs="tp",k=10),data=data,family=gaussian(link="log"))
gam.check(gamP) ## warning

gamP<-gam(chl~ti(doy,bs="cc",k=25)+ti(date_dec,bs="tp",k=25)+ti(pheo,bs="tp",k=10)+ti(tn,bs="tp",k=8)+ti(do_per,bs="tp",k=10)+
            ti(sio2,bs="tp",k=10)+ti(tp,bs="tp",k=8)+ti(tss,bs="tp",k=18)+ti(nh4,bs="tp",k=8)+ti(sal,bs="tp",k=10),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=20)+ti(date_dec,bs="tp",k=20)+ti(pheo,bs="tp",k=10)+ti(tn,bs="tp",k=8)+ti(do_per,bs="tp",k=10)+
            ti(sio2,bs="tp",k=10)+ti(tp,bs="tp",k=8)+ti(tss,bs="tp",k=18)+ti(nh4,bs="tp",k=8)+ti(sal,bs="tp"),data=data,family=gaussian(link="log"))
gam.check(gamP) ## go with this

perStationFullMod[[22]]=gamP

####
data=perStation[[23]]

gamP<-gam(chl~ti(doy,bs="cc")+ti(date_dec,bs="tp")+ti(pheo,bs="tp")+ti(tn,bs="tp")+ti(do_per,bs="tp")+
            ti(sio2,bs="tp")+ti(tp,bs="tp")+ti(tss,bs="tp")+ti(nh4,bs="tp")+ti(sal,bs="tp"),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc")+ti(date_dec,bs="tp")+ti(pheo,bs="tp",k=10)+ti(tn,bs="tp")+ti(do_per,bs="tp")+
            ti(sio2,bs="tp")+ti(tp,bs="tp")+ti(tss,bs="tp")+ti(nh4,bs="tp")+ti(sal,bs="tp"),data=data,family=gaussian(link="log"))
gam.check(gamP)


gamP<-gam(chl~ti(doy,bs="cc")+ti(date_dec,bs="tp")+ti(pheo,bs="tp",k=10)+ti(tn,bs="tp")+ti(do_per,bs="tp")+
            ti(sio2,bs="tp")+ti(tp,bs="tp")+ti(tss,bs="tp")+ti(nh4,bs="tp")+ti(sal,bs="tp",k=10),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc")+ti(date_dec,bs="tp")+ti(pheo,bs="tp",k=10)+ti(tn,bs="tp")+ti(do_per,bs="tp")+
            ti(sio2,bs="tp")+ti(tp,bs="tp")+ti(tss,bs="tp",k=10)+ti(nh4,bs="tp")+ti(sal,bs="tp",k=10),data=data,family=gaussian(link="log"))
gam.check(gamP) ## warning

gamP<-gam(chl~ti(doy,bs="cc")+ti(date_dec,bs="tp")+ti(pheo,bs="tp",k=10)+ti(tn,bs="tp")+ti(do_per,bs="tp")+
            ti(sio2,bs="tp")+ti(tp,bs="tp")+ti(tss,bs="tp",k=8)+ti(nh4,bs="tp")+ti(sal,bs="tp",k=10),data=data,family=gaussian(link="log"))
gam.check(gamP)  ## warning

gamP<-gam(chl~ti(doy,bs="cc",k=15)+ti(date_dec,bs="tp",k=15)+ti(pheo,bs="tp",k=10)+ti(tn,bs="tp")+ti(do_per,bs="tp")+
            ti(sio2,bs="tp")+ti(tp,bs="tp")+ti(tss,bs="tp")+ti(nh4,bs="tp")+ti(sal,bs="tp",k=10),data=data,family=gaussian(link="log"))
gam.check(gamP) 

gamP<-gam(chl~ti(doy,bs="cc",k=15)+ti(date_dec,bs="tp",k=15)+ti(pheo,bs="tp",k=10)+ti(tn,bs="tp")+ti(do_per,bs="tp",k=8)+
            ti(sio2,bs="tp")+ti(tp,bs="tp")+ti(tss,bs="tp")+ti(nh4,bs="tp")+ti(sal,bs="tp",k=10),data=data,family=gaussian(link="log"))
gam.check(gamP) 

gamP<-gam(chl~ti(doy,bs="cc",k=15)+ti(date_dec,bs="tp",k=15)+ti(pheo,bs="tp",k=10)+ti(tn,bs="tp")+ti(do_per,bs="tp",k=8)+
            ti(sio2,bs="tp",k=8)+ti(tp,bs="tp")+ti(tss,bs="tp")+ti(nh4,bs="tp")+ti(sal,bs="tp",k=10),data=data,family=gaussian(link="log"))
gam.check(gamP) 

gamP<-gam(chl~ti(doy,bs="cc",k=15)+ti(date_dec,bs="tp",k=15)+ti(pheo,bs="tp",k=10)+ti(tn,bs="tp")+ti(do_per,bs="tp",k=8)+
            ti(sio2,bs="tp",k=8)+ti(tp,bs="tp")+ti(tss,bs="tp")+ti(nh4,bs="tp",k=8)+ti(sal,bs="tp",k=10),data=data,family=gaussian(link="log"))
gam.check(gamP)  ## warning

gamP<-gam(chl~ti(doy,bs="cc",k=15)+ti(date_dec,bs="tp",k=15)+ti(pheo,bs="tp",k=10)+ti(tn,bs="tp")+ti(do_per,bs="tp",k=8)+
            ti(sio2,bs="tp",k=8)+ti(tp,bs="tp")+ti(tss,bs="tp")+ti(nh4,bs="tp",k=7)+ti(sal,bs="tp",k=10),data=data,family=gaussian(link="log"))
gam.check(gamP) ## warning

gamP<-gam(chl~ti(doy,bs="cc",k=15)+ti(date_dec,bs="tp",k=15)+ti(pheo,bs="tp",k=15)+ti(tn,bs="tp")+ti(do_per,bs="tp",k=8)+
            ti(sio2,bs="tp",k=8)+ti(tp,bs="tp")+ti(tss,bs="tp")+ti(nh4,bs="tp")+ti(sal,bs="tp",k=10),data=data,family=gaussian(link="log"))
gam.check(gamP)  ## warning

gamP<-gam(chl~ti(doy,bs="cc",k=15)+ti(date_dec,bs="tp",k=15)+ti(pheo,bs="tp",k=12)+ti(tn,bs="tp")+ti(do_per,bs="tp",k=8)+
            ti(sio2,bs="tp",k=8)+ti(tp,bs="tp")+ti(tss,bs="tp")+ti(nh4,bs="tp")+ti(sal,bs="tp",k=10),data=data,family=gaussian(link="log"))
gam.check(gamP) ## warning

gamP<-gam(chl~ti(doy,bs="cc",k=20)+ti(date_dec,bs="tp",k=20)+ti(pheo,bs="tp",k=10)+ti(tn,bs="tp")+ti(do_per,bs="tp",k=8)+
            ti(sio2,bs="tp",k=8)+ti(tp,bs="tp")+ti(tss,bs="tp")+ti(nh4,bs="tp")+ti(sal,bs="tp",k=10),data=data,family=gaussian(link="log"))
gam.check(gamP) 


gamP<-gam(chl~ti(doy,bs="cc",k=20)+ti(date_dec,bs="tp",k=20)+ti(pheo,bs="tp",k=10)+ti(tn,bs="tp",k=8)+ti(do_per,bs="tp",k=8)+
            ti(sio2,bs="tp",k=8)+ti(tp,bs="tp")+ti(tss,bs="tp")+ti(nh4,bs="tp")+ti(sal,bs="tp",k=10),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=25)+ti(date_dec,bs="tp",k=25)+ti(pheo,bs="tp",k=10)+ti(tn,bs="tp",k=8)+ti(do_per,bs="tp",k=8)+
            ti(sio2,bs="tp",k=8)+ti(tp,bs="tp")+ti(tss,bs="tp")+ti(nh4,bs="tp")+ti(sal,bs="tp",k=10),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=25)+ti(date_dec,bs="tp",k=30)+ti(pheo,bs="tp",k=10)+ti(tn,bs="tp",k=8)+ti(do_per,bs="tp",k=8)+
            ti(sio2,bs="tp",k=8)+ti(tp,bs="tp")+ti(tss,bs="tp")+ti(nh4,bs="tp")+ti(sal,bs="tp",k=10),data=data,family=gaussian(link="log"))
gam.check(gamP)

perStationFullMod[[23]]=gamP
