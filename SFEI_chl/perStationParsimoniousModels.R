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

gamP<-gam(chl~ti(doy,bs="cc",k=5)+ti(date_dec,bs="tp",k=15)+ti(pheo,bs="tp")+ti(tn,bs="tp",k=5)+ti(do_per,bs="tp",k=10),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=5)+ti(date_dec,bs="tp",k=15)+ti(pheo,bs="tp")+ti(tn,bs="tp",k=5)+ti(do_per,bs="tp",k=15),data=data,family=gaussian(link="log"))
gam.check(gamP)## warning

gamP<-gam(chl~ti(doy,bs="cc",k=5)+ti(date_dec,bs="tp",k=15)+ti(pheo,bs="tp")+ti(tn,bs="tp",k=5)+ti(do_per,bs="tp",k=12),data=data,family=gaussian(link="log"))
gam.check(gamP) # warning

gamP<-gam(chl~ti(doy,bs="cc",k=5)+ti(date_dec,bs="tp",k=20)+ti(pheo,bs="tp")+ti(tn,bs="tp",k=5)+ti(do_per,bs="tp",k=10),data=data,family=gaussian(link="log"))
gam.check(gamP) ## slow
gamP<-gam(chl~ti(doy,bs="cc",k=5)+ti(date_dec,bs="tp",k=20)+ti(pheo,bs="tp")+ti(tn,bs="tp",k=10)+ti(do_per,bs="tp",k=10),data=data,family=gaussian(link="log"))
gam.check(gamP)  ## warning

gamP<-gam(chl~ti(doy,bs="cc",k=5)+ti(date_dec,bs="tp",k=20)+ti(pheo,bs="tp")+ti(tn,bs="tp",k=8)+ti(do_per,bs="tp",k=10),data=data,family=gaussian(link="log"))
gam.check(gamP) 

gamP<-gam(chl~ti(doy,bs="cc",k=5)+ti(date_dec,bs="tp",k=20)+ti(pheo,bs="tp",k=10)+ti(tn,bs="tp",k=8)+ti(do_per,bs="tp",k=10),data=data,family=gaussian(link="log"))
gam.check(gamP) 

gamP<-gam(chl~ti(doy,bs="cc",k=10)+ti(date_dec,bs="tp",k=20)+ti(pheo,bs="tp",k=10)+ti(tn,bs="tp",k=8)+ti(do_per,bs="tp",k=10),data=data,family=gaussian(link="log"))
gam.check(gamP) 

gamP<-gam(chl~ti(doy,bs="cc",k=10)+ti(date_dec,bs="tp",k=20)+ti(pheo,bs="tp",k=15)+ti(tn,bs="tp",k=8)+ti(do_per,bs="tp",k=10),data=data,family=gaussian(link="log"))
gam.check(gamP) 

gamP<-gam(chl~ti(doy,bs="cc",k=10)+ti(date_dec,bs="tp",k=25)+ti(pheo,bs="tp",k=15)+ti(tn,bs="tp",k=8)+ti(do_per,bs="tp",k=10),data=data,family=gaussian(link="log"))
gam.check(gamP) 

gamP<-gam(chl~ti(doy,bs="cc",k=10)+ti(date_dec,bs="tp",k=30)+ti(pheo,bs="tp",k=15)+ti(tn,bs="tp",k=8)+ti(do_per,bs="tp",k=10),data=data,family=gaussian(link="log"))
gam.check(gamP) 

gamP<-gam(chl~ti(doy,bs="cc",k=15)+ti(date_dec,bs="tp",k=30)+ti(pheo,bs="tp",k=15)+ti(tn,bs="tp",k=8)+ti(do_per,bs="tp",k=10),data=data,family=gaussian(link="log"))
gam.check(gamP) 

gamP<-gam(chl~ti(doy,bs="cc",k=15)+ti(date_dec,bs="tp",k=40)+ti(pheo,bs="tp",k=15)+ti(tn,bs="tp",k=8)+ti(do_per,bs="tp",k=10),data=data,family=gaussian(link="log"))
gam.check(gamP) 

gamP<-gam(chl~ti(doy,bs="cc",k=15)+ti(date_dec,bs="tp",k=45)+ti(pheo,bs="tp",k=15)+ti(tn,bs="tp",k=8)+ti(do_per,bs="tp",k=10),data=data,family=gaussian(link="log"))
gam.check(gamP) 

gamP<-gam(chl~ti(doy,bs="cc",k=15)+ti(date_dec,bs="tp",k=50)+ti(pheo,bs="tp",k=15)+ti(tn,bs="tp",k=8)+ti(do_per,bs="tp",k=10),data=data,family=gaussian(link="log"))
gam.check(gamP)  ## this is going to have to be good enough

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

gamP<-gam(chl~ti(doy,bs="cc",k=15)+ti(date_dec,bs="tp",k=25)+ti(pheo,bs="tp",k=10)+ti(tn,bs="tp")+ti(do_per,bs="tp"),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=15)+ti(date_dec,bs="tp",k=25)+ti(pheo,bs="tp",k=10)+ti(tn,bs="tp",k=10)+ti(do_per,bs="tp"),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=15)+ti(date_dec,bs="tp",k=25)+ti(pheo,bs="tp",k=10)+ti(tn,bs="tp",k=15)+ti(do_per,bs="tp"),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=15)+ti(date_dec,bs="tp",k=25)+ti(pheo,bs="tp",k=10)+ti(tn,bs="tp",k=15)+ti(do_per,bs="tp",k=10),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=15)+ti(date_dec,bs="tp",k=25)+ti(pheo,bs="tp",k=15)+ti(tn,bs="tp",k=15)+ti(do_per,bs="tp",k=10),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=15)+ti(date_dec,bs="tp",k=30)+ti(pheo,bs="tp",k=15)+ti(tn,bs="tp",k=15)+ti(do_per,bs="tp",k=10),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=15)+ti(date_dec,bs="tp",k=35)+ti(pheo,bs="tp",k=15)+ti(tn,bs="tp",k=15)+ti(do_per,bs="tp",k=10),data=data,family=gaussian(link="log"))
gam.check(gamP)

perStationParsMod[[2]]=gamP ## 

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

####

data=perStation[[4]]

gamP<-gam(chl~ti(doy,bs="cc")+ti(date_dec,bs="tp")+ti(pheo,bs="tp")+ti(tn,bs="tp")+ti(do_per,bs="tp"),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=10)+ti(date_dec,bs="tp",k=10)+ti(pheo,bs="tp",k=10)+ti(tn,bs="tp")+ti(do_per,bs="tp",k=10),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=15)+ti(date_dec,bs="tp",k=15)+ti(pheo,bs="tp",k=15)+ti(tn,bs="tp",k=10)+ti(do_per,bs="tp",k=15),data=data,family=gaussian(link="log"))
gam.check(gamP) ## seems good, but resids v. linear pred plot looks bad

## try expanding just in case
gamP<-gam(chl~ti(doy,bs="cc",k=20)+ti(date_dec,bs="tp",k=20)+ti(pheo,bs="tp",k=20)+ti(tn,bs="tp",k=15)+ti(do_per,bs="tp",k=20),data=data,family=gaussian(link="log"))
gam.check(gamP) ## that helped but warnings

gamP<-gam(chl~ti(doy,bs="cc",k=20)+ti(date_dec,bs="tp",k=20)+ti(pheo,bs="tp",k=15)+ti(tn,bs="tp",k=15)+ti(do_per,bs="tp",k=20),data=data,family=gaussian(link="log"))
gam.check(gamP) ## still warnings

gamP<-gam(chl~ti(doy,bs="cc",k=17)+ti(date_dec,bs="tp",k=17)+ti(pheo,bs="tp",k=15)+ti(tn,bs="tp",k=15)+ti(do_per,bs="tp",k=17),data=data,family=gaussian(link="log"))
gam.check(gamP) ## still warnings

gamP<-gam(chl~ti(doy,bs="cc",k=15)+ti(date_dec,bs="tp",k=15)+ti(pheo,bs="tp",k=15)+ti(tn,bs="tp",k=15)+ti(do_per,bs="tp",k=17),data=data,family=gaussian(link="log"))
gam.check(gamP) ## still doesn't work

gamP<-gam(chl~ti(doy,bs="cc",k=16)+ti(date_dec,bs="tp",k=15)+ti(pheo,bs="tp",k=15)+ti(tn,bs="tp",k=10)+ti(do_per,bs="tp",k=15),data=data,family=gaussian(link="log"))
gamP<-gam(chl~ti(doy,bs="cc",k=17)+ti(date_dec,bs="tp",k=15)+ti(pheo,bs="tp",k=15)+ti(tn,bs="tp",k=10)+ti(do_per,bs="tp",k=15),data=data,family=gaussian(link="log"))
gamP<-gam(chl~ti(doy,bs="cc",k=18)+ti(date_dec,bs="tp",k=15)+ti(pheo,bs="tp",k=15)+ti(tn,bs="tp",k=10)+ti(do_per,bs="tp",k=15),data=data,family=gaussian(link="log"))
gamP<-gam(chl~ti(doy,bs="cc",k=19)+ti(date_dec,bs="tp",k=15)+ti(pheo,bs="tp",k=15)+ti(tn,bs="tp",k=10)+ti(do_per,bs="tp",k=15),data=data,family=gaussian(link="log"))
gamP<-gam(chl~ti(doy,bs="cc",k=20)+ti(date_dec,bs="tp",k=15)+ti(pheo,bs="tp",k=15)+ti(tn,bs="tp",k=10)+ti(do_per,bs="tp",k=15),data=data,family=gaussian(link="log"))
## all of these work, next variable
gam.check(gamP)
gamP<-gam(chl~ti(doy,bs="cc",k=20)+ti(date_dec,bs="tp",k=15)+ti(pheo,bs="tp",k=15)+ti(tn,bs="tp",k=10)+ti(do_per,bs="tp",k=16),data=data,family=gaussian(link="log"))
gamP<-gam(chl~ti(doy,bs="cc",k=20)+ti(date_dec,bs="tp",k=15)+ti(pheo,bs="tp",k=15)+ti(tn,bs="tp",k=10)+ti(do_per,bs="tp",k=17),data=data,family=gaussian(link="log"))
gamP<-gam(chl~ti(doy,bs="cc",k=20)+ti(date_dec,bs="tp",k=15)+ti(pheo,bs="tp",k=15)+ti(tn,bs="tp",k=10)+ti(do_per,bs="tp",k=18),data=data,family=gaussian(link="log"))
gamP<-gam(chl~ti(doy,bs="cc",k=20)+ti(date_dec,bs="tp",k=15)+ti(pheo,bs="tp",k=15)+ti(tn,bs="tp",k=10)+ti(do_per,bs="tp",k=19),data=data,family=gaussian(link="log"))
gam.check(gamP)
## this took a long time, go back to 18 and check, also resids v. linear pred look really weird
gamP<-gam(chl~ti(doy,bs="cc",k=20)+ti(date_dec,bs="tp",k=15)+ti(pheo,bs="tp",k=15)+ti(tn,bs="tp",k=10)+ti(do_per,bs="tp",k=18),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=20)+ti(date_dec,bs="tp",k=15)+ti(pheo,bs="tp",k=15)+ti(tn,bs="tp",k=10)+ti(do_per,bs="tp",k=20),data=data,family=gaussian(link="log"))
## here is where we get a warning
gamP<-gam(chl~ti(doy,bs="cc",k=20)+ti(date_dec,bs="tp",k=15)+ti(pheo,bs="tp",k=15)+ti(tn,bs="tp",k=15)+ti(do_per,bs="tp",k=18),data=data,family=gaussian(link="log"))
## still errors here

gamP<-gam(chl~ti(doy,bs="cc",k=20)+ti(date_dec,bs="tp",k=15)+ti(pheo,bs="tp",k=15)+ti(tn,bs="tp",k=15)+ti(do_per,bs="tp",k=15),data=data,family=gaussian(link="log"))
## still errors

## going back to something that worked above
gamP<-gam(chl~ti(doy,bs="cc",k=20)+ti(date_dec,bs="tp",k=15)+ti(pheo,bs="tp",k=15)+ti(tn,bs="tp",k=10)+ti(do_per,bs="tp",k=15),data=data,family=gaussian(link="log"))
gam.check(gamP) ## resids v. linear pred looks bad, but everything else looks decent

perStationParsMod[[4]]=gamP 

####
data=perStation[[5]]

gamP<-gam(chl~ti(doy,bs="cc")+ti(date_dec,bs="tp")+ti(pheo,bs="tp")+ti(tn,bs="tp")+ti(do_per,bs="tp"),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc")+ti(date_dec,bs="tp",k=10)+ti(pheo,bs="tp")+ti(tn,bs="tp",k=10)+ti(do_per,bs="tp"),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc")+ti(date_dec,bs="tp",k=15)+ti(pheo,bs="tp")+ti(tn,bs="tp",k=15)+ti(do_per,bs="tp"),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=10)+ti(date_dec,bs="tp",k=15)+ti(pheo,bs="tp",k=10)+ti(tn,bs="tp",k=15)+ti(do_per,bs="tp"),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=15)+ti(date_dec,bs="tp",k=15)+ti(pheo,bs="tp",k=15)+ti(tn,bs="tp",k=15)+ti(do_per,bs="tp"),data=data,family=gaussian(link="log"))
gam.check(gamP) ## warnings

gamP<-gam(chl~ti(doy,bs="cc",k=11)+ti(date_dec,bs="tp",k=15)+ti(pheo,bs="tp",k=10)+ti(tn,bs="tp",k=15)+ti(do_per,bs="tp"),data=data,family=gaussian(link="log"))
gam.check(gamP) ## this is fine

gamP<-gam(chl~ti(doy,bs="cc",k=10)+ti(date_dec,bs="tp",k=15)+ti(pheo,bs="tp",k=11)+ti(tn,bs="tp",k=15)+ti(do_per,bs="tp"),data=data,family=gaussian(link="log"))
gam.check(gamP) ## this is fine

gamP<-gam(chl~ti(doy,bs="cc",k=13)+ti(date_dec,bs="tp",k=15)+ti(pheo,bs="tp",k=13)+ti(tn,bs="tp",k=15)+ti(do_per,bs="tp"),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=13)+ti(date_dec,bs="tp",k=15)+ti(pheo,bs="tp",k=13)+ti(tn,bs="tp",k=15)+ti(do_per,bs="tp",k=10),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=13)+ti(date_dec,bs="tp",k=20)+ti(pheo,bs="tp",k=13)+ti(tn,bs="tp",k=15)+ti(do_per,bs="tp",k=10),data=data,family=gaussian(link="log"))
gam.check(gamP) ## I think this is as good as we are going to get.

perStationParsMod[[5]]=gamP 

####

data=perStation[[6]]

gamP<-gam(chl~ti(doy,bs="cc")+ti(date_dec,bs="tp")+ti(pheo,bs="tp")+ti(tn,bs="tp")+ti(do_per,bs="tp"),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc")+ti(date_dec,bs="tp",k=10)+ti(pheo,bs="tp",k=10)+ti(tn,bs="tp")+ti(do_per,bs="tp"),data=data,family=gaussian(link="log"))
gam.check(gamP) ## warnings

gamP<-gam(chl~ti(doy,bs="cc")+ti(date_dec,bs="tp")+ti(pheo,bs="tp",k=10)+ti(tn,bs="tp")+ti(do_per,bs="tp"),data=data,family=gaussian(link="log"))
gam.check(gamP) ## warnings

gamP<-gam(chl~ti(doy,bs="cc")+ti(date_dec,bs="tp")+ti(pheo,bs="tp",k=8)+ti(tn,bs="tp")+ti(do_per,bs="tp"),data=data,family=gaussian(link="log"))
gam.check(gamP) ## fine

gamP<-gam(chl~ti(doy,bs="cc")+ti(date_dec,bs="tp",k=10)+ti(pheo,bs="tp",k=8)+ti(tn,bs="tp")+ti(do_per,bs="tp"),data=data,family=gaussian(link="log"))
gam.check(gamP) ## fine

gamP<-gam(chl~ti(doy,bs="cc")+ti(date_dec,bs="tp",k=15)+ti(pheo,bs="tp",k=8)+ti(tn,bs="tp")+ti(do_per,bs="tp"),data=data,family=gaussian(link="log"))
gam.check(gamP) ## fine

gamP<-gam(chl~ti(doy,bs="cc")+ti(date_dec,bs="tp",k=20)+ti(pheo,bs="tp",k=8)+ti(tn,bs="tp")+ti(do_per,bs="tp"),data=data,family=gaussian(link="log"))
gam.check(gamP) ## fine

gamP<-gam(chl~ti(doy,bs="cc")+ti(date_dec,bs="tp",k=20)+ti(pheo,bs="tp",k=9)+ti(tn,bs="tp")+ti(do_per,bs="tp"),data=data,family=gaussian(link="log"))
gam.check(gamP) ## warning

gamP<-gam(chl~ti(doy,bs="cc",k=10)+ti(date_dec,bs="tp",k=20)+ti(pheo,bs="tp",k=8)+ti(tn,bs="tp")+ti(do_per,bs="tp"),data=data,family=gaussian(link="log"))
gam.check(gamP) 

gamP<-gam(chl~ti(doy,bs="cc",k=15)+ti(date_dec,bs="tp",k=20)+ti(pheo,bs="tp",k=8)+ti(tn,bs="tp")+ti(do_per,bs="tp"),data=data,family=gaussian(link="log"))
gam.check(gamP) 

gamP<-gam(chl~ti(doy,bs="cc",k=15)+ti(date_dec,bs="tp",k=20)+ti(pheo,bs="tp",k=8)+ti(tn,bs="tp")+ti(do_per,bs="tp",k=10),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=20)+ti(date_dec,bs="tp",k=20)+ti(pheo,bs="tp",k=8)+ti(tn,bs="tp")+ti(do_per,bs="tp",k=10),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=20)+ti(date_dec,bs="tp",k=20)+ti(pheo,bs="tp",k=8)+ti(tn,bs="tp")+ti(do_per,bs="tp",k=15),data=data,family=gaussian(link="log"))
gam.check(gamP) ##warning

gamP<-gam(chl~ti(doy,bs="cc",k=20)+ti(date_dec,bs="tp",k=20)+ti(pheo,bs="tp",k=8)+ti(tn,bs="tp")+ti(do_per,bs="tp",k=12),data=data,family=gaussian(link="log"))
gam.check(gamP) ## fine

gamP<-gam(chl~ti(doy,bs="cc",k=20)+ti(date_dec,bs="tp",k=20)+ti(pheo,bs="tp",k=8)+ti(tn,bs="tp")+ti(do_per,bs="tp",k=14),data=data,family=gaussian(link="log"))
gam.check(gamP) ## warning

gamP<-gam(chl~ti(doy,bs="cc",k=20)+ti(date_dec,bs="tp",k=20)+ti(pheo,bs="tp",k=8)+ti(tn,bs="tp")+ti(do_per,bs="tp",k=12),data=data,family=gaussian(link="log"))
gam.check(gamP)  ## go with this

perStationParsMod[[6]]=gamP 

####

data=perStation[[7]]
gamP<-gam(chl~ti(doy,bs="cc")+ti(date_dec,bs="tp")+ti(pheo,bs="tp")+ti(tn,bs="tp")+ti(do_per,bs="tp"),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=10)+ti(date_dec,bs="tp",k=10)+ti(pheo,bs="tp",k=10)+ti(tn,bs="tp",k=10)+ti(do_per,bs="tp",k=10),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=15)+ti(date_dec,bs="tp",k=15)+ti(pheo,bs="tp",k=15)+ti(tn,bs="tp",k=15)+ti(do_per,bs="tp",k=15),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=15)+ti(date_dec,bs="tp",k=20)+ti(pheo,bs="tp",k=15)+ti(tn,bs="tp",k=15)+ti(do_per,bs="tp",k=15),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=15)+ti(date_dec,bs="tp",k=25)+ti(pheo,bs="tp",k=15)+ti(tn,bs="tp",k=15)+ti(do_per,bs="tp",k=15),data=data,family=gaussian(link="log"))
gam.check(gamP)

perStationParsMod[[7]]=gamP 

####

data=perStation[[8]]
gamP<-gam(chl~ti(doy,bs="cc")+ti(date_dec,bs="tp")+ti(pheo,bs="tp")+ti(tn,bs="tp")+ti(do_per,bs="tp"),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc")+ti(date_dec,bs="tp",k=10)+ti(pheo,bs="tp",k=10)+ti(tn,bs="tp",k=10)+ti(do_per,bs="tp"),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc")+ti(date_dec,bs="tp",k=15)+ti(pheo,bs="tp",k=10)+ti(tn,bs="tp",k=10)+ti(do_per,bs="tp"),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc")+ti(date_dec,bs="tp",k=20)+ti(pheo,bs="tp",k=10)+ti(tn,bs="tp",k=10)+ti(do_per,bs="tp"),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc")+ti(date_dec,bs="tp",k=25)+ti(pheo,bs="tp",k=15)+ti(tn,bs="tp",k=10)+ti(do_per,bs="tp"),data=data,family=gaussian(link="log"))
gam.check(gamP) ## warnings

gamP<-gam(chl~ti(doy,bs="cc")+ti(date_dec,bs="tp",k=25)+ti(pheo,bs="tp",k=10)+ti(tn,bs="tp",k=10)+ti(do_per,bs="tp"),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc")+ti(date_dec,bs="tp",k=30)+ti(pheo,bs="tp",k=10)+ti(tn,bs="tp",k=10)+ti(do_per,bs="tp"),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc")+ti(date_dec,bs="tp",k=40)+ti(pheo,bs="tp",k=10)+ti(tn,bs="tp",k=10)+ti(do_per,bs="tp"),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc")+ti(date_dec,bs="tp",k=40)+ti(pheo,bs="tp",k=15)+ti(tn,bs="tp",k=10)+ti(do_per,bs="tp"),data=data,family=gaussian(link="log"))
gam.check(gamP) ## warnings

gamP<-gam(chl~ti(doy,bs="cc")+ti(date_dec,bs="tp",k=40)+ti(pheo,bs="tp",k=12)+ti(tn,bs="tp",k=10)+ti(do_per,bs="tp"),data=data,family=gaussian(link="log"))
gam.check(gamP)  ## still warnings

gamP<-gam(chl~ti(doy,bs="cc")+ti(date_dec,bs="tp",k=40)+ti(pheo,bs="tp",k=10)+ti(tn,bs="tp",k=15)+ti(do_per,bs="tp"),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc")+ti(date_dec,bs="tp",k=50)+ti(pheo,bs="tp",k=10)+ti(tn,bs="tp",k=15)+ti(do_per,bs="tp"),data=data,family=gaussian(link="log"))
gam.check(gamP) 

gamP<-gam(chl~ti(doy,bs="cc")+ti(date_dec,bs="tp",k=50)+ti(pheo,bs="tp",k=11)+ti(tn,bs="tp",k=15)+ti(do_per,bs="tp"),data=data,family=gaussian(link="log"))
gam.check(gamP)  ## warning

gamP<-gam(chl~ti(doy,bs="cc")+ti(date_dec,bs="tp",k=50)+ti(pheo,bs="tp",k=10)+ti(tn,bs="tp",k=15)+ti(do_per,bs="tp"),data=data,family=gaussian(link="log"))
gam.check(gamP)  ## stick to this, although note we wish we could expand pheo

perStationParsMod[[8]]=gamP 

####

data=perStation[[9]]
gamP<-gam(chl~ti(doy,bs="cc")+ti(date_dec,bs="tp")+ti(pheo,bs="tp")+ti(tn,bs="tp")+ti(do_per,bs="tp"),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=10)+ti(date_dec,bs="tp",k=10)+ti(pheo,bs="tp",k=10)+ti(tn,bs="tp",k=10)+ti(do_per,bs="tp",k=10),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=15)+ti(date_dec,bs="tp",k=15)+ti(pheo,bs="tp",k=10)+ti(tn,bs="tp",k=15)+ti(do_per,bs="tp",k=15),data=data,family=gaussian(link="log"))
gam.check(gamP) ##warnings

gamP<-gam(chl~ti(doy,bs="cc",k=15)+ti(date_dec,bs="tp",k=10)+ti(pheo,bs="tp",k=10)+ti(tn,bs="tp",k=10)+ti(do_per,bs="tp",k=10),data=data,family=gaussian(link="log"))
gam.check(gamP) ## fine

gamP<-gam(chl~ti(doy,bs="cc",k=15)+ti(date_dec,bs="tp",k=15)+ti(pheo,bs="tp",k=10)+ti(tn,bs="tp",k=10)+ti(do_per,bs="tp",k=10),data=data,family=gaussian(link="log"))
gam.check(gamP) ## fine

gamP<-gam(chl~ti(doy,bs="cc",k=15)+ti(date_dec,bs="tp",k=15)+ti(pheo,bs="tp",k=10)+ti(tn,bs="tp",k=15)+ti(do_per,bs="tp",k=10),data=data,family=gaussian(link="log"))
gam.check(gamP) ## here is where the warning comes in, come back to, first test out last variable

gamP<-gam(chl~ti(doy,bs="cc",k=15)+ti(date_dec,bs="tp",k=15)+ti(pheo,bs="tp",k=10)+ti(tn,bs="tp",k=10)+ti(do_per,bs="tp",k=15),data=data,family=gaussian(link="log"))
gam.check(gamP) ## fine

gamP<-gam(chl~ti(doy,bs="cc",k=15)+ti(date_dec,bs="tp",k=20)+ti(pheo,bs="tp",k=10)+ti(tn,bs="tp",k=10)+ti(do_per,bs="tp",k=15),data=data,family=gaussian(link="log"))
gam.check(gamP) 

gamP<-gam(chl~ti(doy,bs="cc",k=15)+ti(date_dec,bs="tp",k=20)+ti(pheo,bs="tp",k=15)+ti(tn,bs="tp",k=10)+ti(do_per,bs="tp",k=15),data=data,family=gaussian(link="log"))
gam.check(gamP) 

gamP<-gam(chl~ti(doy,bs="cc",k=15)+ti(date_dec,bs="tp",k=20)+ti(pheo,bs="tp",k=15)+ti(tn,bs="tp",k=12)+ti(do_per,bs="tp",k=15),data=data,family=gaussian(link="log"))
gam.check(gamP)  ## this is fine

gamP<-gam(chl~ti(doy,bs="cc",k=15)+ti(date_dec,bs="tp",k=20)+ti(pheo,bs="tp",k=15)+ti(tn,bs="tp",k=14)+ti(do_per,bs="tp",k=15),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=20)+ti(date_dec,bs="tp",k=20)+ti(pheo,bs="tp",k=20)+ti(tn,bs="tp",k=14)+ti(do_per,bs="tp",k=15),data=data,family=gaussian(link="log"))
gam.check(gamP) ## warnings

gamP<-gam(chl~ti(doy,bs="cc",k=20)+ti(date_dec,bs="tp",k=20)+ti(pheo,bs="tp",k=15)+ti(tn,bs="tp",k=14)+ti(do_per,bs="tp",k=15),data=data,family=gaussian(link="log"))
gam.check(gamP) ## this is fine

gamP<-gam(chl~ti(doy,bs="cc",k=25)+ti(date_dec,bs="tp",k=20)+ti(pheo,bs="tp",k=15)+ti(tn,bs="tp",k=14)+ti(do_per,bs="tp",k=15),data=data,family=gaussian(link="log"))
gam.check(gamP) 

gamP<-gam(chl~ti(doy,bs="cc",k=25)+ti(date_dec,bs="tp",k=20)+ti(pheo,bs="tp",k=20)+ti(tn,bs="tp",k=14)+ti(do_per,bs="tp",k=15),data=data,family=gaussian(link="log"))
gam.check(gamP)  ## warnings

gamP<-gam(chl~ti(doy,bs="cc",k=25)+ti(date_dec,bs="tp",k=20)+ti(pheo,bs="tp",k=15)+ti(tn,bs="tp",k=15)+ti(do_per,bs="tp",k=15),data=data,family=gaussian(link="log"))
gam.check(gamP)  ## warnings

gamP<-gam(chl~ti(doy,bs="cc",k=25)+ti(date_dec,bs="tp",k=20)+ti(pheo,bs="tp",k=18)+ti(tn,bs="tp",k=14)+ti(do_per,bs="tp",k=15),data=data,family=gaussian(link="log"))
gam.check(gamP)  ## slow but ok, let's just go with this

perStationParsMod[[9]]=gamP 

####

data=perStation[[10]]
gamP<-gam(chl~ti(doy,bs="cc")+ti(date_dec,bs="tp")+ti(pheo,bs="tp")+ti(tn,bs="tp")+ti(do_per,bs="tp"),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc")+ti(date_dec,bs="tp",k=10)+ti(pheo,bs="tp",k=10)+ti(tn,bs="tp")+ti(do_per,bs="tp"),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc")+ti(date_dec,bs="tp",k=20)+ti(pheo,bs="tp",k=20)+ti(tn,bs="tp")+ti(do_per,bs="tp"),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc")+ti(date_dec,bs="tp",k=30)+ti(pheo,bs="tp",k=20)+ti(tn,bs="tp")+ti(do_per,bs="tp"),data=data,family=gaussian(link="log"))
gam.check(gamP)

perStationParsMod[[10]]=gamP 


####

data=perStation[[11]]
gamP<-gam(chl~ti(doy,bs="cc")+ti(date_dec,bs="tp")+ti(pheo,bs="tp")+ti(tn,bs="tp")+ti(do_per,bs="tp"),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=10)+ti(date_dec,bs="tp",k=10)+ti(pheo,bs="tp",k=10)+ti(tn,bs="tp",k=10)+ti(do_per,bs="tp",k=10),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=15)+ti(date_dec,bs="tp",k=15)+ti(pheo,bs="tp",k=15)+ti(tn,bs="tp",k=10)+ti(do_per,bs="tp",k=15),data=data,family=gaussian(link="log"))
gam.check(gamP) ## warnings

gamP<-gam(chl~ti(doy,bs="cc",k=15)+ti(date_dec,bs="tp",k=10)+ti(pheo,bs="tp",k=10)+ti(tn,bs="tp",k=10)+ti(do_per,bs="tp",k=10),data=data,family=gaussian(link="log"))
gam.check(gamP) ## this works

gamP<-gam(chl~ti(doy,bs="cc",k=20)+ti(date_dec,bs="tp",k=10)+ti(pheo,bs="tp",k=10)+ti(tn,bs="tp",k=10)+ti(do_per,bs="tp",k=10),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=25)+ti(date_dec,bs="tp",k=10)+ti(pheo,bs="tp",k=10)+ti(tn,bs="tp",k=10)+ti(do_per,bs="tp",k=10),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=25)+ti(date_dec,bs="tp",k=15)+ti(pheo,bs="tp",k=10)+ti(tn,bs="tp",k=10)+ti(do_per,bs="tp",k=10),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=25)+ti(date_dec,bs="tp",k=20)+ti(pheo,bs="tp",k=10)+ti(tn,bs="tp",k=10)+ti(do_per,bs="tp",k=10),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=25)+ti(date_dec,bs="tp",k=25)+ti(pheo,bs="tp",k=10)+ti(tn,bs="tp",k=10)+ti(do_per,bs="tp",k=10),data=data,family=gaussian(link="log"))
gam.check(gamP) ## warnings

gamP<-gam(chl~ti(doy,bs="cc",k=30)+ti(date_dec,bs="tp",k=25)+ti(pheo,bs="tp",k=10)+ti(tn,bs="tp",k=10)+ti(do_per,bs="tp",k=10),data=data,family=gaussian(link="log"))
gam.check(gamP)  ## warnings

gamP<-gam(chl~ti(doy,bs="cc",k=25)+ti(date_dec,bs="tp",k=20)+ti(pheo,bs="tp",k=15)+ti(tn,bs="tp",k=10)+ti(do_per,bs="tp",k=10),data=data,family=gaussian(link="log"))
gam.check(gamP) ## warnings

gamP<-gam(chl~ti(doy,bs="cc",k=25)+ti(date_dec,bs="tp",k=20)+ti(pheo,bs="tp",k=12)+ti(tn,bs="tp",k=10)+ti(do_per,bs="tp",k=10),data=data,family=gaussian(link="log"))
gam.check(gamP) ## still warnings

gamP<-gam(chl~ti(doy,bs="cc",k=25)+ti(date_dec,bs="tp",k=20)+ti(pheo,bs="tp",k=11)+ti(tn,bs="tp",k=10)+ti(do_per,bs="tp",k=10),data=data,family=gaussian(link="log"))
gam.check(gamP) ## this works

gamP<-gam(chl~ti(doy,bs="cc",k=28)+ti(date_dec,bs="tp",k=20)+ti(pheo,bs="tp",k=11)+ti(tn,bs="tp",k=10)+ti(do_per,bs="tp",k=10),data=data,family=gaussian(link="log"))
gam.check(gamP) 

gamP<-gam(chl~ti(doy,bs="cc",k=28)+ti(date_dec,bs="tp",k=20)+ti(pheo,bs="tp",k=11)+ti(tn,bs="tp",k=15)+ti(do_per,bs="tp",k=10),data=data,family=gaussian(link="log"))
gam.check(gamP)  ## slow but works

gamP<-gam(chl~ti(doy,bs="cc",k=28)+ti(date_dec,bs="tp",k=25)+ti(pheo,bs="tp",k=11)+ti(tn,bs="tp",k=15)+ti(do_per,bs="tp",k=10),data=data,family=gaussian(link="log"))
gam.check(gamP)  ## warning

gamP<-gam(chl~ti(doy,bs="cc",k=28)+ti(date_dec,bs="tp",k=23)+ti(pheo,bs="tp",k=11)+ti(tn,bs="tp",k=15)+ti(do_per,bs="tp",k=10),data=data,family=gaussian(link="log"))
gam.check(gamP) ## warning

gamP<-gam(chl~ti(doy,bs="cc",k=28)+ti(date_dec,bs="tp",k=22)+ti(pheo,bs="tp",k=11)+ti(tn,bs="tp",k=15)+ti(do_per,bs="tp",k=10),data=data,family=gaussian(link="log"))
gam.check(gamP) ## this works, slow

gamP<-gam(chl~ti(doy,bs="cc",k=28)+ti(date_dec,bs="tp",k=22)+ti(pheo,bs="tp",k=11)+ti(tn,bs="tp",k=20)+ti(do_per,bs="tp",k=10),data=data,family=gaussian(link="log"))
gam.check(gamP) ## warnings

gamP<-gam(chl~ti(doy,bs="cc",k=28)+ti(date_dec,bs="tp",k=22)+ti(pheo,bs="tp",k=11)+ti(tn,bs="tp",k=18)+ti(do_per,bs="tp",k=10),data=data,family=gaussian(link="log"))
gam.check(gamP) ## warnings

gamP<-gam(chl~ti(doy,bs="cc",k=28)+ti(date_dec,bs="tp",k=22)+ti(pheo,bs="tp",k=11)+ti(tn,bs="tp",k=16)+ti(do_per,bs="tp",k=10),data=data,family=gaussian(link="log"))
gam.check(gamP)  ## works

gamP<-gam(chl~ti(doy,bs="cc",k=28)+ti(date_dec,bs="tp",k=22)+ti(pheo,bs="tp",k=11)+ti(tn,bs="tp",k=17)+ti(do_per,bs="tp",k=10),data=data,family=gaussian(link="log"))
gam.check(gamP)  ## warning

gamP<-gam(chl~ti(doy,bs="cc",k=28)+ti(date_dec,bs="tp",k=22)+ti(pheo,bs="tp",k=11)+ti(tn,bs="tp",k=16)+ti(do_per,bs="tp",k=10),data=data,family=gaussian(link="log"))
gam.check(gamP)  ## go with this, slow

perStationParsMod[[11]]=gamP

####

dim(perStation[[12]]) ## only has 38 entries, skip and look into later

####

data=perStation[[13]]
gamP<-gam(chl~ti(doy,bs="cc")+ti(date_dec,bs="tp")+ti(pheo,bs="tp")+ti(tn,bs="tp")+ti(do_per,bs="tp"),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=10)+ti(date_dec,bs="tp",k=10)+ti(pheo,bs="tp",k=10)+ti(tn,bs="tp",k=10)+ti(do_per,bs="tp",k=10),data=data,family=gaussian(link="log"))
gam.check(gamP) ## warnings, need to start slower

gamP<-gam(chl~ti(doy,bs="cc")+ti(date_dec,bs="tp",k=10)+ti(pheo,bs="tp")+ti(tn,bs="tp")+ti(do_per,bs="tp"),data=data,family=gaussian(link="log"))
gam.check(gamP) ## fine

gamP<-gam(chl~ti(doy,bs="cc")+ti(date_dec,bs="tp",k=20)+ti(pheo,bs="tp")+ti(tn,bs="tp")+ti(do_per,bs="tp"),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc")+ti(date_dec,bs="tp",k=30)+ti(pheo,bs="tp")+ti(tn,bs="tp")+ti(do_per,bs="tp"),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc")+ti(date_dec,bs="tp",k=40)+ti(pheo,bs="tp")+ti(tn,bs="tp")+ti(do_per,bs="tp"),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc")+ti(date_dec,bs="tp",k=40)+ti(pheo,bs="tp",k=10)+ti(tn,bs="tp")+ti(do_per,bs="tp"),data=data,family=gaussian(link="log"))
gam.check(gamP) ## warnings, come back

gamP<-gam(chl~ti(doy,bs="cc")+ti(date_dec,bs="tp",k=40)+ti(pheo,bs="tp")+ti(tn,bs="tp",k=10)+ti(do_per,bs="tp"),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc")+ti(date_dec,bs="tp",k=40)+ti(pheo,bs="tp")+ti(tn,bs="tp",k=10)+ti(do_per,bs="tp",k=10),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=10)+ti(date_dec,bs="tp",k=40)+ti(pheo,bs="tp")+ti(tn,bs="tp",k=10)+ti(do_per,bs="tp",k=10),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=20)+ti(date_dec,bs="tp",k=40)+ti(pheo,bs="tp")+ti(tn,bs="tp",k=10)+ti(do_per,bs="tp",k=10),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=20)+ti(date_dec,bs="tp",k=40)+ti(pheo,bs="tp",k=8)+ti(tn,bs="tp",k=10)+ti(do_per,bs="tp",k=10),data=data,family=gaussian(link="log"))
gam.check(gamP) ## warnings 

gamP<-gam(chl~ti(doy,bs="cc",k=20)+ti(date_dec,bs="tp",k=40)+ti(pheo,bs="tp",k=7)+ti(tn,bs="tp",k=10)+ti(do_per,bs="tp",k=10),data=data,family=gaussian(link="log"))
gam.check(gamP) ## works, go with this

perStationParsMod[[13]]=gamP

####

data=perStation[[14]]
gamP<-gam(chl~ti(doy,bs="cc")+ti(date_dec,bs="tp")+ti(pheo,bs="tp")+ti(tn,bs="tp")+ti(do_per,bs="tp"),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=10)+ti(date_dec,bs="tp",k=10)+ti(pheo,bs="tp",k=10)+ti(tn,bs="tp")+ti(do_per,bs="tp",k=10),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=10)+ti(date_dec,bs="tp",k=15)+ti(pheo,bs="tp",k=10)+ti(tn,bs="tp",k=10)+ti(do_per,bs="tp",k=10),data=data,family=gaussian(link="log"))
gam.check(gamP) ## warnings

gamP<-gam(chl~ti(doy,bs="cc",k=10)+ti(date_dec,bs="tp",k=10)+ti(pheo,bs="tp",k=10)+ti(tn,bs="tp",k=10)+ti(do_per,bs="tp",k=10),data=data,family=gaussian(link="log"))
gam.check(gamP) ## warnings

gamP<-gam(chl~ti(doy,bs="cc",k=10)+ti(date_dec,bs="tp",k=15)+ti(pheo,bs="tp",k=10)+ti(tn,bs="tp")+ti(do_per,bs="tp",k=10),data=data,family=gaussian(link="log"))
gam.check(gamP) ## good

gamP<-gam(chl~ti(doy,bs="cc",k=10)+ti(date_dec,bs="tp",k=15)+ti(pheo,bs="tp",k=10)+ti(tn,bs="tp",k=8)+ti(do_per,bs="tp",k=10),data=data,family=gaussian(link="log"))
gam.check(gamP) ## good

gamP<-gam(chl~ti(doy,bs="cc",k=15)+ti(date_dec,bs="tp",k=15)+ti(pheo,bs="tp",k=10)+ti(tn,bs="tp",k=8)+ti(do_per,bs="tp",k=10),data=data,family=gaussian(link="log"))
gam.check(gamP) 

gamP<-gam(chl~ti(doy,bs="cc",k=20)+ti(date_dec,bs="tp",k=15)+ti(pheo,bs="tp",k=10)+ti(tn,bs="tp",k=8)+ti(do_per,bs="tp",k=10),data=data,family=gaussian(link="log"))
gam.check(gamP) ## good go with this

perStationParsMod[[14]]=gamP

####

data=perStation[[15]]
gamP<-gam(chl~ti(doy,bs="cc")+ti(date_dec,bs="tp")+ti(pheo,bs="tp")+ti(tn,bs="tp")+ti(do_per,bs="tp"),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=10)+ti(date_dec,bs="tp",k=10)+ti(pheo,bs="tp",k=10)+ti(tn,bs="tp")+ti(do_per,bs="tp"),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=15)+ti(date_dec,bs="tp",k=15)+ti(pheo,bs="tp",k=15)+ti(tn,bs="tp",k=10)+ti(do_per,bs="tp",k=10),data=data,family=gaussian(link="log"))
gam.check(gamP) ## warnings

gamP<-gam(chl~ti(doy,bs="cc",k=15)+ti(date_dec,bs="tp",k=15)+ti(pheo,bs="tp",k=15)+ti(tn,bs="tp")+ti(do_per,bs="tp"),data=data,family=gaussian(link="log"))
gam.check(gamP) ## warnings

gamP<-gam(chl~ti(doy,bs="cc",k=15)+ti(date_dec,bs="tp",k=10)+ti(pheo,bs="tp",k=10)+ti(tn,bs="tp")+ti(do_per,bs="tp"),data=data,family=gaussian(link="log"))
gam.check(gamP) ## fine

gamP<-gam(chl~ti(doy,bs="cc",k=15)+ti(date_dec,bs="tp",k=10)+ti(pheo,bs="tp",k=15)+ti(tn,bs="tp")+ti(do_per,bs="tp"),data=data,family=gaussian(link="log"))
gam.check(gamP) ## warnings

gamP<-gam(chl~ti(doy,bs="cc",k=15)+ti(date_dec,bs="tp",k=15)+ti(pheo,bs="tp",k=10)+ti(tn,bs="tp")+ti(do_per,bs="tp"),data=data,family=gaussian(link="log"))
gam.check(gamP) ## fine

gamP<-gam(chl~ti(doy,bs="cc",k=15)+ti(date_dec,bs="tp",k=20)+ti(pheo,bs="tp",k=10)+ti(tn,bs="tp")+ti(do_per,bs="tp"),data=data,family=gaussian(link="log"))
gam.check(gamP) 

gamP<-gam(chl~ti(doy,bs="cc",k=15)+ti(date_dec,bs="tp",k=25)+ti(pheo,bs="tp",k=10)+ti(tn,bs="tp")+ti(do_per,bs="tp"),data=data,family=gaussian(link="log"))
gam.check(gamP) 

gamP<-gam(chl~ti(doy,bs="cc",k=15)+ti(date_dec,bs="tp",k=25)+ti(pheo,bs="tp",k=10)+ti(tn,bs="tp",k=10)+ti(do_per,bs="tp"),data=data,family=gaussian(link="log"))
gam.check(gamP) 

gamP<-gam(chl~ti(doy,bs="cc",k=15)+ti(date_dec,bs="tp",k=25)+ti(pheo,bs="tp",k=10)+ti(tn,bs="tp",k=15)+ti(do_per,bs="tp"),data=data,family=gaussian(link="log"))
gam.check(gamP) 

gamP<-gam(chl~ti(doy,bs="cc",k=15)+ti(date_dec,bs="tp",k=25)+ti(pheo,bs="tp",k=10)+ti(tn,bs="tp",k=15)+ti(do_per,bs="tp",k=10),data=data,family=gaussian(link="log"))
gam.check(gamP) 

gamP<-gam(chl~ti(doy,bs="cc",k=20)+ti(date_dec,bs="tp",k=25)+ti(pheo,bs="tp",k=10)+ti(tn,bs="tp",k=15)+ti(do_per,bs="tp",k=10),data=data,family=gaussian(link="log"))
gam.check(gamP) 

gamP<-gam(chl~ti(doy,bs="cc",k=20)+ti(date_dec,bs="tp",k=25)+ti(pheo,bs="tp",k=12)+ti(tn,bs="tp",k=15)+ti(do_per,bs="tp",k=10),data=data,family=gaussian(link="log"))
gam.check(gamP) ## slow but works

gamP<-gam(chl~ti(doy,bs="cc",k=20)+ti(date_dec,bs="tp",k=25)+ti(pheo,bs="tp",k=14)+ti(tn,bs="tp",k=15)+ti(do_per,bs="tp",k=10),data=data,family=gaussian(link="log"))
gam.check(gamP) ## warnings

gamP<-gam(chl~ti(doy,bs="cc",k=20)+ti(date_dec,bs="tp",k=25)+ti(pheo,bs="tp",k=13)+ti(tn,bs="tp",k=15)+ti(do_per,bs="tp",k=10),data=data,family=gaussian(link="log"))
gam.check(gamP) ## warnings

gamP<-gam(chl~ti(doy,bs="cc",k=20)+ti(date_dec,bs="tp",k=25)+ti(pheo,bs="tp",k=12)+ti(tn,bs="tp",k=15)+ti(do_per,bs="tp",k=10),data=data,family=gaussian(link="log"))
gam.check(gamP) ## go with this

perStationParsMod[[15]]=gamP


####

data=perStation[[16]]
gamP<-gam(chl~ti(doy,bs="cc")+ti(date_dec,bs="tp")+ti(pheo,bs="tp")+ti(tn,bs="tp")+ti(do_per,bs="tp"),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=10)+ti(date_dec,bs="tp",k=10)+ti(pheo,bs="tp",k=10)+ti(tn,bs="tp",k=10)+ti(do_per,bs="tp",k=10),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=15)+ti(date_dec,bs="tp",k=15)+ti(pheo,bs="tp",k=15)+ti(tn,bs="tp",k=15)+ti(do_per,bs="tp",k=15),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=20)+ti(date_dec,bs="tp",k=20)+ti(pheo,bs="tp",k=20)+ti(tn,bs="tp",k=20)+ti(do_per,bs="tp",k=20),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=20)+ti(date_dec,bs="tp",k=25)+ti(pheo,bs="tp",k=25)+ti(tn,bs="tp",k=20)+ti(do_per,bs="tp",k=20),data=data,family=gaussian(link="log"))
gam.check(gamP) ## warnings

gamP<-gam(chl~ti(doy,bs="cc",k=20)+ti(date_dec,bs="tp",k=25)+ti(pheo,bs="tp",k=20)+ti(tn,bs="tp",k=20)+ti(do_per,bs="tp",k=20),data=data,family=gaussian(link="log"))
gam.check(gamP) ## this works

gamP<-gam(chl~ti(doy,bs="cc",k=20)+ti(date_dec,bs="tp",k=30)+ti(pheo,bs="tp",k=20)+ti(tn,bs="tp",k=20)+ti(do_per,bs="tp",k=20),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=20)+ti(date_dec,bs="tp",k=35)+ti(pheo,bs="tp",k=20)+ti(tn,bs="tp",k=20)+ti(do_per,bs="tp",k=20),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=20)+ti(date_dec,bs="tp",k=40)+ti(pheo,bs="tp",k=20)+ti(tn,bs="tp",k=20)+ti(do_per,bs="tp",k=20),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=20)+ti(date_dec,bs="tp",k=50)+ti(pheo,bs="tp",k=20)+ti(tn,bs="tp",k=20)+ti(do_per,bs="tp",k=20),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=25)+ti(date_dec,bs="tp",k=50)+ti(pheo,bs="tp",k=20)+ti(tn,bs="tp",k=20)+ti(do_per,bs="tp",k=25),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=25)+ti(date_dec,bs="tp",k=55)+ti(pheo,bs="tp",k=20)+ti(tn,bs="tp",k=20)+ti(do_per,bs="tp",k=25),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=30)+ti(date_dec,bs="tp",k=55)+ti(pheo,bs="tp",k=20)+ti(tn,bs="tp",k=20)+ti(do_per,bs="tp",k=25),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=35)+ti(date_dec,bs="tp",k=55)+ti(pheo,bs="tp",k=20)+ti(tn,bs="tp",k=20)+ti(do_per,bs="tp",k=25),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=35)+ti(date_dec,bs="tp",k=55)+ti(pheo,bs="tp",k=23)+ti(tn,bs="tp",k=20)+ti(do_per,bs="tp",k=25),data=data,family=gaussian(link="log"))
gam.check(gamP) ## Really slow

gamP<-gam(chl~ti(doy,bs="cc",k=35)+ti(date_dec,bs="tp",k=55)+ti(pheo,bs="tp",k=24)+ti(tn,bs="tp",k=20)+ti(do_per,bs="tp",k=25),data=data,family=gaussian(link="log"))
gam.check(gamP)  ## warnings

gamP<-gam(chl~ti(doy,bs="cc",k=35)+ti(date_dec,bs="tp",k=55)+ti(pheo,bs="tp",k=23)+ti(tn,bs="tp",k=20)+ti(do_per,bs="tp",k=25),data=data,family=gaussian(link="log"))
gam.check(gamP) ## go with this, slowest one yet 

perStationParsMod[[16]]=gamP

####

data=perStation[[17]]
gamP<-gam(chl~ti(doy,bs="cc")+ti(date_dec,bs="tp")+ti(pheo,bs="tp")+ti(tn,bs="tp")+ti(do_per,bs="tp"),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc")+ti(date_dec,bs="tp",k=10)+ti(pheo,bs="tp",k=10)+ti(tn,bs="tp",k=10)+ti(do_per,bs="tp",k=10),data=data,family=gaussian(link="log"))
gam.check(gamP) ## warning

gamP<-gam(chl~ti(doy,bs="cc")+ti(date_dec,bs="tp",k=10)+ti(pheo,bs="tp")+ti(tn,bs="tp")+ti(do_per,bs="tp"),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc")+ti(date_dec,bs="tp",k=20)+ti(pheo,bs="tp")+ti(tn,bs="tp")+ti(do_per,bs="tp"),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc")+ti(date_dec,bs="tp",k=20)+ti(pheo,bs="tp")+ti(tn,bs="tp")+ti(do_per,bs="tp",k=10),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc")+ti(date_dec,bs="tp",k=20)+ti(pheo,bs="tp")+ti(tn,bs="tp")+ti(do_per,bs="tp",k=15),data=data,family=gaussian(link="log"))
gam.check(gamP) ## warning

gamP<-gam(chl~ti(doy,bs="cc")+ti(date_dec,bs="tp",k=20)+ti(pheo,bs="tp")+ti(tn,bs="tp",k=10)+ti(do_per,bs="tp",k=10),data=data,family=gaussian(link="log"))
gam.check(gamP) ## warning

gamP<-gam(chl~ti(doy,bs="cc")+ti(date_dec,bs="tp",k=20)+ti(pheo,bs="tp")+ti(tn,bs="tp",k=8)+ti(do_per,bs="tp",k=10),data=data,family=gaussian(link="log"))
gam.check(gamP) 

gamP<-gam(chl~ti(doy,bs="cc")+ti(date_dec,bs="tp",k=20)+ti(pheo,bs="tp",k=10)+ti(tn,bs="tp",k=8)+ti(do_per,bs="tp",k=10),data=data,family=gaussian(link="log"))
gam.check(gamP) 

gamP<-gam(chl~ti(doy,bs="cc")+ti(date_dec,bs="tp",k=20)+ti(pheo,bs="tp",k=15)+ti(tn,bs="tp",k=8)+ti(do_per,bs="tp",k=10),data=data,family=gaussian(link="log"))
gam.check(gamP) 

gamP<-gam(chl~ti(doy,bs="cc",k=10)+ti(date_dec,bs="tp",k=20)+ti(pheo,bs="tp",k=15)+ti(tn,bs="tp",k=8)+ti(do_per,bs="tp",k=10),data=data,family=gaussian(link="log"))
gam.check(gamP) 

gamP<-gam(chl~ti(doy,bs="cc",k=15)+ti(date_dec,bs="tp",k=20)+ti(pheo,bs="tp",k=15)+ti(tn,bs="tp",k=8)+ti(do_per,bs="tp",k=10),data=data,family=gaussian(link="log"))
gam.check(gamP) 

gamP<-gam(chl~ti(doy,bs="cc",k=15)+ti(date_dec,bs="tp",k=25)+ti(pheo,bs="tp",k=15)+ti(tn,bs="tp",k=8)+ti(do_per,bs="tp",k=10),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=15)+ti(date_dec,bs="tp",k=30)+ti(pheo,bs="tp",k=15)+ti(tn,bs="tp",k=8)+ti(do_per,bs="tp",k=10),data=data,family=gaussian(link="log"))
gam.check(gamP) 

gamP<-gam(chl~ti(doy,bs="cc",k=20)+ti(date_dec,bs="tp",k=40)+ti(pheo,bs="tp",k=15)+ti(tn,bs="tp",k=8)+ti(do_per,bs="tp",k=10),data=data,family=gaussian(link="log"))
gam.check(gamP) 

gamP<-gam(chl~ti(doy,bs="cc",k=20)+ti(date_dec,bs="tp",k=60)+ti(pheo,bs="tp",k=15)+ti(tn,bs="tp",k=8)+ti(do_per,bs="tp",k=10),data=data,family=gaussian(link="log"))
gam.check(gamP)  ## This is getting crazy

gamP<-gam(chl~ti(doy,bs="cc",k=20)+ti(date_dec,bs="tp",k=70)+ti(pheo,bs="tp",k=15)+ti(tn,bs="tp",k=8)+ti(do_per,bs="tp",k=10),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=25)+ti(date_dec,bs="tp",k=70)+ti(pheo,bs="tp",k=20)+ti(tn,bs="tp",k=8)+ti(do_per,bs="tp",k=15),data=data,family=gaussian(link="log"))
gam.check(gamP) ## warnings

gamP<-gam(chl~ti(doy,bs="cc",k=20)+ti(date_dec,bs="tp",k=70)+ti(pheo,bs="tp",k=20)+ti(tn,bs="tp",k=8)+ti(do_per,bs="tp",k=10),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=20)+ti(date_dec,bs="tp",k=70)+ti(pheo,bs="tp",k=20)+ti(tn,bs="tp",k=8)+ti(do_per,bs="tp",k=15),data=data,family=gaussian(link="log"))
gam.check(gamP) ## warnings

gamP<-gam(chl~ti(doy,bs="cc",k=20)+ti(date_dec,bs="tp",k=70)+ti(pheo,bs="tp",k=20)+ti(tn,bs="tp",k=8)+ti(do_per,bs="tp",k=12),data=data,family=gaussian(link="log"))
gam.check(gamP) ## warnings

gamP<-gam(chl~ti(doy,bs="cc",k=20)+ti(date_dec,bs="tp",k=70)+ti(pheo,bs="tp",k=20)+ti(tn,bs="tp",k=8)+ti(do_per,bs="tp",k=11),data=data,family=gaussian(link="log"))
gam.check(gamP)  ## warnings, stuck in this variable

gamP<-gam(chl~ti(doy,bs="cc",k=25)+ti(date_dec,bs="tp",k=70)+ti(pheo,bs="tp",k=20)+ti(tn,bs="tp",k=8)+ti(do_per,bs="tp",k=10),data=data,family=gaussian(link="log"))
gam.check(gamP) ## wow, keep needing bigger k, already super slow

gamP<-gam(chl~ti(doy,bs="cc",k=30)+ti(date_dec,bs="tp",k=80)+ti(pheo,bs="tp",k=20)+ti(tn,bs="tp",k=8)+ti(do_per,bs="tp",k=10),data=data,family=gaussian(link="log"))
gam.check(gamP) 

gamP<-gam(chl~ti(doy,bs="cc",k=30)+ti(date_dec,bs="tp",k=100)+ti(pheo,bs="tp",k=20)+ti(tn,bs="tp",k=8)+ti(do_per,bs="tp",k=10),data=data,family=gaussian(link="log"))
gam.check(gamP) 

## ok something is going on here, come back
## let's try this again
data=perStation[[17]]
gamP<-gam(chl~ti(doy,bs="cc")+ti(date_dec,bs="tp")+ti(pheo,bs="tp")+ti(tn,bs="tp")+ti(do_per,bs="tp"),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc")+ti(date_dec,bs="tp",k=10)+ti(pheo,bs="tp")+ti(tn,bs="tp")+ti(do_per,bs="tp"),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc")+ti(date_dec,bs="tp",k=10)+ti(pheo,bs="tp")+ti(tn,bs="tp",k=10)+ti(do_per,bs="tp"),data=data,family=gaussian(link="log"))
gam.check(gamP)## warning

gamP<-gam(chl~ti(doy,bs="cc")+ti(date_dec,bs="tp",k=10)+ti(pheo,bs="tp")+ti(tn,bs="tp",k=8)+ti(do_per,bs="tp"),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc")+ti(date_dec,bs="tp",k=10)+ti(pheo,bs="tp")+ti(tn,bs="tp",k=8)+ti(do_per,bs="tp",k=10),data=data,family=gaussian(link="log"))
gam.check(gamP) ## Now we are getting somewhere

gamP<-gam(chl~ti(doy,bs="cc")+ti(date_dec,bs="tp",k=10)+ti(pheo,bs="tp")+ti(tn,bs="tp",k=8)+ti(do_per,bs="tp",k=15),data=data,family=gaussian(link="log"))
gam.check(gamP) ## warning

gamP<-gam(chl~ti(doy,bs="cc")+ti(date_dec,bs="tp",k=10)+ti(pheo,bs="tp")+ti(tn,bs="tp",k=8)+ti(do_per,bs="tp",k=12),data=data,family=gaussian(link="log"))
gam.check(gamP) ## warning

gamP<-gam(chl~ti(doy,bs="cc")+ti(date_dec,bs="tp",k=10)+ti(pheo,bs="tp")+ti(tn,bs="tp",k=8)+ti(do_per,bs="tp",k=11),data=data,family=gaussian(link="log"))
gam.check(gamP) ## warning, stuck at this value for parameter

gamP<-gam(chl~ti(doy,bs="cc")+ti(date_dec,bs="tp",k=10)+ti(pheo,bs="tp",k=10)+ti(tn,bs="tp",k=8)+ti(do_per,bs="tp",k=10),data=data,family=gaussian(link="log"))
gam.check(gamP)  ## getting slow now

gamP<-gam(chl~ti(doy,bs="cc",k=10)+ti(date_dec,bs="tp",k=15)+ti(pheo,bs="tp",k=10)+ti(tn,bs="tp",k=8)+ti(do_per,bs="tp",k=10),data=data,family=gaussian(link="log"))
gam.check(gamP) 

gamP<-gam(chl~ti(doy,bs="cc",k=15)+ti(date_dec,bs="tp",k=15)+ti(pheo,bs="tp",k=10)+ti(tn,bs="tp",k=8)+ti(do_per,bs="tp",k=10),data=data,family=gaussian(link="log"))
gam.check(gamP) 

gamP<-gam(chl~ti(doy,bs="cc",k=15)+ti(date_dec,bs="tp",k=15)+ti(pheo,bs="tp",k=15)+ti(tn,bs="tp",k=8)+ti(do_per,bs="tp",k=10),data=data,family=gaussian(link="log"))
gam.check(gamP) 

gamP<-gam(chl~ti(doy,bs="cc",k=15)+ti(date_dec,bs="tp",k=15)+ti(pheo,bs="tp",k=20)+ti(tn,bs="tp",k=8)+ti(do_per,bs="tp",k=10),data=data,family=gaussian(link="log"))
gam.check(gamP) 

gamP<-gam(chl~ti(doy,bs="cc",k=15)+ti(date_dec,bs="tp",k=20)+ti(pheo,bs="tp",k=20)+ti(tn,bs="tp",k=8)+ti(do_per,bs="tp",k=10),data=data,family=gaussian(link="log"))
gam.check(gamP) 

gamP<-gam(chl~ti(doy,bs="cc",k=15)+ti(date_dec,bs="tp",k=20)+ti(pheo,bs="tp",k=25)+ti(tn,bs="tp",k=8)+ti(do_per,bs="tp",k=10),data=data,family=gaussian(link="log"))
gam.check(gamP) ## warning

gamP<-gam(chl~ti(doy,bs="cc",k=15)+ti(date_dec,bs="tp",k=20)+ti(pheo,bs="tp",k=23)+ti(tn,bs="tp",k=8)+ti(do_per,bs="tp",k=10),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=15)+ti(date_dec,bs="tp",k=20)+ti(pheo,bs="tp",k=24)+ti(tn,bs="tp",k=8)+ti(do_per,bs="tp",k=10),data=data,family=gaussian(link="log"))
gam.check(gamP) ## getting slow

gamP<-gam(chl~ti(doy,bs="cc",k=15)+ti(date_dec,bs="tp",k=25)+ti(pheo,bs="tp",k=24)+ti(tn,bs="tp",k=8)+ti(do_per,bs="tp",k=10),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=20)+ti(date_dec,bs="tp",k=25)+ti(pheo,bs="tp",k=24)+ti(tn,bs="tp",k=8)+ti(do_per,bs="tp",k=10),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=20)+ti(date_dec,bs="tp",k=30)+ti(pheo,bs="tp",k=24)+ti(tn,bs="tp",k=8)+ti(do_per,bs="tp",k=10),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=20)+ti(date_dec,bs="tp",k=35)+ti(pheo,bs="tp",k=24)+ti(tn,bs="tp",k=8)+ti(do_per,bs="tp",k=10),data=data,family=gaussian(link="log"))
gam.check(gamP) ## go with this

perStationParsMod[[17]]=gamP
####

data=perStation[[18]]
gamP<-gam(chl~ti(doy,bs="cc")+ti(date_dec,bs="tp")+ti(pheo,bs="tp")+ti(tn,bs="tp")+ti(do_per,bs="tp"),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc")+ti(date_dec,bs="tp",k=10)+ti(pheo,bs="tp")+ti(tn,bs="tp")+ti(do_per,bs="tp"),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc")+ti(date_dec,bs="tp",k=10)+ti(pheo,bs="tp")+ti(tn,bs="tp")+ti(do_per,bs="tp",k=10),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=10)+ti(date_dec,bs="tp",k=10)+ti(pheo,bs="tp",k=10)+ti(tn,bs="tp",k=10)+ti(do_per,bs="tp",k=10),data=data,family=gaussian(link="log"))
gam.check(gamP) #warning


gamP<-gam(chl~ti(doy,bs="cc",k=10)+ti(date_dec,bs="tp",k=10)+ti(pheo,bs="tp")+ti(tn,bs="tp")+ti(do_per,bs="tp",k=10),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=10)+ti(date_dec,bs="tp",k=10)+ti(pheo,bs="tp",k=10)+ti(tn,bs="tp")+ti(do_per,bs="tp",k=10),data=data,family=gaussian(link="log"))
gam.check(gamP) ## warning

gamP<-gam(chl~ti(doy,bs="cc",k=10)+ti(date_dec,bs="tp",k=10)+ti(pheo,bs="tp",k=8)+ti(tn,bs="tp")+ti(do_per,bs="tp",k=10),data=data,family=gaussian(link="log"))
gam.check(gamP) ## warning

gamP<-gam(chl~ti(doy,bs="cc",k=10)+ti(date_dec,bs="tp",k=10)+ti(pheo,bs="tp",k=7)+ti(tn,bs="tp")+ti(do_per,bs="tp",k=10),data=data,family=gaussian(link="log"))
gam.check(gamP) 

gamP<-gam(chl~ti(doy,bs="cc",k=10)+ti(date_dec,bs="tp",k=10)+ti(pheo,bs="tp",k=7)+ti(tn,bs="tp",k=10)+ti(do_per,bs="tp",k=10),data=data,family=gaussian(link="log"))
gam.check(gamP) 

gamP<-gam(chl~ti(doy,bs="cc",k=10)+ti(date_dec,bs="tp",k=10)+ti(pheo,bs="tp",k=7)+ti(tn,bs="tp",k=15)+ti(do_per,bs="tp",k=10),data=data,family=gaussian(link="log"))
gam.check(gamP)  ## best we are going to get

perStationParsMod[[18]]=gamP

####

data=perStation[[19]]
gamP<-gam(chl~ti(doy,bs="cc")+ti(date_dec,bs="tp")+ti(pheo,bs="tp")+ti(tn,bs="tp")+ti(do_per,bs="tp"),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=10)+ti(date_dec,bs="tp",k=10)+ti(pheo,bs="tp")+ti(tn,bs="tp",k=10)+ti(do_per,bs="tp"),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=10)+ti(date_dec,bs="tp",k=10)+ti(pheo,bs="tp",k=10)+ti(tn,bs="tp",k=15)+ti(do_per,bs="tp",k=10),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=10)+ti(date_dec,bs="tp",k=15)+ti(pheo,bs="tp",k=15)+ti(tn,bs="tp",k=15)+ti(do_per,bs="tp",k=15),data=data,family=gaussian(link="log"))
gam.check(gamP) ## warnings

gamP<-gam(chl~ti(doy,bs="cc",k=15)+ti(date_dec,bs="tp",k=10)+ti(pheo,bs="tp",k=10)+ti(tn,bs="tp",k=15)+ti(do_per,bs="tp",k=10),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=20)+ti(date_dec,bs="tp",k=10)+ti(pheo,bs="tp",k=10)+ti(tn,bs="tp",k=15)+ti(do_per,bs="tp",k=10),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=25)+ti(date_dec,bs="tp",k=10)+ti(pheo,bs="tp",k=10)+ti(tn,bs="tp",k=15)+ti(do_per,bs="tp",k=10),data=data,family=gaussian(link="log"))
gam.check(gamP)

perStationParsMod[[19]]=gamP

####

data=perStation[[20]]
gamP<-gam(chl~ti(doy,bs="cc")+ti(date_dec,bs="tp")+ti(pheo,bs="tp")+ti(tn,bs="tp")+ti(do_per,bs="tp"),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=10)+ti(date_dec,bs="tp",k=10)+ti(pheo,bs="tp",k=10)+ti(tn,bs="tp")+ti(do_per,bs="tp",k=10),data=data,family=gaussian(link="log"))
gam.check(gamP) ## warning

gamP<-gam(chl~ti(doy,bs="cc",k=10)+ti(date_dec,bs="tp")+ti(pheo,bs="tp")+ti(tn,bs="tp")+ti(do_per,bs="tp"),data=data,family=gaussian(link="log"))
gam.check(gamP)


gamP<-gam(chl~ti(doy,bs="cc",k=10)+ti(date_dec,bs="tp")+ti(pheo,bs="tp")+ti(tn,bs="tp")+ti(do_per,bs="tp",k=10),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=10)+ti(date_dec,bs="tp")+ti(pheo,bs="tp")+ti(tn,bs="tp")+ti(do_per,bs="tp",k=15),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=10)+ti(date_dec,bs="tp",k=10)+ti(pheo,bs="tp")+ti(tn,bs="tp")+ti(do_per,bs="tp",k=15),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=10)+ti(date_dec,bs="tp",k=20)+ti(pheo,bs="tp")+ti(tn,bs="tp")+ti(do_per,bs="tp",k=15),data=data,family=gaussian(link="log"))
gam.check(gamP)


gamP<-gam(chl~ti(doy,bs="cc",k=10)+ti(date_dec,bs="tp",k=20)+ti(pheo,bs="tp",k=10)+ti(tn,bs="tp")+ti(do_per,bs="tp",k=15),data=data,family=gaussian(link="log"))
gam.check(gamP) ## warning

gamP<-gam(chl~ti(doy,bs="cc",k=10)+ti(date_dec,bs="tp",k=20)+ti(pheo,bs="tp",k=8)+ti(tn,bs="tp")+ti(do_per,bs="tp",k=15),data=data,family=gaussian(link="log"))
gam.check(gamP) ## qq plot looks weird, but everything else seems fine

perStationParsMod[[20]]=gamP
 
####

data=perStation[[21]]
gamP<-gam(chl~ti(doy,bs="cc")+ti(date_dec,bs="tp")+ti(pheo,bs="tp")+ti(tn,bs="tp")+ti(do_per,bs="tp"),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc")+ti(date_dec,bs="tp",k=10)+ti(pheo,bs="tp")+ti(tn,bs="tp")+ti(do_per,bs="tp"),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc")+ti(date_dec,bs="tp",k=15)+ti(pheo,bs="tp")+ti(tn,bs="tp")+ti(do_per,bs="tp"),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc")+ti(date_dec,bs="tp",k=20)+ti(pheo,bs="tp")+ti(tn,bs="tp")+ti(do_per,bs="tp"),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc")+ti(date_dec,bs="tp",k=30)+ti(pheo,bs="tp")+ti(tn,bs="tp")+ti(do_per,bs="tp"),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc")+ti(date_dec,bs="tp",k=30)+ti(pheo,bs="tp",k=10)+ti(tn,bs="tp")+ti(do_per,bs="tp"),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc")+ti(date_dec,bs="tp",k=30)+ti(pheo,bs="tp",k=15)+ti(tn,bs="tp")+ti(do_per,bs="tp"),data=data,family=gaussian(link="log"))
gam.check(gamP) ## warning

gamP<-gam(chl~ti(doy,bs="cc")+ti(date_dec,bs="tp",k=30)+ti(pheo,bs="tp",k=12)+ti(tn,bs="tp")+ti(do_per,bs="tp"),data=data,family=gaussian(link="log"))
gam.check(gamP) ## fine

gamP<-gam(chl~ti(doy,bs="cc")+ti(date_dec,bs="tp",k=30)+ti(pheo,bs="tp",k=13)+ti(tn,bs="tp")+ti(do_per,bs="tp"),data=data,family=gaussian(link="log"))
gam.check(gamP) ## warning

gamP<-gam(chl~ti(doy,bs="cc",k=10)+ti(date_dec,bs="tp",k=30)+ti(pheo,bs="tp",k=12)+ti(tn,bs="tp")+ti(do_per,bs="tp"),data=data,family=gaussian(link="log"))
gam.check(gamP) 

gamP<-gam(chl~ti(doy,bs="cc",k=15)+ti(date_dec,bs="tp",k=30)+ti(pheo,bs="tp",k=12)+ti(tn,bs="tp")+ti(do_per,bs="tp"),data=data,family=gaussian(link="log"))
gam.check(gamP) 

gamP<-gam(chl~ti(doy,bs="cc",k=20)+ti(date_dec,bs="tp",k=30)+ti(pheo,bs="tp",k=12)+ti(tn,bs="tp")+ti(do_per,bs="tp"),data=data,family=gaussian(link="log"))
gam.check(gamP) 

gamP<-gam(chl~ti(doy,bs="cc",k=20)+ti(date_dec,bs="tp",k=40)+ti(pheo,bs="tp",k=12)+ti(tn,bs="tp")+ti(do_per,bs="tp"),data=data,family=gaussian(link="log"))
gam.check(gamP) ## seems ok with date_dec but low p-value?

gamP<-gam(chl~ti(doy,bs="cc",k=20)+ti(date_dec,bs="tp",k=50)+ti(pheo,bs="tp",k=12)+ti(tn,bs="tp")+ti(do_per,bs="tp"),data=data,family=gaussian(link="log"))
gam.check(gamP)

## check adding on the lower end before going above k=50
gamP<-gam(chl~ti(doy,bs="cc",k=20)+ti(date_dec,bs="tp",k=50)+ti(pheo,bs="tp",k=12)+ti(tn,bs="tp",k=10)+ti(do_per,bs="tp"),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=20)+ti(date_dec,bs="tp",k=50)+ti(pheo,bs="tp",k=12)+ti(tn,bs="tp",k=10)+ti(do_per,bs="tp",k=10),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=20)+ti(date_dec,bs="tp",k=50)+ti(pheo,bs="tp",k=12)+ti(tn,bs="tp",k=10)+ti(do_per,bs="tp",k=15),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=20)+ti(date_dec,bs="tp",k=50)+ti(pheo,bs="tp",k=12)+ti(tn,bs="tp",k=10)+ti(do_per,bs="tp",k=20),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=20)+ti(date_dec,bs="tp",k=50)+ti(pheo,bs="tp",k=12)+ti(tn,bs="tp",k=10)+ti(do_per,bs="tp",k=25),data=data,family=gaussian(link="log"))
gam.check(gamP) ## warning

gamP<-gam(chl~ti(doy,bs="cc",k=20)+ti(date_dec,bs="tp",k=50)+ti(pheo,bs="tp",k=12)+ti(tn,bs="tp",k=10)+ti(do_per,bs="tp",k=23),data=data,family=gaussian(link="log"))
gam.check(gamP)  ## warning

gamP<-gam(chl~ti(doy,bs="cc",k=20)+ti(date_dec,bs="tp",k=50)+ti(pheo,bs="tp",k=12)+ti(tn,bs="tp",k=10)+ti(do_per,bs="tp",k=22),data=data,family=gaussian(link="log"))
gam.check(gamP)  ## good, but resids v linear pred look bad

gamP<-gam(chl~ti(doy,bs="cc",k=20)+ti(date_dec,bs="tp",k=60)+ti(pheo,bs="tp",k=12)+ti(tn,bs="tp",k=10)+ti(do_per,bs="tp",k=22),data=data,family=gaussian(link="log"))
gam.check(gamP)  ## warning plus getting slow

gamP<-gam(chl~ti(doy,bs="cc",k=20)+ti(date_dec,bs="tp",k=55)+ti(pheo,bs="tp",k=12)+ti(tn,bs="tp",k=10)+ti(do_per,bs="tp",k=22),data=data,family=gaussian(link="log"))
gam.check(gamP) ## resids v linear pred look bad

gamP<-gam(chl~ti(doy,bs="cc",k=20)+ti(date_dec,bs="tp",k=50)+ti(pheo,bs="tp",k=12)+ti(tn,bs="tp",k=10)+ti(do_per,bs="tp",k=15),data=data,family=gaussian(link="log"))
gam.check(gamP) ## going back to this because I'm worried about that weird plot 

perStationParsMod[[21]]=gamP

####

data=perStation[[22]]
gamP<-gam(chl~ti(doy,bs="cc")+ti(date_dec,bs="tp")+ti(pheo,bs="tp")+ti(tn,bs="tp")+ti(do_per,bs="tp"),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=10)+ti(date_dec,bs="tp",k=10)+ti(pheo,bs="tp",k=10)+ti(tn,bs="tp",k=10)+ti(do_per,bs="tp",k=10),data=data,family=gaussian(link="log"))
gam.check(gamP) ## warning

gamP<-gam(chl~ti(doy,bs="cc")+ti(date_dec,bs="tp")+ti(pheo,bs="tp")+ti(tn,bs="tp",k=10)+ti(do_per,bs="tp"),data=data,family=gaussian(link="log"))
gam.check(gamP) ## warning

gamP<-gam(chl~ti(doy,bs="cc")+ti(date_dec,bs="tp")+ti(pheo,bs="tp")+ti(tn,bs="tp",k=8)+ti(do_per,bs="tp"),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc")+ti(date_dec,bs="tp",k=10)+ti(pheo,bs="tp")+ti(tn,bs="tp",k=8)+ti(do_per,bs="tp"),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc")+ti(date_dec,bs="tp",k=15)+ti(pheo,bs="tp")+ti(tn,bs="tp",k=8)+ti(do_per,bs="tp"),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc")+ti(date_dec,bs="tp",k=20)+ti(pheo,bs="tp")+ti(tn,bs="tp",k=8)+ti(do_per,bs="tp"),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc")+ti(date_dec,bs="tp",k=20)+ti(pheo,bs="tp")+ti(tn,bs="tp",k=8)+ti(do_per,bs="tp"),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc")+ti(date_dec,bs="tp",k=20)+ti(pheo,bs="tp",k=10)+ti(tn,bs="tp",k=8)+ti(do_per,bs="tp"),data=data,family=gaussian(link="log"))
gam.check(gamP)

plot(gamP$linear.pred,gamP$residuals,xlim=c(0,5)) ## This looks ok

gamP<-gam(chl~ti(doy,bs="cc")+ti(date_dec,bs="tp",k=25)+ti(pheo,bs="tp",k=10)+ti(tn,bs="tp",k=8)+ti(do_per,bs="tp"),data=data,family=gaussian(link="log"))
gam.check(gamP) ## warning

gamP<-gam(chl~ti(doy,bs="cc")+ti(date_dec,bs="tp",k=23)+ti(pheo,bs="tp",k=10)+ti(tn,bs="tp",k=8)+ti(do_per,bs="tp"),data=data,family=gaussian(link="log"))
gam.check(gamP) ## warning

gamP<-gam(chl~ti(doy,bs="cc")+ti(date_dec,bs="tp",k=22)+ti(pheo,bs="tp",k=10)+ti(tn,bs="tp",k=8)+ti(do_per,bs="tp"),data=data,family=gaussian(link="log"))
gam.check(gamP)  ## go with this
plot(gamP$linear.pred,gamP$residuals,xlim=c(0,5)) ## This looks ok

perStationParsMod[[22]]=gamP

####

data=perStation[[23]]
gamP<-gam(chl~ti(doy,bs="cc")+ti(date_dec,bs="tp")+ti(pheo,bs="tp")+ti(tn,bs="tp")+ti(do_per,bs="tp"),data=data,family=gaussian(link="log"))
gam.check(gamP)
plot(gamP$linear.pred,gamP$residuals,xlim=c(0,5)) ## This looks ok

gamP<-gam(chl~ti(doy,bs="cc")+ti(date_dec,bs="tp",k=10)+ti(pheo,bs="tp")+ti(tn,bs="tp")+ti(do_per,bs="tp"),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc")+ti(date_dec,bs="tp",k=15)+ti(pheo,bs="tp")+ti(tn,bs="tp")+ti(do_per,bs="tp"),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc")+ti(date_dec,bs="tp",k=20)+ti(pheo,bs="tp")+ti(tn,bs="tp")+ti(do_per,bs="tp"),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc")+ti(date_dec,bs="tp",k=30)+ti(pheo,bs="tp")+ti(tn,bs="tp")+ti(do_per,bs="tp"),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc")+ti(date_dec,bs="tp",k=30)+ti(pheo,bs="tp")+ti(tn,bs="tp")+ti(do_per,bs="tp",k=10),data=data,family=gaussian(link="log"))
gam.check(gamP) ## warning

gamP<-gam(chl~ti(doy,bs="cc")+ti(date_dec,bs="tp",k=30)+ti(pheo,bs="tp")+ti(tn,bs="tp")+ti(do_per,bs="tp",k=8),data=data,family=gaussian(link="log"))
gam.check(gamP)  ##fine

gamP<-gam(chl~ti(doy,bs="cc")+ti(date_dec,bs="tp",k=30)+ti(pheo,bs="tp")+ti(tn,bs="tp")+ti(do_per,bs="tp",k=9),data=data,family=gaussian(link="log"))
gam.check(gamP)  ## warning

gamP<-gam(chl~ti(doy,bs="cc")+ti(date_dec,bs="tp",k=30)+ti(pheo,bs="tp",k=10)+ti(tn,bs="tp")+ti(do_per,bs="tp",k=8),data=data,family=gaussian(link="log"))
gam.check(gamP)

perStationParsMod[[23]]=gamP

####
data=perStation[[24]]
gamP<-gam(chl~ti(doy,bs="cc")+ti(date_dec,bs="tp")+ti(pheo,bs="tp")+ti(tn,bs="tp")+ti(do_per,bs="tp"),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc")+ti(date_dec,bs="tp",k=10)+ti(pheo,bs="tp",k=10)+ti(tn,bs="tp")+ti(do_per,bs="tp"),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc")+ti(date_dec,bs="tp",k=15)+ti(pheo,bs="tp",k=15)+ti(tn,bs="tp")+ti(do_per,bs="tp"),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc")+ti(date_dec,bs="tp",k=20)+ti(pheo,bs="tp",k=20)+ti(tn,bs="tp")+ti(do_per,bs="tp"),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc")+ti(date_dec,bs="tp",k=25)+ti(pheo,bs="tp",k=25)+ti(tn,bs="tp")+ti(do_per,bs="tp"),data=data,family=gaussian(link="log"))
gam.check(gamP) ## warning

gamP<-gam(chl~ti(doy,bs="cc")+ti(date_dec,bs="tp",k=20)+ti(pheo,bs="tp",k=25)+ti(tn,bs="tp")+ti(do_per,bs="tp"),data=data,family=gaussian(link="log"))
gam.check(gamP) ## warning


gamP<-gam(chl~ti(doy,bs="cc")+ti(date_dec,bs="tp",k=25)+ti(pheo,bs="tp",k=20)+ti(tn,bs="tp")+ti(do_per,bs="tp"),data=data,family=gaussian(link="log"))
gam.check(gamP) ## slow now

gamP<-gam(chl~ti(doy,bs="cc")+ti(date_dec,bs="tp",k=25)+ti(pheo,bs="tp",k=20)+ti(tn,bs="tp")+ti(do_per,bs="tp",k=10),data=data,family=gaussian(link="log"))
gam.check(gamP) ## slow now

gamP<-gam(chl~ti(doy,bs="cc")+ti(date_dec,bs="tp",k=25)+ti(pheo,bs="tp",k=20)+ti(tn,bs="tp",k=10)+ti(do_per,bs="tp",k=10),data=data,family=gaussian(link="log"))
gam.check(gamP) ## warning

gamP<-gam(chl~ti(doy,bs="cc")+ti(date_dec,bs="tp",k=25)+ti(pheo,bs="tp",k=20)+ti(tn,bs="tp",k=8)+ti(do_per,bs="tp",k=10),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc")+ti(date_dec,bs="tp",k=25)+ti(pheo,bs="tp",k=25)+ti(tn,bs="tp",k=8)+ti(do_per,bs="tp",k=10),data=data,family=gaussian(link="log"))
gam.check(gamP) ##warning

gamP<-gam(chl~ti(doy,bs="cc")+ti(date_dec,bs="tp",k=25)+ti(pheo,bs="tp",k=23)+ti(tn,bs="tp",k=8)+ti(do_per,bs="tp",k=10),data=data,family=gaussian(link="log"))
gam.check(gamP)  ## warning

gamP<-gam(chl~ti(doy,bs="cc")+ti(date_dec,bs="tp",k=25)+ti(pheo,bs="tp",k=22)+ti(tn,bs="tp",k=8)+ti(do_per,bs="tp",k=10),data=data,family=gaussian(link="log"))
gam.check(gamP)  ## warning

gamP<-gam(chl~ti(doy,bs="cc")+ti(date_dec,bs="tp",k=25)+ti(pheo,bs="tp",k=21)+ti(tn,bs="tp",k=8)+ti(do_per,bs="tp",k=10),data=data,family=gaussian(link="log"))
gam.check(gamP) ## warning

gamP<-gam(chl~ti(doy,bs="cc")+ti(date_dec,bs="tp",k=25)+ti(pheo,bs="tp",k=20)+ti(tn,bs="tp",k=8)+ti(do_per,bs="tp",k=10),data=data,family=gaussian(link="log"))
gam.check(gamP) ## really slow


perStationParsMod[[24]]=gamP

####

# skip 25-8 for missing data, look into later

####

data=perStation[[29]]
gamP<-gam(chl~ti(doy,bs="cc")+ti(date_dec,bs="tp")+ti(pheo,bs="tp")+ti(tn,bs="tp")+ti(do_per,bs="tp"),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc")+ti(date_dec,bs="tp",k=10)+ti(pheo,bs="tp")+ti(tn,bs="tp")+ti(do_per,bs="tp"),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc")+ti(date_dec,bs="tp",k=15)+ti(pheo,bs="tp")+ti(tn,bs="tp")+ti(do_per,bs="tp"),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc")+ti(date_dec,bs="tp",k=20)+ti(pheo,bs="tp")+ti(tn,bs="tp")+ti(do_per,bs="tp"),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc")+ti(date_dec,bs="tp",k=25)+ti(pheo,bs="tp")+ti(tn,bs="tp")+ti(do_per,bs="tp"),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc")+ti(date_dec,bs="tp",k=25)+ti(pheo,bs="tp",k=10)+ti(tn,bs="tp")+ti(do_per,bs="tp"),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc")+ti(date_dec,bs="tp",k=25)+ti(pheo,bs="tp",k=15)+ti(tn,bs="tp")+ti(do_per,bs="tp"),data=data,family=gaussian(link="log"))
gam.check(gamP) ## warning

gamP<-gam(chl~ti(doy,bs="cc")+ti(date_dec,bs="tp",k=25)+ti(pheo,bs="tp",k=12)+ti(tn,bs="tp")+ti(do_per,bs="tp"),data=data,family=gaussian(link="log"))
gam.check(gamP) ## warning

gamP<-gam(chl~ti(doy,bs="cc")+ti(date_dec,bs="tp",k=25)+ti(pheo,bs="tp",k=10)+ti(tn,bs="tp")+ti(do_per,bs="tp",k=10),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc")+ti(date_dec,bs="tp",k=25)+ti(pheo,bs="tp",k=10)+ti(tn,bs="tp")+ti(do_per,bs="tp",k=15),data=data,family=gaussian(link="log"))
gam.check(gamP) ## warning

gamP<-gam(chl~ti(doy,bs="cc")+ti(date_dec,bs="tp",k=25)+ti(pheo,bs="tp",k=10)+ti(tn,bs="tp")+ti(do_per,bs="tp",k=12),data=data,family=gaussian(link="log"))
gam.check(gamP) ## warning

gamP<-gam(chl~ti(doy,bs="cc")+ti(date_dec,bs="tp",k=30)+ti(pheo,bs="tp",k=10)+ti(tn,bs="tp")+ti(do_per,bs="tp",k=10),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc")+ti(date_dec,bs="tp",k=40)+ti(pheo,bs="tp",k=10)+ti(tn,bs="tp")+ti(do_per,bs="tp",k=10),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=10)+ti(date_dec,bs="tp",k=40)+ti(pheo,bs="tp",k=10)+ti(tn,bs="tp")+ti(do_per,bs="tp",k=10),data=data,family=gaussian(link="log"))
gam.check(gamP)


gamP<-gam(chl~ti(doy,bs="cc",k=15)+ti(date_dec,bs="tp",k=40)+ti(pheo,bs="tp",k=10)+ti(tn,bs="tp")+ti(do_per,bs="tp",k=10),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=15)+ti(date_dec,bs="tp",k=50)+ti(pheo,bs="tp",k=10)+ti(tn,bs="tp")+ti(do_per,bs="tp",k=10),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=15)+ti(date_dec,bs="tp",k=60)+ti(pheo,bs="tp",k=10)+ti(tn,bs="tp")+ti(do_per,bs="tp",k=10),data=data,family=gaussian(link="log"))
gam.check(gamP) ## getting slow

perStationParsMod[[29]]=gamP

####

data=perStation[[30]]
gamP<-gam(chl~ti(doy,bs="cc")+ti(date_dec,bs="tp")+ti(pheo,bs="tp")+ti(tn,bs="tp")+ti(do_per,bs="tp"),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc")+ti(date_dec,bs="tp",k=10)+ti(pheo,bs="tp")+ti(tn,bs="tp")+ti(do_per,bs="tp"),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc")+ti(date_dec,bs="tp",k=15)+ti(pheo,bs="tp",k=10)+ti(tn,bs="tp")+ti(do_per,bs="tp"),data=data,family=gaussian(link="log"))
gam.check(gamP) ## warning

gamP<-gam(chl~ti(doy,bs="cc")+ti(date_dec,bs="tp",k=15)+ti(pheo,bs="tp")+ti(tn,bs="tp")+ti(do_per,bs="tp"),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc")+ti(date_dec,bs="tp",k=15)+ti(pheo,bs="tp",k=8)+ti(tn,bs="tp")+ti(do_per,bs="tp"),data=data,family=gaussian(link="log"))
gam.check(gamP) ## warning

gamP<-gam(chl~ti(doy,bs="cc")+ti(date_dec,bs="tp",k=15)+ti(pheo,bs="tp",k=7)+ti(tn,bs="tp")+ti(do_per,bs="tp"),data=data,family=gaussian(link="log"))
gam.check(gamP) ## warning

gamP<-gam(chl~ti(doy,bs="cc")+ti(date_dec,bs="tp",k=15)+ti(pheo,bs="tp",k=6)+ti(tn,bs="tp")+ti(do_per,bs="tp"),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=10)+ti(date_dec,bs="tp",k=15)+ti(pheo,bs="tp",k=6)+ti(tn,bs="tp")+ti(do_per,bs="tp"),data=data,family=gaussian(link="log"))
gam.check(gamP)

perStationParsMod[[30]]=gamP

####

data=perStation[[36]]
gamP<-gam(chl~ti(doy,bs="cc")+ti(date_dec,bs="tp")+ti(pheo,bs="tp")+ti(tn,bs="tp")+ti(do_per,bs="tp"),data=data,family=gaussian(link="log"))
## no data on tn, so remove that and continue

gamP<-gam(chl~ti(doy,bs="cc")+ti(date_dec,bs="tp")+ti(pheo,bs="tp")+ti(do_per,bs="tp"),data=data,family=gaussian(link="log"))
gam.check(gamP)


gamP<-gam(chl~ti(doy,bs="cc",k=10)+ti(date_dec,bs="tp")+ti(pheo,bs="tp")+ti(do_per,bs="tp"),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=15)+ti(date_dec,bs="tp")+ti(pheo,bs="tp")+ti(do_per,bs="tp"),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=15)+ti(date_dec,bs="tp",k=10)+ti(pheo,bs="tp")+ti(do_per,bs="tp"),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=15)+ti(date_dec,bs="tp",k=10)+ti(pheo,bs="tp")+ti(do_per,bs="tp",k=10),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=15)+ti(date_dec,bs="tp",k=10)+ti(pheo,bs="tp")+ti(do_per,bs="tp",k=15),data=data,family=gaussian(link="log"))
gam.check(gamP)
plot(gamP$linear.predictor,gamP$residuals,xlim=c(-1,1))

gamP<-gam(chl~ti(doy,bs="cc",k=20)+ti(date_dec,bs="tp",k=10)+ti(pheo,bs="tp")+ti(do_per,bs="tp",k=15),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=25)+ti(date_dec,bs="tp",k=10)+ti(pheo,bs="tp")+ti(do_per,bs="tp",k=15),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=30)+ti(date_dec,bs="tp",k=10)+ti(pheo,bs="tp")+ti(do_per,bs="tp",k=15),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=30)+ti(date_dec,bs="tp",k=10)+ti(pheo,bs="tp")+ti(do_per,bs="tp",k=20),data=data,family=gaussian(link="log"))
gam.check(gamP) ## warning

gamP<-gam(chl~ti(doy,bs="cc",k=30)+ti(date_dec,bs="tp",k=10)+ti(pheo,bs="tp")+ti(do_per,bs="tp",k=18),data=data,family=gaussian(link="log"))
gam.check(gamP) ## warning


gamP<-gam(chl~ti(doy,bs="cc",k=40)+ti(date_dec,bs="tp",k=10)+ti(pheo,bs="tp")+ti(do_per,bs="tp",k=15),data=data,family=gaussian(link="log"))
gam.check(gamP) 

gamP<-gam(chl~ti(doy,bs="cc",k=45)+ti(date_dec,bs="tp",k=10)+ti(pheo,bs="tp")+ti(do_per,bs="tp",k=15),data=data,family=gaussian(link="log"))
gam.check(gamP) 

gamP<-gam(chl~ti(doy,bs="cc",k=50)+ti(date_dec,bs="tp",k=10)+ti(pheo,bs="tp")+ti(do_per,bs="tp",k=15),data=data,family=gaussian(link="log"))
gam.check(gamP) 

toUse=na.omit(data[,c("chl","doy","date_dec","pheo","do_per")])
dim(toUse)
plot(gamP$fitted.values,toUse$chl)
abline(0,1)

perStationParsMod[[36]]=gamP

####
data=perStation[[37]]
gamP<-gam(chl~ti(doy,bs="cc")+ti(date_dec,bs="tp")+ti(pheo,bs="tp")+ti(tn,bs="tp")+ti(do_per,bs="tp"),data=data,family=gaussian(link="log"))
## warning

gamP<-gam(chl~ti(doy,bs="cc",k=10)+ti(date_dec,bs="tp",k=10)+ti(pheo,bs="tp",k=10)+ti(tn,bs="tp",k=10)+ti(do_per,bs="tp",k=10),data=data,family=gaussian(link="log"))
gam.check(gamP) ## this is fine

gamP<-gam(chl~ti(doy,bs="cc",k=15)+ti(date_dec,bs="tp",k=15)+ti(pheo,bs="tp",k=15)+ti(tn,bs="tp",k=15)+ti(do_per,bs="tp",k=15),data=data,family=gaussian(link="log"))
gam.check(gamP)  ## warning

gamP<-gam(chl~ti(doy,bs="cc",k=10)+ti(date_dec,bs="tp",k=10)+ti(pheo,bs="tp",k=15)+ti(tn,bs="tp",k=10)+ti(do_per,bs="tp",k=10),data=data,family=gaussian(link="log"))
gam.check(gamP) ## warning

gamP<-gam(chl~ti(doy,bs="cc",k=10)+ti(date_dec,bs="tp",k=10)+ti(pheo,bs="tp",k=12)+ti(tn,bs="tp",k=10)+ti(do_per,bs="tp",k=10),data=data,family=gaussian(link="log"))
gam.check(gamP)  ## warning

gamP<-gam(chl~ti(doy,bs="cc",k=10)+ti(date_dec,bs="tp",k=15)+ti(pheo,bs="tp",k=10)+ti(tn,bs="tp",k=10)+ti(do_per,bs="tp",k=10),data=data,family=gaussian(link="log"))
gam.check(gamP) 

gamP<-gam(chl~ti(doy,bs="cc",k=10)+ti(date_dec,bs="tp",k=20)+ti(pheo,bs="tp",k=10)+ti(tn,bs="tp",k=10)+ti(do_per,bs="tp",k=10),data=data,family=gaussian(link="log"))
gam.check(gamP) 

gamP<-gam(chl~ti(doy,bs="cc",k=10)+ti(date_dec,bs="tp",k=25)+ti(pheo,bs="tp",k=10)+ti(tn,bs="tp",k=10)+ti(do_per,bs="tp",k=10),data=data,family=gaussian(link="log"))
gam.check(gamP)

perStationParsMod[[37]]=gamP

####

data=perStation[[38]]
gamP<-gam(chl~ti(doy,bs="cc")+ti(date_dec,bs="tp")+ti(pheo,bs="tp")+ti(tn,bs="tp")+ti(do_per,bs="tp"),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=10)+ti(date_dec,bs="tp")+ti(pheo,bs="tp")+ti(tn,bs="tp")+ti(do_per,bs="tp"),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=15)+ti(date_dec,bs="tp")+ti(pheo,bs="tp")+ti(tn,bs="tp")+ti(do_per,bs="tp"),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=20)+ti(date_dec,bs="tp")+ti(pheo,bs="tp")+ti(tn,bs="tp")+ti(do_per,bs="tp"),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=25)+ti(date_dec,bs="tp")+ti(pheo,bs="tp")+ti(tn,bs="tp")+ti(do_per,bs="tp"),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=25)+ti(date_dec,bs="tp")+ti(pheo,bs="tp",k=10)+ti(tn,bs="tp")+ti(do_per,bs="tp"),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=25)+ti(date_dec,bs="tp")+ti(pheo,bs="tp",k=15)+ti(tn,bs="tp")+ti(do_per,bs="tp"),data=data,family=gaussian(link="log"))
gam.check(gamP) ## warning

gamP<-gam(chl~ti(doy,bs="cc",k=25)+ti(date_dec,bs="tp")+ti(pheo,bs="tp",k=12)+ti(tn,bs="tp")+ti(do_per,bs="tp"),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=25)+ti(date_dec,bs="tp")+ti(pheo,bs="tp",k=13)+ti(tn,bs="tp")+ti(do_per,bs="tp"),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=25)+ti(date_dec,bs="tp")+ti(pheo,bs="tp",k=13)+ti(tn,bs="tp")+ti(do_per,bs="tp",k=10),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=25)+ti(date_dec,bs="tp")+ti(pheo,bs="tp",k=13)+ti(tn,bs="tp")+ti(do_per,bs="tp",k=15),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=25)+ti(date_dec,bs="tp")+ti(pheo,bs="tp",k=13)+ti(tn,bs="tp")+ti(do_per,bs="tp",k=20),data=data,family=gaussian(link="log"))
gam.check(gamP) ## slow now

range(gamP$residuals)
#-35.29608  29.97070

gamP<-gam(chl~ti(doy,bs="cc",k=25)+ti(date_dec,bs="tp")+ti(pheo,bs="tp",k=13)+ti(tn,bs="tp")+ti(do_per,bs="tp",k=25),data=data,family=gaussian(link="log"))
gam.check(gamP)  ## warning

gamP<-gam(chl~ti(doy,bs="cc",k=25)+ti(date_dec,bs="tp")+ti(pheo,bs="tp",k=13)+ti(tn,bs="tp")+ti(do_per,bs="tp",k=23),data=data,family=gaussian(link="log"))
gam.check(gamP) ## warning

gamP<-gam(chl~ti(doy,bs="cc",k=25)+ti(date_dec,bs="tp")+ti(pheo,bs="tp",k=13)+ti(tn,bs="tp")+ti(do_per,bs="tp",k=22),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=25)+ti(date_dec,bs="tp",k=10)+ti(pheo,bs="tp",k=13)+ti(tn,bs="tp")+ti(do_per,bs="tp",k=22),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=25)+ti(date_dec,bs="tp",k=15)+ti(pheo,bs="tp",k=13)+ti(tn,bs="tp")+ti(do_per,bs="tp",k=22),data=data,family=gaussian(link="log"))
gam.check(gamP)

range(gamP$residuals)
#-35.29608  29.97070

gamP<-gam(chl~ti(doy,bs="cc",k=25)+ti(date_dec,bs="tp",k=15)+ti(pheo,bs="tp",k=13)+ti(tn,bs="tp",k=10)+ti(do_per,bs="tp",k=22),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=25)+ti(date_dec,bs="tp",k=15)+ti(pheo,bs="tp",k=13)+ti(tn,bs="tp",k=15)+ti(do_per,bs="tp",k=22),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=30)+ti(date_dec,bs="tp",k=15)+ti(pheo,bs="tp",k=13)+ti(tn,bs="tp",k=15)+ti(do_per,bs="tp",k=22),data=data,family=gaussian(link="log"))
gam.check(gamP) ## still pretty slow

perStationParsMod[[38]]=gamP

####

data=perStation[[39]] ## not a lot of data here, let's skip

####

data=perStation[[40]]
gamP<-gam(chl~ti(doy,bs="cc")+ti(date_dec,bs="tp")+ti(pheo,bs="tp")+ti(tn,bs="tp")+ti(do_per,bs="tp"),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc")+ti(date_dec,bs="tp")+ti(pheo,bs="tp",k=10)+ti(tn,bs="tp")+ti(do_per,bs="tp"),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc")+ti(date_dec,bs="tp")+ti(pheo,bs="tp",k=15)+ti(tn,bs="tp")+ti(do_per,bs="tp"),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc")+ti(date_dec,bs="tp",k=10)+ti(pheo,bs="tp",k=15)+ti(tn,bs="tp")+ti(do_per,bs="tp",k=10),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=10)+ti(date_dec,bs="tp",k=10)+ti(pheo,bs="tp",k=15)+ti(tn,bs="tp")+ti(do_per,bs="tp",k=10),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=10)+ti(date_dec,bs="tp",k=15)+ti(pheo,bs="tp",k=15)+ti(tn,bs="tp")+ti(do_per,bs="tp",k=10),data=data,family=gaussian(link="log"))
gam.check(gamP)

perStationParsMod[[40]]=gamP


####

data=perStation[[41]]
gamP<-gam(chl~ti(doy,bs="cc")+ti(date_dec,bs="tp")+ti(pheo,bs="tp")+ti(tn,bs="tp")+ti(do_per,bs="tp"),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=10)+ti(date_dec,bs="tp")+ti(pheo,bs="tp")+ti(tn,bs="tp")+ti(do_per,bs="tp"),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=10)+ti(date_dec,bs="tp",k=10)+ti(pheo,bs="tp")+ti(tn,bs="tp")+ti(do_per,bs="tp"),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=10)+ti(date_dec,bs="tp",k=10)+ti(pheo,bs="tp")+ti(tn,bs="tp",k=10)+ti(do_per,bs="tp",k=10),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=15)+ti(date_dec,bs="tp",k=10)+ti(pheo,bs="tp")+ti(tn,bs="tp",k=10)+ti(do_per,bs="tp",k=10),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=15)+ti(date_dec,bs="tp",k=10)+ti(pheo,bs="tp",k=10)+ti(tn,bs="tp",k=10)+ti(do_per,bs="tp",k=10),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=15)+ti(date_dec,bs="tp",k=10)+ti(pheo,bs="tp",k=15)+ti(tn,bs="tp",k=10)+ti(do_per,bs="tp",k=10),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=15)+ti(date_dec,bs="tp",k=15)+ti(pheo,bs="tp",k=15)+ti(tn,bs="tp",k=10)+ti(do_per,bs="tp",k=10),data=data,family=gaussian(link="log"))
gam.check(gamP)

perStationParsMod[[41]]=gamP

filled<-c()
for(i in 1:41){
  filled<-c(filled,is.null(perStationParsMod[[i]]))
}
sum(filled) ## 12, 17, 25:28, 31:35, 39

names(perStationParsMod)=names(perStation)
save(perStationParsMod,file="perStationParsimoniousModels.Rda")

rowL<-c()
for(i in 1:41){
  rowL<-c(rowL,nrow(perStation[[i]]))
}
rowL
rangeY<-c()
for(i in 1:41){
  rangeY<-rbind(rangeY,range(perStation[[i]]$date))
}
rangeY

## 1, 2, 5, 7, 19, 11, 13, 15, 16, 17, 18(), 21, 22, 23, 29, 40

wholeSeries<-c(1, 2, 5, 7, 9, 11, 13, 15, 16, 17, 18, 21, 22, 23, 29, 40)

plot(sfei$Longitude,sfei$Latitude)
for(i in wholeSeries){
points(perStation[[i]]$Longitude,perStation[[i]]$Latitude,col="red",pch=19)
} ## good spatial range, so I'm just going to use these

## double check all of the models


gam.check(perStationParsMod[[1]]) ## revisit (fixed)
gam.check(perStationParsMod[[2]]) ## revisit (fixed)
gam.check(perStationParsMod[[5]])
gam.check(perStationParsMod[[7]])
gam.check(perStationParsMod[[9]])
gam.check(perStationParsMod[[11]])
gam.check(perStationParsMod[[13]])
gam.check(perStationParsMod[[15]])
gam.check(perStationParsMod[[16]])
gam.check(perStationParsMod[[17]])
gam.check(perStationParsMod[[18]])
gam.check(perStationParsMod[[21]])
gam.check(perStationParsMod[[22]])
gam.check(perStationParsMod[[23]])
gam.check(perStationParsMod[[29]])
gam.check(perStationParsMod[[40]])












