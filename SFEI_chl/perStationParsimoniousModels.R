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