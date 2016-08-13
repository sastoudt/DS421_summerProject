setwd("~/Desktop/sfei")
load(file="perStation.Rda")
require(mgcv)

wholeSeries<-c(1, 2, 5, 7, 11, 13, 15, 16, 17, 18, 21, 22, 23, 29, 40)
perStationMod3Compare=vector("list",length(perStation))


data=perStation[[1]]

gamP<-gam(chl~ti(doy,bs="cc")+ti(date_dec,bs="tp"),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=20)+ti(date_dec,bs="tp",k=20),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=20)+ti(date_dec,bs="tp",k=30),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=20)+ti(date_dec,bs="tp",k=50),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=20)+ti(date_dec,bs="tp",k=70),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=20)+ti(date_dec,bs="tp",k=100),data=data,family=gaussian(link="log"))
gam.check(gamP) ## warning

gamP<-gam(chl~ti(doy,bs="cc",k=20)+ti(date_dec,bs="tp",k=80),data=data,family=gaussian(link="log"))
gam.check(gamP) ## seems good

gamP<-gam(chl~ti(doy,bs="cc",k=20)+ti(date_dec,bs="tp",k=90),data=data,family=gaussian(link="log"))
gam.check(gamP) ## worse

gamP<-gam(chl~ti(doy,bs="cc",k=20)+ti(date_dec,bs="tp",k=80),data=data,family=gaussian(link="log"))
gam.check(gamP) ## go with this

perStationMod3Compare[[1]]=gamP

