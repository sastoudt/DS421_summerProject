### using chl from another station as "flow" variable

setwd("~/Desktop/sfei")
load(file="perStationAdd.Rda")
require(mgcv)

perStationFlowMod=vector("list",length(perStationAdd))
wholeSeries<-c(1, 2, 5, 7, 11, 13, 15, 16, 17, 18, 21, 22, 23, 29, 40)
ctrl <- list(nthreads=4)
data=perStationAdd[[1]]

gamP<-gam(chl~ti(doy,bs="cc")+ti(date_dec,bs="tp")+ti(i.chl,bs="tp"),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=10)+ti(date_dec,bs="tp",k=10)+ti(i.chl,bs="tp",k=10),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=10)+ti(date_dec,bs="tp",k=20)+ti(i.chl,bs="tp",k=10),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=15)+ti(date_dec,bs="tp",k=30)+ti(i.chl,bs="tp",k=15),data=data,family=gaussian(link="log"))
gam.check(gamP) ## warning

gamP<-gam(chl~ti(doy,bs="cc",k=15)+ti(date_dec,bs="tp",k=20)+ti(i.chl,bs="tp",k=10),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=15)+ti(date_dec,bs="tp",k=20)+ti(i.chl,bs="tp",k=15),data=data,family=gaussian(link="log"))
gam.check(gamP) ## warning

gamP<-gam(chl~ti(doy,bs="cc",k=15)+ti(date_dec,bs="tp",k=20)+ti(i.chl,bs="tp",k=12),data=data,family=gaussian(link="log"))
gam.check(gamP) 

gamP<-bam(chl~ti(doy,bs="cc",k=15)+ti(date_dec,bs="tp",k=30)+ti(i.chl,bs="tp",k=12),data=data,family=gaussian(link="log"),control=ctrl)
gam.check(gamP)

gamP<-bam(chl~ti(doy,bs="cc",k=20)+ti(date_dec,bs="tp",k=40)+ti(i.chl,bs="tp",k=12),data=data,family=gaussian(link="log"),control=ctrl)
gam.check(gamP)

gamP<-bam(chl~ti(doy,bs="cc",k=20)+ti(date_dec,bs="tp",k=50)+ti(i.chl,bs="tp",k=12),data=data,family=gaussian(link="log"),control=ctrl)
gam.check(gamP)

perStationFlowMod[[1]]=gamP
####

data=perStationAdd[[2]]

gamP<-gam(chl~ti(doy,bs="cc")+ti(date_dec,bs="tp")+ti(i.chl,bs="tp"),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=10)+ti(date_dec,bs="tp",k=10)+ti(i.chl,bs="tp",k=10),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=15)+ti(date_dec,bs="tp",k=10)+ti(i.chl,bs="tp",k=15),data=data,family=gaussian(link="log"))
gam.check(gamP) ## warning

gamP<-gam(chl~ti(doy,bs="cc",k=15)+ti(date_dec,bs="tp",k=20)+ti(i.chl,bs="tp",k=12),data=data,family=gaussian(link="log"))
gam.check(gamP) 

gamP<-bam(chl~ti(doy,bs="cc",k=15)+ti(date_dec,bs="tp",k=30)+ti(i.chl,bs="tp",k=12),data=data,family=gaussian(link="log"),control=ctrl)
gam.check(gamP) 

gamP<-bam(chl~ti(doy,bs="cc",k=15)+ti(date_dec,bs="tp",k=40)+ti(i.chl,bs="tp",k=12),data=data,family=gaussian(link="log"),control=ctrl)
gam.check(gamP) 

gamP<-bam(chl~ti(doy,bs="cc",k=15)+ti(date_dec,bs="tp",k=50)+ti(i.chl,bs="tp",k=12),data=data,family=gaussian(link="log"),control=ctrl)
gam.check(gamP) 

perStationFlowMod[[2]]=gamP
