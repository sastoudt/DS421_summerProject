## true flow models

setwd("~/Desktop/sfei")
load(file="perStationAdd.Rda")
require(mgcv)

perStationFlowTOT=vector("list",length(perStationAdd))
perStationFlowSpecific=vector("list",length(perStationAdd))
wholeSeries<-c(1, 2, 5, 7, 11, 13, 15, 16, 17, 18, 21, 22, 23, 29, 40)
ctrl <- list(nthreads=4)
data=perStationAdd[[1]]
data$TOT= 0.028316847*data$TOT
#converted to m3/s

gamP<-gam(chl~ti(doy,bs="cc")+ti(date_dec,bs="tp")+ti(TOT,bs="tp"),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=10)+ti(date_dec,bs="tp",k=10)+ti(TOT,bs="tp",k=10),data=data,family=gaussian(link="log"))
gam.check(gamP) ## warning

gamP<-gam(chl~ti(doy,bs="cc",k=10)+ti(date_dec,bs="tp")+ti(TOT,bs="tp"),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-bam(chl~ti(doy,bs="cc",k=10)+ti(date_dec,bs="tp",k=10)+ti(TOT,bs="tp"),data=data,family=gaussian(link="log"),control=ctrl)
gam.check(gamP)

gamP<-bam(chl~ti(doy,bs="cc",k=10)+ti(date_dec,bs="tp",k=10)+ti(TOT,bs="tp",k=8),data=data,family=gaussian(link="log"),control=ctrl)
gam.check(gamP) ## warning

gamP<-bam(chl~ti(doy,bs="cc",k=10)+ti(date_dec,bs="tp",k=10)+ti(TOT,bs="tp",k=7),data=data,family=gaussian(link="log"),control=ctrl)
gam.check(gamP) ## warning

gamP<-bam(chl~ti(doy,bs="cc",k=10)+ti(date_dec,bs="tp",k=10)+ti(TOT,bs="tp",k=6),data=data,family=gaussian(link="log"),control=ctrl)
gam.check(gamP)

gamP<-bam(chl~ti(doy,bs="cc",k=20)+ti(date_dec,bs="tp",k=20)+ti(TOT,bs="tp",k=6),data=data,family=gaussian(link="log"),control=ctrl)
gam.check(gamP)

gamP<-bam(chl~ti(doy,bs="cc",k=40)+ti(date_dec,bs="tp",k=40)+ti(TOT,bs="tp",k=6),data=data,family=gaussian(link="log"),control=ctrl)
gam.check(gamP)

gamP<-bam(chl~ti(doy,bs="cc",k=60)+ti(date_dec,bs="tp",k=60)+ti(TOT,bs="tp",k=6),data=data,family=gaussian(link="log"),control=ctrl)
gam.check(gamP)

gamP<-bam(chl~ti(doy,bs="cc",k=100)+ti(date_dec,bs="tp",k=100)+ti(TOT,bs="tp",k=6),data=data,family=gaussian(link="log"),control=ctrl)
gam.check(gamP)

gamP<-bam(chl~ti(doy,bs="cc",k=120)+ti(date_dec,bs="tp",k=120)+ti(TOT,bs="tp",k=6),data=data,family=gaussian(link="log"),control=ctrl)
gam.check(gamP)

gamP<-bam(chl~ti(doy,bs="cc",k=150)+ti(date_dec,bs="tp",k=150)+ti(TOT,bs="tp",k=6),data=data,family=gaussian(link="log"),control=ctrl)
gam.check(gamP) ## warning

gamP<-bam(chl~ti(doy,bs="cc",k=130)+ti(date_dec,bs="tp",k=130)+ti(TOT,bs="tp",k=6),data=data,family=gaussian(link="log"),control=ctrl)
gam.check(gamP) ## warning

gamP<-bam(chl~ti(doy,bs="cc",k=120)+ti(date_dec,bs="tp",k=130)+ti(TOT,bs="tp",k=6),data=data,family=gaussian(link="log"),control=ctrl)
gam.check(gamP)

gamP<-bam(chl~ti(doy,bs="cc",k=130)+ti(date_dec,bs="tp",k=120)+ti(TOT,bs="tp",k=6),data=data,family=gaussian(link="log"),control=ctrl)
gam.check(gamP)

perStationFlowTOT[[1]]=gamP

names(perStation)[1]
gamP<-bam(chl~ti(doy,bs="cc",k=130)+ti(date_dec,bs="tp",k=120)+ti(TOT,bs="tp",k=6),data=data,family=gaussian(link="log"),control=ctrl)
gam.check(gamP)

### no specific right now

####
data=perStationAdd[[2]]
data$TOT= 0.028316847*data$TOT
#converted to m3/s

gamP<-gam(chl~ti(doy,bs="cc")+ti(date_dec,bs="tp")+ti(TOT,bs="tp"),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-bam(chl~ti(doy,bs="cc",k=10)+ti(date_dec,bs="tp",k=10)+ti(TOT,bs="tp"),data=data,family=gaussian(link="log"),control=ctrl)
gam.check(gamP)

gamP<-bam(chl~ti(doy,bs="cc",k=10)+ti(date_dec,bs="tp",k=10)+ti(TOT,bs="tp",k=10),data=data,family=gaussian(link="log"),control=ctrl)
gam.check(gamP)

gamP<-bam(chl~ti(doy,bs="cc",k=10)+ti(date_dec,bs="tp",k=10)+ti(TOT,bs="tp",k=20),data=data,family=gaussian(link="log"),control=ctrl)
gam.check(gamP) ## warning

gamP<-bam(chl~ti(doy,bs="cc",k=10)+ti(date_dec,bs="tp",k=10)+ti(TOT,bs="tp",k=15),data=data,family=gaussian(link="log"),control=ctrl)
gam.check(gamP) ## warning

gamP<-bam(chl~ti(doy,bs="cc",k=10)+ti(date_dec,bs="tp",k=10)+ti(TOT,bs="tp",k=12),data=data,family=gaussian(link="log"),control=ctrl)
gam.check(gamP)

gamP<-bam(chl~ti(doy,bs="cc",k=50)+ti(date_dec,bs="tp",k=50)+ti(TOT,bs="tp",k=12),data=data,family=gaussian(link="log"),control=ctrl)
gam.check(gamP)

gamP<-bam(chl~ti(doy,bs="cc",k=80)+ti(date_dec,bs="tp",k=80)+ti(TOT,bs="tp",k=12),data=data,family=gaussian(link="log"),control=ctrl)
gam.check(gamP) ## warning

gamP<-bam(chl~ti(doy,bs="cc",k=60)+ti(date_dec,bs="tp",k=60)+ti(TOT,bs="tp",k=12),data=data,family=gaussian(link="log"),control=ctrl)
gam.check(gamP)

gamP<-bam(chl~ti(doy,bs="cc",k=70)+ti(date_dec,bs="tp",k=70)+ti(TOT,bs="tp",k=12),data=data,family=gaussian(link="log"),control=ctrl)
gam.check(gamP)

gamP<-bam(chl~ti(doy,bs="cc",k=80)+ti(date_dec,bs="tp",k=70)+ti(TOT,bs="tp",k=12),data=data,family=gaussian(link="log"),control=ctrl)
gam.check(gamP)

gamP<-bam(chl~ti(doy,bs="cc",k=80)+ti(date_dec,bs="tp",k=80)+ti(TOT,bs="tp",k=12),data=data,family=gaussian(link="log"),control=ctrl)
gam.check(gamP) ## warning

gamP<-bam(chl~ti(doy,bs="cc",k=80)+ti(date_dec,bs="tp",k=75)+ti(TOT,bs="tp",k=12),data=data,family=gaussian(link="log"),control=ctrl)
gam.check(gamP)

gamP<-bam(chl~ti(doy,bs="cc",k=100)+ti(date_dec,bs="tp",k=75)+ti(TOT,bs="tp",k=12),data=data,family=gaussian(link="log"),control=ctrl)
gam.check(gamP)

gamP<-bam(chl~ti(doy,bs="cc",k=120)+ti(date_dec,bs="tp",k=75)+ti(TOT,bs="tp",k=12),data=data,family=gaussian(link="log"),control=ctrl)
gam.check(gamP)

gamP<-bam(chl~ti(doy,bs="cc",k=150)+ti(date_dec,bs="tp",k=75)+ti(TOT,bs="tp",k=12),data=data,family=gaussian(link="log"),control=ctrl)
gam.check(gamP)

gamP<-bam(chl~ti(doy,bs="cc",k=180)+ti(date_dec,bs="tp",k=75)+ti(TOT,bs="tp",k=12),data=data,family=gaussian(link="log"),control=ctrl)
gam.check(gamP)

gamP<-bam(chl~ti(doy,bs="cc",k=200)+ti(date_dec,bs="tp",k=75)+ti(TOT,bs="tp",k=12),data=data,family=gaussian(link="log"),control=ctrl)
gam.check(gamP) ## go with this

perStationFlowTOT[[2]]=gamP
names(perStation)[2] ## no specific flow


####
data=perStationAdd[[5]]
data$TOT= 0.028316847*data$TOT
#converted to m3/s

gamP<-gam(chl~ti(doy,bs="cc")+ti(date_dec,bs="tp")+ti(TOT,bs="tp"),data=data,family=gaussian(link="log"))
gam.check(gamP)
