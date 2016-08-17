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

gamP<-gam(chl~ti(doy,bs="cc",k=10)+ti(date_dec,bs="tp",k=10)+ti(TOT,bs="tp",k=10),data=data,family=gaussian(link="log"))
gam.check(gamP) ## warning

gamP<-gam(chl~ti(doy,bs="cc",k=10)+ti(date_dec,bs="tp",k=10)+ti(TOT,bs="tp",k=8),data=data,family=gaussian(link="log"))
gam.check(gamP) ## warning

gamP<-gam(chl~ti(doy,bs="cc",k=10)+ti(date_dec,bs="tp",k=10)+ti(TOT,bs="tp",k=7),data=data,family=gaussian(link="log"))
gam.check(gamP)  ## warning

gamP<-gam(chl~ti(doy,bs="cc",k=10)+ti(date_dec,bs="tp",k=10)+ti(TOT,bs="tp",k=6),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=30)+ti(date_dec,bs="tp",k=30)+ti(TOT,bs="tp",k=6),data=data,family=gaussian(link="log"))
gam.check(gamP) ## warning

gamP<-gam(chl~ti(doy,bs="cc",k=30)+ti(date_dec,bs="tp",k=20)+ti(TOT,bs="tp",k=6),data=data,family=gaussian(link="log"))
gam.check(gamP) ## warning

gamP<-gam(chl~ti(doy,bs="cc",k=30)+ti(date_dec,bs="tp",k=15)+ti(TOT,bs="tp",k=6),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=30)+ti(date_dec,bs="tp",k=18)+ti(TOT,bs="tp",k=6),data=data,family=gaussian(link="log"))
gam.check(gamP) ## warning

gamP<-gam(chl~ti(doy,bs="cc",k=30)+ti(date_dec,bs="tp",k=17)+ti(TOT,bs="tp",k=6),data=data,family=gaussian(link="log"))
gam.check(gamP)  ## warning

gamP<-gam(chl~ti(doy,bs="cc",k=30)+ti(date_dec,bs="tp",k=16)+ti(TOT,bs="tp",k=6),data=data,family=gaussian(link="log"))
gam.check(gamP) ## warning

gamP<-gam(chl~ti(doy,bs="cc",k=50)+ti(date_dec,bs="tp",k=15)+ti(TOT,bs="tp",k=6),data=data,family=gaussian(link="log"))
gam.check(gamP) ## warning

gamP<-gam(chl~ti(doy,bs="cc",k=40)+ti(date_dec,bs="tp",k=15)+ti(TOT,bs="tp",k=6),data=data,family=gaussian(link="log"))
gam.check(gamP) ## warning

gamP<-gam(chl~ti(doy,bs="cc",k=35)+ti(date_dec,bs="tp",k=15)+ti(TOT,bs="tp",k=6),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=38)+ti(date_dec,bs="tp",k=15)+ti(TOT,bs="tp",k=6),data=data,family=gaussian(link="log"))
gam.check(gamP) ## warning

gamP<-gam(chl~ti(doy,bs="cc",k=37)+ti(date_dec,bs="tp",k=15)+ti(TOT,bs="tp",k=6),data=data,family=gaussian(link="log"))
gam.check(gamP) ## warning

gamP<-gam(chl~ti(doy,bs="cc",k=36)+ti(date_dec,bs="tp",k=15)+ti(TOT,bs="tp",k=6),data=data,family=gaussian(link="log"))
gam.check(gamP) ## warning

gamP<-gam(chl~ti(doy,bs="cc",k=35)+ti(date_dec,bs="tp",k=15)+ti(TOT,bs="tp",k=6),data=data,family=gaussian(link="log"))
gam.check(gamP) ## have to go with this

perStationFlowTOT[[5]]=gamP
names(perStation)[5] 

#SAC =  RIO + YOLO
data$SAC2=(data$RIO+data$YOLO)*0.028316847

#converted to m3/s

gamP<-gam(chl~ti(doy,bs="cc")+ti(date_dec,bs="tp")+ti(SAC2,bs="tp"),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=10)+ti(date_dec,bs="tp",k=10)+ti(SAC2,bs="tp",k=10),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=15)+ti(date_dec,bs="tp",k=15)+ti(SAC2,bs="tp",k=15),data=data,family=gaussian(link="log"))
gam.check(gamP) ## warning

gamP<-bam(chl~ti(doy,bs="cc",k=15)+ti(date_dec,bs="tp",k=15)+ti(SAC2,bs="tp",k=10),data=data,family=gaussian(link="log"),control=ctrl)
gam.check(gamP) ## warning

gamP<-bam(chl~ti(doy,bs="cc",k=15)+ti(date_dec,bs="tp",k=10)+ti(SAC2,bs="tp",k=10),data=data,family=gaussian(link="log"),control=ctrl)
gam.check(gamP) ## warning

gamP<-bam(chl~ti(doy,bs="cc",k=12)+ti(date_dec,bs="tp",k=10)+ti(SAC2,bs="tp",k=10),data=data,family=gaussian(link="log"),control=ctrl)
gam.check(gamP) ## warning

gamP<-bam(chl~ti(doy,bs="cc",k=10)+ti(date_dec,bs="tp",k=15)+ti(SAC2,bs="tp",k=10),data=data,family=gaussian(link="log"),control=ctrl)
gam.check(gamP) ## warning

gamP<-bam(chl~ti(doy,bs="cc",k=10)+ti(date_dec,bs="tp",k=12)+ti(SAC2,bs="tp",k=10),data=data,family=gaussian(link="log"),control=ctrl)
gam.check(gamP) ## warning

gamP<-bam(chl~ti(doy,bs="cc",k=10)+ti(date_dec,bs="tp",k=10)+ti(SAC2,bs="tp",k=10),data=data,family=gaussian(link="log"),control=ctrl)
gam.check(gamP)  ##  warning

gamP<-bam(chl~ti(doy,bs="cc",k=10)+ti(date_dec,bs="tp",k=10)+ti(SAC2,bs="tp",k=8),data=data,family=gaussian(link="log"),control=ctrl)
gam.check(gamP)   ## warning

gamP<-bam(chl~ti(doy,bs="cc")+ti(date_dec,bs="tp")+ti(SAC2,bs="tp",k=8),data=data,family=gaussian(link="log"),control=ctrl)
gam.check(gamP)  ## warning

gamP<-bam(chl~ti(doy,bs="cc")+ti(date_dec,bs="tp",k=10)+ti(SAC2,bs="tp"),data=data,family=gaussian(link="log"),control=ctrl)
gam.check(gamP)  

gamP<-bam(chl~ti(doy,bs="cc",k=10)+ti(date_dec,bs="tp",k=10)+ti(SAC2,bs="tp"),data=data,family=gaussian(link="log"),control=ctrl)
gam.check(gamP) 

gamP<-bam(chl~ti(doy,bs="cc",k=10)+ti(date_dec,bs="tp",k=10)+ti(SAC2,bs="tp",k=7),data=data,family=gaussian(link="log"),control=ctrl)
gam.check(gamP)  ## warning

gamP<-bam(chl~ti(doy,bs="cc",k=10)+ti(date_dec,bs="tp",k=10)+ti(SAC2,bs="tp",k=6),data=data,family=gaussian(link="log"),control=ctrl)
gam.check(gamP) 

gamP<-bam(chl~ti(doy,bs="cc",k=30)+ti(date_dec,bs="tp",k=30)+ti(SAC2,bs="tp",k=6),data=data,family=gaussian(link="log"),control=ctrl)
gam.check(gamP) 

gamP<-bam(chl~ti(doy,bs="cc",k=50)+ti(date_dec,bs="tp",k=50)+ti(SAC2,bs="tp",k=6),data=data,family=gaussian(link="log"),control=ctrl)
gam.check(gamP) 

gamP<-bam(chl~ti(doy,bs="cc",k=80)+ti(date_dec,bs="tp",k=80)+ti(SAC2,bs="tp",k=6),data=data,family=gaussian(link="log"),control=ctrl)
gam.check(gamP) 

gamP<-bam(chl~ti(doy,bs="cc",k=100)+ti(date_dec,bs="tp",k=100)+ti(SAC2,bs="tp",k=6),data=data,family=gaussian(link="log"),control=ctrl)
gam.check(gamP) 

perStationFlowSpecific[[5]]=gamP

####
data=perStationAdd[[7]]
data$TOT= 0.028316847*data$TOT
#converted to m3/s

gamP<-gam(chl~ti(doy,bs="cc")+ti(date_dec,bs="tp")+ti(TOT,bs="tp"),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc")+ti(date_dec,bs="tp")+ti(TOT,bs="tp",k=10),data=data,family=gaussian(link="log"))
gam.check(gamP) ## warning

gamP<-gam(chl~ti(doy,bs="cc")+ti(date_dec,bs="tp")+ti(TOT,bs="tp",k=8),data=data,family=gaussian(link="log"))
gam.check(gamP)  ## warnings

gamP<-gam(chl~ti(doy,bs="cc")+ti(date_dec,bs="tp")+ti(TOT,bs="tp",k=7),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=30)+ti(date_dec,bs="tp",k=30)+ti(TOT,bs="tp",k=7),data=data,family=gaussian(link="log"))
gam.check(gamP) ## warning


gamP<-gam(chl~ti(doy,bs="cc",k=30)+ti(date_dec,bs="tp",k=20)+ti(TOT,bs="tp",k=7),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=30)+ti(date_dec,bs="tp",k=25)+ti(TOT,bs="tp",k=7),data=data,family=gaussian(link="log"))
gam.check(gamP) ## warning

gamP<-bam(chl~ti(doy,bs="cc",k=30)+ti(date_dec,bs="tp",k=22)+ti(TOT,bs="tp",k=7),data=data,family=gaussian(link="log"),control=ctrl)
gam.check(gamP)

gamP<-bam(chl~ti(doy,bs="cc",k=50)+ti(date_dec,bs="tp",k=22)+ti(TOT,bs="tp",k=7),data=data,family=gaussian(link="log"),control=ctrl)
gam.check(gamP)

gamP<-bam(chl~ti(doy,bs="cc",k=80)+ti(date_dec,bs="tp",k=22)+ti(TOT,bs="tp",k=7),data=data,family=gaussian(link="log"),control=ctrl)
gam.check(gamP)

gamP<-bam(chl~ti(doy,bs="cc",k=100)+ti(date_dec,bs="tp",k=22)+ti(TOT,bs="tp",k=7),data=data,family=gaussian(link="log"),control=ctrl)
gam.check(gamP)

gamP<-bam(chl~ti(doy,bs="cc",k=120)+ti(date_dec,bs="tp",k=22)+ti(TOT,bs="tp",k=7),data=data,family=gaussian(link="log"),control=ctrl)
gam.check(gamP)

gamP<-bam(chl~ti(doy,bs="cc",k=150)+ti(date_dec,bs="tp",k=22)+ti(TOT,bs="tp",k=7),data=data,family=gaussian(link="log"),control=ctrl)
gam.check(gamP)

gamP<-bam(chl~ti(doy,bs="cc",k=100)+ti(date_dec,bs="tp",k=22)+ti(TOT,bs="tp",k=7),data=data,family=gaussian(link="log"),control=ctrl)
gam.check(gamP) ## go with this

perStationFlowTOT[[7]]=gamP
names(perStation)[7]

data$SAC2=(data$RIO+data$YOLO)*0.028316847

#converted to m3/s

gamP<-gam(chl~ti(doy,bs="cc")+ti(date_dec,bs="tp")+ti(SAC2,bs="tp"),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc")+ti(date_dec,bs="tp")+ti(SAC2,bs="tp",k=10),data=data,family=gaussian(link="log"))
gam.check(gamP) ## warning

gamP<-gam(chl~ti(doy,bs="cc")+ti(date_dec,bs="tp")+ti(SAC2,bs="tp",k=8),data=data,family=gaussian(link="log"))
gam.check(gamP) ## warning

gamP<-gam(chl~ti(doy,bs="cc")+ti(date_dec,bs="tp")+ti(SAC2,bs="tp",k=7),data=data,family=gaussian(link="log"))
gam.check(gamP) ## warning

gamP<-gam(chl~ti(doy,bs="cc")+ti(date_dec,bs="tp")+ti(SAC2,bs="tp",k=6),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=20)+ti(date_dec,bs="tp",k=20)+ti(SAC2,bs="tp",k=6),data=data,family=gaussian(link="log"))
gam.check(gamP) ## warning

gamP<-gam(chl~ti(doy,bs="cc",k=20)+ti(date_dec,bs="tp",k=15)+ti(SAC2,bs="tp",k=6),data=data,family=gaussian(link="log"))
gam.check(gamP)  ## warning

gamP<-gam(chl~ti(doy,bs="cc",k=20)+ti(date_dec,bs="tp",k=10)+ti(SAC2,bs="tp",k=6),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=40)+ti(date_dec,bs="tp",k=10)+ti(SAC2,bs="tp",k=6),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-bam(chl~ti(doy,bs="cc",k=80)+ti(date_dec,bs="tp",k=10)+ti(SAC2,bs="tp",k=6),data=data,family=gaussian(link="log"),control=ctrl)
gam.check(gamP)

gamP<-bam(chl~ti(doy,bs="cc",k=80)+ti(date_dec,bs="tp",k=12)+ti(SAC2,bs="tp",k=6),data=data,family=gaussian(link="log"),control=ctrl)
gam.check(gamP)

gamP<-bam(chl~ti(doy,bs="cc",k=100)+ti(date_dec,bs="tp",k=12)+ti(SAC2,bs="tp",k=6),data=data,family=gaussian(link="log"),control=ctrl)
gam.check(gamP)

gamP<-bam(chl~ti(doy,bs="cc",k=120)+ti(date_dec,bs="tp",k=12)+ti(SAC2,bs="tp",k=6),data=data,family=gaussian(link="log"),control=ctrl)
gam.check(gamP)

gamP<-bam(chl~ti(doy,bs="cc",k=150)+ti(date_dec,bs="tp",k=12)+ti(SAC2,bs="tp",k=6),data=data,family=gaussian(link="log"),control=ctrl)
gam.check(gamP)

perStationFlowSpecific[[7]]=gamP
####

data=perStationAdd[[11]]
data$TOT= 0.028316847*data$TOT
#converted to m3/s

gamP<-gam(chl~ti(doy,bs="cc")+ti(date_dec,bs="tp")+ti(TOT,bs="tp"),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc")+ti(date_dec,bs="tp")+ti(TOT,bs="tp",k=10),data=data,family=gaussian(link="log"))
gam.check(gamP) ## warning

gamP<-gam(chl~ti(doy,bs="cc")+ti(date_dec,bs="tp")+ti(TOT,bs="tp",k=8),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=30)+ti(date_dec,bs="tp",k=30)+ti(TOT,bs="tp",k=8),data=data,family=gaussian(link="log"))
gam.check(gamP) ## warning

gamP<-bam(chl~ti(doy,bs="cc",k=30)+ti(date_dec,bs="tp",k=20)+ti(TOT,bs="tp",k=8),data=data,family=gaussian(link="log"),control=ctrl)
gam.check(gamP)

gamP<-bam(chl~ti(doy,bs="cc",k=30)+ti(date_dec,bs="tp",k=25)+ti(TOT,bs="tp",k=8),data=data,family=gaussian(link="log"),control=ctrl)
gam.check(gamP) ## warning

gamP<-bam(chl~ti(doy,bs="cc",k=50)+ti(date_dec,bs="tp",k=20)+ti(TOT,bs="tp",k=8),data=data,family=gaussian(link="log"),control=ctrl)
gam.check(gamP) 

gamP<-bam(chl~ti(doy,bs="cc",k=80)+ti(date_dec,bs="tp",k=20)+ti(TOT,bs="tp",k=8),data=data,family=gaussian(link="log"),control=ctrl)
gam.check(gamP) 

gamP<-bam(chl~ti(doy,bs="cc",k=100)+ti(date_dec,bs="tp",k=20)+ti(TOT,bs="tp",k=8),data=data,family=gaussian(link="log"),control=ctrl)
gam.check(gamP) 

gamP<-bam(chl~ti(doy,bs="cc",k=120)+ti(date_dec,bs="tp",k=20)+ti(TOT,bs="tp",k=8),data=data,family=gaussian(link="log"),control=ctrl)
gam.check(gamP) 

perStationFlowTOT[[11]]=gamP
names(perStation)[11]
## no specific on D19

####
data=perStationAdd[[13]]
data$TOT= 0.028316847*data$TOT
#converted to m3/s

gamP<-gam(chl~ti(doy,bs="cc")+ti(date_dec,bs="tp")+ti(TOT,bs="tp"),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc")+ti(date_dec,bs="tp",k=10)+ti(TOT,bs="tp"),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc")+ti(date_dec,bs="tp",k=10)+ti(TOT,bs="tp",k=8),data=data,family=gaussian(link="log"))
gam.check(gamP) ## warning

gamP<-gam(chl~ti(doy,bs="cc")+ti(date_dec,bs="tp",k=10)+ti(TOT,bs="tp",k=7),data=data,family=gaussian(link="log"))
gam.check(gamP) ## warning

gamP<-gam(chl~ti(doy,bs="cc")+ti(date_dec,bs="tp",k=10)+ti(TOT,bs="tp",k=6),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-bam(chl~ti(doy,bs="cc",k=30)+ti(date_dec,bs="tp",k=20)+ti(TOT,bs="tp",k=6),data=data,family=gaussian(link="log"),control=ctrl)
gam.check(gamP)

gamP<-bam(chl~ti(doy,bs="cc",k=30)+ti(date_dec,bs="tp",k=25)+ti(TOT,bs="tp",k=6),data=data,family=gaussian(link="log"),control=ctrl)
gam.check(gamP) ## warning

gamP<-bam(chl~ti(doy,bs="cc",k=50)+ti(date_dec,bs="tp",k=20)+ti(TOT,bs="tp",k=6),data=data,family=gaussian(link="log"),control=ctrl)
gam.check(gamP)

gamP<-bam(chl~ti(doy,bs="cc",k=100)+ti(date_dec,bs="tp",k=20)+ti(TOT,bs="tp",k=6),data=data,family=gaussian(link="log"),control=ctrl)
gam.check(gamP) ## error

gamP<-bam(chl~ti(doy,bs="cc",k=80)+ti(date_dec,bs="tp",k=20)+ti(TOT,bs="tp",k=6),data=data,family=gaussian(link="log"),control=ctrl)
gam.check(gamP) ## warning

gamP<-bam(chl~ti(doy,bs="cc",k=70)+ti(date_dec,bs="tp",k=20)+ti(TOT,bs="tp",k=6),data=data,family=gaussian(link="log"),control=ctrl)
gam.check(gamP) ## warning

gamP<-bam(chl~ti(doy,bs="cc",k=60)+ti(date_dec,bs="tp",k=20)+ti(TOT,bs="tp",k=6),data=data,family=gaussian(link="log"),control=ctrl)
gam.check(gamP) ## warning

gamP<-bam(chl~ti(doy,bs="cc",k=55)+ti(date_dec,bs="tp",k=20)+ti(TOT,bs="tp",k=6),data=data,family=gaussian(link="log"),control=ctrl)
gam.check(gamP)

perStationFlowTOT[[13]]=gamP
names(perStation)[13]

data$SAC2=(data$RIO+data$YOLO)*0.028316847

#converted to m3/s

gamP<-gam(chl~ti(doy,bs="cc")+ti(date_dec,bs="tp")+ti(SAC2,bs="tp"),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc")+ti(date_dec,bs="tp")+ti(SAC2,bs="tp",k=10),data=data,family=gaussian(link="log"))
gam.check(gamP) ## warning

gamP<-gam(chl~ti(doy,bs="cc")+ti(date_dec,bs="tp")+ti(SAC2,bs="tp",k=8),data=data,family=gaussian(link="log"))
gam.check(gamP)  

gamP<-bam(chl~ti(doy,bs="cc",k=30)+ti(date_dec,bs="tp",k=20)+ti(SAC2,bs="tp",k=8),data=data,family=gaussian(link="log"),control=ctrl)
gam.check(gamP)  ## warning

gamP<-bam(chl~ti(doy,bs="cc",k=30)+ti(date_dec,bs="tp",k=20)+ti(SAC2,bs="tp",k=8),data=data,family=gaussian(link="log"),control=ctrl)
gam.check(gamP) ## warning

gamP<-bam(chl~ti(doy,bs="cc",k=25)+ti(date_dec,bs="tp",k=20)+ti(SAC2,bs="tp",k=8),data=data,family=gaussian(link="log"),control=ctrl)
gam.check(gamP) ## warning

gamP<-bam(chl~ti(doy,bs="cc",k=20)+ti(date_dec,bs="tp",k=15)+ti(SAC2,bs="tp",k=8),data=data,family=gaussian(link="log"),control=ctrl)
gam.check(gamP) ## warning

gamP<-bam(chl~ti(doy,bs="cc",k=20)+ti(date_dec,bs="tp",k=10)+ti(SAC2,bs="tp",k=8),data=data,family=gaussian(link="log"),control=ctrl)
gam.check(gamP) ## warning

gamP<-bam(chl~ti(doy,bs="cc",k=15)+ti(date_dec,bs="tp",k=10)+ti(SAC2,bs="tp",k=8),data=data,family=gaussian(link="log"),control=ctrl)
gam.check(gamP) ## warning

gamP<-bam(chl~ti(doy,bs="cc",k=10)+ti(date_dec,bs="tp",k=10)+ti(SAC2,bs="tp",k=8),data=data,family=gaussian(link="log"),control=ctrl)
gam.check(gamP)  ## warning

gamP<-bam(chl~ti(doy,bs="cc",k=10)+ti(date_dec,bs="tp",k=8)+ti(SAC2,bs="tp",k=8),data=data,family=gaussian(link="log"),control=ctrl)
gam.check(gamP)  ## warning

gamP<-bam(chl~ti(doy,bs="cc")+ti(date_dec,bs="tp",k=8)+ti(SAC2,bs="tp",k=8),data=data,family=gaussian(link="log"),control=ctrl)
gam.check(gamP)  ## warning

gamP<-bam(chl~ti(doy,bs="cc")+ti(date_dec,bs="tp")+ti(SAC2,bs="tp",k=8),data=data,family=gaussian(link="log"),control=ctrl)
gam.check(gamP) 

gamP<-bam(chl~ti(doy,bs="cc")+ti(date_dec,bs="tp")+ti(SAC2,bs="tp"),data=data,family=gaussian(link="log"),control=ctrl)
gam.check(gamP) 

gamP<-bam(chl~ti(doy,bs="cc",k=10)+ti(date_dec,bs="tp")+ti(SAC2,bs="tp"),data=data,family=gaussian(link="log"),control=ctrl)
gam.check(gamP) 


gamP<-bam(chl~ti(doy,bs="cc",k=10)+ti(date_dec,bs="tp",k=10)+ti(SAC2,bs="tp"),data=data,family=gaussian(link="log"),control=ctrl)
gam.check(gamP) 

gamP<-bam(chl~ti(doy,bs="cc",k=30)+ti(date_dec,bs="tp",k=20)+ti(SAC2,bs="tp"),data=data,family=gaussian(link="log"),control=ctrl)
gam.check(gamP) 

gamP<-bam(chl~ti(doy,bs="cc",k=30)+ti(date_dec,bs="tp",k=25)+ti(SAC2,bs="tp"),data=data,family=gaussian(link="log"),control=ctrl)
gam.check(gamP)  ## warning

gamP<-bam(chl~ti(doy,bs="cc",k=50)+ti(date_dec,bs="tp",k=20)+ti(SAC2,bs="tp"),data=data,family=gaussian(link="log"),control=ctrl)
gam.check(gamP) ## warning  

gamP<-bam(chl~ti(doy,bs="cc",k=40)+ti(date_dec,bs="tp",k=20)+ti(SAC2,bs="tp"),data=data,family=gaussian(link="log"),control=ctrl)
gam.check(gamP) ## warning

gamP<-bam(chl~ti(doy,bs="cc",k=35)+ti(date_dec,bs="tp",k=20)+ti(SAC2,bs="tp"),data=data,family=gaussian(link="log"),control=ctrl)
gam.check(gamP) ## stuck with this

perStationFlowSpecific[[13]]=gamP

####
data=perStationAdd[[15]]
data$TOT= 0.028316847*data$TOT
#converted to m3/s

gamP<-gam(chl~ti(doy,bs="cc")+ti(date_dec,bs="tp")+ti(TOT,bs="tp"),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc")+ti(date_dec,bs="tp")+ti(TOT,bs="tp",k=10),data=data,family=gaussian(link="log"))
gam.check(gamP) ## warning

gamP<-gam(chl~ti(doy,bs="cc")+ti(date_dec,bs="tp")+ti(TOT,bs="tp",k=8),data=data,family=gaussian(link="log"))
gam.check(gamP) 

gamP<-gam(chl~ti(doy,bs="cc",k=30)+ti(date_dec,bs="tp",k=20)+ti(TOT,bs="tp",k=8),data=data,family=gaussian(link="log"))
gam.check(gamP) 

gamP<-gam(chl~ti(doy,bs="cc",k=30)+ti(date_dec,bs="tp",k=25)+ti(TOT,bs="tp",k=8),data=data,family=gaussian(link="log"))
gam.check(gamP) 

gamP<-gam(chl~ti(doy,bs="cc",k=50)+ti(date_dec,bs="tp",k=25)+ti(TOT,bs="tp",k=8),data=data,family=gaussian(link="log"))
gam.check(gamP) 

gamP<-bam(chl~ti(doy,bs="cc",k=80)+ti(date_dec,bs="tp",k=25)+ti(TOT,bs="tp",k=8),data=data,family=gaussian(link="log"),control=ctrl)
gam.check(gamP) 

perStationFlowTOT[[15]]=gamP
names(perStation)[15]

data$SAC2=(data$RIO+data$YOLO)*0.028316847

#converted to m3/s

gamP<-gam(chl~ti(doy,bs="cc")+ti(date_dec,bs="tp")+ti(SAC2,bs="tp"),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc")+ti(date_dec,bs="tp")+ti(SAC2,bs="tp",k=10),data=data,family=gaussian(link="log"))
gam.check(gamP) ## warning

gamP<-gam(chl~ti(doy,bs="cc")+ti(date_dec,bs="tp")+ti(SAC2,bs="tp",k=8),data=data,family=gaussian(link="log"))
gam.check(gamP)  ## warning

gamP<-gam(chl~ti(doy,bs="cc")+ti(date_dec,bs="tp")+ti(SAC2,bs="tp",k=7),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=30)+ti(date_dec,bs="tp",k=20)+ti(SAC2,bs="tp",k=7),data=data,family=gaussian(link="log"))
gam.check(gamP) ## warning

gamP<-gam(chl~ti(doy,bs="cc",k=30)+ti(date_dec,bs="tp")+ti(SAC2,bs="tp",k=7),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=50)+ti(date_dec,bs="tp",k=10)+ti(SAC2,bs="tp",k=7),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=80)+ti(date_dec,bs="tp",k=15)+ti(SAC2,bs="tp",k=7),data=data,family=gaussian(link="log"))
gam.check(gamP)

perStationFlowSpecific[[15]]=gamP

####

data=perStationAdd[[16]]
data$TOT= 0.028316847*data$TOT
#converted to m3/s

gamP<-gam(chl~ti(doy,bs="cc")+ti(date_dec,bs="tp")+ti(TOT,bs="tp"),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc")+ti(date_dec,bs="tp")+ti(TOT,bs="tp",k=10),data=data,family=gaussian(link="log"))
gam.check(gamP) ## warning

gamP<-gam(chl~ti(doy,bs="cc")+ti(date_dec,bs="tp")+ti(TOT,bs="tp",k=8),data=data,family=gaussian(link="log"))
gam.check(gamP) ## warning

gamP<-gam(chl~ti(doy,bs="cc")+ti(date_dec,bs="tp")+ti(TOT,bs="tp",k=7),data=data,family=gaussian(link="log"))
gam.check(gamP) ## warning

gamP<-gam(chl~ti(doy,bs="cc")+ti(date_dec,bs="tp")+ti(TOT,bs="tp",k=6),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=20)+ti(date_dec,bs="tp",k=15)+ti(TOT,bs="tp",k=6),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-bam(chl~ti(doy,bs="cc",k=20)+ti(date_dec,bs="tp",k=20)+ti(TOT,bs="tp",k=6),data=data,family=gaussian(link="log"),control=ctrl)
gam.check(gamP)

gamP<-bam(chl~ti(doy,bs="cc",k=30)+ti(date_dec,bs="tp",k=25)+ti(TOT,bs="tp",k=6),data=data,family=gaussian(link="log"),control=ctrl)
gam.check(gamP) ## warning

gamP<-bam(chl~ti(doy,bs="cc",k=50)+ti(date_dec,bs="tp",k=20)+ti(TOT,bs="tp",k=6),data=data,family=gaussian(link="log"),control=ctrl)
gam.check(gamP) 

gamP<-bam(chl~ti(doy,bs="cc",k=70)+ti(date_dec,bs="tp",k=20)+ti(TOT,bs="tp",k=6),data=data,family=gaussian(link="log"),control=ctrl)
gam.check(gamP) 

gamP<-bam(chl~ti(doy,bs="cc",k=100)+ti(date_dec,bs="tp",k=20)+ti(TOT,bs="tp",k=6),data=data,family=gaussian(link="log"),control=ctrl)
gam.check(gamP) 

gamP<-bam(chl~ti(doy,bs="cc",k=120)+ti(date_dec,bs="tp",k=20)+ti(TOT,bs="tp",k=6),data=data,family=gaussian(link="log"),control=ctrl)
gam.check(gamP) 

gamP<-bam(chl~ti(doy,bs="cc",k=150)+ti(date_dec,bs="tp",k=20)+ti(TOT,bs="tp",k=6),data=data,family=gaussian(link="log"),control=ctrl)
gam.check(gamP) 

perStationFlowTOT[[16]]=gamP
names(perStation)[16]

data$SAC2=(data$RIO+data$YOLO)*0.028316847

#converted to m3/s

gamP<-gam(chl~ti(doy,bs="cc")+ti(date_dec,bs="tp")+ti(SAC2,bs="tp"),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc")+ti(date_dec,bs="tp")+ti(SAC2,bs="tp",k=10),data=data,family=gaussian(link="log"))
gam.check(gamP) ## warning

gamP<-gam(chl~ti(doy,bs="cc")+ti(date_dec,bs="tp")+ti(SAC2,bs="tp",k=8),data=data,family=gaussian(link="log"))
gam.check(gamP) ## warning

gamP<-gam(chl~ti(doy,bs="cc")+ti(date_dec,bs="tp")+ti(SAC2,bs="tp",k=7),data=data,family=gaussian(link="log"))
gam.check(gamP) ## warning

gamP<-gam(chl~ti(doy,bs="cc")+ti(date_dec,bs="tp")+ti(SAC2,bs="tp",k=6),data=data,family=gaussian(link="log"))
gam.check(gamP) ## warning

gamP<-gam(chl~ti(doy,bs="cc",k=30)+ti(date_dec,bs="tp",k=15)+ti(SAC2,bs="tp"),data=data,family=gaussian(link="log"))
gam.check(gamP) ## warning

gamP<-gam(chl~ti(doy,bs="cc",k=30)+ti(date_dec,bs="tp",k=10)+ti(SAC2,bs="tp"),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-bam(chl~ti(doy,bs="cc",k=80)+ti(date_dec,bs="tp",k=10)+ti(SAC2,bs="tp"),data=data,family=gaussian(link="log"),control=ctrl)
gam.check(gamP)

gamP<-bam(chl~ti(doy,bs="cc",k=80)+ti(date_dec,bs="tp",k=12)+ti(SAC2,bs="tp"),data=data,family=gaussian(link="log"),control=ctrl)
gam.check(gamP)

gamP<-bam(chl~ti(doy,bs="cc",k=100)+ti(date_dec,bs="tp",k=12)+ti(SAC2,bs="tp"),data=data,family=gaussian(link="log"),control=ctrl)
gam.check(gamP)

gamP<-bam(chl~ti(doy,bs="cc",k=120)+ti(date_dec,bs="tp",k=12)+ti(SAC2,bs="tp"),data=data,family=gaussian(link="log"),control=ctrl)
gam.check(gamP)

gamP<-bam(chl~ti(doy,bs="cc",k=150)+ti(date_dec,bs="tp",k=12)+ti(SAC2,bs="tp"),data=data,family=gaussian(link="log"),control=ctrl)
gam.check(gamP)

perStationFlowSpecific[[16]]=gamP

####
data=perStationAdd[[17]]
data$TOT= 0.028316847*data$TOT
#converted to m3/s

gamP<-gam(chl~ti(doy,bs="cc")+ti(date_dec,bs="tp")+ti(TOT,bs="tp"),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc")+ti(date_dec,bs="tp")+ti(TOT,bs="tp",k=10),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=20)+ti(date_dec,bs="tp",k=20)+ti(TOT,bs="tp",k=10),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=20)+ti(date_dec,bs="tp",k=20)+ti(TOT,bs="tp",k=15),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-bam(chl~ti(doy,bs="cc",k=40)+ti(date_dec,bs="tp",k=30)+ti(TOT,bs="tp",k=15),data=data,family=gaussian(link="log"),control=ctrl)
gam.check(gamP) ## warning

gamP<-bam(chl~ti(doy,bs="cc",k=40)+ti(date_dec,bs="tp",k=25)+ti(TOT,bs="tp",k=15),data=data,family=gaussian(link="log"),control=ctrl)
gam.check(gamP) ## warning

gamP<-bam(chl~ti(doy,bs="cc",k=40)+ti(date_dec,bs="tp",k=20)+ti(TOT,bs="tp",k=15),data=data,family=gaussian(link="log"),control=ctrl)
gam.check(gamP)  ## warning

gamP<-bam(chl~ti(doy,bs="cc",k=30)+ti(date_dec,bs="tp",k=20)+ti(TOT,bs="tp",k=15),data=data,family=gaussian(link="log"),control=ctrl)
gam.check(gamP) ## warning

gamP<-bam(chl~ti(doy,bs="cc",k=25)+ti(date_dec,bs="tp",k=20)+ti(TOT,bs="tp",k=15),data=data,family=gaussian(link="log"),control=ctrl)
gam.check(gamP) ## warning

gamP<-gam(chl~ti(doy,bs="cc",k=20)+ti(date_dec,bs="tp",k=20)+ti(TOT,bs="tp",k=20),data=data,family=gaussian(link="log"))
gam.check(gamP) ## warning

gamP<-gam(chl~ti(doy,bs="cc",k=22)+ti(date_dec,bs="tp",k=20)+ti(TOT,bs="tp",k=15),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=22)+ti(date_dec,bs="tp",k=22)+ti(TOT,bs="tp",k=15),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=22)+ti(date_dec,bs="tp",k=22)+ti(TOT,bs="tp",k=18),data=data,family=gaussian(link="log"))
gam.check(gamP) ## warning

gamP<-gam(chl~ti(doy,bs="cc",k=22)+ti(date_dec,bs="tp",k=22)+ti(TOT,bs="tp",k=15),data=data,family=gaussian(link="log"))
gam.check(gamP) ## go with this

perStationFlowTOT[[17]]=gamP
names(perStation)[17]


data$SAC2=(data$RIO+data$YOLO)*0.028316847

#converted to m3/s

gamP<-gam(chl~ti(doy,bs="cc")+ti(date_dec,bs="tp")+ti(SAC2,bs="tp"),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc")+ti(date_dec,bs="tp")+ti(SAC2,bs="tp",k=10),data=data,family=gaussian(link="log"))
gam.check(gamP) ## warning

gamP<-bam(chl~ti(doy,bs="cc")+ti(date_dec,bs="tp")+ti(SAC2,bs="tp",k=8),data=data,family=gaussian(link="log"),control=ctrl)
gam.check(gamP) ## 

gamP<-bam(chl~ti(doy,bs="cc",k=30)+ti(date_dec,bs="tp",k=20)+ti(SAC2,bs="tp",k=8),data=data,family=gaussian(link="log"),control=ctrl)
gam.check(gamP)

gamP<-bam(chl~ti(doy,bs="cc",k=30)+ti(date_dec,bs="tp",k=25)+ti(SAC2,bs="tp",k=8),data=data,family=gaussian(link="log"),control=ctrl)
gam.check(gamP)

gamP<-bam(chl~ti(doy,bs="cc",k=30)+ti(date_dec,bs="tp",k=30)+ti(SAC2,bs="tp",k=8),data=data,family=gaussian(link="log"),control=ctrl)
gam.check(gamP)

gamP<-bam(chl~ti(doy,bs="cc",k=50)+ti(date_dec,bs="tp",k=30)+ti(SAC2,bs="tp",k=8),data=data,family=gaussian(link="log"),control=ctrl)
gam.check(gamP)

gamP<-bam(chl~ti(doy,bs="cc",k=50)+ti(date_dec,bs="tp",k=35)+ti(SAC2,bs="tp",k=8),data=data,family=gaussian(link="log"),control=ctrl)
gam.check(gamP)

gamP<-bam(chl~ti(doy,bs="cc",k=80)+ti(date_dec,bs="tp",k=35)+ti(SAC2,bs="tp",k=8),data=data,family=gaussian(link="log"),control=ctrl)
gam.check(gamP)

gamP<-bam(chl~ti(doy,bs="cc",k=80)+ti(date_dec,bs="tp",k=40)+ti(SAC2,bs="tp",k=8),data=data,family=gaussian(link="log"),control=ctrl)
gam.check(gamP)

gamP<-bam(chl~ti(doy,bs="cc",k=100)+ti(date_dec,bs="tp",k=40)+ti(SAC2,bs="tp",k=8),data=data,family=gaussian(link="log"),control=ctrl)
gam.check(gamP)

gamP<-bam(chl~ti(doy,bs="cc",k=100)+ti(date_dec,bs="tp",k=45)+ti(SAC2,bs="tp",k=8),data=data,family=gaussian(link="log"),control=ctrl)
gam.check(gamP)

gamP<-bam(chl~ti(doy,bs="cc",k=120)+ti(date_dec,bs="tp",k=45)+ti(SAC2,bs="tp",k=8),data=data,family=gaussian(link="log"),control=ctrl)
gam.check(gamP)

perStationFlowSpecific[[17]]=gamP

####
data=perStationAdd[[18]]
data$TOT= 0.028316847*data$TOT
#converted to m3/s

gamP<-gam(chl~ti(doy,bs="cc")+ti(date_dec,bs="tp")+ti(TOT,bs="tp"),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc")+ti(date_dec,bs="tp")+ti(TOT,bs="tp",k=10),data=data,family=gaussian(link="log"))
gam.check(gamP) ## warning

gamP<-gam(chl~ti(doy,bs="cc",k=30)+ti(date_dec,bs="tp",k=20)+ti(TOT,bs="tp",k=8),data=data,family=gaussian(link="log"))
gam.check(gamP) ## warning

gamP<-gam(chl~ti(doy,bs="cc",k=30)+ti(date_dec,bs="tp",k=20)+ti(TOT,bs="tp",k=7),data=data,family=gaussian(link="log"))
gam.check(gamP) 

gamP<-gam(chl~ti(doy,bs="cc",k=30)+ti(date_dec,bs="tp",k=25)+ti(TOT,bs="tp",k=7),data=data,family=gaussian(link="log"))
gam.check(gamP) 

gamP<-gam(chl~ti(doy,bs="cc",k=50)+ti(date_dec,bs="tp",k=30)+ti(TOT,bs="tp",k=7),data=data,family=gaussian(link="log"))
gam.check(gamP) 

gamP<-gam(chl~ti(doy,bs="cc",k=80)+ti(date_dec,bs="tp",k=35)+ti(TOT,bs="tp",k=7),data=data,family=gaussian(link="log"))
gam.check(gamP) 

gamP<-bam(chl~ti(doy,bs="cc",k=80)+ti(date_dec,bs="tp",k=40)+ti(TOT,bs="tp",k=7),data=data,family=gaussian(link="log"),control=ctrl)
gam.check(gamP) 

gamP<-bam(chl~ti(doy,bs="cc",k=100)+ti(date_dec,bs="tp",k=40)+ti(TOT,bs="tp",k=7),data=data,family=gaussian(link="log"),control=ctrl)
gam.check(gamP) 

gamP<-bam(chl~ti(doy,bs="cc",k=100)+ti(date_dec,bs="tp",k=50)+ti(TOT,bs="tp",k=7),data=data,family=gaussian(link="log"),control=ctrl)
gam.check(gamP) 

gamP<-bam(chl~ti(doy,bs="cc",k=120)+ti(date_dec,bs="tp",k=50)+ti(TOT,bs="tp",k=7),data=data,family=gaussian(link="log"),control=ctrl)
gam.check(gamP) 

gamP<-bam(chl~ti(doy,bs="cc",k=150)+ti(date_dec,bs="tp",k=50)+ti(TOT,bs="tp",k=7),data=data,family=gaussian(link="log"),control=ctrl)
gam.check(gamP) 

perStationFlowTOT[[18]]=gamP
names(perStation)[18]
## none for D41

####
data=perStationAdd[[21]]
data$TOT= 0.028316847*data$TOT
#converted to m3/s

gamP<-gam(chl~ti(doy,bs="cc")+ti(date_dec,bs="tp")+ti(TOT,bs="tp"),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc")+ti(date_dec,bs="tp")+ti(TOT,bs="tp",k=10),data=data,family=gaussian(link="log"))
gam.check(gamP) ## warning

gamP<-gam(chl~ti(doy,bs="cc")+ti(date_dec,bs="tp")+ti(TOT,bs="tp",k=8),data=data,family=gaussian(link="log"))
gam.check(gamP)  ## warning

gamP<-gam(chl~ti(doy,bs="cc")+ti(date_dec,bs="tp")+ti(TOT,bs="tp",k=7),data=data,family=gaussian(link="log"))
gam.check(gamP)  ## warning

gamP<-gam(chl~ti(doy,bs="cc")+ti(date_dec,bs="tp")+ti(TOT,bs="tp",k=6),data=data,family=gaussian(link="log"))
gam.check(gamP) 

gamP<-gam(chl~ti(doy,bs="cc",k=30)+ti(date_dec,bs="tp",k=15)+ti(TOT,bs="tp",k=6),data=data,family=gaussian(link="log"))
gam.check(gamP) ## warning 

gamP<-gam(chl~ti(doy,bs="cc",k=30)+ti(date_dec,bs="tp",k=10)+ti(TOT,bs="tp",k=6),data=data,family=gaussian(link="log"))
gam.check(gamP) ## warning

gamP<-gam(chl~ti(doy,bs="cc")+ti(date_dec,bs="tp",k=10)+ti(TOT,bs="tp",k=6),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=10)+ti(date_dec,bs="tp",k=10)+ti(TOT,bs="tp",k=6),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=20)+ti(date_dec,bs="tp",k=10)+ti(TOT,bs="tp",k=6),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=20)+ti(date_dec,bs="tp",k=15)+ti(TOT,bs="tp",k=6),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=25)+ti(date_dec,bs="tp",k=15)+ti(TOT,bs="tp",k=6),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=25)+ti(date_dec,bs="tp",k=20)+ti(TOT,bs="tp",k=6),data=data,family=gaussian(link="log"))
gam.check(gamP) ## warning

gamP<-gam(chl~ti(doy,bs="cc",k=30)+ti(date_dec,bs="tp",k=15)+ti(TOT,bs="tp",k=6),data=data,family=gaussian(link="log"))
gam.check(gamP) ## warning

gamP<-gam(chl~ti(doy,bs="cc",k=28)+ti(date_dec,bs="tp",k=15)+ti(TOT,bs="tp",k=6),data=data,family=gaussian(link="log"))
gam.check(gamP) ## warning

gamP<-gam(chl~ti(doy,bs="cc",k=25)+ti(date_dec,bs="tp",k=15)+ti(TOT,bs="tp",k=6),data=data,family=gaussian(link="log"))
gam.check(gamP) 

gamP<-gam(chl~ti(doy,bs="cc",k=25)+ti(date_dec,bs="tp",k=18)+ti(TOT,bs="tp",k=6),data=data,family=gaussian(link="log"))
gam.check(gamP) 

gamP<-gam(chl~ti(doy,bs="cc",k=27)+ti(date_dec,bs="tp",k=18)+ti(TOT,bs="tp",k=6),data=data,family=gaussian(link="log"))
gam.check(gamP)  ## warning

gamP<-gam(chl~ti(doy,bs="cc",k=26)+ti(date_dec,bs="tp",k=18)+ti(TOT,bs="tp",k=6),data=data,family=gaussian(link="log"))
gam.check(gamP) 

gamP<-gam(chl~ti(doy,bs="cc",k=25)+ti(date_dec,bs="tp",k=18)+ti(TOT,bs="tp",k=6),data=data,family=gaussian(link="log"))
gam.check(gamP) ## best we can do, not great

perStationFlowTOT[[21]]=gamP
names(perStation)[21]
## skip specific

####

data=perStationAdd[[22]]
data$TOT= 0.028316847*data$TOT
#converted to m3/s

gamP<-gam(chl~ti(doy,bs="cc")+ti(date_dec,bs="tp")+ti(TOT,bs="tp"),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc")+ti(date_dec,bs="tp")+ti(TOT,bs="tp",k=10),data=data,family=gaussian(link="log"))
gam.check(gamP) ## warning

gamP<-gam(chl~ti(doy,bs="cc")+ti(date_dec,bs="tp")+ti(TOT,bs="tp",k=8),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc")+ti(date_dec,bs="tp",k=10)+ti(TOT,bs="tp",k=8),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=10)+ti(date_dec,bs="tp",k=10)+ti(TOT,bs="tp",k=8),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=20)+ti(date_dec,bs="tp",k=10)+ti(TOT,bs="tp",k=8),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=20)+ti(date_dec,bs="tp",k=20)+ti(TOT,bs="tp",k=8),data=data,family=gaussian(link="log"))
gam.check(gamP) ## warning


gamP<-bam(chl~ti(doy,bs="cc",k=50)+ti(date_dec,bs="tp",k=15)+ti(TOT,bs="tp",k=8),data=data,family=gaussian(link="log"),control=ctrl)
gam.check(gamP) ## warning

gamP<-bam(chl~ti(doy,bs="cc",k=30)+ti(date_dec,bs="tp",k=15)+ti(TOT,bs="tp",k=8),data=data,family=gaussian(link="log"),control=ctrl)
gam.check(gamP) ## warning

gamP<-bam(chl~ti(doy,bs="cc",k=25)+ti(date_dec,bs="tp",k=15)+ti(TOT,bs="tp",k=8),data=data,family=gaussian(link="log"),control=ctrl)
gam.check(gamP) ## warning

gamP<-gam(chl~ti(doy,bs="cc",k=20)+ti(date_dec,bs="tp",k=15)+ti(TOT,bs="tp",k=8),data=data,family=gaussian(link="log"))
gam.check(gamP)

perStationFlowTOT[[22]]=gamP
names(perStation)[22]
## skip station
####
data=perStationAdd[[23]]
data$TOT= 0.028316847*data$TOT
#converted to m3/s

gamP<-gam(chl~ti(doy,bs="cc")+ti(date_dec,bs="tp")+ti(TOT,bs="tp"),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc")+ti(date_dec,bs="tp")+ti(TOT,bs="tp",k=10),data=data,family=gaussian(link="log"))
gam.check(gamP) ## warning

gamP<-gam(chl~ti(doy,bs="cc")+ti(date_dec,bs="tp")+ti(TOT,bs="tp",k=8),data=data,family=gaussian(link="log"))
gam.check(gamP) ## warning

gamP<-gam(chl~ti(doy,bs="cc")+ti(date_dec,bs="tp")+ti(TOT,bs="tp",k=7),data=data,family=gaussian(link="log"))
gam.check(gamP)  ## warning

gamP<-gam(chl~ti(doy,bs="cc")+ti(date_dec,bs="tp")+ti(TOT,bs="tp",k=6),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=20)+ti(date_dec,bs="tp",k=15)+ti(TOT,bs="tp",k=6),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=20)+ti(date_dec,bs="tp",k=20)+ti(TOT,bs="tp",k=6),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=50)+ti(date_dec,bs="tp",k=20)+ti(TOT,bs="tp",k=6),data=data,family=gaussian(link="log"))
gam.check(gamP) ## warning

gamP<-gam(chl~ti(doy,bs="cc",k=30)+ti(date_dec,bs="tp",k=20)+ti(TOT,bs="tp",k=6),data=data,family=gaussian(link="log"))
gam.check(gamP) ## warning

gamP<-gam(chl~ti(doy,bs="cc",k=25)+ti(date_dec,bs="tp",k=20)+ti(TOT,bs="tp",k=6),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=25)+ti(date_dec,bs="tp",k=30)+ti(TOT,bs="tp",k=6),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=25)+ti(date_dec,bs="tp",k=40)+ti(TOT,bs="tp",k=6),data=data,family=gaussian(link="log"))
gam.check(gamP) ## warning

gamP<-bam(chl~ti(doy,bs="cc",k=25)+ti(date_dec,bs="tp",k=35)+ti(TOT,bs="tp",k=6),data=data,family=gaussian(link="log"),control=ctrl)
gam.check(gamP)  ## warning

gamP<-bam(chl~ti(doy,bs="cc",k=25)+ti(date_dec,bs="tp",k=30)+ti(TOT,bs="tp",k=6),data=data,family=gaussian(link="log"),control=ctrl)
gam.check(gamP) ## stuck with this

perStationFlowTOT[[23]]=gamP
names(perStation)[23]

data$SAC2=(data$RIO+data$YOLO)*0.028316847

#converted to m3/s

gamP<-gam(chl~ti(doy,bs="cc")+ti(date_dec,bs="tp")+ti(SAC2,bs="tp"),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc")+ti(date_dec,bs="tp")+ti(SAC2,bs="tp",k=10),data=data,family=gaussian(link="log"))
gam.check(gamP) ## warning

gamP<-gam(chl~ti(doy,bs="cc")+ti(date_dec,bs="tp")+ti(SAC2,bs="tp",k=8),data=data,family=gaussian(link="log"))
gam.check(gamP) ## warning

gamP<-gam(chl~ti(doy,bs="cc")+ti(date_dec,bs="tp")+ti(SAC2,bs="tp",k=7),data=data,family=gaussian(link="log"))
gam.check(gamP) ## warnings

gamP<-gam(chl~ti(doy,bs="cc")+ti(date_dec,bs="tp")+ti(SAC2,bs="tp",k=6),data=data,family=gaussian(link="log"))
gam.check(gamP) ## warning

gamP<-gam(chl~ti(doy,bs="cc",k=30)+ti(date_dec,bs="tp",k=15)+ti(SAC2,bs="tp"),data=data,family=gaussian(link="log"))
gam.check(gamP) 

gamP<-gam(chl~ti(doy,bs="cc",k=30)+ti(date_dec,bs="tp",k=20)+ti(SAC2,bs="tp"),data=data,family=gaussian(link="log"))
gam.check(gamP) 

gamP<-bam(chl~ti(doy,bs="cc",k=30)+ti(date_dec,bs="tp",k=30)+ti(SAC2,bs="tp"),data=data,family=gaussian(link="log"),control=ctrl)
gam.check(gamP) 

gamP<-bam(chl~ti(doy,bs="cc",k=50)+ti(date_dec,bs="tp",k=30)+ti(SAC2,bs="tp"),data=data,family=gaussian(link="log"),control=ctrl)
gam.check(gamP) 

gamP<-bam(chl~ti(doy,bs="cc",k=50)+ti(date_dec,bs="tp",k=40)+ti(SAC2,bs="tp"),data=data,family=gaussian(link="log"),control=ctrl)
gam.check(gamP) 

gamP<-bam(chl~ti(doy,bs="cc",k=50)+ti(date_dec,bs="tp",k=50)+ti(SAC2,bs="tp"),data=data,family=gaussian(link="log"),control=ctrl)
gam.check(gamP) 

gamP<-bam(chl~ti(doy,bs="cc",k=80)+ti(date_dec,bs="tp",k=50)+ti(SAC2,bs="tp"),data=data,family=gaussian(link="log"),control=ctrl)
gam.check(gamP) 

gamP<-bam(chl~ti(doy,bs="cc",k=80)+ti(date_dec,bs="tp",k=60)+ti(SAC2,bs="tp"),data=data,family=gaussian(link="log"),control=ctrl)
gam.check(gamP) 

gamP<-bam(chl~ti(doy,bs="cc",k=100)+ti(date_dec,bs="tp",k=60)+ti(SAC2,bs="tp"),data=data,family=gaussian(link="log"),control=ctrl)
gam.check(gamP) 

perStationFlowSpecific[[23]]=gamP

####
data=perStationAdd[[29]]
data$TOT= 0.028316847*data$TOT
#converted to m3/s

gamP<-gam(chl~ti(doy,bs="cc")+ti(date_dec,bs="tp")+ti(TOT,bs="tp"),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc")+ti(date_dec,bs="tp")+ti(TOT,bs="tp",k=10),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc")+ti(date_dec,bs="tp")+ti(TOT,bs="tp",k=8),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=30)+ti(date_dec,bs="tp",k=15)+ti(TOT,bs="tp",k=8),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=30)+ti(date_dec,bs="tp",k=25)+ti(TOT,bs="tp",k=8),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=50)+ti(date_dec,bs="tp",k=25)+ti(TOT,bs="tp",k=8),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=50)+ti(date_dec,bs="tp",k=35)+ti(TOT,bs="tp",k=8),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=80)+ti(date_dec,bs="tp",k=35)+ti(TOT,bs="tp",k=8),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=80)+ti(date_dec,bs="tp",k=45)+ti(TOT,bs="tp",k=8),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-bam(chl~ti(doy,bs="cc",k=100)+ti(date_dec,bs="tp",k=45)+ti(TOT,bs="tp",k=8),data=data,family=gaussian(link="log"),control=ctrl)
gam.check(gamP) ## warning

gamP<-bam(chl~ti(doy,bs="cc",k=90)+ti(date_dec,bs="tp",k=45)+ti(TOT,bs="tp",k=8),data=data,family=gaussian(link="log"),control=ctrl)
gam.check(gamP)  ## warning

gamP<-bam(chl~ti(doy,bs="cc",k=80)+ti(date_dec,bs="tp",k=50)+ti(TOT,bs="tp",k=8),data=data,family=gaussian(link="log"),control=ctrl)
gam.check(gamP) ## warning

gamP<-gam(chl~ti(doy,bs="cc",k=80)+ti(date_dec,bs="tp",k=45)+ti(TOT,bs="tp",k=8),data=data,family=gaussian(link="log"))
gam.check(gamP)


perStationFlowTOT[[29]]=gamP
names(perStation)[29]


data$EAST= 0.028316847*data$EAST
#converted to m3/s

gamP<-gam(chl~ti(doy,bs="cc")+ti(date_dec,bs="tp")+ti(EAST,bs="tp"),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc")+ti(date_dec,bs="tp")+ti(EAST,bs="tp",k=10),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc")+ti(date_dec,bs="tp")+ti(EAST,bs="tp",k=8),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=30)+ti(date_dec,bs="tp",k=15)+ti(EAST,bs="tp",k=8),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-bam(chl~ti(doy,bs="cc",k=30)+ti(date_dec,bs="tp",k=20)+ti(EAST,bs="tp",k=8),data=data,family=gaussian(link="log"),control=ctrl)
gam.check(gamP)

gamP<-bam(chl~ti(doy,bs="cc",k=50)+ti(date_dec,bs="tp",k=20)+ti(EAST,bs="tp",k=8),data=data,family=gaussian(link="log"),control=ctrl)
gam.check(gamP)

gamP<-bam(chl~ti(doy,bs="cc",k=50)+ti(date_dec,bs="tp",k=30)+ti(EAST,bs="tp",k=8),data=data,family=gaussian(link="log"),control=ctrl)
gam.check(gamP)

gamP<-bam(chl~ti(doy,bs="cc",k=50)+ti(date_dec,bs="tp",k=40)+ti(EAST,bs="tp",k=8),data=data,family=gaussian(link="log"),control=ctrl)
gam.check(gamP)

gamP<-bam(chl~ti(doy,bs="cc",k=80)+ti(date_dec,bs="tp",k=40)+ti(EAST,bs="tp",k=8),data=data,family=gaussian(link="log"),control=ctrl)
gam.check(gamP)

perStationFlowSpecific[[29]]=gamP
####
data=perStationAdd[[40]]
data$TOT= 0.028316847*data$TOT
#converted to m3/s

gamP<-gam(chl~ti(doy,bs="cc")+ti(date_dec,bs="tp")+ti(TOT,bs="tp"),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc")+ti(date_dec,bs="tp")+ti(TOT,bs="tp",k=10),data=data,family=gaussian(link="log"))
gam.check(gamP) ## warning

gamP<-gam(chl~ti(doy,bs="cc",k=20)+ti(date_dec,bs="tp",k=15)+ti(TOT,bs="tp",k=8),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=30)+ti(date_dec,bs="tp",k=25)+ti(TOT,bs="tp",k=8),data=data,family=gaussian(link="log"))
gam.check(gamP) ## warning

gamP<-gam(chl~ti(doy,bs="cc",k=30)+ti(date_dec,bs="tp",k=20)+ti(TOT,bs="tp",k=8),data=data,family=gaussian(link="log"))
gam.check(gamP) ## warnings

gamP<-gam(chl~ti(doy,bs="cc",k=30)+ti(date_dec,bs="tp",k=15)+ti(TOT,bs="tp",k=8),data=data,family=gaussian(link="log"))
gam.check(gamP) 

gamP<-gam(chl~ti(doy,bs="cc",k=50)+ti(date_dec,bs="tp",k=15)+ti(TOT,bs="tp",k=8),data=data,family=gaussian(link="log"))
gam.check(gamP) 

gamP<-gam(chl~ti(doy,bs="cc",k=50)+ti(date_dec,bs="tp",k=15)+ti(TOT,bs="tp",k=8),data=data,family=gaussian(link="log"))
gam.check(gamP)  ## warning

gamP<-gam(chl~ti(doy,bs="cc",k=40)+ti(date_dec,bs="tp",k=15)+ti(TOT,bs="tp",k=8),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-bam(chl~ti(doy,bs="cc",k=35)+ti(date_dec,bs="tp",k=15)+ti(TOT,bs="tp",k=8),data=data,family=gaussian(link="log"),control=ctrl)
gam.check(gamP) ## warning

gamP<-bam(chl~ti(doy,bs="cc",k=30)+ti(date_dec,bs="tp",k=18)+ti(TOT,bs="tp",k=8),data=data,family=gaussian(link="log"),control=ctrl)
gam.check(gamP) ## warning

gamP<-gam(chl~ti(doy,bs="cc",k=30)+ti(date_dec,bs="tp",k=15)+ti(TOT,bs="tp",k=8),data=data,family=gaussian(link="log"),control=ctrl)
gam.check(gamP) ## stuck here

perStationFlowTOT[[40]]=gamP
names(perStation)[40]

data$EAST= 0.028316847*data$EAST
#converted to m3/s

gamP<-gam(chl~ti(doy,bs="cc")+ti(date_dec,bs="tp")+ti(EAST,bs="tp"),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc")+ti(date_dec,bs="tp")+ti(EAST,bs="tp",k=10),data=data,family=gaussian(link="log"))
gam.check(gamP) ## warning

gamP<-gam(chl~ti(doy,bs="cc")+ti(date_dec,bs="tp")+ti(EAST,bs="tp",k=8),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=30)+ti(date_dec,bs="tp",k=15)+ti(EAST,bs="tp",k=8),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=30)+ti(date_dec,bs="tp",k=20)+ti(EAST,bs="tp",k=8),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-bam(chl~ti(doy,bs="cc",k=50)+ti(date_dec,bs="tp",k=20)+ti(EAST,bs="tp",k=8),data=data,family=gaussian(link="log"),control=ctrl)
gam.check(gamP) ## warning

gamP<-bam(chl~ti(doy,bs="cc",k=30)+ti(date_dec,bs="tp",k=30)+ti(EAST,bs="tp",k=8),data=data,family=gaussian(link="log"),control=ctrl)
gam.check(gamP) ## warning

gamP<-bam(chl~ti(doy,bs="cc",k=30)+ti(date_dec,bs="tp",k=20)+ti(EAST,bs="tp",k=8),data=data,family=gaussian(link="log"),control=ctrl)
gam.check(gamP) ## warning

gamP<-gam(chl~ti(doy,bs="cc",k=30)+ti(date_dec,bs="tp",k=20)+ti(EAST,bs="tp",k=8),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=50)+ti(date_dec,bs="tp",k=20)+ti(EAST,bs="tp",k=8),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=50)+ti(date_dec,bs="tp",k=30)+ti(EAST,bs="tp",k=8),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=80)+ti(date_dec,bs="tp",k=30)+ti(EAST,bs="tp",k=8),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-bam(chl~ti(doy,bs="cc",k=80)+ti(date_dec,bs="tp",k=40)+ti(EAST,bs="tp",k=8),data=data,family=gaussian(link="log"),control=ctrl)
gam.check(gamP) ## warning

gamP<-bam(chl~ti(doy,bs="cc",k=80)+ti(date_dec,bs="tp",k=20)+ti(EAST,bs="tp",k=8),data=data,family=gaussian(link="log"),control=ctrl)
gam.check(gamP) ## warning

gamP<-bam(chl~ti(doy,bs="cc",k=30)+ti(date_dec,bs="tp",k=20)+ti(EAST,bs="tp",k=8),data=data,family=gaussian(link="log"),control=ctrl)
gam.check(gamP)

gamP<-bam(chl~ti(doy,bs="cc",k=20)+ti(date_dec,bs="tp",k=20)+ti(EAST,bs="tp",k=8),data=data,family=gaussian(link="log"),control=ctrl)
gam.check(gamP)

gamP<-bam(chl~ti(doy,bs="cc",k=20)+ti(date_dec,bs="tp",k=20)+ti(EAST,bs="tp",k=8),data=data,family=gaussian(link="log"),control=ctrl)
gam.check(gamP)

gamP<-bam(chl~ti(doy,bs="cc",k=20)+ti(date_dec,bs="tp",k=15)+ti(EAST,bs="tp",k=8),data=data,family=gaussian(link="log"),control=ctrl)
gam.check(gamP)

gamP<-bam(chl~ti(doy,bs="cc")+ti(date_dec,bs="tp")+ti(EAST,bs="tp"),data=data,family=gaussian(link="log"),control=ctrl)
gam.check(gamP)

gamP<-bam(chl~ti(doy,bs="cc")+ti(date_dec,bs="tp")+ti(EAST,bs="tp",k=10),data=data,family=gaussian(link="log"),control=ctrl)
gam.check(gamP) ## warning

gamP<-bam(chl~ti(doy,bs="cc")+ti(date_dec,bs="tp")+ti(EAST,bs="tp",k=8),data=data,family=gaussian(link="log"),control=ctrl)
gam.check(gamP)

gamP<-bam(chl~ti(doy,bs="cc")+ti(date_dec,bs="tp")+ti(EAST,bs="tp",k=7),data=data,family=gaussian(link="log"),control=ctrl)
gam.check(gamP)

gamP<-bam(chl~ti(doy,bs="cc",k=20)+ti(date_dec,bs="tp",k=15)+ti(EAST,bs="tp",k=7),data=data,family=gaussian(link="log"),control=ctrl)
gam.check(gamP)

gamP<-bam(chl~ti(doy,bs="cc",k=40)+ti(date_dec,bs="tp",k=25)+ti(EAST,bs="tp",k=7),data=data,family=gaussian(link="log"),control=ctrl)
gam.check(gamP)

gamP<-bam(chl~ti(doy,bs="cc",k=40)+ti(date_dec,bs="tp",k=30)+ti(EAST,bs="tp",k=7),data=data,family=gaussian(link="log"),control=ctrl)
gam.check(gamP)

gamP<-bam(chl~ti(doy,bs="cc",k=80)+ti(date_dec,bs="tp",k=30)+ti(EAST,bs="tp",k=7),data=data,family=gaussian(link="log"),control=ctrl)
gam.check(gamP)

gamP<-bam(chl~ti(doy,bs="cc",k=80)+ti(date_dec,bs="tp",k=40)+ti(EAST,bs="tp",k=7),data=data,family=gaussian(link="log"),control=ctrl)
gam.check(gamP)

gamP<-bam(chl~ti(doy,bs="cc",k=100)+ti(date_dec,bs="tp",k=40)+ti(EAST,bs="tp",k=7),data=data,family=gaussian(link="log"),control=ctrl)
gam.check(gamP)

perStationFlowSpecific[[40]]=gamP

save(perStationFlowSpecific,file="perStationFlowSpecific.Rda")
save(perStationFlowTOT,file="perStationFlowTOT.Rda")

setwd("~/Desktop/sfei")
load("perStationFlowTOT.Rda")
load("perStationFlowSpecific.Rda")
load("perStationAdd.Rda")

rmseFlow<-c()
for(i in wholeSeries){
  perStationAdd[[i]]$TOT= 0.028316847*perStationAdd[[i]]$TOT
  predVal=predict(perStationFlowTOT[[i]],perStationAdd[[i]])
  rmseFlow<-c(rmseFlow,sqrt(sum(predVal^2)/length(predVal)))
  print(i)
}
rmseFlow ##


for(i in wholeSeries){
  perStationAdd[[i]]$TOT= 0.028316847*perStationAdd[[i]]$TOT
  predVal=predict(perStationFlowTOT[[i]],perStationAdd[[i]])
  perStationAdd[[i]]$TOT=perStationAdd[[i]]$TOT/0.028316847 ## get back to normal
  perStationAdd[[i]]$flowPred=predVal
  print(i)
}

save(perStationAdd,file="perStationAdd.Rda")

rmseFlowSpecific<-c()
for(i in wholeSeries){
  perStationAdd[[i]]$EAST= 0.028316847*perStationAdd[[i]]$EAST
  perStationAdd[[i]]$SAC2=(perStationAdd[[i]]$RIO+perStationAdd[[i]]$YOLO)*0.028316847
  if(!is.null(perStationFlowSpecific[[i]])){
  predVal=predict(perStationFlowSpecific[[i]],perStationAdd[[i]])
  rmseFlowSpecific<-c(rmseFlowSpecific,sqrt(sum(predVal^2)/length(predVal)))
  }else{
    rmseFlowSpecific<-c(rmseFlowSpecific,NA)
  }
  print(i)
}

rmseFlowSpecific

cbind(rmseFlow,rmseFlowSpecific)
## when rmse is bad for flow, often better for rmseFlowSpecific
## when rmse is decent for flow, rmseFlowSpecific worse

cbind(names(perStation)[wholeSeries],rmseFlow,rmseFlowSpecific)
## D7, D22, D28A, D26, P8 pretty large for flow

names(testMerge5)
View(cbind(testMerge5,rmseFlow,rmseFlowSpecific))

test=cbind(testMerge5,rmseFlow,rmseFlowSpecific)



g1<-ggplot(test,aes(x = Longitude, y = Latitude,colour=rmseInt,cex=2))+geom_point()+
  ggtitle("Interaction RMSE by Station")+scale_size(guide=F)

g2<-ggplot(test,aes(x = Longitude, y = Latitude,colour=rmseFlow,cex=2))+geom_point()+
  ggtitle("Flow RMSE by Station")+scale_size(guide=F)

g3<-ggplot(test,aes(x = Longitude, y = Latitude,colour=rmseFlowSpecific,cex=2))+geom_point()+
  ggtitle("Flow Specific RMSE by Station")+scale_size(guide=F)

g4<-ggplot(test,aes(x = Longitude, y = Latitude,colour=rmsePars,cex=2))+geom_point()+
  ggtitle("Parsimonious Model RMSE Intercepts by Station")+scale_size(guide=F)

grid.arrange(g1,g2,g3,g4)

## can be better at places, but other covariates are better
## let's see graphically what is going on

which(testMerge5$rmseInt<testMerge5$rmsePars)

length(which(testMerge5$rmseFull<testMerge5$rmsePars))
nrow(testMerge5)
## 12/15

which(testMerge5$rmseInt<rmseFlow) ## 10/15
which(testMerge5$rmseInt<rmseFlowSpecific) ## 6/15

testMerge5$Station[-which(testMerge5$rmseInt<rmseFlow)]

cbind(rmse,rmseFlow)
which(rmse<rmseFlow)
