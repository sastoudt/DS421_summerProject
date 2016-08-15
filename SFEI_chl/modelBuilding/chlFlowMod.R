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

#### 

data=perStationAdd[[5]]

gamP<-gam(chl~ti(doy,bs="cc")+ti(date_dec,bs="tp")+ti(i.chl,bs="tp"),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=10)+ti(date_dec,bs="tp",k=10)+ti(i.chl,bs="tp",k=10),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=20)+ti(date_dec,bs="tp",k=30)+ti(i.chl,bs="tp",k=12),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-bam(chl~ti(doy,bs="cc",k=30)+ti(date_dec,bs="tp",k=40)+ti(i.chl,bs="tp",k=12),data=data,family=gaussian(link="log"),control=ctrl)
gam.check(gamP)

gamP<-bam(chl~ti(doy,bs="cc",k=40)+ti(date_dec,bs="tp",k=50)+ti(i.chl,bs="tp",k=12),data=data,family=gaussian(link="log"),control=ctrl)
gam.check(gamP)

perStationFlowMod[[5]]=gamP

####

data=perStationAdd[[7]]

gamP<-gam(chl~ti(doy,bs="cc")+ti(date_dec,bs="tp")+ti(i.chl,bs="tp"),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=10)+ti(date_dec,bs="tp",k=10)+ti(i.chl,bs="tp",k=10),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=15)+ti(date_dec,bs="tp",k=30)+ti(i.chl,bs="tp",k=12),data=data,family=gaussian(link="log"))
gam.check(gamP) ## warning

gamP<-gam(chl~ti(doy,bs="cc",k=15)+ti(date_dec,bs="tp",k=10)+ti(i.chl,bs="tp",k=12),data=data,family=gaussian(link="log"))
gam.check(gamP) 

gamP<-gam(chl~ti(doy,bs="cc",k=15)+ti(date_dec,bs="tp",k=20)+ti(i.chl,bs="tp",k=12),data=data,family=gaussian(link="log"))
gam.check(gamP) 

gamP<-gam(chl~ti(doy,bs="cc",k=20)+ti(date_dec,bs="tp",k=25)+ti(i.chl,bs="tp",k=12),data=data,family=gaussian(link="log"))
gam.check(gamP) ## warning

gamP<-gam(chl~ti(doy,bs="cc",k=20)+ti(date_dec,bs="tp",k=22)+ti(i.chl,bs="tp",k=12),data=data,family=gaussian(link="log"))
gam.check(gamP) 

gamP<-gam(chl~ti(doy,bs="cc",k=20)+ti(date_dec,bs="tp",k=23)+ti(i.chl,bs="tp",k=12),data=data,family=gaussian(link="log"))
gam.check(gamP)  ## warning

gamP<-gam(chl~ti(doy,bs="cc",k=20)+ti(date_dec,bs="tp",k=22)+ti(i.chl,bs="tp",k=12),data=data,family=gaussian(link="log"))
gam.check(gamP) ## best we can do

perStationFlowMod[[7]]=gamP

####
data=perStationAdd[[11]]

gamP<-gam(chl~ti(doy,bs="cc")+ti(date_dec,bs="tp")+ti(i.chl,bs="tp"),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=10)+ti(date_dec,bs="tp",k=10)+ti(i.chl,bs="tp",k=10),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=15)+ti(date_dec,bs="tp",k=10)+ti(i.chl,bs="tp",k=20),data=data,family=gaussian(link="log"))
gam.check(gamP) ## warning

gamP<-gam(chl~ti(doy,bs="cc",k=15)+ti(date_dec,bs="tp",k=10)+ti(i.chl,bs="tp",k=10),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=15)+ti(date_dec,bs="tp",k=10)+ti(i.chl,bs="tp",k=12),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=15)+ti(date_dec,bs="tp",k=10)+ti(i.chl,bs="tp",k=15),data=data,family=gaussian(link="log"))
gam.check(gamP) ## warning

gamP<-gam(chl~ti(doy,bs="cc",k=15)+ti(date_dec,bs="tp",k=10)+ti(i.chl,bs="tp",k=13),data=data,family=gaussian(link="log"))
gam.check(gamP) 

gamP<-gam(chl~ti(doy,bs="cc",k=15)+ti(date_dec,bs="tp",k=10)+ti(i.chl,bs="tp",k=14),data=data,family=gaussian(link="log"))
gam.check(gamP)  ## warning

gamP<-gam(chl~ti(doy,bs="cc",k=15)+ti(date_dec,bs="tp",k=10)+ti(i.chl,bs="tp",k=13),data=data,family=gaussian(link="log"))
gam.check(gamP) 

perStationFlowMod[[11]]=gamP

####

data=perStationAdd[[13]]

gamP<-gam(chl~ti(doy,bs="cc")+ti(date_dec,bs="tp")+ti(i.chl,bs="tp"),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=10)+ti(date_dec,bs="tp",k=10)+ti(i.chl,bs="tp",k=10),data=data,family=gaussian(link="log"))
gam.check(gamP) ## warning

gamP<-gam(chl~ti(doy,bs="cc")+ti(date_dec,bs="tp",k=10)+ti(i.chl,bs="tp"),data=data,family=gaussian(link="log"))
gam.check(gamP) 

gamP<-gam(chl~ti(doy,bs="cc",k=10)+ti(date_dec,bs="tp",k=10)+ti(i.chl,bs="tp"),data=data,family=gaussian(link="log"))
gam.check(gamP) 

gamP<-gam(chl~ti(doy,bs="cc",k=10)+ti(date_dec,bs="tp",k=10)+ti(i.chl,bs="tp",k=8),data=data,family=gaussian(link="log"))
gam.check(gamP)  ## warning
 

gamP<-gam(chl~ti(doy,bs="cc",k=10)+ti(date_dec,bs="tp",k=10)+ti(i.chl,bs="tp",k=7),data=data,family=gaussian(link="log"))
gam.check(gamP) 

gamP<-gam(chl~ti(doy,bs="cc",k=10)+ti(date_dec,bs="tp",k=20)+ti(i.chl,bs="tp",k=7),data=data,family=gaussian(link="log"))
gam.check(gamP) 

gamP<-gam(chl~ti(doy,bs="cc",k=15)+ti(date_dec,bs="tp",k=30)+ti(i.chl,bs="tp",k=7),data=data,family=gaussian(link="log"))
gam.check(gamP)  ## warning

gamP<-gam(chl~ti(doy,bs="cc",k=15)+ti(date_dec,bs="tp",k=20)+ti(i.chl,bs="tp",k=7),data=data,family=gaussian(link="log"))
gam.check(gamP) 

gamP<-gam(chl~ti(doy,bs="cc",k=15)+ti(date_dec,bs="tp",k=25)+ti(i.chl,bs="tp",k=7),data=data,family=gaussian(link="log"))
gam.check(gamP)  ## warning

gamP<-gam(chl~ti(doy,bs="cc",k=15)+ti(date_dec,bs="tp",k=22)+ti(i.chl,bs="tp",k=7),data=data,family=gaussian(link="log"))
gam.check(gamP)  

gamP<-gam(chl~ti(doy,bs="cc",k=25)+ti(date_dec,bs="tp",k=22)+ti(i.chl,bs="tp",k=7),data=data,family=gaussian(link="log"))
gam.check(gamP)  

gamP<-gam(chl~ti(doy,bs="cc",k=30)+ti(date_dec,bs="tp",k=22)+ti(i.chl,bs="tp",k=7),data=data,family=gaussian(link="log"))
gam.check(gamP)  ## best we can do

perStationFlowMod[[13]]=gamP

####
data=perStationAdd[[15]]

gamP<-gam(chl~ti(doy,bs="cc")+ti(date_dec,bs="tp")+ti(i.chl,bs="tp"),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=10)+ti(date_dec,bs="tp",k=10)+ti(i.chl,bs="tp",k=10),data=data,family=gaussian(link="log"))
gam.check(gamP) ## warning

gamP<-gam(chl~ti(doy,bs="cc",k=10)+ti(date_dec,bs="tp")+ti(i.chl,bs="tp"),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=10)+ti(date_dec,bs="tp",k=10)+ti(i.chl,bs="tp"),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=10)+ti(date_dec,bs="tp",k=10)+ti(i.chl,bs="tp",k=8),data=data,family=gaussian(link="log"))
gam.check(gamP) ## warning

gamP<-gam(chl~ti(doy,bs="cc",k=10)+ti(date_dec,bs="tp",k=10)+ti(i.chl,bs="tp",k=7),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=15)+ti(date_dec,bs="tp",k=20)+ti(i.chl,bs="tp",k=7),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=25)+ti(date_dec,bs="tp",k=30)+ti(i.chl,bs="tp",k=7),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=40)+ti(date_dec,bs="tp",k=50)+ti(i.chl,bs="tp",k=7),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-bam(chl~ti(doy,bs="cc",k=50)+ti(date_dec,bs="tp",k=60)+ti(i.chl,bs="tp",k=7),data=data,family=gaussian(link="log"),control=ctrl)
gam.check(gamP)

perStationFlowMod[[15]]=gamP

####
data=perStationAdd[[16]]

gamP<-gam(chl~ti(doy,bs="cc")+ti(date_dec,bs="tp")+ti(i.chl,bs="tp"),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=10)+ti(date_dec,bs="tp",k=10)+ti(i.chl,bs="tp",k=10),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=15)+ti(date_dec,bs="tp",k=15)+ti(i.chl,bs="tp",k=15),data=data,family=gaussian(link="log"))
gam.check(gamP) ## warning

gamP<-gam(chl~ti(doy,bs="cc",k=15)+ti(date_dec,bs="tp",k=15)+ti(i.chl,bs="tp",k=12),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=25)+ti(date_dec,bs="tp",k=30)+ti(i.chl,bs="tp",k=12),data=data,family=gaussian(link="log"))
gam.check(gamP) ## warning

gamP<-gam(chl~ti(doy,bs="cc",k=20)+ti(date_dec,bs="tp",k=20)+ti(i.chl,bs="tp",k=12),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=25)+ti(date_dec,bs="tp",k=20)+ti(i.chl,bs="tp",k=12),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=25)+ti(date_dec,bs="tp",k=25)+ti(i.chl,bs="tp",k=12),data=data,family=gaussian(link="log"))
gam.check(gamP) ## warning

gamP<-gam(chl~ti(doy,bs="cc",k=25)+ti(date_dec,bs="tp",k=23)+ti(i.chl,bs="tp",k=12),data=data,family=gaussian(link="log"))
gam.check(gamP) ## warning

gamP<-gam(chl~ti(doy,bs="cc",k=25)+ti(date_dec,bs="tp",k=22)+ti(i.chl,bs="tp",k=12),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=35)+ti(date_dec,bs="tp",k=22)+ti(i.chl,bs="tp",k=12),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=45)+ti(date_dec,bs="tp",k=22)+ti(i.chl,bs="tp",k=12),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=55)+ti(date_dec,bs="tp",k=22)+ti(i.chl,bs="tp",k=12),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-bam(chl~ti(doy,bs="cc",k=65)+ti(date_dec,bs="tp",k=22)+ti(i.chl,bs="tp",k=12),data=data,family=gaussian(link="log"),control=ctrl)
gam.check(gamP)

perStationFlowMod[[16]]=gamP

####
data=perStationAdd[[17]]

gamP<-gam(chl~ti(doy,bs="cc")+ti(date_dec,bs="tp")+ti(i.chl,bs="tp"),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=10)+ti(date_dec,bs="tp",k=10)+ti(i.chl,bs="tp",k=10),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=15)+ti(date_dec,bs="tp",k=15)+ti(i.chl,bs="tp",k=15),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=20)+ti(date_dec,bs="tp",k=30)+ti(i.chl,bs="tp",k=15),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=20)+ti(date_dec,bs="tp",k=40)+ti(i.chl,bs="tp",k=15),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-bam(chl~ti(doy,bs="cc",k=30)+ti(date_dec,bs="tp",k=50)+ti(i.chl,bs="tp",k=15),data=data,family=gaussian(link="log"),control=ctrl)
gam.check(gamP)

perStationFlowMod[[17]]=gamP

####
data=perStationAdd[[18]]

gamP<-gam(chl~ti(doy,bs="cc")+ti(date_dec,bs="tp")+ti(i.chl,bs="tp"),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=10)+ti(date_dec,bs="tp",k=10)+ti(i.chl,bs="tp",k=10),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=15)+ti(date_dec,bs="tp",k=15)+ti(i.chl,bs="tp",k=15),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=20)+ti(date_dec,bs="tp",k=20)+ti(i.chl,bs="tp",k=20),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=25)+ti(date_dec,bs="tp",k=25)+ti(i.chl,bs="tp",k=25),data=data,family=gaussian(link="log"))
gam.check(gamP) ## warning

gamP<-gam(chl~ti(doy,bs="cc",k=20)+ti(date_dec,bs="tp",k=25)+ti(i.chl,bs="tp",k=20),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=20)+ti(date_dec,bs="tp",k=25)+ti(i.chl,bs="tp",k=25),data=data,family=gaussian(link="log"))
gam.check(gamP) ## warning

gamP<-gam(chl~ti(doy,bs="cc",k=20)+ti(date_dec,bs="tp",k=25)+ti(i.chl,bs="tp",k=22),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=20)+ti(date_dec,bs="tp",k=30)+ti(i.chl,bs="tp",k=22),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-bam(chl~ti(doy,bs="cc",k=20)+ti(date_dec,bs="tp",k=50)+ti(i.chl,bs="tp",k=22),data=data,family=gaussian(link="log"),control=ctrl)
gam.check(gamP)

gamP<-bam(chl~ti(doy,bs="cc",k=25)+ti(date_dec,bs="tp",k=60)+ti(i.chl,bs="tp",k=22),data=data,family=gaussian(link="log"),control=ctrl)
gam.check(gamP)

perStationFlowMod[[18]]=gamP

####