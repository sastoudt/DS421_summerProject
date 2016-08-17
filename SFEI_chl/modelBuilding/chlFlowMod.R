### using chl from another station as "flow" variable

setwd("~/Desktop/sfei")
load(file="perStationAdd.Rda")
require(mgcv)

perStationFlowMod=vector("list",length(perStationAdd))
wholeSeries<-c(1, 2, 5, 7, 11, 13, 15, 16, 17, 18, 21, 22, 23, 29, 40)
ctrl <- list(nthreads=4)
data=perStationAdd[[1]]

gamP<-gam(chl~ti(doy,bs="cc")+ti(date_dec,bs="tp")+ti(chl.1,bs="tp"),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=10)+ti(date_dec,bs="tp",k=10)+ti(chl.1,bs="tp",k=10),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=10)+ti(date_dec,bs="tp",k=20)+ti(chl.1,bs="tp",k=10),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=15)+ti(date_dec,bs="tp",k=30)+ti(chl.1,bs="tp",k=15),data=data,family=gaussian(link="log"))
gam.check(gamP) ## warning

gamP<-gam(chl~ti(doy,bs="cc",k=15)+ti(date_dec,bs="tp",k=20)+ti(chl.1,bs="tp",k=10),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=15)+ti(date_dec,bs="tp",k=20)+ti(chl.1,bs="tp",k=15),data=data,family=gaussian(link="log"))
gam.check(gamP) ## warning

gamP<-gam(chl~ti(doy,bs="cc",k=15)+ti(date_dec,bs="tp",k=20)+ti(chl.1,bs="tp",k=12),data=data,family=gaussian(link="log"))
gam.check(gamP) 

gamP<-bam(chl~ti(doy,bs="cc",k=15)+ti(date_dec,bs="tp",k=30)+ti(chl.1,bs="tp",k=12),data=data,family=gaussian(link="log"),control=ctrl)
gam.check(gamP)

gamP<-bam(chl~ti(doy,bs="cc",k=20)+ti(date_dec,bs="tp",k=40)+ti(chl.1,bs="tp",k=12),data=data,family=gaussian(link="log"),control=ctrl)
gam.check(gamP)

gamP<-bam(chl~ti(doy,bs="cc",k=20)+ti(date_dec,bs="tp",k=50)+ti(chl.1,bs="tp",k=12),data=data,family=gaussian(link="log"),control=ctrl)
gam.check(gamP)

gamP<-bam(chl~ti(doy,bs="cc",k=20)+ti(date_dec,bs="tp",k=60)+ti(chl.1,bs="tp",k=12),data=data,family=gaussian(link="log"),control=ctrl)
gam.check(gamP)

perStationFlowMod[[1]]=gamP
####

data=perStationAdd[[2]]

gamP<-gam(chl~ti(doy,bs="cc")+ti(date_dec,bs="tp")+ti(chl.1,bs="tp"),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=10)+ti(date_dec,bs="tp",k=10)+ti(chl.1,bs="tp",k=10),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=15)+ti(date_dec,bs="tp",k=10)+ti(chl.1,bs="tp",k=15),data=data,family=gaussian(link="log"))
gam.check(gamP) ## warning

gamP<-gam(chl~ti(doy,bs="cc",k=15)+ti(date_dec,bs="tp",k=20)+ti(chl.1,bs="tp",k=12),data=data,family=gaussian(link="log"))
gam.check(gamP) 

gamP<-bam(chl~ti(doy,bs="cc",k=15)+ti(date_dec,bs="tp",k=30)+ti(chl.1,bs="tp",k=12),data=data,family=gaussian(link="log"),control=ctrl)
gam.check(gamP) 

gamP<-bam(chl~ti(doy,bs="cc",k=15)+ti(date_dec,bs="tp",k=40)+ti(chl.1,bs="tp",k=12),data=data,family=gaussian(link="log"),control=ctrl)
gam.check(gamP) 

gamP<-bam(chl~ti(doy,bs="cc",k=15)+ti(date_dec,bs="tp",k=50)+ti(chl.1,bs="tp",k=12),data=data,family=gaussian(link="log"),control=ctrl)
gam.check(gamP) 

gamP<-bam(chl~ti(doy,bs="cc",k=20)+ti(date_dec,bs="tp",k=50)+ti(chl.1,bs="tp",k=12),data=data,family=gaussian(link="log"),control=ctrl)
gam.check(gamP) 

gamP<-bam(chl~ti(doy,bs="cc",k=20)+ti(date_dec,bs="tp",k=60)+ti(chl.1,bs="tp",k=12),data=data,family=gaussian(link="log"),control=ctrl)
gam.check(gamP) 

gamP<-bam(chl~ti(doy,bs="cc",k=20)+ti(date_dec,bs="tp",k=80)+ti(chl.1,bs="tp",k=12),data=data,family=gaussian(link="log"),control=ctrl)
gam.check(gamP)  ## warning

gamP<-bam(chl~ti(doy,bs="cc",k=20)+ti(date_dec,bs="tp",k=70)+ti(chl.1,bs="tp",k=12),data=data,family=gaussian(link="log"),control=ctrl)
gam.check(gamP) 

perStationFlowMod[[2]]=gamP

#### 

data=perStationAdd[[5]]

gamP<-gam(chl~ti(doy,bs="cc")+ti(date_dec,bs="tp")+ti(chl.1,bs="tp"),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=10)+ti(date_dec,bs="tp",k=10)+ti(chl.1,bs="tp",k=10),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=20)+ti(date_dec,bs="tp",k=30)+ti(chl.1,bs="tp",k=12),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-bam(chl~ti(doy,bs="cc",k=30)+ti(date_dec,bs="tp",k=40)+ti(chl.1,bs="tp",k=12),data=data,family=gaussian(link="log"),control=ctrl)
gam.check(gamP)

gamP<-bam(chl~ti(doy,bs="cc",k=40)+ti(date_dec,bs="tp",k=50)+ti(chl.1,bs="tp",k=12),data=data,family=gaussian(link="log"),control=ctrl)
gam.check(gamP)

gamP<-bam(chl~ti(doy,bs="cc",k=40)+ti(date_dec,bs="tp",k=60)+ti(chl.1,bs="tp",k=12),data=data,family=gaussian(link="log"),control=ctrl)
gam.check(gamP)

perStationFlowMod[[5]]=gamP

####

data=perStationAdd[[7]]

gamP<-gam(chl~ti(doy,bs="cc")+ti(date_dec,bs="tp")+ti(chl.1,bs="tp"),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=10)+ti(date_dec,bs="tp",k=10)+ti(chl.1,bs="tp",k=10),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=15)+ti(date_dec,bs="tp",k=30)+ti(chl.1,bs="tp",k=12),data=data,family=gaussian(link="log"))
gam.check(gamP) ## warning

gamP<-gam(chl~ti(doy,bs="cc",k=15)+ti(date_dec,bs="tp",k=10)+ti(chl.1,bs="tp",k=12),data=data,family=gaussian(link="log"))
gam.check(gamP) 

gamP<-gam(chl~ti(doy,bs="cc",k=15)+ti(date_dec,bs="tp",k=20)+ti(chl.1,bs="tp",k=12),data=data,family=gaussian(link="log"))
gam.check(gamP) 

gamP<-gam(chl~ti(doy,bs="cc",k=20)+ti(date_dec,bs="tp",k=25)+ti(chl.1,bs="tp",k=12),data=data,family=gaussian(link="log"))
gam.check(gamP) ## warning

gamP<-gam(chl~ti(doy,bs="cc",k=20)+ti(date_dec,bs="tp",k=22)+ti(chl.1,bs="tp",k=12),data=data,family=gaussian(link="log"))
gam.check(gamP) 

gamP<-gam(chl~ti(doy,bs="cc",k=20)+ti(date_dec,bs="tp",k=23)+ti(chl.1,bs="tp",k=12),data=data,family=gaussian(link="log"))
gam.check(gamP)  ## warning

gamP<-gam(chl~ti(doy,bs="cc",k=20)+ti(date_dec,bs="tp",k=22)+ti(chl.1,bs="tp",k=12),data=data,family=gaussian(link="log"))
gam.check(gamP) ## best we can do

gamP<-gam(chl~ti(doy,bs="cc",k=20)+ti(date_dec,bs="tp",k=22)+ti(chl.1,bs="tp",k=15),data=data,family=gaussian(link="log"))
gam.check(gamP) ## warning

gamP<-gam(chl~ti(doy,bs="cc",k=20)+ti(date_dec,bs="tp",k=25)+ti(chl.1,bs="tp",k=15),data=data,family=gaussian(link="log"))
gam.check(gamP) ## warning

gamP<-gam(chl~ti(doy,bs="cc",k=30)+ti(date_dec,bs="tp",k=22)+ti(chl.1,bs="tp",k=12),data=data,family=gaussian(link="log"))
gam.check(gamP) 

gamP<-gam(chl~ti(doy,bs="cc",k=40)+ti(date_dec,bs="tp",k=22)+ti(chl.1,bs="tp",k=12),data=data,family=gaussian(link="log"))
gam.check(gamP) 

perStationFlowMod[[7]]=gamP

####
data=perStationAdd[[11]]

gamP<-gam(chl~ti(doy,bs="cc")+ti(date_dec,bs="tp")+ti(chl.1,bs="tp"),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=10)+ti(date_dec,bs="tp",k=10)+ti(chl.1,bs="tp",k=10),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=15)+ti(date_dec,bs="tp",k=10)+ti(chl.1,bs="tp",k=20),data=data,family=gaussian(link="log"))
gam.check(gamP) ## warning

gamP<-gam(chl~ti(doy,bs="cc",k=15)+ti(date_dec,bs="tp",k=10)+ti(chl.1,bs="tp",k=10),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=15)+ti(date_dec,bs="tp",k=10)+ti(chl.1,bs="tp",k=12),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=15)+ti(date_dec,bs="tp",k=10)+ti(chl.1,bs="tp",k=15),data=data,family=gaussian(link="log"))
gam.check(gamP) ## warning

gamP<-gam(chl~ti(doy,bs="cc",k=15)+ti(date_dec,bs="tp",k=10)+ti(chl.1,bs="tp",k=13),data=data,family=gaussian(link="log"))
gam.check(gamP) 

gamP<-gam(chl~ti(doy,bs="cc",k=15)+ti(date_dec,bs="tp",k=10)+ti(chl.1,bs="tp",k=14),data=data,family=gaussian(link="log"))
gam.check(gamP)  ## warning

gamP<-gam(chl~ti(doy,bs="cc",k=15)+ti(date_dec,bs="tp",k=10)+ti(chl.1,bs="tp",k=13),data=data,family=gaussian(link="log"))
gam.check(gamP) 

perStationFlowMod[[11]]=gamP

####

data=perStationAdd[[13]]

gamP<-gam(chl~ti(doy,bs="cc")+ti(date_dec,bs="tp")+ti(chl.1,bs="tp"),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=10)+ti(date_dec,bs="tp",k=10)+ti(chl.1,bs="tp",k=10),data=data,family=gaussian(link="log"))
gam.check(gamP) ## warning

gamP<-gam(chl~ti(doy,bs="cc")+ti(date_dec,bs="tp",k=10)+ti(chl.1,bs="tp"),data=data,family=gaussian(link="log"))
gam.check(gamP) 

gamP<-gam(chl~ti(doy,bs="cc",k=10)+ti(date_dec,bs="tp",k=10)+ti(chl.1,bs="tp"),data=data,family=gaussian(link="log"))
gam.check(gamP) 

gamP<-gam(chl~ti(doy,bs="cc",k=10)+ti(date_dec,bs="tp",k=10)+ti(chl.1,bs="tp",k=8),data=data,family=gaussian(link="log"))
gam.check(gamP)  ## warning
 

gamP<-gam(chl~ti(doy,bs="cc",k=10)+ti(date_dec,bs="tp",k=10)+ti(chl.1,bs="tp",k=7),data=data,family=gaussian(link="log"))
gam.check(gamP) 

gamP<-gam(chl~ti(doy,bs="cc",k=10)+ti(date_dec,bs="tp",k=20)+ti(chl.1,bs="tp",k=7),data=data,family=gaussian(link="log"))
gam.check(gamP) 

gamP<-gam(chl~ti(doy,bs="cc",k=15)+ti(date_dec,bs="tp",k=30)+ti(chl.1,bs="tp",k=7),data=data,family=gaussian(link="log"))
gam.check(gamP)  ## warning

gamP<-gam(chl~ti(doy,bs="cc",k=15)+ti(date_dec,bs="tp",k=20)+ti(chl.1,bs="tp",k=7),data=data,family=gaussian(link="log"))
gam.check(gamP) 

gamP<-gam(chl~ti(doy,bs="cc",k=15)+ti(date_dec,bs="tp",k=25)+ti(chl.1,bs="tp",k=7),data=data,family=gaussian(link="log"))
gam.check(gamP)  ## warning

gamP<-gam(chl~ti(doy,bs="cc",k=15)+ti(date_dec,bs="tp",k=22)+ti(chl.1,bs="tp",k=7),data=data,family=gaussian(link="log"))
gam.check(gamP)  

gamP<-gam(chl~ti(doy,bs="cc",k=25)+ti(date_dec,bs="tp",k=22)+ti(chl.1,bs="tp",k=7),data=data,family=gaussian(link="log"))
gam.check(gamP)  

gamP<-gam(chl~ti(doy,bs="cc",k=30)+ti(date_dec,bs="tp",k=22)+ti(chl.1,bs="tp",k=7),data=data,family=gaussian(link="log"))
gam.check(gamP)  ## best we can do

gamP<-gam(chl~ti(doy,bs="cc",k=30)+ti(date_dec,bs="tp",k=25)+ti(chl.1,bs="tp",k=7),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=30)+ti(date_dec,bs="tp",k=30)+ti(chl.1,bs="tp",k=7),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=30)+ti(date_dec,bs="tp",k=30)+ti(chl.1,bs="tp",k=10),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=30)+ti(date_dec,bs="tp",k=30)+ti(chl.1,bs="tp",k=15),data=data,family=gaussian(link="log"))
gam.check(gamP) ## warning

gamP<-gam(chl~ti(doy,bs="cc",k=30)+ti(date_dec,bs="tp",k=30)+ti(chl.1,bs="tp",k=12),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=30)+ti(date_dec,bs="tp",k=40)+ti(chl.1,bs="tp",k=12),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=30)+ti(date_dec,bs="tp",k=60)+ti(chl.1,bs="tp",k=12),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=40)+ti(date_dec,bs="tp",k=80)+ti(chl.1,bs="tp",k=12),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=40)+ti(date_dec,bs="tp",k=100)+ti(chl.1,bs="tp",k=12),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=50)+ti(date_dec,bs="tp",k=120)+ti(chl.1,bs="tp",k=12),data=data,family=gaussian(link="log"))
gam.check(gamP)

perStationFlowMod[[13]]=gamP

####
data=perStationAdd[[15]]

gamP<-gam(chl~ti(doy,bs="cc")+ti(date_dec,bs="tp")+ti(chl.1,bs="tp"),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=10)+ti(date_dec,bs="tp",k=10)+ti(chl.1,bs="tp",k=10),data=data,family=gaussian(link="log"))
gam.check(gamP) ## warning

gamP<-gam(chl~ti(doy,bs="cc",k=10)+ti(date_dec,bs="tp")+ti(chl.1,bs="tp"),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=10)+ti(date_dec,bs="tp",k=10)+ti(chl.1,bs="tp"),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=10)+ti(date_dec,bs="tp",k=10)+ti(chl.1,bs="tp",k=8),data=data,family=gaussian(link="log"))
gam.check(gamP) ## warning

gamP<-gam(chl~ti(doy,bs="cc",k=10)+ti(date_dec,bs="tp",k=10)+ti(chl.1,bs="tp",k=7),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=15)+ti(date_dec,bs="tp",k=20)+ti(chl.1,bs="tp",k=7),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=25)+ti(date_dec,bs="tp",k=30)+ti(chl.1,bs="tp",k=7),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=40)+ti(date_dec,bs="tp",k=50)+ti(chl.1,bs="tp",k=7),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-bam(chl~ti(doy,bs="cc",k=50)+ti(date_dec,bs="tp",k=60)+ti(chl.1,bs="tp",k=7),data=data,family=gaussian(link="log"),control=ctrl)
gam.check(gamP)

gamP<-bam(chl~ti(doy,bs="cc",k=50)+ti(date_dec,bs="tp",k=60)+ti(chl.1,bs="tp",k=10),data=data,family=gaussian(link="log"),control=ctrl)
gam.check(gamP)

gamP<-bam(chl~ti(doy,bs="cc",k=50)+ti(date_dec,bs="tp",k=80)+ti(chl.1,bs="tp",k=10),data=data,family=gaussian(link="log"),control=ctrl)
gam.check(gamP)

perStationFlowMod[[15]]=gamP

####
data=perStationAdd[[16]]

gamP<-gam(chl~ti(doy,bs="cc")+ti(date_dec,bs="tp")+ti(chl.1,bs="tp"),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=10)+ti(date_dec,bs="tp",k=10)+ti(chl.1,bs="tp",k=10),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=15)+ti(date_dec,bs="tp",k=15)+ti(chl.1,bs="tp",k=15),data=data,family=gaussian(link="log"))
gam.check(gamP) ## warning

gamP<-gam(chl~ti(doy,bs="cc",k=15)+ti(date_dec,bs="tp",k=15)+ti(chl.1,bs="tp",k=12),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=25)+ti(date_dec,bs="tp",k=30)+ti(chl.1,bs="tp",k=12),data=data,family=gaussian(link="log"))
gam.check(gamP) ## warning

gamP<-gam(chl~ti(doy,bs="cc",k=20)+ti(date_dec,bs="tp",k=20)+ti(chl.1,bs="tp",k=12),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=25)+ti(date_dec,bs="tp",k=20)+ti(chl.1,bs="tp",k=12),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=25)+ti(date_dec,bs="tp",k=25)+ti(chl.1,bs="tp",k=12),data=data,family=gaussian(link="log"))
gam.check(gamP) ## warning

gamP<-gam(chl~ti(doy,bs="cc",k=25)+ti(date_dec,bs="tp",k=23)+ti(chl.1,bs="tp",k=12),data=data,family=gaussian(link="log"))
gam.check(gamP) ## warning

gamP<-gam(chl~ti(doy,bs="cc",k=25)+ti(date_dec,bs="tp",k=22)+ti(chl.1,bs="tp",k=12),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=35)+ti(date_dec,bs="tp",k=22)+ti(chl.1,bs="tp",k=12),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=45)+ti(date_dec,bs="tp",k=22)+ti(chl.1,bs="tp",k=12),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=55)+ti(date_dec,bs="tp",k=22)+ti(chl.1,bs="tp",k=12),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-bam(chl~ti(doy,bs="cc",k=65)+ti(date_dec,bs="tp",k=22)+ti(chl.1,bs="tp",k=12),data=data,family=gaussian(link="log"),control=ctrl)
gam.check(gamP)

perStationFlowMod[[16]]=gamP

####
data=perStationAdd[[17]]

gamP<-gam(chl~ti(doy,bs="cc")+ti(date_dec,bs="tp")+ti(chl.1,bs="tp"),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=10)+ti(date_dec,bs="tp",k=10)+ti(chl.1,bs="tp",k=10),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=15)+ti(date_dec,bs="tp",k=15)+ti(chl.1,bs="tp",k=15),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=20)+ti(date_dec,bs="tp",k=30)+ti(chl.1,bs="tp",k=15),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=20)+ti(date_dec,bs="tp",k=40)+ti(chl.1,bs="tp",k=15),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-bam(chl~ti(doy,bs="cc",k=30)+ti(date_dec,bs="tp",k=50)+ti(chl.1,bs="tp",k=15),data=data,family=gaussian(link="log"),control=ctrl)
gam.check(gamP)

gamP<-bam(chl~ti(doy,bs="cc",k=40)+ti(date_dec,bs="tp",k=60)+ti(chl.1,bs="tp",k=15),data=data,family=gaussian(link="log"),control=ctrl)
gam.check(gamP)


gamP<-bam(chl~ti(doy,bs="cc",k=50)+ti(date_dec,bs="tp",k=80)+ti(chl.1,bs="tp",k=15),data=data,family=gaussian(link="log"),control=ctrl)
gam.check(gamP)

gamP<-bam(chl~ti(doy,bs="cc",k=80)+ti(date_dec,bs="tp",k=100)+ti(chl.1,bs="tp",k=15),data=data,family=gaussian(link="log"),control=ctrl)
gam.check(gamP)

perStationFlowMod[[17]]=gamP

####
data=perStationAdd[[18]]

gamP<-gam(chl~ti(doy,bs="cc")+ti(date_dec,bs="tp")+ti(chl.1,bs="tp"),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=10)+ti(date_dec,bs="tp",k=10)+ti(chl.1,bs="tp",k=10),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=15)+ti(date_dec,bs="tp",k=15)+ti(chl.1,bs="tp",k=15),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=20)+ti(date_dec,bs="tp",k=15)+ti(chl.1,bs="tp",k=15),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=40)+ti(date_dec,bs="tp",k=40)+ti(chl.1,bs="tp",k=15),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=60)+ti(date_dec,bs="tp",k=60)+ti(chl.1,bs="tp",k=15),data=data,family=gaussian(link="log"))
gam.check(gamP)


perStationFlowMod[[18]]=gamP

####
data=perStationAdd[[21]]

gamP<-gam(chl~ti(doy,bs="cc")+ti(date_dec,bs="tp")+ti(chl.1,bs="tp"),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=10)+ti(date_dec,bs="tp",k=10)+ti(chl.1,bs="tp",k=10),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=15)+ti(date_dec,bs="tp",k=15)+ti(chl.1,bs="tp",k=15),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=20)+ti(date_dec,bs="tp",k=20)+ti(chl.1,bs="tp",k=20),data=data,family=gaussian(link="log"))
gam.check(gamP) ## warning

gamP<-gam(chl~ti(doy,bs="cc",k=20)+ti(date_dec,bs="tp",k=15)+ti(chl.1,bs="tp",k=15),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=20)+ti(date_dec,bs="tp",k=15)+ti(chl.1,bs="tp",k=20),data=data,family=gaussian(link="log"))
gam.check(gamP) ## warning

gamP<-gam(chl~ti(doy,bs="cc",k=20)+ti(date_dec,bs="tp",k=15)+ti(chl.1,bs="tp",k=18),data=data,family=gaussian(link="log"))
gam.check(gamP) ## warning

gamP<-gam(chl~ti(doy,bs="cc",k=20)+ti(date_dec,bs="tp",k=15)+ti(chl.1,bs="tp",k=17),data=data,family=gaussian(link="log"))
gam.check(gamP)  ## warning

gamP<-gam(chl~ti(doy,bs="cc",k=20)+ti(date_dec,bs="tp",k=15)+ti(chl.1,bs="tp",k=16),data=data,family=gaussian(link="log"))
gam.check(gamP) 

gamP<-gam(chl~ti(doy,bs="cc",k=20)+ti(date_dec,bs="tp",k=30)+ti(chl.1,bs="tp",k=16),data=data,family=gaussian(link="log"))
gam.check(gamP) 

gamP<-gam(chl~ti(doy,bs="cc",k=30)+ti(date_dec,bs="tp",k=40)+ti(chl.1,bs="tp",k=16),data=data,family=gaussian(link="log"))
gam.check(gamP) 

gamP<-bam(chl~ti(doy,bs="cc",k=40)+ti(date_dec,bs="tp",k=50)+ti(chl.1,bs="tp",k=16),data=data,family=gaussian(link="log"),control=ctrl)
gam.check(gamP) 

gamP<-bam(chl~ti(doy,bs="cc",k=60)+ti(date_dec,bs="tp",k=70)+ti(chl.1,bs="tp",k=16),data=data,family=gaussian(link="log"),control=ctrl)
gam.check(gamP) 

perStationFlowMod[[21]]=gamP

####

data=perStationAdd[[22]]

gamP<-gam(chl~ti(doy,bs="cc")+ti(date_dec,bs="tp")+ti(chl.1,bs="tp"),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=10)+ti(date_dec,bs="tp",k=10)+ti(chl.1,bs="tp",k=10),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=15)+ti(date_dec,bs="tp",k=15)+ti(chl.1,bs="tp",k=15),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=20)+ti(date_dec,bs="tp",k=20)+ti(chl.1,bs="tp",k=20),data=data,family=gaussian(link="log"))
gam.check(gamP) ## warning

gamP<-gam(chl~ti(doy,bs="cc",k=20)+ti(date_dec,bs="tp",k=15)+ti(chl.1,bs="tp",k=15),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=20)+ti(date_dec,bs="tp",k=20)+ti(chl.1,bs="tp",k=15),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=20)+ti(date_dec,bs="tp",k=20)+ti(chl.1,bs="tp",k=18),data=data,family=gaussian(link="log"))
gam.check(gamP) ## warning


gamP<-gam(chl~ti(doy,bs="cc",k=20)+ti(date_dec,bs="tp",k=20)+ti(chl.1,bs="tp",k=17),data=data,family=gaussian(link="log"))
gam.check(gamP) ## warning

gamP<-gam(chl~ti(doy,bs="cc",k=20)+ti(date_dec,bs="tp",k=20)+ti(chl.1,bs="tp",k=16),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=20)+ti(date_dec,bs="tp",k=30)+ti(chl.1,bs="tp",k=16),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=20)+ti(date_dec,bs="tp",k=40)+ti(chl.1,bs="tp",k=16),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=20)+ti(date_dec,bs="tp",k=50)+ti(chl.1,bs="tp",k=16),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=20)+ti(date_dec,bs="tp",k=60)+ti(chl.1,bs="tp",k=16),data=data,family=gaussian(link="log"))
gam.check(gamP)
 
perStationFlowMod[[22]]=gamP

####
data=perStationAdd[[23]]

gamP<-gam(chl~ti(doy,bs="cc")+ti(date_dec,bs="tp")+ti(chl.1,bs="tp"),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=10)+ti(date_dec,bs="tp",k=10)+ti(chl.1,bs="tp",k=10),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=15)+ti(date_dec,bs="tp",k=15)+ti(chl.1,bs="tp",k=15),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=20)+ti(date_dec,bs="tp",k=20)+ti(chl.1,bs="tp",k=15),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=30)+ti(date_dec,bs="tp",k=40)+ti(chl.1,bs="tp",k=15),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=40)+ti(date_dec,bs="tp",k=60)+ti(chl.1,bs="tp",k=15),data=data,family=gaussian(link="log"))
gam.check(gamP)


perStationFlowMod[[23]]=gamP

####
data=perStationAdd[[29]]

gamP<-gam(chl~ti(doy,bs="cc")+ti(date_dec,bs="tp")+ti(chl.1,bs="tp"),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc")+ti(date_dec,bs="tp")+ti(chl.1,bs="tp",k=7),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=30)+ti(date_dec,bs="tp",k=20)+ti(chl.1,bs="tp",k=7),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=30)+ti(date_dec,bs="tp",k=40)+ti(chl.1,bs="tp",k=7),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=30)+ti(date_dec,bs="tp",k=60)+ti(chl.1,bs="tp",k=7),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=40)+ti(date_dec,bs="tp",k=80)+ti(chl.1,bs="tp",k=7),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=40)+ti(date_dec,bs="tp",k=100)+ti(chl.1,bs="tp",k=7),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=60)+ti(date_dec,bs="tp",k=120)+ti(chl.1,bs="tp",k=7),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-bam(chl~ti(doy,bs="cc",k=60)+ti(date_dec,bs="tp",k=140)+ti(chl.1,bs="tp",k=7),data=data,family=gaussian(link="log"),control=ctrl)
gam.check(gamP) ## warning

gamP<-bam(chl~ti(doy,bs="cc",k=60)+ti(date_dec,bs="tp",k=120)+ti(chl.1,bs="tp",k=7),data=data,family=gaussian(link="log"),control=ctrl)
gam.check(gamP) ## warning

gamP<-bam(chl~ti(doy,bs="cc",k=60)+ti(date_dec,bs="tp",k=100)+ti(chl.1,bs="tp",k=7),data=data,family=gaussian(link="log"),control=ctrl)
gam.check(gamP)

gamP<-bam(chl~ti(doy,bs="cc")+ti(date_dec,bs="tp")+ti(chl.1,bs="tp",k=7),data=data,family=gaussian(link="log"),control=ctrl)
gam.check(gamP)

gamP<-bam(chl~ti(doy,bs="cc",k=50)+ti(date_dec,bs="tp",k=50)+ti(chl.1,bs="tp",k=7),data=data,family=gaussian(link="log"),control=ctrl)
gam.check(gamP)

gamP<-bam(chl~ti(doy,bs="cc",k=50)+ti(date_dec,bs="tp",k=80)+ti(chl.1,bs="tp",k=7),data=data,family=gaussian(link="log"),control=ctrl)
gam.check(gamP)

gamP<-bam(chl~ti(doy,bs="cc",k=50)+ti(date_dec,bs="tp",k=100)+ti(chl.1,bs="tp",k=7),data=data,family=gaussian(link="log"),control=ctrl)
gam.check(gamP)

gamP<-bam(chl~ti(doy,bs="cc",k=60)+ti(date_dec,bs="tp",k=110)+ti(chl.1,bs="tp",k=7),data=data,family=gaussian(link="log"),control=ctrl)
gam.check(gamP) ## warning

gamP<-bam(chl~ti(doy,bs="cc",k=50)+ti(date_dec,bs="tp",k=100)+ti(chl.1,bs="tp",k=7),data=data,family=gaussian(link="log"),control=ctrl)
gam.check(gamP) ## go with this

perStationFlowMod[[29]]=gamP

####

data=perStationAdd[[40]]

gamP<-gam(chl~ti(doy,bs="cc")+ti(date_dec,bs="tp")+ti(chl.1,bs="tp"),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=10)+ti(date_dec,bs="tp",k=10)+ti(chl.1,bs="tp",k=10),data=data,family=gaussian(link="log"))
gam.check(gamP) 

gamP<-gam(chl~ti(doy,bs="cc",k=30)+ti(date_dec,bs="tp",k=30)+ti(chl.1,bs="tp",k=10),data=data,family=gaussian(link="log"))
gam.check(gamP) 

gamP<-gam(chl~ti(doy,bs="cc",k=30)+ti(date_dec,bs="tp",k=30)+ti(chl.1,bs="tp",k=15),data=data,family=gaussian(link="log"))
gam.check(gamP) 

gamP<-gam(chl~ti(doy,bs="cc",k=40)+ti(date_dec,bs="tp",k=50)+ti(chl.1,bs="tp",k=15),data=data,family=gaussian(link="log"))
gam.check(gamP) 

gamP<-gam(chl~ti(doy,bs="cc",k=40)+ti(date_dec,bs="tp",k=70)+ti(chl.1,bs="tp",k=15),data=data,family=gaussian(link="log"))
gam.check(gamP) 

gamP<-gam(chl~ti(doy,bs="cc",k=50)+ti(date_dec,bs="tp",k=70)+ti(chl.1,bs="tp",k=15),data=data,family=gaussian(link="log"))
gam.check(gamP) 

gamP<-gam(chl~ti(doy,bs="cc",k=60)+ti(date_dec,bs="tp",k=80)+ti(chl.1,bs="tp",k=15),data=data,family=gaussian(link="log"))
gam.check(gamP) 

gamP<-gam(chl~ti(doy,bs="cc",k=70)+ti(date_dec,bs="tp",k=100)+ti(chl.1,bs="tp",k=15),data=data,family=gaussian(link="log"))
gam.check(gamP) 

gamP<-gam(chl~ti(doy,bs="cc",k=100)+ti(date_dec,bs="tp",k=100)+ti(chl.1,bs="tp",k=15),data=data,family=gaussian(link="log"))
gam.check(gamP) 

gamP<-gam(chl~ti(doy,bs="cc",k=120)+ti(date_dec,bs="tp",k=100)+ti(chl.1,bs="tp",k=15),data=data,family=gaussian(link="log"))
gam.check(gamP) 

gamP<-gam(chl~ti(doy,bs="cc",k=120)+ti(date_dec,bs="tp",k=120)+ti(chl.1,bs="tp",k=15),data=data,family=gaussian(link="log"))
gam.check(gamP) 

gamP<-bam(chl~ti(doy,bs="cc",k=120)+ti(date_dec,bs="tp",k=120)+ti(chl.1,bs="tp",k=20),data=data,family=gaussian(link="log"),control=ctrl)
gam.check(gamP)  ## warning

gamP<-gam(chl~ti(doy,bs="cc",k=120)+ti(date_dec,bs="tp",k=120)+ti(chl.1,bs="tp",k=20),data=data,family=gaussian(link="log"))
gam.check(gamP) 

gamP<-gam(chl~ti(doy,bs="cc",k=120)+ti(date_dec,bs="tp",k=140)+ti(chl.1,bs="tp",k=20),data=data,family=gaussian(link="log"))
gam.check(gamP) 

gamP<-gam(chl~ti(doy,bs="cc",k=120)+ti(date_dec,bs="tp",k=140)+ti(chl.1,bs="tp",k=25),data=data,family=gaussian(link="log"))
gam.check(gamP)  ## warning

gamP<-gam(chl~ti(doy,bs="cc",k=120)+ti(date_dec,bs="tp",k=150)+ti(chl.1,bs="tp",k=20),data=data,family=gaussian(link="log"))
gam.check(gamP) ## warning

gamP<-gam(chl~ti(doy,bs="cc",k=120)+ti(date_dec,bs="tp",k=140)+ti(chl.1,bs="tp",k=20),data=data,family=gaussian(link="log"))
gam.check(gamP) ## go with this

perStationFlowMod[[40]]=gamP

save(perStationFlowMod,file="perStationFlowMod.Rda")
setwd("~/Desktop/sfei")
load(file="perStationFlowMod.Rda")
rmse<-c()
for(i in wholeSeries){
  toUse=na.omit(perStationAdd[[i]][,c("doy","date_dec","chl","chl.1")])
  predV=predict(perStationFlowMod[[i]],toUse)
  resid=(toUse$chl-predV)^2
  rmse=c(rmse,sqrt(sum(resid,na.rm=T)/length(resid)))
  print(i)
}
rmse
# [1] 56.728845  3.425256  7.897022  6.920144  8.768470  4.793684 10.100559  7.845679  6.502875
#[10]  3.106828  3.432924  9.149566  7.169525 11.380804 11.802675

for(i in wholeSeries){
 toUse=na.omit(perStationAdd[[i]][,c("doy","date_dec","chl","chl.1")])
  predV=predict(perStationFlowMod[[i]],toUse)
  perStationAdd[[i]]$chlPred[as.numeric(row.names(toUse))]=predV
  print(i)
}

save(perStationAdd,"perStationAdd.Rda")


names(perStation)[wholeSeries]

cbind(testMerge5$rmseInt,rmse)
## worse for every station except C10

plot(perStationAdd[[11]]$date_dec,perStationAdd[[11]]$chl)
toUse=na.omit(perStationAdd[[11]][,c("doy","date_dec","chl","chl.1")])
predV=predict(perStationFlowMod[[11]],toUse)
lines(toUse$date_dec, predV,col="red",lwd=2)
lines(toUse$date_dec,toUse$chl.1,col="forestgreen",lwd=2)

plot(toUse$chl,toUse$chl.1)
cor(toUse$chl,toUse$chl.1)
## It seems weird that this doesn't help. 

table(toUse$chl>toUse$chl.1)
#FALSE  TRUE 
#185   271 

hist(toUse$chl-toUse$chl.1)
## maybe just want the "flow" to be a linear predictor?

data=perStationAdd[[11]]

gamP<-gam(chl~chl.1+ti(doy,bs="cc",k=80)+ti(date_dec,bs="tp",k=118),data=data,family=gaussian(link="log"))
gam.check(gamP)

sqrt(sum(gamP$residuals^2)/length(gamP$residuals)) ## worse than as a smooth

###
gamP<-gam(chl~chl.1+ti(doy,bs="cc",k=20)+ti(date_dec,bs="tp",k=20)+ti(log(chl.1),bs="tp",k=20),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-bam(chl~chl.1+ti(doy,bs="cc",k=20)+ti(date_dec,bs="tp",k=80)+ti(log(chl.1),bs="tp",k=20),data=data,family=gaussian(link="log"),control=ctrl)
gam.check(gamP)

gamP<-bam(chl~chl.1+ti(doy,bs="cc",k=20)+ti(date_dec,bs="tp",k=100)+ti(log(chl.1),bs="tp",k=20),data=data,family=gaussian(link="log"),control=ctrl)
gam.check(gamP)

gamP<-bam(chl~chl.1+ti(doy,bs="cc",k=20)+ti(date_dec,bs="tp",k=120)+ti(log(chl.1),bs="tp",k=20),data=data,family=gaussian(link="log"),control=ctrl)
gam.check(gamP) ## can't go beyond 120

sqrt(sum(gamP$residuals^2)/length(gamP$residuals)) ## better than before but not as good as interaction

names(testMerge5)
testMerge5$rmsePars[5]
testMerge5$rmseFull[5]
testMerge5$rmse2[5]
