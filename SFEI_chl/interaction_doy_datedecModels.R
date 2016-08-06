setwd("~/Desktop/sfei")
load(file="perStation.Rda")
require(mgcv)

wholeSeries<-c(1, 2, 5, 7, 11, 13, 15, 16, 17, 18, 21, 22, 23, 29, 40)
perStationIntMod=vector("list",length(perStation))


data=perStation[[1]]

gamP<-gam(chl~ti(doy,bs="cc")+ti(date_dec,bs="tp")+ti(doy,date_dec),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=20)+ti(date_dec,bs="tp",k=20)+ti(doy,date_dec),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=20)+ti(date_dec,bs="tp",k=30)+ti(doy,date_dec),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=20)+ti(date_dec,bs="tp",k=40)+ti(doy,date_dec),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=20)+ti(date_dec,bs="tp",k=45)+ti(doy,date_dec),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=20)+ti(date_dec,bs="tp",k=50)+ti(doy,date_dec),data=data,family=gaussian(link="log"))
gam.check(gamP) ## going to stick with this, increase from 50 gets really slow

gamP<-gam(chl~ti(doy,bs="cc",k=20)+ti(date_dec,bs="tp",k=50)+ti(doy,date_dec,k=c(10,10)),data=data,family=gaussian(link="log"))
gam.check(gamP)
perStationIntMod[[1]]=gamP

####
data=perStation[[2]]

gamP<-gam(chl~ti(doy,bs="cc")+ti(date_dec,bs="tp")+ti(doy,date_dec),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=20)+ti(date_dec,bs="tp",k=20)+ti(doy,date_dec),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=20)+ti(date_dec,bs="tp",k=30)+ti(doy,date_dec),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=20)+ti(date_dec,bs="tp",k=40)+ti(doy,date_dec),data=data,family=gaussian(link="log"))
gam.check(gamP)

perStationIntMod[[2]]=gamP

####
data=perStation[[5]]

gamP<-gam(chl~ti(doy,bs="cc")+ti(date_dec,bs="tp")+ti(doy,date_dec),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=20)+ti(date_dec,bs="tp",k=20)+ti(doy,date_dec),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=20)+ti(date_dec,bs="tp",k=40)+ti(doy,date_dec),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=30)+ti(date_dec,bs="tp",k=40)+ti(doy,date_dec),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=30)+ti(date_dec,bs="tp",k=40)+ti(doy,date_dec,k=20),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=30)+ti(date_dec,bs="tp",k=40)+ti(doy,date_dec,k=c(10,10)),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=30)+ti(date_dec,bs="tp",k=40)+ti(doy,date_dec,k=c(5,5)),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=30)+ti(date_dec,bs="tp",k=40)+ti(doy,date_dec,k=c(15,15)),data=data,family=gaussian(link="log"))
gam.check(gamP)

perStationIntMod[[5]]=gamP

####
data=perStation[[7]]

gamP<-gam(chl~ti(doy,bs="cc")+ti(date_dec,bs="tp")+ti(doy,date_dec),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=10)+ti(date_dec,bs="tp",k=10)+ti(doy,date_dec),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=20)+ti(date_dec,bs="tp",k=20)+ti(doy,date_dec),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=20)+ti(date_dec,bs="tp",k=30)+ti(doy,date_dec),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=20)+ti(date_dec,bs="tp",k=30)+ti(doy,date_dec,k=c(8,8)),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=20)+ti(date_dec,bs="tp",k=40)+ti(doy,date_dec,k=c(8,8)),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=20)+ti(date_dec,bs="tp",k=40)+ti(doy,date_dec,k=c(10,10)),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=30)+ti(date_dec,bs="tp",k=50)+ti(doy,date_dec,k=c(10,10)),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=30)+ti(date_dec,bs="tp",k=60)+ti(doy,date_dec,k=c(10,10)),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=30)+ti(date_dec,bs="tp",k=60)+ti(doy,date_dec,k=c(12,12)),data=data,family=gaussian(link="log"))
gam.check(gamP) 

gamP<-gam(chl~ti(doy,bs="cc",k=30)+ti(date_dec,bs="tp",k=70)+ti(doy,date_dec,k=c(12,12)),data=data,family=gaussian(link="log"))
gam.check(gamP) ## this is worse

gamP<-gam(chl~ti(doy,bs="cc",k=30)+ti(date_dec,bs="tp",k=100)+ti(doy,date_dec,k=c(15,15)),data=data,family=gaussian(link="log"))
gam.check(gamP) ## go with this


perStationIntMod[[7]]=gamP

####

data=perStation[[11]]

gamP<-gam(chl~ti(doy,bs="cc")+ti(date_dec,bs="tp")+ti(doy,date_dec),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=10)+ti(date_dec,bs="tp",k=10)+ti(doy,date_dec),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=20)+ti(date_dec,bs="tp",k=20)+ti(doy,date_dec,k=c(8,8)),data=data,family=gaussian(link="log"))
gam.check(gamP) ## warning message

gamP<-gam(chl~ti(doy,bs="cc",k=20)+ti(date_dec,bs="tp",k=20)+ti(doy,date_dec),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=30)+ti(date_dec,bs="tp",k=30)+ti(doy,date_dec),data=data,family=gaussian(link="log"))
gam.check(gamP) ## warnings

gamP<-gam(chl~ti(doy,bs="cc",k=20)+ti(date_dec,bs="tp",k=30)+ti(doy,date_dec),data=data,family=gaussian(link="log"))
gam.check(gamP) ## warning

gamP<-gam(chl~ti(doy,bs="cc",k=20)+ti(date_dec,bs="tp",k=20)+ti(doy,date_dec,k=c(8,8)),data=data,family=gaussian(link="log"))
gam.check(gamP) ## warning

gamP<-gam(chl~ti(doy,bs="cc",k=20)+ti(date_dec,bs="tp",k=20)+ti(doy,date_dec,k=c(6,6)),data=data,family=gaussian(link="log"))
gam.check(gamP) 

gamP<-gam(chl~ti(doy,bs="cc",k=25)+ti(date_dec,bs="tp",k=25)+ti(doy,date_dec,k=c(6,6)),data=data,family=gaussian(link="log"))
gam.check(gamP)  ## warning

gamP<-gam(chl~ti(doy,bs="cc",k=20)+ti(date_dec,bs="tp",k=25)+ti(doy,date_dec,k=c(6,6)),data=data,family=gaussian(link="log"))
gam.check(gamP)  ## warning

gamP<-gam(chl~ti(doy,bs="cc",k=20)+ti(date_dec,bs="tp",k=22)+ti(doy,date_dec,k=c(6,6)),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=20)+ti(date_dec,bs="tp",k=23)+ti(doy,date_dec,k=c(6,6)),data=data,family=gaussian(link="log"))
gam.check(gamP) ## warning

gamP<-gam(chl~ti(doy,bs="cc",k=20)+ti(date_dec,bs="tp",k=22)+ti(doy,date_dec,k=c(7,7)),data=data,family=gaussian(link="log"))
gam.check(gamP) 

gamP<-gam(chl~ti(doy,bs="cc",k=20)+ti(date_dec,bs="tp",k=22)+ti(doy,date_dec,k=c(8,8)),data=data,family=gaussian(link="log"))
gam.check(gamP) 

gamP<-gam(chl~ti(doy,bs="cc",k=20)+ti(date_dec,bs="tp",k=22)+ti(doy,date_dec,k=c(10,10)),data=data,family=gaussian(link="log"))
gam.check(gamP) 

gamP<-gam(chl~ti(doy,bs="cc",k=30)+ti(date_dec,bs="tp",k=22)+ti(doy,date_dec,k=c(10,10)),data=data,family=gaussian(link="log"))
gam.check(gamP)  ## warning

gamP<-gam(chl~ti(doy,bs="cc",k=25)+ti(date_dec,bs="tp",k=22)+ti(doy,date_dec,k=c(10,10)),data=data,family=gaussian(link="log"))
gam.check(gamP) ## warning

gamP<-gam(chl~ti(doy,bs="cc",k=22)+ti(date_dec,bs="tp",k=22)+ti(doy,date_dec,k=c(10,10)),data=data,family=gaussian(link="log"))
gam.check(gamP) ## warning

gamP<-gam(chl~ti(doy,bs="cc",k=21)+ti(date_dec,bs="tp",k=22)+ti(doy,date_dec,k=c(10,10)),data=data,family=gaussian(link="log"))
gam.check(gamP) ## warning

gamP<-gam(chl~ti(doy,bs="cc",k=20)+ti(date_dec,bs="tp",k=22)+ti(doy,date_dec,k=c(12,12)),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=20)+ti(date_dec,bs="tp",k=22)+ti(doy,date_dec,k=c(15,15)),data=data,family=gaussian(link="log"))
gam.check(gamP) ## warning

gamP<-gam(chl~ti(doy,bs="cc",k=20)+ti(date_dec,bs="tp",k=22)+ti(doy,date_dec,k=c(13,13)),data=data,family=gaussian(link="log"))
gam.check(gamP) ## too slow

## have to come back to this

####

data=perStation[[13]]

gamP<-gam(chl~ti(doy,bs="cc")+ti(date_dec,bs="tp")+ti(doy,date_dec),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=10)+ti(date_dec,bs="tp",k=10)+ti(doy,date_dec),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=20)+ti(date_dec,bs="tp",k=20)+ti(doy,date_dec),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=20)+ti(date_dec,bs="tp",k=30)+ti(doy,date_dec),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=20)+ti(date_dec,bs="tp",k=30)+ti(doy,date_dec,k=c(8,8)),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=20)+ti(date_dec,bs="tp",k=30)+ti(doy,date_dec,k=c(10,10)),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=20)+ti(date_dec,bs="tp",k=40)+ti(doy,date_dec,k=c(10,10)),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=20)+ti(date_dec,bs="tp",k=50)+ti(doy,date_dec,k=c(10,10)),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=20)+ti(date_dec,bs="tp",k=60)+ti(doy,date_dec,k=c(10,10)),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=20)+ti(date_dec,bs="tp",k=60)+ti(doy,date_dec,k=c(12,12)),data=data,family=gaussian(link="log"))
gam.check(gamP) ## worse

gamP<-gam(chl~ti(doy,bs="cc",k=20)+ti(date_dec,bs="tp",k=70)+ti(doy,date_dec,k=c(10,10)),data=data,family=gaussian(link="log"))
gam.check(gamP) ## warning

gamP<-gam(chl~ti(doy,bs="cc",k=20)+ti(date_dec,bs="tp",k=65)+ti(doy,date_dec,k=c(10,10)),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=20)+ti(date_dec,bs="tp",k=68)+ti(doy,date_dec,k=c(10,10)),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=20)+ti(date_dec,bs="tp",k=69)+ti(doy,date_dec,k=c(10,10)),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=20)+ti(date_dec,bs="tp",k=69)+ti(doy,date_dec,k=c(11,11)),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=20)+ti(date_dec,bs="tp",k=70)+ti(doy,date_dec,k=c(11,11)),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=20)+ti(date_dec,bs="tp",k=80)+ti(doy,date_dec,k=c(11,11)),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=20)+ti(date_dec,bs="tp",k=80)+ti(doy,date_dec,k=c(10,10)),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=20)+ti(date_dec,bs="tp",k=90)+ti(doy,date_dec,k=c(10,10)),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=20)+ti(date_dec,bs="tp",k=100)+ti(doy,date_dec,k=c(10,10)),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=30)+ti(date_dec,bs="tp",k=100)+ti(doy,date_dec,k=c(10,10)),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=30)+ti(date_dec,bs="tp",k=120)+ti(doy,date_dec,k=c(10,10)),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=30)+ti(date_dec,bs="tp",k=150)+ti(doy,date_dec,k=c(10,10)),data=data,family=gaussian(link="log"))
gam.check(gamP) ## warning

gamP<-gam(chl~ti(doy,bs="cc",k=30)+ti(date_dec,bs="tp",k=140)+ti(doy,date_dec,k=c(10,10)),data=data,family=gaussian(link="log"))
gam.check(gamP) ## error

gamP<-gam(chl~ti(doy,bs="cc",k=30)+ti(date_dec,bs="tp",k=130)+ti(doy,date_dec,k=c(10,10)),data=data,family=gaussian(link="log"))
gam.check(gamP) ## warning

gamP<-gam(chl~ti(doy,bs="cc",k=30)+ti(date_dec,bs="tp",k=125)+ti(doy,date_dec,k=c(10,10)),data=data,family=gaussian(link="log"))
gam.check(gamP) ## error

gamP<-gam(chl~ti(doy,bs="cc",k=30)+ti(date_dec,bs="tp",k=120)+ti(doy,date_dec,k=c(10,10)),data=data,family=gaussian(link="log"))
gam.check(gamP) ## this is the best we are going to get


perStationIntMod[[13]]=gamP

####
data=perStation[[15]]

gamP<-gam(chl~ti(doy,bs="cc")+ti(date_dec,bs="tp")+ti(doy,date_dec),data=data,family=gaussian(link="log"))
gam.check(gamP) ## warning

gamP<-gam(chl~ti(doy,bs="cc",k=10)+ti(date_dec,bs="tp",k=10)+ti(doy,date_dec),data=data,family=gaussian(link="log"))
gam.check(gamP) 


gamP<-gam(chl~ti(doy,bs="cc",k=20)+ti(date_dec,bs="tp",k=20)+ti(doy,date_dec),data=data,family=gaussian(link="log"))
gam.check(gamP)  ## warning

gamP<-gam(chl~ti(doy,bs="cc",k=15)+ti(date_dec,bs="tp",k=10)+ti(doy,date_dec),data=data,family=gaussian(link="log"))
gam.check(gamP) 

gamP<-gam(chl~ti(doy,bs="cc",k=20)+ti(date_dec,bs="tp",k=10)+ti(doy,date_dec),data=data,family=gaussian(link="log"))
gam.check(gamP) 

gamP<-gam(chl~ti(doy,bs="cc",k=20)+ti(date_dec,bs="tp",k=15)+ti(doy,date_dec),data=data,family=gaussian(link="log"))
gam.check(gamP)  ## warning

gamP<-gam(chl~ti(doy,bs="cc",k=20)+ti(date_dec,bs="tp",k=10)+ti(doy,date_dec,k=c(10,10)),data=data,family=gaussian(link="log"))
gam.check(gamP)  ## warning

gamP<-gam(chl~ti(doy,bs="cc",k=20)+ti(date_dec,bs="tp",k=10)+ti(doy,date_dec,k=c(8,8)),data=data,family=gaussian(link="log"))
gam.check(gamP) ## warning

gamP<-gam(chl~ti(doy,bs="cc",k=20)+ti(date_dec,bs="tp",k=10)+ti(doy,date_dec,k=c(6,6)),data=data,family=gaussian(link="log"))
gam.check(gamP) ## warning

gamP<-gam(chl~ti(doy,bs="cc",k=20)+ti(date_dec,bs="tp",k=12)+ti(doy,date_dec),data=data,family=gaussian(link="log"))
gam.check(gamP) ## warning

## so stuck? come back

####

data=perStation[[16]]

gamP<-gam(chl~ti(doy,bs="cc")+ti(date_dec,bs="tp")+ti(doy,date_dec),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=10)+ti(date_dec,bs="tp",k=10)+ti(doy,date_dec),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=20)+ti(date_dec,bs="tp",k=20)+ti(doy,date_dec),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=20)+ti(date_dec,bs="tp",k=30)+ti(doy,date_dec),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=20)+ti(date_dec,bs="tp",k=30)+ti(doy,date_dec,k=c(6,6)),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=20)+ti(date_dec,bs="tp",k=30)+ti(doy,date_dec,k=c(7,7)),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=20)+ti(date_dec,bs="tp",k=40)+ti(doy,date_dec,k=c(7,7)),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=20)+ti(date_dec,bs="tp",k=40)+ti(doy,date_dec,k=c(10,10)),data=data,family=gaussian(link="log"))
gam.check(gamP) ## warning

gamP<-gam(chl~ti(doy,bs="cc",k=20)+ti(date_dec,bs="tp",k=40)+ti(doy,date_dec,k=c(8,8)),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=20)+ti(date_dec,bs="tp",k=45)+ti(doy,date_dec,k=c(8,8)),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=20)+ti(date_dec,bs="tp",k=45)+ti(doy,date_dec,k=c(9,9)),data=data,family=gaussian(link="log"))
gam.check(gamP) ## warning

gamP<-gam(chl~ti(doy,bs="cc",k=20)+ti(date_dec,bs="tp",k=50)+ti(doy,date_dec,k=c(8,8)),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=20)+ti(date_dec,bs="tp",k=60)+ti(doy,date_dec,k=c(8,8)),data=data,family=gaussian(link="log"))
gam.check(gamP) ## getting better

gamP<-gam(chl~ti(doy,bs="cc",k=20)+ti(date_dec,bs="tp",k=70)+ti(doy,date_dec,k=c(8,8)),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=20)+ti(date_dec,bs="tp",k=80)+ti(doy,date_dec,k=c(8,8)),data=data,family=gaussian(link="log"))
gam.check(gamP) ## warning

gamP<-gam(chl~ti(doy,bs="cc",k=20)+ti(date_dec,bs="tp",k=75)+ti(doy,date_dec,k=c(8,8)),data=data,family=gaussian(link="log"))
gam.check(gamP) ## I think this is the best we can do

gamP<-gam(chl~ti(doy,bs="cc",k=20)+ti(date_dec,bs="tp",k=78)+ti(doy,date_dec,k=c(8,8)),data=data,family=gaussian(link="log"))
gam.check(gamP) 

gamP<-gam(chl~ti(doy,bs="cc",k=20)+ti(date_dec,bs="tp",k=79)+ti(doy,date_dec,k=c(8,8)),data=data,family=gaussian(link="log"))
gam.check(gamP) 

gamP<-gam(chl~ti(doy,bs="cc",k=20)+ti(date_dec,bs="tp",k=79)+ti(doy,date_dec,k=c(9,8)),data=data,family=gaussian(link="log"))
gam.check(gamP)  ## going with this

perStationIntMod[[16]]=gamP

####