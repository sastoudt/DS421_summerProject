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

####
data=perStation[[2]]

gamP<-gam(chl~ti(doy,bs="cc")+ti(date_dec,bs="tp"),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=15)+ti(date_dec,bs="tp",k=20),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=30)+ti(date_dec,bs="tp",k=30),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=30)+ti(date_dec,bs="tp",k=50),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=30)+ti(date_dec,bs="tp",k=40),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=30)+ti(date_dec,bs="tp",k=60),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=30)+ti(date_dec,bs="tp",k=50),data=data,family=gaussian(link="log"))
gam.check(gamP) ## go with this

perStationMod3Compare[[2]]=gamP

####

data=perStation[[5]]

gamP<-gam(chl~ti(doy,bs="cc")+ti(date_dec,bs="tp"),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=15)+ti(date_dec,bs="tp",k=20),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=20)+ti(date_dec,bs="tp",k=30),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=30)+ti(date_dec,bs="tp",k=50),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=50)+ti(date_dec,bs="tp",k=80),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=50)+ti(date_dec,bs="tp",k=100),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=50)+ti(date_dec,bs="tp",k=90),data=data,family=gaussian(link="log"))
gam.check(gamP) ## go with this

perStationMod3Compare[[5]]=gamP

####
data=perStation[[7]]

gamP<-gam(chl~ti(doy,bs="cc")+ti(date_dec,bs="tp"),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=15)+ti(date_dec,bs="tp",k=20),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=25)+ti(date_dec,bs="tp",k=40),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=35)+ti(date_dec,bs="tp",k=60),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=40)+ti(date_dec,bs="tp",k=80),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=50)+ti(date_dec,bs="tp",k=80),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=60)+ti(date_dec,bs="tp",k=80),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=60)+ti(date_dec,bs="tp",k=90),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=65)+ti(date_dec,bs="tp",k=100),data=data,family=gaussian(link="log"))
gam.check(gamP) ## better

gamP<-gam(chl~ti(doy,bs="cc",k=70)+ti(date_dec,bs="tp",k=110),data=data,family=gaussian(link="log"))
gam.check(gamP) ## neglibly different, just go with this

perStationMod3Compare[[7]]=gamP

####

data=perStation[[11]]

gamP<-gam(chl~ti(doy,bs="cc")+ti(date_dec,bs="tp"),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=15)+ti(date_dec,bs="tp",k=20),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=15)+ti(date_dec,bs="tp",k=40),data=data,family=gaussian(link="log"))
gam.check(gamP) ## warning

gamP<-gam(chl~ti(doy,bs="cc",k=25)+ti(date_dec,bs="tp",k=60),data=data,family=gaussian(link="log"))
gam.check(gamP) ## warning

gamP<-gam(chl~ti(doy,bs="cc",k=35)+ti(date_dec,bs="tp",k=80),data=data,family=gaussian(link="log"))
gam.check(gamP) ## warning

gamP<-gam(chl~ti(doy,bs="cc",k=35)+ti(date_dec,bs="tp",k=70),data=data,family=gaussian(link="log"))
gam.check(gamP) ## warning

gamP<-gam(chl~ti(doy,bs="cc",k=35)+ti(date_dec,bs="tp",k=60),data=data,family=gaussian(link="log"))
gam.check(gamP) ## warning

gamP<-gam(chl~ti(doy,bs="cc",k=30)+ti(date_dec,bs="tp",k=60),data=data,family=gaussian(link="log"))
gam.check(gamP) ## warning

gamP<-gam(chl~ti(doy,bs="cc",k=28)+ti(date_dec,bs="tp",k=60),data=data,family=gaussian(link="log"))
gam.check(gamP) ## warning
 
gamP<-gam(chl~ti(doy,bs="cc",k=25)+ti(date_dec,bs="tp",k=80),data=data,family=gaussian(link="log"))
gam.check(gamP) ## warning

gamP<-gam(chl~ti(doy,bs="cc",k=25)+ti(date_dec,bs="tp",k=70),data=data,family=gaussian(link="log"))
gam.check(gamP) ## warning

gamP<-gam(chl~ti(doy,bs="cc",k=25)+ti(date_dec,bs="tp",k=65),data=data,family=gaussian(link="log"))
gam.check(gamP) ## warning

gamP<-gam(chl~ti(doy,bs="cc",k=15)+ti(date_dec,bs="tp",k=20),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=15)+ti(date_dec,bs="tp",k=25),data=data,family=gaussian(link="log"))
gam.check(gamP) ## warning

gamP<-gam(chl~ti(doy,bs="cc",k=15)+ti(date_dec,bs="tp",k=23),data=data,family=gaussian(link="log"))
gam.check(gamP) ## warning

gamP<-gam(chl~ti(doy,bs="cc",k=15)+ti(date_dec,bs="tp",k=22),data=data,family=gaussian(link="log"))
gam.check(gamP)  ## warning

gamP<-gam(chl~ti(doy,bs="cc",k=15)+ti(date_dec,bs="tp",k=21),data=data,family=gaussian(link="log"))
gam.check(gamP) 

gamP<-gam(chl~ti(doy,bs="cc",k=18)+ti(date_dec,bs="tp",k=21),data=data,family=gaussian(link="log"))
gam.check(gamP)  ## doesn't make a big difference

gamP<-gam(chl~ti(doy,bs="cc",k=15)+ti(date_dec,bs="tp",k=21),data=data,family=gaussian(link="log"))
gam.check(gamP) ## this looks rough response v. fitted values

gamP<-gam(chl~ti(doy,bs="cc")+ti(date_dec,bs="tp",k=25),data=data,family=gaussian(link="log"))
gam.check(gamP) ## ok this doesn't help

gamP<-gam(chl~ti(doy,bs="cc",k=15)+ti(date_dec,bs="tp",k=21),data=data,family=gaussian(link="log"))
gam.check(gamP) ## stuck with this

gamP<-gam(chl~ti(doy,bs="cc",k=50)+ti(date_dec,bs="tp",k=21),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=100)+ti(date_dec,bs="tp",k=21),data=data,family=gaussian(link="log"))
gam.check(gamP) ## helps

perStationMod3Compare[[11]]=gamP

####

data=perStation[[13]]

gamP<-gam(chl~ti(doy,bs="cc")+ti(date_dec,bs="tp"),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=15)+ti(date_dec,bs="tp",k=20),data=data,family=gaussian(link="log"))
gam.check(gamP)


gamP<-gam(chl~ti(doy,bs="cc",k=25)+ti(date_dec,bs="tp",k=40),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=30)+ti(date_dec,bs="tp",k=60),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=30)+ti(date_dec,bs="tp",k=80),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=30)+ti(date_dec,bs="tp",k=100),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=40)+ti(date_dec,bs="tp",k=100),data=data,family=gaussian(link="log"))
gam.check(gamP) ## looks good

gamP<-gam(chl~ti(doy,bs="cc",k=40)+ti(date_dec,bs="tp",k=120),data=data,family=gaussian(link="log"))
gam.check(gamP) ## looks more bunched up

gamP<-gam(chl~ti(doy,bs="cc",k=40)+ti(date_dec,bs="tp",k=100),data=data,family=gaussian(link="log"))
gam.check(gamP) ## go with this

perStationMod3Compare[[13]]=gamP

####
data=perStation[[15]]

gamP<-gam(chl~ti(doy,bs="cc")+ti(date_dec,bs="tp"),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=15)+ti(date_dec,bs="tp",k=20),data=data,family=gaussian(link="log"))
gam.check(gamP)


gamP<-gam(chl~ti(doy,bs="cc",k=25)+ti(date_dec,bs="tp",k=40),data=data,family=gaussian(link="log"))
gam.check(gamP) ## warning

gamP<-gam(chl~ti(doy,bs="cc",k=15)+ti(date_dec,bs="tp",k=40),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=15)+ti(date_dec,bs="tp",k=60),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=20)+ti(date_dec,bs="tp",k=60),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=20)+ti(date_dec,bs="tp",k=80),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=20)+ti(date_dec,bs="tp",k=100),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=20)+ti(date_dec,bs="tp",k=120),data=data,family=gaussian(link="log"))
gam.check(gamP) ## warning

gamP<-gam(chl~ti(doy,bs="cc",k=20)+ti(date_dec,bs="tp",k=110),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=20)+ti(date_dec,bs="tp",k=115),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=20)+ti(date_dec,bs="tp",k=118),data=data,family=gaussian(link="log"))
gam.check(gamP) ## warning

gamP<-gam(chl~ti(doy,bs="cc",k=20)+ti(date_dec,bs="tp",k=117),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=30)+ti(date_dec,bs="tp",k=117),data=data,family=gaussian(link="log"))
gam.check(gamP) ## warning

gamP<-gam(chl~ti(doy,bs="cc",k=25)+ti(date_dec,bs="tp",k=117),data=data,family=gaussian(link="log"))
gam.check(gamP) ## worse

gamP<-gam(chl~ti(doy,bs="cc",k=20)+ti(date_dec,bs="tp",k=117),data=data,family=gaussian(link="log"))
gam.check(gamP) ## this is the best we can do

gamP<-gam(chl~ti(doy,bs="cc",k=100)+ti(date_dec,bs="tp",k=117),data=data,family=gaussian(link="log"))
gam.check(gamP) ## warning

gamP<-gam(chl~ti(doy,bs="cc",k=80)+ti(date_dec,bs="tp",k=117),data=data,family=gaussian(link="log"))
gam.check(gamP) ## doesn't really help

perStationMod3Compare[[15]]=gamP

####
data=perStation[[16]]

gamP<-gam(chl~ti(doy,bs="cc")+ti(date_dec,bs="tp"),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=15)+ti(date_dec,bs="tp",k=20),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=15)+ti(date_dec,bs="tp",k=40),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=25)+ti(date_dec,bs="tp",k=60),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=25)+ti(date_dec,bs="tp",k=80),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=30)+ti(date_dec,bs="tp",k=100),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=30)+ti(date_dec,bs="tp",k=110),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=40)+ti(date_dec,bs="tp",k=120),data=data,family=gaussian(link="log"))
gam.check(gamP) ## warning

gamP<-gam(chl~ti(doy,bs="cc",k=30)+ti(date_dec,bs="tp",k=120),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=30)+ti(date_dec,bs="tp",k=130),data=data,family=gaussian(link="log"))
gam.check(gamP) ## warning


gamP<-gam(chl~ti(doy,bs="cc",k=30)+ti(date_dec,bs="tp",k=125),data=data,family=gaussian(link="log"))
gam.check(gamP) ## warning

gamP<-gam(chl~ti(doy,bs="cc",k=30)+ti(date_dec,bs="tp",k=122),data=data,family=gaussian(link="log"))
gam.check(gamP) ## warning

gamP<-gam(chl~ti(doy,bs="cc",k=30)+ti(date_dec,bs="tp",k=121),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=40)+ti(date_dec,bs="tp",k=121),data=data,family=gaussian(link="log"))
gam.check(gamP) ## doesn't really help

gamP<-gam(chl~ti(doy,bs="cc",k=30)+ti(date_dec,bs="tp",k=121),data=data,family=gaussian(link="log"))
gam.check(gamP) ## stuck at this

perStationMod3Compare[[16]]=gamP

####
data=perStation[[17]]

gamP<-gam(chl~ti(doy,bs="cc")+ti(date_dec,bs="tp"),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=15)+ti(date_dec,bs="tp",k=20),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=15)+ti(date_dec,bs="tp",k=40),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=125)+ti(date_dec,bs="tp",k=60),data=data,family=gaussian(link="log"))
gam.check(gamP) ## accident but looks pretty good

gamP<-gam(chl~ti(doy,bs="cc",k=25)+ti(date_dec,bs="tp",k=60),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=35)+ti(date_dec,bs="tp",k=80),data=data,family=gaussian(link="log"))
gam.check(gamP) ## warning

gamP<-gam(chl~ti(doy,bs="cc",k=25)+ti(date_dec,bs="tp",k=80),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=25)+ti(date_dec,bs="tp",k=100),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=40)+ti(date_dec,bs="tp",k=100),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=40)+ti(date_dec,bs="tp",k=120),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=40)+ti(date_dec,bs="tp",k=150),data=data,family=gaussian(link="log"))
gam.check(gamP) ## warning

gamP<-gam(chl~ti(doy,bs="cc",k=40)+ti(date_dec,bs="tp",k=140),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=50)+ti(date_dec,bs="tp",k=140),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=50)+ti(date_dec,bs="tp",k=150),data=data,family=gaussian(link="log"))
gam.check(gamP) ## warning

gamP<-gam(chl~ti(doy,bs="cc",k=50)+ti(date_dec,bs="tp",k=145),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=50)+ti(date_dec,bs="tp",k=148),data=data,family=gaussian(link="log"))
gam.check(gamP) ## warning

gamP<-gam(chl~ti(doy,bs="cc",k=50)+ti(date_dec,bs="tp",k=147),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=60)+ti(date_dec,bs="tp",k=147),data=data,family=gaussian(link="log"))
gam.check(gamP) ## doesn't help

gamP<-gam(chl~ti(doy,bs="cc",k=50)+ti(date_dec,bs="tp",k=147),data=data,family=gaussian(link="log"))
gam.check(gamP) ## stuck with this

gamP<-gam(chl~ti(doy,bs="cc",k=100)+ti(date_dec,bs="tp",k=147),data=data,family=gaussian(link="log"))
gam.check(gamP) ## better, weird but go with it

perStationMod3Compare[[17]]=gamP

####
data=perStation[[18]]

gamP<-gam(chl~ti(doy,bs="cc")+ti(date_dec,bs="tp"),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=15)+ti(date_dec,bs="tp",k=20),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=15)+ti(date_dec,bs="tp",k=40),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=15)+ti(date_dec,bs="tp",k=50),data=data,family=gaussian(link="log"))
gam.check(gamP) ## no real difference

gamP<-gam(chl~ti(doy,bs="cc",k=50)+ti(date_dec,bs="tp",k=50),data=data,family=gaussian(link="log"))
gam.check(gamP)  ## doesn't help

gamP<-gam(chl~ti(doy,bs="cc",k=15)+ti(date_dec,bs="tp",k=40),data=data,family=gaussian(link="log"))
gam.check(gamP) ## go with this

perStationMod3Compare[[18]]=gamP

####
data=perStation[[21]]

gamP<-gam(chl~ti(doy,bs="cc")+ti(date_dec,bs="tp"),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=15)+ti(date_dec,bs="tp",k=20),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=15)+ti(date_dec,bs="tp",k=40),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=15)+ti(date_dec,bs="tp",k=50),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=15)+ti(date_dec,bs="tp",k=60),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=25)+ti(date_dec,bs="tp",k=60),data=data,family=gaussian(link="log"))
gam.check(gamP) ## go with this

perStationMod3Compare[[21]]=gamP

####
data=perStation[[22]]

gamP<-gam(chl~ti(doy,bs="cc")+ti(date_dec,bs="tp"),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=15)+ti(date_dec,bs="tp",k=20),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=15)+ti(date_dec,bs="tp",k=40),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=15)+ti(date_dec,bs="tp",k=60),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=25)+ti(date_dec,bs="tp",k=80),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=25)+ti(date_dec,bs="tp",k=100),data=data,family=gaussian(link="log"))
gam.check(gamP) ## warning

gamP<-gam(chl~ti(doy,bs="cc",k=25)+ti(date_dec,bs="tp",k=90),data=data,family=gaussian(link="log"))
gam.check(gamP) 

gamP<-gam(chl~ti(doy,bs="cc",k=25)+ti(date_dec,bs="tp",k=95),data=data,family=gaussian(link="log"))
gam.check(gamP) 

gamP<-gam(chl~ti(doy,bs="cc",k=25)+ti(date_dec,bs="tp",k=98),data=data,family=gaussian(link="log"))
gam.check(gamP) 


gamP<-gam(chl~ti(doy,bs="cc",k=25)+ti(date_dec,bs="tp",k=99),data=data,family=gaussian(link="log"))
gam.check(gamP) 

gamP<-gam(chl~ti(doy,bs="cc",k=50)+ti(date_dec,bs="tp",k=99),data=data,family=gaussian(link="log"))
gam.check(gamP)  ## doesn't help

gamP<-gam(chl~ti(doy,bs="cc",k=25)+ti(date_dec,bs="tp",k=99),data=data,family=gaussian(link="log"))
gam.check(gamP)  ## go with this

perStationMod3Compare[[22]]=gamP

####
data=perStation[[23]]

gamP<-gam(chl~ti(doy,bs="cc")+ti(date_dec,bs="tp"),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=15)+ti(date_dec,bs="tp",k=20),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=125)+ti(date_dec,bs="tp",k=40),data=data,family=gaussian(link="log"))
gam.check(gamP) ## oops

gamP<-gam(chl~ti(doy,bs="cc",k=25)+ti(date_dec,bs="tp",k=40),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=25)+ti(date_dec,bs="tp",k=60),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=30)+ti(date_dec,bs="tp",k=80),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=40)+ti(date_dec,bs="tp",k=100),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=40)+ti(date_dec,bs="tp",k=120),data=data,family=gaussian(link="log"))
gam.check(gamP) ## warning

gamP<-gam(chl~ti(doy,bs="cc",k=40)+ti(date_dec,bs="tp",k=110),data=data,family=gaussian(link="log"))
gam.check(gamP) ## good

gamP<-gam(chl~ti(doy,bs="cc",k=40)+ti(date_dec,bs="tp",k=115),data=data,family=gaussian(link="log"))
gam.check(gamP) ## 

gamP<-gam(chl~ti(doy,bs="cc",k=40)+ti(date_dec,bs="tp",k=118),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=80)+ti(date_dec,bs="tp",k=118),data=data,family=gaussian(link="log"))
gam.check(gamP) ## Helps tighten up response v. fitted values

gamP<-gam(chl~ti(doy,bs="cc",k=100)+ti(date_dec,bs="tp",k=118),data=data,family=gaussian(link="log"))
gam.check(gamP) ## neglible difference

gamP<-gam(chl~ti(doy,bs="cc",k=80)+ti(date_dec,bs="tp",k=118),data=data,family=gaussian(link="log"))
gam.check(gamP) 

perStationMod3Compare[[23]]=gamP

####
data=perStation[[29]]

gamP<-gam(chl~ti(doy,bs="cc")+ti(date_dec,bs="tp"),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=15)+ti(date_dec,bs="tp",k=20),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=15)+ti(date_dec,bs="tp",k=40),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=15)+ti(date_dec,bs="tp",k=60),data=data,family=gaussian(link="log"))
gam.check(gamP) ## warning

gamP<-gam(chl~ti(doy,bs="cc",k=15)+ti(date_dec,bs="tp",k=50),data=data,family=gaussian(link="log"))
gam.check(gamP) 

gamP<-gam(chl~ti(doy,bs="cc",k=25)+ti(date_dec,bs="tp",k=50),data=data,family=gaussian(link="log"))
gam.check(gamP) 

gamP<-gam(chl~ti(doy,bs="cc",k=25)+ti(date_dec,bs="tp",k=55),data=data,family=gaussian(link="log"))
gam.check(gamP) 

gamP<-gam(chl~ti(doy,bs="cc",k=25)+ti(date_dec,bs="tp",k=60),data=data,family=gaussian(link="log"))
gam.check(gamP) 

gamP<-gam(chl~ti(doy,bs="cc",k=35)+ti(date_dec,bs="tp",k=80),data=data,family=gaussian(link="log"))
gam.check(gamP) 

gamP<-gam(chl~ti(doy,bs="cc",k=45)+ti(date_dec,bs="tp",k=100),data=data,family=gaussian(link="log"))
gam.check(gamP) 

gamP<-gam(chl~ti(doy,bs="cc",k=60)+ti(date_dec,bs="tp",k=120),data=data,family=gaussian(link="log"))
gam.check(gamP) 

gamP<-gam(chl~ti(doy,bs="cc",k=60)+ti(date_dec,bs="tp",k=140),data=data,family=gaussian(link="log"))
gam.check(gamP)  ## warning

gamP<-gam(chl~ti(doy,bs="cc",k=60)+ti(date_dec,bs="tp",k=130),data=data,family=gaussian(link="log"))
gam.check(gamP)  ## warning

gamP<-gam(chl~ti(doy,bs="cc",k=60)+ti(date_dec,bs="tp",k=125),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=80)+ti(date_dec,bs="tp",k=125),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=80)+ti(date_dec,bs="tp",k=130),data=data,family=gaussian(link="log"))
gam.check(gamP) ## warning

gamP<-gam(chl~ti(doy,bs="cc",k=100)+ti(date_dec,bs="tp",k=125),data=data,family=gaussian(link="log"))
gam.check(gamP) ## go with this

perStationMod3Compare[[29]]=gamP

####
data=perStation[[40]]

gamP<-gam(chl~ti(doy,bs="cc")+ti(date_dec,bs="tp"),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=15)+ti(date_dec,bs="tp",k=20),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=25)+ti(date_dec,bs="tp",k=40),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=35)+ti(date_dec,bs="tp",k=60),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=45)+ti(date_dec,bs="tp",k=80),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=60)+ti(date_dec,bs="tp",k=100),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=60)+ti(date_dec,bs="tp",k=120),data=data,family=gaussian(link="log"))
gam.check(gamP) ## warning

gamP<-gam(chl~ti(doy,bs="cc",k=60)+ti(date_dec,bs="tp",k=110),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=60)+ti(date_dec,bs="tp",k=115),data=data,family=gaussian(link="log"))
gam.check(gamP)

gamP<-gam(chl~ti(doy,bs="cc",k=60)+ti(date_dec,bs="tp",k=118),data=data,family=gaussian(link="log"))
gam.check(gamP) ## go with this

perStationMod3Compare[[40]]=gamP

setwd("~/Desktop/sfei")
save(perStationMod3Compare,file="perStationMod3Compare.RData")
