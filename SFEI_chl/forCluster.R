## for cluster script

library(mgcv)
setwd("/accounts/grad/sstoudt/tmpSpatGAM/")
allData<-read.csv("allData.csv")


##
mod1<-gam(chl~as.factor(Station)+ti(doy,bs="cc",k=150)+ti(date_dec,bs="tp",k=150),data=allData,family=gaussian(link="log"))
save(mod1,file="mod1.RData")
##
mod2<-gam(chl~as.factor(Station)+ti(doy,bs="cc",k=150)+ti(date_dec,bs="tp",by=as.factor(Station),k=150),data=allData,family=gaussian(link="log"))
save(mod2,file="mod2.RData")
##
mod3<-gam(chl~as.factor(Station)+ti(doy,bs="cc",by=as.factor(Station),k=100)+ti(date_dec,bs="tp",by=as.factor(Station),k=100),data=allData,family=gaussian(link="log"))
save(mod3,file="mod3.RData")
##
mod4<-gam(chl~as.factor(Station)+ti(doy,bs="cc",k=150)+ti(date_dec,bs="tp",by=as.factor(Station),k=150)+
            ti(doy,date_dec,by=as.factor(Station)),data=allData,family=gaussian(link="log"),k=8)
save(mod4,file="mod4.RData")