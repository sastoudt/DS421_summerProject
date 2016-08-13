## for cluster script 2

library(mgcv)
setwd("/accounts/grad/sstoudt/tmpSpatGAM/")
allData<-read.csv("allData.csv")

gamP<-gam(chl~as.factor(Station)+ti(doy,bs="cc",by=as.factor(Station),k=20)+ti(date_dec,bs="tp",by=as.factor(Station),k=20),data=allData,family=gaussian(link="log"))
save(gamP,file="spatialModel3Expand.RData")