## "representative" test sites
##  D7, D19

setwd("~/Desktop/DS421_summerProject/GAM_weightedReg_comparison/shiny/data")
load("dataNice_nolag.RData") 
load("modelsNoLag_Nested.RData")
load("mods_nolag.RData")

grep("D7",names(dataNiceNoLag)) ## 16, 17, 18
grep("D19",names(dataNiceNoLag)) ## 19, 20, 21
bigModels=vector("list",6)
names(dataNiceNoLag)[16:21]

gam.check(modelsNoLag_Nested[[16]])
i=16
tmp <- mods_nolag$data[[i]] %>% 
  mutate(
    dec_time = dec_time(Date)[['dec_time']],
    doy = yday(Date)
  ) %>% 
  rename(
    res = resval, 
    flo = flolag,
    date = Date
  )
tmp=tmp[!is.na(tmp$res),]
tmp=tmp[!is.na(tmp$flo),]
tmp=tmp[order(tmp$date),]
system.time(gamDEFAULT <- gam(res ~ ti(flo,bs="tp",k=30)+ti(doy,bs="cc",k=15)+ti(dec_time,bs="tp",k=40)+
                    ti(flo,doy,bs=c("tp","cc"))+ti(flo,dec_time,bs=c("tp","tp"))+
                    ti(doy,dec_time,bs=c("cc","tp"))+ti(doy,dec_time,flo,
                                                        bs=c("cc","tp","tp"),k=c(4,4,4)), data = tmp))
## only about 5 seconds

system.time(gamDEFAULT <- gam(res ~ ti(flo,bs="tp",k=40)+ti(doy,bs="cc",k=20)+ti(dec_time,bs="tp",k=50)+
                                ti(flo,doy,bs=c("tp","cc"),k=8)+ti(flo,dec_time,bs=c("tp","tp"),k=8)+
                                ti(doy,dec_time,bs=c("cc","tp"),k=8)+ti(doy,dec_time,flo,
                                                                    bs=c("cc","tp","tp")), data = tmp))
gam.check(gamDEFAULT)
## still only about 5 seconds, can't expand too much because then not enough data to identify model
bigModels[[1]]=gamDEFAULT


#####
i=17
tmp <- mods_nolag$data[[i]] %>% 
  mutate(
    dec_time = dec_time(Date)[['dec_time']],
    doy = yday(Date)
  ) %>% 
  rename(
    res = resval, 
    flo = flolag,
    date = Date
  )
tmp=tmp[!is.na(tmp$res),]
tmp=tmp[!is.na(tmp$flo),]
tmp=tmp[order(tmp$date),]
system.time(gamDEFAULT <- gam(res ~ ti(flo,bs="tp",k=30)+ti(doy,bs="cc",k=15)+ti(dec_time,bs="tp",k=40)+
                                ti(flo,doy,bs=c("tp","cc"))+ti(flo,dec_time,bs=c("tp","tp"))+
                                ti(doy,dec_time,bs=c("cc","tp"))+ti(doy,dec_time,flo,
                                                                    bs=c("cc","tp","tp"),k=c(4,4,4)), data = tmp))
## only about 3 seconds
gam.check(gamDEFAULT)

system.time(gamDEFAULT <- gam(res ~ ti(flo,bs="tp",k=40)+ti(doy,bs="cc",k=20)+ti(dec_time,bs="tp",k=50)+
                                ti(flo,doy,bs=c("tp","cc"),k=8)+ti(flo,dec_time,bs=c("tp","tp"),k=8)+
                                ti(doy,dec_time,bs=c("cc","tp"),k=8)+ti(doy,dec_time,flo,
                                                                        bs=c("cc","tp","tp")), data = tmp))
## 20ish seconds
gam.check(gamDEFAULT)
bigModels[[2]]=gamDEFAULT

####

i=18
tmp <- mods_nolag$data[[i]] %>% 
  mutate(
    dec_time = dec_time(Date)[['dec_time']],
    doy = yday(Date)
  ) %>% 
  rename(
    res = resval, 
    flo = flolag,
    date = Date
  )
tmp=tmp[!is.na(tmp$res),]
tmp=tmp[!is.na(tmp$flo),]
tmp=tmp[order(tmp$date),]
system.time(gamDEFAULT <- gam(res ~ ti(flo,bs="tp",k=30)+ti(doy,bs="cc",k=15)+ti(dec_time,bs="tp",k=40)+
                                ti(flo,doy,bs=c("tp","cc"))+ti(flo,dec_time,bs=c("tp","tp"))+
                                ti(doy,dec_time,bs=c("cc","tp"))+ti(doy,dec_time,flo,
                                                                    bs=c("cc","tp","tp"),k=c(4,4,4)), data = tmp))
## < 1 second
gam.check(gamDEFAULT)

system.time(gamDEFAULT <- gam(res ~ ti(flo,bs="tp",k=40)+ti(doy,bs="cc",k=20)+ti(dec_time,bs="tp",k=50)+
                                ti(flo,doy,bs=c("tp","cc"),k=8)+ti(flo,dec_time,bs=c("tp","tp"),k=8)+
                                ti(doy,dec_time,bs=c("cc","tp"),k=8)+ti(doy,dec_time,flo,
                                                                        bs=c("cc","tp","tp")), data = tmp))
## 6 seconds
gam.check(gamDEFAULT)
bigModels[[3]]=gamDEFAULT

####
i=19
tmp <- mods_nolag$data[[i]] %>% 
  mutate(
    dec_time = dec_time(Date)[['dec_time']],
    doy = yday(Date)
  ) %>% 
  rename(
    res = resval, 
    flo = flolag,
    date = Date
  )
tmp=tmp[!is.na(tmp$res),]
tmp=tmp[!is.na(tmp$flo),]
tmp=tmp[order(tmp$date),]
system.time(gamDEFAULT <- gam(res ~ ti(flo,bs="tp",k=15)+ti(doy,bs="cc",k=15)+ti(dec_time,bs="tp",k=15)+
                    ti(flo,doy,bs=c("tp","cc"))+ti(flo,dec_time,bs=c("tp","tp"))+
                    ti(doy,dec_time,bs=c("cc","tp"))+ti(doy,dec_time,flo,
                                                        bs=c("cc","tp","tp")), data = tmp)) ##i=19,20
## 3 sec
gam.check(gamDEFAULT)

system.time(gamDEFAULT <- gam(res ~ ti(flo,bs="tp",k=30)+ti(doy,bs="cc",k=20)+ti(dec_time,bs="tp",k=20)+
                                ti(flo,doy,bs=c("tp","cc"),k=10)+ti(flo,dec_time,bs=c("tp","tp"),k=8)+
                                ti(doy,dec_time,bs=c("cc","tp"))+ti(doy,dec_time,flo,
                                                                    bs=c("cc","tp","tp"),k=6), data = tmp))
## <30 seconds
## biggest we can get
bigModels[[4]]=gamDEFAULT

####
i=20
tmp <- mods_nolag$data[[i]] %>% 
  mutate(
    dec_time = dec_time(Date)[['dec_time']],
    doy = yday(Date)
  ) %>% 
  rename(
    res = resval, 
    flo = flolag,
    date = Date
  )
tmp=tmp[!is.na(tmp$res),]
tmp=tmp[!is.na(tmp$flo),]
tmp=tmp[order(tmp$date),]
system.time(gamDEFAULT <- gam(res ~ ti(flo,bs="tp",k=15)+ti(doy,bs="cc",k=15)+ti(dec_time,bs="tp",k=15)+
                                ti(flo,doy,bs=c("tp","cc"))+ti(flo,dec_time,bs=c("tp","tp"))+
                                ti(doy,dec_time,bs=c("cc","tp"))+ti(doy,dec_time,flo,
                                                                    bs=c("cc","tp","tp")), data = tmp)) ##i=19,20
## 
gam.check(gamDEFAULT)

system.time(gamDEFAULT <- gam(res ~ ti(flo,bs="tp",k=30)+ti(doy,bs="cc",k=20)+ti(dec_time,bs="tp",k=20)+
                                ti(flo,doy,bs=c("tp","cc"),k=10)+ti(flo,dec_time,bs=c("tp","tp"),k=8)+
                                ti(doy,dec_time,bs=c("cc","tp"))+ti(doy,dec_time,flo,
                                                                    bs=c("cc","tp","tp"),k=6), data = tmp))
gam.check(gamDEFAULT)
## <30 seconds
## biggest we can get
bigModels[[5]]=gamDEFAULT

####
i=21
tmp <- mods_nolag$data[[i]] %>% 
  mutate(
    dec_time = dec_time(Date)[['dec_time']],
    doy = yday(Date)
  ) %>% 
  rename(
    res = resval, 
    flo = flolag,
    date = Date
  )
tmp=tmp[!is.na(tmp$res),]
tmp=tmp[!is.na(tmp$flo),]
tmp=tmp[order(tmp$date),]
gamDEFAULT <- gam(res ~ ti(flo,bs="tp",k=15)+ti(doy,bs="cc",k=15)+ti(dec_time,bs="tp",k=22)+
                    ti(flo,doy,bs=c("tp","cc"))+ti(flo,dec_time,bs=c("tp","tp"))+
                    ti(doy,dec_time,bs=c("cc","tp"))+ti(doy,dec_time,flo,bs=c("cc","tp","tp")),data=tmp)
gam.check(gamDEFAULT)

gamDEFAULT <- gam(res ~ ti(flo,bs="tp",k=20)+ti(doy,bs="cc",k=20)+ti(dec_time,bs="tp",k=22)+
                    ti(flo,doy,bs=c("tp","cc"),k=10)+ti(flo,dec_time,bs=c("tp","tp"),k=10)+
                    ti(doy,dec_time,bs=c("cc","tp"),k=8)+ti(doy,dec_time,flo,bs=c("cc","tp","tp"),k=6),data=tmp)
gam.check(gamDEFAULT)
bigModels[[6]]=gamDEFAULT

setwd("~/Desktop/sfei")
save(bigModels,file="bigModels.RData")

test=predict(bigModels[[1]],dataNiceNoLag[[16]])
length(test)
dim(dataNiceNoLag[[16]])

for(i in 1:6){
  dataNiceNoLag[[15+i]]$gamPredExpand=predict(bigModels[[i]],dataNiceNoLag[[15+i]])
  
}
setwd("~/Desktop/DS421_summerProject/GAM_weightedReg_comparison/shiny/data")
save(dataNiceNoLag,file="dataNice_nolag.RData")

ggplot(dataNiceNoLag[[16]], aes(x = date, y = res))+geom_point()+
  geom_line(data=dataNiceNoLag[[16]],aes(y = gamPred, color = 'GAM'),lwd=1)+
  geom_line(data=dataNiceNoLag[[16]],aes(y = gamPredExpand, color = 'GAM expand'), lwd=1)+
  geom_line(data=dataNiceNoLag[[16]],aes(y = wrtdsPred, color = 'WRTDS'), lwd=1)+

  scale_colour_manual(name = '',
                      labels = c('darkblue'='GAM', 'dodgerblue'='GAM expand','red'='WRTDS'),
                      values =c('darkblue','dodgerblue','red')
  ) 

ggplot(dataNiceNoLag[[17]], aes(x = date, y = res))+geom_point()+
  geom_line(data=dataNiceNoLag[[17]],aes(y = gamPred, color = 'GAM'),lwd=1)+
  geom_line(data=dataNiceNoLag[[17]],aes(y = gamPredExpand, color = 'GAM expand'), lwd=1)+
  geom_line(data=dataNiceNoLag[[17]],aes(y = wrtdsPred, color = 'WRTDS'), lwd=1)+
  
  scale_colour_manual(name = '',
                      labels = c('darkblue'='GAM', 'dodgerblue'='GAM expand','red'='WRTDS'),
                      values =c('darkblue','dodgerblue','red')
  )

ggplot(dataNiceNoLag[[18]], aes(x = date, y = res))+geom_point()+
  geom_line(data=dataNiceNoLag[[18]],aes(y = gamPred, color = 'GAM'),lwd=1)+
  geom_line(data=dataNiceNoLag[[18]],aes(y = gamPredExpand, color = 'GAM expand'), lwd=1)+
  geom_line(data=dataNiceNoLag[[18]],aes(y = wrtdsPred, color = 'WRTDS'), lwd=1)+
  
  scale_colour_manual(name = '',
                      labels = c('darkblue'='GAM', 'dodgerblue'='GAM expand','red'='WRTDS'),
                      values =c('darkblue','dodgerblue','red')
  )

ggplot(dataNiceNoLag[[19]], aes(x = date, y = res))+geom_point()+
  geom_line(data=dataNiceNoLag[[19]],aes(y = gamPred, color = 'GAM'),lwd=1)+
  geom_line(data=dataNiceNoLag[[19]],aes(y = gamPredExpand, color = 'GAM expand'), lwd=1)+
  geom_line(data=dataNiceNoLag[[19]],aes(y = wrtdsPred, color = 'WRTDS'), lwd=1)+
  
  scale_colour_manual(name = '',
                      labels = c('darkblue'='GAM', 'dodgerblue'='GAM expand','red'='WRTDS'),
                      values =c('darkblue','dodgerblue','red')
  )

ggplot(dataNiceNoLag[[20]], aes(x = date, y = res))+geom_point()+
  geom_line(data=dataNiceNoLag[[20]],aes(y = gamPred, color = 'GAM'),lwd=1)+
  geom_line(data=dataNiceNoLag[[20]],aes(y = gamPredExpand, color = 'GAM expand'), lwd=1)+
  geom_line(data=dataNiceNoLag[[20]],aes(y = wrtdsPred, color = 'WRTDS'), lwd=1)+
  
  scale_colour_manual(name = '',
                      labels = c('darkblue'='GAM', 'dodgerblue'='GAM expand','red'='WRTDS'),
                      values =c('darkblue','dodgerblue','red')
  )


ggplot(dataNiceNoLag[[21]], aes(x = date, y = res))+geom_point()+
  geom_line(data=dataNiceNoLag[[21]],aes(y = gamPred, color = 'GAM'),lwd=1)+
  geom_line(data=dataNiceNoLag[[21]],aes(y = gamPredExpand, color = 'GAM expand'), lwd=1)+
  geom_line(data=dataNiceNoLag[[21]],aes(y = wrtdsPred, color = 'WRTDS'), lwd=1)+
  
  scale_colour_manual(name = '',
                      labels = c('darkblue'='GAM', 'dodgerblue'='GAM expand','red'='WRTDS'),
                      values =c('darkblue','dodgerblue','red')
  )

## Looks like overall GAM and GAM expand are pretty similar
##(makes sense, can't actually expand that much due to identifiability constraints), 
## WRTDS less extreme then GAM predictions
## so if GAM is always winning, it means it gets high/low enough in the right places
## (not losing big by being large in magnitude when it shouldn't)

getSummaryRMSE_adapt<-function(data,model){
  data=data[!is.nan(data$res),]
  data=data[!is.nan(data$flo),]
  trueVal=data$res ## need to pass this in
  predValGAM=data$gamPred
  predValGAM_big=data$gamPredExpand
  predValWRTDS=data$wrtdsPred
  
  
  rmseGAM=sqrt(sum((trueVal-predValGAM)^2))
  rmseGAMb=sqrt(sum((trueVal-predValGAM_big)^2,na.rm=T))
  rmseWRTDS=sqrt(sum((trueVal-predValWRTDS)^2,na.rm=T))
  
  data$month=as.numeric(strftime(data$date, '%m'))
  data$year=as.numeric(strftime(data$date, '%Y'))
  
  annual1=subset(data,year<1983 & year>=1976)
  annual1I=which(data$year<1983 & data$year>=1976)
  annual1PG=predValGAM[annual1I]
  annual1PG2=predValGAM_big[annual1I]
  annual1PW=predValWRTDS[annual1I]
  
  
  annual2=subset(data,year<1990 & year>=1983)
  annual2I=which(data$year<1990 & data$year>=1983)
  annual2PG=predValGAM[annual2I]
  annual2PG2=predValGAM_big[annual2I]
  annual2PW=predValWRTDS[annual2I]
  
  
  annual3=subset(data,year<1997 & year>=1990)
  annual3I=which(data$year<1997 & data$year>=1990)
  annual3PG=predValGAM[annual3I]
  annual3PG2=predValGAM_big[annual3I]
  annual3PW=predValWRTDS[annual3I]
  
  
  annual4=subset(data,year<2004 & year>=1997)
  annual4I=which(data$year<2004 & data$year>=1997)
  annual4PG=predValGAM[annual4I]
  annual4PG2=predValGAM_big[annual4I]
  annual4PW=predValWRTDS[annual4I]
  
  
  annual5=subset(data,year>=2004) ## has 2 extra years
  annual5I=which( data$year>=2004)
  annual5PG=predValGAM[annual5I]
  annual5PG2=predValGAM_big[annual5I]
  annual5PW=predValWRTDS[annual5I]
  
  
  rmseA1G=sqrt(sum((annual1$res-annual1PG)^2))
  rmseA2G=sqrt(sum((annual2$res-annual2PG)^2))
  rmseA3G=sqrt(sum((annual3$res-annual3PG)^2))
  rmseA4G=sqrt(sum((annual4$res-annual4PG)^2))
  rmseA5G=sqrt(sum((annual5$res-annual5PG)^2))
  
  rmseA1Gb=sqrt(sum((annual1$res-annual1PG2)^2))
  rmseA2Gb=sqrt(sum((annual2$res-annual2PG2)^2))
  rmseA3Gb=sqrt(sum((annual3$res-annual3PG2)^2))
  rmseA4Gb=sqrt(sum((annual4$res-annual4PG2)^2))
  rmseA5Gb=sqrt(sum((annual5$res-annual5PG2)^2))
  
  rmseA1W=sqrt(sum((annual1$res-annual1PW)^2,na.rm=T))
  rmseA2W=sqrt(sum((annual2$res-annual2PW)^2,na.rm=T))
  rmseA3W=sqrt(sum((annual3$res-annual3PW)^2,na.rm=T))
  rmseA4W=sqrt(sum((annual4$res-annual4PW)^2,na.rm=T))
  rmseA5W=sqrt(sum((annual5$res-annual5PW)^2,na.rm=T))
  
  seasonal1=subset(data,month %in% c(1:3))
  seasonal1I=which(data$month %in% c(1:3))
  seasonal1PG=predValGAM[seasonal1I]
  seasonal1PG2=predValGAM_big[seasonal1I]
  seasonal1PW=predValWRTDS[seasonal1I]
  
  
  seasonal2=subset(data,month %in% c(4:6))
  seasonal2I=which(data$month %in% c(4:6))
  seasonal2PG=predValGAM[seasonal2I]
  seasonal2PG2=predValGAM_big[seasonal2I]
  seasonal2PW=predValWRTDS[seasonal2I]
  
  
  seasonal3=subset(data,month %in% c(7:9))
  seasonal3I=which(data$month %in% c(7:9))
  seasonal3PG=predValGAM[seasonal3I]
  seasonal3PG2=predValGAM_big[seasonal3I]
  seasonal3PW=predValWRTDS[seasonal3I]
  
  
  seasonal4=subset(data,month %in% c(10:12))
  seasonal4I=which(data$month %in% c(10:12))
  seasonal4PG=predValGAM[seasonal4I]
  seasonal4PG2=predValGAM_big[seasonal4I]
  seasonal4PW=predValWRTDS[seasonal4I]
  
  rmseS1G=sqrt(sum((seasonal1$res-seasonal1PG)^2))
  rmseS2G=sqrt(sum((seasonal2$res-seasonal2PG)^2))
  rmseS3G=sqrt(sum((seasonal3$res-seasonal3PG)^2))
  rmseS4G=sqrt(sum((seasonal4$res-seasonal4PG)^2))
  
  rmseS1Gb=sqrt(sum((seasonal1$res-seasonal1PG2)^2))
  rmseS2Gb=sqrt(sum((seasonal2$res-seasonal2PG2)^2))
  rmseS3Gb=sqrt(sum((seasonal3$res-seasonal3PG2)^2))
  rmseS4Gb=sqrt(sum((seasonal4$res-seasonal4PG2)^2))
  
  rmseS1W=sqrt(sum((seasonal1$res-seasonal1PW)^2,na.rm=T))
  rmseS2W=sqrt(sum((seasonal2$res-seasonal2PW)^2,na.rm=T))
  rmseS3W=sqrt(sum((seasonal3$res-seasonal3PW)^2,na.rm=T))
  rmseS4W=sqrt(sum((seasonal4$res-seasonal4PW)^2,na.rm=T))
  
  flow1=subset(data,flo<quantile(data$flo,.25))
  flow1I=which(data$flo<quantile(data$flo,0.25))
  flow1PG=predValGAM[flow1I]
  flow1PG2=predValGAM_big[flow1I]
  flow1PW=predValWRTDS[flow1I]
  
  flow2=subset(data,flo>=quantile(data$flo,.25) & flo<quantile(data$flo,0.5))
  flow2I=which(data$flo>=quantile(data$flo,0.25)& data$flo<quantile(data$flo,0.5))
  flow2PG=predValGAM[flow2I]
  flow2PG2=predValGAM_big[flow2I]
  flow2PW=predValWRTDS[flow2I]
  
  flow3=subset(data,flo>=quantile(data$flo,.5) & flo<quantile(data$flo,0.75))
  flow3I=which(data$flo>=quantile(data$flo,0.5)& data$flo<quantile(data$flo,0.75))
  flow3PG=predValGAM[flow3I]
  flow3PG2=predValGAM_big[flow3I]
  flow3PW=predValWRTDS[flow3I]
  
  flow4=subset(data,flo>=quantile(data$flo,.75) )
  flow4I=which(data$flo>=quantile(data$flo,0.75))
  flow4PG=predValGAM[flow4I]
  flow4PG2=predValGAM_big[flow4I]
  flow4PW=predValWRTDS[flow4I]
  
  rmseF1G=sqrt(sum((flow1$res-flow1PG)^2))
  rmseF2G=sqrt(sum((flow2$res-flow2PG)^2))
  rmseF3G=sqrt(sum((flow3$res-flow3PG)^2))
  rmseF4G=sqrt(sum((flow4$res-flow4PG)^2))
  
  rmseF1Gb=sqrt(sum((flow1$res-flow1PG2)^2))
  rmseF2Gb=sqrt(sum((flow2$res-flow2PG2)^2))
  rmseF3Gb=sqrt(sum((flow3$res-flow3PG2)^2))
  rmseF4Gb=sqrt(sum((flow4$res-flow4PG2)^2))
  
  rmseF1W=sqrt(sum((flow1$res-flow1PW)^2,na.rm=T))
  rmseF2W=sqrt(sum((flow2$res-flow2PW)^2,na.rm=T))
  rmseF3W=sqrt(sum((flow3$res-flow3PW)^2,na.rm=T))
  rmseF4W=sqrt(sum((flow4$res-flow4PW)^2,na.rm=T))
  
  rmseG=rbind(rmseGAM,rmseA1G,rmseA2G,rmseA3G,rmseA4G,rmseA5G,rmseS1G,rmseS2G,rmseS3G,rmseS4G,
              rmseF1G,rmseF2G,rmseF3G,rmseF4G)
  
  rmseGb=rbind(rmseGAMb,rmseA1Gb,rmseA2Gb,rmseA3Gb,rmseA4Gb,rmseA5Gb,rmseS1Gb,rmseS2Gb,rmseS3Gb,
               rmseS4Gb,rmseF1Gb,rmseF2Gb,rmseF3Gb,rmseF4Gb)
  
  rmseW=rbind(rmseWRTDS,rmseA1W,rmseA2W,rmseA3W,rmseA4W,rmseA5W,rmseS1W,rmseS2W,rmseS3W,rmseS4W,
              rmseF1W,rmseF2W,rmseF3W,rmseF4W)
  return(cbind(rmseG,rmseGb,rmseW))
  # return(list(all=rmse,annual1=rmseA1,annual2=rmseA2,annual3=rmseA3,annual4=rmseA4,
  #             seasonal1=rmseS1, seasonal2=rmseS2, seasonal3=rmseS3, seasonal4=rmseS4,
  #             flow1=rmseF1,flow2=rmseF2,flow3=rmseF3,flow4=rmseF4))
}

getSummaryRMSE_adapt(dataNiceNoLag[[16]],1)
## gam, gam big, wrtds

getSummaryRMSE_adapt(dataNiceNoLag[[17]],1)

getSummaryRMSE_adapt(dataNiceNoLag[[18]],1)

getSummaryRMSE_adapt(dataNiceNoLag[[19]],1)

getSummaryRMSE_adapt(dataNiceNoLag[[20]],1)

getSummaryRMSE_adapt(dataNiceNoLag[[21]],1)

## all cases, gam b has smaller rmse than gam, meaning still less than wrtds

