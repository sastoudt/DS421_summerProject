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

