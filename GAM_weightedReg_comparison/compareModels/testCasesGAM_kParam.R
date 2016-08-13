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
