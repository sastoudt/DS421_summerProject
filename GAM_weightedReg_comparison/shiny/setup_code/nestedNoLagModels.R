load("data/mods_nolag.RData")
load("data/dataNice_nolag.RData")

modelsNoLag_Nested=vector("list",length(dataNiceNoLag))
modelsNoLag_NoFlow_Nested=vector("list",length(dataNiceNoLag))
for(i in 1:length(dataNiceNoLag)){
  
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
  gamDEFAULT <- gam(res ~ ti(flo,bs="tp",k=30)+ti(doy,bs="cc",k=15)+ti(dec_time,bs="tp",k=40)+
                      ti(flo,doy,bs=c("tp","cc"))+ti(flo,dec_time,bs=c("tp","tp"))+
                      ti(doy,dec_time,bs=c("cc","tp"))+ti(doy,dec_time,flo,
                                                          bs=c("cc","tp","tp"),k=c(4,4,4)), data = tmp)
  gamDEFAULTnoflow <- gam(res ~ ti(doy,bs="cc",k=15)+ti(dec_time,bs="tp",k=45)+
                            ti(doy,dec_time,bs=c("cc","tp"),k=c(7,7)), data = tmp)
  
  #  gamtmp <- gam(res ~ te(dec_time, doy, flo, bs = c("tp", "cc", "tp")), k = c(5, 8, 5), data = tmp, knots = list(doy = c(1, 366)))
  
  modelsNoLag_Nested[[i]]=gamDEFAULT
  modelsNoLag_NoFlow_Nested[[i]]=gamDEFAULTnoflow
  names(modelsNoLag_Nested)[i]=paste(mods_nolag$Site_Code[[i]],mods_nolag$resvar[[i]],sep="_")
  names(modelsNoLag_NoFlow_Nested)[i]=paste(mods_nolag$Site_Code[[i]],mods_nolag$resvar[[i]],sep="_")
  print(i)
}

## warnings on 4, 5, 6 with flow
## warnings on 19, 20, 21 both

## problematic cases

for(i in 4:6){
  
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
  gamDEFAULT <- gam(res ~ ti(flo,bs="tp",k=15)+ti(doy,bs="cc",k=15)+ti(dec_time,bs="tp",k=40)+
                      ti(flo,doy,bs=c("tp","cc"))+ti(flo,dec_time,bs=c("tp","tp"),k=c(6,6))+
                      ti(doy,dec_time,bs=c("cc","tp"))+ti(doy,dec_time,flo,
                                                          bs=c("cc","tp","tp")), data = tmp) ##i=4, 5, 6
  
  modelsNoLag_Nested[[i]]=gamDEFAULT
  names(modelsNoLag_Nested)[i]=paste(mods_nolag$Site_Code[[i]],mods_nolag$resvar[[i]],sep="_")
  
  
}

for(i in 19:20){
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
  gamDEFAULT <- gam(res ~ ti(flo,bs="tp",k=15)+ti(doy,bs="cc",k=15)+ti(dec_time,bs="tp",k=15)+
                      ti(flo,doy,bs=c("tp","cc"))+ti(flo,dec_time,bs=c("tp","tp"))+
                      ti(doy,dec_time,bs=c("cc","tp"))+ti(doy,dec_time,flo,
                                                          bs=c("cc","tp","tp")), data = tmp) ##i=19,20
  modelsNoLag_Nested[[i]]=gamDEFAULT
  names(modelsNoLag_Nested)[i]=paste(mods_nolag$Site_Code[[i]],mods_nolag$resvar[[i]],sep="_")
  
}
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

modelsNoLag_Nested[[i]]=gamDEFAULT
names(modelsNoLag_Nested)[i]=paste(mods_nolag$Site_Code[[i]],mods_nolag$resvar[[i]],sep="_")

save(modelsNoLag_Nested,file="data/modelsNoLag_Nested.RData")
save(modelsNoLag_NoFlow_Nested,file="data/modelsNoLag_NoFlow_Nested.RData")

for(i in 1:length(modelsNoLag_Nested)){
  print(gam.check(modelsNoLag_Nested[[i]]))
  
} ## need to increase ti(flo) ti(dec_time) [most] and three way ti

## check manually, can't find a way to extract k' and edf from this output
## highest edf in the 50s out of 99

for(i in 1:length(modelsNoLag_NoFlow_Nested)){
  print(gam.check(modelsNoLag_NoFlow_Nested[[i]]))
  
}  ## need to increase ti(dec_time) [most] and two way ti
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
gamDEFAULTnoflow <- gam(res ~ ti(doy,bs="cc",k=20)+ti(dec_time,bs="tp",k=23)+
                          ti(doy,dec_time,bs=c("cc","tp")), data = tmp) #k=19
## k=23 is as big as I can get, iffy
gam.check(gamDEFAULTnoflow)
vis.gam(gamDEFAULTnoflow,view=c("doy","dec_time"),plot.type="contour")
vis.gam(gamDEFAULTnoflow)


modelsNoLag_NoFlow_Nested[[i]]=gamDEFAULTnoflow
names(modelsNoLag_NoFlow_Nested)[i]=paste(mods_nolag$Site_Code[[i]],mods_nolag$resvar[[i]],sep="_")


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
gamDEFAULTnoflow <- gam(res ~ ti(doy,bs="cc",k=15)+ti(dec_time,bs="tp",k=20)+
                          ti(doy,dec_time,bs=c("cc","tp")), data = tmp) #k=20
## k=20 is as big as I can get, iffy

modelsNoLag_NoFlow_Nested[[i]]=gamDEFAULTnoflow
names(modelsNoLag_NoFlow_Nested)[i]=paste(mods_nolag$Site_Code[[i]],mods_nolag$resvar[[i]],sep="_")


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
gamDEFAULTnoflow <- gam(res ~ ti(doy,bs="cc",k=20)+ti(dec_time,bs="tp",k=23)+
                          ti(doy,dec_time,bs=c("cc","tp"),k=c(8,8)), data = tmp) #k=21
## k=23 is as big as I can get, iffy

modelsNoLag_NoFlow_Nested[[i]]=gamDEFAULTnoflow
names(modelsNoLag_NoFlow_Nested)[i]=paste(mods_nolag$Site_Code[[i]],mods_nolag$resvar[[i]],sep="_")

save(modelsNoLag_NoFlow_Nested,file="data/modelsNoLag_NoFlow_Nested.RData")
