
load("data/mods_nolag.RData")
load("data/dataNice_nolag.RData")

modelsNoLag_default=vector("list",length(dataNiceNoLag))
modelsNoLag_NoFlow_default=vector("list",length(dataNiceNoLag))
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
  gamDEFAULT <- gam(res ~ te(dec_time, doy, flo, bs = c("tp", "cc", "tp")), data = tmp)
  gamDEFAULTnoflow <- gam(res ~ te(dec_time, doy, bs = c("tp", "cc")), data = tmp)
  
#  gamtmp <- gam(res ~ te(dec_time, doy, flo, bs = c("tp", "cc", "tp")), k = c(5, 8, 5), data = tmp, knots = list(doy = c(1, 366)))
  
  modelsNoLag_default[[i]]=gamDEFAULT
  modelsNoLag_NoFlow_default[[i]]=gamDEFAULTnoflow
  names(modelsNoLag_default)[i]=paste(mods_nolag$Site_Code[[i]],mods_nolag$resvar[[i]],sep="_")
  names(modelsNoLag_NoFlow_default)[i]=paste(mods_nolag$Site_Code[[i]],mods_nolag$resvar[[i]],sep="_")
  print(i)
}

save(modelsNoLag_default,file="modelsNoLag_default.RData")
save(modelsNoLag_NoFlow_default,file="modelsNoLag_NoFlow_default.RData")
