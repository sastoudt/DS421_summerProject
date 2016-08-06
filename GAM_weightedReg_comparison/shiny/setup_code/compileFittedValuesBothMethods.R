#setwd("~/Desktop/EPA_GAMS")
library(dplyr)
library(tidyr)
library(lubridate)
library(WRTDStidal)

load("mods_nolag_mean.RData") 
##https://github.com/fawda123/sf_trends/blob/master/data/mods_nolag_mean.RData
## from Marcus

load("data/dataNice_nolag.RData") 
class(mods_nolag_mean)

mods_nolag_mean[,1] ## location delta/suisun
mods_nolag_mean[,2] ## station code
mods_nolag_mean[,3] ## response variable
mods_nolag_mean[,4] ## flovar
mods_nolag_mean[,5] ## data
mods_nolag_mean[,6] ## mod

mods_nolag_mean$data[[1]]
# mods_nolag$data
# 
tmp <- mods_nolag_mean$data[[1]] %>%
  mutate(
    dec_time = dec_time(Date)[['dec_time']],
    doy = yday(Date)
  ) %>%
  rename(
    res = resval,
    flo = flolag,
    date = Date
  )

head(tmp)
# View(tmp)

names(mods_nolag_mean$mod[[1]])
head(mods_nolag_mean$mod[[1]]$fits) ## fits to the data
head(mods_nolag_mean$mod[[1]]$bt_fits) ## back transformed
head(mods_nolag_mean$mod[[1]]$norm) ## using interpolation grid
head(mods_nolag_mean$mod[[1]]$bt_norm) ## interpolation grid back transformed


#load("tidfitmean.RData")
#names(tidfitmean)

summary(tmp$res)
summary(mods_nolag_mean$mod[[1]]$fits) ## check same scale
length(which(!is.na(mods_nolag_mean$mod[[1]]$res))) 

length(!is.na(tmp$res))


# head(tmp)
# 
# testM=merge(tmp,mods_nolag_mean$mod[[1]],by.x="date",by.y="date",all.x=T)
# View(testM)

## figure out dimensions
length(which(!is.nan(tmp$res))) ##434
dim(mods_nolag_mean$mod[[1]])
length(which(!is.na(mods_nolag_mean$mod[[1]]$res))) ##434

#View(mods_nolag_mean$mod[[1]])

## add predicted values to data themselves for comparison- WRTDSmean
for(i in 1:length(dataNiceNoLag)){
  dataNiceNoLag[[i]]$wrtdsPred=rep(NA,nrow(dataNiceNoLag[[i]]))
  dataNiceNoLag[[i]]$wrtdsPred[which(!is.nan(dataNiceNoLag[[i]]$res) & !is.nan(dataNiceNoLag[[i]]$flo))]=mods_nolag_mean$mod[[i]]$fits[which(!is.na(mods_nolag_mean$mod[[i]]$res))]
print(i)
  }


load("data/modelsNoLag_Nested.RData")

names(modelsNoLag_Nested[[1]])
length(modelsNoLag_Nested[[1]]$fitted.values) ## 434

## add predicted values to data themselves- GAM flow

for(i in 1:length(dataNiceNoLag)){
  dataNiceNoLag[[i]]$gamPred=rep(NA,nrow(dataNiceNoLag[[i]]))
  dataNiceNoLag[[i]]$gamPred[which(!is.nan(dataNiceNoLag[[i]]$res) & !is.nan(dataNiceNoLag[[i]]$flo))]=modelsNoLag_Nested[[i]]$fitted.values
  print(i)
}

save(dataNiceNoLag,file="dataNice_nolag.RData")

