load("data/flow_dat.RData")
load("data/delt_dat.RData")
load("data/mods_nolag.RData")
library(dplyr)
library(WRTDStidal)

lablk <- list(
  shrt = c('din', 'nh', 'no23'),
  lngs = c(
    expression(paste('ln-dissolved inorganic nitrogen (mg ', L^-1, ')')),
    expression(paste('ln-ammonium (mg ', L^-1, ')')),
    expression(paste('ln-nitrite/nitrate (mg ', L^-1, ')'))
  )
)

stalk <- list(
  shrt = c('sjr', 'sac', 'sal'),
  lngs = c('San Joaquin', 'Sacramento', 'Salinity')
)

dataNiceNoLag=vector("list", nrow(mods_nolag))
dataPredNiceNoLag=vector("list",nrow(mods_nolag))
for(i in 1:nrow(mods_nolag)){
# data, respons variable label
dat <- mods_nolag[i, ]$data[[1]]
resvar <- mods_nolag[i, ]$resvar
flovar <- mods_nolag[i, ]$flovar
sta <- mods_nolag[i, ]$Site_Code
reslab <- with(lablk, lngs[shrt == resvar])
flolab <- with(stalk, shrt[lngs == flovar])

tomod<- mods_nolag$data[[i]] %>% 
  mutate(
    dec_time = dec_time(Date)[['dec_time']],
    doy = yday(Date)
  ) %>% 
  rename(
    res = resval, 
    flo = flolag,
    date = Date
  )%>% 
  data.frame

# get flo or salinity variable to predict 
if(flolab == 'sal'){
  
  topred <- filter(delt_dat, Site_Code == sta) %>% 
    mutate(flo = log(1 + sal)) %>% # salinity is only variable with zeroes
    rename(date = Date) %>% 
    select(date, flo) %>% 
    filter(date >= min(tomod$Date,na.rm=T) & date <= max(tomod$Date,na.rm=T)) %>% 
    na.omit %>% 
    data.frame
  
} else {
  
  topred <- filter(flow_dat, station == flolab) %>% 
    mutate(flo = log(q)) %>% 
    rename(date = Date) %>% 
    select(date, flo) %>% 
    filter(date >= min(tomod$Date,na.rm=T) & date <= max(tomod$Date,na.rm=T)) %>% 
    na.omit %>% 
    data.frame
  
  # if D19, remove gap in time series with no data
  if(sta == 'D19'){
    
    dts <- as.Date(c('1995-12-01', '2004-05-01')) # looked at orig record for this
    
    torm <- with(topred, date > dts[1] & date < dts[2])
    topred$flo[torm] <- NA
    
  }
  
}

dataNiceNoLag[[i]]=tomod
dataPredNiceNoLag[[i]]=topred
names(dataNiceNoLag)[i]=paste(sta,resvar,sep="_")
names(dataPredNiceNoLag)[i]=paste(sta,resvar,sep="_")
print(i)
}
setwd("~/Desktop/EPA_GAMS/shiny")
save(dataNiceNoLag,file="dataNice_nolag.RData")
save(dataPredNiceNoLag,file="dataPredNice_nolag.RData")
