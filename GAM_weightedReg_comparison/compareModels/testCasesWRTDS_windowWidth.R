## "representative" test sites
##  D7, D19

setwd("~/Desktop/DS421_summerProject/GAM_weightedReg_comparison/shiny/data")
load("dataNice_nolag.RData") 
load("modelsNoLag_Nested.RData")
load("mods_nolag.RData")

grep("D7",names(dataNiceNoLag)) ## 16, 17, 18
grep("D19",names(dataNiceNoLag)) ## 19, 20, 21

setwd("~/Desktop/EPA_GAMS")
library(dplyr)
library(tidyr)
library(lubridate)
library(WRTDStidal)

load("mods_nolag_mean.RData") 

setwd("~/Desktop/DS421_summerProject/GAM_weightedReg_comparison/shiny/data")
load("dataNice_nolag.RData") 
load("delt_dat.RData")
load("flow_dat.RData")
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

for(i in 16:21){
dat <- mods_nolag_mean[i, ]$data[[1]]
resvar <- mods_nolag_mean[i, ]$resvar
flovar <- mods_nolag_mean[i, ]$flovar
sta <- mods_nolag_mean[i, ]$Site_Code
reslab <- with(lablk, lngs[shrt == resvar])
flolab <- with(stalk, shrt[lngs == flovar])

tomod <- select(dat, Date, resval, flolag, lim) %>% 
  rename(
    res = resval, 
    flo = flolag
  ) %>% 
  data.frame %>% 
  tidal(., 
        reslab = reslab, 
        flolab = expression(paste('ln-flow (standardized)'))
  )


if(flolab == 'sal'){
  
  topred <- filter(delt_dat, Site_Code == sta) %>% 
    mutate(flo = log(1 + sal)) %>% # salinity is only variable with zeroes
    rename(date = Date) %>% 
    select(date, flo) %>% 
    filter(date >= min(tomod$date) & date <= max(tomod$date)) %>% 
    na.omit %>% 
    data.frame
  
} else {
  
  topred <- filter(flow_dat, station == flolab) %>% 
    mutate(flo = log(q)) %>% 
    rename(date = Date) %>% 
    select(date, flo) %>% 
    filter(date >= min(tomod$date) & date <= max(tomod$date)) %>% 
    na.omit %>% 
    data.frame
  
  # if D19, remove gap in time series with no data
  if(sta == 'D19'){
    
    dts <- as.Date(c('1995-12-01', '2004-05-01')) # looked at orig record for this
    
    torm <- with(topred, date > dts[1] & date < dts[2])
    topred$flo[torm] <- NA
    
  }
  
}

tryThis=modfit(tomod,resp_type="mean",wins=list(0.25, 5, 0.25)) ## months, years, salinity/flow
## default is 0.5, 10, 0.5
## do one set smaller windows, one set larger windows

dataNiceNoLag[[i]]=merge(dataNiceNoLag[[i]],tryThis[,c("date","fit0.5")],all.x=T)
names(dataNiceNoLag[[i]])[ncol(dataNiceNoLag[[i]])]="wrtdsPredSmallWindow"

tryThis=modfit(tomod,resp_type="mean",wins=list(0.75, 15, 0.75)) ## months, years, salinity/flow
dataNiceNoLag[[i]]=merge(dataNiceNoLag[[i]],tryThis[,c("date","fit0.5")],all.x=T)
names(dataNiceNoLag[[i]])[ncol(dataNiceNoLag[[i]])]="wrtdsPredBigWindow"

tryThis=modfit(tomod,resp_type="mean",wins=list(1, 20, 1))  ## too extreme?

dataNiceNoLag[[i]]=merge(dataNiceNoLag[[i]],tryThis[,c("date","fit0.5")],all.x=T)
names(dataNiceNoLag[[i]])[ncol(dataNiceNoLag[[i]])]="wrtdsPredBiggerWindow"
}

