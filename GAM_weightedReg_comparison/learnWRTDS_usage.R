## get a feel for working with WRTDStidal


setwd("~/Desktop/EPA_GAMS")
library(dplyr)
library(tidyr)
library(lubridate)
library(WRTDStidal)

load("mods_nolag_mean.RData") 
names(attributes(mods_nolag_mean$mod[[1]]))
attr(mods_nolag_mean$mod[[1]],"half_wins")
## half a month, 10 years, 0.5 (flow/sal)
##https://github.com/fawda123/sf_trends/blob/master/data/mods_nolag_mean.RData
## from Marcus

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

i=1
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

mod <- wrtds(tomod, tau = c(0.1, 0.5, 0.9), wins = list(0.5, 10, 0.5), flo_div = 30, min_obs = 150)


mod=mods_nolag_mean$mod[[1]]
out <- mod %>% 
  respred(dat_pred = tomod[,c("date","flo")]) %>% 
  resnorm


out <- mod %>% 
  respred() %>% ## defaults to predicting on data used to fit model
  resnorm

row.names(tomod[,c("date","flo")])

class(tomod$date)
tryThis=tidalmean(tomod[,1:4],indices=c(1:4))

mod <- wrtds(tomod, wins = list(0.5, 10, 0.5), flo_div = 30, min_obs = 150)

tryThis=modfit(tomod,resp_type="mean")
attr(tryThis,"half_wins") ## default is 0.5, 10, 0.5
