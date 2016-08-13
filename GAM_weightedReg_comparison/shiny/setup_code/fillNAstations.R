
### filling in NAs for
## D4din, D4nh, D4no23, D6din, D6nh, D6no23, D7din, D7nh, D7no23

setwd( "/Users/Sara/Desktop/DS421_summerProject/GAM_weightedReg_comparison/shiny/data")
load("dataNice_nolag.RData") 

setwd("~/Desktop/EPA_GAMS")
library(dplyr)
library(tidyr)
library(lubridate)
library(WRTDStidal)
load("mods_nolag_mean.RData") 
names(dataNiceNoLag)

toRedo=which(names(dataNiceNoLag) %in% c("D4_din", "D4_nh", "D4_no23", "D6_din",
                                  "D6_nh", "D6_no23", "D7_din", "D7_nh", "D7_no23"))
dim(na.omit(mods_nolag_mean$data[[10]])) ## 447


mod=mods_nolag_mean$mod[[toRedo[1]]]
out <- mod %>% 
  respred() %>% ## defaults to predicting on data used to fit model
  resnorm
test=as.data.frame(mods_nolag_mean$data[[10]][,c("Date","flolag")])
names(test)=c("date","flo")

out <- mod %>% 
  respred(test) %>% ## defaults to predicting on data used to fit model
  resnorm

View(out)
names(attributes(out))

dim(dataNiceNoLag[[toRedo[1]]])
sum(!is.na(out$fits))

head(dataNiceNoLag[[toRedo[1]]])

testMerge=merge(dataNiceNoLag[[toRedo[1]]],out,by.x="date",by.y="date",all.x=T,all.y)
dim(testMerge)
View(testMerge)

sum(!is.na(testMerge$fits)) ## only 4

###
mod=mods_nolag_mean$mod[[toRedo[2]]]
out <- mod %>% 
  respred() %>% ## defaults to predicting on data used to fit model
  resnorm

dim(dataNiceNoLag[[toRedo[2]]])
sum(!is.na(out$fits))


testMerge=merge(dataNiceNoLag[[toRedo[2]]],out,by.x="date",by.y="date",all.x=T)
dim(testMerge)
View(testMerge)

sum(!is.na(testMerge$fits)) ## only 4

###
mod=mods_nolag_mean$mod[[toRedo[3]]]
out <- mod %>% 
  respred() %>% ## defaults to predicting on data used to fit model
  resnorm

dim(dataNiceNoLag[[toRedo[3]]])
sum(!is.na(out$fits))


testMerge=merge(dataNiceNoLag[[toRedo[3]]],out,by.x="date",by.y="date",all.x=T)
dim(testMerge)
View(testMerge)

sum(!is.na(testMerge$fits)) ## only 4
## this is what we had before

#### try from scratch

i=toRedo[1]
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
tryThis=modfit(tomod,resp_type="mean")
plot(density(tryThis$res-tryThis$fit0.5))

summary(tryThis$res)
summary(tryThis$fit0.5)
dim(tryThis)
dim(tomod)
sum(!is.na(tryThis$fit0.5))

head(dataNiceNoLag[[10]])
head(tryThis)

dataNiceNoLag[[10]]$wrtdsPred=tryThis$fit0.5 ## this works

######### 
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

for(i in toRedo){
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
  tryThis=modfit(tomod,resp_type="mean")
  
  test=merge(dataNiceNoLag[[i]],tryThis[,c("date","fit0.5")],by.x="date",by.y="date",all.x=T)
  
  dataNiceNoLag[[i]]$wrtdsPred=test$fit0.5 ## this works
  
}