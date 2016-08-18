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
print(i)
}
setwd("~/Desktop/DS421_summerProject/GAM_weightedReg_comparison/shiny/data")
save(dataNiceNoLag,file="dataNice_nolag.RData")

getSummaryRMSE_adapt2<-function(data,model){
  data=data[!is.nan(data$res),]
  data=data[!is.nan(data$flo),]
  trueVal=data$res ## need to pass this in
  predValGAM=data$gamPred
  predValGAM_big=data$gamPredExpand
  predValWRTDS=data$wrtdsPred
  predValWRTDS1=data$wrtdsPredSmallWindow
  predValWRTDS2=data$wrtdsPredBigWindow
  predValWRTDS3=data$wrtdsPredBiggerWindow
  
  
  rmseGAM=sqrt(sum((trueVal-predValGAM)^2)/sum(!is.na((trueVal-predValGAM)^2)))
  rmseGAMb=sqrt(sum((trueVal-predValGAM_big)^2,na.rm=T)/sum(!is.na((trueVal-predValGAM_big)^2)))
  rmseWRTDS=sqrt(sum((trueVal-predValWRTDS)^2,na.rm=T)/sum(!is.na((trueVal-predValWRTDS)^2)))
  
  rmseWRTDS1=sqrt(sum((trueVal-predValWRTDS1)^2,na.rm=T)/sum(!is.na((trueVal-predValWRTDS1)^2)))
  rmseWRTDS2=sqrt(sum((trueVal-predValWRTDS2)^2,na.rm=T)/sum(!is.na((trueVal-predValWRTDS2)^2)))
  rmseWRTDS3=sqrt(sum((trueVal-predValWRTDS3)^2,na.rm=T)/sum(!is.na((trueVal-predValWRTDS3)^2)))
  
  
  
  data$month=as.numeric(strftime(data$date, '%m'))
  data$year=as.numeric(strftime(data$date, '%Y'))
  
  annual1=subset(data,year<1983 & year>=1976)
  annual1I=which(data$year<1983 & data$year>=1976)
  annual1PG=predValGAM[annual1I]
  annual1PG2=predValGAM_big[annual1I]
  annual1PW=predValWRTDS[annual1I]
  annual1PW1=predValWRTDS1[annual1I]
  annual1PW2=predValWRTDS2[annual1I]
  annual1PW3=predValWRTDS3[annual1I]
  
  
  
  annual2=subset(data,year<1990 & year>=1983)
  annual2I=which(data$year<1990 & data$year>=1983)
  annual2PG=predValGAM[annual2I]
  annual2PG2=predValGAM_big[annual2I]
  annual2PW=predValWRTDS[annual2I]
  annual2PW1=predValWRTDS1[annual2I]
  annual2PW2=predValWRTDS2[annual2I]
  annual2PW3=predValWRTDS3[annual2I]
  
  
  annual3=subset(data,year<1997 & year>=1990)
  annual3I=which(data$year<1997 & data$year>=1990)
  annual3PG=predValGAM[annual3I]
  annual3PG2=predValGAM_big[annual3I]
  annual3PW=predValWRTDS[annual3I]
  annual3PW1=predValWRTDS1[annual3I]
  annual3PW2=predValWRTDS2[annual3I]
  annual3PW3=predValWRTDS3[annual3I]
  
  
  annual4=subset(data,year<2004 & year>=1997)
  annual4I=which(data$year<2004 & data$year>=1997)
  annual4PG=predValGAM[annual4I]
  annual4PG2=predValGAM_big[annual4I]
  annual4PW=predValWRTDS[annual4I]
  annual4PW1=predValWRTDS1[annual4I]
  annual4PW2=predValWRTDS2[annual4I]
  annual4PW3=predValWRTDS3[annual4I]
  
  
  annual5=subset(data,year>=2004) ## has 2 extra years
  annual5I=which( data$year>=2004)
  annual5PG=predValGAM[annual5I]
  annual5PG2=predValGAM_big[annual5I]
  annual5PW=predValWRTDS[annual5I]
  annual5PW1=predValWRTDS1[annual5I]
  annual5PW2=predValWRTDS2[annual5I]
  annual5PW3=predValWRTDS3[annual5I]
  
  
  rmseA1G=sqrt(sum((annual1$res-annual1PG)^2)/sum(!is.na((annual1$res-annual1PG)^2)))
  rmseA2G=sqrt(sum((annual2$res-annual2PG)^2)/sum(!is.na((annual2$res-annual2PG)^2)))
  rmseA3G=sqrt(sum((annual3$res-annual3PG)^2)/sum(!is.na((annual3$res-annual3PG)^2)))
  rmseA4G=sqrt(sum((annual4$res-annual4PG)^2)/sum(!is.na((annual4$res-annual4PG)^2)))
  rmseA5G=sqrt(sum((annual5$res-annual5PG)^2)/sum(!is.na((annual5$res-annual5PG)^2)))
  
  rmseA1Gb=sqrt(sum((annual1$res-annual1PG2)^2)/sum(!is.na((annual1$res-annual1PG2)^2)))
  rmseA2Gb=sqrt(sum((annual2$res-annual2PG2)^2)/sum(!is.na((annual2$res-annual2PG2)^2)))
  rmseA3Gb=sqrt(sum((annual3$res-annual3PG2)^2)/sum(!is.na((annual3$res-annual3PG2)^2)))
  rmseA4Gb=sqrt(sum((annual4$res-annual4PG2)^2)/sum(!is.na((annual4$res-annual4PG2)^2)))
  rmseA5Gb=sqrt(sum((annual5$res-annual5PG2)^2)/sum(!is.na((annual5$res-annual5PG2)^2)))
  
  rmseA1W=sqrt(sum((annual1$res-annual1PW)^2,na.rm=T)/sum(!is.na((annual1$res-annual1PW)^2)))
  rmseA2W=sqrt(sum((annual2$res-annual2PW)^2,na.rm=T)/sum(!is.na((annual2$res-annual2PW)^2)))
  rmseA3W=sqrt(sum((annual3$res-annual3PW)^2,na.rm=T)/sum(!is.na((annual3$res-annual3PW)^2)))
  rmseA4W=sqrt(sum((annual4$res-annual4PW)^2,na.rm=T)/sum(!is.na((annual4$res-annual4PW)^2)))
  rmseA5W=sqrt(sum((annual5$res-annual5PW)^2,na.rm=T)/sum(!is.na((annual5$res-annual5PW)^2)))
  
  rmseA1W1=sqrt(sum((annual1$res-annual1PW1)^2,na.rm=T)/sum(!is.na((annual1$res-annual1PW1)^2)))
  rmseA2W1=sqrt(sum((annual2$res-annual2PW1)^2,na.rm=T)/sum(!is.na((annual2$res-annual2PW1)^2)))
  rmseA3W1=sqrt(sum((annual3$res-annual3PW1)^2,na.rm=T)/sum(!is.na((annual3$res-annual3PW1)^2)))
  rmseA4W1=sqrt(sum((annual4$res-annual4PW1)^2,na.rm=T)/sum(!is.na((annual4$res-annual4PW1)^2)))
  rmseA5W1=sqrt(sum((annual5$res-annual5PW1)^2,na.rm=T)/sum(!is.na((annual5$res-annual5PW1)^2)))
  
  rmseA1W2=sqrt(sum((annual1$res-annual1PW2)^2,na.rm=T)/sum(!is.na((annual1$res-annual1PW2)^2)))
  rmseA2W2=sqrt(sum((annual2$res-annual2PW2)^2,na.rm=T)/sum(!is.na((annual2$res-annual2PW2)^2)))
  rmseA3W2=sqrt(sum((annual3$res-annual3PW2)^2,na.rm=T)/sum(!is.na((annual3$res-annual3PW2)^2)))
  rmseA4W2=sqrt(sum((annual4$res-annual4PW2)^2,na.rm=T)/sum(!is.na((annual4$res-annual4PW2)^2)))
  rmseA5W2=sqrt(sum((annual5$res-annual5PW2)^2,na.rm=T)/sum(!is.na((annual5$res-annual5PW2)^2)))
  
  rmseA1W3=sqrt(sum((annual1$res-annual1PW3)^2,na.rm=T)/sum(!is.na((annual1$res-annual1PW3)^2)))
  rmseA2W3=sqrt(sum((annual2$res-annual2PW3)^2,na.rm=T)/sum(!is.na((annual2$res-annual2PW3)^2)))
  rmseA3W3=sqrt(sum((annual3$res-annual3PW3)^2,na.rm=T)/sum(!is.na((annual3$res-annual3PW3)^2)))
  rmseA4W3=sqrt(sum((annual4$res-annual4PW3)^2,na.rm=T)/sum(!is.na((annual4$res-annual4PW3)^2)))
  rmseA5W3=sqrt(sum((annual5$res-annual5PW3)^2,na.rm=T)/sum(!is.na((annual5$res-annual5PW3)^2)))
  
  seasonal1=subset(data,month %in% c(1:3))
  seasonal1I=which(data$month %in% c(1:3))
  seasonal1PG=predValGAM[seasonal1I]
  seasonal1PG2=predValGAM_big[seasonal1I]
  seasonal1PW=predValWRTDS[seasonal1I]
  seasonal1PW1=predValWRTDS1[seasonal1I]
  seasonal1PW2=predValWRTDS2[seasonal1I]
  seasonal1PW3=predValWRTDS3[seasonal1I]
  
  
  seasonal2=subset(data,month %in% c(4:6))
  seasonal2I=which(data$month %in% c(4:6))
  seasonal2PG=predValGAM[seasonal2I]
  seasonal2PG2=predValGAM_big[seasonal2I]
  seasonal2PW=predValWRTDS[seasonal2I]
  seasonal2PW1=predValWRTDS1[seasonal2I]
  seasonal2PW2=predValWRTDS2[seasonal2I]
  seasonal2PW3=predValWRTDS3[seasonal2I]
  
  
  seasonal3=subset(data,month %in% c(7:9))
  seasonal3I=which(data$month %in% c(7:9))
  seasonal3PG=predValGAM[seasonal3I]
  seasonal3PG2=predValGAM_big[seasonal3I]
  seasonal3PW=predValWRTDS[seasonal3I]
  seasonal3PW1=predValWRTDS1[seasonal3I]
  seasonal3PW2=predValWRTDS2[seasonal3I]
  seasonal3PW3=predValWRTDS3[seasonal3I]
  
  
  seasonal4=subset(data,month %in% c(10:12))
  seasonal4I=which(data$month %in% c(10:12))
  seasonal4PG=predValGAM[seasonal4I]
  seasonal4PG2=predValGAM_big[seasonal4I]
  seasonal4PW=predValWRTDS[seasonal4I]
  seasonal4PW1=predValWRTDS1[seasonal4I]
  seasonal4PW2=predValWRTDS2[seasonal4I]
  seasonal4PW3=predValWRTDS3[seasonal4I]
  
  rmseS1G=sqrt(sum((seasonal1$res-seasonal1PG)^2)/sum(!is.na((seasonal1$res-seasonal1PG)^2)))
  rmseS2G=sqrt(sum((seasonal2$res-seasonal2PG)^2)/sum(!is.na((seasonal2$res-seasonal2PG)^2)))
  rmseS3G=sqrt(sum((seasonal3$res-seasonal3PG)^2)/sum(!is.na((seasonal3$res-seasonal3PG)^2)))
  rmseS4G=sqrt(sum((seasonal4$res-seasonal4PG)^2)/sum(!is.na((seasonal4$res-seasonal4PG)^2)))
  
  rmseS1Gb=sqrt(sum((seasonal1$res-seasonal1PG2)^2)/sum(!is.na((seasonal1$res-seasonal1PG2)^2)))
  rmseS2Gb=sqrt(sum((seasonal2$res-seasonal2PG2)^2)/sum(!is.na((seasonal2$res-seasonal2PG2)^2)))
  rmseS3Gb=sqrt(sum((seasonal3$res-seasonal3PG2)^2)/sum(!is.na((seasonal3$res-seasonal3PG2)^2)))
  rmseS4Gb=sqrt(sum((seasonal4$res-seasonal4PG2)^2)/sum(!is.na((seasonal4$res-seasonal4PG2)^2)))
  
  rmseS1W=sqrt(sum((seasonal1$res-seasonal1PW)^2,na.rm=T)/sum(!is.na((seasonal1$res-seasonal1PW)^2)))
  rmseS2W=sqrt(sum((seasonal2$res-seasonal2PW)^2,na.rm=T)/sum(!is.na((seasonal2$res-seasonal2PW)^2)))
  rmseS3W=sqrt(sum((seasonal3$res-seasonal3PW)^2,na.rm=T)/sum(!is.na((seasonal3$res-seasonal3PW)^2)))
  rmseS4W=sqrt(sum((seasonal4$res-seasonal4PW)^2,na.rm=T)/sum(!is.na((seasonal4$res-seasonal4PW)^2)))
  
  rmseS1W1=sqrt(sum((seasonal1$res-seasonal1PW1)^2,na.rm=T)/sum(!is.na((seasonal1$res-seasonal1PW1)^2)))
  rmseS2W1=sqrt(sum((seasonal2$res-seasonal2PW1)^2,na.rm=T)/sum(!is.na((seasonal2$res-seasonal2PW1)^2)))
  rmseS3W1=sqrt(sum((seasonal3$res-seasonal3PW1)^2,na.rm=T)/sum(!is.na((seasonal3$res-seasonal3PW1)^2)))
  rmseS4W1=sqrt(sum((seasonal4$res-seasonal4PW1)^2,na.rm=T)/sum(!is.na((seasonal4$res-seasonal4PW1)^2)))
  
  rmseS1W2=sqrt(sum((seasonal1$res-seasonal1PW2)^2,na.rm=T)/sum(!is.na((seasonal1$res-seasonal1PW2)^2)))
  rmseS2W2=sqrt(sum((seasonal2$res-seasonal2PW2)^2,na.rm=T)/sum(!is.na((seasonal2$res-seasonal2PW2)^2)))
  rmseS3W2=sqrt(sum((seasonal3$res-seasonal3PW2)^2,na.rm=T)/sum(!is.na((seasonal3$res-seasonal3PW2)^2)))
  rmseS4W2=sqrt(sum((seasonal4$res-seasonal4PW2)^2,na.rm=T)/sum(!is.na((seasonal4$res-seasonal4PW2)^2)))
  
  rmseS1W3=sqrt(sum((seasonal1$res-seasonal1PW3)^2,na.rm=T)/sum(!is.na((seasonal1$res-seasonal1PW3)^2)))
  rmseS2W3=sqrt(sum((seasonal2$res-seasonal2PW3)^2,na.rm=T)/sum(!is.na((seasonal2$res-seasonal2PW3)^2)))
  rmseS3W3=sqrt(sum((seasonal3$res-seasonal3PW3)^2,na.rm=T)/sum(!is.na((seasonal3$res-seasonal3PW3)^2)))
  rmseS4W3=sqrt(sum((seasonal4$res-seasonal4PW3)^2,na.rm=T)/sum(!is.na((seasonal4$res-seasonal4PW3)^2)))
  
  flow1=subset(data,flo<quantile(data$flo,.25))
  flow1I=which(data$flo<quantile(data$flo,0.25))
  flow1PG=predValGAM[flow1I]
  flow1PG2=predValGAM_big[flow1I]
  flow1PW=predValWRTDS[flow1I]
  flow1PW1=predValWRTDS1[flow1I]
  flow1PW2=predValWRTDS2[flow1I]
  flow1PW3=predValWRTDS3[flow1I]
  
  flow2=subset(data,flo>=quantile(data$flo,.25) & flo<quantile(data$flo,0.5))
  flow2I=which(data$flo>=quantile(data$flo,0.25)& data$flo<quantile(data$flo,0.5))
  flow2PG=predValGAM[flow2I]
  flow2PG2=predValGAM_big[flow2I]
  flow2PW=predValWRTDS[flow2I]
  flow2PW1=predValWRTDS1[flow2I]
  flow2PW2=predValWRTDS2[flow2I]
  flow2PW3=predValWRTDS3[flow2I]
  
  flow3=subset(data,flo>=quantile(data$flo,.5) & flo<quantile(data$flo,0.75))
  flow3I=which(data$flo>=quantile(data$flo,0.5)& data$flo<quantile(data$flo,0.75))
  flow3PG=predValGAM[flow3I]
  flow3PG2=predValGAM_big[flow3I]
  flow3PW=predValWRTDS[flow3I]
  flow3PW1=predValWRTDS1[flow3I]
  flow3PW2=predValWRTDS2[flow3I]
  flow3PW3=predValWRTDS3[flow3I]
  
  flow4=subset(data,flo>=quantile(data$flo,.75) )
  flow4I=which(data$flo>=quantile(data$flo,0.75))
  flow4PG=predValGAM[flow4I]
  flow4PG2=predValGAM_big[flow4I]
  flow4PW=predValWRTDS[flow4I]
  flow4PW1=predValWRTDS1[flow4I]
  flow4PW2=predValWRTDS2[flow4I]
  flow4PW3=predValWRTDS3[flow4I]
  
  rmseF1G=sqrt(sum((flow1$res-flow1PG)^2)/sum(!is.na((flow1$res-flow1PG)^2)))
  rmseF2G=sqrt(sum((flow2$res-flow2PG)^2)/sum(!is.na((flow2$res-flow2PG)^2)))
  rmseF3G=sqrt(sum((flow3$res-flow3PG)^2)/sum(!is.na((flow3$res-flow3PG)^2)))
  rmseF4G=sqrt(sum((flow4$res-flow4PG)^2)/sum(!is.na((flow4$res-flow4PG)^2)))
  
  rmseF1Gb=sqrt(sum((flow1$res-flow1PG2)^2)/sum(!is.na((flow1$res-flow1PG2)^2)))
  rmseF2Gb=sqrt(sum((flow2$res-flow2PG2)^2)/sum(!is.na((flow2$res-flow2PG2)^2)))
  rmseF3Gb=sqrt(sum((flow3$res-flow3PG2)^2)/sum(!is.na((flow3$res-flow3PG2)^2)))
  rmseF4Gb=sqrt(sum((flow4$res-flow4PG2)^2)/sum(!is.na((flow4$res-flow4PG2)^2)))
  
  rmseF1W=sqrt(sum((flow1$res-flow1PW)^2,na.rm=T)/sum(!is.na((flow1$res-flow1PW)^2)))
  rmseF2W=sqrt(sum((flow2$res-flow2PW)^2,na.rm=T)/sum(!is.na((flow2$res-flow2PW)^2)))
  rmseF3W=sqrt(sum((flow3$res-flow3PW)^2,na.rm=T)/sum(!is.na((flow3$res-flow3PW)^2)))
  rmseF4W=sqrt(sum((flow4$res-flow4PW)^2,na.rm=T)/sum(!is.na((flow4$res-flow4PW)^2)))
  
  rmseF1W1=sqrt(sum((flow1$res-flow1PW1)^2,na.rm=T)/sum(!is.na((flow1$res-flow1PW1)^2)))
  rmseF2W1=sqrt(sum((flow2$res-flow2PW1)^2,na.rm=T)/sum(!is.na((flow2$res-flow2PW1)^2)))
  rmseF3W1=sqrt(sum((flow3$res-flow3PW1)^2,na.rm=T)/sum(!is.na((flow3$res-flow3PW1)^2)))
  rmseF4W1=sqrt(sum((flow4$res-flow4PW1)^2,na.rm=T)/sum(!is.na((flow4$res-flow4PW1)^2)))
  
  rmseF1W2=sqrt(sum((flow1$res-flow1PW2)^2,na.rm=T)/sum(!is.na((flow1$res-flow1PW2)^2)))
  rmseF2W2=sqrt(sum((flow2$res-flow2PW2)^2,na.rm=T)/sum(!is.na((flow2$res-flow2PW2)^2)))
  rmseF3W2=sqrt(sum((flow3$res-flow3PW2)^2,na.rm=T)/sum(!is.na((flow3$res-flow3PW2)^2)))
  rmseF4W2=sqrt(sum((flow4$res-flow4PW2)^2,na.rm=T)/sum(!is.na((flow4$res-flow4PW2)^2)))
  
  rmseF1W3=sqrt(sum((flow1$res-flow1PW3)^2,na.rm=T)/sum(!is.na((flow1$res-flow1PW3)^2)))
  rmseF2W3=sqrt(sum((flow2$res-flow2PW3)^2,na.rm=T)/sum(!is.na((flow2$res-flow2PW3)^2)))
  rmseF3W3=sqrt(sum((flow3$res-flow3PW3)^2,na.rm=T)/sum(!is.na((flow3$res-flow3PW3)^2)))
  rmseF4W3=sqrt(sum((flow4$res-flow4PW3)^2,na.rm=T)/sum(!is.na((flow4$res-flow4PW3)^2)))
  
  rmseG=rbind(rmseGAM,rmseA1G,rmseA2G,rmseA3G,rmseA4G,rmseA5G,rmseS1G,rmseS2G,rmseS3G,rmseS4G,
              rmseF1G,rmseF2G,rmseF3G,rmseF4G)
  
  rmseGb=rbind(rmseGAMb,rmseA1Gb,rmseA2Gb,rmseA3Gb,rmseA4Gb,rmseA5Gb,rmseS1Gb,rmseS2Gb,rmseS3Gb,
               rmseS4Gb,rmseF1Gb,rmseF2Gb,rmseF3Gb,rmseF4Gb)
  
  rmseW=rbind(rmseWRTDS,rmseA1W,rmseA2W,rmseA3W,rmseA4W,rmseA5W,rmseS1W,rmseS2W,rmseS3W,rmseS4W,
              rmseF1W,rmseF2W,rmseF3W,rmseF4W)
  
  rmseW1=rbind(rmseWRTDS1,rmseA1W1,rmseA2W1,rmseA3W1,rmseA4W1,rmseA5W1,rmseS1W1,rmseS2W1,rmseS3W1,
               rmseS4W1,
              rmseF1W1,rmseF2W1,rmseF3W1,rmseF4W1)
  
  rmseW2=rbind(rmseWRTDS2,rmseA1W2,rmseA2W2,rmseA3W2,rmseA4W2,rmseA5W2,rmseS1W2,rmseS2W2,rmseS3W2,
               rmseS4W2,
              rmseF1W2,rmseF2W2,rmseF3W2,rmseF4W2)
  
  rmseW3=rbind(rmseWRTDS3,rmseA1W3,rmseA2W3,rmseA3W3,rmseA4W3,rmseA5W3,rmseS1W3,rmseS2W3,rmseS3W3,
               rmseS4W3,
              rmseF1W3,rmseF2W3,rmseF3W3,rmseF4W3)
  return(cbind(rmseG,rmseGb,rmseW,rmseW1,rmseW2,rmseW3))
  # return(list(all=rmse,annual1=rmseA1,annual2=rmseA2,annual3=rmseA3,annual4=rmseA4,
  #             seasonal1=rmseS1, seasonal2=rmseS2, seasonal3=rmseS3, seasonal4=rmseS4,
  #             flow1=rmseF1,flow2=rmseF2,flow3=rmseF3,flow4=rmseF4))
}

df=getSummaryRMSE_adapt2(dataNiceNoLag[[16]],1)
## gam, gam big, wrtds, small window, big window, bigger window

apply(df,1,which.min) ## gam big
apply(df[,3:6],1,which.min) ## small window

df1=getSummaryRMSE_adapt2(dataNiceNoLag[[17]],1)

apply(df1,1,which.min)
apply(df1[,3:6],1,which.min) ## mostly 2 (small window)


df2=getSummaryRMSE_adapt2(dataNiceNoLag[[18]],1)

apply(df2,1,which.min)
apply(df2[,3:6],1,which.min) ## mostly 2 (small window)


df3=getSummaryRMSE_adapt2(dataNiceNoLag[[19]],1)

apply(df3,1,which.min)
apply(df3[,3:6],1,which.min) ## mostly 2 (small window)


df4=getSummaryRMSE_adapt2(dataNiceNoLag[[20]],1)

apply(df4,1,which.min)
apply(df4[,3:6],1,which.min) ## mostly 2 (small window)


df5=getSummaryRMSE_adapt2(dataNiceNoLag[[21]],1)

apply(df5,1,which.min)
apply(df5[,3:6],1,which.min) ## mostly 2 (small window)


## gam big is always better (conclusion the same after bug fix)

