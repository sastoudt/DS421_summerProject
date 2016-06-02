library(mgcv)
library(dplyr)
library(tidyr)
library(lubridate)
library(WRTDStidal)
setwd("~/Desktop/DS421_summerProject/GAM_weightedReg_comparison/shiny")
load("data/modelsNoLag_default.RData")
summary(modelsNoLag_default[[1]])

attributes(modelsNoLag_default[[1]])
gam.check(modelsNoLag_default[[1]])

plot(residuals(modelsNoLag_default[[1]]))

names(summary(modelsNoLag_default[[1]]))
summary(modelsNoLag_default[[1]])$edf

choose.k(modelsNoLag_default[[1]])

mod1=modelsNoLag_default[[1]]


load("data/mods_nolag.RData")
load("data/dataNice_nolag.RData")

i=1
  
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
  gamDEFAULT <- gam(res ~ ti(dec_time,bs="tp")+ti(doy,bs="cc")+ti(flo,bs="tp")+ti(dec_time,doy,bs=c("tp","cc"))+ti(dec_time,flo,bs=c("tp","tp"))+ti(doy,flo,bs=c("cc","tp"))+ti(dec_time,doy,flo,bs = c("tp", "cc", "tp")), data = tmp)
  #gamDEFAULTnoflow <- gam(res ~ te(dec_time, doy, bs = c("tp", "cc")), data = tmp)
  
  #  gamtmp <- gam(res ~ te(dec_time, doy, flo, bs = c("tp", "cc", "tp")), k = c(5, 8, 5), data = tmp, knots = list(doy = c(1, 366)))
  
  summary(mod1$fitted.values-gamDEFAULT$fitted.values)
  
  summary(gamDEFAULT)
  
  gam.check(gamDEFAULT)
#   k'    edf k-index p-value
#   ti(dec_time)          4.000  3.712   0.887    0.02
#   ti(doy)               3.000  2.719   0.901    0.00
#   ti(flo)               4.000  4.000   0.874    0.02
#   ti(dec_time,doy)     12.000  7.819   0.894    0.00
#   ti(dec_time,flo)     16.000  7.984   1.040    0.80
#   ti(doy,flo)          12.000  8.341   0.963    0.21
#   ti(dec_time,doy,flo) 48.000 23.213   1.037    0.80
  
gamDEFAULT_2 <- gam(res ~ ti(dec_time,bs="tp",k=10)+ti(doy,bs="cc",k=10)+ti(flo,bs="tp",k=10)+ti(dec_time,doy,bs=c("tp","cc"))+ti(dec_time,flo,bs=c("tp","tp"))+ti(doy,flo,bs=c("cc","tp"))+ti(dec_time,doy,flo,bs = c("tp", "cc", "tp")), data = tmp)
  
gam.check(gamDEFAULT_2)

gamDEFAULT_3 <- gam(res ~ ti(dec_time,bs="cr")+ti(doy,bs="cc")+ti(flo,bs="c")+ti(dec_time,doy,bs=c("tp","cc"))+ti(dec_time,flo,bs=c("tp","tp"))+ti(doy,flo,bs=c("cc","tp"))+ti(dec_time,doy,flo,bs = c("tp", "cc", "tp")), data = tmp)

 