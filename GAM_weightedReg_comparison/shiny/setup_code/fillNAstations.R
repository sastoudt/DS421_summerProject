
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

mod=mods_nolag_mean$mod[[toRedo[1]]]
out <- mod %>% 
  respred() %>% ## defaults to predicting on data used to fit model
  resnorm

View(out)
names(attributes(out))

dim(dataNiceNoLag[[toRedo[1]]])
sum(!is.na(out$fits))

head(dataNiceNoLag[[toRedo[1]]])

testMerge=merge(dataNiceNoLag[[toRedo[1]]],out,by.x="date",by.y="date",all.x=T)
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



