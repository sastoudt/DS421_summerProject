#setwd("~/Desktop/EPA_GAMS")
library(dplyr)
library(tidyr)
library(lubridate)
library(WRTDStidal)

setwd("~/Desktop/DS421_summerProject/GAM_weightedReg_comparison/shiny/data")

load("dataNice_nolag.Rdata")
load("modelsNoLag_Nested.RData")

data=dataNiceNoLag[[1]]


head(data)
data=data[!is.na(data$res),]
data=data[!is.na(data$flo),]
data$month=as.numeric(strftime(data$date, '%m'))
data$year=as.numeric(strftime(data$date, '%Y'))
head(data)

byMonth=vector("list",12)

for(i in 1:12){
mon=subset(data,month==i)
byMonth[[i]]=mon$flo
}

summary(modelsNoLag_Nested[[1]])

## for each year, each month all flows for that month


