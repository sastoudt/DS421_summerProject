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

year=seq(min(data$year),max(data$year),by=1)

month=1:12

predGrid=expand.grid(year,month)

predGrid=c()
for(i in 1:12){
  predGrid=rbind(predGrid,expand.grid(i,year,byMonth[[i]]))
  
}
head(predGrid)
dim(predGrid)

View(predGrid)
class(predGrid)
names(predGrid)=c("month","year","flo")

head(data)

dec_time = dec_time(Date)[['dec_time']],
doy = yday(Date)

head(day(data$date),50) ## all the first of the month

## flo, doy, dec_time
predGrid$Date=as.Date(paste(predGrid$year,predGrid$month,"01",sep="-"))
predGrid$dec_time=dec_time(predGrid$Date)[['dec_time']]
predGrid$doy=yday(predGrid$Date)
testPred=predict(modelsNoLag_Nested[[1]],predGrid)
length(testPred)

summary(testPred) ## more compact, to be expected?
summary(data$flo) 

predGrid$testPred=testPred

byYearMonth=group_by(predGrid,month,year)
avgOverFlow=summarise(byYearMonth,meanPred=mean(testPred,na.rm=T))
avgOverFlow

avgOverFlow$Date=as.Date(paste(avgOverFlow$year,avgOverFlow$month,"01",sep="-"))
