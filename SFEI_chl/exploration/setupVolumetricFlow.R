setwd("~/Desktop/sfei")
load(file="perStation.Rda")
allData<-read.csv("allData.csv")

require(lubridate)

head(perStation[[1]]$Date,20) ## looks like 1-14, 15-on would be a safe break


perStation[[1]]$month=month(perStation[[1]]$Date)
table(perStation[[1]]$month) ## more than once a month in April through October
length(unique(year(perStation[[1]]$Date))) ## 40 years

perStation[[1]]$day=day(perStation[[1]]$Date)

tryThis=subset(perStation[[1]],month %in% c(4:10))
tryThis$day[seq(2,nrow(tryThis),by=2)] ## can be more precise, but I think for now just go with
## 1-14, 15-on


hist(tryThis$day)

summary(perStation[[1]]$doy)

setwd("~/Desktop/sfei")
volFlow<-read.csv("VolFingerPrintsMaster.csv")

head(volFlow)
volFlow=volFlow[,-c(seq(8,ncol(volFlow),by=7))] ## remove blank columns

names(volFlow)
head(volFlow)

class(volFlow$TIME)

volFlow$TIME=as.Date(as.character(volFlow$TIME),format="%d-%B-%y")
head(volFlow$TIME)
class(volFlow$TIME)

