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


View(volFlow)
## mean of each column by month/year

volFlow$year=year(volFlow$TIME)
volFlow$month=month(volFlow$TIME)

head(volFlow[,c("year","month")])

require(dplyr)

byMonYr<-group_by(volFlow,year,month)

test=summarise(byMonYr,mean(D10.AG))
head(test)

aggdata <-aggregate(volFlow[,-1], by=list(volFlow$year,volFlow$month), 
                    FUN=mean, na.rm=TRUE)


### get a feel for yearly cycle one station

aggdata <-aggregate(volFlow[,c(3:8)], by=list(volFlow$month), 
                    FUN=mean, na.rm=TRUE)


toPlot=as.table(t(aggdata[,-1]))
#colnames(toPlot)=c(1:12)

barplot(as.matrix(toPlot))

library(reshape2)
toPlot$row<-seq_len(6)
toPlot2<-melt(toPlot,id.vars="row")

require(ggplot2)

ggplot(toPlot2,aes(x=variable,y=value,fill="row"))+geom_bar(stat="identity")
