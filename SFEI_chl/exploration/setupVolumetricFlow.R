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

aggdata <-aggregate(volFlow[,c(2:7)], by=list(volFlow$month), 
                    FUN=mean, na.rm=TRUE)

toPlot=t(aggdata[,-1])
barplot(as.matrix(toPlot))


toPlot=as.data.frame(t(aggdata[,-1]))
#colnames(toPlot)=c(1:12)
names(toPlot)=c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sept","Oct","Nov","Dec")

library(reshape2)
toPlot$row<-seq_len(nrow(toPlot))
toPlot2<-melt(toPlot,id.vars="row")

require(ggplot2)

ggplot(toPlot2,aes(x=variable,y=value,fill=as.factor(row)))+geom_bar(stat="identity")+
  scale_fill_discrete("Volumetric Fingerprint",labels=c("AG","East","Jones","MTZ","SAC","SJR"))+
  xlab("")+ylab("% contribution")+
  ggtitle("Average Composition of D10")


makeCompChart=function(data,stationID,month=T){
  toPlot=as.data.frame(data)
 
  if(month){
  names(toPlot)=c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sept","Oct","Nov","Dec")
  }else{
    names(toPlot)=c(1991:2006)
  }
  
  toPlot$row<-seq_len(nrow(toPlot))
  toPlot2<-melt(toPlot,id.vars="row")
  
  ggplot(toPlot2,aes(x=variable,y=value,fill=as.factor(row)))+geom_bar(stat="identity")+
    scale_fill_discrete("Volumetric Fingerprint",labels=c("AG","East","Jones","MTZ","SAC","SJR"))+
    xlab("")+ylab("% contribution")+
    ggtitle(paste("Average Composition of",stationID,sep=" "))
  
}

makeCompChart(t(aggdata[,-1]),"D10")


aggdata <-aggregate(volFlow[,c(3:8)], by=list(volFlow$year), 
                    FUN=mean, na.rm=TRUE)
makeCompChart(t(aggdata[,-1]),"D10",F)


#### stations share similar fingerprint
compareOverall=apply(volFlow[,-c(1,86,87)],2,function(x){mean(as.numeric(as.character(x)),na.rm=T)})

compareOverall[grepl("AG",names(compareOverall))] ## all pretty low but MD10A
compareOverall[grepl("EAST",names(compareOverall))] ## all pretty low but P8
compareOverall[grepl("JONES",names(compareOverall))] ## all basically zero
compareOverall[grepl("MTZ",names(compareOverall))] ## high for all but D22, D26, D28, P8, D16, MD10A
## majority for D6, D7
compareOverall[grepl("SAC",names(compareOverall))] ## really high except for D6, P8
## majority for D10, D12, D22, D26, D28A, D4, NZ032, NZS42
compareOverall[grepl("SJR",names(compareOverall))] ## moderate except for P8 (majority), MD10A

which.max(compareOverall[grepl("D10",names(compareOverall))]) ## SAC
which.max(compareOverall[grepl("D12",names(compareOverall))]) ## SAC
which.max(compareOverall[grepl("D22",names(compareOverall))]) ## SAC
which.max(compareOverall[grepl("D26",names(compareOverall))]) ## SAC
which.max(compareOverall[grepl("D28A",names(compareOverall))]) ##SAC
which.max(compareOverall[grepl("D4",names(compareOverall))]) ## SAC
which.max(compareOverall[grepl("D6",names(compareOverall))]) ## MTZ
which.max(compareOverall[grepl("D7",names(compareOverall))]) ## MTZ
which.max(compareOverall[grepl("D8",names(compareOverall))]) ##SAC
which.max(compareOverall[grepl("P8",names(compareOverall))]) ## SJR
which.max(compareOverall[grepl("MD10A",names(compareOverall))]) ## SJR same as MD10?

plot(allData$Longitude,allData$Latitude,type="n")
text(allData$Longitude,allData$Latitude, allData$Station)

      
