## http://www.water.ca.gov/dayflow/
wholeSeries<-c(1, 2, 5, 7, 11, 13, 15, 16, 17, 18, 21, 22, 23, 29, 40)

require(lubridate)
for(i in c(1997:2015)){
  data=read.csv(paste("http://www.water.ca.gov/dayflow/docs/dayflowCalculations",i,".csv",sep=""),row.names=NULL,stringsAsFactors=F)
  print(head(data$Mo))
} ## different date formats across different files, joy



library(data.table)
data=c()
for(i in c(1997:2013,2015)){
  dataH=read.csv(paste("http://www.water.ca.gov/dayflow/docs/dayflowCalculations",i,".csv",sep=""),row.names=NULL,stringsAsFactors=F)
ind=which(dataH[,1]=="monthly totals")
dataH=dataH[1:(ind-1),]
dataH$Mo=dmy(dataH$Mo)
data=rbind(data,dataH)
  print(ind)
  }

myfile=read.csv("http://www.water.ca.gov/dayflow/docs/dayflowCalculations2014.csv",row.names=NULL,stringsAsFactors=F)
myfile$row.names=rep(NA,nrow(myfile))
which(myfile[,1]=="monthly totals")

myfile=myfile[,c(ncol(myfile),1:(ncol(myfile)-1))]
myfile$Mo=as.Date(myfile$Date,"%d-%h-%y")
data=rbind(data,myfile[1:(ind-1),])

#apply(data,2,class)

setwd("~/Desktop/sfei")

seventies<-read.csv("wy1970-1983.csv",stringsAsFactors=F)

eighties<-read.csv("wy1984-1996.csv",stringsAsFactors=F)

ncol(seventies)
ncol(eighties)
names(seventies)
names(data)

seventies$extra=rep(NA,nrow(seventies))
eighties$extra=rep(NA,nrow(eighties))
names(seventies)=names(data)
names(eighties)=names(data)

seventies$Mo=as.Date(seventies$Date,"%d-%h-%y")
eighties$Mo=as.Date(eighties$Date,"%d-%h-%y")

data2=rbind(data,seventies,eighties)
row.names(data2)=NULL

data2=data2[,-c(1,ncol(data2))]
names(data2)
data2=data2[,-c(1,3)]
names(data2)

which(is.na(data2$Mo)) ## woo

names(data2)[1]="Date"

write.csv(data2,"flowData.csv",row.names=F)
setwd("~/Desktop/sfei")
flowData<-read.csv("flowData.csv",stringsAsFactors=F)
## merge total flow to everything, by closest date


load("perStationAdd.Rda")
load("perStation.Rda")
perStationAdd=perStation
for(i in wholeSeries){
 
 testMerge=merge(perStationAdd[[i]],flowData,by.x="Date",by.y="Date",all.x=T)
 
 # setDT(perStationAdd[[i]])
 # setDT(flowData)
 # 
 # flowData$Date=as.Date(flowData$Date)
 # perStationAdd2[[i]]$Date=as.Date(perStationAdd2[[i]]$Date)
 # 
 # setkey(perStationAdd[[i]], Date)[, dateMatch:=Date]
 # test=perStationAdd[[i]][flowData, roll='nearest']
 # test$Date=as.Date(test$Date)
 # test$dateMatch=as.Date(test$dateMatch)
 # 
 # test=as.data.frame(test)
 # 
 perStationAdd[[i]]=testMerge
 print(i)
}
save(perStationAdd,file="perStationAdd.Rda")
## then merge more strategically by station based on maximum avg contribution from volumetric data


plot(density(as.numeric(data2$TOT)))

## SAC =  RIO + YOLO
## D10, D12, D22, D26, D28A, D4, D8
## SJR = WEST 
## P8, MD10A

## Skip D6, D7 for now. What is a reasonable equivalent of MTZ?



# require(tidyr)
# require(dplyr)
# flow_dat <- read.csv('flowData.csv',stringsAsFactors=F) %>% 
#   select(DATE, SAC, YOLO, CSMR, MOKE, MISC, SJR, EAST, TOT, XGEO, WEST, PREC, SJR) %>% 
#   mutate(
#     DATE = as.character(DATE), 
#     DATE = as.Date(DATE, format = '%d-%b-%y')
#   ) %>% 
#   gather('var', 'val', -DATE) %>% 
#   mutate(
#     val = val * 0.028316847, #converted to m3/s
#     var = tolower(var)
#   ) %>% 
#   rename(Date = DATE)
# 
# # pull out input stations from Novick et al, combine based on fig 2
# flow_dat <- filter(flow_dat, var %in% c('sjr', 'sac', 'yolo', 'csmr', 'moke', 'misc')) %>% 
#   spread(var, val) %>% 
#   mutate(
#     east = csmr + moke + misc,
#     sac = sac
#   ) %>% 
#   select(Date, sac, east, sjr) %>% 
#   gather('station', 'q', sac:sjr)