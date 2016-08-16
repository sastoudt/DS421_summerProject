
library(data.table)
data=c()
for(i in c(1997:2013,2015)){
  data=rbind(data,read.csv(paste("http://www.water.ca.gov/dayflow/docs/dayflowCalculations",i,".csv",sep=""),row.names=NULL))
print(i)
  }


myfile=read.csv("http://www.water.ca.gov/dayflow/docs/dayflowCalculations2014.csv",row.names=NULL)
myfile$row.names=rep(NA,nrow(myfile))
myfile=myfile[,c(ncol(myfile),1:(ncol(myfile)-1))]
data=rbind(data,myfile)

setwd("~/Desktop/sfei")

seventies<-read.csv("wy1970-1983.csv")
eighties<-read.csv("wy1984-1996.csv")

ncol(seventies)
ncol(eighties)
names(seventies)
names(data)

seventies$extra=rep(NA,nrow(seventies))
eighties$extra=rep(NA,nrow(eighties))
names(seventies)=names(data)
names(eighties)=names(data)

data=rbind(data,seventies,eighties)

data=data[,-c(1,ncol(data))]
names(data)
data=data[,-c(1,3)]
names(data)

class(data$Mo)
data$Mo=as.character(data$Mo)
require(lubridate)
data$Mo=dmy(data$Mo)

mydat <- read.csv(textConnection(myfile), header=T)
setwd("~/Desktop/sfei")
flow_dat=read.csv("wy1970-1983.csv")

require(tidyr)
require(dplyr)
flow_dat <- read.csv('wy1970-1983.csv',stringsAsFactors=F) %>% 
  select(DATE, SAC, YOLO, CSMR, MOKE, MISC, SJR, EAST, TOT, XGEO, WEST, PREC, SJR) %>% 
  mutate(
    DATE = as.character(DATE), 
    DATE = as.Date(DATE, format = '%d-%b-%y')
  ) %>% 
  gather('var', 'val', -DATE) %>% 
  mutate(
    val = val * 0.028316847,
    var = tolower(var)
  ) %>% 
  rename(Date = DATE)

# pull out input stations from Novick et al, combine based on fig 2
flow_dat <- filter(flow_dat, var %in% c('sjr', 'sac', 'yolo', 'csmr', 'moke', 'misc')) %>% 
  spread(var, val) %>% 
  mutate(
    east = csmr + moke + misc,
    sac = sac
  ) %>% 
  select(Date, sac, east, sjr) %>% 
  gather('station', 'q', sac:sjr)