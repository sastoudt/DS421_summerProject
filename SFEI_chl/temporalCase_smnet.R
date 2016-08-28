##https://stat.ethz.ch/R-manual/R-devel/library/splines/html/splineDesign.html
## polynomial b spline model matrix (date_dec)

##https://stat.ethz.ch/R-manual/R-devel/library/mgcv/html/cSplineDes.html
## cyclic b spline model matrix (doy)
require(mgcv)
setwd("~/Desktop/sfei")

allData<-read.csv("allData.csv",stringsAsFactors=F)
allData<-allData[-which(allData$Station=="C10"),]
allData<-allData[-which(allData$Station=="D41"),]
## pick one year, ignore over time for now
require(lubridate)
test=subset(allData,year(allData$Date) %in% c(2012,2013))
nrow(test)
test$Date

which(year(allData$Date)==2014) ## None? really?

y=test$chl
sum(is.na(y)) ## good, no NA
y2=y[-which(is.na(y))]
seg=test$Station
seg=seg[-which(is.na(y))]
class(seg)
y=y2
test=test[-which(is.na(test$chl)),]
nrow(test)
range(test$doy)

knots=seq(1,365,by=5) ##73
seasonal=as.spam(cSplineDes(test$doy, knots, ord = 4))
dim(seasonal)
## 310 x 72

####
require(splines)
range(test$date_dec)
knots=seq(2012, 2014,length.out=72)

temporal=as.spam(splineDesign(knots, test$date_dec, ord = 4, outer.ok = T,
             sparse = FALSE))
dim(temporal)
## 310 x 68

dim(B)
## 310 x 13
