##https://stat.ethz.ch/R-manual/R-devel/library/splines/html/bs.html
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
## 310 x 72

####
bs(test$date_dec, df = NULL, knots = NULL, degree = 3, intercept = FALSE,
   Boundary.knots = range(x))
