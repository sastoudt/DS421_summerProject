### P8 difference between GAM and WRTDS fitted values

require(tidyr)
require(dplyr)
require(lubridate)
require(WRTDStidal)
require(mgcv)
setwd("~/Desktop/DS421_summerProject/GAM_weightedReg_comparison/shiny/data")
load("dataNice_nolag.RData")

## i=7,8,9 P8

plot(dataNiceNoLag[[7]]$gamPred,dataNiceNoLag[[7]]$wrtdsPred)
abline(0,1,col="red")

table(dataNiceNoLag[[7]]$gamPred>dataNiceNoLag[[7]]$wrtdsPred)
#FALSE  TRUE 
#149   299 

## can see the imbalance towards the top right of the plot

plot(dataNiceNoLag[[8]]$gamPred,dataNiceNoLag[[8]]$wrtdsPred)
abline(0,1,col="red")

table(dataNiceNoLag[[8]]$gamPred>dataNiceNoLag[[8]]$wrtdsPred)
##FALSE  TRUE 
#154   294 

## can see the imbalance all over

plot(dataNiceNoLag[[9]]$gamPred,dataNiceNoLag[[9]]$wrtdsPred)
abline(0,1,col="red")

table(dataNiceNoLag[[9]]$gamPred>dataNiceNoLag[[9]]$wrtdsPred)
#FALSE  TRUE 
#165   284 

# can see mostly towards the upper right


plot(dataNiceNoLag[[12]]$gamPred,dataNiceNoLag[[12]]$wrtdsPred)
abline(0,1,col="red")

table(dataNiceNoLag[[12]]$gamPred>dataNiceNoLag[[12]]$wrtdsPred)
##FALSE  TRUE 
#238   213 


plot(dataNiceNoLag[[2]]$gamPred,dataNiceNoLag[[2]]$wrtdsPred)
abline(0,1,col="red")

table(dataNiceNoLag[[2]]$gamPred>dataNiceNoLag[[2]]$wrtdsPred)
##FALSE  TRUE 
#120   312 

plot(dataNiceNoLag[[1]]$gamPred,dataNiceNoLag[[1]]$wrtdsPred)
abline(0,1,col="red")

table(dataNiceNoLag[[1]]$gamPred>dataNiceNoLag[[1]]$wrtdsPred)
#FALSE  TRUE 
#146   286 
## this one is fine

plot(dataNiceNoLag[[15]]$gamPred,dataNiceNoLag[[15]]$wrtdsPred)
abline(0,1,col="red")

##FALSE  TRUE 
#255   195 

## so the opposite trend is also possible

table(dataNiceNoLag[[15]]$gamPred>dataNiceNoLag[[15]]$wrtdsPred)

## wow really bad pattern here, curved, gamPred much more negative than wrtdsPred



## now how does this break down by year, month
dataNiceNoLag[[7]]$year=year(dataNiceNoLag[[7]]$date)
dataNiceNoLag[[8]]$year=year(dataNiceNoLag[[8]]$date)
dataNiceNoLag[[9]]$year=year(dataNiceNoLag[[9]]$date)

dataNiceNoLag[[7]]$month=month(dataNiceNoLag[[7]]$date)
dataNiceNoLag[[8]]$month=month(dataNiceNoLag[[8]]$date)
dataNiceNoLag[[9]]$month=month(dataNiceNoLag[[9]]$date)

dataNiceNoLag[[7]]$diffPred=dataNiceNoLag[[7]]$gamPred-dataNiceNoLag[[7]]$wrtdsPred
dataNiceNoLag[[8]]$diffPred=dataNiceNoLag[[8]]$gamPred-dataNiceNoLag[[8]]$wrtdsPred
dataNiceNoLag[[9]]$diffPred=dataNiceNoLag[[9]]$gamPred-dataNiceNoLag[[9]]$wrtdsPred

## mean of sign(diffPred)

by_year <- group_by(dataNiceNoLag[[7]], year)
signDiffPredY7 <- summarise(by_year,count=n(),meanSign = mean(sign(diffPred), na.rm = TRUE)
                   )

by_month <- group_by(dataNiceNoLag[[7]], month)
signDiffPredM7 <- summarise(by_month,count=n(),meanSign = mean(sign(diffPred), na.rm = TRUE)
)

by_year <- group_by(dataNiceNoLag[[8]], year)
signDiffPredY8 <- summarise(by_year,count=n(),meanSign = mean(sign(diffPred), na.rm = TRUE)
)

by_month <- group_by(dataNiceNoLag[[8]], month)
signDiffPredM8 <- summarise(by_month,count=n(),meanSign = mean(sign(diffPred), na.rm = TRUE)
)

by_year <- group_by(dataNiceNoLag[[9]], year)
signDiffPredY9 <- summarise(by_year,count=n(),meanSign = mean(sign(diffPred), na.rm = TRUE)
)

by_month <- group_by(dataNiceNoLag[[9]], month)
signDiffPredM9 <- summarise(by_month,count=n(),meanSign = mean(sign(diffPred), na.rm = TRUE)
)

View(signDiffPredY7)

