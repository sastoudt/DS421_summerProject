## investigate chl correlation between stations and any potential lags

## merge date, chl from each station

setwd("~/Desktop/sfei")
load(file="perStation.Rda")
allData<-read.csv("allData.csv")

wholeSeries<-c(1, 2, 5, 7, 11, 13, 15, 16, 17, 18, 21, 22, 23, 29, 40)

testMerge=merge(perStation[[1]][,c("Station","chl","Date")],perStation[[2]][,c("Station","chl","Date")],
                by.x="Date",by.y="Date",
                all.x=T,all.y=T)
testMerge=testMerge[,c(1,3,5)]
names(testMerge)=c("Date","C10","C3")
end=3
for(i in wholeSeries[3:length(wholeSeries)]){
  testMerge=merge(testMerge,perStation[[i]][,c("Station","chl","Date")],all.x=T,all.y=T,by.x="Date",by.y="Date")
  testMerge=testMerge[,c(1:end,end+2)]
  names(testMerge)[end+1]=perStation[[i]]$Station[1]
  end=end+1
}

## will want to order these by location to have a sense of what is going on
plot(allData$Longitude,allData$Latitude,type="n")
text(allData$Longitude,allData$Latitude, allData$Station)

orderL2R=c("D41","D6","D7","D8","D10","D4","D12","D22","D19","D26","D28A","MD10","P8","C3","C10")

testMerge=testMerge[,c("Date",orderL2R)]
View(testMerge)

require(Hmisc)
result<-rcorr(as.matrix(testMerge[,-1]))
result$r[result$n<100]<-0 ## get rid of correlations made with not a lot of pairwise matches
result$r

col<- colorRampPalette(c("blue", "white", "red"))(20)
heatmap(x = result$r, col = col, symm = TRUE)

##http://www.sthda.com/english/wiki/correlation-matrix-a-quick-start-guide-to-analyze-format-and-visualize-a-correlation-matrix-using-r-software
flattenCorrMatrix <- function(cormat) {
  ut <- upper.tri(cormat)
  data.frame(
    row = rownames(cormat)[row(cormat)[ut]],
    column = rownames(cormat)[col(cormat)[ut]],
    cor  =(cormat)[ut]
    ## adapted to not care about the pvalues
  )
}
flatCorr=flattenCorrMatrix(result$r)

toUse=flatCorr[which(abs(flatCorr$cor)>.5),]

forMap=allData[,c("Longitude","Latitude","Station")]
forMap=unique(forMap)

tryThis=merge(toUse,forMap,by.x="row",by.y="Station",all.x=T)
names(tryThis)[4:5]=c("Long1","Lat1")
tryThis2=merge(tryThis,forMap,by.x="column",by.y="Station",all.x=T)
names(tryThis2)[6:7]=c("Long2","Lat2")

plot(allData$Longitude,allData$Latitude,type="n")
text(allData$Longitude,allData$Latitude, allData$Station)
arrows(tryThis2$Long1,tryThis2$Lat1,tryThis2$Long2,tryThis2$Lat2,angle=90,code=2,length=0)

toUse=flatCorr[which(abs(flatCorr$cor)>.6),]


tryThis=merge(toUse,forMap,by.x="row",by.y="Station",all.x=T)
names(tryThis)[4:5]=c("Long1","Lat1")
tryThis2=merge(tryThis,forMap,by.x="column",by.y="Station",all.x=T)
names(tryThis2)[6:7]=c("Long2","Lat2")

plot(allData$Longitude,allData$Latitude,type="n")
text(allData$Longitude,allData$Latitude, allData$Station)
arrows(tryThis2$Long1,tryThis2$Lat1,tryThis2$Long2,tryThis2$Lat2,angle=90,code=2,length=0)




toUse=flatCorr[which(abs(flatCorr$cor)>.7),]


tryThis=merge(toUse,forMap,by.x="row",by.y="Station",all.x=T)
names(tryThis)[4:5]=c("Long1","Lat1")
tryThis2=merge(tryThis,forMap,by.x="column",by.y="Station",all.x=T)
names(tryThis2)[6:7]=c("Long2","Lat2")

plot(allData$Longitude,allData$Latitude,type="n")
text(allData$Longitude,allData$Latitude, allData$Station)
arrows(tryThis2$Long1,tryThis2$Lat1,tryThis2$Long2,tryThis2$Lat2,angle=90,code=2,length=0)


toUse=flatCorr[which(abs(flatCorr$cor)>.8),]


tryThis=merge(toUse,forMap,by.x="row",by.y="Station",all.x=T)
names(tryThis)[4:5]=c("Long1","Lat1")
tryThis2=merge(tryThis,forMap,by.x="column",by.y="Station",all.x=T)
names(tryThis2)[6:7]=c("Long2","Lat2")

plot(allData$Longitude,allData$Latitude,type="n")
text(allData$Longitude,allData$Latitude, allData$Station)
arrows(tryThis2$Long1,tryThis2$Lat1,tryThis2$Long2,tryThis2$Lat2,angle=90,code=2,length=0)

ccf(testMerge$D7, testMerge$D8, lag.max=5, plot=T,na.action=na.exclude)
test=ccf(testMerge$D7, testMerge$D8, lag.max=5, plot=F,na.action=na.exclude)
attributes(test)
test$acf
test$lag

relevant=flatCorr[which(flatCorr$cor>0),]
relevant
class(relevant$row)
relevant$row=as.character(relevant$row)
relevant$column=as.character(relevant$column)

unique(c(relevant$row,relevant$column))

toCheck=combn(unique(c(relevant$row,relevant$column)), 2)

bestLag<-c()
zeroLagCorr<-c()
bestLagCorr<-c()
numUsed<-c()
for(i in c(1:9,11,13:24,26:47,49:57)){
 test= ccf(testMerge[,which(names(testMerge)==toCheck[1,i])], 
      testMerge[,which(names(testMerge)==toCheck[2,i])  ], lag.max=5, plot=F,na.action=na.exclude)
  
 bestLag<-c(bestLag,test$lag[which.max(test$acf)] )
 zeroLagCorr<-c(zeroLagCorr,test$acf[which(test$lag==0)])
 bestLagCorr<-c(bestLagCorr,test$acf[which.max(test$acf)])
 numUsed<-c(numUsed,test$n.used)
 print(i)
}


for(i in 59:ncol(toCheck)){
  test= ccf(testMerge[,which(names(testMerge)==toCheck[1,i])], 
            testMerge[,which(names(testMerge)==toCheck[2,i])  ], lag.max=5, plot=F,na.action=na.exclude)
  
  bestLag<-c(bestLag,test$lag[which.max(test$acf)] )
  zeroLagCorr<-c(zeroLagCorr,test$acf[which(test$lag==0)])
  bestLagCorr<-c(bestLagCorr,test$acf[which.max(test$acf)])
  numUsed<-c(numUsed,test$n.used)
  
  print(i)
}

decideLag=as.data.frame(cbind(c(1:9,11,13:24,26:47,49:57,59:ncol(toCheck)),bestLag,zeroLagCorr,bestLagCorr,numUsed))
names(decideLag)[1]="combo"

decideLag$station1=toCheck[1,decideLag$combo]
decideLag$station2=toCheck[2,decideLag$combo]
View(decideLag)

length(unique(c(decideLag$station1,decideLag$station2)))
length(wholeSeries) ## every station has something

## go through and see which are actually significant benefits above lag 0

View(decideLag[which(abs(decideLag$zeroLagCorr-decideLag$bestLagCorr)>.1),])

lookInto<-decideLag[which(abs(decideLag$zeroLagCorr-decideLag$bestLagCorr)>.1),]
dim(lookInto)

lookIntoS=unique(c(lookInto$station1,lookInto$station2))
## "D41"  "D6"   "D7"   "D8"   "D10"  "D4"   "D12"  "D22"  "D19"  "C3"   "MD10" "C10"  "P8"

## where are these stations?
plot(allData$Longitude,allData$Latitude,pch=19)
points(allData$Longitude[which(allData$Station %in% lookIntoS)],allData$Latitude[which(allData$Station %in% lookIntoS)],col="red",pch=19)

###
## find best station/lag combo for each
## maybe do best station lag 0
## and then another one that optimizes lag
## put data together
## add that series as "flow" smooth variable in gam
flatCorr=flattenCorrMatrix(result$r)

flatCorr
class(flatCorr$row)
flatCorr$row=as.character(flatCorr$row)
flatCorr$column=as.character(flatCorr$column)

length(unique(c(flatCorr$row,flatCorr$column)))
length(wholeSeries)

keepTrack<-c()
for(i in names(perStation)[wholeSeries]){
  find=grep(i,flatCorr$row)
  find2=grep(i,flatCorr$column)
  ind=unique(c(find,find2))[which.max(abs(flatCorr[unique(c(find,find2)),"cor"]))]
  
 keepTrack=rbind(keepTrack,flatCorr[ind,])
}

keepTrack=unique(keepTrack)

library(data.table)


## Not same dates
## use this cool trick to find nearest date
## http://stackoverflow.com/questions/28072542/merge-nearest-date-and-related-variables-from-a-another-dataframe-by-group

perStationAdd<- vector(mode = "list", length = length(perStation))


for(i in 1:nrow(keepTrack)){
  find=grep(paste("^",keepTrack[i,1],"$",sep=""),names(perStation))
  find2=grep(paste("^",keepTrack[i,2],"$",sep=""),names(perStation))
  
  setDT(perStation[[find]])
  setDT(perStation[[find2]])
  
  setkey(perStation[[find2]], Date)[, dateMatch:=Date]
  test=perStation[[find2]][perStation[[find]], roll='nearest']
  test$Date=as.Date(test$Date)
  test$dateMatch=as.Date(test$dateMatch)
  
  test=as.data.frame(test)

  test=test[,c(1:38,which(names(test)=="i.chl"))]
  perStationAdd[[find]]=test
  
  setDT(perStation[[find2]])
  setDT(perStation[[find]])
  
  setkey(perStation[[find]], Date)[, dateMatch:=Date]
  test=perStation[[find]][perStation[[find2]], roll='nearest']
  test$Date=as.Date(test$Date)
  
  test=as.data.frame(test)
  
  test=test[,c(1:38, 47)]
  perStationAdd[[find2]]=test
  print(i)
}
save(perStationAdd,file="perStationAdd.Rda")

for(i in wholeSeries){
  print(names(perStation[[i]]))
}

wholeSeries
ncol(perStation[[3]]) ## successfully added 2 to every station of interest (match date, and chl value)

### now look for lags


## don't trust the -1 and 1s

decideLag=decideLag[-unique(which(abs(decideLag$zeroLagCorr)==1),
which(abs(decideLag$bestLagCorr)==1)),]

length(which(decideLag$numUsed<100)) ## over half

decideLag=decideLag[-which(decideLag$numUsed<100),]

keepTrack2<-c()
for(i in names(perStation)[wholeSeries]){
  find=grep(i,decideLag$station1)
  find2=grep(i,decideLag$station2)
  ind=unique(c(find,find2))[which.max(abs(decideLag[unique(c(find,find2)),"bestLagCorr"]))]
  ind2=unique(c(find,find2))[which.max(abs(decideLag[unique(c(find,find2)),"zeroLagCorr"]))]
  
  
  if(abs(decideLag[ind,"bestLagCorr"])<abs(decideLag[ind2,"zeroLagCorr"])){
    keepTrack2=rbind(keepTrack2,decideLag[ind2,])
    
  }else{
    keepTrack2=rbind(keepTrack2,decideLag[ind,])
  }
}
keepTrack2
keepTrack2=unique(keepTrack2)

## keepTrack2 doesn't match keepTrack because in keepTrack we cut off values that were calculated
## with not enough data
## went back and fixed this in keepTrack2

## only C10 and C3 benefit from a lag, lag on the end of the spectrum that I checked, so just 
## go with zero lag for now
