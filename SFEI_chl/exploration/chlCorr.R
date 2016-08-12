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
for(i in c(1:9,11,13:24,26:47,49:57)){
 test= ccf(testMerge[,which(names(testMerge)==toCheck[1,i])], 
      testMerge[,which(names(testMerge)==toCheck[2,i])  ], lag.max=5, plot=F,na.action=na.exclude)
  
 bestLag<-c(bestLag,test$lag[which.max(test$acf)] )
 zeroLagCorr<-c(zeroLagCorr,test$acf[which(test$lag==0)])
 bestLagCorr<-c(bestLagCorr,test$acf[which.max(test$acf)])
 print(i)
}


for(i in 59:ncol(toCheck)){
  test= ccf(testMerge[,which(names(testMerge)==toCheck[1,i])], 
            testMerge[,which(names(testMerge)==toCheck[2,i])  ], lag.max=5, plot=F,na.action=na.exclude)
  
  bestLag<-c(bestLag,test$lag[which.max(test$acf)] )
  zeroLagCorr<-c(zeroLagCorr,test$acf[which(test$lag==0)])
  bestLagCorr<-c(bestLagCorr,test$acf[which.max(test$acf)])
  print(i)
}

decideLag=as.data.frame(cbind(c(1:9,11,13:24,26:47,49:57,59:ncol(toCheck)),bestLag,zeroLagCorr,bestLagCorr))
names(decideLag)[1]="combo"

decideLag$station1=toCheck[1,decideLag$combo]
decideLag$station2=toCheck[2,decideLag$combo]
View(decideLag)
