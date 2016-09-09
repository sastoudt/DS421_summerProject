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

## need to make covariance matrix out of 

flatCorr

num=cbind.data.frame(forMap$Station,1:nrow(forMap))
names(num)=c("station","id")
flatCorr

merge1=merge(flatCorr,num,by.x="row",by.y="station")
merge1

merge2=merge(merge1,num,by.x="column",by.y="station")
merge2

names(merge2)[4:5]=c("i","j")



flattenD4W=matrix(0,nrow=13,ncol=13)
for(i in 1:nrow(flattenD4)){
  row=flattenD4[i,]
  flattenD4W[row[1],row[2]]=row[3]
}

flattenD4W