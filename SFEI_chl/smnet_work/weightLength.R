setwd("~/Desktop/sfei")
allData<-read.csv("allData.csv",stringsAsFactors=F)
allData<-allData[-which(allData$Station=="C10"),]
allData<-allData[-which(allData$Station=="D41"),]

allData<-allData[-which(is.na(allData$chl)),]

forMap=allData[,c("Longitude","Latitude","Station")]

forMap=unique(forMap)

forMap
distM=as.matrix(dist(forMap[,1:2]))

row.names=col.names=forMap[,3]



D2=spam2full(D2)
D4=spam2full(D4)

flattenD2<-c()
for(i in 1:13){
  for(j in 1:13){
    
    flattenD2=rbind(flattenD2,c(i,j,D2[i,j]))
  }
}

which(distM!=0)

flattenD2<-c()
for(i in 1:13){
  for(j in 1:13){
  
    flattenD2=rbind(flattenD2,c(i,j,distM[i,j]*D2[i,j]))
  }
}

flattenD2W=matrix(0,nrow=13,ncol=13)
for(i in 1:nrow(flattenD2)){

  row=flattenD2[i,]
  print(row[3])
  flattenD2W[row[1],row[2]]=row[3]
}

flattenD4<-c()
for(i in 1:13){
  for(j in 1:13){
    flattenD4=rbind(flattenD4,c(i,j,distM[i,j]*D4[i,j]))
  }
}

flattenD4W=matrix(0,nrow=13,ncol=13)
for(i in 1:nrow(flattenD4)){
  row=flattenD4[i,]
  flattenD4W[row[1],row[2]]=row[3]
}

flattenD4W
