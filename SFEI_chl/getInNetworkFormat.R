
## figure out network

setwd("~/Desktop/DS421_summerProject/GAM_weightedReg_comparison/shiny/data")

load("delt_map.RData")

setwd("~/Desktop/sfei")

allData<-read.csv("allData.csv",stringsAsFactors=F)

pdf("deltMapWLoc.pdf",height=10,width=10)
plot(delt_map)
text(allData$Longitude,allData$Latitude,allData$Station,pch=19)
dev.off()

names(attributes(delt_map))
delt_map

##         min        max
#x -122.17935 -121.27622
#y   37.76008   38.82315
 
range(allData$Longitude) ##-122.3729 -121.2647
range(allData$Latitude) ## 37.67934 38.36771

plot(allData$Longitude,allData$Latitude,pch=19)
## bottom right ## C10
## furthest left ## D41
## just keep these out for now, too hard to tell adjacency by eye 

unique(allData$Station)

row.names=col.names=unique(allData$Station)[-c(1,10)]

## don't think the adjacency matrix has to be symmetric, just put in flow direction

sfeiAdjMatrix<-matrix(0,nrow=13,ncol=13)
sfeiAdjMatrix[1,5]=1 ## C3 --> D22
sfeiAdjMatrix[2,11]=1 ## D10 --> D8
sfeiAdjMatrix[3,2]=1 ## D12 --> D10
sfeiAdjMatrix[4,3]=1 ## D19 --> D12
sfeiAdjMatrix[5,8]=1 ## D22 --> D4
sfeiAdjMatrix[6,4]=1 ## D26 --> D19
sfeiAdjMatrix[7,4]=1 ## D28A --> D19
sfeiAdjMatrix[8,2]=1 ## D4 --> D10
sfeiAdjMatrix[11,9]=1 ## D8 --> D6
sfeiAdjMatrix[11,10]=1 ## D8 --> D7
sfeiAdjMatrix[12,6]=1 ## MD10 --> D28A
sfeiAdjMatrix[13,6]=1 ## P8 --> D28A

row.names(sfeiAdjMatrix)=row.names
colnames(sfeiAdjMatrix)=col.names
View(sfeiAdjMatrix)
