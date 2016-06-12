setwd("~/Desktop/EPA_GAMS/sf_trends-master/data")

load("delt_map.RData")
require(sp)
plot(delt_map)
load("sf_bay.RData")
plot(sf_bay)
plot(delt_map,add=T)
load("delt_dat.RData")
head(delt_dat)

points(delt_dat$Longitude,delt_dat$Latitude,pch=19,col="red")

setwd("~/Desktop/DS421_summerProject/GAM_weightedReg_comparison/shiny")
source("getSummaryRMSE_Dev.R")
source("getFlowNormalizedSummary.R")
source("getSummaryDifference.R")
source("getRegressionResults.R")

load("data/dataNice_nolag.RData")
load("data/modelsNoLag_Nested.RData")

rmsePerStation=mapply(getSummaryRMSE,dataNiceNoLag,modelsNoLag_Nested,SIMPLIFY=F)

rbPal<-colorRampPalette(c("red","blue"))

names(rmsePerStation)[1]
rmsePerStation[[1]]

station=unlist(lapply(names(rmsePerStation),function(x){strsplit(x,"_")[[1]][1]}))
response=unlist(lapply(names(rmsePerStation),function(x){strsplit(x,"_")[[1]][2]}))

ofInterest=do.call(cbind,rmsePerStation[which(response=="no23")[order(station[which(response=="no23")])]])
## cbind these results
## quantile over whole matrix
## ordered appropriately

## check order
station[which(response=="no23")[order(station[which(response=="no23")])]]


cuts=quantile(ofInterest,c(0,0.25,0.5,0.75,1))

col=apply(ofInterest,c(1,2),function(x){rbPal(4)[as.numeric(cut(x,breaks=cuts))]})
## makes minimum NA
col[which(is.na(col))]=rbPal(4)[1]

## now need to associate columns with coordinates
ourStations=as.data.frame(unique(station))
names(ourStations)="site"
head(delt_dat)

## unique preserves order
locationLookUp=as.data.frame(cbind(unique(delt_dat$Site_Code),
unique(delt_dat$Latitude),
unique(delt_dat$Longitude)))
names(locationLookUp)=c("site","lat","long")
locationLookUp

coordsRef=merge(ourStations,locationLookUp)
coordsRef$lat=as.numeric(as.character(coordsRef$lat))
coordsRef$lon=as.numeric(as.character(coordsRef$lon))

pdf("test.pdf",height=12,width=16)
par(mfrow=c(3,5),mar=c(rep(0.5,4))) ## fill by row, extra with frame()
for(i in 1:15){
  if(i %in% 1:5){
    plot(delt_map,main=paste("\n ",row.names(ofInterest)[i+1],sep=""))## title is tight on top row
    points(coordsRef$lon,coordsRef$lat,col=col[i+1,],pch=19)
    legend(legend=round(cuts[2:5],2),fill=rbPal(4),"right")
  }else if(i %in% c(10,15)){
    frame()
  }else{ ## need to do \n everywhere, need to make a counter, skip over 10 and 15
    plot(delt_map,main=paste(row.names(ofInterest)[i+1],sep=""))
    points(coordsRef$lon,coordsRef$lat,col=col[i+1,],pch=19)
    legend(legend=round(cuts[2:5],2),fill=rbPal(4),"right")
  }
  
}
dev.off()
pdf("test.pdf",height=12,width=16)
par(mfrow=c(3,4),mar=c(rep(0.5,4)))
plot(delt_map,main=paste("\n ",row.names(ofInterest)[2],sep=""))## title is tight on top row
points(coordsRef$lon,coordsRef$lat,col=col[2,],pch=19)
legend(legend=round(cuts[2:5],2),fill=rbPal(4),"right")
plot(delt_map,main="\n test")
legend(legend=c(1:4),fill=rbPal(4),"right")
plot(delt_map,main="\n test")
legend(legend=c(1:4),fill=rbPal(4),"right")
plot(delt_map,main="\n test")
legend(legend=c(1:4),fill=rbPal(4),"right")
plot(delt_map,main="test")
legend(legend=c(1:4),fill=rbPal(4),"right")
plot(delt_map,main="test")
legend(legend=c(1:4),fill=rbPal(4),"right")
plot(delt_map,main="test")
legend(legend=c(1:4),fill=rbPal(4),"right")
plot(delt_map,main="test")
legend(legend=c(1:4),fill=rbPal(4),"right")
plot(delt_map,main="test")
legend(legend=c(1:4),fill=rbPal(4),"right")
plot(delt_map,main="test")
legend(legend=c(1:4),fill=rbPal(4),"right")
plot(delt_map,main="test")
legend(legend=c(1:4),fill=rbPal(4),"right")
plot(delt_map,main="test")
legend(legend=c(1:4),fill=rbPal(4),"right")
dev.off()
## take a station name and a response


