## do fit for "best" params and get fitted values, rmse breakdown

require(mgcv)
require(spam)
setwd("~/Desktop/sfei")
allData<-read.csv("allData.csv",stringsAsFactors=F)

row.names=col.names=unique(allData$Station)[-c(1,10)]

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
bid=c("111011","111","1111","11111","11101","111110","111111","1110","1","10","11","1111101","1111101")
shreve.order<-c(1,4,3,3,1,2,1,1,5,1,4,1,1)
wgts<-c(1,4,3/4,3,1,2/3,1/3,1/4,5,1/5,4/5,1/2,1/2)
spatial_penalty<-function(adjacency, wgts, lambda, n.segments){
  adj.spam <- make_spam(adjacency)
  pseudo.inds  <- which(colSums.spam(adj.spam) == 1)
  ij.nzero.adj <- triplet(adj.spam)$indices
  in.pseudo    <- ij.nzero.adj[,2] %in% pseudo.inds
  ij.confl     <- ij.nzero.adj[!in.pseudo,]
  n.nzero      <- nrow(ij.confl)
  p.row.ind    <- rep(1:n.nzero, each = 2)
  p.col.ind    <- c(t(ij.confl))
  p.val        <- wgts[rep(ij.confl[,1], each = 2)]*rep(c(-1, 1), n.nzero)
  D2           <- spam(list(i=p.row.ind, j=p.col.ind, p.val), nrow = n.nzero, ncol = n.segments)
  D2           <- t(D2)%*%D2 
  
  if(!is.null(pseudo.inds)){
    ij.pseudo    <- ij.nzero.adj[in.pseudo,]
    n.nzero      <- nrow(ij.pseudo)
    if(is.null(n.nzero)) n.nzero <- 1
    p.row.ind    <- rep(1:n.nzero, each = 2)
    p.col.ind    <- c(t(ij.pseudo))
    if(is.matrix(ij.pseudo)){
      p.val        <- wgts[rep(ij.pseudo[,1], each = 2)]*rep(c(-1, 1), n.nzero)
    }
    if(is.vector(ij.pseudo)){
      p.val        <- wgts[rep(ij.pseudo[1], each = 2)]*rep(c(-1, 1), n.nzero)
    }
    D1           <- spam(list(i=p.row.ind, j=p.col.ind, p.val), nrow = n.nzero, ncol = n.segments)
    D1            <- t(D1)%*%D1
  }  
  return((lambda)*(D2 + D1))  
}
make_spam<-function(M){
  if(class(M) == "matrix") as.spam(M)
  if(class(M) == "spam") as.spam(M)
  else as.spam(as.spam.dgCMatrix(as(M, "dgCMatrix")))
}
lambdaD=1
adjacency=sfeiAdjMatrix
D=spatial_penalty(adjacency,shreve.order,lambdaD,nrow(adjacency))
D=spatial_penalty(adjacency,wgts,lambdaD,nrow(adjacency))



allData<-allData[-which(allData$Station=="C10"),]
allData<-allData[-which(allData$Station=="D41"),]

allData<-allData[-which(is.na(allData$chl)),]
nrow(allData)

y=allData$chl
y=log(allData$chl)
p=nrow(adjacency)
B=matrix(0,nrow=length(y),ncol=p)
seg=allData$Station
lookUp=as.data.frame(cbind(unique(allData$Station),1:length(unique(allData$Station))))
names(lookUp)=c("station","p")
lookUp$station=as.character(lookUp$station)
lookUp$p=as.numeric(as.character(lookUp$p))
## do without a loop later, just get a quick sense
for(i in 1:nrow(B)){
  index=which(lookUp$station==seg[i])
  B[i,index]=1
  print(i)
}

B=as.spam(B)
dim(B)
##  7634   13

knots=seq(1,365,length.out=35) ##73
seasonal=cSplineDes(allData$doy, knots, ord = 4)
seasonal=as.spam(seasonal)
dim(seasonal)
## 7634   34

## I was using too many knots last time


require(splines)

knots=seq(min(allData$date_dec), max(allData$date_dec),length.out=35)

temporal=splineDesign(knots, allData$date_dec, ord = 4, outer.ok = T,
                      sparse = FALSE)
temporal=as.spam(temporal)
dim(temporal)
##  7634   31

require(fields)
D=spam2full(D)

lambdaD=1.720064e-12
lambdaT=5.332215e-14
lambdaS=2.466260e-16

P=bdiag.spam(0,lambdaD*t(D)%*%D,lambdaT*t(temporal)%*%temporal, lambdaS*t(seasonal)%*%seasonal)
dim(P)
## 79 x 79

ridgeNuD=0.25
ridgeNuT=0.25
ridgeNuS=1

Q=diag(c(0,ridgeNuD*as.vector(rep(1,ncol(B))),ridgeNuT*as.vector(rep(1,ncol(temporal))),
         ridgeNuS*as.vector(rep(1,ncol(seasonal)))))
Q=as.spam(Q)
dim(Q)
## 79  x 79


Bnew=cbind(rep(1,nrow(B)),B,temporal,seasonal)
dim(Bnew)
## 7634   79

class(Bnew)
class(P)
class(Q)
betaHat=solve(t(Bnew)%*%Bnew+P+Q)%*%t(Bnew)%*%y ## still fast
dim(betaHat) 

yHat=Bnew%*%betaHat

summary(y)
summary(yHat)

y=allData$chl
yHat=exp(yHat)

plot(y,yHat)
abline(0,1,col="red")

#plot(y,yHat,xlim=c(0,20))
#abline(0,1,col="red")

rmse=sqrt(sum((y-yHat)^2)/length(y)) 
rmse ##6.942123
## hm....

## same for weights derived by hand using w formulation from ODonnell paper

names(allData)

allData$smnetPred=as.vector(yHat)
allData$smnetResidSq=as.vector((y-yHat)^2)

require(dplyr)

by_station=group_by(allData,Station)
predByStation=summarise(by_station,count=n(),sumSqEr=sum(smnetResidSq))
rmse=sqrt(predByStation$sumSqEr/predByStation$count)
rmse ## Pretty variable by station

forMap=allData[,c("Longitude","Latitude","Station")]
forMap=unique(forMap)

forMap$Station
predByStation$Station

forMap$toPlot=rmse

require(ggplot2)
shreve.order<-c(1,4,3,3,1,2,1,1,5,1,4,1,1)


ggplot(forMap,aes(x = Longitude, y = Latitude,colour=toPlot,cex=2))+geom_point()+
  ggtitle("smnet RMSE by Station")+scale_size(guide=F)+annotate("text", x=forMap$Longitude,y=forMap$Latitude, label = shreve.order,col="white",cex=5)

## does not seem to be connected to shreve order (here the weights)

## compare to another model, same general ordering?

wholeSeries<-c(1, 2, 5, 7, 11, 13, 15, 16, 17, 18, 21, 22, 23, 29, 40)

load(file="perStationMod3Compare.RData")
load(file="perStation.Rda")
rmsePerStationCompare<-c()
for(i in wholeSeries){
  true=perStation[[i]]$chl
  fitted=predict(perStationMod3Compare[[i]],perStation[[i]],type="response")
  rmsePerStationCompare=c(rmsePerStationCompare, sqrt(sum((true-fitted)^2,na.rm=T)/sum(!is.na(fitted))))
}
rmsePerStationCompare
names(perStation[wholeSeries])
forMap$Station
rmsePerStationCompare=rmsePerStationCompare[-c(1,10)]
forMap$mod3=rmsePerStationCompare

load("perStationInteractionModels.Rda")
rmsePerStationSepPlain<-c()
for(i in wholeSeries){
  true=perStation[[i]]$chl
  fitted=as.vector(predict(perStationIntMod[[i]],perStation[[i]],type="response"))
  rmsePerStationSepPlain=c(rmsePerStationSepPlain, sqrt(sum((true-fitted)^2,na.rm=T)/sum(!is.na(fitted))))
}

rmsePerStationSepPlain
rmsePerStationCompare=rmsePerStationCompare[-c(1,10)]
forMap$interaction=rmsePerStationCompare

forMap$interaction[4]=NA
forMap$mod3[4]=NA
## this helps a bit to diversify the colors on bottom 2 plots


g1<-ggplot(forMap,aes(x = Longitude, y = Latitude,colour=toPlot,cex=2))+geom_point()+
  ggtitle("smnet RMSE by Station")+scale_size(guide=F)+annotate("text", x=forMap$Longitude,y=forMap$Latitude, label = shreve.order,col="white",cex=5)

g2<-ggplot(forMap,aes(x = Longitude, y = Latitude,colour=mod3,cex=2))+geom_point()+
  ggtitle("Mod 3 RMSE by Station")+scale_size(guide=F)

g3<-ggplot(forMap,aes(x = Longitude, y = Latitude,colour=interaction,cex=2))+geom_point()+
  ggtitle("Interaction RMSE by Station")+scale_size(guide=F)

require(gridExtra)
grid.arrange(g1,g2,g3)

## can't really see what might be going on, doesn't diverge from general idea
## of which stations are "hard"

getSummaryRMSE<-function(data,namePred){
  
  trueVal=data$chl 
  
  predVal=data[,namePred]
  
  rmse=sqrt(sum((trueVal-predVal)^2,na.rm=T)/sum(!is.na((trueVal-predVal)^2)))
  
  data$month=as.numeric(strftime(data$Date, '%m'))
  data$year=as.numeric(strftime(data$Date, '%Y'))
  
  annual1=subset(data,year<1982 & year>=1975)
  annual1I=which(data$year<1982 & data$year>=1975)
  annual1P=predVal[annual1I]
  
  
  annual2=subset(data,year<1989 & year>=1982)
  annual2I=which(data$year<1989 & data$year>=1982)
  annual2P=predVal[annual2I]
  
  
  annual3=subset(data,year<1996 & year>=1989)
  annual3I=which(data$year<1996 & data$year>=1989)
  annual3P=predVal[annual3I]
  
  annual4=subset(data,year<2003 & year>=1996)
  annual4I=which(data$year<2003 & data$year>=1996)
  annual4P=predVal[annual4I]
  
  annual5=subset(data,year<2010 & year>=2003)
  annual5I=which(data$year<2010 & data$year>=2003)
  annual5P=predVal[annual5I]
  
  
  annual6=subset(data,year>=2010) ## has 2 fewer years
  annual6I=which( data$year>=2010)
  annual6P=predVal[annual6I]
  
  
  rmseA1=sqrt(sum((annual1$chl-annual1P)^2,na.rm=T)/sum(!is.na((annual1$chl-annual1P)^2)))
  rmseA2=sqrt(sum((annual2$chl-annual2P)^2,na.rm=T)/sum(!is.na((annual2$chl-annual2P)^2)))
  rmseA3=sqrt(sum((annual3$chl-annual3P)^2,na.rm=T)/sum(!is.na((annual3$chl-annual3P)^2)))
  rmseA4=sqrt(sum((annual4$chl-annual4P)^2,na.rm=T)/sum(!is.na((annual4$chl-annual4P)^2)))
  rmseA5=sqrt(sum((annual5$chl-annual5P)^2,na.rm=T)/sum(!is.na((annual5$chl-annual5P)^2)))
  rmseA6=sqrt(sum((annual6$chl-annual6P)^2,na.rm=T)/sum(!is.na((annual6$chl-annual6P)^2)))
  
  seasonal1=subset(data,month %in% c(1:3))
  seasonal1I=which(data$month %in% c(1:3))
  seasonal1P=predVal[seasonal1I]
  
  seasonal2=subset(data,month %in% c(4:6))
  seasonal2I=which(data$month %in% c(4:6))
  seasonal2P=predVal[seasonal2I]
  
  seasonal3=subset(data,month %in% c(7:9))
  seasonal3I=which(data$month %in% c(7:9))
  seasonal3P=predVal[seasonal3I]
  
  seasonal4=subset(data,month %in% c(10:12))
  seasonal4I=which(data$month %in% c(10:12))
  seasonal4P=predVal[seasonal4I]
  
  
  rmseS1=sqrt(sum((seasonal1$chl-seasonal1P)^2,na.rm=T)/sum(!is.na((seasonal1$chl-seasonal1P)^2)))
  rmseS2=sqrt(sum((seasonal2$chl-seasonal2P)^2,na.rm=T)/sum(!is.na((seasonal2$chl-seasonal2P)^2)))
  rmseS3=sqrt(sum((seasonal3$chl-seasonal3P)^2,na.rm=T)/sum(!is.na((seasonal3$chl-seasonal3P)^2)))
  rmseS4=sqrt(sum((seasonal4$chl-seasonal4P)^2,na.rm=T)/sum(!is.na((seasonal4$chl-seasonal4P)^2)))
  
  
  
  rmse=rbind(rmse,rmseA1,rmseA2,rmseA3,rmseA4,rmseA5,rmseA6,rmseS1,rmseS2,rmseS3,rmseS4)
  
  return(rmse)
  # return(list(all=rmse,annual1=rmseA1,annual2=rmseA2,annual3=rmseA3,annual4=rmseA4,
  #             seasonal1=rmseS1, seasonal2=rmseS2, seasonal3=rmseS3, seasonal4=rmseS4,
  #             flow1=rmseF1,flow2=rmseF2,flow3=rmseF3,flow4=rmseF4))
}

test=getSummaryRMSE(allData,"smnetPred")
test
## gets better over time, better at beginning and end of year

makeFittedValPlot=function(fullData,station){
  data=subset(fullData,Station==station)
  data$Date=as.Date(data$Date)
  ggplot(data,aes(x = Date, y = chl))+geom_point()+
    geom_line(aes(x=Date,y =smnetPred ,col="red"),lwd=1)+
    ggtitle(paste(station, "Fitted Values smnet Model",sep=" "))+
    theme(legend.position='none')+ylab("chl a (microgram/L)")+xlab("Date")
  
}

forMap$Station
makeFittedValPlot(allData,"C3")
makeFittedValPlot(allData,"D10")
makeFittedValPlot(allData,"D12")
makeFittedValPlot(allData,"D19")
makeFittedValPlot(allData,"D22")
makeFittedValPlot(allData,"D26")
makeFittedValPlot(allData,"D28A")
makeFittedValPlot(allData,"D4")
makeFittedValPlot(allData,"D6")
makeFittedValPlot(allData,"D7")
makeFittedValPlot(allData,"D8")
makeFittedValPlot(allData,"MD10")
makeFittedValPlot(allData,"P8")

## definitely not extreme enough, seems to be in phase though
