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


######

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
require(mgcv)
require(spam)
setwd("~/Desktop/sfei")
allData<-read.csv("allData.csv",stringsAsFactors=F)
shreve.order<-c(1,4,3,3,1,2,1,1,5,1,4,1,1)
shreve.orderNorm<-shreve.order/sum(shreve.order)
wgts<-c(1,4,3/4,3,1,2/3,1/3,1/4,5,1/5,4/5,1/2,1/2)
wgtsMax1<-c(1,1,3/4,1,1,2/3,1/3,1/4,1,1/5,4/5,1/2,1/2)
wgtsSame<-rep(1,13)
wgtsZero<-rep(0,13)

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


lambdaD=1
adjacency=sfeiAdjMatrix




#D=spatial_penalty(adjacency,shreve.order/sum(shreve.order),lambdaD,nrow(adjacency))
# D=D2
# D=spatial_penalty(adjacency,wgts,lambdaD,nrow(adjacency))
# D3=spatial_penalty(adjacency,rep(1,nrow(adjacency)),lambdaD,nrow(adjacency))
# D4=spatial_penalty(adjacency,shreve.order/sum(shreve.order),lambdaD,nrow(adjacency))
# max(abs(D-D2)) ##45.72
# max(abs(D-D3)) ##2.055556
# max(abs(D2-D3)) ##45
# max(abs(D4-D3)) ## 2.992347
# max(abs(D4-D2)) ## 47.93878
# max(abs(D4-D)) ##2.218776

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
  #print(i)
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
#D=spam2full(D)

# lambdaD=1.720064e-12
# lambdaT=5.332215e-14
# lambdaS=2.466260e-16

# lambdaD=3.145803e-01
# lambdaT= 1.866849e-06
# lambdaS= 1.193907e-02

## do extreme case first
lambdaD=1
lambdaT= 1
lambdaS= 1

#lambdaD=.9
#lambdaT=.01
#lambdaS=.1

P=bdiag.spam(0,lambdaD*t(flattenD2W)%*%flattenD2W,lambdaT*t(temporal)%*%temporal, lambdaS*t(seasonal)%*%seasonal)
P2=bdiag.spam(0,lambdaD*t(flattenD4W)%*%flattenD4W,lambdaT*t(temporal)%*%temporal, lambdaS*t(seasonal)%*%seasonal)
#P3=bdiag.spam(0,lambdaD*t(DnormAbsExtreme)%*%DnormAbsExtreme,lambdaT*t(temporal)%*%temporal, lambdaS*t(seasonal)%*%seasonal)
#P4=bdiag.spam(0,lambdaD*t(D4)%*%D4,lambdaT*t(temporal)%*%temporal, lambdaS*t(seasonal)%*%seasonal)
#P5=bdiag.spam(0,lambdaD*t(D5)%*%D5,lambdaT*t(temporal)%*%temporal, lambdaS*t(seasonal)%*%seasonal)
#P6=bdiag.spam(0,lambdaD*t(D6)%*%D6,lambdaT*t(temporal)%*%temporal, lambdaS*t(seasonal)%*%seasonal)



ridgeNuD=0.25
ridgeNuT=0.25
ridgeNuS=1

Q=diag(c(0,ridgeNuD*as.vector(rep(1,ncol(B))),ridgeNuT*as.vector(rep(1,ncol(temporal))),
         ridgeNuS*as.vector(rep(1,ncol(seasonal)))))
#Q=diag(c(0,ridgeNuD*as.vector(rep(1,ncol(B)))))
Q=as.spam(Q)
dim(Q)
## 79  x 79


Bnew=cbind(rep(1,nrow(B)),B,temporal,seasonal)
#Bnew=cbind(rep1,nrow(B),B)
dim(Bnew)
## 7634   79

class(Bnew)
class(P)
class(Q)
betaHat1=solve(t(Bnew)%*%Bnew+P+Q)%*%t(Bnew)%*%y ## still fast
betaHat2=solve(t(Bnew)%*%Bnew+P2+Q)%*%t(Bnew)%*%y
#betaHat3=solve(t(Bnew)%*%Bnew+P3+Q)%*%t(Bnew)%*%y
#betaHat4=solve(t(Bnew)%*%Bnew+P4+Q)%*%t(Bnew)%*%y
#betaHat5=solve(t(Bnew)%*%Bnew+P5+Q)%*%t(Bnew)%*%y
#betaHat6=solve(t(Bnew)%*%Bnew+P6+Q)%*%t(Bnew)%*%y
#betaHat=solve(t(Bnew)%*%Bnew+P)%*%t(Bnew)%*%y
dim(betaHat) 

yHat=Bnew%*%betaHat1
yHat2=Bnew%*%betaHat2
#yHat3=Bnew%*%betaHat3
#yHat4=Bnew%*%betaHat4
#yHat5=Bnew%*%betaHat5
#yHat6=Bnew%*%betaHat6

summary(y)
summary(yHat)

y=allData$chl
yHat=exp(yHat)
yHat2=exp(yHat2)
#yHat3=exp(yHat3)
#yHat4=exp(yHat4)
#yHat5=exp(yHat5)
#yHat6=exp(yHat6)

plot(y,yHat)
abline(0,1,col="red")

#plot(y,yHat,xlim=c(0,20))
#abline(0,1,col="red")

rmse=sqrt(sum((y-yHat)^2)/length(y)) 
rmse2=sqrt(sum((y-yHat2)^2)/length(y)) 
#rmse3=sqrt(sum((y-yHat3)^2)/length(y)) 
#rmse4=sqrt(sum((y-yHat4)^2)/length(y)) 
#rmse5=sqrt(sum((y-yHat5)^2)/length(y)) 
#rmse6=sqrt(sum((y-yHat6)^2)/length(y)) 

c(rmse,rmse2)
## 7.325476 7.325483

## not a lot of difference

apply(tail(cbind(betaHat1,betaHat2)),1,sd)
## 3.963632e-07 1.640338e-07 1.485070e-06 2.340834e-06 3.844379e-06 1.201372e-06 

## weights based on "distance" seem to stabilize differences between shreve and ODonnell weights
