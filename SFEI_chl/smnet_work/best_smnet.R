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
