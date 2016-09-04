make_spam<-function(M){
  if(class(M) == "matrix") as.spam(M)
  if(class(M) == "spam") as.spam(M)
  else as.spam(as.spam.dgCMatrix(as(M, "dgCMatrix")))
}
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
shreve.order<-c(1,4,3,3,1,2,1,1,5,1,4,1,1)
shreve.orderNorm<-shreve.order/sum(shreve.order)
wgts<-c(1,4,3/4,3,1,2/3,1/3,1/4,5,1/5,4/5,1/2,1/2)
wgtsMax1<-c(1,1,3/4,1,1,2/3,1/3,1/4,1,1/5,4/5,1/2,1/2)
wgtsSame<-rep(1,13)
wgtsZero<-rep(0,13)

test1=compareWgts(shreve.order)
test2=compareWgts(shreve.orderNorm)
test3=compareWgts(wgts)
test4=compareWgts(wgtsMax1)
test5=compareWgts(wgtsSame)
test6=compareWgts(wgtsZero)
test6$rmse
max(abs(test1$P-test2$P))
max(abs(test1$P-test3$P))
max(abs(test6$P-test1$P))
compareWgts<-function(wgts){
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


lambdaD=1
adjacency=sfeiAdjMatrix
D=spatial_penalty(adjacency,wgts,lambdaD,nrow(adjacency))
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
D=spam2full(D)

lambdaD=1.720064e-12
lambdaT=5.332215e-14
lambdaS=2.466260e-16
D5=matrix(1,nrow=13,ncol=13)
D6=matrix(0,nrow=13,ncol=13)
P=bdiag.spam(0,lambdaD*t(D)%*%D,lambdaT*t(temporal)%*%temporal, lambdaS*t(seasonal)%*%seasonal)
# P2=bdiag.spam(0,lambdaD*t(D2)%*%D2,lambdaT*t(temporal)%*%temporal, lambdaS*t(seasonal)%*%seasonal)
# P3=bdiag.spam(0,lambdaD*t(D3)%*%D3,lambdaT*t(temporal)%*%temporal, lambdaS*t(seasonal)%*%seasonal)
# P4=bdiag.spam(0,lambdaD*t(D4)%*%D4,lambdaT*t(temporal)%*%temporal, lambdaS*t(seasonal)%*%seasonal)
# P5=bdiag.spam(0,lambdaD*t(D5)%*%D5,lambdaT*t(temporal)%*%temporal, lambdaS*t(seasonal)%*%seasonal)
# P=bdiag.spam(0,lambdaD*t(D5)%*%D5,lambdaT*t(temporal)%*%temporal, lambdaS*t(seasonal)%*%seasonal)
# P=bdiag.spam(0,lambdaD*t(D6)%*%D6,lambdaT*t(temporal)%*%temporal, lambdaS*t(seasonal)%*%seasonal)
# P=bdiag.spam(0,lambdaD*t(D)%*%D)
# max(abs(P-P2)) ##5.271966e-09
# max(abs(P-P3)) ##1.855174e-11
# max(abs(P-P4)) ## 1.206212e-11
# max(abs(P-P5)) ## 2.907768e-11
## difference in D really doesn't make a huge difference in P

## structure of D must be impactful though right?
## same rmse for P5 and P6, this doesn't make sense
## P6, no penalty on which station
# max(D) ## 2.28
# max(D2) ## 48
# max(D3) ## 3
# max(D4) ## 0.06122449
# max(D5) ## 1
# max(D6) ## 0


dim(P)
## 79 x 79

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
betaHat=solve(t(Bnew)%*%Bnew+P+Q)%*%t(Bnew)%*%y ## still fast
#betaHat=solve(t(Bnew)%*%Bnew+P)%*%t(Bnew)%*%y
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

return(list(betaHat=betaHat,yHat=yHat,rmse=rmse,P=P))
}
## hm....

### shreve.order 6.942123
### normalized shreve.order 
### weights
### max weights 1
### D=0
### just D, no seasonal/temporal
### trying to get a sense of value added
