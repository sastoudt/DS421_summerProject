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
D=spatial_penalty(adjacency,shreve.order,lambdaD,nrow(adjacency))
D2=spatial_penalty(adjacency,shreve.order/sum(shreve.order),lambdaD,nrow(adjacency))
D3=spatial_penalty(adjacency,wgts,lambdaD,nrow(adjacency))
D4=spatial_penalty(adjacency,wgtsMax1,lambdaD,nrow(adjacency))
D5=spatial_penalty(adjacency,wgtsSame,lambdaD,nrow(adjacency))
D6=spatial_penalty(adjacency,wgtsZero,lambdaD,nrow(adjacency))

max(abs(D-D2)) ## 47.93878
max(abs(D-D3)) ## 30.72
max(abs(D-D4)) ## 45.72
max(abs(D-D5)) ## 45
max(abs(D-D6)) ## 48

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

lambdaD=1.720064e-12
lambdaT=5.332215e-14
lambdaS=2.466260e-16

P=bdiag.spam(0,lambdaD*t(D)%*%D,lambdaT*t(temporal)%*%temporal, lambdaS*t(seasonal)%*%seasonal)
P2=bdiag.spam(0,lambdaD*t(D2)%*%D2,lambdaT*t(temporal)%*%temporal, lambdaS*t(seasonal)%*%seasonal)
P3=bdiag.spam(0,lambdaD*t(D3)%*%D3,lambdaT*t(temporal)%*%temporal, lambdaS*t(seasonal)%*%seasonal)
P4=bdiag.spam(0,lambdaD*t(D4)%*%D4,lambdaT*t(temporal)%*%temporal, lambdaS*t(seasonal)%*%seasonal)
P5=bdiag.spam(0,lambdaD*t(D5)%*%D5,lambdaT*t(temporal)%*%temporal, lambdaS*t(seasonal)%*%seasonal)
P6=bdiag.spam(0,lambdaD*t(D6)%*%D6,lambdaT*t(temporal)%*%temporal, lambdaS*t(seasonal)%*%seasonal)

max(abs(P-P2)) ## 5.284028e-09
max(abs(P-P3)) ## 4.328683e-09
max(abs(P-P4)) ## 5.271966e-09
max(abs(P-P5)) ## 5.263396e-09
max(abs(P-P6)) ##5.284037e-09

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
betaHat1=solve(t(Bnew)%*%Bnew+P+Q)%*%t(Bnew)%*%y ## still fast
betaHat2=solve(t(Bnew)%*%Bnew+P2+Q)%*%t(Bnew)%*%y
betaHat3=solve(t(Bnew)%*%Bnew+P3+Q)%*%t(Bnew)%*%y
betaHat4=solve(t(Bnew)%*%Bnew+P4+Q)%*%t(Bnew)%*%y
betaHat5=solve(t(Bnew)%*%Bnew+P5+Q)%*%t(Bnew)%*%y
betaHat6=solve(t(Bnew)%*%Bnew+P6+Q)%*%t(Bnew)%*%y
#betaHat=solve(t(Bnew)%*%Bnew+P)%*%t(Bnew)%*%y
dim(betaHat) 

yHat=Bnew%*%betaHat1
yHat2=Bnew%*%betaHat2
yHat3=Bnew%*%betaHat3
yHat4=Bnew%*%betaHat4
yHat5=Bnew%*%betaHat5
yHat6=Bnew%*%betaHat6

summary(y)
summary(yHat)

y=allData$chl
yHat=exp(yHat)
yHat2=exp(yHat2)
yHat3=exp(yHat3)
yHat4=exp(yHat4)
yHat5=exp(yHat5)
yHat6=exp(yHat6)

plot(y,yHat)
abline(0,1,col="red")

#plot(y,yHat,xlim=c(0,20))
#abline(0,1,col="red")

rmse=sqrt(sum((y-yHat)^2)/length(y)) 
rmse2=sqrt(sum((y-yHat2)^2)/length(y)) 
rmse3=sqrt(sum((y-yHat3)^2)/length(y)) 
rmse4=sqrt(sum((y-yHat4)^2)/length(y)) 
rmse5=sqrt(sum((y-yHat5)^2)/length(y)) 
rmse6=sqrt(sum((y-yHat6)^2)/length(y)) 

c(rmse,rmse2,rmse3,rmse4,rmse5,rmse6)
tail(cbind(betaHat,betaHat2,betaHat3,betaHat4,betaHat5,betaHat6))


#####

require(mgcv)
require(spam)
setwd("~/Desktop/sfei")
allData<-read.csv("allData.csv",stringsAsFactors=F)
# shreve.order<-c(1,4,3,3,1,2,1,1,5,1,4,1,1)
# shreve.orderNorm<-shreve.order/sum(shreve.order)
# wgts<-c(1,4,3/4,3,1,2/3,1/3,1/4,5,1/5,4/5,1/2,1/2)
# wgtsMax1<-c(1,1,3/4,1,1,2/3,1/3,1/4,1,1/5,4/5,1/2,1/2)
# wgtsSame<-rep(1,13)
# wgtsZero<-rep(0,13)
# 
# row.names=col.names=unique(allData$Station)[-c(1,10)]
# 
# sfeiAdjMatrix<-matrix(0,nrow=13,ncol=13)
# sfeiAdjMatrix[1,5]=1 ## C3 --> D22
# sfeiAdjMatrix[2,11]=1 ## D10 --> D8
# sfeiAdjMatrix[3,2]=1 ## D12 --> D10
# sfeiAdjMatrix[4,3]=1 ## D19 --> D12
# sfeiAdjMatrix[5,8]=1 ## D22 --> D4
# sfeiAdjMatrix[6,4]=1 ## D26 --> D19
# sfeiAdjMatrix[7,4]=1 ## D28A --> D19
# sfeiAdjMatrix[8,2]=1 ## D4 --> D10
# sfeiAdjMatrix[11,9]=1 ## D8 --> D6
# sfeiAdjMatrix[11,10]=1 ## D8 --> D7
# sfeiAdjMatrix[12,6]=1 ## MD10 --> D28A
# sfeiAdjMatrix[13,6]=1 ## P8 --> D28A
# 
# row.names(sfeiAdjMatrix)=row.names
# colnames(sfeiAdjMatrix)=col.names
# bid=c("111011","111","1111","11111","11101","111110","111111","1110","1","10","11","1111101","1111101")
# 
# 
# lambdaD=1
# adjacency=sfeiAdjMatrix
# D=spatial_penalty(adjacency,shreve.order,lambdaD,nrow(adjacency))
# D2=spatial_penalty(adjacency,shreve.order/sum(shreve.order),lambdaD,nrow(adjacency))
# D3=spatial_penalty(adjacency,wgts,lambdaD,nrow(adjacency))
# D4=spatial_penalty(adjacency,wgtsMax1,lambdaD,nrow(adjacency))
# D5=spatial_penalty(adjacency,wgtsSame,lambdaD,nrow(adjacency))
# D6=spatial_penalty(adjacency,wgtsZero,lambdaD,nrow(adjacency))
# 
# max(abs(D-D2)) ## 47.93878
# max(abs(D-D3)) ## 30.72
# max(abs(D-D4)) ## 45.72
# max(abs(D-D5)) ## 45
# max(abs(D-D6)) ## 48
# 
# #D=spatial_penalty(adjacency,shreve.order/sum(shreve.order),lambdaD,nrow(adjacency))
# # D=D2
# # D=spatial_penalty(adjacency,wgts,lambdaD,nrow(adjacency))
# # D3=spatial_penalty(adjacency,rep(1,nrow(adjacency)),lambdaD,nrow(adjacency))
# # D4=spatial_penalty(adjacency,shreve.order/sum(shreve.order),lambdaD,nrow(adjacency))
# # max(abs(D-D2)) ##45.72
# # max(abs(D-D3)) ##2.055556
# # max(abs(D2-D3)) ##45
# # max(abs(D4-D3)) ## 2.992347
# # max(abs(D4-D2)) ## 47.93878
# # max(abs(D4-D)) ##2.218776
# 
allData<-allData[-which(allData$Station=="C10"),]
allData<-allData[-which(allData$Station=="D41"),]

allData<-allData[-which(is.na(allData$chl)),]
# nrow(allData)
# 
# y=allData$chl
 y=log(allData$chl)
# p=nrow(adjacency)
# B=matrix(0,nrow=length(y),ncol=p)
# seg=allData$Station
# lookUp=as.data.frame(cbind(unique(allData$Station),1:length(unique(allData$Station))))
# names(lookUp)=c("station","p")
# lookUp$station=as.character(lookUp$station)
# lookUp$p=as.numeric(as.character(lookUp$p))
# ## do without a loop later, just get a quick sense
# for(i in 1:nrow(B)){
#   index=which(lookUp$station==seg[i])
#   B[i,index]=1
#   #print(i)
# }
# 
# B=as.spam(B)
# dim(B)
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

lambdaD=1.720064e-12
lambdaT=5.332215e-14
lambdaS=2.466260e-16

P=bdiag.spam(0,lambdaT*t(temporal)%*%temporal, lambdaS*t(seasonal)%*%seasonal)
# P=bdiag.spam(0,lambdaD*t(D)%*%D,lambdaT*t(temporal)%*%temporal, lambdaS*t(seasonal)%*%seasonal)
# P2=bdiag.spam(0,lambdaD*t(D2)%*%D2,lambdaT*t(temporal)%*%temporal, lambdaS*t(seasonal)%*%seasonal)
# P3=bdiag.spam(0,lambdaD*t(D3)%*%D3,lambdaT*t(temporal)%*%temporal, lambdaS*t(seasonal)%*%seasonal)
# P4=bdiag.spam(0,lambdaD*t(D4)%*%D4,lambdaT*t(temporal)%*%temporal, lambdaS*t(seasonal)%*%seasonal)
# P5=bdiag.spam(0,lambdaD*t(D5)%*%D5,lambdaT*t(temporal)%*%temporal, lambdaS*t(seasonal)%*%seasonal)
# P6=bdiag.spam(0,lambdaD*t(D6)%*%D6,lambdaT*t(temporal)%*%temporal, lambdaS*t(seasonal)%*%seasonal)
# 
# max(abs(P-P2)) ## 5.284028e-09
# max(abs(P-P3)) ## 4.328683e-09
# max(abs(P-P4)) ## 5.271966e-09
# max(abs(P-P5)) ## 5.263396e-09
# max(abs(P-P6)) ##5.284037e-09

dim(P)
## 66x66

ridgeNuD=0.25
ridgeNuT=0.25
ridgeNuS=1

#Q=diag(c(0,ridgeNuD*as.vector(rep(1,ncol(B))),ridgeNuT*as.vector(rep(1,ncol(temporal))),
#         ridgeNuS*as.vector(rep(1,ncol(seasonal)))))
#Q=diag(c(0,ridgeNuD*as.vector(rep(1,ncol(B)))))
Q=diag(c(0,ridgeNuT*as.vector(rep(1,ncol(temporal))),
         ridgeNuS*as.vector(rep(1,ncol(seasonal)))))
Q=as.spam(Q)
dim(Q)
## 66x66


#Bnew=cbind(rep(1,nrow(B)),B,temporal,seasonal)
#Bnew=cbind(rep1,nrow(B),B)
Bnew=cbind(rep(1,nrow(temporal)),temporal,seasonal)
dim(Bnew)
## 7634   66

class(Bnew)
class(P)
class(Q)
betaHat=solve(t(Bnew)%*%Bnew+P+Q)%*%t(Bnew)%*%y
# betaHat1=solve(t(Bnew)%*%Bnew+P+Q)%*%t(Bnew)%*%y ## still fast
# betaHat2=solve(t(Bnew)%*%Bnew+P2+Q)%*%t(Bnew)%*%y
# betaHat3=solve(t(Bnew)%*%Bnew+P3+Q)%*%t(Bnew)%*%y
# betaHat4=solve(t(Bnew)%*%Bnew+P4+Q)%*%t(Bnew)%*%y
# betaHat5=solve(t(Bnew)%*%Bnew+P5+Q)%*%t(Bnew)%*%y
# betaHat6=solve(t(Bnew)%*%Bnew+P6+Q)%*%t(Bnew)%*%y
#betaHat=solve(t(Bnew)%*%Bnew+P)%*%t(Bnew)%*%y
dim(betaHat) 

yHat=Bnew%*%betaHat
# yHat2=Bnew%*%betaHat2
# yHat3=Bnew%*%betaHat3
# yHat4=Bnew%*%betaHat4
# yHat5=Bnew%*%betaHat5
# yHat6=Bnew%*%betaHat6

summary(y)
summary(yHat)

y=allData$chl
yHat=exp(yHat)
# yHat2=exp(yHat2)
# yHat3=exp(yHat3)
# yHat4=exp(yHat4)
# yHat5=exp(yHat5)
# yHat6=exp(yHat6)

plot(y,yHat)
abline(0,1,col="red")

#plot(y,yHat,xlim=c(0,20))
#abline(0,1,col="red")

rmse=sqrt(sum((y-yHat)^2)/length(y)) 
rmse ## 7.157335 no station info
# rmse2=sqrt(sum((y-yHat2)^2)/length(y)) 
# rmse3=sqrt(sum((y-yHat3)^2)/length(y)) 
# rmse4=sqrt(sum((y-yHat4)^2)/length(y)) 
# rmse5=sqrt(sum((y-yHat5)^2)/length(y)) 
# rmse6=sqrt(sum((y-yHat6)^2)/length(y)) 
# 
# c(rmse,rmse2,rmse3,rmse4,rmse5,rmse6)
# tail(cbind(betaHat,betaHat2,betaHat3,betaHat4,betaHat5,betaHat6))
# 

##########

require(mgcv)
require(spam)
setwd("~/Desktop/sfei")
allData<-read.csv("allData.csv",stringsAsFactors=F)
# shreve.order<-c(1,4,3,3,1,2,1,1,5,1,4,1,1)
# shreve.orderNorm<-shreve.order/sum(shreve.order)
# wgts<-c(1,4,3/4,3,1,2/3,1/3,1/4,5,1/5,4/5,1/2,1/2)
# wgtsMax1<-c(1,1,3/4,1,1,2/3,1/3,1/4,1,1/5,4/5,1/2,1/2)
# wgtsSame<-rep(1,13)
 wgtsZero<-rep(0,13)
# 
 row.names=col.names=unique(allData$Station)[-c(1,10)]
# 
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
# D=spatial_penalty(adjacency,shreve.order,lambdaD,nrow(adjacency))
# D2=spatial_penalty(adjacency,shreve.order/sum(shreve.order),lambdaD,nrow(adjacency))
# D3=spatial_penalty(adjacency,wgts,lambdaD,nrow(adjacency))
# D4=spatial_penalty(adjacency,wgtsMax1,lambdaD,nrow(adjacency))
# D5=spatial_penalty(adjacency,wgtsSame,lambdaD,nrow(adjacency))
 D=spatial_penalty(adjacency,wgtsZero,lambdaD,nrow(adjacency))
# 
# max(abs(D-D2)) ## 47.93878
# max(abs(D-D3)) ## 30.72
# max(abs(D-D4)) ## 45.72
# max(abs(D-D5)) ## 45
# max(abs(D-D6)) ## 48
# 
# #D=spatial_penalty(adjacency,shreve.order/sum(shreve.order),lambdaD,nrow(adjacency))
# # D=D2
# # D=spatial_penalty(adjacency,wgts,lambdaD,nrow(adjacency))
# # D3=spatial_penalty(adjacency,rep(1,nrow(adjacency)),lambdaD,nrow(adjacency))
# # D4=spatial_penalty(adjacency,shreve.order/sum(shreve.order),lambdaD,nrow(adjacency))
# # max(abs(D-D2)) ##45.72
# # max(abs(D-D3)) ##2.055556
# # max(abs(D2-D3)) ##45
# # max(abs(D4-D3)) ## 2.992347
# # max(abs(D4-D2)) ## 47.93878
# # max(abs(D4-D)) ##2.218776
# 
allData<-allData[-which(allData$Station=="C10"),]
allData<-allData[-which(allData$Station=="D41"),]

allData<-allData[-which(is.na(allData$chl)),]
# nrow(allData)
# 
# y=allData$chl
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
#  7634   13

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

lambdaD=1.720064e-12
lambdaT=5.332215e-14
lambdaS=2.466260e-16

#P=bdiag.spam(0,lambdaT*t(temporal)%*%temporal, lambdaS*t(seasonal)%*%seasonal)
 P=bdiag.spam(0,lambdaD*t(D)%*%D,lambdaT*t(temporal)%*%temporal, lambdaS*t(seasonal)%*%seasonal)
# P2=bdiag.spam(0,lambdaD*t(D2)%*%D2,lambdaT*t(temporal)%*%temporal, lambdaS*t(seasonal)%*%seasonal)
# P3=bdiag.spam(0,lambdaD*t(D3)%*%D3,lambdaT*t(temporal)%*%temporal, lambdaS*t(seasonal)%*%seasonal)
# P4=bdiag.spam(0,lambdaD*t(D4)%*%D4,lambdaT*t(temporal)%*%temporal, lambdaS*t(seasonal)%*%seasonal)
# P5=bdiag.spam(0,lambdaD*t(D5)%*%D5,lambdaT*t(temporal)%*%temporal, lambdaS*t(seasonal)%*%seasonal)
# P6=bdiag.spam(0,lambdaD*t(D6)%*%D6,lambdaT*t(temporal)%*%temporal, lambdaS*t(seasonal)%*%seasonal)
# 
# max(abs(P-P2)) ## 5.284028e-09
# max(abs(P-P3)) ## 4.328683e-09
# max(abs(P-P4)) ## 5.271966e-09
# max(abs(P-P5)) ## 5.263396e-09
# max(abs(P-P6)) ##5.284037e-09

dim(P)
## 66x66

ridgeNuD=0.25
ridgeNuT=0.25
ridgeNuS=1

Q=diag(c(0,ridgeNuD*as.vector(rep(1,ncol(B))),ridgeNuT*as.vector(rep(1,ncol(temporal))),
         ridgeNuS*as.vector(rep(1,ncol(seasonal)))))
#Q=diag(c(0,ridgeNuD*as.vector(rep(1,ncol(B)))))
#Q=diag(c(0,ridgeNuT*as.vector(rep(1,ncol(temporal))),
 #        ridgeNuS*as.vector(rep(1,ncol(seasonal)))))
Q=as.spam(Q)
dim(Q)
## 66x66


Bnew=cbind(rep(1,nrow(B)),B,temporal,seasonal)
#Bnew=cbind(rep1,nrow(B),B)
#Bnew=cbind(rep(1,nrow(temporal)),temporal,seasonal)
dim(Bnew)
## 7634   66

class(Bnew)
class(P)
class(Q)
betaHat=solve(t(Bnew)%*%Bnew+P+Q)%*%t(Bnew)%*%y
# betaHat1=solve(t(Bnew)%*%Bnew+P+Q)%*%t(Bnew)%*%y ## still fast
# betaHat2=solve(t(Bnew)%*%Bnew+P2+Q)%*%t(Bnew)%*%y
# betaHat3=solve(t(Bnew)%*%Bnew+P3+Q)%*%t(Bnew)%*%y
# betaHat4=solve(t(Bnew)%*%Bnew+P4+Q)%*%t(Bnew)%*%y
# betaHat5=solve(t(Bnew)%*%Bnew+P5+Q)%*%t(Bnew)%*%y
# betaHat6=solve(t(Bnew)%*%Bnew+P6+Q)%*%t(Bnew)%*%y
#betaHat=solve(t(Bnew)%*%Bnew+P)%*%t(Bnew)%*%y
dim(betaHat) 

yHat=Bnew%*%betaHat
# yHat2=Bnew%*%betaHat2
# yHat3=Bnew%*%betaHat3
# yHat4=Bnew%*%betaHat4
# yHat5=Bnew%*%betaHat5
# yHat6=Bnew%*%betaHat6

summary(y)
summary(yHat)

y=allData$chl
yHat=exp(yHat)
# yHat2=exp(yHat2)
# yHat3=exp(yHat3)
# yHat4=exp(yHat4)
# yHat5=exp(yHat5)
# yHat6=exp(yHat6)

plot(y,yHat)
abline(0,1,col="red")

#plot(y,yHat,xlim=c(0,20))
#abline(0,1,col="red")

rmse=sqrt(sum((y-yHat)^2)/length(y)) 
rmse ##  6.942123 ## no penalty on D but telling which station the observation is at


######
require(mgcv)
require(spam)
setwd("~/Desktop/sfei")
allData<-read.csv("allData.csv",stringsAsFactors=F)
# shreve.order<-c(1,4,3,3,1,2,1,1,5,1,4,1,1)
# shreve.orderNorm<-shreve.order/sum(shreve.order)
# wgts<-c(1,4,3/4,3,1,2/3,1/3,1/4,5,1/5,4/5,1/2,1/2)
 wgtsMax1<-c(1,1,3/4,1,1,2/3,1/3,1/4,1,1/5,4/5,1/2,1/2)
# wgtsSame<-rep(1,13)
#wgtsZero<-rep(0,13)
# 
row.names=col.names=unique(allData$Station)[-c(1,10)]
# 
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
# D=spatial_penalty(adjacency,shreve.order,lambdaD,nrow(adjacency))
# D2=spatial_penalty(adjacency,shreve.order/sum(shreve.order),lambdaD,nrow(adjacency))
# D3=spatial_penalty(adjacency,wgts,lambdaD,nrow(adjacency))
 D=spatial_penalty(adjacency,wgtsMax1,lambdaD,nrow(adjacency))
# D5=spatial_penalty(adjacency,wgtsSame,lambdaD,nrow(adjacency))
#D=spatial_penalty(adjacency,wgtsZero,lambdaD,nrow(adjacency))
# 
# max(abs(D-D2)) ## 47.93878
# max(abs(D-D3)) ## 30.72
# max(abs(D-D4)) ## 45.72
# max(abs(D-D5)) ## 45
# max(abs(D-D6)) ## 48
# 
# #D=spatial_penalty(adjacency,shreve.order/sum(shreve.order),lambdaD,nrow(adjacency))
# # D=D2
# # D=spatial_penalty(adjacency,wgts,lambdaD,nrow(adjacency))
# # D3=spatial_penalty(adjacency,rep(1,nrow(adjacency)),lambdaD,nrow(adjacency))
# # D4=spatial_penalty(adjacency,shreve.order/sum(shreve.order),lambdaD,nrow(adjacency))
# # max(abs(D-D2)) ##45.72
# # max(abs(D-D3)) ##2.055556
# # max(abs(D2-D3)) ##45
# # max(abs(D4-D3)) ## 2.992347
# # max(abs(D4-D2)) ## 47.93878
# # max(abs(D4-D)) ##2.218776
# 
allData<-allData[-which(allData$Station=="C10"),]
allData<-allData[-which(allData$Station=="D41"),]

allData<-allData[-which(is.na(allData$chl)),]
# nrow(allData)
# 
# y=allData$chl
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
#  7634   13

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

lambdaD=1.720064e-12
lambdaT=5.332215e-14
lambdaS=2.466260e-16
#lambdaD=1

#lambdaD=lambdaT=lambdaS=1

#P=bdiag.spam(0,lambdaT*t(temporal)%*%temporal, lambdaS*t(seasonal)%*%seasonal)
P=bdiag.spam(0,lambdaD*t(D)%*%D,lambdaT*t(temporal)%*%temporal, lambdaS*t(seasonal)%*%seasonal)
# P2=bdiag.spam(0,lambdaD*t(D2)%*%D2,lambdaT*t(temporal)%*%temporal, lambdaS*t(seasonal)%*%seasonal)
# P3=bdiag.spam(0,lambdaD*t(D3)%*%D3,lambdaT*t(temporal)%*%temporal, lambdaS*t(seasonal)%*%seasonal)
# P4=bdiag.spam(0,lambdaD*t(D4)%*%D4,lambdaT*t(temporal)%*%temporal, lambdaS*t(seasonal)%*%seasonal)
# P5=bdiag.spam(0,lambdaD*t(D5)%*%D5,lambdaT*t(temporal)%*%temporal, lambdaS*t(seasonal)%*%seasonal)
# P6=bdiag.spam(0,lambdaD*t(D6)%*%D6,lambdaT*t(temporal)%*%temporal, lambdaS*t(seasonal)%*%seasonal)
# 
# max(abs(P-P2)) ## 5.284028e-09
# max(abs(P-P3)) ## 4.328683e-09
# max(abs(P-P4)) ## 5.271966e-09
# max(abs(P-P5)) ## 5.263396e-09
# max(abs(P-P6)) ##5.284037e-09
max(lambdaD*t(D)%*%D) ##1.207072e-11
max(lambdaT*t(temporal)%*%temporal) ## 1.104171e-11
max(lambdaS*t(seasonal)%*%seasonal) ## 3.925069e-14
dim(P)
## 66x66

ridgeNuD=0.25
ridgeNuT=0.25
ridgeNuS=1

Q=diag(c(0,ridgeNuD*as.vector(rep(1,ncol(B))),ridgeNuT*as.vector(rep(1,ncol(temporal))),
         ridgeNuS*as.vector(rep(1,ncol(seasonal)))))
#Q=diag(c(0,ridgeNuD*as.vector(rep(1,ncol(B)))))
#Q=diag(c(0,ridgeNuT*as.vector(rep(1,ncol(temporal))),
#        ridgeNuS*as.vector(rep(1,ncol(seasonal)))))
Q=as.spam(Q)
dim(Q)
## 66x66


Bnew=cbind(rep(1,nrow(B)),B,temporal,seasonal)
#Bnew=cbind(rep1,nrow(B),B)
#Bnew=cbind(rep(1,nrow(temporal)),temporal,seasonal)
dim(Bnew)
## 7634   66

class(Bnew)
class(P)
class(Q)
betaHat=solve(t(Bnew)%*%Bnew+P+Q)%*%t(Bnew)%*%y
# betaHat1=solve(t(Bnew)%*%Bnew+P+Q)%*%t(Bnew)%*%y ## still fast
# betaHat2=solve(t(Bnew)%*%Bnew+P2+Q)%*%t(Bnew)%*%y
# betaHat3=solve(t(Bnew)%*%Bnew+P3+Q)%*%t(Bnew)%*%y
# betaHat4=solve(t(Bnew)%*%Bnew+P4+Q)%*%t(Bnew)%*%y
# betaHat5=solve(t(Bnew)%*%Bnew+P5+Q)%*%t(Bnew)%*%y
# betaHat6=solve(t(Bnew)%*%Bnew+P6+Q)%*%t(Bnew)%*%y
#betaHat=solve(t(Bnew)%*%Bnew+P)%*%t(Bnew)%*%y
dim(betaHat) 

yHat=Bnew%*%betaHat
# yHat2=Bnew%*%betaHat2
# yHat3=Bnew%*%betaHat3
# yHat4=Bnew%*%betaHat4
# yHat5=Bnew%*%betaHat5
# yHat6=Bnew%*%betaHat6

summary(y)
summary(yHat)

y=allData$chl
yHat=exp(yHat)
# yHat2=exp(yHat2)
# yHat3=exp(yHat3)
# yHat4=exp(yHat4)
# yHat5=exp(yHat5)
# yHat6=exp(yHat6)

plot(y,yHat)
abline(0,1,col="red")

#plot(y,yHat,xlim=c(0,20))
#abline(0,1,col="red")

rmse=sqrt(sum((y-yHat)^2)/length(y)) 
rmse ##  6.942123 ## same as before

pred=cbind.data.frame(allData$Station,yHat)
names(pred)=c("station","predVal")
require(dplyr)

byStation=group_by(pred,station)
meanStation=summarise(byStation,meanPred=mean(predVal),sdPred=sd(predVal),minPred=min(predVal),maxPred=max(predVal))
meanStation

## need to visualize penalty

Dfull=spam2full(D)

flatten<-c()
for(i in 1:13){
  for(j in 1:13){
    flatten=rbind(flatten,c(i,j,Dfull[i,j]))
  }
}


flatten

toPlot=as.data.frame(flatten[which(flatten[,3]!=0),])
nam=unique(allData$Station)
toPlot$stat1=nam[toPlot[,1]]
toPlot$stat2=nam[toPlot[,2]]
class(toPlot)
names(toPlot)
forMap=allData[,c("Longitude","Latitude","Station")]
forMap=unique(forMap)

toPlotM=merge(toPlot,forMap,by.x=c("stat1"),by.y=c("Station"))
toPlotM
names(toPlotM)[c(6,7)]=c("long1","lat1")

toPlotM2=merge(toPlotM,forMap,by.x=c("stat2"),by.y=c("Station"))
toPlotM2
names(toPlotM2)[c(8,9)]=c("long2","lat2")


rbPal <- colorRampPalette(c('red','blue'))
toPlotM3=toPlotM2[-which(toPlotM2$V1==toPlotM2$V2),]
b <- rbPal(10)[as.numeric(cut(toPlotM3$V3,breaks = 10))]
plot(forMap$Longitude,forMap$Latitude)
arrows(toPlotM3$long1,toPlotM3$lat1,toPlotM3$long2,toPlotM3$lat2,angle=90,code=2,length=0,lwd=5,col=b)

legend("topleft",col=rbPal(10),levels(cut(toPlotM3$V3,breaks = 10)),lty=1,lwd=2)

vizSpatialPenalty=function(Dfull,lab){
  #Dfull=spam2full(D)
  
  flatten<-c()
  for(i in 1:13){
    for(j in 1:13){
      flatten=rbind(flatten,c(i,j,Dfull[i,j]))
    }
  }
  
  
  toPlot=as.data.frame(flatten[which(flatten[,3]!=0),])
  nam=unique(allData$Station)
  toPlot$stat1=nam[toPlot[,1]]
  toPlot$stat2=nam[toPlot[,2]]
 
  forMap=allData[,c("Longitude","Latitude","Station")]
  forMap=unique(forMap)
  
  toPlotM=merge(toPlot,forMap,by.x=c("stat1"),by.y=c("Station"))
  toPlotM
  names(toPlotM)[c(6,7)]=c("long1","lat1")
  
  toPlotM2=merge(toPlotM,forMap,by.x=c("stat2"),by.y=c("Station"))
  toPlotM2
  names(toPlotM2)[c(8,9)]=c("long2","lat2")
  
  
  rbPal <- colorRampPalette(c('red','blue'))
  toPlotM3=toPlotM2[-which(toPlotM2$V1==toPlotM2$V2),]
  b <- rbPal(10)[as.numeric(cut(toPlotM3$V3,breaks = 10))]
  plot(forMap$Longitude,forMap$Latitude,main=lab)
  arrows(toPlotM3$long1,toPlotM3$lat1,toPlotM3$long2,toPlotM3$lat2,angle=90,code=2,length=0,lwd=5,col=b)
  
  legend("topleft",col=rbPal(10),levels(cut(toPlotM3$V3,breaks = 10)),lty=1,lwd=2,cex=.75)
  
}

## from diffWeights

par(mfrow=c(1,2))
vizSpatialPenalty(test1$D,"shreve")
vizSpatialPenalty(test2$D,"shreveNorm") ## same relative ordering, makes sense, good sanity check

vizSpatialPenalty(test2$D,"shreveNorm") 
vizSpatialPenalty(test4$D,"wgtsMax1") ## noticeable difference
##
text(forMap$Longitude,forMap$Latitude,forMap$Station)

## upper left part of y, flipped
## shreve norm, smallest in magnitude weights, other weights, highest in magnitude
## upper right part of y, same, smallest in magnitude

## ending portion, more extreme (highest in magnitude) for shreve, 
## middle of the pack for the other wieghts

## big change in weight magnitude D10 to D4 and D22 on wgtsMax1,
 ##D12 to D19 and that portion of network

## smoother transition of weights on shreve
