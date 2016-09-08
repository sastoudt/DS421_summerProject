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
p=nrow(adjacency)

allData<-allData[-which(allData$Station=="C10"),]
allData<-allData[-which(allData$Station=="D41"),]

allData<-allData[-which(is.na(allData$chl)),]
nrow(allData)




## ridge regression parameters

ridgeParam1=ridgeParam2=ridgeParam3=c(0.25,0.5,0.75,1,5,10,20)
#lambdaParam1=lambdaParam2=lambdaParam3=seq(0, 1.5, by = 0.2)

paramGrid=expand.grid(ridgeParam1,ridgeParam2,ridgeParam3)
#paramGrid=expand.grid(ridgeParam1,ridgeParam2,ridgeParam3,lambdaParam1,lambdaParam2,lambdaParam3)
dim(paramGrid)
##  343

## function to be applied over paramGrid
## record desired metrics
## probably just easier to return fitted values, then can pick metrics later

## now we can get the optimal lambda parameters given ridge parameters, 
## so now we can do cross validation to get ridge, many fewer combinations
setwd("~/Desktop/sfei/crossValid_smnet")
fold1<-read.csv("fold1.csv",stringsAsFactors=F)
fold2<-read.csv("fold2.csv",stringsAsFactors=F)
fold3<-read.csv("fold3.csv",stringsAsFactors=F)
fold4<-read.csv("fold4.csv",stringsAsFactors=F)
fold5<-read.csv("fold5.csv",stringsAsFactors=F)

fd=vector("list",5)
fd[[1]]=fold1
fd[[2]]=fold2
fd[[3]]=fold3
fd[[4]]=fold4
fd[[5]]=fold5

folds=vector("list",5)
folds[[1]]=rbind.data.frame(fold2,fold3,fold4,fold5)
folds[[2]]=rbind.data.frame(fold1,fold3,fold4,fold5)
folds[[3]]=rbind.data.frame(fold2,fold1,fold4,fold5)
folds[[4]]=rbind.data.frame(fold2,fold3,fold1,fold5)
folds[[5]]=rbind.data.frame(fold2,fold3,fold4,fold1)

get_crit_exact<-function(rhoArgs, X, XTX, P, response, cholFactor, n, 
                         np = nrow(XTX), Xw, Xy, n.sm=n.sm, identifyBit, crit){    
  
  for(j in 1:n.sm) P[[j]]<-P[[j]]*exp(rhoArgs[j])
  Psum=bdiag.spam(0,P[[1]],P[[2]], P[[3]])
  #Psum     <- Reduce("+", P)
  info     <- XTX + Psum + identifyBit
  U        <- try(update.spam.chol.NgPeyton(cholFactor, info), silent = T)
  if(class(U) == "try-error"){
    out <- 10^20
  } else {
    beta_hat <- backsolve.spam(U, forwardsolve.spam(U, Xy))  
    left1    <- forwardsolve.spam(U, t(X))
    ED1      <- sum(left1*left1)
    ED       <- ED1
    fit      <- X %*% beta_hat  
    if(crit == "AICC") out <- log(sum((response - fit)^2)) + (2*(ED+1)/(n-ED-2))
    if(crit == "AIC")  out <- log(sum((response - fit)^2)) + 2*ED/n
    if(crit == "GCV")  out <- sum((response - fit)^2)/(1-(ED/n))^2
  }
  out
}

objective<-function(rhoArgs){
  get_crit_exact(rhoArgs,X,XTX,P,response,cholFactor,n,nrow(XTX),Xw,Xy,n.sm,identifyBit,crit)
}


  lambdaPar=c()
  predValOld=c()
  predValNew=c()
  for(k in 1:5){
    data=folds[[k]]
    #y=allData$chl
    y=log(data$chl)
    seg=data$Station
    
    B=matrix(0,nrow=length(y),ncol=p)
    
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
    
    knots=seq(1,365,length.out=35) ##73
    seasonal=cSplineDes(data$doy, knots, ord = 4)
    seasonal=as.spam(seasonal)
    
    
    knots=seq(min(allData$date_dec), max(allData$date_dec),length.out=35)
    
    temporal=splineDesign(knots, data$date_dec, ord = 4, outer.ok = T,
                          sparse = FALSE)
    temporal=as.spam(temporal)
    
    #D=spam2full(D)
    
    
    #P=bdiag.spam(0,lambdaD*t(D)%*%D,lambdaT*t(temporal)%*%temporal, lambdaS*t(seasonal)%*%seasonal)
    
    ridgeNuD=ridgeNuT=ridgeNuS=1
    #ridgeNuD=as.numeric(nu[1])
    #ridgeNuT=as.numeric(nu[2])
    #ridgeNuS=as.numeric(nu[3])
    
    Q=diag(c(0,ridgeNuD*as.vector(rep(1,ncol(B))),ridgeNuT*as.vector(rep(1,ncol(temporal))),
             ridgeNuS*as.vector(rep(1,ncol(seasonal)))))
    Q=as.spam(Q)
    
    
    Bnew=cbind(rep(1,nrow(B)),B,temporal,seasonal)
    
    
    X=Bnew
    XTX=t(Bnew)%*%Bnew
    P=list(t(D)%*%D,t(temporal)%*%temporal, t(seasonal)%*%seasonal)
    response=log(data$chl)
    P2=bdiag.spam(0,t(D)%*%D,t(temporal)%*%temporal, t(seasonal)%*%seasonal)
    cholFactor=chol.spam(XTX+P2+Q,pivot="MMD",eps = 10^-6)
    n=nrow(data)
    Xw=1 ## Not used
    Xy=t(X)%*%response
    n.sm=3
    identifyBit=Q
    crit="GCV"
    
    start.vals <- rep(0, 3) 
    optimal  <- optim(par = start.vals,fn = objective, method = "Nelder-Mead", control=list(reltol = 10^-8, maxit = 500))
    P3=bdiag.spam(0,exp(optimal$par[1])*t(D)%*%D,exp(optimal$par[2])*t(temporal)%*%temporal, exp(optimal$par[3])*t(seasonal)%*%seasonal)
    
    lambdaPar=rbind(lambdaPar,optimal$par)
    betaHat=solve(t(Bnew)%*%Bnew+P3+Q)%*%t(Bnew)%*%y 
    yHatOld=Bnew%*%betaHat  
    ## need to predict for new values
    data=fd[[k]]
    y=log(data$chl)
    seg=data$Station
    
    B=matrix(0,nrow=length(y),ncol=p)
    
    
    for(i in 1:nrow(B)){
      index=which(lookUp$station==seg[i])
      B[i,index]=1
      #print(i)
    }
    
    B=as.spam(B)
    
    knots=seq(1,365,length.out=35) ##73
    seasonal=cSplineDes(data$doy, knots, ord = 4)
    seasonal=as.spam(seasonal)
    
    
    knots=seq(min(allData$date_dec), max(allData$date_dec),length.out=35)
    
    temporal=splineDesign(knots, data$date_dec, ord = 4, outer.ok = T,
                          sparse = FALSE)
    temporal=as.spam(temporal)
    
    #D=spam2full(D)
    
    
    #P=bdiag.spam(0,lambdaD*t(D)%*%D,lambdaT*t(temporal)%*%temporal, lambdaS*t(seasonal)%*%seasonal)
    
    ridgeNuD=ridgeNuT=ridgeNuS=1
    #ridgeNuD=as.numeric(nu[1])
    #ridgeNuT=as.numeric(nu[2])
    #ridgeNuS=as.numeric(nu[3])
    
    Q=diag(c(0,ridgeNuD*as.vector(rep(1,ncol(B))),ridgeNuT*as.vector(rep(1,ncol(temporal))),
             ridgeNuS*as.vector(rep(1,ncol(seasonal)))))
    Q=as.spam(Q)
    
    
    Bnew=cbind(rep(1,nrow(B)),B,temporal,seasonal)
    
    yHatNew=Bnew%*%betaHat  
    
    
    predValOld=cbind(predValOld,yHatOld)
    predValNew=cbind(predValNew,yHatNew)
  }
  
  dim(predValOld)
dim(predValNew)

processPredVal=function(predVal){
  #predVal=as.data.frame(predVal)
  rmse<-c()
  for(k in 1:5){
    rmse<-c(rmse, sqrt(sum((fd[[k]]$chl-exp(predVal[,k]))^2)/nrow(fd[[k]])))
  }
  return(median(rmse))
}

processPredValFull=function(predVal){
  #predVal=as.data.frame(predVal)
  rmse<-c()
  for(k in 1:5){
    rmse<-c(rmse, sqrt(sum((fd[[k]]$chl-exp(predVal[,k]))^2)/nrow(fd[[k]])))
  }
  return(rmse)
}


medRMSE=processPredVal(predValNew)
medRMSE  ##  6.709683 this seems weird, were doing much better before

plot(density(exp(predValNew[,1])),main="",xlab="")
for(i in 2:5){
  lines(density(exp(predValNew[,i])))
}
lines(density(allData$chl),col="red")
legend("topright",col=c("black","red"),lty=1,c("predict per fold","overall true"))

plot(density(fd[[1]]$chl-exp(predValNew[,1])),main="",xlab="")
for(i in 2:5){
  lines(density(fd[[i]]$chl-exp(predValNew[,i])))
}

processPredValFull(predValNew)
##  6.640046 6.954438 6.709683 7.243133 5.444410

setwd("~/Desktop/sfei/crossValid_smnet")
testing<-read.csv("testing.csv",stringsAsFactors=F)


paramTest=function(nu){
  get_crit_exact<-function(rhoArgs, X, XTX, P, response, cholFactor, n, 
                           np = nrow(XTX), Xw, Xy, n.sm=n.sm, identifyBit, crit){    
    
    for(j in 1:n.sm) P[[j]]<-P[[j]]*exp(rhoArgs[j])
    Psum=bdiag.spam(0,P[[1]],P[[2]], P[[3]])
    #Psum     <- Reduce("+", P)
    info     <- XTX + Psum + identifyBit
    U        <- try(update.spam.chol.NgPeyton(cholFactor, info), silent = T)
    if(class(U) == "try-error"){
      out <- 10^20
    } else {
      beta_hat <- backsolve.spam(U, forwardsolve.spam(U, Xy))  
      left1    <- forwardsolve.spam(U, t(X))
      ED1      <- sum(left1*left1)
      ED       <- ED1
      fit      <- X %*% beta_hat  
      if(crit == "AICC") out <- log(sum((response - fit)^2)) + (2*(ED+1)/(n-ED-2))
      if(crit == "AIC")  out <- log(sum((response - fit)^2)) + 2*ED/n
      if(crit == "GCV")  out <- sum((response - fit)^2)/(1-(ED/n))^2
    }
    out
  }
  
  objective<-function(rhoArgs){
    get_crit_exact(rhoArgs,X,XTX,P,response,cholFactor,n,nrow(XTX),Xw,Xy,n.sm,identifyBit,crit)
  }
  lambdaPar=c()
  predValNew=c()
  predValOld=c()
  for(k in 1:5){
    data=folds[[k]]
    #y=allData$chl
    y=log(data$chl)
    seg=data$Station
    
    B=matrix(0,nrow=length(y),ncol=p)
    
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
    
    knots=seq(1,365,length.out=35) ##73
    seasonal=cSplineDes(data$doy, knots, ord = 4)
    seasonal=as.spam(seasonal)

    
    knots=seq(min(allData$date_dec), max(allData$date_dec),length.out=35)
    
    temporal=splineDesign(knots, data$date_dec, ord = 4, outer.ok = T,
                          sparse = FALSE)
    temporal=as.spam(temporal)
  
    #D=spam2full(D)
   
    
    #P=bdiag.spam(0,lambdaD*t(D)%*%D,lambdaT*t(temporal)%*%temporal, lambdaS*t(seasonal)%*%seasonal)

    
    ridgeNuD=as.numeric(nu[1])
    ridgeNuT=as.numeric(nu[2])
    ridgeNuS=as.numeric(nu[3])
    
    Q=diag(c(0,ridgeNuD*as.vector(rep(1,ncol(B))),ridgeNuT*as.vector(rep(1,ncol(temporal))),
             ridgeNuS*as.vector(rep(1,ncol(seasonal)))))
    Q=as.spam(Q)
  
    
    Bnew=cbind(rep(1,nrow(B)),B,temporal,seasonal)
    
    
    X=Bnew
    XTX=t(Bnew)%*%Bnew
    P=list(t(D)%*%D,t(temporal)%*%temporal, t(seasonal)%*%seasonal)
    response=log(data$chl)
    P2=bdiag.spam(0,t(D)%*%D,t(temporal)%*%temporal, t(seasonal)%*%seasonal)
    cholFactor=chol.spam(XTX+P2+Q,pivot="MMD",eps = 10^-6)
    n=nrow(data)
    Xw=1 ## Not used
    Xy=t(X)%*%response
    n.sm=3
    identifyBit=Q
    crit="GCV"
    
    start.vals <- rep(0, 3) 
optimal  <- optim(par = start.vals,fn = objective, method = "Nelder-Mead", control=list(reltol = 10^-8, maxit = 500))
P3=bdiag.spam(0,exp(optimal$par[1])*t(D)%*%D,exp(optimal$par[2])*t(temporal)%*%temporal, exp(optimal$par[3])*t(seasonal)%*%seasonal)

lambdaPar=rbind(lambdaPar,optimal$par)
betaHat=solve(t(Bnew)%*%Bnew+P3+Q)%*%t(Bnew)%*%y 
yHatOld=Bnew%*%betaHat  
predValOld=cbind(predValOld,yHatOld)

### predict for new values

data=rbind(fd[[k]],testing)
#y=allData$chl
y=log(data$chl)
seg=data$Station

B=matrix(0,nrow=length(y),ncol=p)

## do without a loop later, just get a quick sense
for(i in 1:nrow(B)){
  index=which(lookUp$station==seg[i])
  B[i,index]=1
  #print(i)
}

B=as.spam(B)

knots=seq(1,365,length.out=35) ##73
seasonal=cSplineDes(data$doy, knots, ord = 4)
seasonal=as.spam(seasonal)


knots=seq(min(allData$date_dec), max(allData$date_dec),length.out=35)

temporal=splineDesign(knots, data$date_dec, ord = 4, outer.ok = T,
                      sparse = FALSE)
temporal=as.spam(temporal)
Bnew=cbind(rep(1,nrow(B)),B,temporal,seasonal)

yHatNew=Bnew%*%betaHat  
predValNew=cbind(predValNew,yHatNew)

  }
  return(list(lambdaPar=lambdaPar,predValOld=predValOld,predValNew=predValNew))
}

system.time(paramTest(paramGrid[1,])) ## 16 seconds
test=paramTest(paramGrid[1,])

require(parallel)
ptm <- proc.time()
nuOptim=mclapply(split(paramGrid, 1:nrow(paramGrid)),paramTest,mc.cores=4)
proc.time() - ptm ## 30 minutes, 39 min


save(nuOptim,file="nuOptim_1pt2.Rda")

length(nuOptim)
dim(nuOptim[[1]]$predValOld)
dim(nuOptim[[1]]$predValNew)
dim(nuOptim[[1]]$lambdaPar)


### process info in nuOptim

## minimum median rmse across folds

lambdaPar=lapply(nuOptim,function(x){x$lambdaPar})
predValOld=lapply(nuOptim,function(x){x$predValOld})
predValNew=lapply(nuOptim,function(x){x$predValNew})


medRMSE=unlist(lapply(predValNew,processPredVal))
summary(medRMSE) 

#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#7.520   7.527   7.543   7.556   7.583   7.641 

varRMSE=unlist(lapply(predValNew,processPredValVar))
summary(varRMSE)

#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#0.2151  0.2167  0.2191  0.2193  0.2215  0.2284 


processPredVal=function(predVal){
  predVal=as.data.frame(predVal)
  rmse<-c()
  for(k in 1:5){
   rmse<-c(rmse, sqrt(sum((rbind(fd[[k]],testing)$chl-exp(predVal[,k]))^2)/nrow(rbind(fd[[k]],testing))))
  }
  return(median(rmse))
}

processPredValVar=function(predVal){
  predVal=as.data.frame(predVal)
  rmse<-c()
  for(k in 1:5){
    rmse<-c(rmse, sqrt(sum((rbind(fd[[k]],testing)$chl-exp(predVal[,k]))^2)/nrow(rbind(fd[[k]],testing))))
  }
  return(sd(rmse))
}

paramGrid[which.min(medRMSE),]
#148 0.25 0.25    1

head(cbind(order(medRMSE),order(varRMSE)))

paramGrid[which.min(varRMSE),]
#Var1 Var2 Var3
#43 0.25   20 0.25

varRMSE[which.min(medRMSE)] ##0.2152567 
varRMSE[which.min(varRMSE)] ##0.2151498  
## so not a big deal variability wise

medRMSE[which.min(medRMSE)] ##7.520387  
medRMSE[which.min(varRMSE)] ##7.521771 

lambdaPar[[148]] ## same for each fold

exp(lambdaPar[[148]][1,])
## 3.233715e-01 5.469683e-06 1.289552e-02

apply(exp(lambdaPar[[which.min(medRMSE)]]),2,mean)
##3.145803e-01 1.866849e-06 1.193907e-02
## really should do some kind of weighting based on performance on the fold, but go with this for now

## small amount of smoothing

## smallest ridge param, we should try some smaller values just to be sure we aren't hitting a boundary

ridgeParam1=ridgeParam2=ridgeParam3=c(0.05,0.1,0.15,0.2,0.25)
paramGrid2=expand.grid(ridgeParam1,ridgeParam2,ridgeParam3)
dim(paramGrid2) ## 125 

ptm <- proc.time()
nuOptim2=mclapply(split(paramGrid2, 1:nrow(paramGrid2)),paramTest,mc.cores=4)
proc.time() - ptm ## about 10 minutes, 13 min

save(nuOptim2,file="nuOptim2_pt2.Rda")

lambdaPar=lapply(nuOptim2,function(x){x$lambdaPar})
predValOld=lapply(nuOptim2,function(x){x$predValOld})
predValNew=lapply(nuOptim2,function(x){x$predValNew})

medRMSE=unlist(lapply(predValNew,processPredVal))
summary(medRMSE)

#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#7.520   7.520   7.521   7.521   7.522   7.522 

varRMSE=unlist(lapply(predValNew,processPredValVar))
summary(varRMSE)
# 
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#0.2141  0.2148  0.2150  0.2150  0.2151  0.2152

medRMSE[which.min(medRMSE)] ##3.35037 

## not better

varRMSE[which.min(medRMSE)] ## 7.519621
## less variable

medRMSE[which.min(varRMSE)] ## 3.350481
varRMSE[which.min(varRMSE)] ## 0.340475 

## practically the same median rmse, a little less variability across folds, 
## might be worth taking advantage of

paramGrid2[which.min(varRMSE),]
##  0.05 0.05 0.05

## still on boundary

ridgeParam1=ridgeParam2=ridgeParam3=c(0.00005,0.0005,0.001,0.005,0.01)
paramGrid3=expand.grid(ridgeParam1,ridgeParam2,ridgeParam3)
dim(paramGrid3) ## 125

ptm <- proc.time()
nuOptim3=mclapply(split(paramGrid3, 1:nrow(paramGrid3)),paramTest,mc.cores=4)
proc.time() - ptm ## about 10 minutes,14 min

save(nuOptim3,file="nuOptim3_pt2.Rda")


lambdaPar=lapply(nuOptim3,function(x){x$lambdaPar})
predValOld=lapply(nuOptim3,function(x){x$predValOld})
predValNew=lapply(nuOptim3,function(x){x$predValNew})

medRMSE=unlist(lapply(predValNew,processPredVal))
summary(medRMSE)
##  3.35    3.35    3.35    3.35    3.35    3.35 
## No real improvement

varRMSE=unlist(lapply(predValNew,processPredValVar))
summary(varRMSE)
## slight improvment, but no real variability

## overall, robust to choice of the ridge param
paramGrid3[which.min(medRMSE),]
## 101 5e-05 5e-05 0.01


## since no real difference, go with more regularized

#148 0.25 0.25    1

## lambda
## 1.720064e-12 5.332215e-14 2.466260e-16
## seems small, how much do we give up on rmse if we make this less smooth



paramTestL=function(lambda){
  #lambdaPar=c()
  predValNew=c()
  predValOld=c()
  for(k in 1:5){
    data=folds[[k]]
    #y=allData$chl
    y=log(data$chl)
    seg=data$Station
    
    B=matrix(0,nrow=length(y),ncol=p)
    
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
    
    knots=seq(1,365,length.out=35) ##73
    seasonal=cSplineDes(data$doy, knots, ord = 4)
    seasonal=as.spam(seasonal)
    
    
    knots=seq(min(allData$date_dec), max(allData$date_dec),length.out=35)
    
    temporal=splineDesign(knots, data$date_dec, ord = 4, outer.ok = T,
                          sparse = FALSE)
    temporal=as.spam(temporal)
    
    #D=spam2full(D)
    
    
    #P=bdiag.spam(0,lambdaD*t(D)%*%D,lambdaT*t(temporal)%*%temporal, lambdaS*t(seasonal)%*%seasonal)
    
    
    ridgeNuD=0.25
    ridgeNuT=0.25
    ridgeNuS=1
    
    Q=diag(c(0,ridgeNuD*as.vector(rep(1,ncol(B))),ridgeNuT*as.vector(rep(1,ncol(temporal))),
             ridgeNuS*as.vector(rep(1,ncol(seasonal)))))
    Q=as.spam(Q)
    
    
    Bnew=cbind(rep(1,nrow(B)),B,temporal,seasonal)
    
    
    X=Bnew
    XTX=t(Bnew)%*%Bnew
    #P=list(t(D)%*%D,t(temporal)%*%temporal, t(seasonal)%*%seasonal)
    #response=log(data$chl)
    #P2=bdiag.spam(0,t(D)%*%D,t(temporal)%*%temporal, t(seasonal)%*%seasonal)
    #cholFactor=chol.spam(XTX+P2+Q,pivot="MMD",eps = 10^-6)
    #n=nrow(data)
    #Xw=1 ## Not used
    #Xy=t(X)%*%response
    #n.sm=3
    #identifyBit=Q
    #crit="GCV"
    
    #start.vals <- rep(0, 3) 
    #optimal  <- optim(par = start.vals,fn = objective, method = "Nelder-Mead", control=list(reltol = 10^-8, maxit = 500))
    
    lambda1=as.numeric(lambda[1])
    lambda2=as.numeric(lambda[2])
    lambda3=as.numeric(lambda[3])
    P3=bdiag.spam(0,lambda1*t(D)%*%D,lambda2*t(temporal)%*%temporal, lambda3*t(seasonal)%*%seasonal)
    
    #lambdaPar=rbind(lambdaPar,optimal$par)
    betaHat=solve(t(Bnew)%*%Bnew+P3+Q)%*%t(Bnew)%*%y 
    yHatOld=Bnew%*%betaHat  
    predValOld=cbind(predValOld,yHatOld)
    
    ### predict for new values
    
    data=fd[[k]]
    #y=allData$chl
    y=log(data$chl)
    seg=data$Station
    
    B=matrix(0,nrow=length(y),ncol=p)
    
    ## do without a loop later, just get a quick sense
    for(i in 1:nrow(B)){
      index=which(lookUp$station==seg[i])
      B[i,index]=1
      #print(i)
    }
    
    B=as.spam(B)
    
    knots=seq(1,365,length.out=35) ##73
    seasonal=cSplineDes(data$doy, knots, ord = 4)
    seasonal=as.spam(seasonal)
    
    
    knots=seq(min(allData$date_dec), max(allData$date_dec),length.out=35)
    
    temporal=splineDesign(knots, data$date_dec, ord = 4, outer.ok = T,
                          sparse = FALSE)
    temporal=as.spam(temporal)
    Bnew=cbind(rep(1,nrow(B)),B,temporal,seasonal)
    
    yHatNew=Bnew%*%betaHat  
    predValNew=cbind(predValNew,yHatNew)
    
  }
  return(list(predValOld=predValOld,predValNew=predValNew))
}

lambdaParam1=lambdaParam2=lambdaParam3=c(1e-8,1e-6,1e-4,1e-2,1e-1)
paramGridL=expand.grid(lambdaParam1,lambdaParam2,lambdaParam3)
dim(paramGridL) ## 125

require(parallel)
ptm <- proc.time()
lambdaOptim=mclapply(split(paramGridL, 1:nrow(paramGridL)),paramTestL,mc.cores=4)
proc.time() - ptm ## 2 minutes

setwd("~/Desktop/sfei")
save(lambdaOptim,file="lambdaOptim.Rda")

lambdaPar=lapply(lambdaOptim,function(x){x$lambdaPar})
predValOld=lapply(lambdaOptim,function(x){x$predValOld})
predValNew=lapply(lambdaOptim,function(x){x$predValNew})

medRMSE=unlist(lapply(predValNew,processPredVal))
summary(medRMSE) 
##   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
## 6.701   6.701   6.704   6.710   6.717   6.745 

## ok really hurt by less smoothing

varRMSE=unlist(lapply(predValNew,processPredValVar))
summary(varRMSE)
## Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
## 0.3415  0.3415  0.3422  0.3435  0.3455  0.3501 

## same variability
