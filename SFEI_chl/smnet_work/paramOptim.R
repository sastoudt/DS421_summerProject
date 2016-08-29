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

folds=vector("list",5)
folds[[1]]=cbind.data.frame(fold2,fold3,fold4,fold5)
folds[[2]]=cbind.data.frame(fold1,fold3,fold4,fold5)
folds[[3]]=cbind.data.frame(fold2,fold1,fold4,fold5)
folds[[4]]=cbind.data.frame(fold2,fold3,fold1,fold5)
folds[[5]]=cbind.data.frame(fold2,fold3,fold4,fold1)
paramTest=function(nu){
  lambdaPar=c()
  predVal=c()
  for(k in 1:5){
    data=folds[[k]]
    #y=allData$chl
    y=log(data$chl)
    seg=data$Station
    
    B=matrix(0,nrow=length(y),ncol=p)
    
    lookUp=as.data.frame(cbind(unique(data$Station),1:length(unique(data$Station))))
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

    
    knots=seq(min(data$date_dec), max(data$date_dec),length.out=35)
    
    temporal=splineDesign(knots, data$date_dec, ord = 4, outer.ok = T,
                          sparse = FALSE)
    temporal=as.spam(temporal)
  
    #D=spam2full(D)
   
    
    P=bdiag.spam(0,lambdaD*t(D)%*%D,lambdaT*t(temporal)%*%temporal, lambdaS*t(seasonal)%*%seasonal)

    
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
yHat=Bnew%*%betaHat  
predVal=cbind(predVal,yHat)
  }
  return(list(lambdaPar=lambdaPar,predVal=predVal))
}

system.time(paramTest(paramGrid[1,])) ## <1 sec
test=paramTest(paramGrid[1,])

#require(parallel)
ptm <- proc.time()
nuOptim=apply(paramGrid,1,paramTest)
proc.time() - ptm ## 2.5 minu

length(nuOptim)
names(nuOptim[[1]])

### process info in nuOptim

## minimum median rmse across folds

lambdaPar=lapply(nuOptim,function(x){x$lambdaPar})
predVal=lapply(nuOptim,function(x){x$predVal})

medRMSE=unlist(lapply(predVal,processPredVal))
summary(medRMSE) ## this seems weird, were doing much better before

processPredVal=function(predVal){
  predVal=as.data.frame(predVal)
  rmse<-c()
  for(k in 1:5){
   rmse<-c(rmse, sqrt(sum((folds[[k]]$chl-predVal[,k])^2)/nrow(folds[[k]])))
  }
  return(median(rmse))
}

paramGrid[which.min(medRMSE),]
# 0.25 0.25 0.25

## this seems suspicious, better when everything was =1