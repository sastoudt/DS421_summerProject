X=Bnew
XTX=t(Bnew)%*%Bnew
P=list(lambdaD*t(D)%*%D,lambdaT*t(temporal)%*%temporal, lambdaS*t(seasonal)%*%seasonal)
response=log(allData$chl)
P2=bdiag.spam(0,lambdaD*t(D)%*%D,lambdaT*t(temporal)%*%temporal, lambdaS*t(seasonal)%*%seasonal)
cholFactor=chol.spam(XTX+P2+Q,pivot="MMD",eps = 10^-6)
n=nrow(allData)
Xw=1 ## Not used
Xy=t(X)%*%response
n.sm=3
identifyBit=Q
crit="GCV"

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

system.time(optimal  <- optim(par = start.vals,fn = objective, method = "Nelder-Mead", control=list(reltol = 10^-8, maxit = 500)))


start.vals <- rep(0, 3) ## length(P.flat)  

## 3 lambdas
## on log scale, take exp of params to get between 0 and 1



  