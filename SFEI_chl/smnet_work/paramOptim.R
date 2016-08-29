## ridge regression parameters

ridgeParam1=ridgeParam2=ridgeParam3=c(0.25,0.5,0.75,1,5,10,20)
lambdaParam1=lambdaParam2=lambdaParam3=seq(0, 1.5, by = 0.2)

paramGrid=expand.grid(ridgeParam1,ridgeParam2,ridgeParam3,lambdaParam1,lambdaParam2,lambdaParam3)
dim(paramGrid)
##  175616 

## function to be applied over paramGrid
## record desired metrics
## probably just easier to return fitted values, then can pick metrics later

paramTest=function(){
  
}

## this is going to take forever with the k fold
## spend some time seeing if we can take advantage of the optimization done in smnet

