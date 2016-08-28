## ridge regression parameters

ridgeParam1=ridgeParam2=ridgeParam3=c(0.25,0.5,0.75,1,5,10,20)
lambdaParam1=lambdaParam2=lambdaParam3=seq(0, 1.5, by = 0.2)

paramGrid=expand.grid(ridgeParam1,ridgeParam2,ridgeParam3,lambdaParam1,lambdaParam2,lambdaParam3)
dim(paramGrid)
##  175616 

## function to be applied over paramGrid
## record desired metrics



