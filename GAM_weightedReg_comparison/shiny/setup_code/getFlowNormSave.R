#flowNormalized(data,mod)

setwd("~/Desktop/DS421_summerProject/GAM_weightedReg_comparison/shiny/data")

load("dataNice_nolag.Rdata")
load("modelsNoLag_Nested.RData")

source("~/Desktop/DS421_summerProject/GAM_weightedReg_comparison/shiny/setup_code/flowNormalized_tryAgain.R")

flowNormToSave=vector("list",length(dataNiceNoLag))
for(i in 1:length(dataNiceNoLag)){
  
  data=dataNiceNoLag[[i]]
  mod=modelsNoLag_Nested[[i]]
  flowNorm=flowNormalized(data,mod)
  
  test=cbind.data.frame(data$date,data$res,flowNorm) ## not same dim
  names(test)=c("dateD","resD","dateA","resA")
  
  flowNormToSave[[i]]=test
  print(i)
}

save(flowNormToSave,file="flowNormToLoad.RData")