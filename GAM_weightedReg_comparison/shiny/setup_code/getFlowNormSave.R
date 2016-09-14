#flowNormalized(data,mod)

setwd("~/Desktop/DS421_summerProject/GAM_weightedReg_comparison/shiny/data")

load("dataNice_nolag.Rdata")
load("modelsNoLag_Nested.RData")
require(lubridate)
require(WRTDStidal)
require(dplyr)
source("~/Desktop/DS421_summerProject/GAM_weightedReg_comparison/shiny/setup_code/flowNormalized_tryAgain.R")

flowNormToSave=vector("list",length(dataNiceNoLag))
flowNormToSave2=vector("list",length(dataNiceNoLag))
for(i in 1:length(dataNiceNoLag)){
  
  data=dataNiceNoLag[[i]]
  mod=modelsNoLag_Nested[[i]]
  flowNorm=flowNormalized(data,mod)
  
  test1=cbind.data.frame(data$date,data$res) 
  test2=flowNorm
  names(test1)=c("dateD","resD")
  names(test2)=c("dateA","resA")
  
  flowNormToSave[[i]]=test1
  flowNormToSave2[[i]]=test2
  print(i)
}

save(flowNormToSave,file="flowNormToLoad.RData")
save(flowNormToSave2,file="flowNormToLoad2.RData")
