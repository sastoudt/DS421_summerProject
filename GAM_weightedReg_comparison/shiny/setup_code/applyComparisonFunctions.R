### apply comparisons per station

setwd("~/Desktop/DS421_summerProject/GAM_weightedReg_comparison/shiny")
source("getSummaryRMSE_Dev.R")
source("getFlowNormalizedSummary.R")
source("getSummaryDifference.R")
source("getRegressionResults.R")

load("data/dataNice_nolag.RData")
load("data/modelsNoLag_Nested.RData")

rn=c("rmse","rmseA1","rmseA2","rmseA3","rmseA4","rmseA5","rmseS1",
     "rmseS2","rmseS3","rmseS4","rmseF1","rmseF2","rmseF3","rmseF4")

nam=c("GAM","WRTDS")
rmsePerStation=mapply(getSummaryRMSE,dataNiceNoLag,modelsNoLag_Nested,SIMPLIFY=F)
rmsePerStation<- lapply(rmsePerStation, function(x){ row.names(x)<-rn; x})
rmsePerStation<- lapply(rmsePerStation, function(x){ colnames(x)<-nam; x})

rmsePerStation[[1]]

devPerStation=mapply(getSummaryDeviance,dataNiceNoLag,modelsNoLag_Nested,SIMPLIFY=F)

devPerStation<- lapply(devPerStation, function(x){ row.names(x)<-rn; x})
devPerStation<- lapply(devPerStation, function(x){ colnames(x)<-nam; x})
devPerStation[[1]]

### not officially tested yet without sample modelWRTDS
### but conceptually should work the same

#methodDifferencePerStation=mapply(getSummaryDifference,dataNiceNoLag,modelsNoLag_Nested,
#                                  modelsNoLag_WRTDS,SIMPLIFY=F)

#regressionPerStation=mapply(getRegressionResults,dataNiceNoLag,modelsNoLag_Nested,
# rep(0.95,length(dataNiceNoLag)),SIMPLIFY=F)



