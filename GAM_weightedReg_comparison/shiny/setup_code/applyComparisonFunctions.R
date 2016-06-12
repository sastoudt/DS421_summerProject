### apply comparisons per station

setwd("~/Desktop/DS421_summerProject/GAM_weightedReg_comparison/shiny")
source("getSummaryRMSE_Dev.R")
source("getFlowNormalizedSummary.R")
source("getSummaryDifference.R")
source("getRegressionResults.R")

load("data/dataNice_nolag.RData")
load("data/modelsNoLag_Nested.RData")

rmsePerStation=mapply(getSummaryRMSE,dataNiceNoLag,modelsNoLag_Nested,SIMPLIFY=F)
names(rmsePerStation)
rmsePerStation[[1]]

devPerStation=mapply(getSummaryDeviance,dataNiceNoLag,modelsNoLag_Nested,SIMPLIFY=F)
devPerStation[[1]]

### not officially tested yet without sample modelWRTDS
### but conceptually should work the same

#methodDifferencePerStation=mapply(getSummaryDifference,dataNiceNoLag,modelsNoLag_Nested,
#                                  modelsNoLag_WRTDS,SIMPLIFY=F)

#regressionPerStation=mapply(getRegressionResults,dataNiceNoLag,modelsNoLag_Nested,
# rep(0.95,length(dataNiceNoLag)),SIMPLIFY=F)



