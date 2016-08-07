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

require(xtable)
names(dataNiceNoLag)
i=2
print(xtable(rmsePerStation[[i]]))

# devPerStation=mapply(getSummaryDeviance,dataNiceNoLag,modelsNoLag_Nested,SIMPLIFY=F)

devPerStation<- lapply(devPerStation, function(x){ row.names(x)<-rn; x})
devPerStation<- lapply(devPerStation, function(x){ colnames(x)<-nam; x})
devPerStation[[1]]

### not officially tested yet without sample modelWRTDS
### but conceptually should work the same

#methodDifferencePerStation=mapply(getSummaryDifference,dataNiceNoLag,modelsNoLag_Nested,
#                                  modelsNoLag_WRTDS,SIMPLIFY=F)

#regressionPerStation=mapply(getRegressionResults,dataNiceNoLag,modelsNoLag_Nested,
# rep(0.95,length(dataNiceNoLag)),SIMPLIFY=F)

avgPCPerStation=mapply(getFlowNormalizedSummary,dataNiceNoLag,1:length(dataNiceNoLag),SIMPLIFY=F)
avgPCPerStation[[1]]
rn1=c("avgOverall","annual1M","annual2M","annual3M","annual4M","annual5M","seasonal1M",
     "seasonal2M","seasonal3M","seasonal4M","flow1M","flow2M","flow3M","flow4M")
rn2=c("percentChange","annual1PC","annual2PC","annual3PC","annual4PC","annual5PC","seasonal1PC",
      "seasonal2PC","seasonal3PC","seasonal4PC","flow1PC","flow2PC","flow3PC","flow4PC")


nam=c("GAM","WRTDS")
avgPCPerStation<- lapply(avgPCPerStation, function(x){ row.names(x$avg)<-rn1; x})
avgPCPerStation<- lapply(avgPCPerStation, function(x){ row.names(x$pc) <- rn2; x})

# for(i in 1:length(avgPCPerStation)){
#   row.names(avgPCPerStation[[i]]$pc)<-rn2
# } ## i=19,20,21 missing annual4PCG

avgPCPerStation<- lapply(avgPCPerStation, function(x){ colnames(x$avg)<-nam; x})
avgPCPerStation<- lapply(avgPCPerStation, function(x){ colnames(x$pc)<-nam; x})

avgPCPerStation[[1]]

regResultsPerStation=mapply(getRegressionResults,dataNiceNoLag,1:length(dataNiceNoLag),SIMPLIFY=F)
regResultsPerStation[[1]]

i=19
getRegressionResults(dataNiceNoLag[[i]],i)

missing<-c()
for(i in 1:27){
  missing<-c(missing,sum(is.na(dataNiceNoLag[[i]]$wrtdsPred))/nrow(dataNiceNoLag[[i]]))
}
missing

diffResultsPerStation=mapply(getSummaryDifference,dataNiceNoLag,1:length(dataNiceNoLag),SIMPLIFY=F)
diffResultsPerStation[[10]]
