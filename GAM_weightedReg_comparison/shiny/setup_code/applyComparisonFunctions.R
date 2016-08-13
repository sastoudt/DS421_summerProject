### apply comparisons per station
require(xtable)
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

print(xtable(rmsePerStation[[i]]))

devPerStation=mapply(getSummaryDeviance,dataNiceNoLag,modelsNoLag_Nested,SIMPLIFY=F)
rn=c("dev","devA1","devA2","devA3","devA4","devA5","devS1",
     "devS2","devS3","devS4","devF1","devF2","devF3","devF4")

devPerStation<- lapply(devPerStation, function(x){ row.names(x)<-rn; x})
devPerStation<- lapply(devPerStation, function(x){ colnames(x)<-nam; x})
devPerStation[[1]]

print(xtable(devPerStation[[i]]))

rmseDevPerStation<-vector("list",27)
for(i in 1:27){
  rmseDevPerStation[[i]]=as.data.frame(cbind(rmsePerStation[[i]],devPerStation[[i]]))
}

rn=c("overall","A1","A2","A3","A4","A5","S1",
     "S2","S3","S4","F1","F2","F3","F4")
rmseDevPerStation<- lapply(rmseDevPerStation, function(x){ row.names(x)<-rn; x})
nam<-c("GAM_rmse","WRTDS_rmse","GAM_dev","WRTDS_dev")
rmseDevPerStation<- lapply(rmseDevPerStation, function(x){ colnames(x)<-nam; x})

rmseDevPerStation[[1]]
toUse=gsub("_"," ",names(dataNiceNoLag))
setwd("~/Desktop/DS421_summerProject/GAM_weightedReg_comparison/compareModels")
#for(i in c(1:9,19:27))
for(i in 1:27)
  print(xtable(rmseDevPerStation[[i]],caption=toUse[i]),float=T,type="latex",floating.environment="table",table.placement="H",file="likeTable2.tex",append=T)



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


avgPCPerStation<-lapply(avgPCPerStation,function(x){cbind(x$avg,x$pc)})
rn1=c("overall","annual1","annual2","annual3","annual4","annual5","seasonal1",
      "seasonal2","seasonal3","seasonal4","flow1","flow2","flow3","flow4")
avgPCPerStation<- lapply(avgPCPerStation, function(x){ row.names(x)<-rn1; x})
nam<-c("GAM_avg","WRTDS_avg","GAM_pc","WRTDS_pc")
avgPCPerStation<- lapply(avgPCPerStation, function(x){ colnames(x)<-nam; x})
avgPCPerStation[[1]]

toUse=gsub("_"," ",names(dataNiceNoLag))
setwd("~/Desktop/DS421_summerProject/GAM_weightedReg_comparison/compareModels")
#for(i in c(1:9,19:27))
for(i in 1:27)
print(xtable(avgPCPerStation[[i]],caption=toUse[i]),float=T,type="latex",floating.environment="table",table.placement="H",file="likeTable3_4.tex",append=T)


regResultsPerStation=mapply(getRegressionResults,dataNiceNoLag,1:length(dataNiceNoLag),SIMPLIFY=F)
regResultsPerStation[[1]]

i=20
getRegressionResults(dataNiceNoLag[[i]],i)


nam=c("intercept","interceptSignificant","slope","slopeSignificant")
## Note slope significant from zero not one
fullRegResPerStation=vector("list",27)
#for(i in c(1:9,19:27)){
for(i in 1:27){
  fullRegResPerStation[[i]]=as.data.frame(cbind(regResultsPerStation[[i]]$betas[,1],regResultsPerStation[[i]]$isSignificant[,1],
        regResultsPerStation[[i]]$betas[,2],regResultsPerStation[[i]]$isSignificant[,2]))
  names(fullRegResPerStation[[i]])=nam
}
## apply wasn't working
toUse=gsub("_"," ",names(dataNiceNoLag))
setwd("~/Desktop/DS421_summerProject/GAM_weightedReg_comparison/compareModels")
#for(i in c(1:9,19:27))
for(i in 1:27)
  print(xtable(fullRegResPerStation[[i]],caption=toUse[i]),float=T,type="latex",floating.environment="table",table.placement="H",file="likeTable6.tex",append=T)

##BE CAREFUL, WHEN RERUN DELETE FIRST SINCE APPEND = TRUE


missing<-c()
for(i in 1:27){
  missing<-c(missing,sum(is.na(dataNiceNoLag[[i]]$wrtdsPred))/nrow(dataNiceNoLag[[i]]))
}
missing

diffResultsPerStation=mapply(getSummaryDifference,dataNiceNoLag,1:length(dataNiceNoLag),SIMPLIFY=F)
diffResultsPerStation[[10]]

diffResultsPerStation=lapply(diffResultsPerStation,function(x){cbind(x$avgDiff,x$rmse)})
diffResultsPerStation[[1]]
nam=c("Avg Diff", "RMSE")
diffResultsPerStation=lapply(diffResultsPerStation,function(x){colnames(x)=nam;x})
rn1=c("overall","annual1","annual2","annual3","annual4","annual5","seasonal1",
      "seasonal2","seasonal3","seasonal4","flow1","flow2","flow3","flow4")
diffResultsPerStation=lapply(diffResultsPerStation,function(x){row.names(x)=rn1;x})
diffResultsPerStation[[1]]

toUse=gsub("_"," ",names(dataNiceNoLag))
setwd("~/Desktop/DS421_summerProject/GAM_weightedReg_comparison/compareModels")
#for(i in c(1:9,19:27))
for(i in 1:27)
  print(xtable(diffResultsPerStation[[i]],caption=toUse[i]),float=T,type="latex",floating.environment="table",table.placement="H",file="likeTable5.tex",append=T)

