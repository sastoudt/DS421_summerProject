setwd("~/Desktop/SF_chl")
load(file = "perStation.Rda")
load(file = "perStationParsimoniousModels.Rda")
load(file = "perStationFullModels.Rda")
load(file="perStationInteractionModels.Rda")
#load(file="~/Desktop/DS421_summerProject/GAM_weightedReg_comparison/shiny/data/delt_map.RData")
load(file="mod1Spatial.RData")
load(file="mod2Spatial.RData")
load(file="mod3Spatial.RData")
load(file="mod4Spatial.RData")
load(file="perStationAdd.Rda")
load(file="perStationFlowMod.Rda")
load(file="perStationFlowTOT.Rda")
load(file="perStationPredVal.Rda")
full=read.csv("sfeiPlusDates.csv")
allData=read.csv("allData.csv")
volFlow=read.csv("VolFingerPrintsMaster.csv")

cleanModel1 = function(cm) {
  # just in case we forgot to set
  # y=FALSE and model=FALSE
  cm$y = c()
  cm$model = c()
  
  cm$residuals = c()
  cm$fitted.values = c()
  cm$effects = c()
  cm$qr = c()  
  cm$linear.predictors = c()
  cm$weights = c()
  cm$prior.weights = c()
  cm$data = c()
  cm
}

wholeSeries<-c(1, 2, 5, 7, 11, 13, 15, 16, 17, 18, 21, 22, 23, 29, 40)

for(i in setdiff(1:41,wholeSeries)){
  perStation[[i]]=NULL
}
save(perStation,file="perStation.Rda")

for(i in setdiff(1:41,wholeSeries)){
  perStationAdd[[i]]=NULL
}
save(perStationAdd,file="perStationAdd.Rda")

for(i in setdiff(1:41,wholeSeries)){
  perStationPredVal[[i]]=NULL
}
save(perStationPredVal,file="perStationPredVal.Rda")

for(i in wholeSeries){
  
  perStationParsMod[[i]]=cleanModel1(perStationParsMod[[i]])
  print(i)
}
for(i in setdiff(1:41,wholeSeries)){
  perStationParsMod[[i]]=NULL
}
save(perStationParsMod,file="perStationParsimoniousModels.Rda")

## buggy, skip
for(i in wholeSeries){
  
  perStationFullMod[[i]]=cleanModel1(perStationFullMod[[i]])
  print(i)
}
for(i in setdiff(1:41,wholeSeries)){
  perStationFullMod[[i]]=NULL
}
save(perStationFullMod,file="perStationFullModels.Rda")

##
for(i in wholeSeries){
  
  perStationIntMod[[i]]=cleanModel1(perStationIntMod[[i]])
  print(i)
}
for(i in setdiff(1:41,wholeSeries)){
  perStationIntMod[[i]]=NULL
}
save(perStationIntMod,file="perStationInteractionModels.Rda")

###
for(i in wholeSeries){
  
  perStationFlowMod[[i]]=cleanModel1(perStationFlowMod[[i]])
  print(i)
}
for(i in setdiff(1:41,wholeSeries)){
  perStationFlowMod[[i]]=NULL
}
save(perStationFlowMod,file="perStationFlowModels.Rda")

###
for(i in wholeSeries){
  
  perStationFlowTOT[[i]]=cleanModel1(perStationFlowTOT[[i]])
  print(i)
}
for(i in setdiff(1:41,wholeSeries)){
  perStationFlowTOT[[i]]=NULL
}
save(perStationFlowTOT,file="perStationTOTModels.Rda")

###
mod1=cleanModel1(mod1)
mod2=cleanModel1(mod2)
mod3=cleanModel1(mod3)
mod4=cleanModel1(mod4)

save(mod1,file="mod1Spatial.RData")
save(mod2,file="mod2Spatial.RData")
save(mod3,file="mod3Spatial.RData")
save(mod4,file="mod4Spatial.RData")
