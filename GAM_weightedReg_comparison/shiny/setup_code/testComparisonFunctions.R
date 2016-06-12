### testing summary functions
setwd("~/Desktop/DS421_summerProject/GAM_weightedReg_comparison/shiny")
source("getSummaryRMSE_Dev.R")
source("getFlowNormalizedSummary.R")
source("getSummaryDifference.R")
source("getRegressionResults.R")


load("data/mods_nolag.RData")

tmp <- mods_nolag$data[[1]] %>% 
  mutate(
    dec_time = dec_time(Date)[['dec_time']],
    doy = yday(Date)
  ) %>% 
  rename(
    res = resval, 
    flo = flolag,
    date = Date
  )

load("data/modelsNoLag_Nested.RData")

getSummaryRMSE(tmp,modelsNoLag_Nested[[1]])

getSummaryDeviance(tmp,modelsNoLag_Nested[[1]])

### for testing comparison functions, making fake WRTDS output in the format I am expecting
### can change later if this isn't what WRTDS outputs
modelWRTDS=as.data.frame(cbind(rnorm(length(modelsNoLag_Nested[[1]]$fitted.values),0,1),rnorm(length(modelsNoLag_Nested[[1]]$fitted.values),0,1)))
names(modelWRTDS)=c("blah","fits")
getSummaryDifference(tmp,modelsNoLag_Nested[[1]],modelWRTDS)


getRegressionResults(tmp,modelsNoLag_Nested[[1]],modelWRTDS,0.95)

modelWRTDS=as.data.frame(cbind(rnorm(length(modelsNoLag_Nested[[1]]$fitted.values),0,1),rnorm(length(modelsNoLag_Nested[[1]]$fitted.values),0,1)))
names(modelWRTDS)=c("blah","norms")
getRegressionResults(tmp,modelsNoLag_Nested[[1]],modelWRTDS,0.95)
