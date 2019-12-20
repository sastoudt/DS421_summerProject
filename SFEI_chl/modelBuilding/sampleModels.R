setwd("~/Desktop/sfei")
load(file = "perStation.Rda") ## data per station in the bay delta
library(mgcv)

### response: chl (chlorophyll)

### "parsimonious model" - variables that look promising from EDA plots and correlations

## what is considered "promising"?

# first check missingness
# look at lots of pairs plots
# look at lots of pairwise correlations

## doy  (day of year)
## date_dec (date as a quantitative value)
## pheo (pheophytin a)
## tn (monthly structure): total nitrogen
## do_per: dissolved oxygen

### "full model" - other variables that are of interest in context

## sio2: silica
## tp: total phosphorus
## tss: total suspended solids
## nh4 (when data available): ammonia
## sal: salinity

data <- perStation[[1]] ## one station's data
sum(is.na(data$nh4)) / nrow(data) ## is there enough non-NA ammonium data to use in model?

gamParsimonious <- gam(chl ~ ti(doy, bs = "cc") + ti(date_dec, bs = "tp") + ti(pheo, bs = "tp") + ti(tn, bs = "tp") + ti(do_per, bs = "tp"), data = data, family = gaussian(link = "log"))
gam.check(gamParsimonious)

### spline choices

### bs (basis function)

## "cc": cyclic cubic regression, used for seasonality
## "tp": thin plate regression splines (generally preferred to cubic splines if you have ample data, but not an immense amount due to computational considerations), penalized slightly so a term can be shrunk to zero, especially important for full model
## "cs": cubic spline with shrinkage, useful if you want to be conservative and hedge against too much flexibility (e.g. small amounts of data, don't want to extrapolate beyond your range or be unduly influenced by outliers')

#### k (dimension of basis)

## determined by looking at diagnostic plots produced by gam.check
## increasing k increases model complexity
## need to make sure k is big enough to span the space but not too big that you introduce unnecessary complexity

### For more in depth descriptions of the model building process, read Section 1 here: https://github.com/sastoudt/DS421_summerProject/blob/master/summerSummary.pdf

gamFull <- gam(chl ~ ti(doy, bs = "cc") + ti(date_dec, bs = "tp") + ti(pheo, bs = "tp") + ti(tn, bs = "tp") + ti(do_per, bs = "tp") +
  ti(sio2, bs = "tp") + ti(tp, bs = "tp") + ti(tss, bs = "tp") + ti(nh4, bs = "tp"), data = data, family = gaussian(link = "log"))
gam.check(gamFull)


### model evaluation

## In the context of the data problem we want to assess goodness of fit at different periods of time and in different parts of the season.

## can evaluate the root mean squared error for different datasets (by station) and for different variables (other parts of the project model nutrients other than chlorophyll)


getSummaryRMSE <- function(data, namePred) {
  trueVal <- data$chl

  predVal <- data[, namePred]

  rmse <- sqrt(sum((trueVal - predVal)^2, na.rm = T) / sum(!is.na((trueVal - predVal)^2)))

  data$month <- as.numeric(strftime(data$Date, "%m"))
  data$year <- as.numeric(strftime(data$Date, "%Y"))

  ## not beautiful but gets the job done
  annual1 <- subset(data, year < 1982 & year >= 1975)
  annual1I <- which(data$year < 1982 & data$year >= 1975)
  annual1P <- predVal[annual1I]


  annual2 <- subset(data, year < 1989 & year >= 1982)
  annual2I <- which(data$year < 1989 & data$year >= 1982)
  annual2P <- predVal[annual2I]


  annual3 <- subset(data, year < 1996 & year >= 1989)
  annual3I <- which(data$year < 1996 & data$year >= 1989)
  annual3P <- predVal[annual3I]

  annual4 <- subset(data, year < 2003 & year >= 1996)
  annual4I <- which(data$year < 2003 & data$year >= 1996)
  annual4P <- predVal[annual4I]

  annual5 <- subset(data, year < 2010 & year >= 2003)
  annual5I <- which(data$year < 2010 & data$year >= 2003)
  annual5P <- predVal[annual5I]


  annual6 <- subset(data, year >= 2010) ## has 2 fewer years
  annual6I <- which(data$year >= 2010)
  annual6P <- predVal[annual6I]


  rmseA1 <- sqrt(sum((annual1$chl - annual1P)^2, na.rm = T) / sum(!is.na((annual1$chl - annual1P)^2)))
  rmseA2 <- sqrt(sum((annual2$chl - annual2P)^2, na.rm = T) / sum(!is.na((annual2$chl - annual2P)^2)))
  rmseA3 <- sqrt(sum((annual3$chl - annual3P)^2, na.rm = T) / sum(!is.na((annual3$chl - annual3P)^2)))
  rmseA4 <- sqrt(sum((annual4$chl - annual4P)^2, na.rm = T) / sum(!is.na((annual4$chl - annual4P)^2)))
  rmseA5 <- sqrt(sum((annual5$chl - annual5P)^2, na.rm = T) / sum(!is.na((annual5$chl - annual5P)^2)))
  rmseA6 <- sqrt(sum((annual6$chl - annual6P)^2, na.rm = T) / sum(!is.na((annual6$chl - annual6P)^2)))

  seasonal1 <- subset(data, month %in% c(1:3))
  seasonal1I <- which(data$month %in% c(1:3))
  seasonal1P <- predVal[seasonal1I]

  seasonal2 <- subset(data, month %in% c(4:6))
  seasonal2I <- which(data$month %in% c(4:6))
  seasonal2P <- predVal[seasonal2I]

  seasonal3 <- subset(data, month %in% c(7:9))
  seasonal3I <- which(data$month %in% c(7:9))
  seasonal3P <- predVal[seasonal3I]

  seasonal4 <- subset(data, month %in% c(10:12))
  seasonal4I <- which(data$month %in% c(10:12))
  seasonal4P <- predVal[seasonal4I]


  rmseS1 <- sqrt(sum((seasonal1$chl - seasonal1P)^2, na.rm = T) / sum(!is.na((seasonal1$chl - seasonal1P)^2)))
  rmseS2 <- sqrt(sum((seasonal2$chl - seasonal2P)^2, na.rm = T) / sum(!is.na((seasonal2$chl - seasonal2P)^2)))
  rmseS3 <- sqrt(sum((seasonal3$chl - seasonal3P)^2, na.rm = T) / sum(!is.na((seasonal3$chl - seasonal3P)^2)))
  rmseS4 <- sqrt(sum((seasonal4$chl - seasonal4P)^2, na.rm = T) / sum(!is.na((seasonal4$chl - seasonal4P)^2)))



  rmse <- rbind(rmse, rmseA1, rmseA2, rmseA3, rmseA4, rmseA5, rmseA6, rmseS1, rmseS2, rmseS3, rmseS4)

  return(rmse)
}
