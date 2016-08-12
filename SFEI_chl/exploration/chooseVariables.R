### picking variables of interest to put in "full" model

setwd("~/Desktop/sfei")

sfei<-read.csv("sfeiPlusDates.csv", stringsAsFactors=F)
names(sfei)
pairs(sfei[,c(10,5:9)]) ## bod

pairs(sfei[,c(10,11:15)]) ## tkn

pairs(sfei[,c(10,16:20)]) ## ton

pairs(sfei[,c(10,21:25)]) ## do, pheo, tp

pairs(sfei[,c(10,26:30)])

pairs(sfei[,c(10,31:37)])

## check for seasonality and/or trends over time in nutrients themselves
plot(sfei$doy,sfei$po4)
plot(sfei$date_dec,sfei$po4)

plot(sfei$doy,sfei$nh4) ## winterish?
plot(sfei$date_dec,sfei$nh4)

plot(sfei$doy,sfei$bod)
plot(sfei$date_dec,sfei$bod) ## so no data

plot(sfei$doy,sfei$tkn)
plot(sfei$doy,sfei$tkn,ylim=c(0,3)) ## monthly?
plot(sfei$date_dec,sfei$tkn)


plot(sfei$doy,sfei$ton)
plot(sfei$date_dec,sfei$ton) ## so no data

plot(sfei$doy,sfei$do)
plot(sfei$date_dec,sfei$do)

plot(sfei$doy,sfei$pheo) ## spring/summerish?
plot(sfei$date_dec,sfei$pheo)

plot(sfei$doy,sfei$tp)
plot(sfei$doy,sfei$tp,ylim=c(0,1)) ## definitely a monthly thing going on here
plot(sfei$date_dec,sfei$tp)

plot(sfei$doy,sfei$din)
plot(sfei$doy,sfei$din,ylim=c(0,1)) ## something going on here, but we need data smoothing
plot(sfei$date_dec,sfei$din)

plot(sfei$doy,sfei$sio2)
plot(sfei$doy,sfei$sio2,ylim=c(0,40))
plot(sfei$date_dec,sfei$sio2)

plot(sfei$doy,sfei$tss)
plot(sfei$date_dec,sfei$tss)

plot(sfei$doy,sfei$temp) ## definitely
plot(sfei$date_dec,sfei$temp)

plot(sfei$doy,sfei$din) ## seems like a monthly patterm
plot(sfei$date_dec,sfei$din) ## see some structure

plot(sfei$doy,sfei$tn) ## same thing
plot(sfei$date_dec,sfei$tn) ## more pronouced than before

plot(sfei$doy,sfei$no3)
plot(sfei$date_dec,sfei$no3) ## No data

plot(sfei$doy,sfei$no23) ## same nitrogen pattern
plot(sfei$date_dec,sfei$no23) ## same nitrogen pattern

plot(sfei$doy,sfei$toc)
plot(sfei$date_dec,sfei$toc) ## no data

plot(sfei$doy,sfei$doc)
plot(sfei$date_dec,sfei$doc) ## same

plot(sfei$doy,sfei$pH,xlim=c(0,10)) 
plot(sfei$date_dec,sfei$pH,ylim=c(0,10)) ## missing data gap

plot(sfei$doy,sfei$chl) ## spring/summerish
plot(sfei$date_dec,sfei$chl) ## can see the peaks that we are getting in the data

plot(sfei$doy,sfei$secchi,ylim=c(0,200)) ## monthly structure
plot(sfei$date_dec,sfei$secchi,ylim=c(0,200)) ## structure, need to get rid of clear outliers

plot(sfei$doy,sfei$turb)
plot(sfei$doy,sfei$turb,ylim=c(0,150)) ## monthly structure
plot(sfei$date_dec,sfei$turb) ## definite structure

load("perStation.Rda")

## correlation, have to match day, same size vector
## TO DO: correlation of chl between stations

## Procedure for Selecting Variables for Model:

## Look at correlations/trends for all stations (above). Pick a few that look good and are interesting.
## Also add some other variables of interest in a "full model" for interpretation purposes.
## For each station, do a quick set of pairs plot to make sure the overall model makes sense and/or that
## we aren't missing any important variables for that station.
## Fit GAM, find appropriate k's.
## Keep increasing k until gets slow/get warnings. Then follow this advice:
## (i) fit your model and extract the deviance residuals;
##(ii) for each smooth term in your model, fit an equivalent, single, smooth to the residuals, using a
##substantially increased k to see if there is pattern in the residuals that could potentially be explained
##by increasing k
## Save in named list for future use

## Display nested impact + fitted values in Shiny app like the one created for work with Marcus.

plot(sfei$bod,sfei$chl)
plot(sfei$date_dec,sfei$bod) ## not consistent data

plot(sfei$tkn,sfei$chl) ## not great
plot(sfei$date_dec,sfei$tkn) ##

plot(sfei$ton,sfei$chl)
plot(sfei$date_dec,sfei$ton) ## not consistent data

plot(sfei$do,sfei$chl) ## not great
plot(sfei$date_dec,sfei$do)

plot(sfei$pheo,sfei$chl) ## good
plot(sfei$date_dec,sfei$pheo)

plot(sfei$tp,sfei$chl) ## not great
plot(sfei$date_dec,sfei$tp)

plot(sfei$doy,sfei$chl) ## spring, summer (same with pheo) (temp could also do this?)
plot(sfei$date_dec,sfei$chl) 

### "parsimonious model"

### look promising
## doy 
## date_dec
## pheo
## tn (monthly structure) 

### Add these for "full model"

### of interest
## sio2
## po4
## tss
## nh4
