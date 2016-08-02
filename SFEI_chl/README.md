## Investigation of Chlorophyll for SFEI

## UC Berkeley DS421 Summer Project

## Sara Stoudt 


## Week 1 Goals

- pick promising covariates for "parsimonious" model
- add variables of interest in a "full" model

"parsimonious model" - variables that look promising

- doy: day of year
- date_dec: date as a decimal value
- pheo: pheophytin a
- tn (montly structure): total nitrogen
- do_per (check context of the "per"): dissolved oxygen

"full model"- add variables of interest in context

- sio2: silica
- tp: total phosphorus
- tss: total suspended solids
- nh4 (when data available): ammonia 

- make a GAM model for each station separately
- display results in a Shiny app like in the WRTDS/GAM comparisons


## Week 2 Goals

- incorporate space into the GAM

I anticipate computational problems when adding all stations into one model.

- chunk into regions
- chunk based on correlation of chl between stations?
- investigate lags in this correlation?

## Week 3 Goals

- think about ways to incorporate flow/flow structure
- volumetric data

## Week 4 Goals

- write up
- prepare presentation materials


## Throughout

- continue progress on the WRTDS/GAM comparisons