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
- sal: salinity

- make a GAM model for each station separately
- display results in a Shiny app like in the WRTDS/GAM comparisons

Stations [16/41] To Focus On (full/almost full record)

- C10
- C3
- D10
- D12
- D15
- D22
- D26
- D28A
- D4
- D41
- D6
- D7
- D8
- MD10
- P8
 
Week 1 Update- completed all of the main goals for this week including:

- parsimonious, full, and interaction models made for each station (after discarding some stations for lack of complete data)
- Shiny app displays all of this including nesting structure and "pick two" to better see what is going on
- observations noted about all of the models (including which variables explain the most variability)
- also worked a bit on the WRTDS/GAM comparisons, see that README

To Do

- host Shiny app online
- go through interaction model results and comment DONE

## Week 2 Goals

- incorporate space into the GAM

I anticipate computational problems when adding all stations into one model.

Computational Update
- I anticipated correctly, but there are a few options that look promising for not having to break things up.
- bam (multiple thread gam, huge improvement in speed with 4 threads and I have a max of 8 on my computer)
- put everything up on the cluster (figured out all of the steps to do this [moving files back and forth, submitting jobs])
- I also just switched my linear algebra package to be vecLib version of BLAS which can give up to a linear time improvement.

- chunk into regions (no longer necessary when we are using bam)
- chunk based on correlation of chl between stations (don't need to chunk, just investigate and see whether we are leveraging extra information from other stations in a full spatial model or whether building a seperate model per station is really best)
- investigate lags in this correlation

## Week 3 Goals

- think about ways to incorporate flow/flow structure
- volumetric data

## Week 4 Goals

- write up
- prepare presentation materials


## Throughout

- continue progress on the WRTDS/GAM comparisons