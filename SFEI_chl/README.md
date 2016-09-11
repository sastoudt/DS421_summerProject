## Investigation of Chlorophyll for SFEI

## UC Berkeley DS421 Summer Project

## Sara Stoudt 

Shiny app deployed here: https://sastoudt.shinyapps.io/SF_chl/ 

Fixed as of 9/11. Give it a little bit of time to load and any error messages that pop up initially will go away. 

~~8/24: I still can't get this to run on shiny io. The app works fine locally, but crashes with no error messages when deployed. I even moved all of the large files to Dropbox to remove the size issues, but to no avail. As a last resort, you can now run the app on your own computer, using the following two lines of code.~~

library(shiny)

runGitHub("DS421_summerProject", "sastoudt", subdir = "SFEI_chl/shiny/",launch.browser=T)

This will launch the app in your browser. It will take time to load since it is pulling a large file from Dropbox and then processing it. ~~There are still some glitches in this version that don't occur in the previous version that runs locally. I will focus on working on those next, but everything that does pop up on the Shiny app should be representative.~~

~~8/24 evening: I fixed the glitches, but now pulling from Dropbox is crashing my R, and I can't use the runGitHub() option. Perhaps it is my internet connection at home? I will try again tomorrow from campus.~~

~~8/25: This works from Berkeley internet, so now you should be able to use the runGitHub() command without any glitches. Be patient, things take awhile to load and change between options, sorry!~~

Note: I can't simply put all of the data in the GitHub repo, because the files are larger than the allowed upload size.

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

- chunk into regions (no longer necessary when we are using bam)
- chunk based on correlation of chl between stations (don't need to chunk, just investigate and see whether we are leveraging extra information from other stations in a full spatial model or whether building a seperate model per station is really best)
- investigate lags in this correlation (perhaps save trying to incorporate the results of this exploration for Week 3, has to do with flow structure)
- try to incorporate pheo and/or tn if possible (compare same for each station and by station)

Computational Update
- I anticipated correctly, but there are a few options that look promising for not having to break things up.
- bam (multiple thread gam, huge improvement in speed with 4 threads and I have a max of 8 on my computer)
- put everything up on the cluster (figured out all of the steps to do this [moving files back and forth, submitting jobs])
- I also just switched my linear algebra package to be vecLib version of BLAS which can give up to a linear time improvement.

Week 2 Update- completed all of the main goals for this week including:

- investigated different spatial models including the inclusion of some promising covariates from the parsimonious model
- made progress assessing which is better, full spatial model or fitting a seperate model per station
- set up correlation investigation
- found computational tricks to speeding things up

To Do

- host Shiny app online (need to slim it down a bit)
- waiting for expanded model 3 to run on cluster to get fairer comparison of full model v. one model per station DONE


## Week 3 Goals

- think about ways to incorporate flow/flow structure
- volumetric data
- think about correlation network and optimization of lags of other stations per station

Some notes so far:
1. Add total flow data from http://www.water.ca.gov/dayflow/, need to think more about a structure rather than merely a covariate
2. Visualize volumetric data over months and years, use to pick a more specific flow for #1 (doesn't really help)
3. Added chl from most correlated station, best lag is zero lag for all.

## Week 4 Goals

- write up overall results DONE (summerSummary)
- host slimmed down Shiny app
- one page report describing what I did, what I learned, how I see the project contributing to your longer term career development DONE
- prepare presentation materials (3-5 minute lightning talk) [no slides]
- more TO DOs marked in red in summerSummary

## Throughout

- continue progress on the WRTDS/GAM comparisons

## Into the Semester- Working with smnet

- use smnet framework but build up from source code to bypass GIS preprocessing
- build spatial network manually
- different spatial penalty matrices, trying to understand spatial weights
- optimization of smoothing paramters and ridge parameters
- penalty approach v. covariance approach (correlation: make empirical covariance matrix, is it PSD? can we massage it to be?)
- how to improve smnet approach to be more effective for this particular problem, have already gotten a sense of the ``benefit" of the spatial penalty approach

