# Comparison of Weighted Regression and Generalized Additive Models

## UC Berkeley DS421 Summer Project



## Overview

This work is a portion of my summer project for the DS421 program and is joint work with Marcus Beck ([`fawda123`](https://github.com/fawda123/)).


## Directions
1. Clone the repo: `git clone https://github.com/sastoudt/DS421_summerProject'
2. Cd into ~/DS421_summerProject/GAM_weightedReg_comparison/.
3. Install necessary software. You will need:


-  R, with the following packages installed:
	1. WRTDStidal
	2. dplyr
	3. mgcv
	4. ggplot2
	5. tidyr
	6. lubridate
	7. xtable
	8. shiny


## Run Shiny App

shiny::runGitHub("DS421_summerProject", "sastoudt", subdir = "GAM_weightedReg_comparison/shiny/")

This is created by using https://beckmw.shinyapps.io/sf_trends/ as a framework.

Or view it online: https://sastoudt.shinyapp.io/GAM_Delta

## Progress as of 8/7/16

- working with the mean model response values of WRTDS from Marcus: https://github.com/fawda123/sf_trends/blob/master/data/mods_nolag_mean.RData
- adjusted all summary functions to work with WRTDS predictions
- writing summaries straight to TeX tables from R
- briefly comment on summary results in compareModels
- update Shiny to include a plot comparing the fitted values of WRTDS and GAMs
- update Shiny map plots to make loading faster, include maps for WRTDS
- updated hosted Shiny to reflect all of the changes

Next Steps

- figure out missingness in fitted values of WRTDS in several stations
- think about the results in context (in terms of the spatial location of each station and the different response variables)
- reread Beck and Murphy paper to get a feel for things I should be looking for

## Progress as of 6/14/16

- Fixed Flow Normalization: getFlowNormalized.R
- Annual Option on Shiny

Comparisons (by annual, seasonal, and flow levels):

- Table 2 Beck_and_Murphy_EMA Info: getSummaryRMSE_Dev.R
- Tables 3 and 4 in Beck_and_Murphy_EMA Info: getFlowNormalizedSummary.R
- Table 5 Beck_and_Murphy_EMA Info: getSummaryDifference.R
- Table 6 Beck_and_Murphy_EMA Info: getRegressionResults.R

- test these functions: setup_code/testComparisonFunctions.R
- apply over all station/response combinations: setup_code/applyComparisonFunctions.R
- display comparison results on a color coded map: spatialResultsPlot.R
- include spatial plots in Shiny (bottleneck for loading)
- TO DO: Option for doing these calculations not on the log scale.

GAM Models:

- Changed to a nested format so that we can see the benefit of each term: nestedPlot.R
- Display this nested format in Shiny
- Increased basis dimension when needed, a few are marginally sufficient but are pushing limits
- TO DO: Remove terms that are not significantly helpful, do not really need three way interaction term

Things to Check:

- check if output of WRTDS is in the expected format: for getSummaryDifference.R and getRegressionResults.R I assume that the model has gone through modfit and has an attribute $fits or $norms
and that the predictions match up: order by date before making predictions so the output is ordered by date
- ~~trimming of extreme values in dynagam: what does it mean to constrain plots to salinity limits for the selected month? effectively rounding up or down if our predictions don't match known limits, but how do we know the salinity limits?~~
- do we have access to chl data for this network?




 
