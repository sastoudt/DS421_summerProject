#DS421 Summer Project Proposal

## Sara Stoudt 

## Mentors: Perry de Valpine (ESPM), David Senn and Erica Spotswood (San Francisco Estuary Institute, SFEI)

## Project Description:

The San Francisco Estuary Institute is interested in learning how nutrient concentrations vary across space and time the Bay Delta, what the major drivers of this variability are, and if any mechanistic insights can be gained by exploring the relationships among various variables collected in the Bay Delta. We propose building up a Generalized Additive Model (GAM) from simple to more complex and see what benefits the additional complexity brings. 
From a statistical point of view I am interested in thinking about how we can incorporate flow information into new distance metrics in situations where Euclidean distance does not quite fit in context and how to quantify the uncertainty inherent in this information that is rarely precisely measured at the locations of interest. I have been delving into this literature and aim to extend the current approaches in future research.
In the context of the Bay Delta, the flow direction is less direct as it would be a on a narrower stream network, but flow can still confound trends over space and time if it is not properly accounted for in a model. This project will allow me to build on my previous work with GAMs in my undergraduate thesis as well as provide me with a jumping off point in thinking about flow models in context. Some current work at SFEI is investigating weighted regression for producing flow normalized models. Time permitting I will compare our GAM approach to the weighted regression approach.

## Product Outcomes: 

A series of models in increasing complexity of chlorophyll as a function of various nutrients, available light, temperature, time, space, and measures of flow and water source.

- “Vanilla” Generalized Additive Models (GAMs)
- GAM + adding flow as a predictor in the traditional way
- GAM + adding a flow structure (informed by the literature that incorporates flow structure into other spatial statistics methods)
- Incorporate hydrological flow simulation data (most likely saved for future work)

## Primary Products:
- Github repository with reproducible code and documentation.
- Written report of findings with appropriate visuals.

## Proposed Timeline:
- Before I leave for NIST: SFEI will pass along raw data, code for some preliminary preprocessing, and flow data (including volumetric print data)	
- May-July: I will be at NIST but will familiarize myself with the data in my downtime. This will include getting familiar with the context and format of the data and exploratory data analysis and visualization.
- August: I will return to Berkeley and work full time on this.
