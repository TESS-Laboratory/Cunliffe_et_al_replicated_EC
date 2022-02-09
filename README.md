# Cunliffe_et_al_replicated_EC

This repository contains R code for processing, analysing and visualizing the 
data presented in 'Strong correspondence in evapotranspiration and carbon 
dioxide fluxes between different eddy covariance systems enables quantification 
of landscape heterogeneity in dryland fluxes' by Andrew Cunliffe, Fabio Boschetti,
Robert Clement, Stephen Sitch, Karen Anderson, Tomer Duman, Songyan Zhu, Mikeal 
Schlumpf, Marcy Litvak, Richard Brazier and Timothy Hill.


## The manuscript is in review. DOI TBC

*The processed half hourly EC flux data is archived with the UK Environmental Information Data Center (DOI: 10.5285/e96466c3-5b67-41b0-9252-8f8f393807d7) and AmeriFlux (https://ameriflux.lbl.gov/).*


## This repo contains the following scripts
OUTDATED *analysis_and_visualization.r*
Main script with analysis and visualization of half-hourly fluxes from the 
conventional and low-cost systems. NB. Ideally this large script would be 
separated out into its constituent parts (esp. the time series gap filling in 
REdddyProc and the footprint analysis).

*Additional_Analysis.r*
This script contains code for producing the revised analysis of fluxes following further corrections.

*soil_energy.r*
Script to tidy soil met data and extract soil energy data.

*climate_analysis.R script*
Main script for analysing the climate data (missing PET calculations).

ALSO EXPLAIN FOOTPRINT ANALYSIS SCRIPT


# explain data
data/land_cover_classifications contains the 2 m spatial resolution land cover classification, where values of 0 = "barren"", 1 = "shrubland"" and 2 "herbaceous".



## A static version of this repo is archived on Zenodo
https://doi.org/10.5281/zenodo.4730586

