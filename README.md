# Boschetti_et_al_replicated_EC
Code for processing and analysing eddy covariance data in Boschetti et al. 'Strong correspondence in evapotranspiration and CO2 fluxes between different EC systems enables quantification of landscape heterogeneity in drylands fluxes.'

) NEE_sev_2008_2019.r
   analysis to obtain long term meteo values for US-Seg and US-Ses from the ONEFlux dataset
   
2) REC_gapfill_code.r
   Code from REdddyProc for gapfilling datasets; an almost identical code is in the main script (plot_datasets.r)
   
3) backup_to_P.r
   code to backup into the P drive raw data from flash drives and telemetry, codes and plots

4) input_file_maker.r
   Prepares input files for EdiRe
   
5) plot_cospectra.r
   analysis and plotting of spec.csv files output from EdiRe for cospectral analysis of conventional and LEC towers
   
6) plot_datasets.r
   main script with analysis and plotting of conventional and LEC fluxes. Basically all plots of the EC DRIVING-C paper is done in
   this script
   
7) plot_lags.r
   analysis and plotting of lags.csv files from EdiRe to determine the correct time lags to be fed into the proc files of EdiRe for LEC
   preprocessing
   
   
