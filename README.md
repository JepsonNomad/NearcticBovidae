## README

This is a guide for using the scripts to reproduce results in John & Post 2022 Ecology and Evolution. Please cite the manuscript as needed.

The following workflow will reproduce the results and figures presented in our manuscript, as well as contribute to discussion that arose during the review process. You will need to full directory structure included here. To reproduce the analyses, follow the code in the `/scripts/` folder in order; fill in check boxes as you go.



## Part I: Data download

[x] Download worldclim v2 data using `01_getWC21.py`\
[x] Download and tidy North American Artiodactyla occurrences using `02_rgbif.R`\
[x] Spatially thin the occurrence records using `03_thinBovids.R`\



## Part II: Input data operations

### Part IIa: Predictor data setup

[x] Resample present bioclimatic raster data using `04a_bioclResample.py` and `04b_cmip6Resample.py`\
[x] Generate terrain covariates and resample using `04c_demOperations.py`\
[x] Prep and organize GCAM land cover data using `04d_gcamCdfToTif.R`, `04e_gcamResample.py`, and `04f_gcamCombine.R`\
[x] Prep ice sheet data using `04g_cavResample.R`\
[x] Visualize dynamic predictor data using `05_visualizeWorldclim.R`\
[x] Center present predictor variables; compile present worldclim + terrain into single rasters using `06_presentPredictorPrep.R`\
[x] Center future predictor variables in same fashion as above using `07_futurePredictorPrep.R`\


### Part IIb: Response data setup

[x] Generate a sampling bias grid (see Phillips et al 2009) using `08_biasMatrix.R`\
[x] Generate a set of background data for each species using `09_generateBG.R`\
[x] Visualize covariation among predictor variables `10_predPairsPlots.R`\



## Part III: Modeling

[x] Fit MaxEnt models using `11_maxEnt.R`\
[x] Compute MESS grids using `12_MESS.R`\
[x] Visualize individual MESS grids usign `13a_visualizeMESS.R`\
[x] Visualize combined mess grids by climate scenario using `13b_combineMESS.R`\
[x] Generate prediction rasters from maxent models using `14_predictionRasters.R`\
[x] Visualize prediction rasters using `15_predictionPlots.R`\
[x] Generate comparisons of range shift estimates among species using `16*.R`\
[x] Compare results of models using 5000 and 10000 background points using `17_compareBGresults.R`\
