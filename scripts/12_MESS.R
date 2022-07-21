#### Checking environmental similarity across model projections
## See Elith et al 2010
## https://besjournals.onlinelibrary.wiley.com/doi/10.1111/j.2041-210X.2010.00036.x
## as well as Kass et al 2020
## https://jamiemkass.github.io/ENMeval/reference/similarity.html
## Took 1.7 hrs for all 5 species, 8 climate models, and 2 ssps

startTime = Sys.time()

#### Load packages ----
library(terra)
library(dismo)
library(tidyverse)
library(sf)
library(sp)
library(ENMeval)

#### Define parameters ----
outDir = "data/mess"
if(!dir.exists(outDir)){
  dir.create(outDir)
}

## Species
speciesList = c("Ovis canadensis",
                "Ovis dalli",
                "Oreamnos americanus",
                "Ovibos moschatus",
                "Bison bison")

## CMIP6 models
cmip_models = c("BCC-CSM2-MR",
                "CanESM5",
                "CNRM-CM6-1",
                "CNRM-ESM2-1",
                "IPSL-CM6A-LR",
                "MIROC-ES2L",
                "MIROC6",
                "MRI-ESM2-0")
cmip_scenarios = c("ssp245","ssp585")
cmip_products = apply(expand.grid(cmip_models,
                                  cmip_scenarios),
                      1, paste,
                      collapse="/")
cmip_fps = paste0("data/worldclim_v2/cmip6/",
                  cmip_products,
                  "/reprojected/bioclim.tif")

#### Import data ----
sdmData = readRDS("models/sdm_10k/MaxEnt.RDS")

# Load the raster data
predictors = raster::stack("data/worldclim_v2/bioclim/reprojected/biocl_centered.tif")
names(predictors)
## Aggregate the tree products
predictors[["treeTropical"]] <- predictors[["BET_tro"]] +
  predictors[["BDT_tro"]] 
predictors[["treeTemperate"]] <- predictors[["NET_tem"]] + 
  predictors[["BET_tem"]] + predictors[["BDT_tem"]]
predictors[["treeBoreal"]] <-  predictors[["NET_bor"]] + 
  predictors[["NDT_bor"]] + 
  predictors[["BDT_bor"]]
## Remove the unaggregated base layers
predictors = dropLayer(predictors,
                       c("BET_tro","BDT_tro",
                         "NET_tem","BET_tem","BDT_tem",
                         "NET_bor","NDT_bor","BDT_bor"))
names(predictors)

futures = lapply(cmip_fps,
                 raster::stack)
futures = lapply(futures,
                 FUN = function(x){
                   x[["treeTropical"]] <- x[["BET_tro"]] +
                     x[["BDT_tro"]] 
                   x[["treeTemperate"]] <- x[["NET_tem"]] + 
                     x[["BET_tem"]] + x[["BDT_tem"]]
                   x[["treeBoreal"]] <-  x[["NET_bor"]] + 
                     x[["NDT_bor"]] + 
                     x[["BDT_bor"]]
                   ## Remove the unaggregated base layers
                   x = dropLayer(x,
                                 c("BET_tro","BDT_tro",
                                   "NET_tem","BET_tem","BDT_tem",
                                   "NET_bor","NDT_bor","BDT_bor"))
                   return(x)
                 })
names(futures) <- gsub("/","_",cmip_products)

#### Define functions ----
## A function to return mess layers for a given species 
## and all time period predictors
## mySpecies is a character vector with the species name
## meDat is the list of Maxent data products generated in the maxEnt script
## preds is a stack of present predictors
## futrs is a list of stacks of future predictors (n list elements = n cmip models)
messFunc = function(mySpecies = speciesList[1],
                    meDat = sdmData,
                    preds = predictors,
                    futrs = futures){
  message("Running ENMeval MESS protocol...")
  message(paste0("Species: "), mySpecies)
  message(Sys.time())
  maxEntSp = meDat[[mySpecies]]
  ## Get extracted values from presence locations
  occs.z <- maxEntSp$MaxEnt@data@data[maxEntSp$MaxEnt@data@pa == 1,]
  
  #### Start with the present ----
  ## Subset the preds data to just relevant layers
  predSp = preds[[names(occs.z)]]
  plot(predSp)
  occs.z 
  ## Calculate similarity
  pres.mess = mess(x = predSp, v = occs.z)
  plot(pres.mess)
  # Remove inf values
  pres.mess[pres.mess == -Inf] <- NA
  pres.mess[pres.mess == Inf] <- NA
  writeRaster(pres.mess,
              filename = paste0(outDir,
                                "/messPresent_",
                                mySpecies,
                                ".tif"),
              overwrite = TRUE)
  
  #### Next, the future paths ----
  for(i in 1:length(futrs)){
    cmipScenario = names(futrs)[i]
    futrSp = futrs[[i]][[names(occs.z)]]
    ## Calculate similarity
    futr.mess = mess(x = futrSp, v = occs.z)
    # Remove inf values
    futr.mess[futr.mess == -Inf] <- NA
    futr.mess[futr.mess == Inf] <- NA
    writeRaster(futr.mess,
                filename = paste0(outDir,
                                  "/messFuture_",
                                  cmipScenario,
                                  "_",
                                  mySpecies,
                                  ".tif"),
                overwrite = TRUE)
  }
}


#### Run analysis ----
lapply(speciesList,
       messFunc)


stopTime = Sys.time()
print(stopTime-startTime)
BRRR::skrrrahh(4)

