# Predicting present and future predictions
# Predictions limited to positive MESS areas
# About 32 for all spp
startTime = Sys.time()

#### Import packages ----
library(raster)
library(SDMtune)

#### Define parameters ----
speciesList = c("Ovis canadensis",
                "Ovis dalli",
                "Oreamnos americanus",
                "Ovibos moschatus",
                "Bison bison")

## Output directory
outDir = "data/modeled_dist_10k"
if(!dir.exists(outDir)){
  dir.create(outDir)
}

#### Load data ----

## > Model data ----
maxentList = readRDS("models/sdm_10k/MaxEnt.RDS")
maxentList

## > Predictors ----
predictors = raster::stack("data/worldclim_v2/bioclim/reprojected/biocl_centered.tif")
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

## > Future predictors ----
## Find the models of interest
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
futureNames = gsub("/","_",
                   cmip_products)
futurePaths = paste0("./data/worldclim_v2/cmip6/",
                     cmip_products,
                     "/reprojected/bioclim.tif")
futures = lapply(futurePaths,
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

## > Mess layers ----
## Prep present layers
messPresFPs = list.files(path = "data/mess",pattern = "Present",
                         full.names = TRUE)
messPresList = lapply(messPresFPs,
                      raster::stack)
messPresList = lapply(messPresList,
                      function(x){
                        y = names(x)
                        names(x)<- gsub("messPresent_",
                                        "",
                                        y)
                        return(x)
                      })
names(messPresList) <- lapply(messPresList,names)
## Prep future mess layers
messFutsFPs = list.files(path = "data/mess",
                         pattern = "Future",
                         full.names = TRUE)
messFutsList = lapply(messFutsFPs,
                      raster::stack)
messFutsList = lapply(messFutsList,
                      function(x){
                        y = names(x)
                        names(x)<- gsub("messFuture_",
                                        "",
                                        y)
                        return(x)
                      })
names(messFutsList) <- lapply(messFutsList,names)

####### Define the grand predictor ----
# Arguments:
# x is a character: species name used in filepaths (e.g. "bighorn" or other 6-character name from earlier scripts)
genPreds = function(x = speciesList[1],
                    meList = maxentList,
                    preds = predictors,
                    futs = futures,
                    futNames = futureNames,
                    messPres = messPresList,
                    messFuts = messFutsList){
  mySp = x
  message(paste0("Species: ", mySp))
  ## Subset maxent list to the species element
  speciesME = meList[[mySp]]
  
  ## Subset to species-specific present MESS grid 
  speciesPresMESS = messPres[[gsub(" ","_",mySp)]]
  ## Subset to species-specific present MESS grid 
  speciesFutsMESS = messFuts[grep(gsub(" ","_",mySp),names(messFuts))]
    
  ## Generate predictions - present ----
  distPresent = SDMtune::predict(speciesME$MaxEnt, preds)
  ## Remove negative MESS cases
  distPresent[speciesPresMESS < 0] <- NA
  ## Save output
  writeRaster(distPresent,
              filename = paste0(
                outDir, "/Predicted_Present_",
                mySp,
                ".tif"),
              overwrite = TRUE)
  
  ## Generate predictions - future ----
  distFutures = lapply(futs,
                       SDMtune::predict,
                       object = speciesME$MaxEnt)
  distFutures = do.call(stack,
                        distFutures)
  ## Mask to MESS consistency and save
  for(i in 1:nlayers(distFutures)){
    futID = futNames[i]
    distFut = distFutures[[i]]
    messFut = speciesFutsMESS[grep(gsub("-",".",futID),
                                   names(speciesFutsMESS))]
    ## Remove negative MESS cases
    distFut[messFut[[1]] < 0] <- NA
    writeRaster(distFut,
                filename = paste0(
                  outDir, "/Predicted_Future_",
                  futNames[i], 
                  "_",
                  mySp,
                  ".tif"),
                overwrite = TRUE)
  }
}


#### Generate, save prediction rasters ----
lapply(X = speciesList,
       FUN = genPreds,
       meList = maxentList,
       preds = predictors,
       futs = futures,
       futNames = futureNames)

endTime = Sys.time()
print(endTime-startTime)

BRRR::skrrrahh(12)
