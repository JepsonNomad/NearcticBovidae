# Comparing 5k and 10k background results
startTime = Sys.time()

#### Import packages ----
library(raster)
library(SDMtune)
library(sf)
library(tidyverse)


#### Load data ----
# Species list
speciesList = c("Ovis canadensis",
                "Ovis dalli",
                "Oreamnos americanus",
                "Ovibos moschatus",
                "Bison bison")

maxentList = readRDS("models/sdm_10k/MaxEnt.RDS")
maxentList

#### Filepaths ----
## Presence paths
path_to_rast05 = "data/modeled_dist_5k"
path_to_rast10 = "data/modeled_dist_10k"


#### Define function ----
compareSp = function(speciesName = "Ovis canadensis"){
  maxEntSp = maxentList[[speciesName]]
  thresholdSp = maxEntSp$Thresholds[5,2]
  
  files_05 = list.files(path_to_rast05,
                        pattern = speciesName,
                        full.names = TRUE)
  files_05_sp = files_05[grep("Present",files_05)]
  files_10 = list.files(path_to_rast10,
                        pattern = speciesName,
                        full.names = TRUE)
  files_10_sp = files_10[grep("Present",files_10)]
  
  rast05 = raster(files_05_sp)
  rast10 = raster(files_10_sp)
  
  rast05[rast05 >= thresholdSp] <- 1
  rast05[rast05 < thresholdSp] <- 0
  rast10[rast10 >= thresholdSp] <- 1
  rast10[rast10 < thresholdSp] <- 0
  
  # plot(rast05)
  # plot(rast10)
  
  presentStack = stack(rast05, rast10)
  
  rasCor = cor(values(presentStack)[,1],
               values(presentStack)[,2],
               use = "na.or.complete")
  return(rasCor)
}

#### Execute function ----
spCorrs = lapply(speciesList,
                 compareSp)

#### View results ----
unlist(spCorrs)

# [1] 0.8701184 0.7235100 0.8357660 0.9458042 0.7041574


stopTime = Sys.time()
print(stopTime-startTime)
BRRR::skrrrahh(12)
