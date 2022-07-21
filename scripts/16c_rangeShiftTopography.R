# Calculating mean elevation of modeled ranges
# ~11 min
startTime = Sys.time()

#### Import packages ----
library(raster)
library(tidyverse)
library(kableExtra)

#### Define parameters ----
# Species info
speciesList = c("Ovis canadensis",
                "Ovis dalli",
                "Oreamnos americanus",
                "Ovibos moschatus",
                "Bison bison")

#### Import data ----
sdmData = readRDS("models/sdm_10k/MaxEnt.RDS")

myDEM = raster::raster("data/reprojected_dem/topographicCovariates.tif",
                       bands = 1)

#### Define functions ----
extractElev = function(mySp,
                       elev_stack = myDEM) {
  message(mySp)
  message(Sys.time())
  # Import species-specific maxent model
  maxEntSp = sdmData[[mySp]]
  ## Select the max sensitivity + specificity threshold
  maxEntThresh = maxEntSp$Thresholds[5,2]
  # Read prediction data
  distPreds_stack = raster(paste0("data/modeled_dist_10k/Predicted_Present_",
                                  mySp,
                                  ".tif"))
  names(distPreds_stack) = "Predicted"
  # Identify scenarios
  mySSPs = c("45","85")
  # A function to compile all forecast distributions for a scenario
  makeSSPras = function(ssp){
    # Identify relevant files
    futureFiles = list.files(path = "data/modeled_dist_10k/",
                             pattern = "Future_",
                             full.names = TRUE)
    glmFutures = futureFiles[grep(pattern = mySp,
                                  x = futureFiles)]
    glmSSPfns = glmFutures[grep(pattern = ssp,
                                x = glmFutures)]
    ## Find the name of the cmip models
    cmipFileNames = basename(glmSSPfns) 
    cmipFileNames = gsub(pattern = "Predicted_Future_",
                         replacement = "",
                         x = cmipFileNames)
    cmipFileNames = gsub(pattern = paste0("_",mySp),
                         replacement = "",
                         x = cmipFileNames)
    cmipModelNames = gsub(pattern = ".tif",
                          replacement = "",
                          x = cmipFileNames)
    
    # Read prediction data
    distFuts_list = lapply(X = glmSSPfns,
                           FUN = raster)
    # Rename raster layers according to generation scheme in predictDist()
    distFuts_list <- lapply(distFuts_list,
                            FUN = function(x){
                              names(x) <- "Predicted"
                              return(x)})
    for(i in 1:length(distFuts_list)){
      names(distFuts_list)[[i]] <- paste0(names(distFuts_list[[i]]),
                                          "_",
                                          cmipModelNames[i])
    } 
    distFuts_stack = raster::stack(distFuts_list)
  }
  
  ssp45Stack = makeSSPras(mySSPs[1])
  ssp85Stack = makeSSPras(mySSPs[2])
  
  # Reassign raster values based on Maxent threshold
  distPreds_stack[distPreds_stack < maxEntThresh] <- 0
  distPreds_stack[distPreds_stack >= maxEntThresh] <- 1
  ssp45Stack[ssp45Stack < maxEntThresh] <- 0
  ssp45Stack[ssp45Stack >= maxEntThresh] <- 1
  ssp85Stack[ssp85Stack < maxEntThresh] <- 0
  ssp85Stack[ssp85Stack >= maxEntThresh] <- 1
  
  ## Calculate range shift
  currElMean = mean(elev_stack[distPreds_stack[]==1],
                    na.rm = T)
  names(currElMean) <- NULL
  ## Extract mean elevation of each GCM modeled range under SSP2-4.5
  ssp2ElMeans = list()
  for(i in 1:nlayers(ssp45Stack)){
    ssp2ElMeans[[i]] = mean(elev_stack[ssp45Stack[[i]][]==1],
                            na.rm = T)
  }
  ## Extract mean elevation of each GCM modeled range under SSP5-8.5
  ssp5ElMeans = list()
  for(i in 1:nlayers(ssp85Stack)){
    ssp5ElMeans[[i]] = mean(elev_stack[ssp85Stack[[i]][]==1],
                            na.rm = T)
  }
  
  ## Find mean, s.e. of futures
  ssp2mean = mean(unlist(ssp2ElMeans))
  ssp2se = plotrix::std.error(unlist(ssp2ElMeans))
  ssp5mean = mean(unlist(ssp5ElMeans))
  ssp5se = plotrix::std.error(unlist(ssp5ElMeans))
  
  return(c("Current" = currElMean,
           "meanSSP2" = ssp2mean,
           "seSSP2" = ssp2se,
           "meanSSP5" = ssp5mean,
           "seSSP5" = ssp5se))
}



#### Implement functions ----
elList = lapply(X = speciesList,
                FUN = extractElev)


#### Format results ----
## Convert to DF
elDF = do.call(rbind,
               elList) %>%
  as.data.frame()
## Add species column
elDF$Species = speciesList

## Create readable columns
elDF = elDF %>%
  mutate(Current = sprintf("%.2f",Current),
         SSP2_45 = paste0(sprintf("%.2f",meanSSP2),
                          " ± ",
                          sprintf("%.2f",seSSP2)),
         SSP5_85 = paste0(sprintf("%.2f",meanSSP5),
                          " ± ",
                          sprintf("%.2f",seSSP5))) %>%
  select(Species,
         Current,
         SSP2_45,
         SSP5_85)


## Convert to kable and export
elDF %>%
  rename("SSP2-4.5" = SSP2_45,
         "SSP5-8.5" = SSP5_85) %>%
  kable(table.attr = "style='width:40%;'") %>%
  kable_classic() %>%
  save_kable(file = "tables/predictedElChange.png",
             zoom = 4)


