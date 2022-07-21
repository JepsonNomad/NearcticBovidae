# Measuring surface area of modeled ranges
# ~5 min
startTime = Sys.time()

#### Import packages ----
library(raster)
library(tidyverse)
library(kableExtra)

#### Import data ----
sdmData = readRDS("models/sdm_10k/MaxEnt.RDS")


#### Define parameters ----
# Species info
speciesList = c("Ovis canadensis",
                "Ovis dalli",
                "Oreamnos americanus",
                "Ovibos moschatus",
                "Bison bison")

speciesNamesList = c("Bighorn sheep",
                     "Thinhorn sheep",
                     "Mountain goat",
                     "Muskox",
                     "American bison")

#### Define functions ----
extractArea = function(mySp) {
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
  
  ## Calculate pixel areas. Since we are working in projected coordinate space, and because predicted present pixels are 1 and predicted absent are 0, we can sum the values in each layer and convert to physical area.
  ## 6km x 6km pixels = 36 km^2 pixel area
  currArea = cellStats(distPreds_stack, stat = sum, na.rm = T)*36
  ssp2Area = cellStats(ssp45Stack, stat = sum, na.rm = T)*36
  ssp5Area = cellStats(ssp85Stack, stat = sum, na.rm = T)*36
  
  ## Find mean, s.e. of futures
  names(ssp2Area) <- NULL
  names(ssp5Area) <- NULL
  
  ssp2mean = mean(ssp2Area)
  ssp2se = plotrix::std.error(ssp2Area)
  
  ssp5mean = mean(ssp5Area)
  ssp5se = plotrix::std.error(ssp5Area)
  return(c("Current" = currArea,
           "meanSSP2" = ssp2mean,
           "seSSP2" = ssp2se,
           "meanSSP5" = ssp5mean,
           "seSSP5" = ssp5se))
}



#### Implement functions ----
areaList = lapply(X = speciesList,
                  FUN = extractArea)



#### Format results ----
## Convert to DF
areaDF = do.call(rbind,
                 areaList) %>%
  as.data.frame()
## Add species column
areaDF$Species = speciesNamesList

## Create readable columns
areaDF = areaDF %>%
  mutate(Current = sprintf("%.2f", Current/1e6),
         SSP2_45 = paste0(sprintf("%.2f",meanSSP2/1e6),
                          " ± ",
                          sprintf("%.2f",seSSP2/1e6)),
         SSP5_85 = paste0(sprintf("%.2f",meanSSP5/1e6),
                          " ± ",
                          sprintf("%.2f",seSSP5/1e6))) %>%
  select(Species,
         Current,
         SSP2_45,
         SSP5_85)


## Convert to kable and export
areaDF %>%
  rename("SSP2-4.5" = SSP2_45,
         "SSP5-8.5" = SSP5_85) %>%
  kable(table.attr = "style='width:40%;'") %>%
  kable_classic() %>%
  save_kable(file = "tables/predictedDismoChange.png",
             zoom = 4)


