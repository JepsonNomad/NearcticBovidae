startTime = Sys.time()
# Setting up environmental variables to match other data formats
# This takes a little over 10 minutes.

library(terra)
library(sf)
library(tidyverse)


#### Define parameters ----
albers = st_crs("+proj=aea +lat_1=20 +lat_2=60 +lat_0=40 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs")


#### Import data ----
NorthAmerica <- read_sf("./data/ROIs/NorthAmerica.shp") %>%
  st_transform(albers)
Bioclim_Centers = read.table("data/worldclim_v2/Bioclim_Centers.csv",
                             sep = ",",
                             header = TRUE)
my.topo <- rast("data/reprojected_dem/topographicCovariates.tif")
names(my.topo) <- c("DEM","TRI")

## North America landcover
my.landcover_ssp2 = rast("data/GCAM/aggregated/ssp2.tif")
my.landcover_ssp5 = rast("data/GCAM/aggregated/ssp5.tif")

# ROI
samplingArea = st_read("data/ROIs/SamplingArea.shp")
samplingAreaVec = vect(samplingArea)


#### Define functions ----
# x is a cmip forecast raster stack
# m is a shapefile with the ROI for masking
EnvironStacker = function(x,
                          topo = my.topo,
                          lnd = my.landcover,
                          centers = Bioclim_Centers$mean,
                          centered = Bioclim_Centers$Centered,
                          m = NorthAmerica){
  # First, benchmark
  start_time = Sys.time()
  message(start_time)
  # Stack bioclimatic variables with topographic covariates
  predictors <- rast(list(x,
                          topo,
                          lnd)) %>%
    crop(samplingAreaVec) %>%
    mask(samplingAreaVec)
  
  # Center predictor layers based on present-day means for GLM translation
  predictors_centered <- rast()
  for(i in 1:nlyr(predictors)){
    if(centered[i] == TRUE){
      layer_centered = predictors[[i]]-centers[i]
      predictors_centered = rast(list(predictors_centered, layer_centered))
    }else{
      predictors_centered = rast(list(predictors_centered, predictors[[i]]))
    }
  }
  names(predictors_centered) <- Bioclim_Centers$Variable
  # Match topo layers to bioclimatic layers - this was done using
  # gdalwarp_batch.py - crs and extent information extracted
  # from source crs and extent in R.

  # Final benchmark and message
  end_time = Sys.time()
  print(end_time-start_time)
  
  # Return final predictors stack
  return(predictors_centered)
}



#### Data wrangling ----
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

p = cmip_products[1]

#### Apply function and save results ----
## Apply file import and environ stacker across all model products
lapply(cmip_products,
       FUN = function(p){
         message(p)
         ## Find cmip scenario
         scenario = str_split(p,"/")[[1]][2]
         ## Create a list of filepaths, with each element of the 
         ## list corresponding to a cmip product
         cmip_file = list.files(paste0("data/worldclim_v2/cmip6/",
                                       p,
                                       "/reprojected"),
                                pattern = "2100.tif",
                                full.names = TRUE)
         cmip_ras = rast(cmip_file)
         cmip_namesraw = names(cmip_ras)
         cmip_names_date = sapply(str_split(cmip_namesraw, "_"),
                                  "[",
                                  6)
         cmip_nums = sapply(str_split(cmip_namesraw, "_"),
                            "[",
                            7) %>%
           str_pad(width = 2,
                   side = "left",
                   pad = "0")
         names(cmip_ras) <- paste0("Biocl_", cmip_nums)
         if(scenario == "ssp245"){
           in_landcover = my.landcover_ssp2
         }else if(scenario == "ssp585"){
           in_landcover = my.landcover_ssp5
         }else{
           message("unexpected scenario detected:")
           message(scenario)
           }
         myStack = EnvironStacker(x = cmip_ras,
                                  lnd = in_landcover)
         writeRaster(myStack,
                     paste0("data/worldclim_v2/cmip6/",
                            p,
                            "/reprojected/bioclim.tif"),
                     overwrite = TRUE)
       })


endTime = Sys.time()
print(endTime - startTime)
BRRR::skrrrahh(12)

## Took about 12.5 minutes
