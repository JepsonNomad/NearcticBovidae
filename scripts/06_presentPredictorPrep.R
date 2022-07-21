###### MASKING AND STACKING BIOCLIMATIC VARIABLES
## Takes about 3 minutes
startTime = Sys.time()

library(terra)
library(tidyverse)
library(sf)

#### Import data ----
## Worldclim2
wcPath = "data/worldclim_v2/bioclim/reprojected"
wcFilesAll = list.files(path = wcPath,
                        pattern = "*_bio_",
                        full.names = TRUE)
wcFilesTif = wcFilesAll[grep(pattern="*.tif",x=wcFilesAll)]
my.biocl = rast(wcFilesTif)

## Terrain data
my.terrain = rast("data/reprojected_dem/NA_terrain_alb.tif")

## Landcover
my.landcover = rast("data/GCAM/aggregated/ssp1.tif")
# plot(my.landcover)
# hist(my.landcover)

# ROI
samplingArea = st_read("data/ROIs/SamplingArea.shp")
samplingAreaVec = vect(samplingArea)

#### Data wrangling ----
## Extract bioclimatic layer number and rename
wcLayerNum <- sapply(X = str_split(names(my.biocl), "_"),
                     "[",
                     4) %>%
  str_pad(width = 2,
          side = "left",
          pad = "0")
names(my.biocl) <- paste0("Biocl_",wcLayerNum)
# Reorder stack
my.biocl = my.biocl[[order(names(my.biocl))]]
my.biocl

# Rename stack
names(my.terrain) <- c("DEM","TRI")
my.topo = my.terrain[[c("DEM","TRI")]]
plot(my.topo)
writeRaster(x = my.topo,
            filename = "data/reprojected_dem/topographicCovariates.tif",
            overwrite = TRUE)


## Stack all the datasets together
plot(my.biocl[[1]])
plot(my.topo[[1]])
predictors_NA = rast(list(my.biocl,
                          my.topo,
                          my.landcover)) %>%
  crop(samplingAreaVec) %>%
  mask(samplingAreaVec)
names(predictors_NA)
# Center the data by raster layer. Report mean values to future climate scenarios can also be transformed based on the centering here. Some predictors are inappropriate to center (e.g. circular measurements like aspect)
predsToCenter = c(1:21)

centers = global(predictors_NA, stat='mean', na.rm=TRUE)
rownames(centers) <- NULL
bioclCenters.df = data.frame("Variable" = names(predictors_NA),
                             "Mean" = centers)
bioclCenters.df$Centered = FALSE
bioclCenters.df$Centered[predsToCenter] <- TRUE
write.table(x = bioclCenters.df,
            file="data/worldclim_v2/Bioclim_Centers.csv",
            sep = ",",
            row.names = FALSE,
            col.names = TRUE)
## Center the data, where appropriate
for(i in predsToCenter){
  predictors_NA[[i]] = predictors_NA[[i]] - as.numeric(centers[i,1])
}

## Save centered predictors
writeRaster(x = predictors_NA,
            filename = 
              "data/worldclim_v2/bioclim/reprojected/biocl_centered.tif", 
            overwrite = TRUE)

stopTime = Sys.time()
print(stopTime - startTime)
BRRR::skrrrahh(14)
