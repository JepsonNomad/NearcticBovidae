## Takes about 13 minutes in total for:
## > 5 spp 
## > with presence + background points
## > and 31 raster layers

startTime = Sys.time()

# Arranging and extracting environmental variables
library(terra)
library(tidyverse)
library(sf)
library(GGally)

#### Define parameters ----
albers = st_crs("+proj=aea +lat_1=20 +lat_2=60 +lat_0=40 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs")

#### Define parameters
## Predictor layers of interest 
## Layer order is: 
## 1-19: Bioclimatic layers
## 20-25: "DEM","SLP","cosASP","sinASP","TRI","TPI"
## 26-33: Landcover layers: Needleleaf, Broadleaf, Shrubs, Grasses, AgRainfed, AgIrrigated, Urban, Barren
completeLayerList = c(1:34)
layerList = c(1,4,5,6,12,15,16,17,20,21,24,26:33)

#### Import data ----
# Bring in North America shapefile
countries <- rnaturalearth::ne_countries(returnclass = "sf")
NorthAmerica <- countries[countries$continent == "North America",] %>%
  st_transform(albers)
unique(NorthAmerica$name)

## Bioclim layers
predictors_NA = rast("data/worldclim_v2/bioclim/reprojected/biocl_centered.tif",
                     lyrs = layerList)
# plot(predictors_NA)

## Occurrence data
occurrences_sf = read_sf("data/gbif/bovidae_thinned.shp") %>%
  st_transform(albers) %>%
  dplyr::select(species) %>%
  mutate("Presence" = "Present")
## Background data 
background_sf = read_sf("data/background/Background_10000.shp") %>%
  st_transform(albers) %>%
  dplyr::select(species) %>%
  mutate("Presence" = "Absent")

#### Data extraction ----
bovidae_clean = rbind(occurrences_sf,
                      background_sf)
names(bovidae_clean)
bovidae_vect = vect(bovidae_clean)

# The extraction step takes a long time given the number of points and layers. Grab a good book!
# Don't forget to benchmark
start_time_SDMextract = Sys.time()
presExtract <- terra::extract(predictors_NA, bovidae_vect, xy = TRUE)
presExtract$species = bovidae_clean$species
presExtract$presence = bovidae_clean$Presence
end_time_SDMextract = Sys.time()
print(end_time_SDMextract - start_time_SDMextract)

# Plot covariance among predictors
for(i in unique(bovidae_clean$species)){
  speciesSubset = bovidae_clean$species == i
  speciesVals = presExtract[speciesSubset,]
  myPairs = ggpairs(speciesVals %>%
                      # mutate(LND = as.factor(LND)) %>%
                      dplyr::select(-ID,-x,-y),
                    size = 0.01, alpha = 0.05,
                    ggplot2::aes(col = presence))
  ggsave(paste0("plots/pairs/predictors_",
                gsub(i, pattern = " ", replacement = "-"),
                "_pairs.jpg"),
         myPairs,
         width = 30, height = 30, units = "in", dpi = 300)
}
  


saveRDS(presExtract,
        "data/sdmData.RDS")
stopTime = Sys.time()
print(stopTime-startTime)
BRRR::skrrrahh(4)

