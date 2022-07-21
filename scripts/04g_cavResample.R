library(terra)
library(tidyverse)
library(sf)

#### Define parameters ----
albers = st_crs("+proj=aea +lat_1=20 +lat_2=60 +lat_0=40 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs")


#### Import data ----
## > Raster ----
## Data from Raynolds et al 2019
## https://www.sciencedirect.com/science/article/pii/S0034425719303165#s0095
## https://data.mendeley.com/datasets/c4xj5rv6kv/1
r = rast("data/CAVM/raster_cavm_v1.tif")
plot(r)

## Template raster
templateRas = rast("data/worldclim_v2/bioclim/reprojected/wc2.1_2.5m_bio_1.tif",
                   lyrs = 1)

## > Vector ----
GL = rnaturalearth::ne_countries(country = "Greenland",
                                 returnclass = "sf") 
GL_buff = GL %>%
  st_transform(crs(r)) %>%
  st_buffer(100000) %>%
  vect()

NorthAmerica <- rnaturalearth::ne_countries(continent = "North America",
                                            scale = 10,
                                            returnclass = "sf") 
NorthAmericaAlb <- NorthAmerica  %>%
  st_transform(albers)


#### Data wrangling ----
## First, create an ice layer
ICE = mask(crop(r,GL_buff),GL_buff)
## Then mask to the relevant value (93 is ice)
ICE[ICE != 93] <- NA
## Call everything that isn't NA (i.e. not not ice), 1
ICE[!is.na(ICE)] <- 1
## Resample to target crs, extent, and resolution
ICE = resample(ICE,templateRas)
## Preview
plot(ICE)

#### Convert to poly ----
## Ice mask
# Convert binary raster to sf object
# Amazing rasterToPolygons() alternative
# Thanks Mikko https://gis.stackexchange.com/a/357792/67264
icePoly <- sf::st_as_sf(stars::st_as_stars(ICE), 
                        as_points = FALSE, 
                        merge = TRUE) %>%
  st_transform(albers) %>%
  st_make_valid()
# requires the sf, sp, raster and stars packages

# Isolate GL shape to plot outline with ice to verify our mask is effective
ggplot() +
  geom_sf(data = GL, fill = "orange") +
  geom_sf(data = icePoly, fill = "black")
# Looks good.

## Find difference between polygons;
## Remove ice from sampleable area
## Maykka's problem was my solution
# https://gis.stackexchange.com/q/353633/67264
st_erase = function(x, y) st_difference(x, st_union(st_combine(y)))
SamplingArea = st_erase(NorthAmericaAlb,
                        icePoly)


## Check the sampling area
ggplot() +
  geom_sf(data = SamplingArea,
          fill = "purple")

#### Save outputs ----
## Save the ice raster layer
writeRaster(ICE,
            filename = "data/CAVM/reprojected/GreenlandIce.tif",
            overwrite = TRUE)
## Save the ice-differenced vector data 
st_write(SamplingArea,
         dsn = "data/ROIs/SamplingArea.shp",
         layer = "SamplingArea")
