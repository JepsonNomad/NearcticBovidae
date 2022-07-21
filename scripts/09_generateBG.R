# Generate background data using the bias grid from 07_biasMatrix.R
startTime = Sys.time()
## Takes just over a minute

#### Load packages ----
library(terra)
library(sf)
library(tidyverse)

set.seed(19891109) # Berlin wall falls


#### Define parameters ----
albers = st_crs("+proj=aea +lat_1=20 +lat_2=60 +lat_0=40 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs")

## Species names
speciesList = c("Ovis canadensis",
                "Ovis dalli",
                "Oreamnos americanus",
                "Ovibos moschatus",
                "Bison bison")


#### Import data ----
## Load the countries dataset and isolate North America
countries <- rnaturalearth::ne_countries(returnclass = "sf", scale = 10)
head(countries)
NorthAmerica <- countries[countries$continent == "North America",]
unique(NorthAmerica$name)
NorthAmericaAlb <- NorthAmerica  %>%
  st_transform(albers)

## Bias raster
bias = rast("data/gbif/biasGrid.tif")

## Sampling Area (which excludes GL Ice cap)
SamplingArea = read_sf("data/ROIs/SamplingArea.shp")

## Occurrence data
occurrences = read_sf("data/gbif/bovidae_thinned.shp")

#### Data wrangling ----
## Check the sampling area
ggplot() +
  geom_sf(data = SamplingArea,
          fill = "purple")

## Convert sampling area to terra vect
SamplingVect = terra::vect(SamplingArea)
SamplingGrid = mask(bias, SamplingVect)

#### Generate background points ----
bgGenerator = function(speciesName = speciesList[4],
                       occPoints = occurrences){
  # plot(SamplingGrid)
  bg <- spatSample(SamplingGrid, size = 10000, method = "weights",
                   as.points = TRUE) %>%
    st_as_sf()
  bg$species = speciesName
  return(bg)
}

bgList = lapply(speciesList,
                bgGenerator)

bg_sf = do.call("rbind",
                bgList)


# Visualize the background data
backgroundPoints = ggplot() +
  geom_sf(data = rnaturalearth::ne_countries(continent = "North America", 
                                             scale = 110,
                                             returnclass = "sf") %>%
            st_transform(albers)) +
  geom_sf(data = bg_sf, aes(col = species),
          size = 0.001) +
  scale_color_manual(values = CJsBasics::KellyCols[3:20])
ggsave("plots/backgroundPoints10000.jpg",
       backgroundPoints,
       width = 16,
       height = 16,
       units = "in", dpi = 300)

#### Write outputs ----
# Save the data as .shp
write_sf(bg_sf,
         dsn = "data/background/Background_10000.shp",
         layer = "Background")
# Save the data as RDS
saveRDS(bg_sf,
        "data/background/Background_10000.RDS")


stopTime = Sys.time()
print(stopTime-startTime)
BRRR::skrrrahh(14)