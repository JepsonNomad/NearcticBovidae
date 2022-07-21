#### Spatially thinning occurrence reccords for maxent ----
# 17 May 2020
# Christian John
# Takes about 23 minutes
startTime = Sys.time()
# Designed with reference to Scott Rinnan's post
# https://scottrinnan.wordpress.com/2015/08/31/how-to-construct-a-bias-file-with-r-for-use-in-maxent-modeling/
# And the discussion here:
# https://github.com/danlwarren/ENMTools/issues/191
# But using terra

#### Load packages ----
library(spThin)
library(sf)
library(tidyverse)

set.seed(18891120) # Birth of Hubble

#### Define parameters ----
## Species names
speciesList = c("Ovis canadensis",
                "Ovis dalli",
                "Oreamnos americanus",
                "Ovibos moschatus",
                "Bison bison")


albers = st_crs("+proj=aea +lat_1=20 +lat_2=60 +lat_0=40 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs")

#### Import data ----
## Region of interest 
NorthAmerica = rnaturalearth::ne_countries(scale = 110,
                                           continent = "North America")
## Occurrence data (all N Am artiodactyla)
occurrences = read_sf("data/gbif/bovidae_clean.shp")
occurrences %>%
  st_drop_geometry() %>%
  select(species) %>%
  count(species)

# # A tibble: 5 × 2
# species                 n
# <chr>               <int>
#   1 Bison bison          5482
# 2 Oreamnos americanus  3648
# 3 Ovibos moschatus      438
# 4 Ovis canadensis      8228
# 5 Ovis dalli            689
nrow(occurrences)
# 18485

#### Define thinning function ----
## x is the latin name of a species which will be drawn from the occurrence data
## Second is using spThin which is extremely slow for big datasets
## Using a 6km thinning parameter
thinner = function(x = speciesList[6]){
  message(x)
  message(Sys.time())
  spOutName = gsub(" ", "_", x)
  spOccsDF = occurrences %>%
    dplyr::filter(species == x) %>%
    st_drop_geometry()
  spOccsThin = spThin::thin(spOccsDF,
               lat.col = "lat",
               long.col = "lon",
               spec.col = "species",
               locs.thinned.list.return = TRUE,
               out.dir = "data/gbif",
               out.base = spOutName,
               thin.par = 6,
               reps = 1)[[1]]
  spOccsThin$species = x
  return(spOccsThin)
}


#### Run function ----
## thinner takes about 23 minutes
startTime = Sys.time()
occs_thinned = lapply(speciesList,
                       thinner)
stopTime = Sys.time()
print(stopTime-startTime)


occs_compiled = do.call("rbind",
                        occs_thinned)

#### Data wrangling ----
occs_sf = occs_compiled %>%
  st_as_sf(coords = c("Longitude","Latitude")) %>%
  st_set_crs(4326) %>%
  st_transform(albers)


#### Plot results
thinnedPlot = ggplot() +
  geom_sf(data = st_as_sf(NorthAmerica)) +
  geom_sf(data = occs_sf,
          aes(col = species)) +
  facet_wrap(~species) +
  coord_sf(expand = FALSE,
           crs = 5070) +
  theme(panel.background = element_rect(fill = "lightblue"))

ggsave("plots/occs_bovidOccsThinned.jpg",
       thinnedPlot,
       width = 9, height = 9,
       units = "in", dpi = 600)


#### Save outputs ----
st_write(occs_sf,
         dsn = "data/gbif/bovidae_thinned.shp",
         layer = "bovidae_thinned")

write.csv(x = occs_compiled,
          file = "data/gbif/bovidae_thinned.csv")

#### Summarize ----
occs_compiled %>%
  dplyr::select(species) %>%
  count(species)

# species    n
# 1         Bison bison  519
# 2 Oreamnos americanus  659
# 3    Ovibos moschatus  218
# 4     Ovis canadensis 1915
# 5          Ovis dalli  246
nrow(occs_compiled)
# [1] 3558

stopTime = Sys.time()
print(startTime - stopTime)
BRRR::skrrrahh(12)
