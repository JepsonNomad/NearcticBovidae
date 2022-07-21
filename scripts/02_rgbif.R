# Species distribution data using the rgbif interface with GBIF database
# Modified 5 May 2022 to collect all occurrence data in one dataset
# And upgrade to mostly sf operations


library(mapview)
library(tidyverse)
library(rgbif)
library(sp)
library(sf)

#### Import data ----
countries <- rnaturalearth::ne_countries(scale = 10)
countriesLQ <- rnaturalearth::ne_countries(scale = 110)
NorthAmerica <- countries[countries$continent == "North America",]
unique(NorthAmerica$name)
NorthAmericaLQ <- rnaturalearth::ne_countries(scale = 110,
                                              continent = "North America")

#### Prep rgbif downloads ----
myTaxa = c(2441119,# Bighorn sheep
           2441118,# Thinhorn sheep
           2441151,# Mtn goat
           2441108,# Muskox
           2441176,# American bison
           2440902,# Pronghorn
           2440974,# Mule deer
           2440965,# White tailed deer
           8600904,# Elk
           4262283,# Moose
           5220114,# Caribou
           2440996 # Collared peccary
)

## Count records
# lapply(as.character(myTaxa), 
#        function(x) occ_count(taxonKey = x))
# 
# [[1]]
# [1] 11499
# 
# [[2]]
# [1] 2089
# 
# [[3]]
# [1] 4680
# 
# [[4]]
# [1] 8008
# 
# [[5]]
# [1] 15325
# 
# [[6]]
# [1] 8300
# 
# [[7]]
# [1] 53317
# 
# [[8]]
# [1] 118117
# 
# [[9]]
# [1] 14128
# 
# [[10]]
# [1] 3605
# 
# [[11]]
# [1] 62804
# 
# [[12]]
# [1] 2003


#### System settings
#### *** DELETE AFTER RUNNING ***
# Sys.setenv("GBIF_USER"='YOURUSERNAMEHERE')
# Sys.setenv("GBIF_PWD"='YOURPASSWORDHERE')
# Sys.setenv("GBIF_EMAIL"='YOUREMAILHERE')
#### *** DELETE AFTER RUNNING ***

# ## Download records
# # Downloaded 5 May 2022
# dlKey = occ_download(pred_in('taxonKey', 
#                              myTaxa),
#                      pred("hasCoordinate", TRUE),
#                      pred("hasGeospatialIssue", FALSE))
# # dlKey is 0266154-210914110416597
# d1 <- occ_download_get(dlKey[1], path = "data/gbif/")
# 

## Import occurrence data
data_raw = rgbif::occ_download_import(key="0266154-210914110416597",
                                      path="data/gbif/")
nrow(data_raw)
# 265307 total occurrences

data_raw %>%
  select(species) %>%
  table()
#    Alces americanus  Antilocapra americana            Bison bison 
#                2700                   6731                  11921 
#      Cervus elaphus    Odocoileus hemionus Odocoileus virginianus 
#               13303                  50272                 108683 
# Oreamnos americanus       Ovibos moschatus        Ovis canadensis 
#                3939                   6350                   9631 
#          Ovis dalli          Pecari tajacu      Rangifer tarandus 
#                1158                   1423                  49196 

#### Data wrangling ----
## Inspect raw data
data_raw %>%
  head() %>%
  View()

## Keep only core columns of interest
data_raw = data_raw %>%
  select(gbifID,
         species,
         scientificName,
         family,
         genus,
         specificEpithet,
         infraspecificEpithet,
         acceptedScientificName,
         taxonRank,
         basisOfRecord,
         eventDate,
         eventTime,
         year,
         continent,
         countryCode,
         level0Name,
         level1Name,
         level2Name,
         identifier,
         license,
         publisher,
         institutionCode,
         collectionCode,
         datasetName,
         occurrenceID,
         catalogNumber,
         individualCount,
         sex,
         lifeStage,
         behavior,
         coordinateUncertaintyInMeters,
         decimalLatitude,
         decimalLongitude)

## Make some new lat/lon columns so we don't lose them when making spatial objects
data_raw$lat = data_raw$decimalLatitude
data_raw$lon = data_raw$decimalLongitude
## Create spatial object
coordinates(data_raw) <- ~decimalLongitude+decimalLatitude
data_geo = st_as_sf(data_raw)
st_crs(data_geo) <- st_crs(4326)

## Get just bovids
bovidae_geo = data_geo %>%
  filter(species %in% c("Ovis canadensis",
                        "Ovis dalli",
                        "Oreamnos americanus",
                        "Ovibos moschatus",
                        "Bison bison"))
## Check the count
nrow(bovidae_geo)
## 32999 observations prior to cleaning

# Look at the data
allBovidOccsPlot = ggplot() +
  geom_sf(data = st_as_sf(countriesLQ)) +
  geom_sf(data = bovidae_geo) +
  facet_wrap(~species) +
  coord_sf(expand = FALSE)
ggsave("plots/occs_allBovids.jpg",
       allBovidOccsPlot,
       width = 12, height = 9,
       units = "in", dpi = 600)

#### Obvious geo problems ----
# First look at instances where lon or lat == 0
bovidae_lon0 = bovidae_geo %>%
  filter(lon == 0)
print(nrow(bovidae_lon0))

bovidae_lat0 = bovidae_geo %>%
  filter(lat == 0)
print(nrow(bovidae_lat0))
# No cases for either.

# Check for duplicated observations
print(sum(duplicated(bovidae_geo)))
# No duplicated observations.

#### Find points over sea ----
# Check country boundaries and instances of observations over sea
bovidae_ovr = over(as(bovidae_geo, "Spatial"),
                   NorthAmerica)
# Extract country names
bovidae_cntr = bovidae_ovr$name
# find observations over the ocean
seapoints = which(is.na(bovidae_cntr))
seapoints

## Plot seapoints
## Seapoints in purple, land points in green.
sealandPlot = ggplot() +
  geom_sf(data = st_as_sf(countriesLQ)) +
  geom_sf(data = bovidae_geo[-seapoints,],
          col = "green2",
          size = 0.25) +
  geom_sf(data = bovidae_geo[seapoints,],
          col = "black") +
  geom_sf(data = bovidae_geo[seapoints,],
          col = "purple",
          size = 0.25) +
  facet_wrap(~species) +
  coord_sf(expand = FALSE) +
  theme(panel.background = element_rect(fill = "lightblue"))
ggsave("plots/occs_seapoints.jpg",
       sealandPlot,
       width = 12, height = 9,
       units = "in", dpi = 600)

#### Country mismatches ----
# check for country issues:
country_mismatches = which(bovidae_ovr$brk_name != bovidae_geo$level0Name)
country_mismatches

mismatchPlot = ggplot() +
  geom_sf(data = st_as_sf(countriesLQ)) +
  geom_sf(data = bovidae_geo[-country_mismatches,],
          col = "green2",
          size = 0.25) +
  geom_sf(data = bovidae_geo[country_mismatches,],
          col = "black") +
  geom_sf(data = bovidae_geo[country_mismatches,],
          col = "purple",
          size = 0.25) +
  facet_wrap(~species) +
  coord_sf(expand = FALSE) +
  theme(panel.background = element_rect(fill = "lightblue"))
ggsave("plots/occs_countryMismatches.jpg",
       mismatchPlot,
       width = 12, height = 9,
       units = "in", dpi = 600)


#### Compile problematic data ----
questionable_data_indices = c(seapoints,country_mismatches)
questionable_data = bovidae_geo[questionable_data_indices,]
questionable_data

# Plot the obviously problematic points
questionableDataPlot = ggplot() +
  geom_sf(data = st_as_sf(countriesLQ)) +
  geom_sf(data = questionable_data,
          col = "red") +
  facet_wrap(~species) +
  coord_sf(expand = FALSE) +
  theme(panel.background = element_rect(fill = "lightblue"))
ggsave("plots/occs_questionableData.jpg",
       questionableDataPlot,
       width = 12, height = 9,
       units = "in", dpi = 600)


# Remove the problem data
nrow(bovidae_geo)
## 32999
bovidae_clean = bovidae_geo[-questionable_data_indices,]
nrow(bovidae_clean)
## 26969

#### Cleaning as a function of year ----
bovidae_clean = bovidae_clean %>%
  filter(!is.na(year),
         year >= 1980)
nrow(bovidae_clean)
## 24353

#### Remove preserved, fossil specimens ----
bovidae_clean = bovidae_clean %>%
  filter(basisOfRecord != "PRESERVED_SPECIMEN",
         basisOfRecord != "FOSSIL_SPECIMEN")
nrow(bovidae_clean)
## 18494


## Preview
ggplot() +
  geom_sf(data = st_as_sf(NorthAmericaLQ)) +
  geom_sf(data = bovidae_clean %>%
            filter(genus == "Ovis"),
          aes(col = species)) +
  scale_color_manual(values = CJsBasics::KellyCols[5:20])


#### Final manual cleaning ----
# Remove additional problem data, using gbif ID:

## > Bighorn cleaning ----
# mapview(NorthAmericaLQ,
#         alpha.regions=0,
#         map.types = "OpenStreetMap") +
#   mapview(bovidae_clean %>% filter(genus == "Ovis",
#                                    specificEpithet == "canadensis"),
#           alpha.regions=1, pch = 1, color = "darkred")

# 922490545 - Denali (probably thinhorn)
# 1019041688 - Recorded at zoo in Lansing Michigan
bovidae_clean = bovidae_clean %>%
  filter(gbifID != 922490545,
         gbifID != 1019041688)

## Check result
# mapview(NorthAmericaLQ,
#         alpha.regions=0,
#         map.types = "OpenStreetMap") +
#   mapview(bovidae_clean %>% filter(species == "Ovis canadensis"), 
#           alpha.regions=0, color = "darkred")


## > Thinhorn cleaning ----
# mapview(NorthAmericaLQ,
#         alpha.regions=0,
#         map.types = "OpenStreetMap") +
#   mapview(bovidae_clean %>% filter(genus == "Ovis",
#                                    specificEpithet == "dalli"),
#           alpha.regions=0)
## Thinhorn occurrences look good


## > Mtn goat cleaning ----
# mapview(NorthAmericaLQ,
#         alpha.regions=0,
#         map.types = "OpenStreetMap") +
#   mapview(bovidae_clean %>% filter(species == "Oreamnos americanus"),
#           alpha.regions=0, color = "darkred")
## Mtn goat occurrences look good


## > Muskox cleaning ----
# mapview(NorthAmericaLQ,
#         alpha.regions=0,
#         map.types = "OpenStreetMap") +
#   mapview(bovidae_clean %>% filter(species == "Ovibos moschatus"),
#           alpha.regions=0, color = "darkred")
## Muskox occurrences look good


## > Bison cleaning ----
# mapview(NorthAmericaLQ,
#         alpha.regions=0,
#         map.types = "OpenStreetMap") +
#   mapview(bovidae_clean %>% filter(genus == "Bison",
#                                    specificEpithet == "bison"),
#           alpha.regions=0, color = "darkred")


# 2596125567 - Camp Pendleton, introduced
# 1850921137 - Camp Pendleton, introduced
# 2381410738 - Camp Pendleton, introduced
# 2269206648 - Camp Pendleton, introduced
# 3079682696 - Camp Pendleton, introduced
# 3415452210 - Camp Pendleton, introduced
# 3457071522 - Camp Pendleton, introduced
# 2631191303 - San Fransisco Zoo
# 2631191308 - San Fransisco Zoo
# 2631191306 - Golden Gate Park
# 1893583451 - Weird Mexico loc's
# 1893583408 - Weird Mexico loc's
# 3456432067 - Ranch near Santa Ysabel

bovidae_clean = bovidae_clean %>%
  filter(gbifID != 2596125567,
         gbifID != 1850921137,
         gbifID != 2381410738,
         gbifID != 2269206648,
         gbifID != 3079682696,
         gbifID != 3415452210,
         gbifID != 3457071522,
         gbifID != 2631191303,
         gbifID != 2631191308,
         gbifID != 2631191306,
         gbifID != 1893583451,
         gbifID != 1893583408,
         gbifID != 3456432067)

mapview(NorthAmericaLQ,
        alpha.regions=0,
        map.types = "OpenStreetMap") +
  mapview(bovidae_clean %>% filter(genus == "Bison",
                                   specificEpithet == "bison"),
          alpha.regions=0, color = "darkred")

#### Plot final distribution data ----
nrow(bovidae_clean)
## 18485


## Count number of obs per species
bovidae_clean %>%
  st_drop_geometry() %>%
  count(species)
# # A tibble: 5 × 2
# species                 n
# <chr>               <int>
#   1 Bison bison          5482
# 2 Oreamnos americanus  3648
# 3 Ovibos moschatus      438
# 4 Ovis canadensis      8228
# 5 Ovis dalli            689

bovidCleanPlot = ggplot() +
  geom_sf(data = st_as_sf(NorthAmericaLQ)) +
  geom_sf(data = bovidae_clean,
          aes(col = species)) +
  facet_wrap(~species) +
  coord_sf(expand = FALSE,
           crs = 5070) +
  theme(panel.background = element_rect(fill = "lightblue"))
ggsave("plots/occs_bovidOccurrences.jpg",
       bovidCleanPlot,
       width = 9, height = 9,
       units = "in", dpi = 600)

#### Generate an all ungulates dataset for bias ----
## Make an all ungulates dataset for the bias layer
allUngulates = data_geo %>%
  filter(basisOfRecord != "PRESERVED_SPECIMEN",
         basisOfRecord != "FOSSIL_SPECIMEN",
         !is.na(year),
         year >= 1980) %>%
  st_intersection(st_as_sf(NorthAmerica))

## Visualize
allUngulateSpPlot = ggplot() +
  geom_sf(data = st_as_sf(NorthAmericaLQ)) +
  geom_sf(data = allUngulates,
          aes(col = species),
          size = 0.01) +
  scale_color_manual(values = c(CJsBasics::KellyCols[2:5],
                                CJsBasics::KellyCols[7:20])) +
  coord_sf(expand = FALSE,
           crs = 5070) +
  theme(panel.background = element_rect(fill = "lightblue"))
ggsave("plots/occs_allUngulateSp.jpg",
       allUngulateSpPlot,
       width = 9, height = 9,
       units = "in", dpi = 600)

allUngulateOccPlot = ggplot() +
  geom_sf(data = st_as_sf(NorthAmericaLQ)) +
  geom_sf(data = allUngulates,
          col = "orange3",
          size = 0.01,
          alpha = 0.05) +
  coord_sf(expand = FALSE,
           crs = 5070) +
  theme(panel.background = element_rect(fill = "lightblue"))
ggsave("plots/occs_allUngulateOcc.jpg",
       allUngulateOccPlot,
       width = 9, height = 9,
       units = "in", dpi = 600)


#### Save outputs ----
## North America shapefile
# write_sf(st_as_sf(NorthAmericaLQ),
#          dsn = "./data/ROIs/NorthAmerica.shp",
#          layer = "NorthAmerica")

## Cleaned Bovidae occurrences
write_sf(bovidae_clean,
         dsn = "data/gbif/bovidae_clean.shp",
         layer = "bovidae_clean")
saveRDS(bovidae_clean,
        file = "data/gbif/bovidae_clean.RDS")

## All North America even-toed ungulate occurrences
write_sf(allUngulates,
         dsn = "data/gbif/all_ungulates.shp",
         layer = "all_ungulates")
saveRDS(allUngulates,
        file = "data/gbif/all_ungulates.RDS")


BRRR::skrrrahh(15)
