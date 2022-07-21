# Measuring overlap between modeled ranges and protected areas
# ~5 min

#### Import packages ----
library(raster)
library(rgdal)
library(tidyverse)
library(kableExtra)

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
speciesLookupTable = data.frame("Species" = speciesList,
                                "Common" = speciesNamesList)

#### Identify data ----
## Raster datasets
presentFPs = list.files(path = "data/modeled_dist_10k/",
                        pattern = "Present", full.names = TRUE)
futureFPs =  list.files(path = "data/modeled_dist_10k/",
                        pattern = "Future", full.names = TRUE)

#### Import data ----
## Raster datasets
presentRas = lapply(presentFPs,
                    raster)
futureRas = lapply(futureFPs,
                   raster)

## Vector dataset
ROI = readOGR("PATH/TO/WDPA/NorthAmerica/",
              "WDPA_designated")

## SDM data
sdmData = readRDS("models/sdm_10k/MaxEnt.RDS")

#### Data wrangling ----
## Identify layer names of presentRas dataset
presentBasenames = basename(presentFPs)
presentSp = sapply(str_split(presentBasenames, 
                             pattern = "_"),
                   "[",
                   3) %>%
  gsub(pattern = ".tif", replacement = "")
names(presentRas) <- presentSp

## Identify layer names and SSP and GCM of futureRas dataset
futureBasenames = basename(futureFPs)
futureSp = sapply(str_split(futureBasenames, 
                            pattern = "_"),
                  "[",
                  5) %>%
  gsub(pattern = ".tif", replacement = "")
futureSSP = sapply(str_split(futureBasenames, 
                             pattern = "_"),
                   "[",
                   4) %>%
  gsub(pattern = "ssp", replacement = "")
futureGCM = sapply(str_split(futureBasenames, 
                             pattern = "_"),
                   "[",
                   3)
futureNames = paste0(futureSp, "_",
                     futureGCM, "_",
                     futureSSP)
names(futureRas) <- futureNames


## Convert ROI to raster type
ROI_ras = fasterize::fasterize(ROI %>%
                                 sf::st_as_sf(),
                               raster = presentRas[[1]])
plot(ROI_ras)
names(ROI_ras) <- "Protected"

## For present and future rasters, binarize using the Maxent thresholds
for(i in speciesList){
  maxEntThresh = sdmData[[i]]$Thresholds[5,2]
  ## Replace present values
  presentRas[[i]][presentRas[[i]] < maxEntThresh] <- 0
  presentRas[[i]][presentRas[[i]] >= maxEntThresh] <- 1
  ## Identify future layers to work on
  futRastIndices = grep(x = names(futureRas), pattern = i)
  for(j in futRastIndices){
    futureRas[[j]][futureRas[[j]] < maxEntThresh] <- 0
    futureRas[[j]][futureRas[[j]] >= maxEntThresh] <- 1
    futureRas[[j]][futureRas[[j]] < maxEntThresh] <- 0
    futureRas[[j]][futureRas[[j]] >= maxEntThresh] <- 1
  }
}


## Calculate overlap area for each raster
plot(presentRas[[5]])
lines(ROI)

plot(futureRas[[15]])
lines(ROI)

## A function to calculate the number of pixels of overlap and convert to area in square kilometers based on x and y resolutions defined as r1 and r2. Works for present prediction data
calcROIoverlapPresent = function(x,
                                 p = ROI_ras,
                                 r1 = 6,
                                 r2 = 6){
  crosstab(x, p, long = TRUE) %>%
    mutate(areaKm2 = r1*r2*Freq) %>%
    mutate(Species = names(.)[1]) %>%
    rename(Predicted_Present = 1) %>%
    separate(Species, into = c("Pred","Time","Species1", "Species2")) %>%
    mutate(Species = paste0(Species1, " ", Species2)) %>%
    select(-Pred, -Species1, -Species2)
}
presentOverlap.list = lapply(X = presentRas,
                             FUN = calcROIoverlapPresent)
presentOverlap.df = do.call("rbind",
                            presentOverlap.list)

## A function to calculate the number of pixels of overlap and convert to area in square kilometers based on x and y resolutions defined as r1 and r2. Works for future prediction data
calcROIoverlapFuture = function(x,
                                p = ROI_ras,
                                r1 = 6,
                                r2 = 6){
  crosstab(x, p, long = TRUE) %>%
    mutate(areaKm2 = r1*r2*Freq) %>%
    mutate(Species = names(.)[1]) %>%
    rename(Predicted_Present = 1) %>%
    separate(Species,
             into = c("Pred","Time","GCM","SSP","Species1","Species2"),
             sep = "_") %>%
    mutate(Species = paste0(Species1, " ", Species2)) %>%
    select(-Species1, -Species2, -Pred)
}
futureOverlap.list = lapply(X = futureRas,
                            FUN = calcROIoverlapFuture)
futureOverlap.df = do.call("rbind",
                           futureOverlap.list)

## Join area datasets
present_forJoin = presentOverlap.df %>%
  filter(Predicted_Present == 1) %>%
  mutate(GCM = "Hist",
         SSP = "Hist")
future_forJoin = futureOverlap.df %>%
  filter(Predicted_Present == 1)

mydata = bind_rows(present_forJoin,
                   future_forJoin) %>%
  left_join(speciesLookupTable, by = "Species") %>%
  mutate(SpeciesFac = factor(Common,
                             levels = speciesNamesList)) %>%
  mutate(SSP = ifelse(SSP == "ssp245",
                      "SSP2-4.5",
                      "SSP5-8.5"))

currProt = mydata %>%
  filter(Time == "Present") %>%
  group_by(Species) %>%
  summarize("Current" = areaKm2/1e6)

projProt = mydata %>%
  filter(Time != "Present") %>%
  group_by(Species, SSP) %>%
  summarize("mean" = mean(areaKm2/1e6),
            "se" = plotrix::std.error(areaKm2/1e6)) %>%
  pivot_wider(names_from = SSP,
              values_from = c(mean, se)) %>%
  mutate("SSP2-4.5" = paste0(sprintf("%.2f",`mean_SSP2-4.5`), 
                             " ± ", 
                             sprintf("%.2f",`se_SSP2-4.5`)),
         "SSP5-8.5" = paste0(sprintf("%.2f",`mean_SSP5-8.5`),
                             " ± ", 
                             sprintf("%.2f",`se_SSP5-8.5`))) %>%
  select(-`mean_SSP2-4.5`,
         -`se_SSP2-4.5`,
         -`mean_SSP5-8.5`,
         -`se_SSP5-8.5`)

## Measuring the degree of change across species and SSP's
protectedTable = full_join(currProt,
                           projProt)[c(2,5,3,4,1),]
protectedTable %>%
  separate("SSP2-4.5", into = c("mean2","se2"),
           sep=" ± ") %>%
  mutate("dSSP2" = 1-as.numeric(mean2)/(as.numeric(Current))) %>%
  separate("SSP5-8.5", into = c("mean5","se5"),
           sep=" ± ") %>%
  mutate("dSSP5" = 1-as.numeric(mean5)/(as.numeric(Current))) %>%
  mutate("scenarioDiff" = dSSP5-dSSP2)

## Save summary table
protectedTable %>%
  mutate(Current = round(Current, digits = 3)) %>%
  kable(table.attr = "style='width:40%;'") %>%
  kable_classic() %>%
  save_kable(file = "tables/protectedAreaChange.png",
             zoom = 4)



#### Plot results ----
areaComp = ggplot() +
  geom_boxplot(data = mydata %>%
                 filter(Time == "Future"),
               aes(x = SpeciesFac, y = areaKm2/10^6,
                   group = paste0(Species, SSP),
                   col = SSP),
               outlier.size = 1) +
  geom_point(data = mydata %>%
               filter(Time == "Present"),
             aes(x = SpeciesFac, y = areaKm2/10^6),
             shape = 15, size = 2) +
  scale_color_manual("SSP", values = CJsBasics::KellyCols[c(4,8)]) +
  ylab(expression(paste("Protected area (millions ",km^2,")"))) +
  xlab("") +
  CJsBasics::BasicTheme +
  theme(axis.text.x = element_text(angle = 45,
                                   hjust = 1))

#### Save outputs ----
ggsave(filename = "plots/protectedArea.jpg",
       areaComp,
       width = 6,
       height = 4,
       units = "in",
       dpi = 300)
