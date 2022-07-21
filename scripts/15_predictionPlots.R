# Plotting present and future predictions
# 5 min
startTime = Sys.time()

#### Import packages ----
library(raster)
library(SDMtune)
# library(rgdal)
library(tidyverse)
library(RStoolbox)
library(sf)
library(cowplot)

#### Define parameters ----
albers = st_crs("+proj=aea +lat_1=20 +lat_2=60 +lat_0=40 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs")
## maxpixels argument for fortify()
fortRes = 8e5
gcmCount = 8

#### Load data ----
# Species list
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
speciesLookupTable = data.frame("Latin" = speciesList,
                                "Common" = speciesNamesList)
# Get North America shapefile
NorthAmerica <- rnaturalearth::ne_countries(continent = "North America",
                                            returnclass = "sf") %>%
  st_transform(albers) %>%
  st_crop(c(xmin = -4000000, 
            ymin = -3522576, 
            xmax = 3971637,
            ymax =  5477010))

# Get protected areas shapefile
protAreas = read_sf("PATH/TO/WDPA/NorthAmerica/WDPA_designated.shp") %>%
  st_crop(c(xmin = -4000000, 
            ymin = -3522576, 
            xmax = 3971637,
            ymax =  5477010))

## > Model data ----
maxentList = readRDS("models/sdm_10k/MaxEnt.RDS")
maxentList


## Presence path
path_to_rasts = "data/modeled_dist_10k"

## Future predictors 
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

####### Define a plotting function ----
# Arguments:
# x is a character: species name used in filepaths (e.g. "bighorn" or other 6-character name from earlier scripts)
# timePeriod should be "present" or "future" for modeled present or modeled 2070 distributions, respsectively
# modelNames should be a vector of length n where n is the number of layers in the output raster from 08a and 08b Predictions_timePeriod.R scripts, with each element being a string indicating model names in order
plotPreds = function(x = speciesList[1],
                     meList = maxentList,
                     path_to_rasts = "data/modeled_dist_10k"){
  mySp = x
  message(paste0("Species: ", mySp))
  speciesName = speciesLookupTable$Common[speciesLookupTable$Latin == mySp]
  ## Subset maxent list to the species element
  speciesME = meList[[mySp]]
  
  ## Generate predictions ----
  distPresent = raster::stack(paste0(path_to_rasts,
                                     "/Predicted_Present_",
                                     mySp,
                                     ".tif"))
  ## Identify thresholds
  ESS  = speciesME$Thresholds$`Cloglog value`[4]
  MSS  = speciesME$Thresholds$`Cloglog value`[5]
  # Equal Sens + Spec
  distPres_ESS = distPresent
  distPres_ESS[distPres_ESS >= ESS] <- 1
  distPres_ESS[distPres_ESS < ESS] <- NA
  names(distPres_ESS) <- "ESS"
  # Max Sens + Spec
  distPres_MSS = distPresent
  distPres_MSS[distPres_MSS >= MSS] <- 1
  distPres_MSS[distPres_MSS < MSS] <- NA
  names(distPres_MSS) <- "MSS"

  distPres_ESS_df = fortify(distPres_ESS, maxpixels = fortRes) %>%
    filter(!is.na(ESS))
  distPres_MSS_df = fortify(distPres_MSS, maxpixels = fortRes) %>%
    filter(!is.na(MSS))
  distPres_ANY_df = rbind(distPres_ESS_df[c("x","y")],
                          distPres_MSS_df[c("x","y")]) %>%
    filter(!duplicated(paste0(x,y)))
  
  # Generate thresholded columns
  presPlot = ggplot() +
    geom_sf(data = NorthAmerica, fill = "black") +
    geom_raster(data = distPres_ANY_df,
                aes(x = x, y = y),
                fill = "white") +
    geom_raster(data = distPres_ESS_df,
                aes(x = x, y = y,
                    fill = ESS),
                fill = "lightblue", alpha = 0.75) +
    geom_raster(data = distPres_MSS_df,
                aes(x = x, y = y,
                    fill = MSS),
                fill = "darkseagreen1", alpha = 0.75) +
    # geom_sf(data = protAreas,
    #         fill = "transparent",
    #         col = "#CCCCFF") +
    theme_void() +
    theme(legend.position = "none") +
    ggtitle(speciesName) +
    theme(plot.title = element_text(face = "bold",
                               hjust = 0.5),
          plot.margin = margin(0.05,2,2.5,2, unit = "cm"))
  # presPlot
  
  ## Make predictions - future
  futureFPs = list.files(path = path_to_rasts,
                         pattern = paste0("Predicted_Future_*"),
                         full.names = TRUE)
  futureSps = futureFPs[grep(mySp, futureFPs)]
  futureSps245 = futureSps[grep("ssp245",futureSps)]
  futureSps585 = futureSps[grep("ssp585",futureSps)]
  
  ssp45Stack = raster::stack(futureSps245)
  ssp45Stack[ssp45Stack >= MSS] <- 1
  ssp45Stack[ssp45Stack < MSS] <- NA
  names(ssp45Stack) <- gsub("Predicted_","",names(ssp45Stack))
  ssp85Stack = raster::stack(futureSps585)
  ssp85Stack[ssp85Stack >= MSS] <- 1
  ssp85Stack[ssp85Stack < MSS] <- NA
  names(ssp85Stack) <- gsub("Predicted_","",names(ssp85Stack))
  distModel_ssps = stack(ssp45Stack,
                         ssp85Stack)
  # Convert raster to data.frame()
  distModel_Futs = fortify(distModel_ssps, maxpixels = fortRes)
  # Generate thresholded columns
  distModel_Futs_arranged = distModel_Futs %>%
    pivot_longer(cols = c(-x, -y),
                 names_to = c("distModel","climateModel","sspScenario"),
                 names_sep = "_",
                 values_to = "Predicted") %>%
    select(-distModel) %>%
    mutate(sspScenario = ifelse(sspScenario == "ssp245",
                  "SSP2-4.5",
                  "SSP5-8.5"))
  distModel_Futs_cons = distModel_Futs_arranged %>%
    group_by(x, y, sspScenario) %>%
    summarize(Consensus = sum(Predicted,
                              na.rm = TRUE)) %>%
    mutate(Consensus = ifelse(Consensus == 0,
                              NA,
                              Consensus))
  ## Generate plots
  ## Consensus plot:
  fut1Plot = ggplot() +
    geom_sf(data = NorthAmerica,
            size = 0.1) +
    geom_raster(data = distModel_Futs_cons %>%
                  filter(sspScenario == "SSP2-4.5"),
                aes(x = x, y = y,
                    fill = Consensus)) +
    scale_fill_viridis_c("Projected\nconsensus",
                         limits = c(0,gcmCount),
                         na.value = "transparent") +
    geom_sf(data = NorthAmerica,
            fill = "transparent",
            col = "grey60",
            size = 0.1) +
    theme_void() +
    theme(strip.text.y = element_text(face = "bold",
                                      angle = 270),
          plot.margin = margin(0,0,0,0, unit = "cm"),
          rect = element_rect(fill = "transparent",
                              color = "transparent")) +
    annotate(geom = "text",
             x = -500000,
             y = -3000000,
             size = 10/2.8,
             hjust = 1,
             label = "bold('SSP2-4.5')",
             parse  = TRUE)
  fut2Plot = ggplot() +
    geom_sf(data = NorthAmerica,
            size = 0.1) +
    geom_raster(data = distModel_Futs_cons %>%
                  filter(sspScenario == "SSP5-8.5"),
                aes(x = x, y = y,
                    fill = Consensus)) +
    scale_fill_viridis_c("Projected\nconsensus",
                         limits = c(0,gcmCount),
                         na.value = "transparent") +
    geom_sf(data = NorthAmerica,
            fill = "transparent",
            col = "grey60",
            size = 0.1) +
    theme_void() +
    theme(strip.text.y = element_text(face = "bold",
                                      angle = 270),
          plot.margin = margin(0,0,0,0, unit = "cm"),
          rect = element_rect(fill = "transparent",
                              color = "transparent")) +
    theme(legend.position = "none") +
    annotate(geom = "text",
             x = -500000,
             y = -3000000,
             size = 10/2.8,
             hjust = 1,
             label = "bold('SSP5-8.5')",
             parse  = TRUE)
  # Extract legend from futsPlot
  futsPlotLeg = cowplot::get_legend(fut1Plot)
  # Remove legend from futsPlot
  fut1Plot = fut1Plot +
    theme(legend.position = "none")
  
  ## Compile plots
  # fullPred1 = cowplot::plot_grid(presPlot,
  #                               futsPlot,
  #                               nrow = 2, ncol = 1,
  #                               rel_heights = c(1,1))
  fullPred2 = cowplot::ggdraw() +
    draw_plot(presPlot,
              x = 0, y = 0, 
              width = 1, height = 1) +
    draw_plot(fut1Plot,
              x = 0, y = 0, 
              width = 0.5, height = 0.5) +
    draw_plot(fut2Plot,
              x = 0.5, y = 0, 
              width = 0.5, height = 0.5)
  ## Return output
  return(list(fullPred2,
              futsPlotLeg))
}


#### Apply the plotting function ----
x = speciesList[5]

myPlots = lapply(X = speciesList,
                 FUN = plotPreds,
                 path_to_rasts = path_to_rasts)

protArPlot = ggplot() +
  geom_sf(data = NorthAmerica) +
  geom_sf(data = protAreas,
          fill = "#7B353950",
          col = "#7B3539") +
  theme_void() +
  theme(legend.position = "none") +
  ggtitle("Protected areas") +
  theme(plot.title = element_text(face = "bold",
                                  hjust = 0.5),
        plot.margin = margin(0,0.5,0,0.5,unit = "cm"))

presLegPlot = data = data.frame("a" = c("Absent","ESS","MSS"),
                                "b" = c(4,5,6),
                                "c" = c(8,9,10)) %>%
  ggplot() +
  geom_point(shape = 22, size = 4, aes(x = b, y = c), 
             fill = "black", col = "grey40") +
  geom_point(shape = 22, size = 4, aes(x = b, y = c, fill = a), 
             alpha = 0.5) +
  scale_fill_manual("Current\npresence",
                    values = c("black","lightblue","darkseagreen1")) +
  theme_void()
presLeg = cowplot::get_legend(presLegPlot)
consLeg = myPlots[[1]][[2]]
legendsPlot = cowplot::plot_grid(presLeg,
                                 consLeg,
                                 nrow = 2,
                                 ncol = 1)
bottomRightPlot = cowplot::plot_grid(legendsPlot,
                                     protArPlot,
                                     nrow = 1,
                                     rel_widths = c(0.2,0.8)) +
  theme(plot.margin = margin(0.75,0,0.75,1, unit="cm"))

plot_List = list(myPlots[[1]][[1]],
                 myPlots[[2]][[1]],
                 myPlots[[3]][[1]],
                 myPlots[[4]][[1]],
                 myPlots[[5]][[1]],
                 bottomRightPlot)

#### Save outputs ----
distributions = cowplot::plot_grid(plotlist = plot_List,
                                   nrow = 2)
ggsave("plots/distribution_bovidae_commonNames.jpg",
       distributions,
       width = 25,
       height = 20,
       units = "cm",
       dpi = 600)

endTime = Sys.time()
print(endTime-startTime)

BRRR::skrrrahh(12)
