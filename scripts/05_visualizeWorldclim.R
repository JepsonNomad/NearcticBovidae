#### Previewing predictor data
startTime = Sys.time()


#### Define parameters ----
## Degree of aggregation for bioclimatic layers
aggFactor = 4 


#### Load packages ----
library(terra)
library(tidyverse)
library(cowplot)


#### Import data ----
## Projected data
cmip_models = c("BCC-CSM2-MR",
                "CanESM5",
                "CNRM-CM6-1",
                "CNRM-ESM2-1",
                "IPSL-CM6A-LR",
                "MIROC-ES2L",
                "MIROC6",
                "MRI-ESM2-0")
cmip_scenarios = c("ssp245","ssp585")
cmip_products = apply(expand.grid(cmip_models, cmip_scenarios), 
                      1, paste, collapse="/")

## Create a list of filepaths, with each element of the list corresponding to a cmip product
cmip_fileList = lapply(X = paste0("data/worldclim_v2/cmip6/",
                                  cmip_products,"/reprojected"),
                       FUN = function(x){
                         p <- list.files(path = x,
                                         pattern = ".tif",
                                    full.names = TRUE)
                         
                         p = p[grep(pattern = "wc2", 
                                    x = p)]
                       })

cmip_list = lapply(X = cmip_fileList,
                   FUN = function(s){
                     terra::rast(s) %>%
                       aggregate(fact = aggFactor)
                   })

## Historical data
hist_fileList = list.files("data/worldclim_v2/bioclim/reprojected",
                           full.names = TRUE)
hist_fileList = hist_fileList[grep(pattern = "wc2", 
                                  x = hist_fileList)]
hist_fileRas = hist_fileList[grep(pattern = ".tif", 
                                  x = hist_fileList)]
hist = terra::rast(hist_fileRas) %>%
  aggregate(fact = aggFactor)
names(hist)


#### Data wrangling ----
## Layer names
# CMIP
cmip_namesRaw = lapply(cmip_list,
                       names)
cmip_layerNums = lapply(cmip_namesRaw,
                        function(n){
                          o <- sapply(str_split(n, "_"),
                                      "[",
                                      7)
                          q <- str_pad(o,
                                       width = 2,
                                       side = "left",
                                       pad = "0")
                          return(q)
                        })
## Rename layers
for(i in 1:length(cmip_list)){
  names(cmip_list[[i]]) <- paste0("Biocl_",cmip_layerNums[[i]])
  
}

# Historical
hist_namesRaw = names(hist)
hist_layerNums = sapply(str_split(hist_namesRaw, "_"),
                        "[",
                        4) %>%
  str_pad(width = 2,
          side = "left",
          pad = "0")
names(hist) <- paste0("Biocl_",hist_layerNums)
# Reorder hist to match biocl layer order
hist <- hist[[order(names(hist))]]

## Fortify rasters
## Convert cmip bioclim dataset to data.frame() and add product model and scenario columns
cmip_df_list <- lapply(cmip_list,
                       function(s){
                         as.data.frame(s, xy = TRUE)
                       })

for(i in 1:length(cmip_df_list)){
  cmip_df_list[[i]]$product <- cmip_products[i]
  cmip_df_list[[i]]$model <- str_split(cmip_products[i], "/")[[1]][1]
  cmip_df_list[[i]]$scenario <- str_split(cmip_products[i], "/")[[1]][2]
}
cmip_df <- do.call("rbind",
                   cmip_df_list)
head(cmip_df)


## Convert historic worldclim dataset to data.frame() and add product model and scenario columns
hist_df <- as.data.frame(hist, xy = TRUE)
hist_df$product = "hist"
hist_df$model = "worldclim"
hist_df$scenario = "hist"
# Look at the formatted data
head(cmip_df)
head(hist_df)
## Join datasets
mydata = rbind(cmip_df,
               hist_df)

# Pivot to tidy format. Start by removing rows where all Biocl values are NA
mydata_naRem = mydata[rowSums(is.na(mydata))<19,]
mydata_long = mydata_naRem %>%
  select(-product) %>%
  pivot_longer(cols = c(-x, -y, -model, - scenario),
               names_to = "layer",
               values_to = "value")

#### Plotting ----
## A function to plot a single biocl layer across products
myPlotFunction = function(myLayer){
  pltMin = min(mydata_long %>%
    filter(layer == myLayer) %>%
      pull(value),
    na.rm = T)
  pltMax = max(mydata_long %>%
                 filter(layer == myLayer) %>%
                 pull(value),
               na.rm = T)
  plt1 <- mydata_long %>%
    filter(layer == myLayer,
           scenario == "hist") %>%
    ggplot() +
    geom_raster(aes(x = x,
                    y = y,
                    fill = value)) +
    facet_grid(scenario~model) +
    scale_fill_viridis_c(limits = c(pltMin, pltMax)) +
    coord_equal() +
    ggtitle(myLayer) +
    theme(legend.position = "none")
  plt2 <- mydata_long %>%
    filter(layer == myLayer,
           scenario != "hist") %>%
    ggplot() +
    geom_raster(aes(x = x,
                    y = y,
                    fill = value)) +
    facet_grid(scenario~model) +
    scale_fill_viridis_c(limits = c(pltMin, pltMax)) +
    coord_equal() +
    ggtitle(myLayer) +
    xlab("") +
    ylab("") +
    theme(axis.text = element_blank())
  plt3 <- plot_grid(plt1, plt2,
                    nrow = 1,
                    ncol = 2,
                    rel_widths = c(1,1.2),
                    align = "hv",
                    axis = "tblr")
  return(plt3)
}

## Get the unique layer names in order
myLayerList = unique(mydata_long$layer) %>%
  sort()

## Apply plotting function across unique layers
myPlotList = lapply(X = myLayerList,
                    FUN = myPlotFunction)

## Check it out
# myPlotList[[1]]
# myPlotList[[10]]

# Compile plots
myCowPlot = cowplot::plot_grid(plotlist = myPlotList,
                               ncol = 3)
# myCowPlot


#### Save outputs ----
ggsave("plots/BioClimLayers.jpg",
       plot = myCowPlot,
       width = 40, height = 40, units = "in", dpi = 300,
       limitsize = FALSE)

stopTime = Sys.time()
message(stopTime-startTime)
BRRR::skrrrahh(4)
## Took 1.75 min