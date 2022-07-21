library(terra)
library(metR)
library(tidyverse)
library(sf)
library(cowplot)

fps = list.files(path = "data/mess",
                 pattern = "*.tif",
                 full.names = TRUE)
fps_futures = fps[grep("messFuture",fps)]
fps_cmips = sapply(str_split(fps_futures,"_"),
                   "[[",
                   2)
fps_ssps = sapply(str_split(fps_futures,"_"),
                  "[[",
                  3)
fps_spps = gsub(".tif",
                "",
                sapply(str_split(fps_futures,"_"),
                   "[[",
                   4))

sppSspCmipPlotter = function(i){
  bn = gsub(x = basename(i),
            pattern = ".tif",
            replacement="")
  bnSplit = str_split(bn, "_")[[1]]
  sp = ifelse(length(bnSplit) == 4,
              bnSplit[4],
              bnSplit[2])
  r = rast(i)
  rdf = as.data.frame(r, xy = TRUE)
  names(rdf) <- c("x","y","MESS")
  rplt = rdf %>%
    mutate(MESSfact = as.factor(ifelse(MESS < 0, -1, 
                                       ifelse(MESS > 0,
                                              1,
                                              0)))) %>%
    ggplot() +
    geom_sf(data = rnaturalearth::ne_countries(continent = "North America",
                                               scale = 110,
                                               returnclass = "sf") %>%
              st_transform(crs(r)),
            fill = "grey40") +
    geom_raster(aes(x = x, y = y, fill = MESSfact)) +
    geom_sf(data = rnaturalearth::ne_countries(continent = "North America",
                                               scale = 110,
                                               returnclass = "sf") %>%
              st_transform(crs(r)),
            fill = "transparent",
            size = 0.5) +
    ggtitle(sp) +
    xlab("") +
    ylab("") +
    scale_fill_manual("MESS", 
                      values = c("darksalmon","white","steelblue1")) +
    theme(legend.position = "none",
          plot.title = element_text(size = 6),
          axis.text = element_blank())
  return(rplt)
}

# Generate plots for eacch species/cmip/ssp
sppPlts = lapply(fps_futures,
                 sppSspCmipPlotter)

# Create a function to compile plots by cmip and ssp
cmipSSPcompiler = function(cmip,ssp){
  myPlts = sppPlts[fps_cmips == cmip & fps_ssps == ssp]
  plot_grid(plotlist = myPlts) +
    ggtitle(paste0(cmip, " ", ssp))
}


for(i in unique(fps_cmips)){
  for(j in unique(fps_ssps)){
    myPlotGrid = cmipSSPcompiler(i,j)
    ggsave(filename = paste0("plots/mess/compiled_",
                             i,"_",j,".jpg"),
           myPlotGrid,
           width = 5, height = 3, units = "in", dpi = 300)
  }
}
