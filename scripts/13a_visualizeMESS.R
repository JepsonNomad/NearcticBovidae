library(terra)
library(metR)
library(tidyverse)
library(sf)

fps = list.files(path = "data/mess",
                 pattern = "*.tif",
                 full.names = TRUE)
for(i in fps){
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
    mutate(MESS = ifelse(MESS < 0, NA, MESS)) %>%
    ggplot() +
    geom_sf(data = rnaturalearth::ne_countries(continent = "North America",
                                               scale = 110,
                                               returnclass = "sf") %>%
              st_transform(crs(r)),
            fill = "grey40") +
    geom_raster(aes(x = x, y = y, fill = MESS)) +
    geom_sf(data = rnaturalearth::ne_countries(continent = "North America",
                                               scale = 110,
                                               returnclass = "sf") %>%
              st_transform(crs(r)),
            fill = "transparent") +
    ggtitle(sp) +
    xlab("") +
    ylab("") +
    scale_fill_viridis_c("MESS")
  ggsave(paste0("plots/mess/",
                bn,
                ".jpg"),
         rplt,
         width = 3, height = 3, units = "in", dpi = 600)
}
