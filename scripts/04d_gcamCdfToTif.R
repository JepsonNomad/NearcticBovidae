###### Reformatting GCAM datasets from netcdf to tif with compressed layers
#### This takes a long time, run it once and be done with it.
#### ONCE THIS IS DONE, DO NOT REPEAT!!

library(raster)
library(tidyverse)

#### Define functions ----
## A function to reogranize a single netcdf raster stack
# See also https://cran.r-project.org/web/packages/futureheatwaves/vignettes/starting_from_netcdf.html for dealing with netcdf beast
netCDF_org = function(x){
  message(x)
  # Generate output filename
  x_basename = basename(x)
  x_fn = tools::file_path_sans_ext(x_basename)
  outFolder = "data/GCAM/reformatted/"
  if(!dir.exists(outFolder)){
    dir.create(outFolder)
  }
  outname = paste0(outFolder,
                   x_fn,
                   ".tif")
  # Raster flips coords for whatever reason, so rectify here.
  impFlip = function(x, PFT){
    r = raster(x, varname = PFT)
    r =  flip(t(r), direction = "x")
    proj4string(r) = "+init=epsg:4326"
    return(r)
  }
  PFT0 = impFlip(x, PFT = "PFT0")
  PFT1 = impFlip(x, PFT = "PFT1")
  PFT2 = impFlip(x, PFT = "PFT2")
  PFT3 = impFlip(x, PFT = "PFT3")
  PFT4 = impFlip(x, PFT = "PFT4")
  PFT5 = impFlip(x, PFT = "PFT5")
  PFT6 = impFlip(x, PFT = "PFT6")
  PFT7 = impFlip(x, PFT = "PFT7")
  PFT8 = impFlip(x, PFT = "PFT8")
  PFT9 = impFlip(x, PFT = "PFT9")
  PFT10 = impFlip(x, PFT = "PFT10")
  PFT11 = impFlip(x, PFT = "PFT11")
  PFT12 = impFlip(x, PFT = "PFT12")
  PFT13 = impFlip(x, PFT = "PFT13")
  PFT14 = impFlip(x, PFT = "PFT14")
  PFT15 = impFlip(x, PFT = "PFT15")
  PFT16 = impFlip(x, PFT = "PFT16")
  PFT17 = impFlip(x, PFT = "PFT17")
  PFT18 = impFlip(x, PFT = "PFT18")
  PFT19 = impFlip(x, PFT = "PFT19")
  PFT20 = impFlip(x, PFT = "PFT20")
  PFT21 = impFlip(x, PFT = "PFT21")
  PFT22 = impFlip(x, PFT = "PFT22")
  PFT23 = impFlip(x, PFT = "PFT23")
  PFT24 = impFlip(x, PFT = "PFT24")
  PFT25 = impFlip(x, PFT = "PFT25")
  PFT26 = impFlip(x, PFT = "PFT26")
  PFT27 = impFlip(x, PFT = "PFT27")
  PFT28 = impFlip(x, PFT = "PFT28")
  PFT29 = impFlip(x, PFT = "PFT29")
  PFT30 = impFlip(x, PFT = "PFT30")
  PFT31 = impFlip(x, PFT = "PFT31")
  PFT32 = impFlip(x, PFT = "PFT32")
  
  # See also https://www.nature.com/articles/s41597-020-00669-x/tables/3
  # for layer info
  NET_tem = PFT1
  NET_bor = PFT2
  NDT_bor = PFT3
  BET_tro = PFT4
  BET_tem = PFT5 
  BDT_tro = PFT6 
  BDT_tem = PFT7
  BDT_bor	= PFT8
  BES_tem = PFT9
  BDS_tem = PFT10 
  BDS_bor = PFT11
  C3_gra_arc = PFT12 
  C3_gra = PFT13 
  C4_gra = PFT14
  AGR =  PFT15 +  # Rainfed
    PFT17 + 
    PFT19 + 
    PFT21 + 
    PFT23 + 
    PFT25 + 
    PFT27 + 
    PFT29 +
    PFT16 + # Irrigated
    PFT18 + 
    PFT20 + 
    PFT22 + 
    PFT24 + 
    PFT26 + 
    PFT28 + 
    PFT30
  Urban = PFT31
  Barren = PFT32
  
  ras = raster::stack(list(NET_tem,
                           NET_bor,
                           NDT_bor,
                           BET_tro,
                           BET_tem, 
                           BDT_tro, 
                           BDT_tem,
                           BDT_bor,
                           BES_tem,
                           BDS_tem,  
                           BDS_bor, 
                           C3_gra_arc,  
                           C3_gra ,  
                           C4_gra , 
                           AGR,
                           Urban,
                           Barren))
  # Plot to verify
  par(mfrow = c(2,2))
  image(ras[[1]], main = "Temperate needle leaf evergrn")
  image(ras[[2]], main = "Boreal needle leaf evergrn")
  image(ras[[12]], main = "C3 Arctic grasses")
  image(ras[[13]], main = "C3 grasses")
  
  writeRaster(x = ras,
              filename = outname,
              overwrite = TRUE)
}



#### Import data ----
## GCAM
gdPath = "data/GCAM"
gdFilesAll = list.files(path = gdPath,
                        recursive = TRUE,
                        pattern = "*.nc",
                        full.names = TRUE)
my.gcamList = lapply(X = gdFilesAll,
                     FUN = netCDF_org)

BRRR::skrrrahh(12)
