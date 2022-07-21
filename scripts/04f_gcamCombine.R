## A script to combine and plot the fractional land cover data from GCAM demeter
## 2 min
startTime = Sys.time()

library(terra)
library(tidyverse)

#### Define functions ----
## Can use ssp as the scenario basis because ssp1 is 2015 starting point
## then ssp2 is ssp2-4.5 and ssp5 is ssp5-8.5 for 2100

gcamCombiner = function(scenario = "ssp2",
                        path = "data/GCAM/reformatted/reprojected/"){
  ## Find the files of interest
  fileList = list.files(path = path,
                        pattern = scenario,
                        full.names = TRUE)
  tifList = fileList[grep(x = fileList,pattern = ".tif")]
  ## Identify layer names
  layerList = c("NET_tem",
                "NET_bor",
                "NDT_bor",
                "BET_tro",
                "BET_tem", 
                "BDT_tro",
                "BDT_tem",
                "BDT_bor",
                "BES_tem",
                "BDS_tem",  
                "BDS_bor", 
                "C3_gra_arc",  
                "C3_gra",  
                "C4_gra", 
                "AGR",
                "Urban",
                "Barren")
  ## Import a template raster
  baseRast = rast(tifList[1])
  names(baseRast) <- layerList
  ## Generate an empty output raster object
  aggRas = rast(ncol=ncol(baseRast), 
                nrow=nrow(baseRast), 
                xmin=ext(baseRast)$xmin, 
                xmax=ext(baseRast)$xmax, 
                ymin=ext(baseRast)$ymin,
                ymax=ext(baseRast)$ymax,
                nlyr=length(layerList))
  # Find the layer-wise mean across cmip scenarios and add to aggRas
  for(i in 1:nlyr(baseRast)){
    inRastList = lapply(tifList,
                        FUN = function(x){
                          rast(x,lyr=i)
                        })
    outRast = rast(inRastList) %>%
      app(mean, na.rm = T)
    names(outRast) <- layerList[i]
    # plot(outRast)
    aggRas[[i]] <- outRast
  }
  # Check the result
  plot(aggRas)
  # Save the output
  writeRaster(x = aggRas,
              filename = paste0("data/GCAM/aggregated/",
                                scenario,
                                ".tif"),
              overwrite = TRUE)
}

for(i in c("ssp1","ssp2","ssp5")){
  gcamCombiner(scenario = i)
}

stopTime = Sys.time()
print(stopTime-startTime)
BRRR::skrrrahh(12)