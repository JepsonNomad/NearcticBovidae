#### Creating a bias layer for maxent ----
# 6 May 2020
# Christian John

startTime = Sys.time()
## Takes about 6.25 minutes

# Designed with reference to Scott Rinnan's post
# https://scottrinnan.wordpress.com/2015/08/31/how-to-construct-a-bias-file-with-r-for-use-in-maxent-modeling/
# But using terra

#### Load packages ----
library(terra)
library(sf)
library(MASS)

#### Import data ----
## Region of interest 
NorthAmerica = rnaturalearth::ne_countries(scale = 10,
                                           continent = "North America")
## Occurrence data (all N Am artiodactyla)
occurrences = read_sf("data/gbif/all_ungulates.shp")
## Prediction raster
template_rast = rast("data/worldclim_v2/bioclim/reprojected/biocl_centered.tif",
                     lyrs = 1)
plot(template_rast)


#### Data wrangling ----
## Create vect object from occurrences
occ_vect = vect(st_transform(occurrences,
                             st_crs(template_rast)))
plot(occ_vect)
rm(occurrences)

#### Generate bias raster ----
## First, generate rasterized version of occurrences at the scale 
## of the predictor data
occ_rast = rasterize(occ_vect, template_rast, 1)
plot(occ_rast)
rm(occ_vect)

## Next, identify which raster cells had at least 1 occurrence
cells_present <- which(values(occ_rast) == 1)
## And extract the location of the cell centroids
locs_present <- xyFromCell(occ_rast, cells_present)

## Apply the 2d kernel density estimator from MASS
?kde2d
kde <- kde2d(locs_present[,"x"], locs_present[,"y"], 
             n = c(nrow(occ_rast), 
                   ncol(occ_rast)))
## Preview the result
image(kde)

## Finally, convert kernel density output to a raster
## For whatever reason, terra won't play nice with the kde output
## https://github.com/rspatial/terra/issues/630
## Two options are f1: transpose matrix, or f2: expand grid
f1 = function(l,scal){
  mat = l$z
  mat = apply(t(mat), 2, rev) # or mat = t(mat[, ncol(mat):1L])
  r = terra::rast(mat*scal)
  # plot(r3)
  return(r)
}
f2 = function(l,scal){
  df = expand.grid(x = l$x, y = l$y, KEEP.OUT.ATTRS = FALSE)
  ## Multiply by a constant to deal with terra's issue with tiny values
  df$z = as.vector(l$z)*(scal)
  r = rast(df)
  # plot(kde_rast)
  return(r)
}
# rbenchmark::benchmark(f1(kde,1e11),replications = 10)
# rbenchmark::benchmark(f2(kde,1e11),replications = 10)
## Expand grid takes a lot longer but you get to keep your spatial information

kde_rast = f2(kde,1e11)
## Check and asssign spatial info as needed
crs(kde_rast) <- crs(template_rast)

## Resample to match predictors data
kde_rast = resample(kde_rast, template_rast, "bilinear")
plot(kde_rast)
plot(template_rast)

#### Save outputs ----
terra::writeRaster(kde_rast,
                   filename="data/gbif/biasGrid.tif",
                   overwrite= TRUE)

r = terra::rast("data/gbif/biasGrid.tif")
plot(r)
r

stopTime = Sys.time()
print(stopTime-startTime)
BRRR::skrrrahh(14)