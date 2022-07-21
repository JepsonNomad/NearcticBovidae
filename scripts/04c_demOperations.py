# Batch reprojecting using gdalwarp within python
# http://sainsb.github.io/2013/05/31/gdal-batch-reprojection/
# Repeat only once

import os
import glob

os.chdir("data/")
path = os.getcwd()

# Make a new folder to dump reprojected imagery
folder = 'reprojected_dem'
if not os.path.exists(folder):
    os.makedirs(folder)

# Cutline file path
cutline = "ROIs/NorthAmerica.shp"

## Calculate TRI before aggregating
os.system('gdaldem tri'\
	+ ' PATH/TO/NA_Elevation/w001001.adf'\
	+ ' '+os.path.join(path,folder,("NA_TRI_raw.tif")))
# Reproject TRI
os.system('gdalwarp'\
    + ' -s_srs "+proj=laea +lat_0=45 +lon_0=-100 +x_0=0 +y_0=0 +a=6370997 +b=6370997 +units=m +no_defs"'\
    + ' -t_srs "+proj=aea +lat_1=20 +lat_2=60 +lat_0=40 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"'\
    + ' -r bilinear'\
    + ' -cutline ' + cutline\
    + ' -crop_to_cutline'\
    + ' -tr 6000 6000'\
    + ' -of vrt'\
	+ ' '+os.path.join(path,folder,("NA_TRI_raw.tif"))\
    + ' '+os.path.join(path,folder,("NA_TRI_alb.vrt")))
os.system('gdal_translate'\
    + ' -co compress=LZW'\
    + ' '+os.path.join(path,folder,("NA_TRI_alb.vrt"))\
    + ' '+os.path.join(path,folder,("NA_TRI_alb.tif")))

# Reproject DEM, dump into new folder using updated name.
# Force source srs due to discussion here:
# https://gis.stackexchange.com/questions/291256/reprojecting-raster-between-laea-and-lon-lat-alignment-issues#291260
# Begin by converting to longlat so that "north facing" interpretations makes sense
os.system('gdalwarp'\
    + ' -s_srs "+proj=laea +lat_0=45 +lon_0=-100 +x_0=0 +y_0=0 +a=6370997 +b=6370997 +units=m +no_defs"'\
    + ' -t_srs "+proj=aea +lat_1=20 +lat_2=60 +lat_0=40 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"'\
    + ' -r bilinear'\
    + ' -cutline ' + cutline\
    + ' -crop_to_cutline'\
    + ' -tr 6000 6000'\
    + ' -of vrt'\
    + ' /PATH/TO/NA_Elevation/w001001.adf'\
    + ' '+os.path.join(path,folder,("NA_DEM_alb.vrt")))
os.system('gdal_translate'\
    + ' -co compress=LZW'\
    + ' '+os.path.join(path,folder,("NA_DEM_alb.vrt"))\
    + ' '+os.path.join(path,folder,("NA_DEM_alb.tif")))

# Combine the datasets
os.system('gdal_merge.py'\
	+ ' -separate'\
	+ ' -o '+os.path.join(path,folder,("NA_terrain_alb.tif"))\
	+ ' '+os.path.join(path,folder,("NA_DEM_alb.tif"))\
	+ ' '+os.path.join(path,folder,("NA_TRI_alb.tif")))





