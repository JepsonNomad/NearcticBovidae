# Batch reprojecting using gdalwarp within python
# http://sainsb.github.io/2013/05/31/gdal-batch-reprojection/
# Run only once; this automatically iterates over all files within a folder

import os
import glob

os.chdir("data/worldclim_v2/bioclim/")
path = os.getcwd()

# Make a new folder to dump reprojected imagery
folder = 'reprojected'
if not os.path.exists(folder):
    os.makedirs(folder)

# Generate file list with absolute paths
listOfFiles = glob.glob(os.getcwd()+"/*.tif")
# listOfFiles

# Cutline file path
cutline = "../.././data/ROIs/NorthAmerica.shp"
# Reproject imagery, dump into new folder using same name as original data
for file in listOfFiles:
    if file.endswith(".tif"):
        filename = os.path.basename(file)
        fn, extn = os.path.splitext(filename)
        print(filename)
        os.system('gdalwarp'\
            + ' -t_srs "+proj=aea +lat_1=20 +lat_2=60 +lat_0=40 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs "'\
			+ ' -tr 6000 6000'\
            + ' -cutline ' + cutline\
            + ' -crop_to_cutline'\
            + ' -r bilinear'\
            + ' -of vrt'\
            + ' '+os.path.join(path,(fn+".tif"))\
            + ' '+os.path.join(path,folder,(fn+".vrt")))
        os.system('gdal_translate'\
            + ' -co compress=LZW'\
            + ' '+os.path.join(path,folder,(fn+".vrt"))\
            + ' '+os.path.join(path,folder,(fn+".tif")))



