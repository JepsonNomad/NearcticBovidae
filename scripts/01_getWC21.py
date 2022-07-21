import requests, zipfile, io
import os

os.chdir("data/worldclim_v2")

#myPaths = [
#"http://biogeo.ucdavis.edu/data/worldclim/v2.1/fut/2.5m/wc2.1_2.5m_bioc_CNRM-CM6-1_ssp245_2081-2100.zip",
#"http://biogeo.ucdavis.edu/data/worldclim/v2.1/fut/2.5m/wc2.1_2.5m_bioc_CNRM-CM6-1_ssp585_2081-2100.zip",
#"http://biogeo.ucdavis.edu/data/worldclim/v2.1/fut/2.5m/wc2.1_2.5m_bioc_CNRM-ESM2-1_ssp245_2081-2100.zip",
#"http://biogeo.ucdavis.edu/data/worldclim/v2.1/fut/2.5m/wc2.1_2.5m_bioc_CNRM-ESM2-1_ssp585_2081-2100.zip",
#"http://biogeo.ucdavis.edu/data/worldclim/v2.1/fut/2.5m/wc2.1_2.5m_bioc_CanESM5_ssp245_2081-2100.zip",
#"http://biogeo.ucdavis.edu/data/worldclim/v2.1/fut/2.5m/wc2.1_2.5m_bioc_CanESM5_ssp585_2081-2100.zip",
#"http://biogeo.ucdavis.edu/data/worldclim/v2.1/fut/2.5m/wc2.1_2.5m_bioc_MIROC6_ssp245_2081-2100.zip",
#"http://biogeo.ucdavis.edu/data/worldclim/v2.1/fut/2.5m/wc2.1_2.5m_bioc_MIROC6_ssp585_2081-2100.zip"]


# for i in myPaths:
i = "http://biogeo.ucdavis.edu/data/worldclim/v2.1/fut/2.5m/wc2.1_2.5m_bioc_MRI-ESM2-0_ssp585_2081-2100.zip"
r = requests.get(i)
z = zipfile.ZipFile(io.BytesIO(r.content))
z.extractall()
