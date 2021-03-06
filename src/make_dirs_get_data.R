library(tidyverse)
library(assertthat)
library(rvest)
library(httr)
library(purrr)
library(sf)
library(raster)
library(rgdal)
library(rasterVis)
library(velox)
library(lwgeom)
library(pbapply)
library(rgeos)
library(zoo)
library(ncdf4)
library(parallel)
library(lubridate)
library(mblm)
library(gridExtra)

source('src/functions.R')

p4string_latlong <- "+proj=longlat +datum=WGS84 +no_defs"
p4string_ea <- '+proj=laea +lat_0=45 +lon_0=-100 +x_0=0 +y_0=0 +a=6370997 +b=6370997 +units=m +no_defs'

prefix <- file.path("data")
forests_dir <- file.path(prefix, 'forests')
raw_prefix <- file.path(prefix, "raw")
us_prefix <- file.path(raw_prefix, "cb_2016_us_state_20m")
site_prefix <- file.path(raw_prefix, "neon_site")
mtbs_prefix <- file.path(raw_prefix, "mtbs_fod_perimeter_data")
forest_prefix <- file.path(raw_prefix, "conus_forestgroup")
raw_baecv_dir <- file.path(raw_prefix, 'baecv')
cd_dir <- file.path(raw_prefix, 'nm_cont_div')
fire_dir <- file.path(prefix, 'fire')
buffered_mtbs <- file.path(fire_dir, 'buffered_mtbs')
domain_prefix <- file.path(raw_prefix, 'NEONDomains_0')
baecv_dir <- file.path(fire_dir, 'baevc')

tmmn_dir <- file.path('data', 'climate', 'tmmn')
tmax_dir <- file.path('data', 'climate', 'tmmx')
temp_mean_dir <- file.path('data', 'climate', 'tmean', 'monthly_mean')

temp_anomalies_dir <- file.path('data', 'climate', 'tmean', 'monthly_anomalies')
pdsi_mean_dir <- file.path('data', 'climate', 'pdsi', 'monthly_mean')
pdsi_anomalies_dir <- file.path('data', 'climate', 'pdsi', 'monthly_anomalies')

# Check if directory exists for all variable aggregate outputs, if not then create
var_dir <- list(prefix, raw_prefix, us_prefix, site_prefix, mtbs_prefix, forests_dir, cd_dir,
                fire_dir, buffered_mtbs, forest_prefix, forests_dir, domain_prefix, 
                baecv_dir, raw_baecv_dir, niwot_climate_dir, tmean_dir, pdsi_mean_dir, pdsi_anomalies_dir)
lapply(var_dir, function(x) if(!dir.exists(x)) dir.create(x, showWarnings = FALSE))

# Niwot ridge site shapefile was downloaded from 'http://niwot.colorado.edu/data/geospatial'
# The raw file comes in the antiquated .e00 format.
# R has a hard time handling those files so converted to ESRI shapefile in QGIS

#Download the USA States layer
us_shp <- file.path(us_prefix, "cb_2016_us_state_20m.shp")
if (!file.exists(us_shp)) {
  loc <- "https://www2.census.gov/geo/tiger/GENZ2016/shp/cb_2016_us_state_20m.zip"
  dest <- paste0(us_prefix, ".zip")
  download.file(loc, dest)
  unzip(dest, exdir = us_prefix)
  unlink(dest)
  assert_that(file.exists(us_shp))
}

#Download the MTBS fire polygons
mtbs_shp <- file.path(mtbs_prefix, 'mtbs_perimeter_data_v2', 'dissolve_mtbs_perims_1984-2015_DD_20170501.shp')
if (!file.exists(mtbs_shp)) {
  loc <- "https://edcintl.cr.usgs.gov/downloads/sciweb1/shared/MTBS_Fire/data/composite_data/burned_area_extent_shapefile/mtbs_perimeter_data.zip"
  dest <- paste0(mtbs_prefix, ".zip")
  download.file(loc, dest)
  unzip(dest, exdir = mtbs_prefix)
  unlink(dest)
  assert_that(file.exists(mtbs_shp))
  system(paste0("aws s3 sync ",
                raw_prefix, " ",
                s3_raw_prefix))
}

#Download US forest groups
forest_img <- file.path(forest_prefix, "conus_forestgroup.img")
if (!file.exists(forest_img)) {
  loc <- "https://data.fs.usda.gov/geodata/rastergateway/forest_type/conus_forestgroup.zip"
  dest <- paste0(prefix, ".zip")
  download.file(loc, dest)
  unzip(dest, exdir = forest_prefix)
  unlink(dest)
  assert_that(file.exists(forest_img))
}

#Download the NEON domains
domain_shp <- file.path(domain_prefix, "NEON_Domains.shp")
if (!file.exists(domain_shp)) {
  loc <- "http://www.neonscience.org/sites/default/files/NEONDomains_0.zip"
  dest <- paste0(prefix, ".zip")
  download.file(loc, dest)
  unzip(dest, exdir = domain_prefix)
  unlink(dest)
  assert_that(file.exists(domain_shp))
}

#Download the continental divide
#https://catalog.data.gov/dataset/continental-divide-trail
cd_shp <- file.path(cd_dir, "condivl020")
if (!file.exists(cd_shp)) {
  loc <- "https://github.com/jalbertbowden/us-data/raw/master/shapefiles/us-continental-divide/condivl020_nt00155.tar.gz"
  dest <- paste0(prefix, ".zip")
  download.file(loc, dest)
  untar(dest, exdir = cd_dir)
  unlink(dest)
  assert_that(file.exists(cd_shp))
}
