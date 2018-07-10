
if (!exists("usa_shp")){
  usa <- st_read(file.path(us_prefix), layer = "cb_2016_us_state_20m") %>%
    sf::st_transform(p4string_ea) %>%
    dplyr::filter(!STUSPS %in% c("HI", "AK", "PR"))
  usa$STUSPS <- droplevels(usa$STUSPS)
}

if (!exists("niwot")){
  niwot <- st_read(file.path(site_prefix), layer = "niwot_ridge_site") %>%
    sf::st_transform(p4string_ea) 
  niwot_sites <- st_read(file.path(site_prefix), layer = "NEON_Field_Sites") %>%
    sf::st_transform(p4string_ea) %>%
    filter(SiteName == 'Niwot Ridge Mountain Research Station')
}

if (!exists("mtbs")) {
  mtbs <- st_read(file.path(mtbs_prefix, 'mtbs_perimeter_data_v2'), layer = 'dissolve_mtbs_perims_1984-2015_DD_20170501') %>%
    sf::st_transform(p4string_ea) 
}

###### Buffer niwot ridge by 200k and extract MTBS polygons ###### 

niwot_buf_200k <- niwot %>%
  st_buffer(dist = 200000) 

mtbs_niwot_200k <- mtbs %>%
  st_join(niwot_buf_200k, join = st_intersects) %>%
  na.omit(BIOSHP_RES)

st_write(mtbs_niwot_200k, file.path(buffered_mtbs, 'mtbs_niwot_200k.shp'),
         delete_layer = TRUE)

mtbs_1980s_200k <- mtbs_niwot_200k %>%
  filter(Year < 1990)

mtbs_1990s_200k <- mtbs_niwot_200k %>%
  filter(Year >= 1990 & Year < 2000)

mtbs_2000s_200k <- mtbs_niwot_200k %>%
  filter(Year >= 2000 & Year < 2010)

mtbs_2010s_200k <- mtbs_niwot_200k %>%
  filter(Year >= 2010)

st_write(mtbs_1980s_200k, file.path(buffered_mtbs, 'mtbs_niwot_200k_1980s.shp'),
         delete_layer = TRUE)
st_write(mtbs_1990s_200k, file.path(buffered_mtbs, 'mtbs_niwot_200k_1990s.shp'),
         delete_layer = TRUE)
st_write(mtbs_2000s_200k, file.path(buffered_mtbs, 'mtbs_niwot_200k_2000s.shp'),
         delete_layer = TRUE)
st_write(mtbs_2010s_200k, file.path(buffered_mtbs, 'mtbs_niwot_200k_2010s.shp'),
         delete_layer = TRUE)

# Map at 200k
niwot_forest_proj_200k <- niwot_buf_200 %>%
  st_transform('+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD27 +units=m +no_defs +ellps=clrk66 +nadgrids=@conus,@alaska,@ntv2_0.gsb,@ntv1_can.dat')

# Import the forest groups and project to albers equal area
forests_200k <- raster(file.path(forest_prefix, "conus_forestgroup.img")) %>%
  crop(as(niwot_forest_proj_200k, "Spatial")) %>%
  mask(as(niwot_forest_proj_200k, "Spatial")) %>%
  projectRaster(crs = p4string_ea, res = 100, method = 'ngb') 

# 180 Pinyon/Juniper Group  
# 200	Douglas-fir Group
# 220	Ponderosa Pine Group
# 260	Fir/Spruce/Mountain Hemlock Group
# 280	Lodgepole Pine Group
# 360	Other Western Softwood Group
# 700	Elm/Ash/Cottonwood Group
# 900	Aspen/Birch Group
# 920	Western Oak Group
# 950	Other Western Hardwoods Group

rcl_mtrx <- matrix(c(-Inf, 258, NA,
                   259, 261, 1, # spruce/fir
                   262, 278, NA,
                   279, 281, 2, # lodgepole
                   282, Inf, NA), ncol=3, byrow=TRUE)
forests_200k <- reclassify(forests_200k, rcl_mtrx)

#define color palette
cpal <- c('olivedrab4', 'olivedrab3')
r2 <- ratify(forests_200k)
levelplot(r2, col.regions=cpal, att='ID', alpha=0.5) +
  layer(sp.polygons(as(mtbs_1980s_200k, 'Spatial'), fill = '#fef0d9', col = '#fef0d9')) +
  layer(sp.polygons(as(mtbs_1990s_200k, 'Spatial'), fill = '#fdcc8a', col = '#fdcc8a')) +
  layer(sp.polygons(as(mtbs_2000s_200k, 'Spatial'), fill = '#fc8d59', col = '#fc8d59')) +
  layer(sp.polygons(as(mtbs_2010s_200k, 'Spatial'), fill = '#d7301f', col = '#d7301f')) +
  layer(sp.polygons(as(niwot_sites, 'Spatial'), pch = 23, cex = 2, col = 'black', fill = 'yellow1')) +
  layer(sp.polygons(as(niwot_buf_200k, 'Spatial')))
  

###### Buffer niwot ridge by 500k and extract MTBS polygons ###### 

niwot_buf_500k <- niwot %>%
  st_buffer(dist = 500000) 

mtbs_niwot_500k <- mtbs %>%
  st_join(niwot_buf_500k, join = st_intersects) %>%
  na.omit(BIOSHP_RES)

st_write(mtbs_niwot_500k, file.path(buffered_mtbs, 'mtbs_niwot_500k.shp'),
         delete_layer = TRUE)

mtbs_1980s_500k <- mtbs_niwot_200k %>%
  filter(Year < 1990)

mtbs_1990s_500k <- mtbs_niwot_200k %>%
  filter(Year >= 1990 & Year < 2000)

mtbs_2000s_500k <- mtbs_niwot_200k %>%
  filter(Year >= 2000 & Year < 2010)

mtbs_2010s_500k <- mtbs_niwot_200k %>%
  filter(Year >= 2010)

st_write(mtbs_1980s_500k, file.path(buffered_mtbs, 'mtbs_niwot_500k_1980s.shp'),
         delete_layer = TRUE)
st_write(mtbs_1990s_500k, file.path(buffered_mtbs, 'mtbs_niwot_500k_1990s.shp'),
         delete_layer = TRUE)
st_write(mtbs_2000s_500k, file.path(buffered_mtbs, 'mtbs_niwot_500k_2000s.shp'),
         delete_layer = TRUE)
st_write(mtbs_2010s_500k, file.path(buffered_mtbs, 'mtbs_niwot_500k_2010s.shp'),
         delete_layer = TRUE)

# Map at 200k
niwot_forest_proj_500k <- niwot_buf_500k %>%
  st_transform('+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD27 +units=m +no_defs +ellps=clrk66 +nadgrids=@conus,@alaska,@ntv2_0.gsb,@ntv1_can.dat')

# Import the forest groups and project to albers equal area
forests_500k <- raster(file.path(forest_prefix, "conus_forestgroup.img")) %>%
  crop(as(niwot_forest_proj_500k, "Spatial")) %>%
  mask(as(niwot_forest_proj_500k, "Spatial")) %>%
  projectRaster(crs = p4string_ea, res = 250, method = 'ngb') 

rcl_mtrx <- matrix(c(-Inf, 258, NA,
                     259, 261, 1, # spruce/fir
                     262, 278, NA,
                     279, 281, 2, # lodgepole
                     282, Inf, NA), ncol=3, byrow=TRUE)
forests_500k <- reclassify(forests_500k, rcl_mtrx)

#define color palette
cpal <- c('olivedrab4', 'olivedrab3')
r2 <- ratify(forests_500k)
levelplot(r2, col.regions=cpal, att='ID', alpha=0.5) +
  layer(sp.polygons(as(mtbs_1980s_500k, 'Spatial'), fill = '#fef0d9', col = '#fef0d9')) +
  layer(sp.polygons(as(mtbs_1990s_500k, 'Spatial'), fill = '#fdcc8a', col = '#fdcc8a')) +
  layer(sp.polygons(as(mtbs_2000s_500k, 'Spatial'), fill = '#fc8d59', col = '#fc8d59')) +
  layer(sp.polygons(as(mtbs_2010s_500k, 'Spatial'), fill = '#d7301f', col = '#d7301f')) +
  layer(sp.polygons(as(niwot_sites, 'Spatial'), pch = 23, cex = 2, col = 'black', fill = 'yellow1')) +
  layer(sp.polygons(as(niwot_buf_500k, 'Spatial')))



