
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
usa_small <- niwot_buf_200k %>%
  st_intersection(., usa)

mtbs_niwot_200k <- mtbs %>%
  st_join(niwot_buf_200k, join = st_intersects) %>%
  na.omit(BIOSHP_RES)

as.data.frame(mtbs_niwot_200k) %>%
  mutate(decade = ifelse(Year < 1989, 1980,
                         ifelse(Year >= 1990 & Year < 2000, 1990,
                                ifelse(Year >= 2000 & Year < 2010, 2000, 2010)))) %>%
  group_by(decade) %>%
  summarise(mean_fire_size = mean(Acres)) %>%
  ggplot(aes(x = decade, y = mean_fire_size)) +
  geom_bar(stat= 'identity')

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
system('aws s3 sync s3://earthlab-natem/niwot-ridge-wildfires data')

if (!file.exists(file.path(forests_dir, 'forests_200k.tif'))) {
  niwot_forest_proj_200k <- niwot_buf_200k %>%
    st_transform('+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD27 +units=m +no_defs +ellps=clrk66 +nadgrids=@conus,@alaska,@ntv2_0.gsb,@ntv1_can.dat')
  
  # Import the forest groups and project to albers equal area
  forests_200k <- raster(file.path(forest_prefix, "conus_forestgroup.img")) %>%
    crop(as(niwot_forest_proj_200k, "Spatial")) %>%
    mask(as(niwot_forest_proj_200k, "Spatial")) %>%
    projectRaster(crs = p4string_ea, res = 250, method = 'ngb') 
  
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
  
  rcl_mtrx <- matrix(c(-Inf, 0, NA,
                       0, 219, 4,
                       219, 221, 1, # ponderosa pine
                       222, 258, 4,
                       259, 261, 3, # spruce/fir
                       262, 278, 4,
                       279, 281, 2, # lodgepole
                       282, 951, 4,
                       951, Inf, NA), ncol=3, byrow=TRUE)
  forests_200k <- reclassify(forests_200k, rcl_mtrx)
  writeRaster(forests_200k, file.path(forests_dir, 'forests_200k.tif'), 
              format = 'GTiff', overwrite = TRUE)
  system('aws s3 sync s3://earthlab-natem/niwot-ridge-wildfires data')
  
} else {
  forests_200k <- raster(file.path(forests_dir, 'forests_200k.tif'))
}

#define color palette
cpal <- c('olivedrab1', 'olivedrab2', 'olivedrab3', 'gray')
r2 <- ratify(forests_200k)
levelplot(r2, col.regions=cpal, att='ID', 
          margin = FALSE,                       
          colorkey = list(
            space = 'right',                   
            labels = list(c('Lodgepole', 'Spruce/fir'), font = 4),
            axis.line = list(col='black')       
          ),    
          par.settings = list(
            axis.line = list(col = 'transparent') 
          ),
          scales = list(draw = FALSE),            
          panel=panel.levelplot.raster) +
  layer(sp.polygons(as(mtbs_1980s_200k, 'Spatial'), fill = '#fc8d59', col = '#fc8d59')) +
  layer(sp.polygons(as(mtbs_1990s_200k, 'Spatial'), fill = '#ef6548', col = '#ef6548')) +
  layer(sp.polygons(as(mtbs_2000s_200k, 'Spatial'), fill = '#d7301f', col = '#d7301f')) +
  layer(sp.polygons(as(mtbs_2010s_200k, 'Spatial'), fill = '#990000', col = '#990000')) +
  layer(sp.polygons(as(niwot_sites, 'Spatial'), pch = 23, cex = 2, col = 'black', fill = 'yellow1')) +
  layer(sp.polygons(as(niwot_buf_200k, 'Spatial'))) +
  layer(sp.polygons(as(usa_small, 'Spatial')))


