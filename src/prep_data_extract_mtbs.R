
if (!exists("usa_shp")){
  usa <- st_read(file.path(us_prefix), layer = "cb_2016_us_state_20m") %>%
    sf::st_transform(p4string_ea) %>%
    dplyr::filter(!STUSPS %in% c("HI", "AK", "PR"))
  usa$STUSPS <- droplevels(usa$STUSPS)
}

if (!exists("niwot")){
  niwot <- st_read(file.path(site_prefix), layer = "niwot_ridge_site") %>%
    sf::st_transform(p4string_ea) 
}

if (!exists("mtbs")) {
  mtbs <- st_read(file.path(mtbs_prefix, 'mtbs_perimeter_data_v2'), layer = 'dissolve_mtbs_perims_1984-2015_DD_20170501') %>%
    sf::st_transform(p4string_ea) 
}

# Buffer niwot ridge by 500k and extract MTBS polygons

niwot_buf_500k <- niwot %>%
  st_buffer(dist = 500000) 

mtbs_niwot_500k <- mtbs %>%
  st_join(niwot_buf_500k, join = st_intersects) %>%
  na.omit(BIOSHP_RES)

st_write(mtbs_niwot_500k, file.path(buffered_mtbs, 'mtbs_niwot_500k.shp'),
         delete_layer = TRUE)

# Buffer niwot ridge by 200k and extract MTBS polygons

niwot_buf_200 <- niwot %>%
  st_buffer(dist = 200000) 

mtbs_niwot_200k <- mtbs %>%
  st_join(niwot_buf_200, join = st_intersects) %>%
  na.omit(BIOSHP_RES)

st_write(mtbs_niwot_200k, file.path(buffered_mtbs, 'mtbs_niwot_200k.shp'),
         delete_layer = TRUE)

