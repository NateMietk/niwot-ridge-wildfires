
if (!exists("usa_shp")){
  usa <- st_read(file.path(us_prefix), layer = "cb_2016_us_state_20m") %>%
    sf::st_transform(p4string_ea) %>%
    dplyr::filter(!STUSPS %in% c("HI", "AK", "PR")) %>%
    mutate(regions = as.factor(if_else(!(STUSPS %in% c("FL", "GA", "AL", "MS", "LA", "AR", "TN", "NC", "SC", "TX", "OK", "ME", "NH", "VT", "NY", "PA", "DE", "NJ", "RI", "CT",
                                                       "MI", "MD", "MA", "WI", "IL", "IN", "OH", "WV", "VA", "KY", "MO", "IA", "MN")), "West", 'Central/East')))
  usa$STUSPS <- droplevels(usa$STUSPS)
}

if (!exists("niwot")){
  niwot <- st_read(file.path(site_prefix), layer = "niwot_ridge_site") %>%
    st_transform(st_crs(usa)) 
}

if (!exists("domains")){
  domains <- st_read(file.path(domain_prefix), layer = "NEON_Domains") %>%
    st_transform(st_crs(usa)) %>%
    st_intersection(., st_union(usa)) %>%
    filter(DomainID %in% c('12', '13', '16')) %>%
    st_transform(p4string_latlong)
}

if (!exists("niwot_sites")){
  niwot_sites <- st_read(file.path(site_prefix), layer = "NEON_Field_Sites") %>%
    st_transform(st_crs(usa)) %>%
    filter(SiteName == 'Niwot Ridge Mountain Research Station')
}

if (!exists("niwot_buf_200k")){
  niwot_buf_200k <- niwot %>%
    st_buffer(dist = 200000) 
  usa_small <- niwot_buf_200k %>%
    st_intersection(., usa)
  }

if (!exists('cd')) {
  cd <- st_read(file.path(cd_dir), layer = "condivl020")
  st_crs(cd) = p4string_latlong
  
  cd_buffer <- cd %>%
    st_transform(st_crs(usa)) %>%
    st_intersection(., niwot_buf_200k) %>%
    st_union() %>%
    st_centroid() %>%
    st_buffer(dist = 200000) 
  
  cd <- cd %>%
    st_transform(st_crs(usa)) %>%
    st_intersection(., cd_buffer) %>%
    st_union() 

  cd <- as(cd_buffer, 'Spatial') %>%
    gIntersection(., as(cd, 'Spatial')) %>%
    gBuffer(., width = 0.000001) %>%
    gDifference(as(cd_buffer, 'Spatial'), .)               
  
  east <- SpatialPolygons(list(Polygons(list(cd@polygons[[1]]@Polygons[[1]]), "1")))
  west <- SpatialPolygons(list(Polygons(list(cd@polygons[[1]]@Polygons[[2]]), "2")))
  
  cd <- st_as_sf(rbind(east, west))
  st_crs(cd) = st_crs(usa)
  cd <- cd %>%
    mutate(id = row_number(),
           regions = ifelse(id == 1, 'East', 'West'))
  cd_ll <- cd %>%
    st_transform('+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs')
  cd_baecv_proj <- cd %>%
    st_transform('+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0')
}

if (!exists("mtbs")) {
  mtbs <- st_read(file.path(mtbs_prefix, 'mtbs_perimeter_data_v2'), layer = 'dissolve_mtbs_perims_1984-2015_DD_20170501') %>%
    sf::st_transform(p4string_ea) %>%
    st_join(., usa, join = st_intersects) %>%
    st_intersection(., st_union(usa))
  
  mtbs_fitted_mblm <- as.data.frame(mtbs) %>%
    filter(regions == 'West') %>%
    group_by(Year) %>%
    summarise(mean_fire_size = mean(Acres)) %>%
    mblm(mean_fire_size ~ Year, data = ., repeated = FALSE)
  pct_increase <- (max(mtbs_fitted_mblm$fitted.values)/min(mtbs_fitted_mblm$fitted.values))*100
  
  p1 <- as.data.frame(mtbs) %>%
    filter(regions == 'West') %>%
    group_by(Year) %>%
    summarise(mean_fire_size = mean(Acres)) %>%
    ggplot(aes(x = Year, y = mean_fire_size)) +
    geom_bar(stat= 'identity') +
    geom_abline(intercept = coef(mtbs_fitted_mblm)[1], 
                slope = coef(mtbs_fitted_mblm)[2], 
                col = 'red', linetype = "dashed", size = 1.25) +
    geom_text(aes(label=paste('% change from 1984-2015 = ', round(pct_increase[1],1), '%'), 
                  x = 1990, y = 20000), size = 4) +
    ylab('Mean fire size (acres)') + xlab('Year') +
    ggtitle('Average fire size across the western US from 1984-2015') +
    theme_pub()
  ggsave("figures/wus_mean_fire_size.pdf", p1, width = 6, height = 5, dpi = 600, units = "cm", scale = 3)
  
  
}

if (!exists("mtbs_niwot_200k")){
  mtbs_niwot_200k <- mtbs %>%
    st_join(niwot_buf_200k, join = st_intersects) %>%
    na.omit(BIOSHP_RES) %>%
    mutate(decade = as.integer(case_when(
      Year <= 1989 ~ 1,
      Year >= 1990 & Year < 1995 ~ 2,
      Year >= 1995 & Year <= 1999 ~ 3,
      Year >= 2000 & Year < 2005 ~ 4,
      Year >= 2005 & Year <= 2009 ~ 5,
      Year >= 2010 & Year <= 2015 ~ 6, TRUE ~ NA_real_)),
      decade_title = as.factor(case_when(
        decade == 1 ~ '1984-1989',
        decade == 2 ~ '1990-1994',
        decade == 3 ~ '1995-1999',
        decade == 4 ~ '2000-2004',
        decade == 5 ~ '2005-2009',
        decade == 6 ~ '2010-2015',
        TRUE ~ NA_character_))) 
}

if(!file.exists(file.path(baecv, file.path(baecv_dir, 'niwot_baecv_2015.tif')))) {
  baecv_list <- list.files(file.path(raw_baecv_dir),
                           pattern = 'tif$',
                           recursive = TRUE,
                           full.names = TRUE)
  
  
  pboptions(type = 'txt', use_lb = TRUE)
  cl <- makeCluster(getOption("cl.cores", parallel::detectCores()))
  
  baecv <- pblapply(baecv_list,
                    FUN = function(filename, out_dir, mask, p4string_ea) {
                      require(tidyverse)
                      require(raster)
                      
                      out_name <- filename %>%
                        basename() %>%
                        gsub('BAECV_bc', 'niwot_baecv', .) %>%
                        gsub('_v1_20160825', '', .)
                      
                      baecv <- raster::raster(filename) %>%
                        crop(mask) %>%
                        mask(mask)
                      
                      writeRaster(baecv, file.path(out_dir, out_name))
                      return(baecv)
                    },
                    p4string_ea = p4string_ea,
                    out_dir = baecv_dir,
                    mask = as(cd_baecv_proj, 'Spatial'),
                    cl = cl)
  
  stopCluster(cl) 
  
} else {
  baecv_list <- list.files(file.path(baecv_dir),
                           pattern = 'tif$',
                           recursive = TRUE,
                           full.names = TRUE)
  baecv <- stack(baecv_list) 
}





###### Buffer niwot ridge by 200k and extract MTBS polygons ###### 

if(!file.exists('figures/niwot_mean_fire_size.pdf')){
  mtbs_200k_fitted_mblm <- as.data.frame(mtbs_niwot_200k) %>%
    group_by(decade) %>%
    summarise(mean_fire_size = mean(Acres)) %>%
    mblm(mean_fire_size ~ decade, data = ., repeated = FALSE)
  
  pct_increase <- (max(mtbs_200k_fitted_mblm$fitted.values)/min(mtbs_200k_fitted_mblm$fitted.values))*100
  
  p2 <- as.data.frame(mtbs_niwot_200k)  %>%
    group_by(decade_title) %>%
    summarise(mean_fire_size = mean(Acres)) %>%
    ggplot(aes(x = decade_title, y = mean_fire_size)) +
    geom_bar(stat= 'identity') +
    geom_abline(intercept = coef(mtbs_200k_fitted_mblm)[1], 
                slope = coef(mtbs_200k_fitted_mblm)[2], 
                col = 'red', linetype = "dashed", size = 1.25) +
    geom_text(aes(label=paste('% change from 1984-2015 = ', round(pct_increase[1],1), '%'), 
                  x = 1.75, y = 8500), size = 4) +
    ylab('Mean fire size (acres)') + xlab('Year') +
    ggtitle('Average fire size within a 200k radius from Niwot station from 1984-2015') +
    theme_pub()
  ggsave("figures/niwot_mean_fire_size.pdf", p2, width = 6, height = 5, dpi = 600, units = "cm", scale = 3) 
}


mtbs_1980s_200k <- mtbs_niwot_200k %>%
  filter(Year < 1990)

mtbs_1990s_200k <- mtbs_niwot_200k %>%
  filter(Year >= 1990 & Year < 2000)

mtbs_2000s_200k <- mtbs_niwot_200k %>%
  filter(Year >= 2000 & Year < 2010)

mtbs_2010s_200k <- mtbs_niwot_200k %>%
  filter(Year >= 2010)

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


