
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
  domains_ll <- st_read(file.path(domain_prefix), layer = "NEON_Domains") %>%
    st_transform(st_crs(usa)) %>%
    st_intersection(., st_union(usa)) %>%
    filter(DomainID %in% c('12', '13', '16')) %>%
    st_transform(p4string_latlong) %>%
    st_cast('POLYGON')

  domains <- st_read(file.path(domain_prefix), layer = "NEON_Domains") %>%
    st_transform(st_crs(usa)) %>%
    st_intersection(., st_union(usa)) %>%
    filter(DomainID %in% c('12', '13', '16'))
}

if (!exists("niwot_sites")){
  niwot_sites <- st_read(file.path(site_prefix), layer = "NEON_Field_Sites") %>%
    st_transform(st_crs(usa)) %>%
    filter(SiteName == 'Niwot Ridge Mountain Research Station') %>%
    st_transform('+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0')
  
}

if (!exists("niwot_buf_200k")){
  niwot_buf_200k <- niwot %>%
    st_buffer(dist = 200000)
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
  
  usa_small <- cd %>%
    st_intersection(., usa) %>%
    st_transform('+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0')
  
  cd_baecv_proj <- cd %>%
    st_transform('+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0')
}

if (!exists("mtbs")) {
  mtbs <- st_read(file.path(mtbs_prefix, 'mtbs_perimeter_data_v2'), layer = 'dissolve_mtbs_perims_1984-2015_DD_20170501') %>%
    sf::st_transform(p4string_ea) %>%
    st_join(., usa, join = st_intersects) %>%
    st_intersection(., st_union(usa))
  }

if (!exists("mtbs_cd_200k")){
  mtbs_cd_200k <- mtbs %>%
    st_join(cd, join = st_intersects) %>%
    na.omit(BIOSHP_RES)
    
    mtbs_1980s_200k <- mtbs_cd_200k %>%
      filter(Year < 1990)
    
    mtbs_1990s_200k <- mtbs_cd_200k %>%
      filter(Year >= 1990 & Year < 2000)
    
    mtbs_2000s_200k <- mtbs_cd_200k %>%
      filter(Year >= 2000 & Year < 2010)
    
    mtbs_2010s_200k <- mtbs_cd_200k %>%
      filter(Year >= 2010)
}

if(!file.exists(file.path(baecv_dir, 'niwot_baecv_2015.tif'))) {
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
  
  baecv_1980 <- dropLayer(baecv, 6:32) %>%
    calc(., fun = max)
  
  baecv_1990 <- dropLayer(baecv, c(1:6, 17:32)) %>%
    calc(., fun = max)
  
  baecv_2000 <- dropLayer(baecv, c(1:16, 27:32)) %>%
    calc(., fun = max)
  
  baecv_2010 <- dropLayer(baecv, 1:26) %>%
    calc(., fun = max)
}

if (!file.exists(file.path(forests_dir, 'forests_200k.tif'))) {
  niwot_forest_proj_200k <- cd %>%
    st_transform('+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD27 +units=m +no_defs +ellps=clrk66 +nadgrids=@conus,@alaska,@ntv2_0.gsb,@ntv1_can.dat')
  
  # Import the forest groups and project to albers equal area
  forests_200k <- raster(file.path(forest_prefix, "conus_forestgroup.img")) %>%
    crop(as(niwot_forest_proj_200k, "Spatial")) %>%
    mask(as(niwot_forest_proj_200k, "Spatial")) %>%
    projectRaster(crs = '+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs', res = 250, method = 'ngb')
  
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

###### Buffer niwot ridge by 200k and extract MTBS polygons ######
cpal <- c('olivedrab1', 'olivedrab2', 'olivedrab3', 'white')
forest_rat <- ratify(forests_200k)

pdf("figures/niwot_forest_baecv.pdf")
print(levelplot(forest_rat, col.regions=cpal, att='ID',
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
        levelplot(baecv_1980, col.regions = c('transparent', '#fee8c8'), att='year') +
        levelplot(baecv_1990, col.regions = c('transparent', '#ef6548'), att='year') +
        levelplot(baecv_2000, col.regions = c('transparent', '#d7301f'), att='year') +
        levelplot(baecv_2010, col.regions = c('transparent', '#7f0000'), att='year') +
        layer(sp.polygons(as(niwot_sites, 'Spatial'), pch = 23, cex = 2, col = 'black', fill = 'yellow1')) +
        layer(sp.polygons(as(niwot_forest_proj_200k, 'Spatial'))) +
        layer(sp.polygons(as(usa_small, 'Spatial'))))
dev.off()

