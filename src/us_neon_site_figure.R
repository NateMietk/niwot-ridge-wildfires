
################## Read in and process all spatial layers

# Dissolve to the USA Boundary
conus <- st_union(usa)

# Import the forest groups and project to albers equal area
if(!file.exists(file.path(forests_dir, 'us_forests.tif'))){
  us_forests <- raster(file.path(forest_prefix, "conus_forestgroup.img")) %>%
    projectRaster(crs = p4string_ea, res = 500) %>%
    crop(as(states, "Spatial")) %>%
    mask(as(states, "Spatial"))

  rcl_mtrx <- matrix(c(-Inf, 1, NA,
                       1, 999, 2,
                       999, Inf, NA), ncol=3, byrow=TRUE)
  
  us_forests <- reclassify(us_forests, rcl_mtrx)
  writeRaster(us_forests, file.path(forests_dir, 'us_forests.tif'), 
              format = 'GTiff', overwrite = TRUE)
  } else {
    us_forests <- readRaster(us_forests, file.path(forests_dir, 'us_forests.tif'), 
                             format = 'GTiff')
  }

# Import the NEON domains and project to albers equal area
neon_domains <- st_read(dsn = domain_prefix,
                        layer = "NEON_Domains", quiet= TRUE) %>%
  st_transform(p4string_ea) %>%  # e.g. US National Atlas Equal Area
  filter(!(DomainName %in% c("Taiga", "Tundra", "Pacific Tropical"))) %>%
  st_intersection(., st_union(states)) %>%
  mutate(id = row_number())

# Import the NEON sites, clean to terrestial only, and project to albers equal area
neon_sites <- st_read(dsn = site_prefix,
                      layer = "NEON_Field_Sites", quiet= TRUE) %>%
  st_transform(p4string_ea) %>%  # e.g. US National Atlas Equal Area
  filter(!(State %in% c("AK", "HI", "PR"))) %>%
  filter(SiteType == "Core Terrestrial") 

neon_forest_sites <- velox(us_forests)$extract_points(sp = neon_sites) %>%
  as_tibble() %>% 
  mutate(PMC = as.data.frame(neon_sites)$PMC) %>%
  left_join(neon_sites, ., by = 'PMC') %>%
  na.omit() %>%
  mutate(group = 'Forested') 

neon_forest_sites_buffered <- neon_forest_sites %>%
  st_buffer(dist = 200000)

neon_nonforest_sites <- as.data.frame(neon_sites)  %>% 
  dplyr::select(PMC) %>%
  anti_join(., neon_forest_sites, by = 'PMC') %>%
  left_join(., neon_sites,  by = 'PMC') %>%
  st_as_sf() %>%
  filter(PMC != 'D13CT1') 

neon_nonforest_sites_buffered <- neon_nonforest_sites %>%
  st_buffer(dist = 200000)

# Create the map
p <- cpal <- c( 'darkgreen')
rus_forests <- ratify(us_forests)
levelplot(rus_forests, col.regions=cpal, att='ID', 
          margin = FALSE,                       
          # colorkey = list(
          #   space = 'right',                   
          #   labels = list(c('Lodgepole', 'Spruce/fir'), font = 4),
          #   axis.line = list(col='black')       
          # ),    
          par.settings = list(
            axis.line = list(col = 'transparent') 
          ),
          scales = list(draw = FALSE),            
          panel=panel.levelplot.raster) +
  layer(sp.polygons(as(neon_nonforest_sites_buffered , 'Spatial'), colour = "black", fill = 'red', alpha = 0.5)) +
  layer(sp.polygons(as(neon_nonforest_sites , 'Spatial'), colour = "black", fill = 'black', pch = 21)) +
  layer(sp.polygons(as(neon_forest_sites_buffered , 'Spatial'), colour = "black", fill = 'black', alpha = 0.5)) +
  layer(sp.polygons(as(neon_forest_sites , 'Spatial'), colour = "black", fill = 'black', pch = 23)) +
  layer(sp.polygons(as(niwot_buf_200k, 'Spatial'), colour='black', fill = 'yellow', pch = 18)) +
  layer(sp.polygons(as(niwot_sites, 'Spatial'), colour='black', fill = 'black', pch = 18)) +
  layer(sp.polygons(as(neon_domains, 'Spatial'), col = 'black')) +
  layer(sp.polygons(as(conus, 'Spatial'), col = 'black'))


ggsave(file = "figures/us_site_map.pdf", p, width = 4, height = 3, 
       dpi = 300, units = "cm") #saves p
