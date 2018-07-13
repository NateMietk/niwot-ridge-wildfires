source("src/R/prep_pdsi.R")z
source("src/functions/ggplot_theme.R")

# Extract the site specific data 

# Populate file lists
pdsi_list <- list.files(file.path('data', 'climate', 'pdsi', 'monthly_anomalies'),
                        pattern = 'tif$', recursive = TRUE, full.names = TRUE)
tanomalies_list <- list.files(file.path('data', 'climate', 'tmean', 'monthly_anomalies'),
                         pattern = 'tif$', recursive = TRUE,  full.names = TRUE)
tmean_list <- list.files(file.path('data', 'climate', 'tmean', "monthly_mean"), 
                         pattern = ".tif", full.names = TRUE)

#  Load and process mean PDSI
nc <- nc_open(file.path(prefix, "climate/pdsi/monthly_mean/pdsi_19792016.nc"))
nc_att <- attributes(nc$var)$names
ncvar <- ncvar_get(nc, nc_att)
tvar <- aperm(ncvar, c(3,2,1))
proj <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0 "
rasters <- brick(tvar, crs= proj)
extent(rasters) <- c(-124.793, -67.043, 25.04186, 49.41686)

pdsi_mean <- projectRaster(rasters ,crs = p4string_ea, res = 4000) %>%
  crop(as(cd, "Spatial")) %>%
  mask(as(cd, "Spatial"))
pdsi_mean <- dropLayer(pdsi_mean, 1:12)
idx <- seq(as.Date("1984/1/1"), as.Date("2016/12/31"), by = "month")
pdsi_mean <- setZ(pdsi_mean, idx)

#  Load and process mean PDSI anomalies
pdsi_anomalies <- raster::stack(pdsi_list) %>%
  crop(as(cd, 'Spatial')) %>%
  mask(as(cd, 'Spatial')) 

#  Load and process mean temperature
tmean <- raster::stack(tmean_list) %>%
  crop(as(cd, 'Spatial')) %>%
  mask(as(cd, 'Spatial')) 
tmean <- setZ(tmean, idx, 'months')

#  Load and process mean temperature anomalies
tmean_anomalies <- raster::stack(tanomalies_list) %>%
  crop(as(cd, 'Spatial')) %>%
  mask(as(cd, 'Spatial')) 

# pdsi time series
pdsi_anomalies_df <- velox(pdsi_anomalies)$extract(sp = cd, fun = function(x) mean(x, na.rm = TRUE), df = TRUE) %>%
  as_tibble() 
colnames(pdsi_anomalies_df) <- c('ID_sp', names(pdsi_anomalies))
pdsi_anomalies_df <- pdsi_anomalies_df %>%
  gather(key = key, value = pdsi_anomalies, -ID_sp) %>%
  separate(key,
           into = c("variable", 'year', 'anom'),
           sep = "_") %>%
  separate(anom,
           into = c("anom", 'month'),
           sep = "\\.") %>%
  mutate(id = ID_sp) %>%
  dplyr::select(id, year, month, pdsi_anomalies)

pdsi_mean_df <- velox(pdsi_mean)$extract(sp = cd, fun = function(x) mean(x, na.rm = TRUE), df = TRUE) %>%
  as_tibble() 
colnames(pdsi_mean_df) <- c('ID_sp', names(pdsi_mean))
pdsi_mean_df <- pdsi_mean_df %>%
  gather(key = key, value = pdsi_mean, -ID_sp) %>%
  separate(key,
           into = c("variable", 'year', 'anom'),
           sep = "_") %>%
  separate(anom,
           into = c("anom", 'month'),
           sep = "\\.") %>%
  mutate(id = ID_sp) %>%
  dplyr::select(id, year, month, pdsi_mean)

# tmean time series
tanomalies_mean <- velox(tmean_anomalies)$extract(sp = cd, fun = function(x) mean(x, na.rm = TRUE), df = TRUE) %>%
  as_tibble() 
colnames(tmean_mean) <- c('ID_sp', names(tmean_anomalies))
tmean_mean <- tmean_mean %>%
  gather(key = key, value = tmean_anomalies, -ID_sp) %>%
  separate(key,
           into = c("variable", 'year', 'anom'),
           sep = "_") %>%
  separate(anom,
           into = c("anom", 'month'),
           sep = "\\.") %>%
  mutate(id = ID_sp) %>%
  dplyr::select(id, year, month, tmean_anomalies)

tmean_mean <- velox(tmean)$extract(sp = cd, fun = function(x) mean(x, na.rm = TRUE), df = TRUE) %>%
  as_tibble() 
colnames(tmean_mean) <- c('ID_sp', names(tmean))
tmean_mean <- tmean_mean %>%
  gather(key = key, value = tmean_mean, -ID_sp) %>%
  separate(key,
           into = c("variable", 'year', 'anom'),
           sep = "_") %>%
  separate(anom,
           into = c("anom", 'month'),
           sep = "\\.") %>%
  mutate(id = ID_sp) %>%
  dplyr::select(id, year, month, tmean_mean)

# Prep data for plotting and calulate a 36-month moving average 
cd_df <- as.data.frame(cd) %>%
  dplyr::select(-geometry)

pdsi_anom_cln <- pdsi_mean %>%
  left_join(., cd_df, by = 'id') %>%
  group_by(regions) %>%
  arrange(regions) %>%
  mutate( med_5yr =rollapply(pdsi_anomalies, 24, mean, align='center', fill=NA)) %>%
  ungroup() %>%
  mutate(date = as.POSIXct(paste0(year, '-', month, '-01'), format = "%Y-%m-%d")) %>%
  filter(month %in% c(6, 7, 8))

tanom_mean_cln <- tanomalies_mean %>%
  left_join(., cd_df, by = 'id') %>%
  group_by(regions) %>%
  arrange(regions) %>%
  mutate( med_5yr = rollapply(tmean_anomalies, 24, mean, align='center', fill=NA)) %>%
  ungroup() %>%
  mutate(date = as.POSIXct(paste0(year, '-', month, '-01'), format = "%Y-%m-%d")) %>%
  filter(month %in% c(6, 7, 8))

tmean_mean_cln <- tmean_mean %>%
  left_join(., cd_df, by = 'id') %>%
  group_by(regions) %>%
  arrange(regions) %>%
  mutate( med_5yr = rollapply(tmean_mean, 24, mean, align='center', fill=NA)) %>%
  ungroup() %>%
  mutate(date = as.POSIXct(paste0(year, '-', month, '-01'), format = "%Y-%m-%d")) %>%
  filter(month %in% c(6, 7, 8))

# Plot the time series for summer months of tmean and pdsi

pdsi_ts <- pdsi_mean_cln %>%
  filter(date >= "1984-01-01" & date <= "2016-12-01") %>%
  ggplot(aes(x = date, y = med_5yr, color = regions, group = regions)) +
  #geom_point() +
  #geom_line(size = 0.5) +
  geom_line(aes(y = med_5yr), size = 0.5) +
  geom_hline(yintercept = 0, size = 0.5) + 
  scale_y_continuous(limits = c(-6, 6), breaks = c(-4, -2, 0, 2, 4)) +
  # scale_color_manual(values = c('', 'green')) +
  scale_x_datetime(date_breaks = "4 year", date_labels = "%Y", expand = c(0, 0),
                   limits = c(
                     as.POSIXct("1984-01-01"),
                     as.POSIXct("2016-12-01")
                   )) + 
  xlab("") + ylab("Anomolous mean \nsummer drought (PDSI)") +
  theme_pub() + theme(legend.position = 'none',
                      axis.text.x=element_blank())

tmean_ts <- tmean_mean_cln %>%
  filter(date >= "1984-01-01" & date <= "2016-12-01") %>%
  ggplot(aes(x = date, y = med_5yr, color = regions, group = regions)) +
  #geom_point(alpha = 0.15) +
  #geom_line(size = 0.5, alpha = 0.15) +
  geom_line(aes(y = med_5yr), size = 0.5) +
  # geom_hline(yintercept = 0, size = 0.5) + 
  # scale_y_continuous(limits = c(-1, 1.5)) +
  scale_x_datetime(date_breaks = "4 year", 
                   date_labels = "%Y", expand = c(0, 0),
                   limits = c(
                     as.POSIXct("1984-01-01"),
                     as.POSIXct("2016-12-01")
                   )) + 
  labs(x = "Year", y = "Anomolous mean \nsummer temperature", color = "Regions relative\n to the continential divide") +
  theme_pub() + theme(legend.position = c(0.2, 0.85))

library(gridExtra)
grid.arrange(pdsi_ts, tmean_ts, nrow = 2)
g <- arrangeGrob(pdsi_ts, tmean_ts, nrow = 2)

ggsave(file = "figures/timeseries_regions.pdf", g, width = 5, height = 4,
       scale = 4, dpi = 600, units = "cm") #saves p

