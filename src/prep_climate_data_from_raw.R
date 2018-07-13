
# Import and process the PDSI data
start_date <- as.Date(paste("1980", "01", "01", sep = "-"))
end_date <- as.Date(paste("2016", "12", "31", sep = "-"))
date_seq <- seq(start_date, end_date, by = "1 month")
month_seq <- month(date_seq)
year_seq <- year(date_seq)

# Create raster stack of monthly mean
pdsi_list <- list.files(file.path('data', 'climate', 'pdsi', 'monthly_mean'),
                        pattern = 'tif$', recursive = TRUE, full.names = TRUE)

#  Load and process mean PDSI anomalies
pdsi_mean <- raster::stack(pdsi_list) %>%
  crop(as(cd_ll, 'Spatial')) %>%
  mask(as( cd_ll, 'Spatial')) 
pdsi_mean <- dropLayer(pdsi_mean, 1:12)
idx = seq(as.Date("1980/1/1"), as.Date("2016/12/31"), by = "month")
pdsi_mean = setZ(pdsi_mean, idx)

# Create anomalies
# Split 1984-2016 period and take climatology
pclimatology = subset(pdsi_mean,
                      which(getZ(pdsi_mean)>=as.Date('1980-01-01') &
                              getZ(pdsi_mean)<=as.Date('2016-12-31')))
pclimatology_mon = zApply(pclimatology, by=months, mean, name=month.abb[])

# Reorder the climatology from alphabetical
pclimatology_mon <- stack(
  pclimatology_mon[[5]],pclimatology_mon[[4]],pclimatology_mon[[8]],
  pclimatology_mon[[1]],pclimatology_mon[[9]], pclimatology_mon[[7]],
  pclimatology_mon[[6]],pclimatology_mon[[2]],pclimatology_mon[[12]],
  pclimatology_mon[[11]],pclimatology_mon[[10]], pclimatology_mon[[3]])

# Produce monthly anomalies
fun <- function(x, y) {
  x - y
}
pdsi_anomalies <- overlay(x = pdsi_mean, y = pclimatology_mon, fun = fun)
pdsi_anomalies = setZ(pdsi_anomalies, idx)
names(pdsi_anomalies) <- paste0('pdsi_anomalies_', year(date_seq),  "_", month(date_seq))
writeRaster(pdsi_anomalies, file.path('data', 'climate', 'pdsi', 'monthly_anomalies', 'niwot_pdsi_anomalies.tif'))


anom_fun <- function(x, y) {
  x - y
}
mean_fun <- function(x, y) {
  (x + y)/2
}

celsius <- function(x) {
  x - 272.15
}


# Create raster stack of monthly mean
daily_tmmx_files <- list.files(file.path(prefix, 'climate','temp', 'tmmx','daily_mean'), 
                                 recursive = TRUE, full.names = TRUE)
pboptions(type = 'txt', use_lb = TRUE)
cl <- makeCluster(getOption("cl.cores", parallel::detectCores()))

baecv <- pblapply(daily_tmmx_files,
                  FUN = daily_to_monthly,
                  mask = cd_ll,
                  cl = cl)

stopCluster(cl) 

tmmx_mean <- dropLayer(tmmx_mean, 1:12)
idx = seq(as.Date("1980/1/1"), as.Date("2016/12/31"), by = "month")
tmmx_mean = setZ(tmmx_mean, idx)

monthly_tmmn_files <- list.files(file.path(prefix, "climate/tmmn","monthly_mean"), 
                                 pattern = ".tif", full.names = TRUE)

tmmn_mean <- stack(monthly_tmmn_files) 
tmmn_mean <- dropLayer(tmmn_mean, 1:12)

tmean <- overlay(x = tmmx_mean, y = tmmn_mean, fun = mean_fun)

