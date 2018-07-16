
# Import and process the PDSI data
start_date <- as.Date(paste("1980", "01", "01", sep = "-"))
end_date <- as.Date(paste("2016", "12", "31", sep = "-"))
date_seq <- seq(start_date, end_date, by = "1 month")
month_seq <- month(date_seq)
year_seq <- year(date_seq)

# Create raster stack of monthly mean
if (!file.exists(file.path(niwot_climate_dir, 'niwot_pdsi_monthly_anomalies.tif'))) {
  pdsi_list <- list.files(file.path('data', 'climate', 'pdsi', 'monthly_mean'),
                          pattern = 'tif$', recursive = TRUE, full.names = TRUE)
  
  #  Load and process mean PDSI anomalies
  pdsi_mean <- raster::stack(pdsi_list) %>%
    crop(as(cd_ll, 'Spatial')) %>%
    mask(as( cd_ll, 'Spatial')) 
  pdsi_mean <- dropLayer(pdsi_mean, 1:12)
  idx = seq(as.Date("1980/1/1"), as.Date("2016/12/31"), by = "month")
  pdsi_mean = setZ(pdsi_mean, idx)
  writeRaster(pdsi_mean, file.path(niwot_climate_dir, 'niwot_pdsi_monthly_mean.tif')) 
  
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
  
  writeRaster(pdsi_anomalies, file.path(niwot_climate_dir, 'niwot_pdsi_monthly_anomalies.tif')) 
} else {
  pdsi_anomalies <- raster::stack(file.path(niwot_climate_dir, 'niwot_pdsi_monthly_anomalies.tif'))
  pdsi_mean <- raster::stack(file.path(niwot_climate_dir, 'niwot_pdsi_monthly_mean.tif'))
}

# Create raster stack of monthly mean max temperature
if (!exists('tmmx_mean')) {
  daily_tmmx_files <- list.files(file.path(prefix, 'climate', 'tmmx','daily_mean'), 
                                 recursive = TRUE, full.names = TRUE)
  pboptions(type = 'txt', use_lb = TRUE)
  cl <- makeCluster(getOption("cl.cores", parallel::detectCores()/2))
  
  pblapply(daily_tmmx_files,
           FUN = daily_to_monthly,
           cl = cl)
  
  stopCluster(cl) 
  
  monthly_tmmx_files <- list.files(file.path(prefix, 'climate', 'tmmx','monthly_mean'), 
                                   recursive = TRUE, full.names = TRUE)
  
  tmmx_mean <- raster::stack(monthly_tmmx_files)
  tmmx_mean <- dropLayer(tmmx_mean, 1:12)
  idx = seq(as.Date("1980/1/1"), as.Date("2016/12/31"), by = "month")
  tmmx_mean = setZ(tmmx_mean, idx)
  
}

# Create raster stack of monthly mean min temperature
if (!exists('tmmn_mean')) {
  daily_tmmn_files <- list.files(file.path(prefix, 'climate', 'tmmn','daily_mean'), 
                                 recursive = TRUE, full.names = TRUE)
  pboptions(type = 'txt', use_lb = TRUE)
  cl <- makeCluster(getOption("cl.cores", parallel::detectCores()/2))
  
  pblapply(daily_tmmn_files,
           FUN = daily_to_monthly,
           cl = cl)
  
  stopCluster(cl) 
  
  monthly_tmmn_files <- list.files(file.path(prefix, 'climate', 'tmmn','monthly_mean'), 
                                   recursive = TRUE, full.names = TRUE)
  
  tmmn_mean <- raster::stack(monthly_tmmn_files)
  tmmn_mean <- dropLayer(tmmn_mean, 1:12)
  idx = seq(as.Date("1980/1/1"), as.Date("2016/12/31"), by = "month")
  tmmn_mean = setZ(tmmn_mean, idx)
} 

# Create raster stack of monthly mean average temperature
if (!exists('tmean_mean')) {
  tmean_mean <- overlay(x = tmmx_mean, y = tmmn_mean, fun = mean_fun)
  idx = seq(as.Date("1980/1/1"), as.Date("2016/12/31"), by = "month")
  tmean_mean = setZ(tmean_mean, idx)
  names(tmean_mean) <- paste0('tmean_mean_', year(date_seq),  "_", month(date_seq))
  
  # Monthly average tmean
  year <- 1980:2016
  for(i in year){
    r_sub <- subset(tmean_mean,  grep(i, names(tmean_mean))) # subset based on year
    # Write out the monthly anomalies by year
    if(!file.exists(file.path('data', 'climate', 'tmean', 'monthly_mean', paste0("tmean_", i, "_mean.tif")))) {
      writeRaster(r_sub, filename = file.path('data', 'climate', 'tmean', 'monthly_mean', paste0("tmean_", i, "_mean.tif")),
                  format = "GTiff") 
      
    }
  }
  
  # For import after the monthly mean creation - this is so we do not have to recalculate means everytime.
  tmean_list <- list.files(file.path('data', 'climate', 'tmean', 'monthly_mean'), 
                           pattern = ".tif", full.names = TRUE)
  
  tmean_mean <- raster::stack(tmean_list)
  idx <- seq(as.Date("1980/1/1"), as.Date("2016/12/31"), by = "month")
  tmean_mean <- setZ(tmean_mean, idx, 'months')
  names(tmean_mean) <- paste0('tmean_mean_', year(date_seq),  "_", month(date_seq))
  
  # Create anomalies
  # Split 1984-2016 period and take climatology
  tclimatology = subset(tmean_mean, 
                        which(getZ(tmean_mean)>=as.Date('1980-01-01') & 
                                getZ(tmean_mean)<=as.Date('2016-12-31')))
  names(tclimatology) <- paste0('tmean_climatology_', year(date_seq),  "_", month(date_seq))
  tclimatology_mon = zApply(tclimatology, by = months, mean, name = month.abb[])
  
  # Reorder the climatology from alphabetical
  tclimatology_mon <- 
    stack(tclimatology_mon[[5]],tclimatology_mon[[4]],tclimatology_mon[[8]],
          tclimatology_mon[[1]],tclimatology_mon[[9]], tclimatology_mon[[7]], 
          tclimatology_mon[[6]],tclimatology_mon[[2]],tclimatology_mon[[12]],
          tclimatology_mon[[11]],tclimatology_mon[[10]], tclimatology_mon[[3]])
  
  # Produce monthly anomalies
  tmean_anomalies <- overlay(x = tmean_mean, y = tclimatology_mon, fun = anom_fun)
  tmean_anomalies = setZ(tmean_anomalies, idx)
  names(tmean_anomalies) <- paste0('tmean_anomalies_', year(date_seq),  "_", month(date_seq))
  
  # Monthly average tmean
  year <- 1980:2016
  for(i in year){
    r_sub <- subset(tmean_anomalies,  grep(i, names(tmean_anomalies))) # subset based on year
    # Write out the monthly anomalies by year
    if(!file.exists(file.path('data', 'climate', 'tmean', 'anomalies', paste0("tmean_", i, "_anomalies.tif")))) {
      writeRaster(r_sub, filename = file.path('data', 'climate', 'tmean', 'anomalies', paste0("tmean_", i, "_anomalies.tif")),
                  format = "GTiff") 
    }
  }
  
  system('aws s3 sync data s3://earthlab-natem/niwot-ridge-wildfires')
} else {
  tmean <- raster::stack(file.path('data', 'climate', 'niwot', 'niwot_tmean_monthly_mean.tif'))
  tmean_anomalies <- raster::stack(file.path('data', 'climate', 'niwot', 'niwot_tmean_monthly_anomalies.tif'))
}




