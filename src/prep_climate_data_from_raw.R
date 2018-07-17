
# Import and process the PDSI data
start_date <- as.Date(paste("1980", "01", "01", sep = "-"))
end_date <- as.Date(paste("2016", "12", "31", sep = "-"))
date_seq <- seq(start_date, end_date, by = "1 month")
month_seq <- month(date_seq)
year_seq <- year(date_seq)
idx = seq(as.Date("1980/1/1"), as.Date("2016/12/31"), by = "month")

# Create raster stack of monthly mean
if (length(list.files(pdsi_anomalies_dir, pattern = 'tif$', recursive = TRUE, full.names = TRUE)) != 37) {
  
  #  Load and process mean PDSI anomalies
  pdsi_mean <- raster::stack(pdsi_list) 
  pdsi_mean <- dropLayer(pdsi_mean, 1:12)
  pdsi_mean = setZ(pdsi_mean, idx)
  
  # Create anomalies
  # Split 1984-2016 period and take climatology
  pclimatology = subset(pdsi_mean,
                        which(getZ(pdsi_mean) >= as.Date('1980-01-01') &
                                getZ(pdsi_mean) <= as.Date('2016-12-31')))
  pclimatology_mon = zApply(pclimatology, by = months, mean, name = month.abb[])
  
  # Reorder the climatology from alphabetical
  pclimatology_mon <- stack(
    pclimatology_mon[[5]],pclimatology_mon[[4]],pclimatology_mon[[8]],
    pclimatology_mon[[1]],pclimatology_mon[[9]], pclimatology_mon[[7]],
    pclimatology_mon[[6]],pclimatology_mon[[2]],pclimatology_mon[[12]],
    pclimatology_mon[[11]],pclimatology_mon[[10]], pclimatology_mon[[3]])
  
  # Produce monthly anomalies
  pdsi_anomalies <- overlay(x = pdsi_mean, y = pclimatology_mon, fun = anom_fun)
  pdsi_anomalies = setZ(pdsi_anomalies, idx)
  names(pdsi_anomalies) <- paste0('pdsi_anomalies_', year(date_seq),  "_", month(date_seq))
  
  # Monthly average tmean
  year <- 1980:2016
  for(i in year){
    r_sub <- subset(pdsi_anomalies,  grep(i, names(pdsi_anomalies))) # subset based on year
    # Write out the monthly anomalies by year
    if(!file.exists(file.path(pdsi_anomalies_dir, paste0("pdsi_mean_", i, "_anomalies.tif")))) {
      writeRaster(r_sub, filename = file.path(pdsi_anomalies_dir, paste0("pdsi_mean_", i, "_anomalies.tif")),
                  format = "GTiff") 
    }
  }
} else {
  pdsi_mean_list <- list.files(pdsi_mean_dir, pattern = 'tif$', recursive = TRUE, full.names = TRUE)
  pdsi_anomalies_list <- list.files(pdsi_anomalies_dir, pattern = 'tif$', recursive = TRUE, full.names = TRUE)
  
  pdsi_mean <- raster::stack(pdsi_anomalies_dir)
  pdsi_anomalies <- raster::stack(pdsi_mean_list)
  
}

# Create raster stack of monthly mean max temperature
if (length(list.files(tmax_dir, pattern = 'tif$', recursive = TRUE, full.names = TRUE)) != 38) {
  daily_tmmx_files <- list.files(file.path(prefix, 'climate', 'raw', 'tmmx'), 
                                 recursive = TRUE, full.names = TRUE)
  pboptions(type = 'txt', use_lb = TRUE)
  cl <- makeCluster(getOption("cl.cores", parallel::detectCores()/2))
  
  pblapply(daily_tmmx_files,
           FUN = daily_to_monthly,
           cl = cl)
  
  stopCluster(cl) 
  
  monthly_tmmx_files <- list.files(tmax_dir, recursive = TRUE, full.names = TRUE)
  
  tmmx_mean <- raster::stack(monthly_tmmx_files)
  tmmx_mean <- dropLayer(tmmx_mean, 1:12)
  tmmx_mean = setZ(tmmx_mean, idx)
  
} else {
  monthly_tmmx_list <- list.files(tmmx_dir, pattern = 'tif$', recursive = TRUE, full.names = TRUE)
  
  tmmx_mean <- raster::stack(monthly_tmmx_list)
  tmmx_mean <- dropLayer(tmmx_mean, 1:12)
  tmmx_mean = setZ(tmmx_mean, idx)
}

# Create raster stack of monthly mean min temperature

if (length(list.files(tmmn_dir, pattern = 'tif$', recursive = TRUE, full.names = TRUE)) != 38) {
  daily_tmmn_files <- list.files(file.path(prefix, 'climate', 'raw', 'tmmn'), 
                                 recursive = TRUE, full.names = TRUE)
  pboptions(type = 'txt', use_lb = TRUE)
  cl <- makeCluster(getOption("cl.cores", parallel::detectCores()/2))
  
  pblapply(daily_tmmn_files,
           FUN = daily_to_monthly,
           cl = cl)
  
  stopCluster(cl) 
  
  monthly_tmmn_list <- list.files(tmmn_dir, pattern = 'tif$', recursive = TRUE, full.names = TRUE)
  
  tmmn_mean <- raster::stack(monthly_tmmn_files)
  tmmn_mean <- dropLayer(tmmn_mean, 1:12)
  tmmn_mean = setZ(tmmn_mean, idx)
} else {
  monthly_tmmn_list <- list.files(tmmn_dir, pattern = 'tif$', recursive = TRUE, full.names = TRUE)
  
  tmmn_mean <- raster::stack(monthly_tmmn_list)
  tmmn_mean <- dropLayer(tmmn_mean, 1:12)
  tmmn_mean = setZ(tmmn_mean, idx)
}

# Create raster stack of monthly mean average temperature
if (length(list.files(temp_anomalies_dir, pattern = 'tif$', recursive = TRUE, full.names = TRUE)) != 37) {
  
  temp_mean <- overlay(x = tmmx_mean, y = tmmn_mean, fun = mean_fun)
  temp_mean = setZ(temp_mean, idx)
  names(temp_mean) <- paste0('temp_mean_', year(date_seq),  "_", month(date_seq))
  
  # Monthly average temp
  year <- 1980:2016
  for(i in year){
    r_sub <- subset(temp_mean,  grep(i, names(temp_mean))) # subset based on year
    # Write out the monthly anomalies by year
    if(!file.exists(file.path('data', 'climate', 'tmean', 'monthly_mean', paste0("temp_", i, "_mean.tif")))) {
      writeRaster(r_sub, filename = file.path('data', 'climate', 'tmean', 'monthly_mean', paste0("temp_", i, "_mean.tif")),
                  format = "GTiff") 
      
    }
  }
  
  # For import after the monthly mean creation - this is so we do not have to recalculate means everytime.
  temp_list <- list.files(temp_anomalies_dir, pattern = ".tif", full.names = TRUE)
  
  temp_mean <- raster::stack(temp_list)
  idx <- seq(as.Date("1980/1/1"), as.Date("2016/12/31"), by = "month")
  temp_mean <- setZ(temp_mean, idx, 'months')
  names(temp_mean) <- paste0('temp_mean_', year(date_seq),  "_", month(date_seq))
  
  # Create anomalies
  # Split 1984-2016 period and take climatology
  tclimatology = subset(temp_mean, 
                        which(getZ(temp_mean)>=as.Date('1980-01-01') & 
                                getZ(temp_mean)<=as.Date('2016-12-31')))
  names(tclimatology) <- paste0('temp_climatology_', year(date_seq),  "_", month(date_seq))
  tclimatology_mon = zApply(tclimatology, by = months, mean, name = month.abb[])
  
  # Reorder the climatology from alphabetical
  tclimatology_mon <- 
    stack(tclimatology_mon[[5]],tclimatology_mon[[4]],tclimatology_mon[[8]],
          tclimatology_mon[[1]],tclimatology_mon[[9]], tclimatology_mon[[7]], 
          tclimatology_mon[[6]],tclimatology_mon[[2]],tclimatology_mon[[12]],
          tclimatology_mon[[11]],tclimatology_mon[[10]], tclimatology_mon[[3]])
  
  # Produce monthly anomalies
  temp_mean_anomalies <- overlay(x = temp_mean, y = tclimatology_mon, fun = anom_fun)
  temp_mean_anomalies = setZ(temp_mean_anomalies, idx)
  names(temp_mean_anomalies) <- paste0('temp_anomalies_', year(date_seq),  "_", month(date_seq))
  
  # Monthly average tmean
  year <- 1980:2016
  for(i in year){
    r_sub <- subset(temp_mean_anomalies,  grep(i, names(temp_mean_anomalies))) # subset based on year
    # Write out the monthly anomalies by year
    if(!file.exists(file.path(temp_anomalies_dir, paste0("temp_mean_", i, "_anomalies.tif")))) {
      writeRaster(r_sub, filename = file.path(temp_anomalies_dir, paste0("temp_mean_", i, "_anomalies.tif")),
                  format = "GTiff") 
    }
  }
  
  system('aws s3 sync data s3://earthlab-natem/niwot-ridge-wildfires')
} else {
  temp_mean_list <- list.files(temp_mean_dir, pattern = 'tif$', recursive = TRUE, full.names = TRUE)
  temp_mean_anomalies_list <- list.files(temp_anomalies_dir, pattern = 'tif$', recursive = TRUE, full.names = TRUE)
  
  temp_mean <- raster::stack(temp_mean_list)
  temp_mean_anomalies <- raster::stack(temp_mean_anomalies_list)
}




