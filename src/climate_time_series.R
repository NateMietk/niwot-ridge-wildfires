domains_df <- as.data.frame(domains) %>%
  dplyr::select(DomainID, DomainName)

# pdsi time series
if(!file.exists(file.path('data', 'climate', 'pdsi_summer_mean_domains.rds'))) {
  pdsi_mean_df <- velox(pdsi_mean)$extract(sp = domains_ll, fun = function(x) mean(x, na.rm = TRUE), df = TRUE) %>%
    as_tibble() 
  colnames(pdsi_mean_df) <- c('ID_sp', names(pdsi_mean))
  pdsi_mean_df <- pdsi_mean_df %>%
    gather(key = key, value = pdsi_mean, -ID_sp) %>%
    separate(key,
             into = c("variable", 'year', 'mean'),
             sep = "_") %>%
    separate(mean,
             into = c("mean", 'month'),
             sep = "\\.") %>%
    mutate(DomainID = ifelse(ID_sp == 1, 12, 
                             ifelse(ID_sp == 2, 13, 16))) %>%
    dplyr::select(DomainID, year, month, pdsi_mean) %>%
    left_join(., domains_df, by = 'DomainID') %>%
    group_by(DomainName) %>%
    arrange(DomainName) %>%
    mutate( med_5yr =rollapply(pdsi_mean, 24, mean, align='center', fill=NA)) %>%
    ungroup() %>%
    mutate(date = as.POSIXct(paste0(year, '-', month, '-01'), format = "%Y-%m-%d")) %>%
    filter(month %in% c(6, 7, 8))
  write_rds(pdsi_mean_df, file.path('data', 'climate', 'pdsi_summer_mean_domains.rds'))
  
} else {
  pdsi_mean_df <- read_rds(file.path('data', 'climate', 'pdsi_summer_mean_domains.rds')) %>%
    setNames(tolower(names(.)))
}

if(!file.exists(file.path('data', 'climate', 'pdsi_summer_anomalies_domains.rds'))) {
  pdsi_anomalies_df <- velox(pdsi_anomalies)$extract(sp = domains_ll, fun = function(x) mean(x, na.rm = TRUE), df = TRUE) %>%
    as_tibble() 
  colnames(pdsi_anomalies_df) <- c('ID_sp', names(pdsi_anomalies))
  pdsi_anomalies_df <- pdsi_anomalies_df %>%
    gather(key = key, value = pdsi_anomalies, -ID_sp) %>%
    separate(key,
             into = c("variable", 'anom', 'year', 'month'),
             sep = "_") %>%
    mutate(DomainID = ifelse(ID_sp == 1, 12, 
                             ifelse(ID_sp == 2, 13, 16))) %>%
    dplyr::select(DomainID, year, month, pdsi_anomalies) %>%
    left_join(., domains_df, by = 'DomainID') %>%
    group_by(DomainName) %>%
    arrange(DomainName) %>%
    mutate( med_5yr =rollapply(pdsi_anomalies, 24, mean, align='center', fill=NA)) %>%
    ungroup() %>%
    mutate(date = as.POSIXct(paste0(year, '-', month, '-01'), format = "%Y-%m-%d")) %>%
    filter(month %in% c(6, 7, 8))
  write_rds(pdsi_anomalies_df, file.path('data', 'climate', 'pdsi_summer_anomalies_domains.rds'))
} else {
  pdsi_anomalies_df <- read_rds(file.path('data', 'climate', 'pdsi_summer_anomalies_domains.rds')) %>%
    setNames(tolower(names(.)))
}

# tmean time series
if(!file.exists(file.path('data', 'climate', 'temp_summer_mean_domains.rds'))) {
  temp_mean_df <- velox(temp_mean)$extract(sp = domains_ll, fun = function(x) mean(x, na.rm = TRUE), df = TRUE) %>%
    as_tibble() 
  colnames(temp_mean_df) <- c('ID_sp', names(temp_mean))
  temp_mean_df_t <- temp_mean_df %>%
    gather(key = key, value = temp_mean, -ID_sp) %>%
    separate(key,
             into = c("variable", 'year', 'mean'),
             sep = "_") %>%
    separate(mean,
             into = c("mean", 'month'),
             sep = "\\.") %>%
    mutate(DomainID = ifelse(ID_sp == 1, 12, 
                             ifelse(ID_sp == 2, 13, 16))) %>%
    dplyr::select(DomainID, year, month, temp_mean) %>%
    left_join(., domains_df, by = 'DomainID') %>%
    group_by(DomainName) %>%
    arrange(DomainName) %>%
    mutate( med_5yr = rollapply(temp_mean, 24, mean, align='center', fill=NA)) %>%
    ungroup() %>%
    mutate(date = as.POSIXct(paste0(year, '-', month, '-01'), format = "%Y-%m-%d"),
           temp_mean = celsius(temp_mean),
           med_5yr = celsius(med_5yr)) %>%
    filter(month %in% c(6, 7, 8)) %>%
    setNames(tolower(names(.))) %>%
    mutate(year = as.integer(year))
  write_rds(temp_mean_df, file.path('data', 'climate', 'temp_summer_mean_domains.rds'))
} else {
  temp_mean_df <- read_rds(file.path('data', 'climate', 'temp_summer_mean_domains.rds'))
}

if(!file.exists(file.path('data', 'climate', 'temp_summer_anomalies_domains.rds'))) {
  
  temp_mean_anomalies_df <- velox(temp_mean_anomalies)$extract(sp = domains_ll, fun = function(x) mean(x, na.rm = TRUE), df = TRUE) %>%
    as_tibble() 
  colnames(temp_mean_anomalies_df) <- c('ID_sp', names(temp_mean_anomalies))
  temp_mean_anomalies_df <- temp_mean_anomalies_df %>%
    gather(key = key, value = temp_anomalies, -ID_sp) %>%
    separate(key,
             into = c("variable", 'year', 'anom'),
             sep = "_") %>%
    separate(anom,
             into = c("anom", 'month'),
             sep = "\\.") %>%
    mutate(DomainID = ifelse(ID_sp == 1, 12, 
                             ifelse(ID_sp == 2, 13, 16))) %>%
    dplyr::select(DomainID, year, month, temp_anomalies) %>%
    left_join(., domains_df, by = 'DomainID') %>%
    group_by(DomainName) %>%
    arrange(DomainName) %>%
    mutate( med_5yr = rollapply(temp_anomalies, 24, mean, align='center', fill=NA)) %>%
    ungroup() %>%
    mutate(date = as.POSIXct(paste0(year, '-', month, '-01'), format = "%Y-%m-%d")) %>%
    filter(month %in% c(6, 7, 8)) %>%
    setNames(tolower(names(.))) %>%
    mutate(year = as.integer(year))
  write_rds(temp_mean_anomalies_df, file.path('data', 'climate', 'temp_summer_anomalies_domains.rds'))
  
} else {
  temp_mean_anomalies_df <- read_rds(file.path('data', 'climate', 'temp_summer_anomalies_domains.rds'))
}

if(!file.exists(file.path('data', 'climate', 'tmmn_summer_domains.rds'))) {
  
  tmmn_mean_df <- velox(tmmn_mean)$extract(sp = domains_ll, fun = function(x) mean(x, na.rm = TRUE), df = TRUE) %>%
    as_tibble() 
  colnames(tmmn_mean_df) <- c('ID_sp', names(tmmn_mean))
  tmmn_mean_df_t <- tmmn_mean_df %>%
    gather(key = key, value = tmmn, -ID_sp) %>%
    separate(key,
             into = c("variable", 'year', 'anom'),
             sep = "_") %>%
    separate(anom,
             into = c("anom", 'month'),
             sep = "\\.") %>%
    mutate(DomainID = ifelse(ID_sp == 1, 12, 
                             ifelse(ID_sp == 2, 13, 16))) %>%
    dplyr::select(DomainID, year, month, tmmn) %>%
    left_join(., domains_df, by = 'DomainID') %>%
    group_by(DomainName) %>%
    arrange(DomainName) %>%
    mutate( med_5yr = rollapply(tmmn, 24, mean, align='center', fill=NA)) %>%
    ungroup() %>%
    mutate(date = as.POSIXct(paste0(year, '-', month, '-01'), format = "%Y-%m-%d"),
           tmmn = celsius(tmmn),
           med_5yr = celsius(med_5yr)) %>%    
    filter(month %in% c(6, 7, 8)) %>%
    setNames(tolower(names(.))) %>%
    mutate(year = as.integer(year))
  write_rds(tmmn_mean_df, file.path('data', 'climate', 'tmmn_summer_domains.rds'))
  
} else {
  tmmn_mean_df <- read_rds(file.path('data', 'climate', 'tmmn_summer_domains.rds'))
}
