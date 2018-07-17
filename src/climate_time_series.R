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
    dplyr::select(DomainID, year, month, temp_anomalies) %>%
    left_join(., domains_df, by = 'DomainID') %>%
    group_by(DomainName) %>%
    arrange(DomainName) %>%
    mutate( med_5yr =rollapply(pdsi_mean, 24, mean, align='center', fill=NA)) %>%
    ungroup() %>%
    mutate(date = as.POSIXct(paste0(year, '-', month, '-01'), format = "%Y-%m-%d")) %>%
    filter(month %in% c(6, 7, 8))
  write_rds(pdsi_mean_df, file.path('data', 'climate', 'pdsi_summer_mean_domains.rds'))
  
} else {
  pdsi_mean_df <- read_rds(file.path('data', 'climate', 'pdsi_summer_mean_domains.rds'))
}

if(!file.exists(file.path('data', 'climate', 'pdsi_summer_anomalies_domains.rds'))) {
  pdsi_anomalies_df <- velox(pdsi_anomalies)$extract(sp = domains_ll, fun = function(x) mean(x, na.rm = TRUE), df = TRUE) %>%
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
    mutate(DomainID = ifelse(ID_sp == 1, 12, 
                             ifelse(ID_sp == 2, 13, 16))) %>%
    dplyr::select(DomainID, year, month, temp_anomalies) %>%
    left_join(., domains_df, by = 'DomainID') %>%
    group_by(DomainName) %>%
    arrange(DomainName) %>%
    mutate( med_5yr =rollapply(pdsi_anomalies, 24, mean, align='center', fill=NA)) %>%
    ungroup() %>%
    mutate(date = as.POSIXct(paste0(year, '-', month, '-01'), format = "%Y-%m-%d")) %>%
    filter(month %in% c(6, 7, 8))
  write_rds(pdsi_anomalies_df, file.path('data', 'climate', 'pdsi_summer_anomalies_domains.rds'))
} else {
  pdsi_anomalies_df <- read_rds(file.path('data', 'climate', 'pdsi_summer_anomalies_domains.rds'))
}

# tmean time series
if(!file.exists(file.path('data', 'climate', 'temp_summer_mean_domains.rds'))) {
  temp_mean_df <- velox(temp_mean)$extract(sp = domains_ll, fun = function(x) mean(x, na.rm = TRUE), df = TRUE) %>%
    as_tibble() 
  colnames(temp_mean_df) <- c('ID_sp', names(temp_mean))
  temp_mean_df <- temp_mean_df %>%
    gather(key = key, value = temp_mean, -ID_sp) %>%
    separate(key,
             into = c("variable", 'year', 'mean'),
             sep = "_") %>%
    separate(mean,
             into = c("mean", 'month'),
             sep = "\\.") %>%
    mutate(DomainID = ifelse(ID_sp == 1, 12, 
                             ifelse(ID_sp == 2, 13, 16))) %>%
    dplyr::select(DomainID, year, month, temp_anomalies) %>%
    left_join(., domains_df, by = 'DomainID') %>%
    group_by(DomainName) %>%
    arrange(DomainName) %>%
    mutate( med_5yr = rollapply(temp_mean, 24, mean, align='center', fill=NA)) %>%
    ungroup() %>%
    mutate(date = as.POSIXct(paste0(year, '-', month, '-01'), format = "%Y-%m-%d")) %>%
    filter(month %in% c(6, 7, 8))
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
    filter(month %in% c(6, 7, 8))
  write_rds(temp_mean_anomalies_df, file.path('data', 'climate', 'temp_summer_anomalies_domains.rds'))
  
} else {
  temp_mean_anomalies_df <- read_rds(file.path('data', 'climate', 'temp_summer_anomalies_domains.rds'))
}

# Plot the time series for summer months of temp and pdsi

temp_mean_anomalies_p <- temp_mean_anomalies_df %>%
  filter(date >= "1984-01-01" & date <= "2016-12-01") %>%
  ggplot(aes(x = date, y = med_5yr, color = DomainName, group = DomainName)) +
  #geom_point(alpha = 0.15) +
  #geom_line(size = 0.5, alpha = 0.15) +
  geom_line(aes(y = med_5yr), size = 0.5) +
  geom_hline(yintercept = 0, size = 0.5) + 
  scale_y_continuous(limits = c(-1.25, 1.5)) +
  scale_x_datetime(date_breaks = "4 year", 
                   date_labels = "%Y", expand = c(0, 0),
                   limits = c(
                     as.POSIXct("1984-01-01"),
                     as.POSIXct("2016-12-01")
                   )) + 
  labs(x = "Year", y = "Anomolous mean \nsummer temperature", color = "Regions relative\n to the continential divide") +
  theme_pub() + theme(legend.position = c(0.2, 0.85))


pdsi_mean_p <- pdsi_mean_df %>%
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

temp_mean_p <- temp_mean_df %>%
  filter(date >= "1984-01-01" & date <= "2016-12-01") %>%
  ggplot(aes(x = date, y = med_5yr, color = regions, group = regions)) +
  #geom_point(alpha = 0.15) +
  #geom_line(size = 0.5, alpha = 0.15) +
  geom_line(aes(y = med_5yr), size = 0.5) +
  geom_hline(yintercept = 0, size = 0.5) + 
  scale_y_continuous(limits = c(-1.25, 1.5)) +
  scale_x_datetime(date_breaks = "4 year", 
                   date_labels = "%Y", expand = c(0, 0),
                   limits = c(
                     as.POSIXct("1984-01-01"),
                     as.POSIXct("2016-12-01")
                   )) + 
  labs(x = "Year", y = "Anomolous mean \nsummer temperature", color = "Regions relative\n to the continential divide") +
  theme_pub() + theme(legend.position = c(0.2, 0.85))

library(gridExtra)
grid.arrange(pdsi_ts, temp_ts, nrow = 2)
g <- arrangeGrob(pdsi_ts, temp_ts, nrow = 2)

ggsave(file = "figures/timeseries_regions.pdf", g, width = 5, height = 4,
       scale = 4, dpi = 600, units = "cm") #saves p

