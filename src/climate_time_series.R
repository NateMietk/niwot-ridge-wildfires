domains_df <- as.data.frame(domains) %>%
  dplyr::select(DomainID)

# pdsi time series
if(!exists('pdsi_mean_df')) {
  pdsi_mean_df <- velox(pdsi_mean)$extract(sp = domains, fun = function(x) mean(x, na.rm = TRUE), df = TRUE) %>%
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
    mutate(DomainID = ID_sp) %>%
    dplyr::select(DomainID, year, month, pdsi_mean) %>%
    left_join(., cd_df, by = 'id') %>%
    group_by(regions) %>%
    arrange(regions) %>%
    mutate( med_5yr =rollapply(pdsi_mean, 24, mean, align='center', fill=NA)) %>%
    ungroup() %>%
    mutate(date = as.POSIXct(paste0(year, '-', month, '-01'), format = "%Y-%m-%d")) %>%
    filter(month %in% c(6, 7, 8))
}

if(!exists('pdsi_anomalies_df')) {
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
    dplyr::select(id, year, month, pdsi_anomalies) %>%
    left_join(., cd_df, by = 'id') %>%
    group_by(regions) %>%
    arrange(regions) %>%
    mutate( med_5yr =rollapply(pdsi_anomalies, 24, mean, align='center', fill=NA)) %>%
    ungroup() %>%
    mutate(date = as.POSIXct(paste0(year, '-', month, '-01'), format = "%Y-%m-%d")) %>%
    filter(month %in% c(6, 7, 8))
}

# tmean time series
if(!exists('temp_mean_df')) {
  temp_mean_df <- velox(temp_mean)$extract(sp = cd, fun = function(x) mean(x, na.rm = TRUE), df = TRUE) %>%
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
    mutate(id = ID_sp) %>%
    dplyr::select(id, year, month, temp_mean) %>%
    left_join(., cd_df, by = 'id') %>%
    group_by(regions) %>%
    arrange(regions) %>%
    mutate( med_5yr = rollapply(temp_mean, 24, mean, align='center', fill=NA)) %>%
    ungroup() %>%
    mutate(date = as.POSIXct(paste0(year, '-', month, '-01'), format = "%Y-%m-%d")) %>%
    filter(month %in% c(6, 7, 8))
}

if(!exists('temp_anomalies_df')) {
  
temp_anomalies_df <- velox(temp_anomalies)$extract(sp = cd, fun = function(x) mean(x, na.rm = TRUE), df = TRUE) %>%
  as_tibble() 
colnames(temp_anomalies_df) <- c('ID_sp', names(temp_anomalies))
temp_anomalies_df <- temp_anomalies_df %>%
  gather(key = key, value = temp_anomalies, -ID_sp) %>%
  separate(key,
           into = c("variable", 'year', 'anom'),
           sep = "_") %>%
  separate(anom,
           into = c("anom", 'month'),
           sep = "\\.") %>%
  mutate(id = ID_sp) %>%
  dplyr::select(id, year, month, temp_anomalies) %>%
  left_join(., cd_df, by = 'id') %>%
  group_by(regions) %>%
  arrange(regions) %>%
  mutate( med_5yr = rollapply(temp_anomalies, 24, mean, align='center', fill=NA)) %>%
  ungroup() %>%
  mutate(date = as.POSIXct(paste0(year, '-', month, '-01'), format = "%Y-%m-%d")) %>%
  filter(month %in% c(6, 7, 8))


}

# Plot the time series for summer months of temp and pdsi

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

