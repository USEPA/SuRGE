names(emissions)

# summarize all emissions by site_id
emissions %>% 
  select(lake_id, site_id, visit, 
         ch4_erate_mg_h, ch4_drate_mg_h_best, ch4_trate_mg_h,
         co2_erate_mg_h, co2_drate_mg_h_best, co2_trate_mg_h) %>%
  pivot_longer(-c(lake_id, site_id, visit)) %>%
  filter(!is.na(value)) %>% # omit rates that couldn't be calculated
  group_by(name) %>% 
  summarize(n_obs = n(),
            n_lake = length(unique(lake_id)),
            min = min(value), # 0.00001 mg CH4/m2/hr
            max = max(value), # 92 mg CH4/m2/hr
            mean = mean(value), # 3.3 mg CH4/m2/hr
            median = median(value),
            iqr25_prop_diff_ch4 = quantile(value, 1/4, na.rm = TRUE),
            iqr75_prop_diff_ch4 = quantile(value, 3/4, na.rm = TRUE),
            n_positive = sum(value >=0)) %>%
  mutate(prop_positive = n_positive / n_obs)

# name                n_obs   n_lake       min      max     mean    median
# ch4_drate_mg_h_best  1299    105        0.0169    1098.   4.32    1.27   
# ch4_erate_mg_h       1376     90        0         92.1    2.64    0.155  
# ch4_trate_mg_h       1012     86        0.0169    358.    5.64    2.09   
# co2_drate_mg_h_best   556     81        -282.     1702.   120.    55.5    
# co2_erate_mg_h       1374     91        0         10.2    0.117   0.00549
# co2_trate_mg_h        408     64        -282.     1702.   121.    54.6 

# summarize all emissions by site_id
emissions %>% 
  select(lake_id, site_id, visit, 
         ch4_trate_mg_h) %>%
  group_by(lake_id) %>%
  summarize(mean_t_ch4 = mean(ch4_trate_mg_h)) %>%
  arrange(mean_t_ch4)
  mutate(prop_positive = n_positive / n_obs)

# summarize diffusion vs ebullition by site_id
emissions %>%
  mutate(prop_diff_ch4 = ch4_drate_mg_h_best / ch4_trate_mg_h) %>%
  summarize(n_total = sum(!is.na(ch4_trate_mg_h)), #1012
            prop_diff_mean_ch4 = mean(prop_diff_ch4, na.rm = TRUE), #75%
            prop_diff_median_ch4 = median(prop_diff_ch4, na.rm = TRUE), #89%
            iqr25_prop_diff_ch4 = quantile(prop_diff_ch4, 1/4, na.rm = TRUE),
            iqr75_prop_diff_ch4 = quantile(prop_diff_ch4, 3/4, na.rm = TRUE))

names(emissions)

poo <- emissions %>%
  select(lake_id, matches("erate|h_best|trate"), -n2o_erate_mg_h) %>%
  pivot_longer(!lake_id) %>%
  group_by(lake_id, name) %>%
  summarize(mean_e = mean(value, na.rm = T),
            median_e = median(value, na.rm = T)) %>%
  mutate(lake_id = gsub("_.*$", "", lake_id) %>% as.numeric) %>%
  left_join(., 
          lake.list %>% select(lake_id, lat_dd83, lon_dd83)) %>%  #fld_sheet %>% select(lake_id, long, lat)
  st_as_sf(coords=c("lon_dd83","lat_dd83"),crs=4269) %>%
  ungroup()

plot(st_geometry(poo))

states <- USAboundaries::us_states() %>%
  dplyr::filter(!state_name %in% c("Alaska", "District of Columbia", "Hawaii", "Puerto Rico")) %>%
  st_transform(4269) # convert to CONUS Albers

ggplot() + 
  geom_sf(data = usa) +
  geom_sf(data = poo, aes(size = mean_e)) +
  facet_wrap(~name)
  

ggplot() + 
  geom_sf(data = usa) +
  geom_sf(data = poo, aes(size = median_e)) +
  facet_wrap(~name)

  geom_sf(aes(color = median_e)) + facet_wrap(~name)

  summary(gc$ch4_ppm/10000)
  