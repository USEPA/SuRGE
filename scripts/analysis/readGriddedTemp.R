# 2m air temp, mixed layer temp, bottom water temp from ERA5

# READ DATA------------
# decadal means
met_temp <- read_csv(paste0(userPath, "data/siteDescriptors/RTP_gridded_data/Lake_ERA5LAND_TEMP_R0.csv")) %>%
  janitor::clean_names()

# on hour of chamber deployment
met_chamber <- read_csv(file.path(userPath,"/data/siteDescriptors/",
                                  "RTP_gridded_data/Sites/Chamber/",
                                  "Chamber_Precip_Temp_Wind.csv")) %>%
  janitor::clean_names() %>%
  select(-wind_u_ms, -wind_v_ms) %>% # don't need wind components
  mutate(precipitation_units = "m",
         wind_speed_units = "m s-1",
         temp_air_2m = temperature_2m_k - 273.15,
         temp_air_2m_units = "C",
         date_time_units = "UTC") %>%
  rename(precipitation = precipitation_m,
         wind_speed = wind_speed_ms,
         date_time = std_time) %>%
  select(-temperature_2m_k)

# DECADAL DATA PREVIEW-------
# French Creek example
met_temp %>%
  select(-value, -lake_var_id, -variable, -contains("std")) %>%
  pivot_longer(cols = -c("lake_name", "variable_name")) %>%
  mutate(month = sub(".*\\_", "", name),
         month = substr(month, 1, 3),
         month = factor(month, levels = tolower(month.abb)),
         value = value - 273.15) %>%
  select(-name) %>%
  filter(lake_name == "French Creek") %>%
  ggplot(aes(month, value)) +
  geom_point(aes(color = variable_name)) +
  geom_point(data = tribble(
    ~value, ~month, ~variable_name, ~observed,
    32.3, "aug", "Lake Mix Layer Temp", "observed",
    29.5, "aug", "Lake Bottom Temp", "observed"),
    aes(month, value, color = variable_name, shape = observed)) +
  scale_shape_manual(values = 17) +
  ggtitle("French Creek")


# all lakes
met_temp %>%
  select(-value, -lake_var_id, -variable, -contains("std")) %>%
  pivot_longer(cols = -c("lake_name", "variable_name")) %>%
  mutate(month = sub(".*\\_", "", name),
         month = substr(month, 1, 3),
         month = factor(month, levels = tolower(month.abb)),
         value = value - 273.15) %>%
  select(-name) %>%
  ggplot(aes(month, value)) +
  geom_point(aes(color = variable_name)) +
  facet_wrap(~lake_name)

# Dacey Reservoir
met_temp %>%
  select(-value, -lake_var_id, -variable, -contains("std")) %>%
  pivot_longer(cols = -c("lake_name", "variable_name")) %>%
  mutate(month = sub(".*\\_", "", name),
         month = substr(month, 1, 3),
         month = factor(month, levels = tolower(month.abb)),
         value = value - 273.15) %>%
  select(-name) %>%
  filter(lake_name == "Dacey Reservoir") %>%
  ggplot(aes(month, value)) +
  geom_point(aes(color = variable_name)) +
  geom_point(data = tribble(
    ~value, ~month, ~variable_name, ~observed,
    25.5, "aug", "Lake Mix Layer Temp", "observed",
    25.2, "aug", "Lake Bottom Temp", "observed"),
    aes(month, value, color = variable_name, shape = observed)) +
  scale_shape_manual(values = 17) +
  ggtitle("Dacey Reservoir")



# CHAMBER DATA PREVIEW
### NEW DATA FILES DON'T HAVE ERA5 TEMP [10/15/2025]
# era5_bias_dat <- inner_join(met_chamber,
#                             fld_sheet %>%
#                               select(lake_id, site_id, visit, temp_s) %>%
#                               mutate(
#                                 # move habitat from lake_id to site_id
#                                 site_id = case_when(grepl("lacustrine", lake_id) ~ paste0(site_id, "_lacustrine"),
#                                                     grepl("transitional", lake_id) ~ paste0(site_id, "_transitional"),
#                                                     grepl("riverine", lake_id) ~ paste0(site_id, "_riverine"),
#                                                     TRUE ~ as.character(site_id)),
#                                 # remove transitional, lacustrine, riverine from lake_id
#                                 # retain character class initially, then convert to numeric.
#                                 lake_id = case_when(lake_id %in% c("69_lacustrine", "69_riverine", "69_transitional") ~ "69",
#                                                     lake_id %in% c("70_lacustrine", "70_riverine", "70_transitional") ~ "70",
#                                                     TRUE ~ lake_id),
#                                 lake_id = as.numeric(lake_id))
# )
# 
# # plot
#   ggplot(era5_bias_dat, aes(temp_s, temp_lake_mix_layer_c)) +
#   geom_point() +
#   geom_abline(slope = 1, intercept = 0) +
#   geom_smooth(method = "lm")
# 
# # equation
# era5_mod <- lm(temp_lake_mix_layer_c ~ temp_s, data = era5_bias_dat)
# 
# # compare predicted and observed
# # simulated era5 data
# sim_temp <- seq(from = min(era5_bias_dat$temp_s, na.rm = TRUE),
#                 to = max(era5_bias_dat$temp_s, na.rm = TRUE),
#                 by = 1)
# 
# # 3.2 degree cold bias
# tibble(true_temp = sim_temp,
#        predicted_temp = predict(object = era5_mod,
#                                 newdata = data.frame(
#                                   temp_s = sim_temp))) %>%
#   dplyr::rowwise() %>%
#   mutate(temp_bias = true_temp - predicted_temp) %>%
#   ungroup %>%
#   summarise(temp_bias = mean(temp_bias))
# 


# PREP DATA FOR MERGE WITH OTHER VARIABLES---------------- 
# reshape to wide for merge with all_obs
met_temp <- met_temp %>%
  mutate(variable_name = case_when(variable_name == "2m Temp" ~ "2m_air_temp",
                                   TRUE ~ variable_name)) %>%
  mutate(across(where(is.numeric) & !value, ~.x - 273.15)) %>% # kelvin to celsius
  select(-lake_name, -variable, -lake_var_id) %>%
  rename(lake_id = value) %>%
  pivot_wider(names_from = variable_name, values_from = mean_jan:std_dec) %>%
  janitor::clean_names()

# GATHER ELEVATION DATA FOR BAROMETRIC PRESSURE CORRECTIONS

con <- gzfile(paste0(userPath,
                     "data/siteDescriptors/lake_information.csv.gz"), "rt")  # "rt" for reading text
lagos_elev <- read.csv(con)            
close(con)  

# lagos_elev<- readr::read_csv(paste0(userPath,
#                          "data/siteDescriptors/lake_information.csv"))

elevation_lagos<- lagos_elev %>%
  filter(lagoslakeid %in% lagos_links$lagoslakeid) %>%
  select (lagoslakeid,lake_elevation_m,lake_nhdid)

el<-left_join(elevation_lagos, lagos_links)%>%
  select(lake_id,lake_elevation_m)%>%
  distinct()

elevation<-lake.list.all %>%
  select(lake_id,elevation) %>%
  mutate(lake_id= as.numeric(case_when(lake_id %in% c("69_riverine", "69_transitional", "69_lacustrine") ~ "69",
                                      lake_id %in% c("70_riverine", "70_transitional", "70_lacustrine") ~ "70",
                                      TRUE ~ lake_id)))%>%
  left_join(el)%>%
  #elevations are in meters above sea level. Elevations for lakes 1009 and 1010 are in NGVD29 datum 
  mutate(lake_elevation=ifelse(!is.na(lake_elevation_m), lake_elevation_m,
                               ifelse(lake_id=="1000",96.2,
                                            ifelse(lake_id=="1009",313.3 ,
                                                  ifelse(lake_id=="1010", 223.1, 
                                                         elevation)))))%>%
  group_by(lake_id) %>%
  summarise(lake_elevation=lake_elevation[1])%>%
  filter(lake_id != 1033) # omit Falls Lake

#Write csv for jeremy
#write.csv(elevation,file="output/SuRGE_elevations.csv")
  
