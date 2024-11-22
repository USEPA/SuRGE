# 2m air temp, mixed layer temp, bottom water temp from ERA5

# READ DATA------------
met_temp <- read_csv(paste0(userPath, "data/siteDescriptors/RTP_gridded_data/Temp/Lake_ERA5LAND_TEMP_R0.csv")) %>%
  janitor::clean_names()



# DATA PREVIEW-------
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
