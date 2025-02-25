# TEMPERATURE ADJUSTMENT FUNCTION-------------
adj_temp <- function(flux, # preferred units
                     temp_obs, # C
                     temp_pred, # C
                     Em = 0.96){ # eV, default value
  # Em = 0.96 [0.82 - 1.03] eV, Yvon-Durocher et al. 2014 for CH4
  # not sure how to deal with CO2. 0.3 for photosynthesis, 0.65 for respiration, Yvon-Durocher et al. 2014 
  k <- 8.62e-5 # Boltzmann constant eV K-1
  temp_obs <- temp_obs + 273.15
  temp_pred <- temp_pred + 273.15
  flux_sign <- ifelse(flux < 0, -1, 1) # flag for negative CO2 values
  flux <- abs(flux) # abs for - CO2 values, otherwise breaks log
  
  exp(Em * ((1 / (k * temp_obs)) - (1 / (k * temp_pred))) + log(flux)) * flux_sign# natural log, 
  
  # troubleshooting
  #exp(0.96 * ((1 / (8.62e-5 * 301.05)) - (1 / (8.62e-5 * 281.85))) + log(10.3)) 
}


# DATA---------------

# Monthly mean temperature for each lake
met_temp # readGriddedTemp.R
nrow(met_temp) # 146
met_temp %>% distinct(lake_id) %>% summarize(n()) # 146 lakes


# aggregated emission rates
emissions_agg # need to switch to dat_agg when that object is ready
emissions_agg %>% distinct(lake_id) %>% summarize(n()) # 146 lakes

# All lakes in both datasets?
met_temp %>% filter(!(lake_id %in% emissions_agg$lake_id)) # Falls Lake not in emissions
emissions_agg %>% filter(!(lake_id %in% met_temp$lake_id)) # lake 45 not in met_temp. coastal not in ERA5 Land


# JOIN EMISSIONS_AGG, METEOROLOGY, AND SAMPLE DATE. ANNUALIZE------------------
emissions_agg_annual <- inner_join( # keep all common records. Missing 1033 and 45
  emissions_agg,
  met_temp) %>% 
  #distinct(lake_id) # 145 lakes, 149 unique site x visit
  # slice(1) %>% testing
  select(lake_id, visit, 
         ch4_ebullition_lake, ch4_diffusion_lake, ch4_total_lake,
         # don't know what activation energy to use for CO2.
         # see adj_temp function
         #co2_ebullition_lake, co2_diffusion_lake, co2_total_lake,
         contains("air_temp") & contains("mean"),
         contains("mix_layer_temp") & contains("mean")) %>% # air and mixed layer temp
  pivot_longer(!c(lake_id, visit, matches(c("ch4|co2")))) %>%
  mutate(temp_source = case_when(grepl("air_temp", name) ~ "air_temp",
                                 grepl("mix_layer_temp", name) ~ "mixed_layer",
                                 TRUE ~ "FLY YOU FOOLS")) %>% 
  # pull month name out of air temperature variable
  # https://www.statology.org/r-extract-string-between-characters/
  mutate(prediction_month = str_match(name, "_\\s*(.*?)\\s*_")[,2] %>% # extracts month abbreviation
           substr(., 1, 3) %>% # grab 3 letter abbreviation (consistent with base::month.abb)
           str_to_title() %>% # capitalize first letter (consistent with base::month.abb)
           match(., month.abb),
         # now get the number of days in each prediction month. Needed to sum emissions across months
         days_in_month = lubridate::days_in_month(prediction_month)) %>% # convert to numeric month of year
  # join sample month
  left_join(dat %>%
              as_tibble %>%
              select(lake_id, visit, trap_deply_date_time) %>% # assume trap deployment time stamp
              mutate(obs_month = format(trap_deply_date_time, "%m") %>% as.numeric) %>% # convert to month
              select(-trap_deply_date_time) %>% # drop date_time object
              distinct %>% # collapse to distinct per lake x visit
              drop_na %>%
              # some lake x visit encompass >1 month
              # collapse to 1 month. take mean of multiple months, then round
              group_by(lake_id, visit) %>% 
              summarize(obs_month = mean(obs_month) %>% round)) %>%
  # execute by lake, visit, and temperature source (air, mixed layer)
  group_by(lake_id, visit, temp_source) %>%
  # create column holding mean temperature for month of sampling
  mutate(temp_obs = case_when(obs_month == prediction_month ~ value,
                              TRUE ~ NA)) %>%
  # fill column of mean temperature during month of sampling
  tidyr::fill(temp_obs, .direction = "downup") %>%
  # Calculate mean flux rate per month [mg CH4|CO2 m-2 h-1]
  # multiply by hours in day * days in month
  # final units: mg ch4|co2 / month
  mutate(across(contains("ch4"), ~adj_temp(flux = ., # mg CH4 m-2 h-1
                                             temp_obs = temp_obs,
                                             temp_pred = value) * 24 * days_in_month)) %>%
  # troubleshooting
  # filter(lake_id == 1) %>% select(lake_id, visit, contains("ch4_ebullition"), name, value, temp_obs, days_in_month) %>%
  # mutate(ch4_ebullition_lake = ch4_ebullition_lake * 24 * days_in_month)
  # aggregate flux across year, scale to mg CH4|CO2 m-2 h-1
  # sum gives mg ch4|co2 per year. divide by 365*24 to get back to mg m-2 h-1
  summarize(across(matches("_lake"), ~sum(.) / (365 * 24), .names = "{.col}_annual")) %>% # mg CH4|CO2 m-2 h-1
  ungroup()
  
# MERGE ANNUALIZED EMISSIONS WITH SUMMER LAKE-SCALE EMISSIONS-----------
# annual is always less than summer
inner_join(emissions_agg,
           emissions_agg_annual) %>%
  select(-matches(c("std_error|margin_of_error|95pct|units"))) %>%
  pivot_longer(!c(lake_id, visit, temp_source)) %>%
  mutate(temporal_scale = case_when(grepl("_annual", name) ~ "annual",
                                    TRUE ~ "summer"),
         name = gsub("_annual", "", name)) %>%
  pivot_wider(names_from = temporal_scale, values_from = value) %>%
  mutate(diff = summer - annual) %>%
  ggplot(aes(1, diff)) +
  geom_point() +
  facet_wrap(~name, scales = "free")

# annual is always less than summer
inner_join(emissions_agg,
           emissions_agg_annual) %>%
  ggplot(aes(ch4_ebullition_lake_annual, ch4_ebullition_lake)) +
  #ggplot(aes(co2_total_lake_annual, co2_total_lake)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0) +
  facet_wrap(~temp_source) +
  ylab("summer total CH4 emission") +
  xlab("annualized CH4 emission")

# compare air vs mixed layer temp adjustment
 emissions_agg_annual %>%
   pivot_longer(!c(lake_id, visit, temp_source)) %>%
  pivot_wider(values_from = value, names_from = c(name, temp_source)) %>%
  ggplot(aes(ch4_total_lake_annual_air_temp, ch4_total_lake_annual_mixed_layer)) +
  #ggplot(aes(co2_total_lake_annual, co2_total_lake)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0) +
   ylab("total annualized CH4 (mixed layer temp)") +
   xlab("total annualized CH4 (mixed layer temp)")
 