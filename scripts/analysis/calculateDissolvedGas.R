# SCRIPT FOR CALCULATING OBSERVED AND SATURATED DISSOLVED GAS CONCENTRATIONS
# USES FUNCTIONS FROM FORK OF NEONScience NEON-dissolved-gas PACKAGE.  

# Load function
source("scripts/analysis/def.calc.sdg.R")


# Compile data needed to calculate dissolved gas
# Information is distributed across gc_lake_agg, dg_sheet, and fld_sheet
dissolved_gas_input <- left_join(
  # left_join starting with GC data. Cant calculate dissolved gas concentration
  # without GC data, so start here
  
  # first compile the air and dissolved gas data
  gc_lakeid_agg %>% 
    # extract air and dissolved gas
    filter(type %in% c("dg", "air")) %>%
    select(lake_id, site_id, visit, type, sample_depth_m, 
           n2o_ppm, co2_ppm, ch4_ppm) %>% 
    rename(sample_depth = sample_depth_m) %>%
    pivot_longer(!c(lake_id, site_id, visit, sample_depth, type)) %>%
    pivot_wider(names_from = c(type, name), values_from = value) %>%
    group_by(lake_id, site_id, visit) %>%
    # need to fill "air" measurements
    fill(contains("air"), .direction = "downup") %>%
    # now get rid of row where air concentration data was stored
    filter(if_all(c(sample_depth, dg_n2o_ppm), ~ !is.na(.x))) %>%
    # generalize sampling depth to facilitate join with other data.
    # minor discrepancies in depths recorded on "data" and "dissolved_gas"
    # worksheets cause join to fail. 
    mutate(sample_depth = case_when(sample_depth == 0.1 ~ "shallow",
                                    TRUE ~ "deep")),
  
  # Now get required data from dg_sheet
  dg_sheet %>%
    select(-dg_extn, -dg_notes) %>%
    rename(sample_depth = sample_depth_m) %>%
    group_by(lake_id, site_id, visit, sample_depth) %>%
    summarise(across(everything(), mean)) %>%
    # generalize sampling depth to facilitate join with other data.
    # minor discrepancies in depths recorded on "data" and "dissolved_gas"
    # worksheets cause join to fail. 
    mutate(sample_depth = case_when(sample_depth == 0.1 ~ "shallow",
                                    TRUE ~ "deep"))
) %>% # end first left_join
  left_join(.,
            # Now get water temperature for fld_sheet
            fld_sheet %>%
              select(lake_id, site_id, visit, 
                     sample_depth_s, sample_depth_d,
                     temp_s, temp_d) %>%
              #filter(lake_id == "3") %>% # for development
              pivot_longer(-c(lake_id, site_id, visit)) %>%
              # strip "_d" and "_s"
              mutate(sample_depth_cat = str_sub(name, start = -1),
                     name = str_sub(name, end = -3)) %>% # remove final 2 characters
              pivot_wider(names_from = name, values_from = value) %>%
              # generalize sampling depth to facilitate join with other data.
              # minor discrepancies in depths recorded on "data" and "dissolved_gas"
              # worksheets cause join to fail. 
              mutate(sample_depth = case_when(sample_depth_cat == "s" ~ "shallow",
                                              sample_depth_cat == "d" ~ "deep",
                                              TRUE ~ "FLY YOU FOOLS!")) %>%
              select(-sample_depth_cat) %>%
              rename(water_temperature = temp) 
  ) %>% # end second left_join
  # deal with missing headspace equilibration temperature
  mutate(dg_extn_temp = case_when(!is.na(dg_extn_temp) ~ dg_extn_temp,
                                  is.na(dg_extn_temp) & is.na(water_temperature) ~ air_temperature,
                                  is.na(dg_extn_temp) & !is.na(water_temperature) ~ mean(c(air_temperature, water_temperature)),
                                  TRUE ~ 999999999), # error code
         water_temperature = case_when(lake_id == "207" & site_id == "25" & sample_depth == "deep" ~ 9.4, # from NLA profile
                                       TRUE ~ water_temperature)) %>%
  ungroup()

# check for error flags
dissolved_gas_input %>% 
  filter(dg_extn_temp == 999999999, # none, good
         sample_depth == "FLY YOU FOOLS!")


# calculate dissolved gas concentration
dissolved_gas <- with(dissolved_gas_input, 
                      def.calc.sdg(inputFile = dissolved_gas_input, 
                                            volGas = air_vol, volH2O = water_vol, 
                                            baro = atm_pressure, # units? 
                                            waterTemp = water_temperature, # lake temp     
                                            headspaceTemp = dg_extn_temp, # 
                                            eqCO2 = dg_co2_ppm, # co2 in equilibrated headspace
                                            sourceCO2 = air_co2_ppm, # measured CO2 in gas used for headspace (air)
                                            airCO2 = air_co2_ppm, # measured air CO2
                                            eqCH4 = dg_ch4_ppm, # ch4 in equilibrated headspace
                                            sourceCH4 = air_ch4_ppm, # measured Ch4 in gas used for headspace (air) 
                                            airCH4 = air_ch4_ppm, # measured air ch4
                                            eqN2O = dg_n2o_ppm, # n2o in equilibrated headspace
                                            sourceN2O = air_n2o_ppm, # measured n2o in gas used for headspace (air)
                                            airN2O = air_n2o_ppm)) %>% # measured air n2o
  janitor::clean_names(.) 

# Quick inspection of values before calculating derived quantities
# a few negative CO2 concentrations. set to NA
dissolved_gas %>%
  filter(if_any(contains("dissolved"), ~ .x < 0))

dissolved_gas <- dissolved_gas %>%
  mutate(dissolved_co2 = case_when(dissolved_co2 < 0 ~ NA_real_,
                                   TRUE ~ dissolved_co2))

# Calculate a few derived quantities and add units and flags
dissolved_gas <- dissolved_gas %>%
  mutate(co2_sat_ratio = dissolved_co2 / sat_co2,
         ch4_sat_ratio = dissolved_ch4 / sat_ch4,
         n2o_sat_ratio = dissolved_n2o / sat_n2o) %>%
  mutate(
    # units
    dissolved_co2_units = "mol co2 L-1",
    dissolved_ch4_units = "mol ch4 L-1",
    dissolved_n2o_units = "mol n2o L-1",
    sat_co2_units = "mol co2 L-1",
    sat_ch4_units = "mol ch4 L-1",
    sat_n2o_units = "mol n2o L-1",
    ch4_sat_ratio_units = "unitless",
    co2_sat_ratio_units = "unitless",
    n2o_sat_ratio_units = "unitless",
    
    # flags
    dissolved_co2_flags = NA,
    dissolved_ch4_flags = NA,
    dissolved_n2o_flags = NA,
    sat_co2_flags = NA,
    sat_ch4_flags = NA,
    sat_n2o_flags = NA,
    ch4_sat_ratio_flags = NA,
    co2_sat_ratio_flags = NA,
    n2o_sat_ratio_flags = NA,
    
    # identifier
    sample_type = "unknown") %>%
  ungroup()

