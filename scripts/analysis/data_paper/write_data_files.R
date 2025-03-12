
# 1. MASTER DICTIONARY
#
master_dictionary <- tribble(~variable, ~definition,
                                     "site_id", "Unique identifier for sample site within a waterbody",
                                     "lake_id", "Unique identifier for each waterbody",
                                     "visit", "First (1) or second (2) site visit",
                                     
                                     "sample_depth", "Measurement depth",
                                     "sample_depth_units", "Units for sample depth",
                                     
                                     "temp", "Water temperature",
                                     "temp_units", "Units for temp field",
                                     
                                     "do", "Dissolved oxygen concentration",
                                     "do_sat", "Dissolved oxygen concentration expressed as percent saturation",
                                     "do_units", "Dissolved oxygen concentration units",
                                     "do_flag", "Value of 1 if dissolved oxygen failed post-deployment calibration check or value was otherwise suspicious",
                                     "do_comment", "Field or data analyst notes pertaining to dissolved oxygen concentration measurement",
                                     
                                     
                                     "sp_cond", "Specific conductivity",
                                     "sp_cond_units", "Specific conductivity units",
                                     "sp_cond_flag", "Value of 1 if specific conductivity failed post-deployment calibration check or value was otherwise suspicious",
                                     "sp_cond_comment", "Field or data analyst notes pertaining to specific conductivity measurement",
                                     
                                     
                                     "ph", "pH",
                                     "ph_flag", "Value of 1 if pH failed post-deployment calibration check or value was otherwise suspicious",
                                     "ph_comment",  "Field or data analyst notes pertaining to pH measurement",
                                     
                                     
                                     "turbidity", "Turbidity",
                                     "turbidity_units", "Turbidity units",
                                     "turbidity_flag", "Value of 1 if turbidity failed post-deployment calibration check or value was otherwise suspicious",
                                     "turbidity_comment", "Field or data analyst notes pertaining to turbidity measurement",
                                     
                                     
                                     "chla_sonde", "Chlorophyll a concentration measured with sonde",
                                     "chla_sonde_units", "Units of sonde-based chlorophyll a measurement",
                                     "chla_sonde_flag", "Value of 1 if sonde-based chlorophyll a measurement failed post-deployment calibration check or value was otherwise suspicious",
                                     "chla_sonde_comment", "Field or data analyst notes pertaining to sonde-based chlorophyll a measurement",
                                     
                                     "phycocyanin_sonde", "Phycocyanic concentration measured with sonde",
                                     "phycocyanin_units", "Units of sonde-based phycocyanic measurement",
                                     "phycocyanin_sonde_flag", "Value of 1 if sonde-based pycocyanin measurement failed post-deployment calibration check or value was otherwise suspicious",
                                     "phycocyanin_sonde_comment", "Field or data analyst notes pertaining to sonde-based phycocyanin measurement",
                             
                            # existing data links
                             "nhdplus_comid", "NDHPlusV2 waterbody comid",
                             "hylak_id", "HydroLAKES unique id",
                            "nla07_site_id"
                            "gnis_id"
                            "lagoslakeid"
                            "nla12_site_id"
                            "nid_id"
                            "nla17_site_id"
                            
                            # morphometry
                             "surface_area", "Waterbody surface area",
                             "shoreline_length", "Length of waterbody shoreline",
                             "shoreline_development", "Waterbody perimeter (m) to area (m2) ratio. Area is square-rooted to account for size dependency.",
                             "max_width", "Maximum lake width is defined as the maximum in lake distance that is perpendicular to the fetch."
                             "mean_width", "Lake surface area divided by fetch",
                             "max_length", "Maximum lake length is defined as the longest open water distance of a lake.",
                             "circularity", "Circularity compares the area of the lake (m2) with the area of the minimum boundary circle (MBC; m2) around it. It differentiates between circular (value approaches 1) and elongated lakes (value approaches 0).",
                             "mean_depth_measured", "Mean of depth measured at probabilistic survey sites within a lake",
                             "max_depth_measured", "Maximum measured depth at probabilistic survey sites within a lake",
                             
                             # sedimentation
                             "sedimentation", "Total sedimentation rate.",
                             "sediment_oc", "Sediment organic carbon content", 
                             "basin_slope"
                             "basin_forest"
                             "clow_surface_area"
                             "basin_crop"
                             "basin_wetland"
                             "basin_kfact"
                             "basin_soc0_5"
                             "basin_barren"
                             
# NID
                             "year_completed",
                             
                             # water isotope
                             "e_i"
                             "sd_e_i"
                             "retention_time"
                             "sd_retention_time"
                             "retention_time_ei_repeat_visits"
                             "e_i_type"
                             
                             # survey design variables
                             "wgt"
                             "ag_eco9"
                             "ag_eco9_nm"
                             "depth_cat"
                             "chla_cat")
                             
                             
                             
                             
                             
                                     
                                     
)

# 3. LAKE SCALE VALUES-----------
lake_scale_data_paper <- list(
  
  # e.	Links to existing data
  # this needs to be long due to numerous nhdplus_comid, lagos, etc values per lake
  read.csv(paste0(userPath, "data\\siteDescriptors\\surge_master_crosswalk_long_hollister.csv"), header = T) %>% 
    select(-lake_name) %>% 
    # these are duplicated in nl07_site_id and nhdplus_comid
    filter(!(join_id_name %in% c("lmorpho_comid", "lmorpho_nla07", "hylak_comid"))) %>% 
    mutate(join_id_name = replace(join_id_name, join_id_name == "nl07_site_id", "nla07_site_id")) %>%
    rename(name = join_id_name,
           value = join_id) %>%
    mutate(units = NA),

    # b.	Morphometry indices
  morpho %>% 
    # most depth-based variables not ready 3/10/2025
    select(lake_id, surface_area, shoreline_length, shoreline_development, max_width, 
           mean_width, max_length, circularity, mean_depth_measured, max_depth_measured) %>%
    pivot_longer(!lake_id) %>%
    mutate(units = case_when(name == "surface_area" ~ "m2",
                             name == "shoreline_development" ~ "dimensionless",
                             name == "circularity" ~ "dimensionless",
                             TRUE ~ "m")), # all others meters
  
  # c.	Sedimentation rates
  sedimentation_link %>%
    select(-sediment_predictor_type) %>% # not sure about this variable
    mutate(across(!lake_id, as.character)) %>%
    # subset for development
    #filter(lake_id == 100) %>%
    #select(lake_id, contains("sedimentation"), contains("sediment")) %>%
    # all variables presenting a value must end with "_value". All variables
    # presenting units already end with "_units"
    rename_with(~ifelse(!grepl(c("units|lake_id"), .x), paste0(.x, "_value"), .x)) %>%
    pivot_longer(-lake_id, 
                 # anything to left of pattern is "name"
                 # every matching group to right creates new value column
                 names_to = c("name", ".value"), 
                 # breaking pattern in final _
                 names_pattern = "(.+)_(.+)"),
  
  # d.	Year of construction
  nid_link %>%
    select(lake_id, year_completed) %>%
    pivot_longer(!lake_id) %>%
    mutate(units = "Gregorian calendar year"),
  
  # f.	E:I and Residence Time estimates
  water_isotope_agg %>%
    mutate(lake_id = as.numeric(lake_id),
           across(-lake_id, as.character)) %>% # allow character and numeric in same column
    # all variables presenting a value must end with "_value". All variables
    # presenting units already end with "_units"
    rename_with(~ifelse(!grepl(c("units|lake_id"), .x), paste0(.x, "_value"), .x)) %>%
    pivot_longer(!lake_id,
                 # anything to left of pattern is "name"
                 # every matching group to right creates new value column
                 names_to = c("name", ".value"), 
                 # breaking pattern in final _
                 names_pattern = "(.+)_(.+)"),
  
  # survey design parameters
  bind_rows(lake.list, lake.list.2016) %>%
    filter(eval_status_code == "S",
           visit == 1) %>%
    select(lake_id, wgt, ag_eco9, ag_eco9_nm, depth_cat, chla_cat) %>%
    mutate(wgt_units = "dimensionless",
           across(!lake_id, as.character)) %>% # needed to pivot all values to one column
    # all variables presenting a value must end with "_value". All variables
    # presenting units already end with "_units"
    rename_with(~ifelse(!grepl(c("units|lake_id"), .x), paste0(.x, "_value"), .x)) %>%
    pivot_longer(!lake_id,
                 # anything to left of pattern is "name"
                 # every matching group to right creates new value column
                 names_to = c("name", ".value"), 
                 # breaking pattern in final _
                 names_pattern = "(.+)_(.+)")
    
  ) %>%
    map(., ~.x %>% mutate(value = as.character(value))) %>% # character to enable all to collapse into one column
    map_dfr(., bind_rows) 

#write.table(unique(lake_scale_data_paper$name), file = "clipboard", row.names = FALSE)


# 4. DEPTH PROFILES----
# see readDepthProfiles.R for primary data source
# depth_profile_69_70 is just lacustrine sites. depth_profile_surge
# includes riverine and transitional. Although all other lakes only have a single
# depth profile, lets report all zones in the paper.
depth_profiles_data_paper <- list(
  # lacustrine zone first
  depth_profile_69_70 %>%
    mutate(lake_id = sub("_.*", "", lake_id), # extract string before first _
           site_id = paste0(site_id, "_lacustrine")), # move lacustrine to site_id
  
  # riverine and transitional zones
  depth_profile_surge %>%
    mutate(
      site_id = case_when(grepl("transitional", lake_id) ~ paste0(site_id, "_transitional"),
                          grepl("riverine", lake_id) ~ paste0(site_id, "_riverine"),
                          TRUE ~ as.character(site_id)),
      # remove transitional, riverine from lake_id
      # retain character class initially, then convert to numeric.
      lake_id = case_when(lake_id %in% c("69_riverine", "69_transitional") ~ "69",
                          lake_id %in% c("70_riverine", "70_transitional") ~ "70",
                          TRUE ~ lake_id)),
  
  # 2016 data
  depth_profile_2016) %>%
  map(., ~.x %>% 
        mutate(lake_id = as.numeric(lake_id),
               site_id = as.character(site_id))) %>%
  map_dfr(., bind_rows) # rbinds into one df

depth_profiles_data_paper_dictionary <- master_dictionary %>%
  filter(variable %in% colnames(depth_profiles_data_paper))
