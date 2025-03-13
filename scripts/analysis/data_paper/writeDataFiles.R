
# 1. MASTER DICTIONARY--------------
#
master_dictionary <- tribble(~variable, ~definition,
                             # COMMON TO SEVERAL FILES
                             "name", "Analyte name",
                             "site_id", "Unique identifier for sample site within a waterbody",
                             "lake_id", "Unique identifier for each waterbody",
                             "visit", "First (1) or second (2) site visit",
                             "units", "Measurement units",
                             "comment", "Comment pertaining to measurement",
                             
                             # DEPTH PROFILES
                             "sample_depth", "Measurement depth",
                             "sample_depth_units", "Units for sample depth",
                             
                             "temp", "Water temperature",
                             "temp_units", "Units for temp field",
                             
                             "do", "Dissolved oxygen concentration",
                             "do_sat", "Dissolved oxygen concentration expressed as percent saturation",
                             "do_units", "Dissolved oxygen concentration units",
                             "do_flag", "Value of 1 if dissolved oxygen failed post-deployment calibration check or value was otherwise suspicious, value of I if data was interpolated",
                             "do_comment", "Field or data analyst notes pertaining to dissolved oxygen concentration measurement",
                             
                             
                             "sp_cond", "Specific conductivity",
                             "sp_cond_units", "Specific conductivity units",
                             "sp_cond_flag", "Value of 1 if specific conductivity failed post-deployment calibration check or value was otherwise suspicious, value of I if data was interpolated",
                             "sp_cond_comment", "Field or data analyst notes pertaining to specific conductivity measurement",
                             
                             
                             "ph", "pH",
                             "ph_flag", "Value of 1 if pH failed post-deployment calibration check or value was otherwise suspicious, value of I if data was interpolated",
                             "ph_comment",  "Field or data analyst notes pertaining to pH measurement",
                             
                             
                             "turbidity", "Turbidity",
                             "turbidity_units", "Turbidity units",
                             "turbidity_flag", "Value of 1 if turbidity failed post-deployment calibration check or value was otherwise suspicious, value of I if data was interpolated",
                             "turbidity_comment", "Field or data analyst notes pertaining to turbidity measurement",
                             
                             
                             "chla_sonde", "Chlorophyll a concentration measured with sonde",
                             "chla_sonde_units", "Units of sonde-based chlorophyll a measurement",
                             "chla_sonde_flag", "Value of 1 if sonde-based chlorophyll a measurement failed post-deployment calibration check or value was otherwise suspicious, value of I if data was interpolated",
                             "chla_sonde_comment", "Field or data analyst notes pertaining to sonde-based chlorophyll a measurement",
                             
                             "phycocyanin_sonde", "Phycocyanic concentration measured with sonde",
                             "phycocyanin_units", "Units of sonde-based phycocyanic measurement",
                             "phycocyanin_sonde_flag", "Value of 1 if sonde-based pycocyanin measurement failed post-deployment calibration check or value was otherwise suspicious",
                             "phycocyanin_sonde_comment", "Field or data analyst notes pertaining to sonde-based phycocyanin measurement",
                             
                             # LAKE SCALE DATA SET
                             # existing data links
                             "nhdplus_comid", "NDHPlusV2 waterbody comid",
                             "hylak_id", "HydroLAKES unique id",
                             "nla07_site_id", "Unique ID assigned to lakes included in EPA's 2007 National Lakes Assessment.",
                             "gnis_id", "A permanent, unique number assigned by the Geographic Names Information System (GNIS) to a geographic feature name for the sole purpose of uniquely identifying that name application as a record in any information system database, dataset, file, or document",
                             "lagoslakeid", "Unique ID assigned to lakes included in the LAGOS dataset (https://lagoslakes.org/).",
                             "nla12_site_id", "Unique ID assigned to lakes included in EPA's 2012 National Lakes Assessment.",
                             "nid_id", "Unique ID assigned to dams in the USACE National Inventory of Dams dataset.",
                             "nla17_site_id", "Unique ID assigned to lakes included in EPA's 2017 National Lakes Assessment.",
                             
                             # morphometry
                             "surface_area", "Waterbody surface area",
                             "shoreline_length", "Length of waterbody shoreline",
                             "shoreline_development", "Waterbody perimeter (m) to area (m2) ratio. Area is square-rooted to account for size dependency.",
                             "max_width", "Maximum lake width is defined as the maximum in lake distance that is perpendicular to the fetch.",
                             "mean_width", "Lake surface area divided by fetch",
                             "max_length", "Maximum lake length is defined as the longest open water distance of a lake.",
                             "circularity", "Circularity compares the area of the lake (m2) with the area of the minimum boundary circle (MBC; m2) around it. It differentiates between circular (value approaches 1) and elongated lakes (value approaches 0).",
                             "mean_depth_measured", "Mean of depth measured at probabilistic survey sites within a lake",
                             "max_depth_measured", "Maximum measured depth at probabilistic survey sites within a lake",
                             
                             # sedimentation
                             "sedimentation", "Total sedimentation rate.",
                             "sediment_oc", "Sediment organic carbon content", 
                             "basin_slope", "Mean slope of lake basin",
                             "basin_forest", "Percent of lake basin with forest land cover.",
                             "clow_surface_area", "Lake surface area used for calculation of sedimentation rates.",
                             "basin_crop", "Percent of lake basin with crop land cover.",
                             "basin_wetland", "Percent of lake basin with wetland land cover.",
                             "basin_kfact", "Mean of STATSGO Kffactor raster on land (NLCD 2006) within the lake basin. The Universal Soil Loss Equation (USLE) and represents a relative index of susceptibility of bare, cultivated soil to particle detachment and transport by rainfall",
                             "basin_soc0_5", "Mean soil organic carbon content in top 5 cm of soil in lake basin",
                             "basin_barren", "Percent of lake basin with barren land cover.",
                             
                             # NID
                             "year_completed", "Year dam was completed",
                             
                             # water isotope
                             "e_i", "Proportion of water entering a lake that leaves through evaporation",
                             "sd_e_i", "Standard deviation of repeated estimates of the proportion of water entering a lake that leaves through evaporation",
                             "retention_time", "Lake water residence time",
                             "sd_retention_time", "Standard deviation of repeated estimates of lake water residence time",
                             "retention_time_ei_repeat_visits", "Number of time retention time and e_i was estimated",
                             "e_i_type", "Hydrologic regime based on e_i. e_i < 0.2 is run-of-river. e_i > 0.2 is storage reservoir",
                             
                             # survey design variables
                             "wgt", "Weight for probabilistic survey design. If no value provided the lake was hand-picked",
                             "ag_eco9", "Abbreviation for the 9 aggregated ecoregions used for the SuRGE survey design",
                             "ag_eco9_nm", "Full name of the 9 aggregated ecoregions used for the SuRGE survey design",
                             "depth_cat", "Depth category used for the SuRGE survey design",
                             "chla_cat", "Chlorophyll a category used for the SuRGE survery design",
                             
                             # SITE DATA
                             # sonde
                             "chla_sonde", "Chlorophyll a measured with sonde optical sensor",
                             "do", "Dissolved oxygen",
                             "ph", "pH",
                             "sp_cond", "Specific conductivity",
                             "temp", "Water temperature",
                             "turb", "Turbidity measured with sonde optical sensor",
                             "phycocyanin_sonde", "Pycocyanin measured with sonde optical sensor",
                             "al", "aluminum",
                             "as", "arsenic",
                             "ba", "barium",
                             "be", "beryllium",
                             "br", "bromine",
                             "ca", "calcium",
                             "cd", "cadmium",
                             "cl", "chlorine",
                             "cr", "chromium",
                             "cu", "copper",
                             "doc", "dissolved organic carbon",
                             "f", "fluorine",
                             "fe", "iron",
                             "k", "potassium",
                             "li", "lithium",
                             "mg", "magnesium",
                             "mn", "manganese",
                             "na", "sodium",
                             "ni", "nickel",
                             "no2", "nitrite",
                             "p", "phosphorus",
                             "pb", "lead",
                             "s", "sulfur",
                             "sb", "antimony",
                             "si", "silicon",
                             "sn", "tin",
                             "so4", "sulfate",
                             "sr", "strontium",
                             "toc", "total organic carbon",
                             "v", "vanadium",
                             "zn","zinc",
                             "chla_lab", "Laboratory based chloropyll a",
                             "nh4", "ammonium",
                             "no2_3", "nitrite + nitrate",
                             "op", "orthophosphate",
                             "tn", "total nitrogen",
                             "tp", "total phosphorus",
                             "no3", "nitrate",
                             "flags", "1: failed post-deployment calibration check or value was otherwise suspicious. L: value is < reporting limit but > minimum detection limit. ND: analyte not detected and minimum detection limit reported. H: holding time violation. S: sampled warmed during shipping",
                             
                             
                             
                             #emissions
                             "ch4_diffusion_best", "areal ch4 diffusion flux from floating chamber calculated using most preferred model (could be linear or exponential)",
                             "ch4_diffusion_units", "ch4_diffusion_best units",
                             "ch4_ebullition", "areal ch4 ebullition flux from bubble traps",
                             "ch4_ebullition_units", "ch4_ebullition units",
                             "ch4_total", "the sum of ch4 diffusion and ch4 ebullition when both were measured",
                             "ch4_total_units", "ch4_total units",
                             "co2_diffusion_best", "areal co2 diffusion flux from floating chamber calculated using most preferred model (could be linear or exponential)",
                             "co2_diffusion_units", "co2_diffusion units",
                             "co2flag", "Value of U if the floating chamber experienced an unstable start",
                             "co2_ebullition", "areal co2 ebullition flux from bubble traps",
                             "co2_ebullition_units", "co2_ebullition units",
                             "co2_total", "the sum of co2 diffusion and co2 ebullition when both were measured",
                             "co2_total_units", "co2_total units",
                             "chamb_deply_date_time", "date and time of floating chamber deployment in UTC",
                             "trap_deply_date_time", "date and time of bubble trap deployment in UTC",
                             "trap_rtrvl_date_time", "date and time of bubble trap retrieval in UTC"
)

# 3. LAKE SCALE VALUES-----------
lake_scale_data <- list(
  
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


# Data dictionary
#write.table(unique(lake_scale_data_paper$name), file = "clipboard", row.names = FALSE)
lake_scale_dictionary <- master_dictionary %>%
  filter(variable %in% unique(lake_scale_data$name))

# write data
write.csv(x = lake_scale_data, 
          file = "../../../communications/manuscript/data_paper/3_lake_scale.csv")

# write dictionary
write.csv(x = lake_scale_dictionary, 
          file = "../../../communications/manuscript/data_paper/3_lake_scale_dictionary.csv")

# 4. DEPTH PROFILES----
# WIDE FORMAT
# see readDepthProfiles.R for primary data source
# depth_profile_69_70 is just lacustrine sites. depth_profile_surge includes
# riverine and transitional. Although all other lakes only have a single
# depth profile, lets report all zones in the paper.
depth_profiles_data <- list(
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

# Data dictionary
depth_profiles_dictionary <- master_dictionary %>%
  filter(variable %in% colnames(depth_profiles_data))

# write data
write.csv(x = depth_profiles_data, 
          file = "../../../communications/manuscript/data_paper/4_depth_profiles.csv")

# write dictionary
write.csv(x = depth_profiles_dictionary, 
          file = "../../../communications/manuscript/data_paper/4_depth_profiles_dictionary.csv")


# 5. SITE DATA------------

# a.	Unit columns for all measurement
# b.	Flat file
# c.	lake_id
# d.	site_id
# e.	visit
# f. chemistry
# h.	Dissolved gas not ready [3/12/2025]

site_data <- 
  bind_rows(
    # SONDE DATA FIRST
    dat %>%
      select(lake_id, site_id, visit,
             # Sonde (deep and shallow)
             contains("sonde"), # chlorophyll and phycocyanin
             contains("do_mg"), 
             contains("_ph") & !contains("nla") & !contains("phycocyanin"), # exclude NLA pH and lab phycocyanin 
             contains("sp_cond"),
             matches(c("shallow_temp|deep_temp")),
             contains("turb") & !contains("nla")) %>% # exclude NLA turbidity
      # exclude do units from column name
      rename_with(~ifelse(grepl("do_mg", .x), gsub("_mg", "", .x), .x)) %>%
      # every column name must end with _flags, _comment, _units, or _value. This will
      # be used to pivot_longer
      rename_with(~ifelse(
        !grepl(c("lake_id|site_id|visit|flag|comment|units"), .x), # columns that aren't ID, flag, comment, or units
        paste0(.x, "_value"), # append _value suffix
        .x)) %>% # else return original name
      pivot_longer(-c(lake_id, site_id, visit),
                   # anything to left of pattern is "name"
                   # every matching group to right creates new value column
                   names_to = c("name", ".value"),
                   # breaking pattern is final _
                   names_pattern = "(.+)_(.+)") %>%
      mutate(sample_depth = sub("\\_.*", "", name), # move depth to new column
             name = gsub(c("deep_|shallow_"), "", name)) %>% # remove depth from name column
      # create units columns
      mutate(units = case_when(name == "chla_sonde" ~ "ug_l",
                               name == "phycocyanin_sonde" ~ "ug_l",
                               name == "do" ~ "mg_l",
                               name == "ph" ~ "ph",
                               name == "sp_cond" ~ "us_cm",
                               name == "turb" ~ "ntu",
                               name == "temp" ~ "c",
                               TRUE ~ "FLY YOU FOOLS!"),
             value = as.numeric(value)) %>% # something in here is causing a character value?
      drop_na(value), # omit record if no value reported
    
    
    # SuRGE CHEMISTRY DATA
    chemistry_all %>%
      filter(sample_type != "blank") %>% # omit blanks
      select(-sample_type, -analyte_group,
             -contains("phycocyanin_lab")) %>%
      # move transitional, lacustrine, riverine from lake_id to site_id
      mutate( 
        site_id = case_when(grepl("lacustrine", lake_id) ~ paste0(site_id, "_lacustrine"),
                            grepl("transitional", lake_id) ~ paste0(site_id, "_transitional"),
                            grepl("riverine", lake_id) ~ paste0(site_id, "_riverine"),
                            TRUE ~ as.character(site_id)),
        # remove transitional, lacustrine, riverine from lake_id
        # retain character class initially, then convert to numeric.
        lake_id = case_when(lake_id %in% c("69_lacustrine", "69_riverine", "69_transitional") ~ "69",
                            lake_id %in% c("70_lacustrine", "70_riverine", "70_transitional") ~ "70",
                            TRUE ~ lake_id),
        lake_id = as.numeric(lake_id)) %>%
      # some units are missing, even when an analyte value is presented.
      # lake_id == 17, analyte == cl for example
      # fill all units
      fill(contains("units"), .direction = "updown") %>%
      group_by(lake_id, site_id, visit, sample_depth) %>%
      summarize(across(!matches(c("flags|units")), mean),
                # This takes nearly 20 seconds to run!
                # Check if any flags are present across grouped analytes;
                # If every analyte has a flag, keep it. Otherwise, NA.
                across(contains("flag"),
                       ~ case_when(
                         # Check for all combinations of flags
                         all(str_detect(., "ND H S")) ~ "ND H S",
                         all(str_detect(., "L H S")) ~ "L H S",
                         all(str_detect(., "ND.*H")) ~ "ND H",
                         all(str_detect(., "L.*H|H.*L")) ~ "L H",
                         all(str_detect(., "ND.*S")) ~ "ND S",
                         all(str_detect(., "L.*S")) ~ "L S",
                         all(str_detect(., "H.*S")) ~ "H S",
                         all(str_detect(., "ND")) ~ "ND",
                         all(str_detect(., "L")) ~ "L",
                         all(str_detect(., "H")) ~ "H",
                         all(str_detect(., "S")) ~ "S",
                         # All other combinations should result in NA
                         TRUE ~ NA_character_)), 
                # Retain units columns; look for any non-NA value
                across(contains("unit"), 
                       ~ min(., na.rm = TRUE))) %>%
      # every column name must end with _flags, _units, or _value. This will
      # be used to pivot_longer
      rename_with(~ifelse(
        !grepl(c("lake_id|site_id|sample_depth|visit|flag|units"), .x), # columns that aren't ID, flag, or units
        paste0(.x, "_value"), # append _value suffix
        .x)) %>% # else return original name
      pivot_longer(-c(lake_id, site_id, visit, sample_depth),
                   # anything to left of pattern is "name"
                   # every matching group to right creates new value column
                   names_to = c("name", ".value"),
                   # breaking pattern is final _
                   names_pattern = "(.+)_(.+)") %>%
      ungroup %>%
      drop_na(value), # omit record if no value reported
    
    
    # 2016 CHEMISTRY DATA   
    dat_2016 %>%
      select(lake_id, site_id, visit,
             matches(c("_chla_lab|_tn|_nh4|_no2|_no2_3|_toc|_tp|_op")),
             contains("units") & !matches(c("ebullition|diffusion|total"))) %>%
      mutate(site_id = as.character(site_id)) %>% # needed to bind with above that have 8_lacustrine....
      # all 2016 chemistry observations are shallow. Remove from variable
      # name to simplify things. Add in as new variable below
      rename_with(.cols = contains("shallow"), # columns that aren't ID or units
                  ~ gsub("shallow_", "", .x)) %>%
      # every column name must end with _units, or _value. This will
      # be used to pivot_longer
      rename_with(.cols = !matches(c("lake_id|site_id|visit|units")), # columns that aren't ID or units
                  ~paste0(.x, "_value")) %>% # append _value suffix
      pivot_longer(-c(lake_id, site_id, visit),
                   # anything to left of pattern is "name"
                   # every matching group to right creates new value column
                   names_to = c("name", ".value"),
                   # breaking pattern is final _
                   names_pattern = "(.+)_(.+)") %>%
      mutate(sample_depth = "shallow") %>%
      drop_na(value) # omit record if no value reported
  )

#write.table(unique(site_data$name), file = "clipboard", row.names = FALSE)

# Data dictionary
site_data_dictionary <- master_dictionary %>%
  filter(variable %in% colnames(s) |
         variable %in% unique(site_data$name))

# Are all values in data dictionary?
colnames(site_data) %in% site_data_dictionary$variable

# write data
write.csv(x = site_data, 
          file = "../../../communications/manuscript/data_paper/5_site_data.csv")

# write dictionary
write.csv(x = site_data_dictionary, 
          file = "../../../communications/manuscript/data_paper/5_site_data_dictionary.csv")

  # 6. EMISSIONS RATES (POINT)----
  
  #First need to compile correct chamber deployment times into an object
  #pull chamber deployments from the chm_deply object created in writeSuRGElakesToGpkg for SuRGE
  #and from the dat_2016 object for the 2016 sites
  chm_dep <- bind_rows (
    chm_deply %>%
      select(lake_id, site_id, visit, chamb_deply_date_time),
    
    dat_2016 %>%
      select(lake_id, site_id, visit, chamb_deply_date_time) %>%
      mutate(lake_id = as.numeric(lake_id), site_id = as.character(site_id))
    )
  
  trp_dep <- bind_rows (
    dat_surge_sf %>%
      select(
        lake_id,
        site_id,
        visit,
        trap_deply_date_time,
        trap_rtrvl_date_time
      ),
    
    dat_2016 %>%
      select(
        lake_id,
        site_id,
        visit,
        trap_deply_date_time,
        trap_rtrvl_date_time
      ) %>%
      mutate(lake_id = as.numeric(lake_id), site_id = as.character(site_id))
    )
  
  dep <- left_join(chm_dep, trp_dep)
  dep <- as.data.frame(dep) %>%
    select(-geom)
  
  emission_rate_points_data_paper <- left_join(
    dat %>%
      select(
        lake_id,
        site_id,
        visit,
        ch4_diffusion_best,
        ch4_diffusion_units,
        ch4_ebullition,
        ch4_ebullition_units,
        ch4_total,
        ch4_total_units,
        co2_diffusion_best,
        co2_diffusion_units,
        co2flag,
        co2_ebullition,
        co2_ebullition_units,
        co2_total,
        co2_total_units
      ),
    dep
  )
  
  # Data dictionary
  emission_rate_points_data_paper_dictionary <- master_dictionary %>%
    filter(variable %in% colnames(emission_rate_points_data_paper))
  
  # write data
  write.csv(
    x = emission_rate_points_data_paper,
    file = paste0(
      userPath,
      "communications/manuscript/data_paper/6_emission_rate_points.csv"
    )
  )
  
  # write dictionary
  write.csv(
    x = emission_rate_points_data_paper_dictionary,
    file = paste0(
      userPath,
      "communications/manuscript/data_paper/6_emission_rate_points_dictionary.csv"
    )
  )
