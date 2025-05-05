
# 1. MASTER DICTIONARY--------------
#
master_dictionary <- tribble(~variable, ~definition,
                             # COMMON TO SEVERAL FILES
                             "name", "Variable name",
                             "site_id", "Unique identifier for sample site within a waterbody",
                             "lake_id", "Unique identifier for each waterbody",
                             "visit", "First (1) or second (2) site visit",
                             "units", "Measurement units",
                             "comment", "Comment pertaining to measurement",
                             "value", "Value of corresponding variable",
                             
                             # DEPTH PROFILES
                             "sample_depth", "Measurement depth",
                             "sample_depth_units", "Units for sample depth",
                             "sample_date", "Observation date",
                             
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
                             "max_width", "Maximum lake width is defined as the maximum shore to shore distance that is perpendicular to the maximum lake length.",
                             "mean_width", "Lake surface area divided by fetch",
                             "max_length", "Maximum lake length is defined as the longest open water distance of a lake.",
                             "circularity", "Circularity compares the area of the lake (m2) with the area of the minimum boundary circle (MBC; m2) around it. It differentiates between circular (value approaches 1) and elongated lakes (value approaches 0).",
                             "mean_depth", "Mean lake depth",
                             "max_depth", "Maximum lake depth",
                             "volume", "Volume of lake",
                             "major_axis", "The major axis of a lake is defined as the longest line intersecting the convex hull formed around its polygon while passing through its center. Its value represents the distance across a lake without regard to land-water configuration.",
                             "minor_axis", "The minor axis of a lake is defined as the shortest line intersecting the convex hull formed around the lake polygon while passing through its center. In contrast to max width, its value represents the distance across a lake with regard to the the convex hull and without consideration of the land-water configuration.",
                             "axis_ratio", "The ratio of the lake major axis length to the minor axis length is also known as the aspect ratio",
                             "fetch", "Fetch is the maximum open water distance",
                             "dynamic_ratio", "Surface area to mean depth ratio of the lake. Low values indicate lakes that are bowlshaped, whereas high values are associated to dish-like lakes.",
                             "littoral_fraction", "Relative proportion of lake surface area that is shallower than 2.5 m",
                             
                             # national wetlands inventory
                             "lacustrine", "percent of reservoir classified as a lacustrine system in the National Wetlands Inventory",
                             "palustrine", "percent of reservoir classified as a palustrine system in the National Wetlands Inventory",
                             "riverine", "percent of reservoir classified as a riverine system in the National Wetlands Inventory",
                             "limnetic", "percent of reservoir classified as a limnetic subsystem of lacustrine in the National Wetlands Inventory",
                             "littoral", "percent of reservoir classified as a littoral subsystem of lacustrine in the National Wetlands Inventory",
                             "intermittent", "percent of reservoir classified as intermittent subsystem of riverine in the National Wetlands Inventory",
                             "lower_perennial", "percent of reservoir classified as lower perennial subsystem of riverine in the National Wetlands Inventory",
                             "upper_perennial", "percent of reservoir classified as upper perennial subsystem of riverine in the National Wetlands Inventory",
                             "emergent", "percent of reservoir classified as emergent class, can come from combination of lacustrine, palustrine, and riverine systems, in the National Wetlands Inventory",
                             "aquatic_bed", "percent of reservoir classified as aquatic bed, can come from combination of lacustrine, palustrine, and riverine systems, in the National Wetlands Inventory",     
                             
                             # sedimentation
                             "sedimentation", "Total sedimentation rate.",
                             "sedimentation_units", "Units for sedimentation",
                             "sediment_oc", "Sediment organic carbon content",
                             "sediment_oc_units", "Units for sediment_oc",
                             "basin_slope", "Mean slope of lake basin",
                             "basin_slope_units", "units for basin_slope",
                             "basin_forest", "Percent of lake basin with forest land cover",
                             "basin_forest_units","units for basin_forest",
                             "clow_surface_area", "Lake surface area used for calculation of sedimentation rates.",
                             "clow_surface_area_units","units for clow_surface_area",
                             "basin_crop", "Percent of lake basin with crop land cover",
                             "basin_crop_units", "units for basin_crop_units",
                             "basin_wetland", "Percent of lake basin with wetland land cover",
                             "basin_wetland_units","units for basin_wetland",
                             "basin_kfact", "Mean of STATSGO Kffactor raster on land (NLCD 2006) within the lake basin. The Universal Soil Loss Equation (USLE) and represents a relative index of susceptibility of bare, cultivated soil to particle detachment and transport by rainfall",
                             "basin_kfact_units","units for basin_kfact",
                             "basin_soc0_5", "Mean soil organic carbon content in top 5 cm of soil in lake basin",
                             "basin_soc0_5","units for basin_soc0_5",
                             "basin_barren", "Percent of lake basin with barren land cover",
                             "basin_barren_units","units for basin_barren",
                             
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
                             "study", "Survey effort the reservoir was included in. See data paper Figure 1.",
                             
                             #emissions (point)
                             "air_temp", "temperature of the air at the time the floating chamber was deployed",
                             "air_temp_units", "units for air_temp",
                             "ch4_diffusion","areal ch4 diffusion flux from floating chamber calculated using most preferred model (could be linear or exponential)",
                             "ch4_diffusion_units","ch4_diffusion units",
                             "ch4_ebullition","areal ch4 ebullition flux from bubble traps",
                             "ch4_ebullition_units","ch4_ebullition units",
                             "ch4_total","the sum of ch4 diffusion and ch4 ebullition when both were measured",
                             "ch4_total_units","ch4_total units",
                             "ch4_deployment_length","the length of floating chamber time for which a diffusive methane flux was calculated",
                             "ch4_deployment_length_units","ch4_deployment_length units in seconds",
                             "co2_diffusion","areal co2 diffusion flux from floating chamber calculated using most preferred model (could be linear or exponential)",
                             "co2_diffusion_units","co2_diffusion units",
                             "co2flag","Value of U if the floating chamber experienced an unstable start",
                             "co2_ebullition","areal co2 ebullition flux from bubble traps",
                             "co2_ebullition_units","co2_ebullition units",
                             "co2_total","the sum of co2 diffusion and co2 ebullition when both were measured",
                             "co2_total_units","co2_total units",
                             "co2_deployment_length","the length of floating chamber time for which a diffusive carbon dioxide flux was calculated",
                             "co2_deployment_length_units","co2_deployment_length units in seconds",
                             "chamb_deply_date_time","date and time of floating chamber deployment in UTC",
                             "chamb_deply_date_time_units","timezone for chamb_deply_date_time",
                             "ch4_deployment_length", "Duration of CH4 time series used to calculate diffusive CH4 emissions",
                             "ch4_deployment_length_units", "Units used to express duration of CH4 time series used to calculate diffusive CH4 emissions",
                             "co2_deployment_length", "Duration of CO2 time series used to calculate diffusive CO2 emissions",
                             "co2_deployment_length_units", "Units used to express duration of CO2 time series used to calculate diffusive CO2 emissions",
                             "trap_deply_date_time","date and time of bubble trap deployment in UTC",
                             "trap_deply_date_time_units","timezone for trap_deply_date_time",
                             "trap_rtrvl_date_time", "date and time of bubble trap retrieval in UTC",
                             "trap_rtrvl_date_time_units", "timezone for trap_rtrvl_date_time",
                             

                             #emissions(lake)
                             "ch4_ebullition_lake","lakewide areal methane ebullition flux estimated using survey site weights",
                             "ch4_ebullition_units_lake","units for ch4_ebullition_lake_units",
                             "ch4_diffusion_lake", "lakewide areal methane diffusion flux estimated using survey site weights",
                             "ch4_diffusion_units_lake", "units for ch4_diffusion_lake",
                             "ch4_total_lake", "lakewide total methane flux estimated using survey site weights",
                             "ch4_total_units_lake","units for ch4_total_lake",
                             "co2_ebullition_lake", "lakewide areal carbon dioxide ebullition flux estimated using survey site weights",
                             "co2_ebullition_units_lake", "units for co2_ebullition_lake",
                             "co2_diffusion_lake", "lakewide areal carbon dioxide diffuion flux estimated using survey site weights",
                             "co2_diffusion_units_lake", "units for co2_diffusion_lake",
                             "co2_total_lake", "lakewide areal total carbon dioxide flux estimated using survey site weights",
                             "co2_total_units_lake","units for co2_total_lake_units",
                             "ch4_ebullition_std_error_lake","standard error of ch4_ebullition_lake",
                             "ch4_diffusion_std_error_lake", "standard error of ch4_diffusion_lake",
                             "ch4_total_std_error_lake", "standard error of ch4_total_lake",
                             "co2_ebullition_std_error_lake", "standard error of co2_ebullition_lake",
                             "co2_diffusion_std_error_lake", "standard error of co2_diffusion_lake",
                             "co2_total_std_error_lake", "standard error of co2_total_lake",
                             "ch4_ebullition_margin_of_error_lake", paste("The average half-width of the 95% confidence interval of the lake-scale ch4 ebullition rate estimate.",
                                                                          "See Michael Dumelle, Tom Kincaid, Anthony R. Olsen, Marc Weber (2023). spsurvey: Spatial Sampling Design",
                                                                          "and Analysis in R. Journal of Statistical Software, 105(3), 1-29. doi:10.18637/jss.v105.i03"),
                             "ch4_diffusion_margin_of_error_lake", paste("The average half-width of the 95% confidence interval of the lake-scale ch4 diffusion rate estimate.",
                                                                         "See Michael Dumelle, Tom Kincaid, Anthony R. Olsen, Marc Weber (2023). spsurvey: Spatial Sampling Design",
                                                                         "and Analysis in R. Journal of Statistical Software, 105(3), 1-29. doi:10.18637/jss.v105.i03"),
                             "ch4_total_margin_of_error_lake", paste("The average half-width of the 95% confidence interval of the lake-scale total ch4 emission rate estimate.",
                                                                     "See Michael Dumelle, Tom Kincaid, Anthony R. Olsen, Marc Weber (2023). spsurvey: Spatial Sampling Design",
                                                                     "and Analysis in R. Journal of Statistical Software, 105(3), 1-29. doi:10.18637/jss.v105.i03"),
                             "co2_ebullition_margin_of_error_lake", paste("The average half-width of the 95% confidence interval of the lake-scale co2 ebullition rate estimate.",
                                                                          "See Michael Dumelle, Tom Kincaid, Anthony R. Olsen, Marc Weber (2023). spsurvey: Spatial Sampling Design",
                                                                          "and Analysis in R. Journal of Statistical Software, 105(3), 1-29. doi:10.18637/jss.v105.i03"),
                             "co2_diffusion_margin_of_error_lake", paste("The average half-width of the 95% confidence interval of the lake-scale co2 diffusion rate estimate.",
                                                                         "See Michael Dumelle, Tom Kincaid, Anthony R. Olsen, Marc Weber (2023). spsurvey: Spatial Sampling Design",
                                                                         "and Analysis in R. Journal of Statistical Software, 105(3), 1-29. doi:10.18637/jss.v105.i03"),
                             "co2_total_margin_of_error_lake", paste("The average half-width of the 95% confidence interval of the lake-scale total co2 emission rate estimate.",
                                                                     "See Michael Dumelle, Tom Kincaid, Anthony R. Olsen, Marc Weber (2023). spsurvey: Spatial Sampling Design",
                                                                     "and Analysis in R. Journal of Statistical Software, 105(3), 1-29. doi:10.18637/jss.v105.i03"),
                             "ch4_ebullition_lcb95pct_lake", "lower bound 95 percent confidence interval for ch4_ebullition_lake",
                             "ch4_diffusion_lcb95pct_lake", "lower bound 95 percent confidence interval for ch4_diffusion_lake",
                             "ch4_total_lcb95pct_lake", "lower bound 95 percent confidence interval for ch4_total_lake",
                             "co2_ebullition_lcb95pct_lake", "lower bound 95 percent confidence interval for co2_ebullition_lake",
                             "co2_diffusion_lcb95pct_lake", "lower bound 95 percent confidence interval for co2_diffusion_lake",
                             "co2_total_lcb95pct_lake", "lower bound 95 percent confidence interval for co2_total_lake",
                             "ch4_ebullition_ucb95pct_lake", "upper bound 95 percent confidence interval for ch4_ebullition_lake",
                             "ch4_diffusion_ucb95pct_lake", "upper bound 95 percent confidence interval for ch4_diffusion_lake",
                             "ch4_total_ucb95pct_lake", "upper bound 95 percent confidence interval for ch4_total_lake",
                             "co2_ebullition_ucb95pct_lake", "upper bound 95 percent confidence interval for co2_ebullition_lake",
                             "co2_diffusion_ucb95pct_lake", "upper bound 95 percent confidence interval for co2_diffusion_lake",
                             "co2_total_ucb95pct_lake", "upper bound 95 percent confidence interval for co2_total_lake",
                             
                             
                             #remote sensing (lake)
                             "chl_predicted_sample_month_visit1", paste("predicted chlorophyll a concentration from the same month",
                                                                  "and year that emissions were collected during the first site visit if available, otherwise mean predicted",
                                                                  "chlorophyll from 2018-2020 during the same month emissions were collected, ",
                                                                  "predictions are from LAGOS-US LANDSAT (1984-2020)"),
                             "chl_predicted_sample_month_visit2", paste("predicted chlorophyll a concentration from the same month",
                                                                        "and year that emissions were collected during the second site visit  if available, otherwise mean predicted",
                                                                        "chlorophyll from 2018-2020 during the same month emissions were collected, ",
                                                                        "predictions are from LAGOS-US LANDSAT (1984-2020)"),
                             "chl_predicted_sample_month_units", "units for chl_predicted_sample_month",
                             "doc_predicted_sample_month_visit1", paste("predicted dissolved organic carbon concentration from the same month",
                                                                  "and year that emissions were collected during the first site visit if available, otherwise mean predicted",
                                                                  "dissolved organic carbon concentration from 2018-2020 during the same month emissions",
                                                                  "were collected, predictions are from LAGOS-US LANDSAT (1984-2020)"),
                             "doc_predicted_sample_month_visit2", paste("predicted dissolved organic carbon concentration from the same month",
                                                                        "and year that emissions were collected during the second site visit if available, otherwise mean predicted",
                                                                        "dissolved organic carbon concentration from 2018-2020 during the same month emissions",
                                                                        "were collected, predictions are from LAGOS-US LANDSAT (1984-2020)"),
                             "doc_predicted_sample_month_units", "units for doc_predicted_sample_month",
                             "chl_predicted_sample_season_visit1", paste("mean predicted chlorophyll a concentration during June to September",
                                                                   "of the three years leading up to the emission sampling year as well as the sampling year of the first visit",
                                                                   "when available, predictions are from LAGOS-US LANDSAT (1984-2020)"),
                             "chl_predicted_sample_season_visit2", paste("mean predicted chlorophyll a concentration during June to September",
                                                                         "of the three years leading up to the emission sampling year as well as the sampling year of the second visit",
                                                                         "when available, predictions are from LAGOS-US LANDSAT (1984-2020)"),
                             "chl_predicted_sample_season_units","units for chl_predicted_sample_season",
                             "doc_predicted_sample_season_visit1", paste("mean predicted dissolved organic carbon concentration during June to September",
                                                                   "of the three years leading up to the emission sampling year as well as the sampling year of the first visit,",
                                                                   " when available, predictions are from LAGOS-US LANDSAT (1984-2020)"),
                             "doc_predicted_sample_season_visit2", paste("mean predicted dissolved organic carbon concentration during June to September",
                                                                         "of the three years leading up to the emission sampling year as well as the sampling year of the second visit,",
                                                                         "when available, predictions are from LAGOS-US LANDSAT (1984-2020)"),
                             "doc_predicted_sample_season_units", "units for doc_predicted_sample_season",

                             # SITE DATA
                             # sonde (see above)
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
                             "chla_lab", "Laboratory based chlorophyll a",
                             "nh4", "ammonium",
                             "no2_3", "nitrite + nitrate",
                             "op", "orthophosphate measured via colorimetry. Sometimes called reactive phosphorus.",
                             "tn", "total nitrogen",
                             "tp", "total phosphorus",
                             "no3", "nitrate",
                             "microcystin", "microcystin",
                             "dissolved_n2o", "dissolved nitrous oxide concentration",
                             "dissolved_ch4", "dissolved methane concentration",
                             "dissolved_co2", "dissolved carbon dioxide concentration",
                             "n2o_sat_ratio", "Ratio of observed to equilibrium dissolved nitrous oxide concentration",
                             "ch4_sat_ratio", "Ratio of observed to equilibrium dissolved methane concentration",
                             "co2_sat_ratio", "Ratio of observed to equilibrium dissolved carbon dioxide concentration",
                             "flags", "1: failed post-deployment calibration check or value was otherwise suspicious. L: value is < reporting limit but > minimum detection limit. ND: analyte not detected and minimum detection limit reported. H: holding time violation. S: sampled warmed during shipping",
                             

                             # 8. site descriptors
                             "site_depth", "Depth of reservoir at sampling site",
                             "site_depth_units", "Units of site_depth measurement",
                             "site_wgt", "Weight for lake-specific probabilistic survey design.",
                             "site_wgt_units", "Units for site_wgt",
                             "lat", "Latitude of sampling location in decimal degrees",
                             "long", "Longitude of sampling location in decimal degrees",
                             "sample_start", "First day of sampling campaign at lake",
                             "sample_end", "Last day of sampling campaign at lake",
                             "chla_collection_date", "Date that sample was collected for laboratory-based chlorophyll a measurement",
                             
                             # 9. meteorology
                             "date_time", "Date and time (hour) of floating chamber deployment and associated meteoroligcal observations",
                             "date_time_units", "Time zone of date_time values",
                             "precipitation", "Total precipitation during hour of chamber deployment",
                             "precipitation_units", "Units of precipitation values",
                             "wind_speed", "Mean wind speed during hour of floating chamber deployment",
                             "wind_speed_units", "Units of wind speed",
                             "temp_air_2m", "Air temperature 2m above the water surface during hour of floating chamber deployment",
                             "temp_air_2m_units", "2m air temperature units",
                             
                             # 10. phytoplankton
                             "algal_group", "Broad algal group classification",
                             "phylum", "Phylum of taxon",
                             "class", "Class of taxon",
                             "order", "Order of taxon",
                             "family", "Family of taxon",
                             "genus", "Genus of taxon",
                             "density", "Density of organisms enumerated from sample",
                             "density_units", "Units used for density of organisms enumerated from sample"
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
    select(-lake_name) %>%
    mutate(
      # variables with no decimals
      across(c(surface_area, shoreline_length, max_width, mean_width, max_length, volume), 
           ~format(round(., 0), nsmall = 0)),
      # variables with 1 decimal
      across(c(mean_depth, max_depth), ~ format(round(., 1), nsmall = 1)),
      # variables with two decimals
      across(c(shoreline_development, littoral_fraction), ~ format(round(., 2), nsmall = 2)),
      # variables with three decimals, not including circularity in data paper
      #circularity = format(round(circularity, 3), nsmall = 3)
      ) %>%
    mutate(across(!lake_id, as.character)) %>% # needed to collapse into one column
    pivot_longer(!lake_id) %>%
    mutate(units = case_when(name == "surface_area" ~ "m2",
                             name == "shoreline_development" ~ "dimensionless",
                             #name == "circularity" ~ "dimensionless",
                             name == "volume" ~ "m3",
                             #name == "axis_ratio" ~ "dimensionless",
                             #name == "dynamic_ratio" ~ "dimensionless",
                             TRUE ~ "m")) %>% # all others meters
    mutate(across(!lake_id, as.character)), # needed to collapse into one column
  
  # c.  NWI
  
  nwi_link %>%
    pivot_longer(!lake_id)%>%
    mutate(value = format(round ((value * 100),2), nsmall=2)) %>%
    mutate (units = "percent"),
  
  # X.	Sedimentation rates
  # sedimentation_link %>%
  #   select(- c(sediment_predictor_type, sedimentation, sedimentation_units)) %>% # not sure about this variable or sedimentation numbers
  #   mutate(
  #     # Fix decimal places, otherwise many are shown when converted to character
  #     basin_kfact = format(round(basin_kfact, 3), nsmall = 3),
  #     across(c(basin_forest, basin_crop, basin_wetland, sediment_oc, basin_slope),
  #            ~ format(round(., 2), nsmall = 2)),
  #     across(c(clow_surface_area, basin_soc0_5), ~ format(round(., 0), nsmall = 0))
  #     ) %>%
  #   # subset for development
  #   #filter(lake_id == 100) %>%
  #   #select(lake_id, contains("sedimentation"), contains("sediment")) %>%
  #   # all variables presenting a value must end with "_value". All variables
  #   # presenting units already end with "_units"
  #   rename_with(~ifelse(!grepl(c("units|lake_id"), .x), paste0(.x, "_value"), .x)) %>%
  #   pivot_longer(-lake_id, 
  #                # anything to left of pattern is "name"
  #                # every matching group to right creates new value column
  #                names_to = c("name", ".value"), 
  #                # breaking pattern in final _
  #                names_pattern = "(.+)_(.+)"),
  
  # d.	Year of construction
  nid_link %>%
    select(lake_id, year_completed) %>%
    pivot_longer(!lake_id) %>%
    mutate(units = "Gregorian calendar year"),
  
  # f.	E:I and Residence Time estimates
  water_isotope_agg %>%
    mutate(lake_id = as.numeric(lake_id),
           # decimals inherited from Renee's data release (10.23719/1531017)
           e_i = format(round(e_i, 3), nsmall = 3),
           sd_e_i = format(round(sd_e_i, 3), nsmall = 3),
           retention_time = format(round(retention_time, 3), nsmall = 3),
           sd_retention_time = case_when(is.na(sd_retention_time) ~ NA_character_,
                                         TRUE ~ format(round(sd_retention_time, 3), nsmall = 3)),
           
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
  
  # survey design parameters and study 
  bind_rows(lake.list, lake.list.2016) %>%
    filter(eval_status_code == "S",
           visit == 1) %>% 
    select(lake_id, wgt, ag_eco9, ag_eco9_nm, depth_cat, chla_cat) %>%
    mutate(wgt_units = "dimensionless",
           study = case_when(lake_id %in% 1:998 ~ "SuRGE",
                             lake_id %in% 1001:1032 ~ "2016 Regional Survey",
                             lake_id %in% 999:1000 ~ "Hand picked",
                             TRUE ~ "Fly you fools!"), # error code
           across(!lake_id, as.character)) %>% # needed to pivot all values to one column
    # all variables presenting a value must end with "_value". All variables
    # presenting units already end with "_units"
    rename_with(~ifelse(!grepl(c("units|lake_id"), .x), paste0(.x, "_value"), .x)) %>%
    pivot_longer(!lake_id,
                 # anything to left of pattern is "name"
                 # every matching group to right creates new value column
                 names_to = c("name", ".value"), 
                 # breaking pattern in final _
                 names_pattern = "(.+)_(.+)"),
  
  # LAGOS trophic status data
  lagos_ts_agg_link %>%
    select(lake_id, visit,
           chl_predicted_sample_month, doc_predicted_sample_month,
           chl_predicted_sample_season, doc_predicted_sample_season)%>%
    mutate(chl_predicted_sample_month_units = "ug_l",
           doc_predicted_sample_month_units = "mg_l",
           chl_predicted_sample_season_units = "ug_l",
           doc_predicted_sample_season_units = "mg_l") %>%
    # all variables presenting a value must end with "_value". All variables
    # presenting units already end with "_units"
    rename_with(~ifelse(!grepl(c("units|lake_id|visit"), .x), paste0(.x, "_value"), .x)) %>%
    pivot_longer(!c(lake_id, visit),
                 # anything to left of pattern is "name"
                 # every matching group to right creates new value column
                 names_to = c("name", ".value"), 
                 # breaking pattern in final _
                 names_pattern = "(.+)_(.+)") %>%
    # move visit from "visit" column to variable name. This is because this
    # data object doesn't have a visit column
    mutate(name = paste0(name, "_visit", visit)) %>%
    select(-visit) %>%
    # enforce digits, otherwise many digits are shown when converted to character below
    mutate(value = format(round(value, 2), nsmall = 2))

  ) %>%
    map(., ~.x %>% mutate(value = as.character(value))) %>% # character to enable all to collapse into one column
    map_dfr(., bind_rows) %>%
  filter(lake_id != 1033) # omit Falls Lake)
  

# Data dictionary
# write.table(unique(lake_scale_data_paper$name), file = "clipboard", row.names = FALSE)
lake_scale_dictionary <- master_dictionary %>%
  filter(variable %in% unique(lake_scale_data$name))

# Are all values in data dictionary?
ifelse (c(colnames(lake_scale_data) %in% site_data_dictionary$variable,
          unique(lake_scale_data$name) %in% lake_scale_dictionary$variable) %>% 
          {!.} %>%
          sum(.) == 0,
        "Site data dictionary is complete", 
        "Site data dictionary is incomplete")

# write data
write.csv(x = lake_scale_data, 
          file = "communications/manuscript/data_paper/3_lake_scale.csv",
          row.names = FALSE)

# write dictionary
write.csv(x = lake_scale_dictionary, 
          file = "communications/manuscript/data_paper/3_lake_scale_dictionary.csv",
          row.names = FALSE)


# 4. DEPTH PROFILES----
# WIDE FORMAT
# see readDepthProfiles.R for primary data source
# depth_profile_all includes riverine, transitional, and lacustine. Although all
# other lakes only have a single depth profile, lets report all zones in the paper.

depth_profiles_data <- depth_profiles_all %>%
  mutate(
    # riverine and transitional zones. Move habitat from lake to site_id
    site_id = case_when(grepl("transitional", lake_id) ~ paste0(site_id, "_transitional"),
                        grepl("riverine", lake_id) ~ paste0(site_id, "_riverine"),
                        grepl("lacustrine", lake_id) ~ paste0(site_id, "_lacustrine"),
                        TRUE ~ as.character(site_id)),
    # remove transitional, riverine, lacustrine from lake_id
    # retain character class initially, then convert to numeric.
    lake_id = case_when(lake_id %in% c("69_riverine", "69_transitional", "69_lacustrine") ~ "69",
                        lake_id %in% c("70_riverine", "70_transitional", "70_lacustrine") ~ "70",
                        TRUE ~ lake_id))

# Data dictionary
depth_profiles_dictionary <- master_dictionary %>%
  filter(variable %in% colnames(depth_profiles_data))

# Are all values in data dictionary?
ifelse (colnames(depth_profiles_data) %in% depth_profiles_dictionary$variable %>% 
          {!.} %>%
          sum(.) == 0,
        "Site data dictionary is complete", 
        "Site data dictionary is incomplete")

# write data
write.csv(x = depth_profiles_data, 
          file = "communications/manuscript/data_paper/4_depth_profiles.csv",
          row.names = FALSE)

# write dictionary
write.csv(x = depth_profiles_dictionary, 
          file = "communications/manuscript/data_paper/4_depth_profiles_dictionary.csv",
          row.names = FALSE)



# 5. SITE DATA------------

# a.	Unit columns for all measurement
# b.	Flat file
# c.	lake_id
# d.	site_id
# e.	visit
# f. chemistry
# h.	Dissolved gas not ready

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
      # fix turbidity name
      mutate(name = case_when(name == "turb" ~ "turbidity",
                              TRUE ~ name)) %>%
      # create units columns
      mutate(units = case_when(name == "chla_sonde" ~ "ug_l",
                               name == "phycocyanin_sonde" ~ "ug_l",
                               name == "do" ~ "mg_l",
                               name == "ph" ~ "ph",
                               name == "sp_cond" ~ "us_cm",
                               name == "turbidity" ~ "ntu",
                               name == "temp" ~ "c",
                               TRUE ~ "FLY YOU FOOLS!"),
             value = as.numeric(value)) %>% # something in here is causing a character value?
      # NLA conventions
      mutate(value = case_when(name == "ph" ~ round(value, 2),
                               name == "temp" ~ round(value, 1),
                               name == "do" ~ round(value, 1),
                               name == "sp_cond" ~ round(value, 0),
                               name == "turbidity" ~ round(value, 1),
                               name == "chla_sonde" ~ round(value, 1),
                               TRUE ~ value)) %>%
      drop_na(value), # omit record if no value reported
    
    
    # SuRGE CHEMISTRY DATA
    chemistry_all %>%
      filter(sample_type != "blank") %>% # omit blanks
      select(-sample_type,
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
  ) %>%
  # enforce decimal points
  filter(!(name == "po4")) %>% # omit IC based po4 from TTEB
  mutate(value = case_when(
    # nitrogen analytes reported in ug_n_l
    name %in% c("nh4", "no2", "no2_3", "no3", "tn") ~ round(value, 1),
    # TP and colorimetric base p in ug_p_l
    name %in% c("op", "tp") ~ round(value, 1),
    # 5 decimals reported for tteb data
    name %in% metals ~ round(value, 5), # metals is from chemSampleList.R
    # anions in mg_l. decimals based on MDL. See Wiki, 
    # Anions: analytical methods, detection limits, and holding times/CIN-TTEB Table
    name %in% c("br", "f") ~ round(value, 3), 
    name %in% c("cl", "so4") ~ round(value, 2), 
    # based on NLA convention
    name %in% c("doc", "toc") ~ round(value, 2),
    # based on NLA convention
    name == "chla_lab" ~ round(value, 2),
    # error flag
    TRUE ~ value))

#write.table(unique(site_data$name), file = "clipboard", row.names = FALSE)

# Data dictionary
site_data_dictionary <- master_dictionary %>%
  filter(variable %in% colnames(site_data) |
         variable %in% unique(site_data$name))

# Are all values in data dictionary?
ifelse (c(colnames(site_data) %in% site_data_dictionary$variable,
          unique(site_data$name) %in% site_data_dictionary$variable) %>% 
          {!.} %>%
          sum(.) == 0,
"Site data dictionary is complete", 
"Site data dictionary is incomplete")

# write data
write.csv(x = site_data, 
          # writing data to repo rather than SharePoint
          file = "communications/manuscript/data_paper/5_site_data.csv",
          row.names = FALSE)

# write dictionary
write.csv(x = site_data_dictionary, 
          file = "communications/manuscript/data_paper/5_site_data_dictionary.csv",
          row.names = FALSE)


# 6. EMISSION RATES (POINT)----

# First need to compile correct chamber deployment times into an object
# pull chamber deployments from the chm_deply object created in writeSuRGElakesToGpkg for SuRGE
# and from the dat_2016 object for the 2016 sites
chm_dep <- bind_rows (
  chm_deply %>%
    select(lake_id, site_id, visit, chamb_deply_date_time)%>%
    mutate(chamb_deply_date_time_units="UTC"),
  
  dat_2016 %>%
    select(lake_id, site_id, visit, chamb_deply_date_time) %>%
    mutate(lake_id = as.numeric(lake_id), site_id = as.character(site_id),
           chamb_deply_date_time_units="UTC")
)

emission_rate_points_data_paper <- left_join(
  # primary data first
  dat %>%
    # add units for trap deployment and retrieval
    mutate(trap_deply_date_time_units="UTC",
           trap_rtrvl_date_time_units="UTC") %>%
    select(
      lake_id,
      site_id,
      visit,
      # air_temp,
      # air_temp_units,
      ch4_diffusion_best,
      ch4_diffusion_units,
      ch4_ebullition,
      ch4_ebullition_units,
      ch4_total,
      ch4_total_units,
      ch4_deployment_length,
      ch4_deployment_length_units,
      co2_diffusion_best,
      co2_diffusion_units,
      co2flag,
      co2_ebullition,
      co2_ebullition_units,
      co2_total,
      co2_total_units,
      co2_deployment_length,
      co2_deployment_length_units,
      trap_deply_date_time,
      trap_deply_date_time_units),
  # bind with chamber deployment dates (not distinguished by gas)
  chm_dep
) %>%
  # remove "best" from diffusion variable
  rename_with(.cols = contains("best"), ~sub(pattern = "_best", replacement = "", x = .)) %>%
  # enforce decimal points
  mutate(
    # smallest non-zero ch4 diffusive emission rate is 0.0277
    ch4_diffusion = round(ch4_diffusion, 2),
    # smallest non-zero ch4 ebullition emission rate is 4.906696e-06
    ch4_ebullition = round(ch4_ebullition, 6),
    # smallest non-zero ch4 total emission rate is 1.394003e-05
    ch4_total = round(ch4_total, 5),
    # smallest non-zero abs(co2 diffusive emission rate) is 2.519658
    co2_diffusion = round(co2_diffusion, 2),   
    # smallest non-zero co2 ebullition is 0.0001472518
    co2_ebullition = round(co2_ebullition, 4),
    # smallest non-zero abs(co2 total) is 0.0001876054
    co2_total = round(co2_total, 4))

# Data dictionary
emission_rate_points_data_paper_dictionary <- master_dictionary %>%
  filter(variable %in% colnames(emission_rate_points_data_paper))

# Are all values in data dictionary?
ifelse (c(colnames(emission_rate_points_data_paper) %in% emission_rate_points_data_paper_dictionary$variable) %>% 
          {!.} %>%
          sum(.) == 0,
        "Site data dictionary is complete", 
        "Site data dictionary is incomplete")

# write data
write.csv(
  x = emission_rate_points_data_paper,
  file = "communications/manuscript/data_paper/6_emission_rate_points.csv",
  row.names = FALSE
)

# write dictionary
write.csv(
  x = emission_rate_points_data_paper_dictionary,
  file = "communications/manuscript/data_paper/6_emission_rate_points_dictionary.csv",
  row.names = FALSE
)


# 7. EMISSION RATES LAKE------

emissions_lake_data_paper <- emissions_agg


# Data dictionary
emissions_lake_data_paper_dictionary <- master_dictionary %>%
  filter(variable %in% colnames(emissions_lake_data_paper))

# Reorder variables so they match the data dictionary
emissions_lake_data_paper <- emissions_lake_data_paper %>%
  relocate(emissions_lake_data_paper_dictionary$variable)

# Are all values in data dictionary?
ifelse (c(colnames(emissions_lake_data_paper) %in% emissions_lake_data_paper_dictionary$variable) %>% 
          {!.} %>%
          sum(.) == 0,
        "Site data dictionary is complete", 
        "Site data dictionary is incomplete")

# write data
write.csv(
  x = emissions_lake_data_paper,
  file = "communications/manuscript/data_paper/7_emissions_lake.csv",
  row.names = FALSE)

# write dictionary
write.csv(
  x = emissions_lake_data_paper_dictionary,
  file = "communications/manuscript/data_paper/7_emissions_lake_dictionary.csv",
  row.names = FALSE)

# 8. SITE DESCRIPTORS--------------
# -.	Site Descriptors
# -.	Lake_id
# -.	Site_id
# -.	Visit
# -.  Coordinates
# -.	site_depth
# -.	Units
# -.	Site weight from the survey design
# -.	Start and end dates of lake-specific sampling campaign
# -.	Filter based chlorophyll sampling date


site_descriptors_data <- 
  left_join(
    # keep all unique IDs in dat
    # DATA FIRST
    dat %>%
      select(lake_id, site_id, visit, site_depth, site_wgt, lat, long) %>%
      mutate(site_depth = round(site_depth, 1),
             site_depth_units = "m",
             site_wgt_units = "dimensionless",
             across(matches(c("lon|lat")), ~format(round(., 6), nsmall =6))),
    
    # Gather all available information pertaining sample dates, then calculate sample duration
    bind_rows(
      # chlorophyll sampling date first
      # chlorophyll samples collected across multiple days for 69 and 70
      # occasionally multiple samples collected over a few days (999, 2018 lake)
      read_excel(paste0(userPath, "/data/algalIndicators/pigments/surgeFilteredVolumes.xlsx")) %>%
        janitor::clean_names() %>%
        filter(sample_type == "UNK", analyte == "chlorophyll") %>%
        #filter(lake_id == 1000) %>%
        mutate(
          # remove transitional, riverine from lake_id
          # retain character class initially, then convert to numeric.
          lake_id = case_when(
            grepl("69", lake_id) ~ "69",
            grepl("70", lake_id) ~ "70",
            TRUE ~ lake_id),
          lake_id = gsub(".*?([0-9]+).*", "\\1", lake_id) %>% as.numeric,
          collection_date = as.Date(collection_date, format = "%m.%d.%Y")) %>%
        select(lake_id, visit, collection_date) %>%
        group_by(lake_id, visit) %>%
        reframe(collection_date = unique(collection_date)),
      
      
      # Now bring in trap deployment/retrieval date/time
      dat %>%
        select(lake_id, visit, trap_deply_date_time, trap_rtrvl_date_time) %>%
        mutate(across(contains("date_time"), as.Date)) %>%
        pivot_longer(-c(lake_id, visit), values_to = "collection_date") %>%
        select(-name)
    ) %>% # close bind_rows
      # calculate first and last sampling date per lake
      group_by(lake_id, visit) %>%
      summarize(
        sample_start = min(collection_date, na.rm = TRUE),
        sample_end = max(collection_date, na.rm = TRUE))
  ) %>% # close left join
  # Now date of chlorophyll sampling
  left_join(
    bind_rows(
      # ADD DATE SuRGE CHLOROPHYLL SAMPLES WERE COLLECTED
      read_excel(paste0(userPath, "/data/algalIndicators/pigments/surgeFilteredVolumes.xlsx")) %>%
        janitor::clean_names() %>%
        filter(sample_type == "UNK",
               analyte == "chlorophyll") %>%
        select(lake_id, site_id, visit, collection_date) %>%
        mutate(
          site_id = gsub(".*?([0-9]+).*", "\\1", site_id), # clean site_id values
          site_id = case_when(grepl("transitional", lake_id) ~ paste0(site_id, "_transitional"),
                              grepl("riverine", lake_id) ~ paste0(site_id, "_riverine"),
                              TRUE ~ as.character(site_id)),
          # remove transitional, riverine from lake_id
          # retain character class initially, then convert to numeric.
          lake_id = case_when(grepl("69", lake_id) ~ "69",
                              grepl("70", lake_id) ~ "70",
                              TRUE ~ lake_id),
          lake_id = gsub(".*?([0-9]+).*", "\\1", lake_id) %>% as.numeric) %>%
        rename(chla_collection_date = collection_date) %>%
        mutate(chla_collection_date = as.Date(chla_collection_date, format = "%m.%d.%Y")),
      
      # ADD DATE 2016 CHLOROPHYLL SAMPLES WERE COLLECTED
      # transcribed from field sheets
      tribble(~lake_id, ~site_id, ~visit, ~chla_collection_date,
              1023, 1, 1, as.Date("2016-07-26"), 
              1023, 30, 1, as.Date("2016-07-26"),
              1025, 31, 1, as.Date("2016-07-13"),
              1025, 4, 1, as.Date("2016-07-13"),
              1011, 1, 1, as.Date("2016-07-18"),
              1011, 16, 1, as.Date("2016-07-18"),
              1027, 34, 1, as.Date("2016-07-19"),
              1027, 7, 1, as.Date("2016-07-19"),
              1003, 31, 1, as.Date("2016-07-20"),
              1003, 6, 1, as.Date("2016-07-20"),
              1028, 31, 1, as.Date("2016-07-11"),
              1028, 7, 1, as.Date("2016-07-11"),
              1026, 33, 1, as.Date("2016-06-27"),
              1026, 10, 1, as.Date("2016-06-27"),
              1030, 28, 1, as.Date("2016-06-28"),
              1030, 7, 1, as.Date("2016-06-28"),
              1004, 33, 1, as.Date("2016-06-29"),
              1004, 3, 1, as.Date("2016-06-29"),
              1008, 32, 1, as.Date("2016-06-16"),
              1008, 12, 1, as.Date("2016-06-16"),
              1007, 21, 1, as.Date("2016-06-20"),
              1007, 5, 1, as.Date("2016-06-20"),
              1015, 15, 1, as.Date("2016-06-20"),
              1015, 7, 1, as.Date("2016-06-20"),
              1002, 30, 1, as.Date("2016-06-07"),
              1002, 4, 1, as.Date("2016-06-07"),
              1013, 30, 1, as.Date("2016-06-08"),
              1013, 8, 1, as.Date("2016-06-08"),
              1017, 33, 1, as.Date("2016-06-09"),
              1017, 1, 1, as.Date("2016-06-09"),
              1001, 18, 1, as.Date("2016-05-31"),
              1001, 4, 1, as.Date("2016-05-31"),
              1012, 41, 1, as.Date("2016-06-02"),
              1012, 4, 1, as.Date("2016-06-02"),
              1014, 6, 1, as.Date("2016-09-15"),
              1022, 9, 1, as.Date("2016-09-09"),
              1006, 31, 1, as.Date("2016-08-24"),
              1005, 14, 1, as.Date("2016-08-18"),
              1019, 4, 1, as.Date("2016-08-03"),
              1010, 46, 1, as.Date("2016-08-09"),
              1010, 7, 1, as.Date("2016-08-09"),
              1029, 21, 1, as.Date("2016-09-13"),
              1029, 4, 1, as.Date("2016-09-13"),
              1024, 23, 1, as.Date("2016-09-06"),
              1024, 4, 1, as.Date("2016-09-06"),
              1022, 32, 1, as.Date("2016-09-08"),
              1022, 9, 1, as.Date("2016-09-08"),
              1018, 40, 1, as.Date("2016-09-07"),
              1018, 2, 1, as.Date("2016-09-07"),
              1021, 28, 1, as.Date("2016-08-29"),
              1021, 3, 1, as.Date("2016-08-29"),
              1020, 34, 1, as.Date("2016-08-30"),
              1020, 1, 1, as.Date("2016-08-30"),
              1032, 16, 1, as.Date("2016-08-31"),
              1009, 29, 1, as.Date("2016-08-22"),
              1009, 4, 1, as.Date("2016-08-22"),
              1006, 31, 1, as.Date("2016-08-24"),
              1006, 3, 1, as.Date("2016-08-24"),
              1031, 31, 1, as.Date("2016-08-01"),
              1031, 3, 1, as.Date("2016-08-01"),
              1031, 31, 1, as.Date("2016-08-01")
      ) %>%
        mutate(site_id = as.character(site_id))
    ) # close bind_rows
  )  # close left_join

# Data quality checks
# Are all chl sample dates within sampling date range?
if (site_descriptors_data %>%
    filter(!is.na(chla_collection_date)) %>%
    mutate(date_check = !((chla_collection_date >= sample_start) & (chla_collection_date <= sample_end))) %>%
    summarize(date_check = sum(date_check)) %>% pull() > 1) {
  "All dates are good"
} else {
    "problem with dates"
  }


# Data dictionary
site_descriptors_dictionary <- master_dictionary %>%
  filter(variable %in% colnames(site_descriptors_data))

# Are all values in data dictionary?
ifelse (
  #TRUE if variable is in dictionary, FALSE if not
  colnames(site_descriptors_data) %in% site_descriptors_dictionary$variable %>% # TRUE if variable is present 
          {!.} %>% # convert TRUE to FALSE, and FALSE to TRUE
          sum(.) == 0, # all TRUE add up
        "Site data dictionary is complete", # if 0 (all variables are present) 
        "Site data dictionary is incomplete") # if not 0 (>=1 variable missing)

# write data
write.csv(x = site_descriptors_data, 
          file = "communications/manuscript/data_paper/8_site_descriptors_data.csv",
          row.names = FALSE)

# write dictionary
write.csv(x = site_descriptors_dictionary, 
          file = "communications/manuscript/data_paper/8_site_descriptors_dictionary.csv",
          row.names = FALSE)

# 9. METEOROLOGY-----------
# 4/18/2025 only have wind speed, air temp, and precipitation
met_data <- met_chamber %>%
  select(-temp_lake_mix_layer_c) %>%
  relocate(lake_id, site_id, visit, date_time, precipitation, wind_speed, temp_air_2m)

# Data dictionary
met_dictionary <- master_dictionary %>%
  filter(variable %in% colnames(met_data))

# Are all values in data dictionary?
ifelse (
  #TRUE if variable is in dictionary, FALSE if not
  colnames(met_data) %in% met_dictionary$variable %>% # TRUE if variable is present 
    {!.} %>% # convert TRUE to FALSE, and FALSE to TRUE
    sum(.) == 0, # all TRUE add up
  "Site data dictionary is complete", # if 0 (all variables are present) 
  "Site data dictionary is incomplete") # if not 0 (>=1 variable missing)

# write data
write.csv(x = met_data, 
          file = "communications/manuscript/data_paper/9_met_data.csv",
          row.names = FALSE)

# write dictionary
write.csv(x = met_dictionary, 
          file = "communications/manuscript/data_paper/9_met_dictionary.csv",
          row.names = FALSE)



# 10. PHYTOPLANKTON-------------------
phyto_data <-  read_excel(paste0(userPath,
                                 "data/algalIndicators/SuRGE Taxonomy 2021-23 v4.xlsx"), 
                          sheet = "SuRGE Taxonomy- 2021-23") %>%
  janitor::clean_names() %>%
  select(site_id, year_col, algal_group, phylum,class, order, family, genus, density) %>%
  # distinct(site_id) # no lacustrine...
  rename(lake_id = site_id) %>%
  mutate(lake_id = str_extract(lake_id, "(\\d+$)") %>% # extract numeric part of lake_id
           as.numeric(), # convert lake_id to numeric
         visit = case_when(lake_id == 147 & year_col == 2021 ~ 1,
                           lake_id == 147 & year_col == 2023 ~ 2,
                           lake_id == 148 & year_col == 2021 ~ 1,
                           lake_id == 148 & year_col == 2023 ~ 2,
                           lake_id == 250 ~ 2, # samples from visit 1 lost
                           lake_id == 281 ~ 2, # samples from visit 1 lost
                           TRUE ~ 1), # visit 1 for all others
         density_units = "cells_ml") %>%
  select(-year_col) %>%
  relocate(lake_id, visit) %>%
  filter(!is.na(lake_id))


# Data dictionary
phyto_dictionary <- master_dictionary %>%
  filter(variable %in% colnames(phyto_data))

# Are all values in data dictionary?
ifelse (
  #TRUE if variable is in dictionary, FALSE if not
  colnames(phyto_data) %in% phyto_dictionary$variable %>% # TRUE if variable is present 
    {!.} %>% # convert TRUE to FALSE, and FALSE to TRUE
    sum(.) == 0, # all TRUE add up
  "Site data dictionary is complete", # if 0 (all variables are present) 
  "Site data dictionary is incomplete") # if not 0 (>=1 variable missing)
          

# write data
write.csv(x = phyto_data, 
          file = "communications/manuscript/data_paper/10_phyto_data.csv",
          row.names = FALSE)

# write dictionary
write.csv(x = phyto_dictionary, 
          file = "communications/manuscript/data_paper/10_phyto_dictionary.csv",
          row.names = FALSE)

