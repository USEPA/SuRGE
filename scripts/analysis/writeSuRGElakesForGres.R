# PREP MINIMUM DATA FOR G-RES

# MORPHOMETRY DATA---------
str(morpho) # 147 lakes
unique(morpho$lake_id) # numeric

morpho %>%
  janitor::get_dupes(lake_id)


# LAKE POLYGONS-------------
str(surge_lakes) # sf object, 114 lakes
unique(surge_lakes$lake_id) # numeric
str(lakes_2016) # sf object, 33 lakes
unique(lakes_2016$lake_id) # numeric

lakes_foo <- c(unique(surge_lakes$lake_id),
                   unique(lakes_2016$lake_id))


# AGRREGATED EMISSION RATES------
str(emissions_agg) # 150 observations
unique(emissions_agg$lake_id) # 146 lakes. No Falls Lake

str(emissions_agg_annual) # 149 observations
unique(emissions_agg_annual$lake_id) # 146 lakes. No Falls Lake (1033) or Four Prong Lake (45)


# DATA INVENTORY-----------
# Do all lakes with morphometry estimates have a corresponding polygon?
morpho %>% filter(!(lake_id %in% lakes_foo)) # yes

# Do all lakes with morphometry estimates have emission estimates?
morpho %>% filter(!(lake_id %in% unique(emissions_agg$lake_id))) # no, Falls Lake

# Do all lake polygons have morphometry values
lakes_foo[!(lakes_foo %in% morpho$lake_id)] 

# any lakes with missing missing morphometry?
emissions_agg[!(emissions_agg$lake_id %in% unique(morpho$lake_id)),] # no

# any lakes with emission estimates missing a polygon?
emissions_agg[!(emissions_agg$lake_id %in% unique(lakes_foo)),] # no



## WRITE GPKG----
bind_rows(list(surge_lakes, lakes_2016)) %>% # merge polygons
  st_make_valid() %>%
  left_join(morpho) %>%
  left_join(emissions_agg) %>%
  left_join(emissions_agg_annual %>% 
              filter(temp_source == "mixed_layer") %>%
              select(-temp_source)) %>%
  left_join(dat %>% 
              select(lake_id, visit, ag_eco9_nm, year_completed) %>%
              distinct) %>%
  st_write(., file.path( "../../../lakeDsn", paste0("all_lakes_gres_", Sys.Date(), ".gpkg")), # write to .gpkg
           layer = "all_lakes",
           append = FALSE)

## WRITE DATA LIBRARY------
# get a list of columns in .gpkg that require a defintion
bind_rows(list(surge_lakes, lakes_2016)) %>% # merge polygons
  st_drop_geometry() %>%
  left_join(morpho) %>%
  left_join(emissions_agg) %>%
  left_join(emissions_agg_annual %>% 
              filter(temp_source == "mixed_layer") %>%
              select(-temp_source)) %>%
  left_join(dat %>% select(lake_id, visit, ag_eco9_nm, year_completed)) %>%
  colnames %>% 
  write.table("clipboard", sep = ",", row.names = FALSE) # past below

data_dictionary_gres <- tribble(
  ~variable, ~definition,
  "lake_name", "Reservoir name used locally. Sometimes this is the NHD GNIS name, other times a name used locally.",
  "lake_id", "Unique ID for each reservoir",
  "surface_area", "Best estimate of reservoir surface area during time of sampling (m2)",
  "shoreline_length", "Length of shoreline (m)",
  "shoreline_development", "Shoreline development is a measure of the complexity of the lake shoreline. It is simply the ratio of the shoreline length (i.e. perimeter) to the perimeter of an equally sized circle.",
  "max_depth", "Max depth estimated based on the assumption that the slope of the surrounding topography is similar to the bathymetry of the lake (m).", 
  "volume", "The volume is calculated using maximum lake depth and maximum distance ratio to transform all pixels and thus, distances, to an estimated depth. These depths are multiplied by the area of the pixel and summed (m3).",
  "mean_depth", "Average depth of lake calculated as lake volume divided by lake surface area (m).",
  "max_width", "Maximum lake width is defined as the maximum in lake distance that is perpendicular to the maximumlakelength (m).",
  "mean_width", "Mean lake width is the result of lake surface area divded by the maximum length (m).",
  "fetch", "The function calculates the maximum in lake distance of a line along an input bearing (m). An arbitrary bearing of 0 degrees used here. See max_length.", ###COME BACK HERE. WHAT BEARING?
  "max_length", "Maximum lake length is defined as the longest open water distance of a lake (m).",
  "circularity", "Circularity compares the area of the lake (m2) with the area of the minimum boundary circle (MBC; m2) around it. It differentiates between circular (value approaches 1) and elongated lakes (value approaches 0). Unitless.",
  "dynamic_ratio", "Dynamic ratio (Håkanson 1982) is an area (in km2) to mean depth (m) ratio of the lake. Low values indicate lakes that are bowl-shaped, whereas high values are associated to dish-like lakes. Area is square-rooted to account for size dependency. Units are km m−1.",
  "littoral_fraction", "Littoral fraction is the relative proportion of lake surface area that is shallower than 2.5 m. 1-(1-(2.5/max depth))^((max depth/mean depth) - 1).",
  "mean_depth_measured", "Mean of depth measurements made at 15 or more randomly selected locations (m).",
  "max_depth_measured", "Maximum depth (m) measured at 15 or more measurement locations selected via a generalized random tessellation stratified surey design..",
  "visit", "First or second visit to lake.",
  "ch4_ebullition_lake", "Lake-scale aggregation of CH4 ebullition observed at 15 or more measurement locations selected via a generalized random tessellation stratified surey design.",
  "ch4_diffusion_lake", "Lake-scale aggregation of CH4 diffusion observed at 15 or more measurement locations selected via a generalized random tessellation stratified surey design.",
  "ch4_total_lake", "Lake-scale aggregation of total CH4 emissions observed at 15 or more measurement locations selected via a generalized random tessellation stratified surey design. Total is only computed at sites where both ebullition and diffusion were measured.",
  "co2_ebullition_lake", "Lake-scale aggregation of CO2 ebullition observed at 15 or more measurement locations selected via a generalized random tessellation stratified surey design.",
  "co2_diffusion_lake", "Lake-scale aggregation of CO2 diffusion observed at 15 or more measurement locations selected via a generalized random tessellation stratified surey design.",
  "co2_total_lake", "Lake-scale aggregation of total CO2 emissions observed at 15 or more measurement locations selected via a generalized random tessellation stratified surey design. Total is only computed at sites where both ebullition and diffusion were measured.",
  "ch4_ebullition_std_error_lake", "standard error of lake-scale mean CH4 ebullition estimate",
  "ch4_diffusion_std_error_lake", "standard error of lake-scale mean CH4 diffusion estimate",
  "ch4_total_std_error_lake", "standard error of lake-scale mean total CH4 emission estimate",
  "co2_ebullition_std_error_lake", "standard error of lake-scale mean CO2 ebullition estimate",
  "co2_diffusion_std_error_lake", "standard error of lake-scale mean CO2 diffusion estimate",
  "co2_total_std_error_lake", "standard error of lake-scale mean total CO2 emission estimate",
  "ch4_ebullition_margin_of_error_lake", "Amount of random sampling error in the lake-scale CH4 ebullition estimate. The larger the margin of error, the less confidence one should have that a survey result would reflect the result of a census of the entire population.",
  "ch4_diffusion_margin_of_error_lake", "Amount of random sampling error in the lake-scale CH4 diffusion estimate. The larger the margin of error, the less confidence one should have that a survey result would reflect the result of a census of the entire population.",
  "ch4_total_margin_of_error_lake", "Amount of random sampling error in the lake-scale total CH4 emission estimate. The larger the margin of error, the less confidence one should have that a survey result would reflect the result of a census of the entire population.",
  "co2_ebullition_margin_of_error_lake", "Amount of random sampling error in the lake-scale CO2 ebullition estimate. The larger the margin of error, the less confidence one should have that a survey result would reflect the result of a census of the entire population.",
  "co2_diffusion_margin_of_error_lake", "Amount of random sampling error in the lake-scale CO2 diffusion estimate. The larger the margin of error, the less confidence one should have that a survey result would reflect the result of a census of the entire population.",
  "co2_total_margin_of_error_lake", "Amount of random sampling error in the lake-scale total CO2 emission estimate. The larger the margin of error, the less confidence one should have that a survey result would reflect the result of a census of the entire population.",
  "ch4_ebullition_lcb95pct_lake", "95% lower confidence bound of lake-scale mean CH4 ebullition estimate",
  "ch4_diffusion_lcb95pct_lake", "95% lower confidence bound of lake-scale mean CH4 diffusion estimate",
  "ch4_total_lcb95pct_lake", "95% lower confidence bound of lake-scale mean total CH4 emission estimate",
  "co2_ebullition_lcb95pct_lake", "95% lower confidence bound of lake-scale mean CO2 ebullition estimate",
  "co2_diffusion_lcb95pct_lake", "95% lower confidence bound of lake-scale  mean CO2 diffusion estimate",
  "co2_total_lcb95pct_lake", "95% lower confidence bound of lake-scale  mean total CO2 emission estimate",
  "ch4_ebullition_ucb95pct_lake", "95% upper confidence bound of lake-scale mean CH4 ebullition estimate",
  "ch4_diffusion_ucb95pct_lake", "95% upper confidence bound of lake-scale mean CH4 diffusion estimate",
  "ch4_total_ucb95pct_lake", "95% upper confidence bound of lake-scale mean total CH4 emission estimate",
  "co2_ebullition_ucb95pct_lake", "95% upper confidence bound of lake-scale mean CO2 ebullition estimate",
  "co2_diffusion_ucb95pct_lake", "95% upper confidence bound of lake-scale  mean CO2 diffusion estimate",
  "co2_total_ucb95pct_lake", "95% upper confidence bound of lake-scale  mean total CO2 emission estimate",
  "ch4_ebullition_lake_annual", "Lake-scale aggregation of CH4 ebullition observed at 15 or more measurement locations selected via a generalized random tessellation stratified surey design. Data annualized by applying a temperature correction based on mean monthly mixed layer water temperature and Boltzmann-Arrhenius equation with an activation energy of 0.96 eV (Yvon-Durocher et al. 2014). ",
  "ch4_diffusion_lake_annual", "Lake-scale aggregation of CH4 diffusion observed at 15 or more measurement locations selected via a generalized random tessellation stratified surey design. Data annualized by applying a temperature correction based on mean monthly mixed layer water temperature and Boltzmann-Arrhenius equation with an activation energy of 0.96 eV (Yvon-Durocher et al. 2014). ",
  "ch4_total_lake_annual", "Lake-scale aggregation of total CH4 emissions observed at 15 or more measurement locations selected via a generalized random tessellation stratified surey design. Data annualized by applying a temperature correction based on mean monthly mixed layer water temperature and Boltzmann-Arrhenius equation with an activation energy of 0.96 eV (Yvon-Durocher et al. 2014). ",
  "ch4_ebullition_units_lake", "CH4 ebullition rate units",
  "ch4_diffusion_units_lake", "CH4 diffusion rate units",
  "ch4_total_units_lake", "CH4 total emission rate units",
  "co2_ebullition_units_lake", "CO2 ebullition rates units",
  "co2_diffusion_units_lake", "CO2 diffusion rates units",
  "co2_total_units_lake", "CO2 total rates units",
  "ag_eco9_nm", "level 9 aggregated ecoregion",
  "year_completed", "year that waterbody was impounded or created (e.g. excavated)"
)

# make sure all variables are included in dictionary
gpkg_names <- bind_rows(list(surge_lakes, lakes_2016)) %>% # merge polygons
  st_drop_geometry() %>%
  left_join(morpho) %>%
  left_join(emissions_agg_annual %>% 
              filter(temp_source == "mixed_layer") %>%
              select(-temp_source)) %>%
  left_join(dat %>% select(lake_id, visit, ag_eco9_nm, year_completed)) %>%
  colnames

gpkg_names[!(gpkg_names %in% data_dictionary_gres$variable)] # all accounted for

st_write(data_dictionary_gres, 
         dsn = file.path(paste0(userPath, "lakeDsn/all_lakes_gres_", Sys.Date(), ".gpkg")),
         layer = "data_dictionary", append = FALSE)

