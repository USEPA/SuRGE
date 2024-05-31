# Names of lakeCat variables
lake_cat_vars <- lc_get_params(param='name') %>%
  as_tibble %>%
  # ignore older data
  filter(!grepl(paste0(1984:2018, collapse = "|"), value)) %>%
  pull


# SuRGE comid
surge_comid <- lake.list %>% 
  filter(!is.na(sample_year)) %>%
  select(nhd_plus_waterbody_comid) %>% 
  distinct %>% # values repeated for revisits
  pull
length(surge_comid) # 116 unique values

# Get lakeCat data
lake_cat <- lc_get_data(metric = paste(lake_cat_vars, collapse=","), 
                       aoi = 'catchment,watershed', 
                       comid = surge_comid) %>%
  janitor::clean_names()



# Inventory records
dim(lake_cat) # 115 rows.  which one is missing?  

surge_comid[!(surge_comid %in% lake_cat$comid)] #800045123 is not in lakeCat

lake.list %>% filter(nhd_plus_waterbody_comid == 800045123) # Puerto Rico
