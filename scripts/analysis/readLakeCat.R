# Names of lakeCat variables
lake_cat_vars <- lc_get_params(param='name') %>%
  as_tibble %>%
  # ignore older data
  filter(!grepl(paste0(1984:2018, collapse = "|"), value)) %>%
  pull


# SuRGE comid
surge_comid <- rbind(lake.list, lake.list.2016) %>% 
  filter(!is.na(sample_year)) %>%
  select(nhd_plus_waterbody_comid, lake_id) %>% 
  distinct %>% # values repeated for revisits
  filter(!is.na(nhd_plus_waterbody_comid)) %>% # NA breaks lc_get_data
  rename(comid = nhd_plus_waterbody_comid)

dim(surge_comid) # 148 unique values

# Get lakeCat data
lake_cat <- lc_get_data(metric = paste(lake_cat_vars, collapse=","), 
                       aoi = 'catchment,watershed', 
                       comid = surge_comid$comid) %>%
  janitor::clean_names() %>%
  # add lake_id
  left_join(surge_comid)

lake_cat_abbv<-lake_cat %>%
  mutate(NitrogenCat=manurecat+cbnfcat+fertcat)%>%
  select(comid, lake_id, kffactcat,runoffcat,omcat,scat,pctcrop2019cat,catareasqkm,NitrogenCat,
         npdesdenscat,agkffactcat,damnidstorcat,elevcat)


# Inventory records
dim(lake_cat) # 147 rows.  which one is missing?  

surge_comid[!(surge_comid$comid %in% lake_cat$comid), ] #800045123 is not in lakeCat

lake.list %>% filter(nhd_plus_waterbody_comid == 800045123) # Puerto Rico
