
# DATA PULLED FROM API, THEN STORED LOCALLY
# CODE TO PULL FROM API IS COMMENTED OUT

# # devtools::install_github("USEPA/StreamCatTools", build_vignettes=FALSE)
# # download from API
# # Names of lakeCat variables
# # no longer pulling "catareasqkm", "wsareasqkm"! See 
# # https://github.com/USEPA/StreamCatTools/issues/62
# lake_cat_vars <- lc_get_params(param='name') %>% 
#   as_tibble %>%
#   # ignore older data
#   filter(!grepl(paste0(1984:2018, collapse = "|"), value)) %>%
#   pull 
#   
# 
# 
# # SuRGE comid
# surge_comid <- rbind(lake.list, lake.list.2016) %>% 
#   filter(!is.na(sample_year)) %>%
#   select(nhd_plus_waterbody_comid, lake_id) %>% 
#   distinct %>% # values repeated for revisits
#   filter(!is.na(nhd_plus_waterbody_comid)) %>% # NA breaks lc_get_data
#   rename(comid = nhd_plus_waterbody_comid)
# 
# dim(surge_comid) # 148 unique values
# 
# # Get lakeCat data
# lake_cat <- lc_get_data(metric = paste(lake_cat_vars, collapse=","), 
#                        aoi = 'cat,ws', 
#                        comid = surge_comid$comid) %>%
#   janitor::clean_names() %>%
#   # add lake_id
#   left_join(surge_comid)
# 
# write_csv(lake_cat, "SuRGE_Sharepoint/data/siteDescriptors/lake_cat.csv")

# READ LOCAL FILE
lake_cat <- read_csv("SuRGE_Sharepoint/data/siteDescriptors/lake_cat.csv")

lake_cat_abbv <- lake_cat %>%
  mutate(NitrogenCat = manurecat + cbnfcat + fertcat) %>%
  mutate(NitrogenWs = manurews + cbnfws + fertws) %>%
  select(
    comid,
    lake_id,
    kffactcat,
    kffactws,
    runoffcat,
    runoffws,
    omcat,
    omws,
    scat,
    sws,
    pctcrop2019cat,
    pctcrop2019ws,
    #catareasqkm, # not available in StreamCatTools.0.4.0
    #wsareasqkm, # not available in StreamCatTools.0.4.0
    NitrogenCat,
    NitrogenWs,
    npdesdenscat,
    npdesdensws,
    agkffactcat,
    agkffactws,
    damnidstorcat,
    damnidstorws,
    elevcat,
    elevws
  )

# Inventory records
dim(lake_cat_abbv) # 147 rows.  which one is missing?  

surge_comid[!(surge_comid$comid %in% lake_cat$comid_abbv), ] #800045123 is not in lakeCat

lake.list %>% filter(nhd_plus_waterbody_comid == 800045123) # Puerto Rico
