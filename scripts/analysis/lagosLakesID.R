## Create Lake-link Crosswalk, Compile LAGOS Data for Surge Sites 
## Last edited 6/28/2024

# Installed via renv
# devtools::install_github("cont-limno/LAGOSUS", dependencies = TRUE)

# loaded from masterlibrary.R
# library(LAGOSUS)
# library(httr) #for reading trophic status lagos data, only necessary for first run


# 1. READ LOCUS-LAKE_LINK--------

# LOCUS-LAKE_LINK
# [6/12/2024] working on Jake's machine!!  If acting up, skip to line 18
lagosus_get(dest_folder = lagosus_path()) # run once, then hash out
locus <- lagosus_load(modules = c("locus"))
names(locus)
locus_link <- locus$locus$lake_link

# Can also read file from stored file or web if the above isn't working.
# https://portal.edirepository.org/nis/mapbrowse?packageid=edi.854.1
# ll <- read.csv(paste0(userPath, "data/siteDescriptors/lake_link_lagos.csv")) # downloded locally
# ll <- read_csv("https://portal.edirepository.org/nis/dataviewer?packageid=edi.854.1&entityid=5488e333ce818597fa3dbfc9b4e0c131") # from web

# Now create single record for each lake
# There are multiple lagosus_legacysiteids and wqp_monitoringlocationidentifiers values per lake
locus_link_aggregated <- locus_link %>%
  group_by(nhdplusv2_comid) %>% # unique per lake
  filter(row_number() == 1) %>% # first record for each comid. alternatives: slice_head(1) and  top_n(n = 1) 
  select(lagoslakeid, nla2012_siteid, nla2007_siteid, 
         lake_nhdid, # this is NHD HR Permanent ID
         lake_namegnis, nhdhr_gnisid, # I believe these are both from NHD HR
         nhdplusv2_comid, 
         lake_reachcode) %>% # in at least one istance I checked: lake_reachcode = NHDHD REACHCODE = NHDPlusV2 REACHCODE
  rename(nla07_site_id = nla2007_siteid, # per SuRGE convention
         nla12_site_id = nla2012_siteid)

# 2. READ LOCUS-LAKE_CHARACTERISTICS------------
locus_characteristics <- locus$locus$lake_characteristics

# object contains many variables we will get from other sources (e.g. morphometry from Jeff,
# watershed from Alex).  Subset to connectivity variables unique to lagos
locus_connectivity <- locus_characteristics %>% 
  select(lagoslakeid, lake_connectivity_class, lake_connectivity_fluctuates, lake_connectivity_permanent, 
         lake_lakes4ha_upstream_ha, lake_lakes4ha_upstream_n, lake_lakes1ha_upstream_ha, lake_lakes1ha_upstream_n, 
         lake_waterarea_ha, #placeholder for surface area
         lake_lakes10ha_upstream_n, lake_lakes10ha_upstream_ha)

# 3. READ LAGOS RESERVOIRS----------
#emailed lead author of networks paper, Katelyn King

# inUrl1  <- "https://pasta.lternet.edu/package/data/eml/edi/1016/2/dac3a0d7e34070639f4894ccc316cbd1" 
# infile1 <- tempfile()
# try(download.file(inUrl1,infile1,method="curl"))
# if (is.na(file.size(infile1))) download.file(inUrl1,infile1,method="auto")
# 
# 
# dt1 <-read.csv(infile1,header=F 
#                ,skip=1
#                ,sep=","  
#                ,quot='"' 
#                , col.names=c(
#                  "lagoslakeid",     
#                  "lake_rsvr_nidid",     
#                  "lake_nhdid",     
#                  "neon_zoneid",     
#                  "lake_rsvr_model_class",     
#                  "lake_lat_decdeg",     
#                  "lake_lon_decdeg",     
#                  "lake_connectivity_class",     
#                  "lake_rsvr_probnl",     
#                  "lake_rsvr_probrsvr",     
#                  "lake_rsvr_probdiff",     
#                  "lake_rsvr_model",     
#                  "lake_rsvr_nlneardam_flag",     
#                  "lake_rsvr_rsvrisolated_flag",     
#                  "lake_rsvr_classmethod",     
#                  "lake_centroidstate",     
#                  "lake_namelagos",     
#                  "lake_shorelinedevfactor",     
#                  "lake_rsvr_nidlat_decdeg",     
#                  "lake_rsvr_nidlon_decdeg",     
#                  "lake_shape",     
#                  "FType",     
#                  "lake_rsvr_class"    ), check.names=TRUE)
# 
# unlink(infile1)
# 
# locus_reservoir <- dt1 %>%
#   select(lagoslakeid,
#          lake_rsvr_probrsvr,
#          lake_rsvr_classmethod,
#          lake_rsvr_class)

# 4. READ LAGOS NETWORKS FILE-----------

#downloaded the nets_networkmetrics_medres file from https://doi.org/10.6073/pasta/98c9f11df55958065985c3e84a4fe995
#saved to the SuRGE site descriptors subfolder of the data folder

ln<-read_csv(file=(paste0(userPath, "data/siteDescriptors/nets_networkmetrics_medres_LAGOS_NETWORKS.csv")))
#received email from Katelyn King about parsing issues on 8/7/2024... she explained that
#this is due to their being multiple dam ids associated with lake_nets_nearestdamup_id
#if we decide we want to use this column she gave some script for pulling out each id
# read_csv(file) %>% 
#   tidyr:: separate(col= lake_nets_nearestdamup_id, 
#                    into = c("dam1", "dam2" , "dam3"),
#                    sep = ";")


lagos_network<- ln %>%
  select(lagoslakeid,lake_nets_nearestdamup_km,lake_nets_totaldamup_n)


# 5. READ LAGOS TROPHIC STATUS-----

# lagos productivity estimates
# preprint link is: https://www.biorxiv.org/content/10.1101/2024.05.10.593626v1
# data repository here: https://portal.edirepository.org/nis/mapbrowse?scope=edi&identifier=1427
# url_lagos <- "https://portal-s.edirepository.org/nis/dataviewer?packageid=edi.1427.3&entityid=3cb4f20440cbd7b8e828e4068d2ab734" # deprecated
url_lagos <-  "https://pasta.lternet.edu/package/data/eml/edi/1427/1/3cb4f20440cbd7b8e828e4068d2ab734" 
# 7 GB, takes about 15 minutes to download via httr.  Download once, save to disk, 
# then load from disk.  Much faster.
httr::GET(url_lagos, progress(), write_disk(tf <- tempfile(fileext = ".csv"))) # about 30 minutes at AWBERC
lagos_ts <- read.csv(tf) # another 15 minutes

# Alternatively, load .csv stored locally
# lagos_ts <- read_csv("C:/Users/JBEAULIE/OneDrive - Environmental Protection Agency (EPA)/GIS_data/LAGOS_US/LAGOS_US_LANDSAT_Predictions_v1_QAQC.csv")

# enforce naming conventions, define time
# this takes a few minutes
lagos_ts <- lagos_ts %>%
   janitor::clean_names() %>%
   mutate(month = gsub( " .*$", "", sensing_time) %>% as.Date(., format = "%Y-%m-%d") %>% lubridate::month(.),
         year = gsub( " .*$", "", sensing_time) %>% as.Date(., format = "%Y-%m-%d") %>% lubridate::year(.)) %>%
   select(-sensing_time)

# save to SharePoint, read from SharePoint to save time.
# saveRDS(lagos_ts, paste0(userPath, "data/siteDescriptors/lagos_ts.rds"))
# lagos_ts <- readRDS(paste0(userPath, "data/siteDescriptors/lagos_ts.rds"))


# 6. READ SURGE LAKES-------
# Prepare a list of SuRGE lakes (+2016, Falls Lake) to merge with LAGOS data
# Want a unique record for each lake + visit. 147, 148, 250, 281 sampled twice
lake_list_for_lagos_merge <- lake.list.all %>% # see readSurgeLakes.R
  # collapse lacustrine/riverine/transitional into one lake
  mutate(lake_id = case_when(lake_id %in% c("69_lacustrine", "69_riverine", "69_transitional") ~ "69",
                             lake_id %in% c("70_lacustrine", "70_riverine", "70_transitional") ~ "70",
                             TRUE ~ lake_id),
         lake_id = as.numeric(lake_id)) %>%
  filter(lake_id != 1033) %>% # exclude Falls Lake, too complicated right now [10/18/2024]
  mutate(nhdplusv2_comid = as.character(nhd_plus_waterbody_comid)) %>% # not sure why this is necessary
  select(lake_id, visit, nhdplusv2_comid) %>% # gnis_name, gnis_id included earlier. needed?
  distinct() # this collapses 69, 70, into one record each


# We want to add sample month to enable more precise matching with LAGOS trophic status estimates.
# LAGOS only covers 1984 - 2020, so we can only match specific months for surge
# sites sampled during that time frame: Falls Lake (2014), R10 (2018, 2020), CIN (2016/2020).

# 69 and 70 sampled in June and July, so 2 rows for each lake
# June conditions are likely more related to observed emissions,
# so only keep June record for merging with LAGOS TS.
# 147, 148, 250, 281 were resampled, so 2 rows for each lake
surge_sample_month_year <- fld_sheet %>%
  mutate(sample_year = lubridate::year(trap_deply_date),
         sample_month = lubridate::month(trap_deply_date),
         # convert 69/70 lacustrine.. from the missouri river to just 69/70
         lake_id = case_when(grepl("69_", lake_id) ~ "69",
                             grepl("70_", lake_id) ~ "70",
                             TRUE ~ lake_id),
         lake_id = as.numeric(lake_id)) %>%
  filter(!is.na(sample_year), # exclude empty rows (eval_status == PI/NS/TS) 
         !(lake_id %in% 69:70 & sample_month == 7)) %>% # exclude July for 69/70 (see above) 
  distinct(lake_id, visit, sample_year, sample_month) # collapse 69/70 into one record each


sample_month_year_2016 <- dat_2016 %>%
  mutate(sample_year = lubridate::year(trap_deply_date_time),
         sample_month = lubridate::month(trap_deply_date_time)) %>% 
  select(lake_id, visit, sample_year, sample_month) %>%
  distinct %>%
  filter(complete.cases(.)) 
  

# merge sample months with surge_sites
# naively joins on lake_id and visit
lake_list_for_lagos_merge_ts <- full_join(lake_list_for_lagos_merge, 
                         bind_rows(surge_sample_month_year, sample_month_year_2016))


dim(lake_list_for_lagos_merge_ts) # 150. only 146 lakes (falls lake excluded), but revisits for 4 



# 7. ADD LAGOS LAKE ID TO LIST OF SURGE SAMPLE MONTH AND YEAR-----
# Need to add lagoslakeid to surge_sites for use in trophic status processing.
# lagoslakeid and nhdplusv2 are in locus-lake_link$lagoslakeid
# nhdplusv2_comid is in lake_list_for_lagos_merge_ts
# therefore merge on nhdplusv2_comid

# Add other locus-lake_link and locus-lake_characteristic data after trophic
# status has been merged
lake_list_for_lagos_merge_ts_lagoslakeid <- left_join(lake_list_for_lagos_merge_ts,
                                                      locus_link_aggregated %>% # dervied from locus-lake_link$lagoslakeid
                                                        select(lagoslakeid, nhdplusv2_comid), # only keep merge variable and lagoslakeid
                                                      by = "nhdplusv2_comid") %>% # will naively join on this, specifying for clarity
  #  add the Lagos link for 10, 1009, and 1010 since lagos lacked the comid ID to link to SuRGE
  mutate(lagoslakeid = case_when(lake_id == 10 ~ 201797,
                                 lake_id == 1009 ~ 260194,
                                 lake_id == 1010 ~ 260193,
                                 TRUE ~ lagoslakeid)) %>%
  select(-nhdplusv2_comid) # remove merging variable, no longer needed
dim(lake_list_for_lagos_merge_ts_lagoslakeid) # 150. only 146 lakes (falls lake excluded), but revisits for 4 

# any missing lagos_id
lake_list_for_lagos_merge_ts_lagoslakeid %>% 
  filter(is.na(lagoslakeid)) %>% {dim(.)} # only two lakes w/out LAGOS record!  One is PR.


# 8. MERGE LAGOS TROPHIC STATUS-------
# lagos_ts is a big file, largely because it contains a time series of observations
# for 187,000 lakes. Lets see how many surge lakes have a lagos trophic status value.

# pull out observations for SuRGE lakes
lagos_ts_small <- lagos_ts %>%
  filter(lagoslakeid %in% lake_list_for_lagos_merge_ts_lagoslakeid$lagoslakeid) %>%
  # the columns specified below have variable values across the time series and would need to be condensed
  # to a single value when predicted chl a is aggregated by month or season. It doesn't
  # make sense to compute a summary stat for these (e.g. mean, median), and unique()
  # would create multiple values per month/season. Best to omit these variables.
  select(-c(negative_reflectance_min, negative_reflectance_median, pixel_perc_of_max, duplicate_day, qaqc_recommend))

# lakes 1000 (Puerto Rico) and 14 don't have lagos ID. lake 166 has lagos ID, but no
# trophic status records
lake_list_for_lagos_merge_ts_lagoslakeid %>%
  filter(!(lagoslakeid %in% lagos_ts_small$lagoslakeid)) # only 3 lakes without lagos trophic status records
dim(lagos_ts_small) # 76,848 observations

# inspect the trophic status data
# minimum of 155 observations/lake, max of 1331/lake, median of 511/lake
lagos_ts_small %>% 
  group_by(lagoslakeid) %>%
  summarise(n_obs = sum(!is.na(chl_predicted))) %>%
  {summary(.$n_obs)}

# visualize data
ggplot(lagos_ts_small, aes(as.factor(lagoslakeid), chl_predicted)) +
  geom_point()

# Merge and aggregate trophic status time series

# 1. merge trophic status with lake IDs
lagos_ts_agg <- left_join(lake_list_for_lagos_merge_ts_lagoslakeid, lagos_ts_small, na_matches = "never", 
                         relationship = "many-to-many") %>% # lake w/multiple visits have multiple records
  # 79,296, including three surge lakes without lagoslakeid match to lagos_ts. These
  # single observations are appended to end of record of matched observations.
  # create variable to uniquely identify each 'lake x sample_month x visit'. 
  mutate(split_variable = paste(lake_id, sample_month, visit)) %>% # 148 sampled on month 8 is 21 and 23, but visit uniquely IDs these
  split(.$split_variable) %>% # split into named list elements: format = (lake_id sample_month visit)
  
  # 2. create logicals to define aggregation periods. See github issue 106:
  # "For predicting CO2, I like the idea of pulling chlorophyll data from just the month-of-year that matches 
  # the sampling date from a few previous years (e.g. 2018-2020)... we can see how well they match our measured 
  # chlorophyll. For predicting CH4, maybe it makes more sense to get a full June-September average across many 
  # years (as a measure of overall productivity that gets integrated into the sediment?"
  # for troubleshooting: '69 7 1' sampled in 2021  '239 7 1' sampled in 2018
  map_df(~mutate(., 
                 filter_month = case_when(month == sample_month ~ TRUE, # TRUE if lagos data collected same month as SuRGE sampling (not necessarily same year)
                                          # lagoslakeid %in% c(6445, 6573) & month == 8 ~ TRUE, # sampled in month 9, but no data available for that month
                                          # lagoslakeid == 2039 & month == 7 ~ TRUE, # sampled in month 6, but no data available for that month
                                          TRUE ~ FALSE), # else FALSE
                 filter_season = case_when(month %in% 6:9 ~ TRUE, # SuRGE season (June - September of al years)
                                           TRUE ~ FALSE), # else FALSE
                 filter_year = case_when(year %in% (unique(sample_year) - 3):unique(sample_year) ~ TRUE, # true within 3 years of sampling (e.g. 2020,2021,2022,2023 for 2023 lakes)
                                         TRUE ~ FALSE)# FALSE for all other conditions
         ) %>% 
           # 3. calculate means for the aggregation periods defined above
           summarize(
             # First for the sampling month
             across(contains("predicted"), # for all lagos predicted variables
                    ~mean(.[filter_month == TRUE & filter_year == TRUE]), # calculate mean for specified month and year(s)
                    # specify new column names. Must change 'predicted' to 'predict', else the new variable will be grabbed in next across
                    .names = "{str_replace(.col, 'predicted', 'predict')}_sample_month"), # change 'predicted' to 'predict,'append "_sample_month" to variable name 
             # next for sampling season
             across(contains("predicted"), 
                    ~mean(.[filter_season == TRUE & filter_year == TRUE]), # calculate mean for specified months and year(s)
                    .names = "{.col}_sample_season"), # append "_sample_season" to variable name 
             # need to retain unique identifiers
             distinct(across(c(lake_id, lagoslakeid, visit)))) %>%
           rename_with(~str_replace(., "_predict_", "_predicted_"), contains("_predict_")) # change 'predict' back to 'predicted'
  )

dim(lagos_ts_agg) # 150 observations    
lagos_ts_agg %>% distinct(lake_id, visit) # 150 lake_id X visit records
lagos_ts_agg %>% distinct(lake_id) # 146 unique lakes, good (Falls lake omitted)


# 9. ADD NHDPLUSV2 COMID-----------
lagos_ts_agg <- lagos_ts_agg %>%
  inner_join(lake_list_for_lagos_merge %>% select(lake_id, visit, nhdplusv2_comid))
# merges on lagoslakeid and visit
dim(lake_list_for_lagos_merge) #150
dim(lagos_ts_agg) #150

# 9. MERGE LAGOS-LOCUS-------------
lagos_ts_agg_link <- left_join(lagos_ts_agg,
                                           locus_link_aggregated,
                                           # lagoslakeid was previously assigned to trophic
                                           # status data. nhdplusv2_comid comes from 'surge_sites'.
                                           # left_join will naturally join on these variables,
                                           # specifying here for clarity.
                                           by = c("lagoslakeid", "nhdplusv2_comid")) %>%
  # add NHD high resolution IDs to the reservoirs that aren't in Lagos or where Lagos doesn't have an nhdplus COMID
  mutate(lake_nhdid = case_when(lake_id == 14 ~ "{26f31221-6370-4bfa-a387-5b9665aae9f3}",
                                lake_id == 1000 ~ "26441842",
                                lake_id == 10 ~ "605A5DB3-01F6-4EC4-9EC4-640CD814795F",
                                lake_id == 1009 ~ "120022128",
                                lake_id == 1010 ~ "120021486",
                                TRUE ~ lake_nhdid)) #%>%
# # 1009 Carr Fork Lake is symbolized as a flow path in NHDPlusV2 with comid ID 456124.  Not including here.
# mutate(nhd_plus_waterbody_comid = case_when(lake_id == 1009 ~ 456124
#                                             TRUE ~ nhd_plus_waterbody_comid))

dim(lagos_ts_agg_link ) # 150, good
lagos_ts_agg_link %>% filter(is.na(lagoslakeid)) %>% {dim(.)} # only two lakes w/out LAGOS record!  One is PR.



# 10. MERGE LAGOS-LAKE CONNECTIVITY-------------
lagos_ts_agg_link_connectivity <- left_join(lagos_ts_agg_link,
                                                        locus_connectivity)

dim(lagos_ts_agg_link_connectivity) # 150, good

# how many missing
lagos_ts_agg_link_connectivity %>% 
  filter(is.na(lake_connectivity_class)) %>% {dim(.)} # 3 missing connectivity, PR, New Melones, and 14

# which missing
lagos_ts_agg_link_connectivity %>% 
  filter(is.na(lake_connectivity_class)) %>%
  select(lagoslakeid, lake_id, visit)

# 11. ADD LAGOS-NETWORK TO MERGE, not adding to final predictors yet----------
lagos_ts_agg_link_connectivity_network <- left_join(lagos_ts_agg_link_connectivity, lagos_network)


# 12. MERGE LAGOS-RESERVOIR-------------  
#-#- not adding to final predictors yet since not convinced it will be useful 
# lagos_ts_agg_link_connectivity_network_reservoir <- left_join(lagos_ts_agg_link_connectivity_network,
#                                                                   locus_reservoir)
# dim(lagos_ts_agg_link_connectivity_network_reservoir) # 150, good
# lagos_ts_agg_link_connectivity_network_reservoir %>% 
#   filter(is.na(lake_rsvr_class)) %>% {dim(.)} #3 missing, the 2 above + Cloud
# 
# lagos_ts_agg_link_connectivity_network_reservoir %>% 
#   filter(is.na(lake_rsvr_class)) %>%
#   select(lagoslakeid, lake_id, visit)
#LAGOS incorrectly predicts 10 reservoirs as lakes

# 13. RENAME FINAL MERGED OBJECT---------
lagos_links <- lagos_ts_agg_link_connectivity_network
#rm(lagos_ts_agg_surge_sites_link_connectivity)
