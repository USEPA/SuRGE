## Create Lake-link Crosswalk, Compile LAGOS Data for Surge Sites 
## Last edited 6/28/2024

# Installed via renv
# devtools::install_github("cont-limno/LAGOSUS", dependencies = TRUE)

# loaded from masterlibrary.R
# library(LAGOSUS)
# library(httr) #for reading trophic status lagos data, only necessary for first run


#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
#-#- READ AND FORMAT LAGOS LOCUS-LAKE_LINK AND LOCUS-LAKE_CHARACTERISTICS

# LOCUS-LAKE_LINK
# [6/12/2024] working on Jake's machine!!  If acting up, skip to line 18
# lagosus_get(dest_folder = lagosus_path()) # run once, then hash out
locus <- lagosus_load(modules = c("locus"))
names(locus)
locus_link <- locus$locus$lake_link


# Read lake_link from SharePoint or web if the above isn't working.
# https://portal.edirepository.org/nis/mapbrowse?packageid=edi.854.1
# ll <- read.csv(paste0(userPath, "data/siteDescriptors/lake_link_lagos.csv")) # downloded locally
# ll <- read_csv("https://portal.edirepository.org/nis/dataviewer?packageid=edi.854.1&entityid=5488e333ce818597fa3dbfc9b4e0c131") # from web

# Now create single record for each lake
# There are multiple lagosus_legacysiteids and wqp_monitoringlocationidentifiers values per lake
locus_link_aggregated <- locus_link %>%
  group_by(nhdplusv2_comid) %>% # unique per lake
  filter(row_number() == 1) %>% # first record for each comid. alternatives: slice_head(1) and  top_n(n = 1) 
  select(lagoslakeid, nla2012_siteid, nla2007_siteid, lake_nhdid, lake_namegnis, 
         nhdhr_gnisid, nhdplusv2_comid) %>% # just keep what we need
  rename(nla07_site_id = nla2007_siteid, # per SuRGE convention
         nla12_site_id = nla2012_siteid)

# LOCUS-LAKE_CHARACTERISTICS
locus_characteristics <- locus$locus$lake_characteristics
# object contains many variables we will get from other sources (e.g. morphometry from Jeff,
# watershed from Alex).  Subset to those unique to lagos
locus_connectivity <- locus_characteristics %>% 
  select(lagoslakeid, lake_connectivity_class, lake_connectivity_fluctuates, lake_connectivity_permanent, 
         lake_lakes4ha_upstream_ha, lake_lakes4ha_upstream_n, lake_lakes1ha_upstream_ha, lake_lakes1ha_upstream_n, 
         lake_lakes10ha_upstream_n, lake_lakes10ha_upstream_ha)




#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
#-#- READ LAGOS TROPHIC STATUS

# lagos productivity estimates
# preprint link is: https://www.biorxiv.org/content/10.1101/2024.05.10.593626v1
# url_lagos <- "https://portal-s.edirepository.org/nis/dataviewer?packageid=edi.1427.3&entityid=3cb4f20440cbd7b8e828e4068d2ab734"

# 7 GB, takes about 30 minutes to download via httr.  Download once, save to disk, 
# then load from disk.  Much faster.
# httr::GET(url_lagos, progress(), write_disk(tf <- tempfile(fileext = ".csv"))) # about 30 minutes at AWBERC
# lagos_ts <- read.csv(tf) # another 15 minutes

# Alternatively, load .csv stored locally
# lagos_ts <- read_csv("C:/Users/JBEAULIE/OneDrive - Environmental Protection Agency (EPA)/GIS_data/LAGOS_US/LAGOS_US_LANDSAT_Predictions_v1_QAQC.csv")

# enforce naming conventions, define time
# this takes a few minutes
# lagos_ts <- lagos_ts %>%
#   janitor::clean_names() %>%
#   mutate(month = gsub( " .*$", "", sensing_time) %>% as.Date(., format = "%Y-%m-%d") %>% lubridate::month(.),
#          year = gsub( " .*$", "", sensing_time) %>% as.Date(., format = "%Y-%m-%d") %>% lubridate::year(.)) %>%
#   select(-sensing_time)

# save to SharePoint, read from SharePoint to save time.
# saveRDS(lagos_ts, paste0(userPath, "data/siteDescriptors/lagos_ts.rds"))
lagos_ts <- readRDS(paste0(userPath, "data/siteDescriptors/lagos_ts.rds"))




#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
#-#- READ SURGE LAKES
# Read in the SuRGE sites from the updated eval_status spreadsheet.
surge_sites <- read_xlsx(paste0(userPath, "surgeDsn/SuRGE_design_20191206_eval_status.xlsx"), na = "NA") %>%
  filter(`EvalStatus Code` == "S") %>% # only sampled
  janitor::clean_names() %>%
  dplyr::rename(lake_id = site_id) %>%
  mutate(lake_id = str_extract(lake_id, "(\\d+$)") %>% # extract numeric part of lake_id
           as.numeric()) %>% # convert lake_id to numeric
  rename(nla17_site_id = site_id_2) %>%
  mutate(nhdplusv2_comid = as.character(nhd_plus_waterbody_comid))%>%
  select(lake_id, sample_year, nhdplusv2_comid, gnis_name)

# We want to add sample month to enable more precise matching with LAGOS trophic status estimates.
# LAGOS only covers 1984 - 2020, so we can only match specific months for surge
# sites sampled in during that time frame.  Falls Lake (2014), R10 (2018, 2020), CIN (2020).
# Get sample months for those lakes, but not Falls Lake.  data entry not complete [6/25/2024]
surge_sample_month_year <- fld_sheet %>%
  mutate(sample_year = lubridate::year(trap_deply_date),
         sample_month = lubridate::month(trap_deply_date),
         # convert 69/70 lacustrine.. from the missouri river to just 69/70
         lake_id = case_when(grepl("69_", lake_id) ~ "69",
                             grepl("70_", lake_id) ~ "70",
                             TRUE ~ lake_id)) %>%
  filter(!is.na(sample_year)) %>% # omits 2016 survey data.  not yet included in fld_sheet [6/25/2024]
  distinct(lake_id, visit, sample_year, sample_month) %>% # 69 and 70 sampled in June and July, so 2 rows for each lake
  mutate(lake_id = as.numeric(lake_id)) %>%
  # sample_year not needed because it is contained in surge_sites. Keeping in both screws up 
  # join because 147 and 148 were sampled in 2021 and 2023, but surge_sites only 
  # contains 2021 record
  select(-sample_year) 

# merge sample months with surge_sites
surge_sites <- left_join(surge_sites, surge_sample_month_year, by = "lake_id", 
                         relationship = "many-to-many") # visit 1 and 2 in surge_sample_month_year matches to one record in surge_sites 

dim(surge_sites) # 153. only 147 lakes, but revisits for 4 plus 2 records for 69 and 70 which were sampled over two months. Need to be careful with these below.



#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
#-#- MERGE SURGE SITES AND LOCUS-LAKE_LINK$LAGOSLAKEID
# Need to add lagoslakeid to surge_sites for use in trophic status processing.
# Merge on nhdplusv2_comid
# Add other locus-lake_link and locus-lake_characteristic data after trophic
# status has been merged
surge_sites_lagoslakeid <- left_join(surge_sites %>% 
                                       select(lake_id, visit, sample_month, sample_year, nhdplusv2_comid), # only keep minimum needed variables
                                     locus_link_aggregated %>% select(lagoslakeid, nhdplusv2_comid), # only keep merge variable and lagoslakeid 
                                     by = "nhdplusv2_comid") %>%
  #  add the Lagos link for 10, 1009, and 1010 since lagos lacked the comid ID to link to SuRGE
  mutate(lagoslakeid = case_when(lake_id == 10 ~ 201797,
                                 lake_id == 1009 ~ 260194,
                                 lake_id == 1010 ~ 260193,
                                 TRUE ~ lagoslakeid)) %>%
  select(-nhdplusv2_comid) # remove grouping variable, no longer needed
dim(surge_sites_lagoslakeid ) # 153
surge_sites_lagoslakeid %>% filter(is.na(lagoslakeid)) %>% {dim(.)} # only two lakes w/out LAGOS record!  One is PR.




#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
#-#- MERGE LAGOS TROPHIC STATUS
# lagos_ts is a big file, largely because it contains a time series of observations
# for 187,000 lakes. Lets see how many surge lakes have a lagos trophic status value.

# pull out observations for SuRGE lakes
lagos_ts_small <- lagos_ts %>%
  filter(lagoslakeid %in% surge_sites_lagoslakeid$lagoslakeid) %>%
  # the columns specified below have variable values across the time series and would need to be condensed
  # to a single value when predicted chl a is aggregated by month or season. It doesn't
  # make sense to compute a summary stat for these (e.g. mean, median), and unique()
  # would create multiple values per month/season. Best to omit these variables.
  select(-c(negative_reflectance_min, negative_reflectance_median, pixel_perc_of_max, duplicate_day, qaqc_recommend))

# lakes 1000 (Puerto Rico) and 14 don't have lagos ID. lake 166 has lagos ID, but no
# trophic status records
lagos_ts_small %>% distinct(lagoslakeid) # 144 SuRGE sites with trophic status records, 3 without
dim(lagos_ts_small) # 77,802 observations

# inspect the trophic status data
# minimum of 155 observations/lake, max of 1331/lake, median of 515/lake
lagos_ts_small %>% 
  group_by(lagoslakeid) %>%
  summarise(n_obs = sum(!is.na(chl_predicted))) %>%
  {summary(.$n_obs)}

# visualize data
ggplot(lagos_ts_small, aes(as.factor(lagoslakeid), chl_predicted)) +
  geom_point()

# Merge and aggregate trophic status time series

# 1. merge trophic status with lake IDs
lagos_ts_agg <- left_join(surge_sites_lagoslakeid, lagos_ts_small, na_matches = "never", 
                         relationship = "many-to-many") %>% # lake w/multiple visits, or sampling that spanned two months (69/70) have multiple records in surge_locus_links_connectivity
  # 81,806, including two surge lakes without lagoslakeid match to lagos_ts. These
  # single observations are appended to end of record of matched observations.
  #dim %>%
  # create variable to uniquely identify each 'lake x sample_month x visit'. 
  mutate(split_variable = paste(lake_id, sample_month, visit)) %>%
  split(.$split_variable) %>% # split into named list elements: format = (lake_id sample_month visit)
  
  # 2. create logicals to define aggregation periods. See github issue 106:
  # "For predicting CO2, I like the idea of pulling chlorophyll data from just the month-of-year that matches 
  # the sampling date from a few previous years (e.g. 2018-2020)... we can see how well they match our measured 
  # chlorophyll. For predicting CH4, maybe it makes more sense to get a full June-September average across many 
  # years (as a measure of overall productivity that gets integrated into the sediment?"
  # for troubleshooting: '69 7 1' sampled in 2021  '239 7 1' sampled in 2018
  map_df(~mutate(., 
                 filter_month = case_when(month == sample_month ~ TRUE, # TRUE if remote sensing data collected same month as SuRGE sampling
                                          lagoslakeid %in% c(6445, 6573) & month == 8 ~ TRUE, # sampled in month 9, but no data available for that month
                                          lagoslakeid == 2039 & month == 7 ~ TRUE, # sampled in month 6, but no data available for that month
                                          TRUE ~ FALSE), # else FALSE
                 filter_season = case_when(month %in% 6:9 ~ TRUE, # SuRGE season (June - September)
                                           TRUE ~ FALSE), # else FALSE
                 filter_year = case_when(sample_year == year ~ TRUE, # TRUE if remote sensing data collected same year as SuRGE sampling, else 
                                         # if surge sampling conducted outside remote sensing time series, use 3 most recent years
                                         sample_year >= 2021 & year %in% (2018:2020) ~ TRUE, 
                                         TRUE ~ FALSE)) %>% # FALSE for all other conditions
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
  ) %>%
  # 5. aggregate values for lake sampled across multiple months (69 and 70)
  group_by(lake_id, visit, lagoslakeid) %>% # including lagoslakeid so the value is preserved in aggregation below
  summarize(across(everything(), mean)) %>%
  ungroup

dim(lagos_ts_agg) # 151 observations    
lagos_ts_agg %>% distinct(lake_id, visit) # 151 lake_id X visit records
lagos_ts_agg %>% distinct(lake_id) # 147 unique lakes, good



#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
#-#- MERGE TROPHIC STATUS AND SURGE_SITES
lagos_ts_agg_surge_sites <- full_join(lagos_ts_agg,
                                      # lakes 69 and 70 have two records each in surge_site, with each
                                      # record representing one of the two months over which the sampling
                                      # was distributed. Remove sample_month (which uniquely defines these
                                      # records) and sample_year (no longer needed), then filter down to 
                                      # unique records. This get us back down to 151 records (147 lakes
                                      # pluss 4 revisits)
                                      surge_sites %>% select(-sample_year, -sample_month) %>% distinct(),  
                                     by = c("lake_id", "visit"))
dim(lagos_ts_agg) # 151 observations
dim(surge_sites) # 153 observations (contains extra 69 and 70 records, see full_join comment)
dim(lagos_ts_agg_surge_sites) # 151 observations, good



#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
#-#- ADD LAGOS-LOCUS TO MERGE
lagos_ts_agg_surge_sites_link <- left_join(lagos_ts_agg_surge_sites,
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

dim(lagos_ts_agg_surge_sites_link ) # 151, good
lagos_ts_agg_surge_sites_link %>% filter(is.na(lagoslakeid)) %>% {dim(.)} # only two lakes w/out LAGOS record!  One is PR.


#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
#-#- ADD LAGOS-LAKE CONNECTIVITY TO MERGE
lagos_ts_agg_surge_sites_link_connectivity <- left_join(lagos_ts_agg_surge_sites_link,
                                                        locus_connectivity)

dim(lagos_ts_agg_surge_sites_link_connectivity) # 151, good
lagos_ts_agg_surge_sites_link_connectivity %>% 
  filter(is.na(lake_connectivity_class)) %>% {dim(.)} # 3 missing connectivity, the 2 above + New Melones
lagos_ts_agg_surge_sites_link_connectivity %>% filter(is.na(lake_connectivity_class))




#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
#-#- RENAME FINAL MERGED OBJECT
lagos_links <- lagos_ts_agg_surge_sites_link_connectivity
rm(lagos_ts_agg_surge_sites_link_connectivity)
