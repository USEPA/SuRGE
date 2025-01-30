## 2016 data
# load 2016 data-----------
load(paste0(userPath, "data/CIN/2016_survey/eqAreaData.RData")) # loads eqAreaData

dat_2016 <- eqAreaData # rename to dat_2016
remove(eqAreaData) # remove original object


# Format-------------
dat_2016 <- dat_2016 %>% 
  janitor::clean_names() %>% 
  # remove extra Acton Lake observations
  filter(!(lake_name %in% c("Acton Lake Aug", "Acton Lake July", "Acton Lake Oct")),
         eval_status == "sampled") %>% # only sampled sites
  # grab required variables
  select(lake_name, site_id, 
         chla_sample, tn, tnh4, tno2, tno2_3, toc, tp, trp, 
         ch4_drate_mg_h_best, ch4_erate_mg_h, ch4_trate_mg_h, #ch4_best_model,
         co2_drate_mg_h_best, co2_erate_mg_h, co2_trate_mg_h, #co2_best_model,
         n2o_erate_mg_h, 
         eb_ml_hr_m2, # volumetric_ebullition and volumetric_ebullition_units
         lat_samp, long_smp, # sample site coordinates lat and long
         xcoord, ycoord, # survey design site coordinates
         adj_wgt, # survey design weights
         stratum, # survey design stratum
         eval_status, # evaluation status of survey design sites
         trap_deply_dt_tm, # trap_deply_date_time
         trap_rtrv_dt_tm, # trap_rtrvl_date_time
         chm_deply_dt_tm,
         chla_d, chla_s,
         do_l_d, do_l_s,
         p_h_d, p_h_s,
         sp_cn_d, sp_cn_s,
         tmp_c_d, tmp_c_s,
         tr_ntu_d, tr_ntu_s,
         sm_dpth_d, sm_dpth_s,
         wtr_dpth) # site_depth


# Rename variables to be consistent with SuRGE
dat_2016 <- dat_2016 %>%
  rename(
    
    # emission rates
    ch4_diffusion_best = ch4_drate_mg_h_best,
    ch4_ebullition = ch4_erate_mg_h,
    ch4_total = ch4_trate_mg_h,
    #ch4_best_model = ch4_best_model,
    co2_diffusion_best = co2_drate_mg_h_best,
    co2_ebullition = co2_erate_mg_h,
    co2_total = co2_trate_mg_h,
    #co2_best_model = co2_best_model,
    n2o_ebullition = n2o_erate_mg_h,
    volumetric_ebullition  = eb_ml_hr_m2,
    
    # dates
    trap_deply_date_time = trap_deply_dt_tm, 
    trap_rtrvl_date_time = trap_rtrv_dt_tm, 
    chamb_deply_date_time = chm_deply_dt_tm,
    
    # coordinates
    lat = lat_samp,
    long = long_smp,
    
    # design parameters
    site_wgt = adj_wgt, # this is adjusted weight, simplifying name here
    site_stratum = stratum,
    site_eval_status = eval_status,
    
    # chemistry
    shallow_chla_lab = chla_sample,
    shallow_tn = tn,
    shallow_nh4 = tnh4,
    shallow_no2 = tno2,
    shallow_no2_3 = tno2_3,
    shallow_toc = toc,
    shallow_tp = tp,
    shallow_op = trp,
    
    # sonde
    deep_chla_sonde = chla_d,
    shallow_chla_sonde = chla_s,
    deep_do_mg = do_l_d,
    shallow_do_mg = do_l_s,
    deep_ph = p_h_d,
    shallow_ph = p_h_s,
    deep_sp_cond = sp_cn_d,
    shallow_sp_cond = sp_cn_s,
    deep_temp = tmp_c_d,
    shallow_temp = tmp_c_s,
    deep_turb = tr_ntu_d,
    shallow_turb = tr_ntu_s,
    
    # sample depths
    deep_sample_depth_m = sm_dpth_d,
    shallow_sample_depth_m = sm_dpth_s,
    site_depth = wtr_dpth
    
  ) %>%
  mutate(
    # identifiers
    visit = 1,
    site_id = as.numeric(gsub(".*?([0-9]+).*", "\\1", site_id)),
    sample_date = as.Date(trap_deply_date_time), # grab earliest date
    
    # coordinates
    long = long * -1, # longitude should be negative
    
    # emission units
    ch4_diffusion_units = "mg_ch4_m2_h",
    ch4_ebullition_units = "mg_ch4_m2_h",
    ch4_total_units = "mg_ch4_m2_h",
    co2_diffusion_units = "mg_co2_m2_h",
    co2_ebullition_units = "mg_co2_m2_h",
    co2_total_units = "mg_co2_m2_h",
    n2o_ebullition_units = "mg_n2o_m2_h",
    volumetric_ebullition_units = "ml_m2_h",
    
    # chemistry units
    chla_lab_units = "ug_l",
    tn_units = "ug_n_l",
    nh4_units = "ug_n_l",
    no2_units = "ug_n_l",
    no2_3_units = "ug_n_l",
    toc_units = "mg_c_l",
    tp_units = "ug_p_l",
    op_units = "ug_p_l") %>%
  
  # add lake_id
  left_join(lake.list.2016 %>% select(lake_id, eval_status_code_comment), by = c("lake_name" = "eval_status_code_comment")) %>%
  
  # restrict to fields present in SuRGE data
  select(-lake_name)



# Data fixes-----------------
# We have a few sites where emissions were measured but lat/long were not.
# Lets assume the measurements were made at the locations specified by the
# survey design coordinates. These coordinates are in "xcoord" and "ycoord"
# in Conus Albers. Below we 1) get the lat and long of these xcoord and ycoord
# values, then 2) replace the missing lat long values.
missing_coord <-  dat_2016 %>% 
  # observations with no lat long of sample site, but trap was deployed (4 observations)
  filter((is.na(lat)|is.na(long))&!is.na(trap_deply_date_time)) %>%
  st_as_sf(coords = c("xcoord", "ycoord")) %>% # convert to sf based on survey design
  `st_crs<-`("ESRI:102008") %>% # original 2016 data in Conus Albers. (5070)
  st_transform(crs="EPSG:4326") %>% # lat/long
  mutate(long = st_coordinates(.)[,1], # extract long from sf coordinates (column 1)
         lat = st_coordinates(.)[,2]) %>% # extract lat from sf coordinates (column 2)
  st_drop_geometry %>% # convert to df
  select(lake_id, site_id, visit, lat, long)

# Super cool dplyr function for updating values, rather than joining, then
# dealing with lat.x and lat.y, etc.
dat_2016 <- rows_update(dat_2016, # object to be updated
                        missing_coord, # new values taken from here
                        # join on these. unspecified variables (lat long) will be updated in x
                        by = c("lake_id", "site_id", "visit")) 


# recorded lat and long for some site were clearly erroneous.
# replace with survey design value
new_coords <- dat_2016 %>%
  filter(lake_id == 1001 & site_id == 11 | # Acton
           lake_id == 1006 & site_id %in% c(29, 33) | 
           lake_id == 1009 & site_id %in% c(6, 29, 30, 33, 34) | 
           lake_id == 1010 & site_id == 48 | 
           lake_id == 1013 & site_id == 30 | 
           lake_id == 1015 & site_id %in% c(7, 16) | 
           lake_id == 1019 & site_id %in% c(31, 32) | 
           lake_id == 1021 & site_id %in% c(2, 3, 6) | 
           lake_id == 1023 & site_id %in% c(25, 28)) %>%
  st_as_sf(coords = c("xcoord", "ycoord")) %>% # convert to sf based on survey design
  `st_crs<-`("ESRI:102008") %>% # original 2016 data in Conus Albers. (5070)
  st_transform(crs="EPSG:4326") %>% # lat/long
  mutate(long = st_coordinates(.)[,1], # extract long from sf coordinates (column 1)
         lat = st_coordinates(.)[,2]) %>% # extract lat from sf coordinates (column 2)
  st_drop_geometry %>% # convert to df
  select(lake_id, site_id, visit, lat, long)

# Super cool dplyr function for updating values, rather than joining, then
# dealing with lat.x and lat.y, etc.
dat_2016 <- rows_update(dat_2016, # object to be updated
                        new_coords, # new values taken from here
                        # join on these. unspecified variables (lat long) will be updated in x
                        by = c("lake_id", "site_id", "visit")) %>%
  select(-xcoord, -ycoord) # no longer need these columns


# One site where longitude was entered as -93 (invalid value) rather than
# -83. Need to fix
dat_2016 <- dat_2016 %>%
  mutate(long = replace(long, 
                        lake_id == 1016 & site_id == 10 & visit == 1, 
                        -83.97523)) # just this one instance
  

# site depth wasn't measured in first few lakes (oops) but we have bathymetry data
# for those lakes. need to add depth estimates for 2016 lakes missing this measurement 
# (1001, 1002, 1012, 1013, 1016, 1017). See estimateDepth2016.R



# Read in lake-scale aggregated data---------
# not certain what we want from here yet, so hold off for now.
# load(paste0(userPath, "data/CIN/2016_survey/meanVariance.c.lake.lu.agg.Rdata"))
# 
# dat_2016_agg <-  meanVariance.c.lake.lu.agg # rename to dat_2016_agg
# remove(meanVariance.c.lake.lu.agg) # remove original object
  




