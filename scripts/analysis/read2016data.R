## 2016 data
# load 2016 data
load(paste0(userPath, "data/CIN/2016_survey/eqAreaData.RData")) # loads eqAreaData

dat_2016 <- eqAreaData # rename to dat_2016
remove(eqAreaData) # remove original object

dat_2016 <- dat_2016 %>% janitor::clean_names()

dat_2016 %>% select(lake_name, site_id, chla_sample, pheo_sample, tn, tnh4, tno2)

# site depth wasn't measured in first few lakes (oops) but we have bathymetry data
# for those lakes.

# Rename variables to be consistent with SuRGE
dat_2016_ <- dat_2016 %>%
  rename(
    
    # emission rates
    ch4_diffusion_best = ch4_drate_mg_h_best,
    ch4_ebullition = ch4_erate_mg_h,
    ch4_total = ch4_trate_mg_h,
    ch4_best_model = ch4_best_model,
    co2_diffusion_best = co2_drate_mg_h_best,
    co2_ebullition = co2_erate_mg_h,
    co2_total = co2_trate_mg_h,
    co2_best_model = co2_best_model,
    n2o_ebullition = n2o_erate_mg_h,
    
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
    deep_chla_sonde = "chla_d",
    shallow_chla_sonde = "chla_s",
    deep_do_mg = "do_l_d",
    shallow_do_mg = "do_l_s",
    deep_ph = "p_h_d",
    shallow_ph = "p_h_s",
    deep_sp_cond = "sp_cn_d",
    shallow_sp_cond = "sp_cn_s",
    deep_temp = "tmp_c_d",
    shallow_temp = "tmp_c_s",
    deep_turb = "tr_ntu_d",
    shallow_turb = "tr_ntu_s",
    
    # sample depths
    deep_sample_depth_m = "sm_dpth_d",
    shallow_sample_depth_m = "sm_dpth_s"
    
  ) %>%
  mutate(
    # identifiers
    visit = 1,
    site_id = as.numeric(gsub(".*?([0-9]+).*", "\\1", site_id)),
    
    # emission units
    ch4_diffusion_units = "mg_ch4_m2_h",
    ch4_ebullition_units = "mg_ch4_m2_h",
    ch4_total_units = "mg_ch4_m2_h",
    co2_diffusion_units = "mg_co2_m2_h",
    co2_ebullition_units = "mg_co2_m2_h",
    co2_total_units = "mg_co2_m2_h",
    n2o_ebullition_units = "mg_n2o_m2_h",
    
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
  
  # define index site. Chemistry sampled at deep and shallow sites, but only
  # deep site for SuRGE. set `index_site = TRUE for deep sampling site, but keep
  # deep and shallow chemistry in this file, it might come in handy for the 
  # point by point analysis. Can filter out shallow site when merging with SuRGE
  # at the lake scale
  left_join( # hard code 2016 index site locations.
    tribble(~lake_id, ~site_id, ~index_site,
                    1001, 4, TRUE,
                    1002, 4, TRUE, 
                    1003, 6, TRUE, 
                    1004, 3, TRUE, 
                    1005, 7, TRUE, 
                    1006, 3, TRUE, 
                    1007, 5, TRUE, 
                    1008, 12, TRUE, 
                    1009, 4, TRUE, 
                    1010, 7, TRUE, 
                    1011, 16, TRUE, 
                    1012, 4, TRUE, 
                    1013, 8, TRUE, 
                    1014, 2, TRUE, 
                    1015, 15, TRUE, 
                    1016, 2, TRUE, 
                    1017, 1, TRUE, 
                    1018, 2, TRUE, 
                    1019, 4, TRUE, 
                    1020, 1, TRUE, 
                    1021, 3, TRUE, 
                    1022, 9, TRUE, 
                    1023, 1, TRUE, 
                    1024, 34, TRUE, 
                    1025, 4, TRUE, 
                    1026, 10, TRUE, 
                    1027, 7, TRUE, 
                    1028, 7, TRUE, 
                    1029, 4, TRUE, 
                    1030, 7, TRUE, 
                    1031, 3, TRUE, 
                    1032, 16, TRUE))

# Read in lake-scale aggregated data
load(paste0(userPath, "data/CIN/2016_survey/meanVariance.c.lake.lu.agg.Rdata"))

dat_2016_agg <-  meanVariance.c.lake.lu.agg # rename to dat_2016_agg
remove(meanVariance.c.lake.lu.agg) # remove original object
  
# dat_2016
# morpho
"area_km2""perim_km" "wtr_dpth"



