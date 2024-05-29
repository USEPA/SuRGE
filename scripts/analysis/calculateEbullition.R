
# CALCULATE EBULLITION RATE------------------

# 1. merge relevant columns from fld_sheets and dg_sheets with gc_lakeid_agg
# note that as of 4/2/2024 only trap runs have been read into gc_lakeid_agg,
# but a few dissolved gas samples slipped in
eb_data <- full_join( # keep all data.  Will assume GC data if not present (e.g. insufficient volume for sample)
  gc_lakeid_agg, # GC data
  # now select from fld_sheets
  fld_sheet %>% 
    select(lake_id, site_id, visit,
                       contains("trap")) %>%
    select(-contains("extn")) %>%
    filter(!is.na(trap_volume))) %>% # exclude where no data are reported for trap volume (e.g. tube fell off)
  # now join with dg_sheet to get pressure and temp for ebullition calcs
  left_join(., # keep all data from above, discard dg_sheet data that don't match up 
            dg_sheet %>% select(lake_id, site_id, visit,
                                atm_pressure, air_temperature) %>%
              distinct(.) %>%
              #lake_id == 275, site_id == 12, visit == 1 has two values for BP and air_temp
              # because sampling was conducted across two days.  Must aggregate across these
              # or unintend duplicates turn up in data frame.
              summarise(across(c(atm_pressure, air_temperature), mean), .by= c(lake_id, site_id, visit))
  )

dim(eb_data) # 1798

# 2. Calculate volumetric ebullition rate.  Straightforward operation
# that can be vectorized across the entire df.
eb_data <- mutate(eb_data, 
                     eb_ml_hr_m2 = trap_volume / 
                       (as.numeric(trap_rtrvl_date_time - trap_deply_date_time, units = "hours") * # time difference in hours
                          ((3.14*.28^2)))) # diameter = 22.25in=0.56m, r=.28m))

# Mass flux rate must be calculated by Lake.  Tried to apply by group using
# by_group, ddply, and lapply.  I couldn't figure it out, resorted to for loop

my_eb_list <- list()
for (i in 1:length(unique(eb_data$lake_id))) {
  case.i <- eb_data %>% distinct(lake_id, visit) %>% slice(i)
  data.i <- filter(eb_data, lake_id == case.i$lake_id, visit == case.i$visit)
  out.ch4 <- mass.rate(data.i, choice1 = "ch4") 
  out.co2 <- mass.rate(data.i, choice1 = "co2")
  out.n2o <- mass.rate(data.i, choice1 = "n2o")
  
  my_eb_list[[i]] <- data.frame(eb_ch4_mg_m2_h = out.ch4,
                                eb_co2_mg_m2_h = out.co2,
                                eb_n2o_mg_m2_h = out.n2o,
                                lake_id = data.i$lake_id,
                                site_id = data.i$site_id,
                                visit = data.i$visit)
}

eb_results <- do.call("rbind", my_eb_list) %>%  # This coerces the list into a dataframe. Cool.
  left_join(eb_data %>% select(lake_id, site_id, visit, eb_ml_hr_m2)) %>%
  rename(ch4_erate_mg_h = eb_ch4_mg_m2_h,
         co2_erate_mg_h = eb_co2_mg_m2_h,
         n2o_erate_mg_h = eb_n2o_mg_m2_h)
  



str(eb_results)  # 1738 observations

