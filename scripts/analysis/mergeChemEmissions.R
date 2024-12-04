# chem_fld (see mergeChemistryFieldSheets.R) is in long format.  Cast to
# wide to facilitate merge with emission rates.

# first pivot to long, then back to wide, then join with emissions
chem_fld_wide <- chem_fld %>%
  mutate(across(!c(lake_id, site_id, visit, eval_status, lat, long, sample_date, 
                   site_depth, sample_depth), 
                   # trap_deply_date_time, trap_rtrvl_date_time, # don't distinguish between deep and shallow
                   # phycocyanin_lab, phycocyanin_lab_units, phycocyanin_lab_flags, # only shallow samples
                   # chla_lab, chla_lab_units, chla_lab_flags), # only shallow samples
                as.character)) %>% # enforce consistent type
  pivot_longer(!c(lake_id, site_id, eval_status, lat, long, sample_date, 
                  site_depth, sample_depth, visit)) %>% #
  pivot_wider(names_from = c(sample_depth, name), values_from = value) %>% # cast to wide
  # units are repeated for deep and shallow, not necessary.  Remove one of them
  # and strip depth reference from the other
  select(-(contains("units") & contains("shallow"))) %>% # strip shallow units
  rename_with(~sub("deep_", "", .), # strip "deep_
              .cols = (contains("units") & contains("deep"))) %>%
  select(-matches("deep_phycocyanin_lab|deep_chla_lab")) %>% # lab pigments only measured in shallow
  # trap deply and rtrvl times duplicated for deep and shallow.  delete shallow, strip "deep" from deep
  # could have renamed shallow, deleted deep, doesn't matter
  select(-matches("shallow_trap")) %>% # omit shallow_trap_ rtrvl and deply times
  rename_with(~sub("deep_", "", .), # strip "deep" from trap rtrvl and deply times
              .cols = (contains("deep_trap"))) %>%
  # convert values back to appropriate class
  mutate(across(!matches(paste(c("lake_id", "site_id", "visit", "eval_status", "lat", "long", "sample_date", 
                   "site_depth",  "units", "flag", "date_time"), collapse = "|")),
                as.numeric), # chemistry values back to numeric
         across(contains("trap_"),
                as.POSIXct)) # deployment and retrieval times back to posixct

#Replace deep sonde NA values with shallow measurements for sites less than 1 meter deep
chem_fld_wide$deep_do_mg <- ifelse(
  chem_fld_wide$site_depth < 1 &
    is.na(chem_fld_wide$deep_do_mg),
  chem_fld_wide$shallow_do_mg,
  chem_fld_wide$deep_do_mg
)
chem_fld_wide$deep_temp <- ifelse(
  chem_fld_wide$site_depth < 1 &
    is.na(chem_fld_wide$deep_temp),
  chem_fld_wide$shallow_temp,
  chem_fld_wide$deep_temp
)
chem_fld_wide$deep_sp_cond <- ifelse(
  chem_fld_wide$site_depth < 1 &
    is.na(chem_fld_wide$deep_sp_cond),
  chem_fld_wide$shallow_sp_cond,
  chem_fld_wide$deep_sp_cond
)
chem_fld_wide$deep_chla_sonde <- ifelse(
  chem_fld_wide$site_depth < 1 &
    is.na(chem_fld_wide$deep_chla_sonde),
  chem_fld_wide$shallow_chla_sonde,
  chem_fld_wide$deep_chla_sonde
)
chem_fld_wide$deep_turb <- ifelse(
  chem_fld_wide$site_depth < 1 &
    is.na(chem_fld_wide$deep_turb),
  chem_fld_wide$shallow_turb,
  chem_fld_wide$deep_turb
)

# Now merge with emissions
all_obs <- full_join(chem_fld_wide, # keep all observations
                     # omit diffusion model fit statistics
                     emissions %>% 
                       select(lake_id, visit, site_id, co2note, 
                              matches("diffusion|ebullition|total|volumetric"))) %>%
  # arrange merged data frame
  select(lake_id, site_id, eval_status, visit, sample_date, lat, long, site_depth, # these first
         matches("diffusion|ebullition|total|volumetric"), # then these
         everything()) # then everything else, unchanged

dim(chem_fld_wide) # 2057, 250
dim(emissions) # 1866, 54
dim(all_obs) # 2057, 269

# all observations from emissions are in chem
emissions[!(with(emissions, paste(lake_id, site_id, visit)) %in% 
            with(chem_fld_wide, paste(lake_id, site_id, visit))),]

# 191 observations from chem_fld are not in emissions
# these are mostly sites that weren't sampled but kept in database
# for potential weight adjustments.  Also includes 69_lacustrine sites
# with chemistry (sonde) but no emissions?  Not sure how this occurred but accurately
# reflects field sheets.
chem_fld_wide[!(with(chem_fld_wide, paste(lake_id, site_id, visit)) %in%
                  with(emissions, paste(lake_id, site_id, visit))),
              c("lake_id", "site_id", "visit", "eval_status", "deep_sample_depth_m")] %>%
  print(n=Inf)

# write to disk
save(all_obs, file = paste0("output/all_obs_", Sys.Date(), ".RData"))
#write.table(all_obs, file = paste0(userPath, "data/all_obs_",  Sys.Date(),".txt"), row.names = F, col.names = T)
