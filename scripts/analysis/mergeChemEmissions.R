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
  select(-matches("deep_phycocyanin_lab|deep_chla_lab|deep_microcystin")) %>% # lab pigments only measured in shallow
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



# Now merge with emissions
all_obs <- full_join(chem_fld_wide, # keep all observations
                     # omit diffusion model fit statistics
                     emissions %>% 
                       select(lake_id, visit, site_id, air_temp, air_temp_units, co2flag, co2_deployment_length, co2_deployment_length_units,
                              ch4_deployment_length, ch4_deployment_length_units,
                              matches("diffusion|ebullition|total|volumetric"))) %>%
  # arrange merged data frame
  select(lake_id, site_id, eval_status, visit, sample_date, lat, long, site_depth, # these first
         matches("diffusion|ebullition|total|volumetric"), # then these
         everything()) # then everything else, unchanged

dim(chem_fld_wide) # 1869, 283
dim(emissions) # 1867, 56
dim(all_obs) # 1869, 304

# all observations from emissions are in chem
emissions[!(with(emissions, paste(lake_id, site_id, visit)) %in% 
            with(chem_fld_wide, paste(lake_id, site_id, visit))),]

# observations from chem_fld not in emissions
# lake_id == 148, site_id == 14, visit == 1
# lake_id == 71, site_id == 1, visit == 1

# tube fell off trap. No good diffusion data.
chem_fld_wide[!(with(chem_fld_wide, paste(lake_id, site_id, visit)) %in%
                  with(emissions, paste(lake_id, site_id, visit))),
              c("lake_id", "site_id", "visit", "eval_status", "deep_sample_depth_m")] %>%
  print(n=Inf)

# write to disk
#save(all_obs, file = paste0(userPath, "data/all_obs_", Sys.Date(), ".RData"))
#write.table(all_obs, file = paste0(userPath, "data/all_obs_",  Sys.Date(),".txt"), row.names = F, col.names = T)
