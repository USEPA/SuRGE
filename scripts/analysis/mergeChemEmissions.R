# chem_fld (see mergeChemistryFieldSheets.R) is in long format.  Cast to
# wide to facilitate merge with emission rates.

# first pivot to long, then back to wide, then join with emissions
chem_fld_wide <- chem_fld %>%
  mutate(across(!c(lake_id, site_id, visit, eval_status, lat, long, sample_date, 
                   site_depth, sample_depth ), 
                as.character)) %>% # enforce consistent type
  pivot_longer(!c(lake_id, site_id, eval_status, lat, long, sample_date, 
                  site_depth, sample_depth, visit)) %>%
  pivot_wider(names_from = c(sample_depth,name), values_from = value) %>% # cast to wide
  # units are repeated for deep and shallow, not necessary.  Remove one of them
  # and strip depth reference from the other
  select(-(contains("units") & contains("shallow"))) %>% # strip shallow units
  rename_with(~sub("deep_", "", .), # strip "deep_
              .cols = (contains("units") & contains("deep"))) 




# Now merge with emissions
all_obs <- full_join(chem_fld_wide, # keep all observations
                     # omit diffusion model fit statistics
                     emissions %>% 
                       select(lake_id, visit, site_id, 
                              matches("diffusion|ebullition|total"))) %>%
  # arrange merged data frame
  select(lake_id, site_id, eval_status, visit, sample_date, lat, long, site_depth, # these first
         matches("diffusion|ebullition|total"), # then these
         everything()) # then everything else, unchanged

dim(chem_fld_wide) # 2057, 256
dim(emissions) # 1867, 50
dim(all_obs) # 2058, 270

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
