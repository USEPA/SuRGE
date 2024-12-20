# MERGE PREDICTOR VARIABLES AND PREPARE FOR ANALYSIS

# dat WILL CONTAIN DATA FROM ALL SITES.
# dat_agg WILL HAVE DATA AGGREGATED BY LAKE_ID



# 1. Merge 2016 and SuRGE data----------
names(dat_2016)[!(names(dat_2016) %in% names(all_obs))] # remove "chamb_deply_date_time" from dat_2016
names(all_obs)[!(names(all_obs) %in% names(dat_2016))] 

dat <- bind_rows(dat_2016 %>% 
                   mutate(lake_id = as.character(lake_id)) %>% # converted to numeric below
                   select(-chamb_deply_date_time),
                 all_obs)
dim(dat_2016) # 1426
dim(all_obs) # 2057
dim(dat) # 3483, good

# 2. Fill chemistry variables----
# chemistry variables measured at one location.  NA reported
# for all other sites in lake.  Here we fill all NAs using
# the one measured value.

# vector of analytes to fill
# expand.grid to create all combinations of two vectors
analytes_to_fill <-  expand.grid(c("deep_", "shallow_"), 
                              c("nh4", "no2_3", "no2", "tn", "tp", "op", # nutrients
                                "f", "cl", "br", "so4", # anions
                                "doc", "toc", # organics
                                "al", "as", "ba", "be", "ca", # metals  
                                "cd", "cr", "cu", "fe",
                                "k", "li",  "mg", "mn", "na",
                                "ni", "pb", "p", "sb", "si",
                                "sn", "sr", "s", "v", "zn"),
                              stringsAsFactors = FALSE) %>% 
  mutate(analytes_to_fill = paste0(Var1, Var2)) %>% # paste 
  pull(analytes_to_fill) %>% # extract to vector
  c(., "shallow_phycocyanin_lab", "shallow_chla_lab") # add lab algae (only shallow)

# Fill
dat <- dat %>%
  group_by(lake_id) %>% # for each lake
  # 2016 data have shallow chemistry at two sites but no deep chemistry. SuRGE 
  # has deep and shallow at one site. For SuRGE lakes, 'fill` will populate all
  # NAs with the one observed value. For 2016 sites, values are filled by site_id,
  # so site_id values close the shallow site get the shallow site number and
  # id's close to the deep site number get that number.
  fill(all_of(analytes_to_fill), .direction = "downup") %>%
  ungroup 

dim(dat) # 3483

# 3. Merge SuRGE lake list----
dat <- dat %>%
  left_join(
    # expand lake.list to include lacustrine, riverine, and transitional
    lake.list %>% # Surge sites
      select(-eval_status) %>% # this is for lake-scale. same variable in dat is for each point.
      filter(lake_id %in% 69:70) %>% # pull out 69 and 70
      group_by(lake_id) %>%
      slice(rep(1,3)) %>% # selects the first row (by group), 3 times, giving the repeated rows desired;
      ungroup() %>%
      # rename the duplicated records
      mutate(lake_id = c("69_lacustrine", "69_transitional", "69_riverine",
                         "70_lacustrine", "70_transitional", "70_riverine"),
             # Associating NLA17 ID with appropriate river section based on 
             # lat long of NLA17 Index site. This is necessary to match NLA17
             # chem values with appropriate river section.
             nla17_site_id = case_when(lake_id == "70_transitional" ~ "NLA17_SD-10053", # NLA 43.39241 -99.13541
                                       lake_id == "69_lacustrine" ~ "NLA17_SD-10001", # NLA 45.01971 -100.26474 
                                       TRUE ~ NA_character_)) %>%
      # merge new records with original df and 2016 sites
      # eval_status from lake lists pertains to the entire reservoir. eval_status
      # in dat (derived from all_obs) pertains to each site. To prevent the left_join
      # from joining on these column, I'll omit eval_status from lake lists.
      rbind(., # oahe and FC from above
            lake.list %>% select(-eval_status), # omit lake-scale eval_status
            lake.list.2016 %>% select(-eval_status)) %>% # omit lake-scale eval_status 
      # remove records for 69 and 70
      filter(!lake_id == c(69|70)))

dim(dat) # 3483


# 3. Merge NLA chemistry----
 # common names
 names(nla17_chem)[names(nla17_chem) %in% names(dat)] # nla17_site_id
 
 # 1000 (Puerto Rico), 69_riverine, 69_transitional, 70_lacustrine, 
 # 70_riverine, and most 2016 sites not in nla17_chem as expected. 
# 69_lacustrine and 70_transitional are included. See above.
 dat$lake_id[!(dat$nla17_site_id %in% nla17_chem$nla17_site_id)] %>% unique
 
 # merge
 dat <- dat %>%
   left_join(nla17_chem)
 dim(dat) # 3483
 
 # impute missing chem with NLA numbers
 dat <- dat %>%
   # NLA  method measures ammonia and ammonium;the relative proportion between these
   # two analytes depends on pH. Typically, NLA (and other NARS) samples consist of mostly ammonium.
   mutate(shallow_nh4 = case_when(is.na(shallow_nh4) ~ nla17_ammonia_n * 1000, #mg N/L in NLA
                                  TRUE ~ shallow_nh4),
          shallow_chla_lab = case_when(is.na(shallow_chla_lab) ~ nla17_chla, #ug/L in NLA
                                       TRUE ~ shallow_chla_lab),
          shallow_doc = case_when(is.na(shallow_doc) ~ nla17_doc, #mg/L in NLA
                                  TRUE ~ shallow_doc),
          # NLA has very few no2_3 data.  Have to sum no2 and no3.
          shallow_no2_3 = case_when(is.na(shallow_no2_3) ~ (nla17_nitrate_n + nla17_nitrite_n) * 1000, #mg/L in NLA
                                    TRUE ~ shallow_no2_3),
          shallow_tp = case_when(is.na(shallow_tp) ~ nla17_ptl, # assuming ptl = TP, ug/L in NLA
                                 TRUE ~ shallow_tp),
          shallow_tn = case_when(is.na(shallow_tn) ~ nla17_ntl * 1000, # assuming ntl = TN, mg/L in NLA
                                 TRUE ~ shallow_tn),
          shallow_so4 = case_when(is.na(shallow_so4) ~ nla17_sulfate, # mg/l in NLA
                                  TRUE ~ shallow_so4))


 # 4. Index site location----
 dat <- dat %>%
   left_join(index_site, 
             by = c("lake_id", "site_id", "visit")) # default, but specifying for clarity
 dim(dat) # 3483
 
 
# 5. Merge Waterisotope----
 dat <- dat %>%
   left_join(water_isotope_agg %>%
               # water isotope samples collected at NLA Index site. Here we 
               # associate the values with the correct river segment based
               # on index site lat long.
               mutate(lake_id = case_when(lake_id == "70" ~ "70_transitional", # NLA 43.39241 -99.13541
                                          lake_id == "69" ~ "69_lacustrine", # NLA 45.01971 -100.26474 
                                          TRUE ~ lake_id)))
 
 dim(dat) # 3483

# 6. Stratification Indices
 dat <- dat %>%
   left_join(strat_link, by=c("lake_id","visit"))
 
 dim(dat) #3483
 
# 7. Phytoplankton Composition from Avery----
 dat <- dat %>%
   left_join(phyto_SuRGE_link, by="lake_id")
 dim(dat) # 3483
 
# 8. Move lacustrine, transitional, and riverine to site_id.----
 # this will facilitate merging with variables the pertain to 
 # entire lake
 dat <- dat %>%
   mutate( # move transitional, lacustrine, riverine from lake_id to site_id
     site_id = case_when(grepl("lacustrine", lake_id) ~ paste0(site_id, "_lacustrine"),
                         grepl("transitional", lake_id) ~ paste0(site_id, "_transitional"),
                         grepl("riverine", lake_id) ~ paste0(site_id, "_riverine"),
                         TRUE ~ as.character(site_id)),
     # remove transitional, lacustrine, riverine from lake_id
     # retain character class initially, then convert to numeric.
     lake_id = case_when(lake_id %in% c("69_lacustrine", "69_riverine", "69_transitional") ~ "69",
                         lake_id %in% c("70_lacustrine", "70_riverine", "70_transitional") ~ "70",
                         TRUE ~ lake_id),
     lake_id = as.numeric(lake_id)) 
 
 dim(dat) # 3483
 
# 9. Merge lakeMorpho data (readMorpho.R)----
 dat <- dat %>%
   left_join(.,
             morpho)

 dim(morpho) # 145, 20
 dim(dat) # 3483

# 10. Merge hydroLakes ID----
 dat <- dat %>%
   left_join(hylak_link)
 
 dim(hylak_link) #127, 16
 dim(dat) # 3483
 
# 11. Merge LAGOS----
  dat <- dat %>%
   left_join(lagos_links)
 
 dim(lagos_links) #150, 16
 dim(dat) # 3483
 
# 12. Merge NID----
 dat <- dat %>%
   left_join(nid_link)
 
 dim(nid_link) #147, 16
 dim(dat) # 3483
 
# 13. Merge NHDPlusV2 - lakeCat----
dat <- dat %>%
   left_join(lake_cat_abbv,  by = c("nhd_plus_waterbody_comid" = "comid", "lake_id" = "lake_id"))
 
 dim(lake_cat) #147, 16
 dim(dat) # 3483

# 14. National Wetland Inventory----
dat <- dat %>%
  left_join(nwi_link)

 dim(nwi_link) #148, 16
 dim(dat) # 3483
 
# 15. Reservoir Sedimentaion----
dat <- dat %>%
  left_join(sedimentation_link, by = "lake_id")

 dim(sedimentation_link) # 139, 5
 dim(dat) # 3483
 
# 16. Water level change indices----
dat<- dat %>%
  left_join(walev_link, by = c("lake_id","visit"))
 
dim(walev_link) #21
dim(dat) # 3483

# 17. IPCC Climate Zone-----
dat <- dat %>%
  left_join(surge_climate, by = "lake_id")

dim(surge_climate) #149
dim(dat) #3483

# 18. ERA5 Water and Air Temperature----
dat <- dat %>%
  left_join(met_temp, by = "lake_id") # values are identical for multiple visits

dim(met_temp) #146
dim(dat) #3483


# 19. Aggregate by lake_id----------
# 10/10/2024 NOT WORKING, IN PROGRESS....
# This should be done using grts algorithms and survey design weights.
# Under a time crunch, so ignoring that step for now.


dat_agg <- dat %>%
    select(-site_id, -eval_status, -trap_rtrvl_date_time, - trap_deply_date_time, 
           -co2_total, -ch4_total) %>% # recalc totals after aggregating by lake
    fill(contains("units")) %>% # populates every row with a value.  This simplifies some of the aggregation below
    group_by(lake_id, visit) %>%
    summarise(across(where(is.numeric), \(x) mean(x, na.rm = TRUE)), # calculate arithmetic mean
              # NA in flags and units cause headaches.  If NA and non-NA values are present
              # for any analyte in a unique lake x visit combination, "unique" returns
              # multiple values (e.g. NA, mg/l) and the lake x visit combination is
              # collapsed into 2 rows rather than 1.  Here we use na.omit to exlcude
              # NA and return only the unit (e.g. mg/l).  This fails, however, when
              # no units are present for any analyte in a unique lake x visit combination.
              # under this condition, identified by length(na.omit(.)) == 0, we return
              # NA.
              across(contains("flags"), \(x) case_when(length(na.omit(x)) >= 1 ~ paste(unique(na.omit(x)), collapse = " "), # If >1 flag, then concatenate
                                                    length(na.omit(x)) == 0 ~ NA_character_, # only NA present, return NA
                                                          TRUE ~ "fly you fools!")), # look out for the Balrog!!
              # `fill` above ensures no NA for units
              across(contains("unit"), ~ unique(.) %>% pluck(1)), # If >1 unit present (e.g. cond, cond@25) display first one
              sample_date = unique(na.omit(sample_date)) %>% pluck(1), # if multiple, choose earliest
              lake_name = case_when(all(is.na(lake_name)) ~ NA_character_, # only NA present (Puerto Rico), return NA
                                    !all(is.na(lake_name)) ~ unique(lake_name), # if any values, then grab one, only one name per lake
                                    TRUE ~ "fly you fools!"), # look out for the Balrog 
              stratum = case_when(all(is.na(stratum)) ~ NA_character_, # only NA present (Puerto Rico, 2016 survey), return NA
                                  !all(is.na(stratum)) ~ unique(stratum), # if any values, then grab one, only one stratum per lake
                                    TRUE ~ "fly you fools!"), # look out for the Balrog
              lab = case_when(all(is.na(lab)) ~ NA_character_, # only NA present (Puerto Rico), return NA 
                              !all(is.na(lab)) ~ unique(lab), # if any values, then grab one, only one lab per lake, 
                              TRUE ~ "fly you fools!"), # look out for the Balrog,
              nla17_site_id = case_when(all(is.na(nla17_site_id)) ~ NA_character_, # only NA present (Puerto Rico), return NA 
                                 !all(is.na(nla17_site_id)) ~ unique(nla17_site_id), # if any values, then grab one, only one nla_id per lake, 
                                 TRUE ~ "fly you fools!"), # look out for the Balrog,
              nla07_site_id = case_when(all(is.na(nla07_site_id)) ~ NA_character_, # only NA present (Puerto Rico), return NA 
                                        !all(is.na(nla07_site_id)) ~ unique(nla07_site_id), # if any values, then grab one, only one nla_id per lake, 
                                        TRUE ~ "fly you fools!"), # look out for the Balrog,
              nla12_site_id = case_when(all(is.na(nla12_site_id)) ~ NA_character_, # only NA present (Puerto Rico), return NA 
                                        !all(is.na(nla12_site_id)) ~ unique(nla12_site_id), # if any values, then grab one, only one nla_id per lake, 
                                        TRUE ~ "fly you fools!"), # look out for the Balrog,
              nla_unique_id = case_when(all(is.na(nla_unique_id)) ~ NA_character_, # only NA present (Puerto Rico), return NA 
                                        !all(is.na(nla_unique_id)) ~ unique(nla_unique_id), # if any values, then grab one, only one nla_id per lake, 
                                        TRUE ~ "fly you fools!"), # look out for the Balrog,
              ag_eco9_nm = case_when(all(is.na(ag_eco9_nm)) ~ NA_character_, # only NA present (Puerto Rico), return NA
                                     !all(is.na(ag_eco9_nm)) ~ unique(ag_eco9_nm), # if any values, then grab one,  only one level per lake
                                     TRUE ~ "fly you fools!"), # look out for the Balrog,
              lake_nhdid = case_when(all(is.na(lake_nhdid)) ~ NA_character_, # only NA present (Puerto Rico), return NA
                                     !all(is.na(lake_nhdid)) == 0 ~ unique(lake_nhdid), # if any values, then grab one,  only one level per lake
                                     TRUE ~ "fly you fools!")) %>% # look out for the Balrog
  mutate(ch4_total = ch4_diffusion_best + ch4_ebullition,
         co2_total = co2_diffusion_best + co2_ebullition) %>%
    ungroup 



dim(all_obs) #2057
dim(dat) #3486
dim(dat_agg) # 122


# write lake ID's to disk
write_csv(x = dat_agg %>% 
            select(lake_id, contains("site_id"), hylak_id, lagoslakeid, grand_id,
                               lagoslakeid, nhd_plus_waterbody_comid),
          file = paste0(userPath, "data/siteDescriptors/surge_master_crosswalk_wide_beaulieu.csv"))

