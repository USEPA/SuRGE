# MERGE PREDICTOR VARIABLES AND PREPARE FOR ANALYSIS

# dat WILL CONTAIN DATA FROM ALL SITES.
# dat_agg WILL HAVE DATA AGGREGATED BY LAKE_ID



### ALL DATA--------------

# 1. Fill chemistry variables
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
dat <- all_obs %>%
  group_by(lake_id) %>% # for each lake
  fill(all_of(analytes_to_fill)) %>% # fill these analytes using only value present per group/analyte
  ungroup 


# 2. Merge lakeMorpho data (readMorpho.R)
dat <- dat %>%
  # # morpho currently missing data for Oahe and Francis Case
  # when ready, data will be for entire reservoir, not lacustrine, riverine,
  # transitional.  Need to figure out how to deal with this.  Ignoring for now.
  # mutate(lake_id_simple = case_when(grepl("69", lake_id) ~ "69",
  #                                   grepl("70", lake_id) ~ "70"))
  left_join(.,
            morpho %>% mutate(lake_id = as.character(lake_id)))

dim(all_obs) #2057, 264
dim(morpho) #156, 15
dim(dat) # 2057, 278

# 3. Merge lake list
 dat <- dat %>%
   left_join(lake.list %>% 
               filter(!is.na(sample_year)) %>%
               select(lake_id, visit, stratum, sample_year, lab, nla_id, ag_eco9_nm,
                                  nhd_plus_waterbody_comid) %>%
               mutate(lake_id = as.character(lake_id)))
 dim(dat) # 2057


# 4. Merge NLA chemistry 
 # common names
 names(nla17_chem)[names(nla17_chem) %in% names(dat)] # nla_id
 
 # 1000 (Puerto Rico) and lacustrine/riverine/transitional not in nla17_chem
 # as expected
 dat$lake_id[!(dat$nla_id %in% nla17_chem$nla_id)] %>% unique
 
 # merge
 dat <- dat %>%
   left_join(nla17_chem)
 dim(dat) # 2057
 
 # impute missing chem with NLA numbers
 dat <- dat %>%
   # NLA  method measures ammonia and ammonium;the relative proportion between these
   # two analytes depends on pH. Typically, NLA (and other NARS) samples consist of mostly ammonium.
   mutate(shallow_nh4 = case_when(is.na(shallow_nh4) ~ nla_ammonia_n * 1000, #mg N/L in NLA
                                  TRUE ~ shallow_nh4),
          shallow_chla_lab = case_when(is.na(shallow_chla_lab) ~ nla_chla, #ug/L in NLA
                                       TRUE ~ shallow_chla_lab),
          shallow_doc = case_when(is.na(shallow_doc) ~ nla_doc, #mg/L in NLA
                                  TRUE ~ shallow_doc),
          # NLA has very few no2_3 data.  Have to sum no2 and no3.
          shallow_no2_3 = case_when(is.na(shallow_no2_3) ~ (nla_nitrate_n + nla_nitrite_n) * 1000, #mg/L in NLA
                                    TRUE ~ shallow_no2_3),
          shallow_tp = case_when(is.na(shallow_tp) ~ nla_ptl, # assuming ptl = TP, ug/L in NLA
                                 TRUE ~ shallow_tp),
          shallow_tn = case_when(is.na(shallow_tn) ~ nla_ntl * 1000, # assuming ntl = TN, mg/L in NLA
                                 TRUE ~ shallow_tn),
          shallow_so4 = case_when(is.na(shallow_so4) ~ nla_sulfate, # mg/l in NLA
                                  TRUE ~ shallow_so4))
 
 # 5. Merge hydroLakes ID
 dat <- dat %>%
   left_join(hylak_link %>%
               mutate(lake_id = as.character(lake_id)))
 
 # 6. Merge LAGOS
 # not many useful predictors now, revisit after reading in more lagos data
 dat <- dat %>%
   left_join(lagos_links %>%
               select(lake_id, lagoslakeid, lake_nhdid))
 
# 7. Merge NID
 dat <- dat %>%
   left_join(nid_link %>%
               mutate(lake_id = as.character(lake_id)))
 
 # 8. Merge NHDPlusV2 - lakeCat 
dat <- dat %>%
  left_join(lakeCat,  by = c("nhd_plus_waterbody_comid" = "comid"))
 
 
 ### AGGREGATED BY LAKE_ID----------
# This should be done using grts algorithms and survey design weights.
# Under a time crunch, so ignoring that step for now.

# 1.  Aggregate

dat_agg <- dat %>%
    select(-site_id, -eval_status, -trap_rtrvl_date_time, - trap_deply_date_time) %>%
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
              lake_name = case_when(all(is.na(lake_name)) ~ NA_character_, # only NA present (Puerto Rico), return NAonly one nla_id per lake
                                    !all(is.na(lake_name)) ~ unique(lake_name), # if any values, then grab one, only one nla_id per lake
                                    TRUE ~ "fly you fools!"), # look out for the Balrog 
              stratum = case_when(all(is.na(stratum)) ~ NA_character_, # only NA present (Puerto Rico), return NA
                                  !all(is.na(stratum)) ~ unique(stratum), # if any values, then grab one, only one stratum per lake
                                    TRUE ~ "fly you fools!"), # look out for the Balrog
              lab = case_when(all(is.na(lab)) ~ NA_character_, # only NA present (Puerto Rico), return NA 
                              !all(is.na(lab)) ~ unique(lab), # if any values, then grab one, only one nla_id per lake, 
                              TRUE ~ "fly you fools!"), # look out for the Balrog,
              nla_id = case_when(all(is.na(nla_id)) ~ NA_character_, # only NA present (Puerto Rico), return NA 
                                 !all(is.na(nla_id)) ~ unique(nla_id), # if any values, then grab one, only one nla_id per lake, 
                                 TRUE ~ "fly you fools!"), # look out for the Balrog,
              ag_eco9_nm = case_when(all(is.na(ag_eco9_nm)) ~ NA_character_, # only NA present (Puerto Rico), return NA
                                     !all(is.na(ag_eco9_nm)) ~ unique(ag_eco9_nm), # if any values, then grab one,  only one level per lake
                                     TRUE ~ "fly you fools!"), # look out for the Balrog,
              lake_nhdid = case_when(all(is.na(lake_nhdid)) ~ NA_character_, # only NA present (Puerto Rico), return NA
                                     !all(is.na(lake_nhdid)) == 0 ~ unique(lake_nhdid), # if any values, then grab one,  only one level per lake
                                     TRUE ~ "fly you fools!")) %>% # look out for the Balrog
    ungroup 



dim(all_obs) #2057
dim(dat) #2057
dim(dat_agg) # 122




