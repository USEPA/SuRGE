# MERGE PREDICTOR VARIABLES AND PREPARE FOR ANALYSIS

# dat WILL CONTAIN DATA FROM ALL SITES.
# dat_agg WILL HAVE DATA AGGREGATED BY LAKE_ID



# 1. Merge 2016 and SuRGE data----------
# Names in dat_2016, but not in all_obs
names(dat_2016)[!(names(dat_2016) %in% names(all_obs))] # chamb_deply_date_time, site_wgt, site_stratum, site_eval_status
# names in all_obs, but not in dat_2016
names(all_obs)[!(names(all_obs) %in% names(dat_2016))] # lots, eval_status is for points= 

dat <- bind_rows(dat_2016 %>% 
                   mutate(lake_id = as.character(lake_id)) %>% # converted to numeric below
                   select(-chamb_deply_date_time, -site_wgt, -site_stratum), # wgt and stratum get added later
                 all_obs %>%
                   # eval_status is inherited from fld_sheet and reflects
                   # site_id evaluations made during sampling. Changing name here,
                   # rather than earlier, to avoid unanticipated consequences.
                   # eval_status is referenced in numerous existing lines of code
                   rename(site_eval_status = eval_status)) # consistent with dat_2016

dim(dat_2016) # 498
dim(all_obs) # 1869
dim(dat) # 2367, good


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

dim(dat) # 2367

# 3. Merge SuRGE lake list----
dat <- dat %>%
  left_join(
    # expand lake.list to include lacustrine, riverine, and transitional
    lake.list %>% # Surge sites
      select(-eval_status) %>% # this is for lake-scale, not needed. site_eval_status is for each point.
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
      filter(!lake_id == c(69|70))) %>%
  # differentiate SuRGE-scale design (e.g. lake_wgt) from lake-scale design (e.g. site_wgt) 
  rename(lake_mdcaty = mdcaty,
         lake_wgt = wgt,
         lake_stratum = stratum,
         lake_panel = panel,
         lake_site_type = site_type,
         lake_eval_status_code = eval_status_code, # all S? Do I need this variable?
         lake_eval_status_code_comment = eval_status_code_comment
         )

dim(dat) # 2367


# 4. Merge SuRGE + Falls Lake + 2016 Design Details------------
# dat_2016 contains site_wgt and site_stratum which were omitted from initial
# merge with all_obs (step 1).
# SuRGE and Falls Lake site_wgt and site_stratum are in lake_dsn

# join design details from both sources, then join into dat
dat <- bind_rows(
  # 2016 numbers
  dat_2016 %>% 
    select(lake_id, site_id, site_wgt, site_stratum) %>%
    mutate(lake_id = as.character(lake_id)), # lake_id is character in dat and lake_dsn
  # SuRGE numbers
  lake_dsn) %>%
  # joint design details with data
  right_join(dat)
          
dim(lake_dsn) # 4463, includes all oversample sites
dim(dat) # 2367

# 5. Merge NLA chemistry----
 # common names
 names(nla17_chem)[names(nla17_chem) %in% names(dat)] # nla17_site_id
 
 # 1000 (Puerto Rico), 69_riverine, 69_transitional, 70_lacustrine, 
 # 70_riverine, and most 2016 sites not in nla17_chem as expected. 
# 69_lacustrine and 70_transitional are included. See above.
 dat$lake_id[!(dat$nla17_site_id %in% nla17_chem$nla17_site_id)] %>% unique
 
 # merge
 dat <- dat %>%
   left_join(nla17_chem)
 dim(dat) # 2367
 
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


 # 6. Index site location----
 dat <- dat %>%
   left_join(index_site, 
             by = c("lake_id", "site_id", "visit")) # default, but specifying for clarity
 dim(dat) # 2367
 
 
# 7. Merge Waterisotope----
 dat <- dat %>%
   left_join(water_isotope_agg %>%
               # water isotope samples collected at NLA Index site. Here we 
               # associate the values with the correct river segment based
               # on index site lat long.
               mutate(lake_id = case_when(lake_id == "70" ~ "70_transitional", # NLA 43.39241 -99.13541
                                          lake_id == "69" ~ "69_lacustrine", # NLA 45.01971 -100.26474 
                                          TRUE ~ lake_id)))
 
 dim(dat) # 2367

# 8. Stratification Indices
 dat <- dat %>%
   left_join(strat_link, by=c("lake_id","visit"))
 
 dim(dat) # 2367
 
# 9. Phytoplankton Composition from Avery----
 dat <- dat %>%
   left_join(phyto_SuRGE_link, by="lake_id")
 dim(dat) # 2367
 
# 10. Move lacustrine, transitional, and riverine to site_id.----
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
 
 dim(dat) # 2367
 
# 11. Merge lakeMorpho data (readMorpho.R)----
 dat <- dat %>%
   left_join(.,
             morpho)

 dim(morpho) # 147
 dim(dat) # 2367

# 12. Merge hydroLakes ID----
 dat <- dat %>%
   left_join(hylak_link)
 
 dim(hylak_link) #127, 16
 dim(dat) # 2367
 
# 13. Merge LAGOS----
  dat <- dat %>%
   left_join(lagos_links)
 
 dim(lagos_links) #150, 16
 dim(dat) # 2367
 
# 14. Merge NID----
 dat <- dat %>%
   left_join(nid_link)
 
 dim(nid_link) #147, 16
 dim(dat) # 2367
 
# 15. Merge NHDPlusV2 - lakeCat----
dat <- dat %>%
   left_join(lake_cat_abbv,  by = c("nhd_plus_waterbody_comid" = "comid", "lake_id" = "lake_id"))
 
 dim(lake_cat) #147, 16
 dim(dat) # 2367

# 16. National Wetland Inventory----
dat <- dat %>%
  left_join(nwi_link)

 dim(nwi_link) #148, 16
 dim(dat) # 2367
 
# 17. Reservoir Sedimentaion----
dat <- dat %>%
  left_join(sedimentation_link, by = "lake_id")

 dim(sedimentation_link) # 139, 5
 dim(dat) # 2367
 
# 18. Water level change indices----
dat<- dat %>%
  left_join(walev_link, by = c("lake_id","visit"))
 
dim(walev_link) #21
dim(dat) # 2367

# 19. IPCC Climate Zone-----
dat <- dat %>%
  left_join(surge_climate, by = "lake_id")

dim(surge_climate) #149
dim(dat) # 2367

# 20. ERA5 Water and Air Temperature----
dat <- dat %>%
  left_join(met_temp, by = "lake_id") # values are identical for multiple visits

dim(met_temp) #146
dim(dat) # 2367

# write to disk
save(dat, file = paste0("output/dat_", Sys.Date(), ".RData"))

# 1. AGGREGATE BY LAKE_ID USING SITE WEIGHTS----------

## 1.1 aggregate emission rates across all lakes including transitional/riverine/lacustrine-----
emissions_agg <- dat %>%
  # All NAs break cont_analysis. No CH4 diffusion or from lake 253, set to 0
  # then filter results at end
  mutate(ch4_diffusion_best = replace(ch4_diffusion_best, lake_id == 253, 0),
         ch4_total = replace(ch4_total, lake_id == 253, 0),
         # add subsection identifiers back into lake_id
         lake_id = as.character(lake_id),
         lake_id = case_when(grepl("lacustrine", site_id) ~ paste0(lake_id, "_lacustrine"),
                             grepl("riverine", site_id) ~ paste0(lake_id, "_riverine"),
                             grepl("transitional", site_id) ~ paste0(lake_id, "_transitional"),
                             TRUE ~ lake_id)) %>% 
  rename_with(~gsub("_best", "", .x)) %>% 
  # split into list elements. use base::split to enable list
  # element naming
  split(., paste(.$lake_id, .$visit, sep = "_")) %>%
  #.[c("69_lacustrine_1", "69_transitional_1", "69_riverine_1", "70_lacustrine_1", "70_transitional_1", "70_riverine_1", "147_1")] %>% # subset for development. 10 is unstratified
  #within(., rm("253_1")) %>% # exclude list element by name for development
  imap(~.x %>%
        st_as_sf(., coords = c("long", "lat")) %>% # convert to sf object
        `st_crs<-` (4326) %>% # latitude and longitude
        st_transform(., 5070) %>% # Conus Albers
        cont_analysis(., # calculate statistics
                      siteID = "site_id",
                      vars = c("ch4_ebullition", "ch4_diffusion", "ch4_total",
                               "co2_ebullition", "co2_diffusion", "co2_total"),
                      weight = "site_wgt",
                      stratumID = "site_stratum") %>%
         # above function produces a list with 4 elements (CDF, Pct, Mean, Total)
         # We may want all of these, maybe not. Extracting mean for now
        .[["Mean"]] %>% # extract list element "Mean" as a dataframe
         janitor::clean_names(.) %>% # clean names
         rename(margin_of_error = marginof_error) %>%
         # add lake_id identifiers to data frame
         mutate(lake_id = str_extract(.y, "(^.+)\\_") %>% # everything up to and including final _
                  str_sub(., end = -2), # strip final character, which is _ here.
                visit = str_extract(.y, "(?<=\\_)\\d+$*") %>% as.numeric, # add visit. string after final _
                units = case_when(grepl("ch4", indicator) ~ "mg_ch4_m2_h",
                                  grepl("co2", indicator) ~ "mg_co2_m2_h",
                                  TRUE ~"Fly you fools")) %>%
         # Remove rows where emission = 0. This occurs when data were unavailable
         # and set to 0 so cont_analysis wouldn't break (e.g. 253, see above)
         filter(estimate != 0) %>% 
         select(-type, -subpopulation, -n_resp) %>%
         relocate(lake_id, visit) %>%
         as_tibble %>%
         pivot_wider(names_from = indicator, 
                     values_from = c(estimate, std_error, margin_of_error, lcb95pct, ucb95pct, units),
                     names_glue = "{indicator}_{.value}_lake") %>%
         rename_with(~ gsub("estimate_", "", .x, fixed = TRUE))) %>%
  dplyr::bind_rows(.)

## 1.2 Aggregate Missouri River impoundments based on habitat weights-----  
missouri_agg <- emissions_agg %>%
  filter(grepl(c("69|70"), lake_id)) %>% # grab 69 and 70, lac, riv, lac
  # scale values by proportion of each habitat. Applying to mean and SE.
  # If SE are not scaled, the aggregated SE are too big. I think scaling
  # SE is correct
  mutate(
    # mean emission rates
    across(c(ch4_ebullition_lake, ch4_diffusion_lake, ch4_total_lake,
             co2_ebullition_lake, co2_diffusion_lake, co2_total_lake),
           # proportions defined in missoureRiverHabitatWeights.R
           ~case_when(lake_id == "69_lacustrine" ~ . * 0.685, # proportion of 69 in lacustrine
                      lake_id == "69_transitional" ~ . * 0.259, # proportion of 69 in transitional
                      lake_id == "69_riverine" ~ . * 0.0553, # proportion of 69 in riverine
                      lake_id == "70_lacustrine" ~ . * 0.46, # proportion of 70 in lacustrine
                      lake_id == "70_transitional" ~ . * 0.176, # proportion of 70 in transitional
                      lake_id == "70_riverine" ~ . * 0.364, # proportion of 70 in riverine
                      TRUE ~ . * 99999)),
    # standard errors
    across(contains("std_error"),
           # proportions defined in missoureRiverHabitatWeights.R
           ~case_when(lake_id == "69_lacustrine" ~ . * 0.685, # proportion of 69 in lacustrine
                      lake_id == "69_transitional" ~ . * 0.259, # proportion of 69 in transitional
                      lake_id == "69_riverine" ~ . * 0.0553, # proportion of 69 in riverine
                      lake_id == "70_lacustrine" ~ . * 0.46, # proportion of 70 in lacustrine
                      lake_id == "70_transitional" ~ . * 0.176, # proportion of 70 in transitional
                      lake_id == "70_riverine" ~ . * 0.364, # proportion of 70 in riverine
                      TRUE ~ . * 99999))) %>%
  # omit riv/lac/trans from lake_id to facilitate grouping by reservoir
  mutate(lake_id = case_when(grepl("69", lake_id) ~ "69",
                             grepl("70", lake_id) ~ "70",
                             TRUE ~ lake_id)) %>%
  group_by(lake_id, visit) %>% # only one visit, but this carries the variable through
  #select(ch4_diffusion_std_error_lake) %>% # for development
  summarize(
    # Sum habitat weighted means
    across(c(ch4_ebullition_lake, ch4_diffusion_lake, ch4_total_lake,
             co2_ebullition_lake, co2_diffusion_lake, co2_total_lake),
           sum),
    # aggregate habitat specific standard errors as square root of sum of squares
    across(contains("std_error"), ~ sqrt(sum(.x^2))),
    # retain units
    across(contains("units"), ~ unique(.x))) %>% 
  ungroup %>%
  # recalc 95% CI mean +/- (1.96*SE)
  mutate(
    # CH4 lower 95% confidence bound
    ch4_ebullition_lcb95pct_lake = ch4_ebullition_lake - (1.96 * ch4_ebullition_std_error_lake),
    ch4_diffusion_lcb95pct_lake = ch4_diffusion_lake - (1.96 * ch4_diffusion_std_error_lake),
    ch4_total_lcb95pct_lake = ch4_total_lake - (1.96 * ch4_total_std_error_lake),
    # CH4 upper 95% confidence bound
    ch4_ebullition_ucb95pct_lake = ch4_ebullition_lake + (1.96 * ch4_ebullition_std_error_lake),
    ch4_diffusion_ucb95pct_lake = ch4_diffusion_lake + (1.96 * ch4_diffusion_std_error_lake),
    ch4_total_ucb95pct_lake = ch4_total_lake + (1.96 * ch4_total_std_error_lake),
    # CO2 lower 95% confidence bound
    co2_ebullition_lcb95pct_lake = co2_ebullition_lake - (1.96 * co2_ebullition_std_error_lake),
    co2_diffusion_lcb95pct_lake = co2_diffusion_lake - (1.96 * co2_diffusion_std_error_lake),
    co2_total_lcb95pct_lake = co2_total_lake - (1.96 * co2_total_std_error_lake),
    # CO2 upper 95% confidence bound
    co2_ebullition_ucb95pct_lake = co2_ebullition_lake + (1.96 * co2_ebullition_std_error_lake),
    co2_diffusion_ucb95pct_lake = co2_diffusion_lake + (1.96 * co2_diffusion_std_error_lake),
    co2_total_ucb95pct_lake = co2_total_lake + (1.96 * co2_total_std_error_lake)
  )

## 1.3 Check aggregated Missouri River data against habitat specific values-------
# these seem reasonable
bind_rows(missouri_agg,
          emissions_agg %>% 
            filter(grepl(c("69|70"), lake_id)) %>% # grab 69 and 70, lac, riv, lac
            select(-contains("margin"))) %>%
  select(-contains("units")) %>%
  pivot_longer(!(lake_id)) %>%
  #filter(grepl("co2", name)) %>% # just co2 to make faceted panes readable
  filter(grepl("ch4", name)) %>% # just ch4 to make faceted panes readable
  ggplot(aes(lake_id, value)) +
  geom_point() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
  facet_wrap(~name, scales = "free")


## 1.4 Replace lac/riv/trans in emissions_agg with aggregated values-------
emissions_agg <- emissions_agg %>%
  # strip the habitat specific estimates for 69 and 70
  filter(!grepl(c("69|70"), lake_id)) %>%
  # now add aggregated Missouri River values
  bind_rows(missouri_agg) %>%
  mutate(lake_id = as.numeric(lake_id))


## 1.5 make sure lake-wide means are within the range of site data from each lake-----
bind_rows(emissions_agg %>%
            select(-contains("95"), # remove 95% confidence interval
                   -contains("error"), # remove margin of error 
                   -contains("units")) %>% # remove emission rate units
            mutate(source = "lake") %>% # whole-lake estimate, as opposed to site specific
            rename_with(~gsub("_lake", "", .)), # remove "_lake" from lake_id values
          dat %>%
            # add lac/river/trans to lake_id to facilitate merge.
            # mutate(lake_id = as.character(lake_id),
            #        lake_id = case_when(grepl("lacustrine", site_id) ~ paste0(lake_id, "_lacustrine"),
            #                            grepl("riverine", site_id) ~ paste0(lake_id, "_riverine"),
            #                            grepl("transitional", site_id) ~ paste0(lake_id, "_transitional"),
            #                            TRUE ~ lake_id)) %>%
            select(lake_id, visit, contains("ch4"), contains("co2"),
                   -contains("units"), -contains("note")) %>%
            rename_with(~gsub("_best", "", .)) %>%
            mutate(source = "site")) %>% 
  mutate(lake_id = as.factor(lake_id)) %>%
  # ggplot(aes(lake_id, ch4_diffusion)) +
  # geom_point(aes(color = source)) +
  # scale_y_log10()
  

# ggplot(aes(lake_id, ch4_ebullition)) +
#   geom_point(aes(color = source)) +
#   scale_y_log10()


ggplot(aes(lake_id, ch4_total)) +
  geom_point(aes(color = source)) +
  scale_y_log10()


ggplot(aes(lake_id, co2_diffusion)) +
  geom_point(aes(color = source)) 


ggplot(aes(lake_id, co2_total)) +
  geom_point(aes(color = source)) 


# 10/10/2024 NOT WORKING, IN PROGRESS....---------
# Might be better to start with: 
# dat <- bind_rows(dat_2016 %>% 
#                    mutate(lake_id = as.character(lake_id)) %>% # converted to numeric below
#                    select(-chamb_deply_date_time, -site_wgt, -site_stratum), # these get added later
#                  all_obs %>%
#                    # eval_status is inherited from fld_sheet and reflects
#                    # site_id evaluations made during sampling. Changing name here,
#                    # rather than earlier, to avoid unanticipated consequences.
#                    # eval_status is referenced in numerous existing lines of code
#                    rename(site_eval_status = eval_status)) # consistent with dat_2016
# then aggregate by lake below, then join with other tables.

dat_agg <- dat %>%
    select(-site_id, -site_wgt, -lat, - long, -site_depth,-site_eval_status, -trap_rtrvl_date_time, -trap_deply_date_time, 
           -site_depth, 
           -deep_chla_sonde_comment, -deep_do_mg_comment, -deep_ph_comment, -deep_phycocyanin_sonde_comment,
           -deep_temp_comment, -deep_turb_comment, -shallow_chla_sonde_comment, -shallow_do_mg_comment,
           -shallow_ph_comment, -shallow_phycocyanin_sonde_comment, -shallow_sp_cond_comment, -shallow_temp_comment,
           -shallow_turb_comment, -sample_year, 
           -E_I_type, 
           -contains("ch4"), -contains("co2")) %>% # recalc totals after aggregating by lake
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

