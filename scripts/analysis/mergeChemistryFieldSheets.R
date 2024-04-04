# MERGING FIELD SHEETS (DATA TAB) AND CLEAN CHEMISTRY FILE (NO DUPS/BLANKS)
# only merging sonde data from field sheets for now

# Merge these two object
chemistry
fld_sheet

# fld_sheet in wide, but chemistry in long.  pivot fld_sheet
# to long before merge

# First, split out data fields not associated with a depth
fld_sheet_no_depth <- fld_sheet %>% 
  select(lake_id, site_id, visit, lat, long, eval_status,
         #chm_vol_l, # pulled from fld_sheet in calculateDiffusion.  can be dropped
         site_depth, # numeric, total depth
         trap_deply_date) %>% # keep this to indicate sample_year
  rename(sample_date = trap_deply_date)

  
# pull out sonde data, pivot to long, then back to wider
fld_sheet_sonde <- fld_sheet %>% 
  select(-eval_status,
         -contains("trap"),
         -contains("chamb"),
         -contains("air"),
         -contains("check"),
         -lat, -long, -site_depth, -chm_vol_l) %>%
  mutate(across(everything(), ~as.character(.x))) %>% # all columns must be same class
  pivot_longer(!(c(lake_id, site_id, visit))) %>%
  # move depth info from column names into separate columns
  mutate(sample_depth = case_when(
    grepl("_s$", name) ~ "shallow", # values that end with _s (i.e. sample_depth_s)
    grepl("s_flag", name) ~ "shallow", # get shallow flag
    grepl("_s_comment", name) ~ "shallow", # get shallow comment
    grepl("_d$", name) ~ "deep", # values that end with _d (i.e. sample_depth_d)
    grepl("d_flag", name) ~ "deep", # get shallow flag
    grepl("_d_comment", name) ~ "deep", # get shallow comment
    TRUE ~ NA_character_),
    # remove sample depth info from column names
    name = case_when(grepl("_s$", name) 
                     ~ gsub("_s$", "", name), # Remove _s from end of value
                     grepl("s_flag", name) 
                     ~ gsub("s_flag", "flag", name), # Remove _s from _s_flag
                     grepl("s_comment", name) 
                     ~ gsub("s_comment", "comment", name), # Remove _s from _s_comment
                     grepl("_d$", name) 
                     ~ gsub("_d$", "", name), # Remove _d from end of value
                     grepl("d_flag", name) 
                     ~ gsub("d_flag", "flag", name), # Remove _d from _d_flag
                     grepl("d_comment", name) 
                     ~ gsub("d_comment", "comment", name), # Remove _d from _d_comment
                     TRUE ~ name),
    #sample_depth is 'shallow' or 'deep'.  The actual depth of sample collection will be 'sample_depth_m'
    name = replace(name, name == "sample_depth", "sample_depth_m")) 

# Before pivoting to wide, make sure no rows have more than one value
problem_rows <- fld_sheet_sonde %>% 
  group_by(lake_id, site_id, visit, sample_depth, name) %>%  
  summarize(n = n()) %>%          
  filter(n > 1) # identify any rows that have more than one value                

# Pivot back to wide, but now longer, just like chemistry
fld_sheet_sonde <- fld_sheet_sonde %>% 
  pivot_wider(names_from = name, values_from = value) %>% 
  # convert numeric back to numeric
  mutate(across(.cols = c(site_id, sample_depth_m, temp, 
                          do_mg, sp_cond, ph, chl, turb,
                          visit), 
                ~ as.numeric(.x))) %>%
  # sort sonde parameters alphabetically
  select(sort(tidyselect::peek_vars())) %>%
  relocate(lake_id, site_id, 
           sample_depth, sample_depth_m) # put identifiers first

    


# Now merge in non depth specific fields from fld_sheets
fld_sheet_sonde1 <- full_join(fld_sheet_sonde, fld_sheet_no_depth)
dim(fld_sheet_sonde) # 4104, 26 [4/3/2024]
dim(fld_sheet_no_depth) #2052, 9 [4/3/2024]
dim(fld_sheet_sonde1) #4104, 32 good [4/3/2024]

# Will merge on common names: lake_id, site_id, visit
names(chemistry)[names(chemistry) %in% names(fld_sheet_sonde1)] #lake_id, site_id, sample_depth, visit
names(fld_sheet_sonde1)[names(fld_sheet_sonde1) %in% names(chemistry)] #lake_id, site_id, sample_depth, visit
class(chemistry$lake_id) == class(fld_sheet_sonde1$lake_id) # TRUE
class(chemistry$site_id) == class(fld_sheet_sonde1$site_id) # TRUE

# Check dimensions
dim(chemistry) # 265, 124 [4/3/2024]
dim(fld_sheet_sonde1) # 4104, 32 [4/3/2024]

# Check for correspondence among unique identifiers
chemistry %>% filter(!(lake_id %in% fld_sheet_sonde1$lake_id)) %>%
  select(lake_id) %>% distinct() %>% print(n=Inf)


# any lakes in fld_sheets, but not in chemistry? 
# Good
fld_sheet_sonde1 %>% filter(!(lake_id %in% chemistry$lake_id)) %>%
  select(lake_id) %>% distinct() %>% print(n=Inf)

chem_fld <- full_join(chemistry, fld_sheet_sonde1) %>%
  relocate(lake_id, site_id, lat, long, sample_date, site_depth, sample_depth, sample_depth_m)
dim(chem_fld) # 4111, 152 [4/3/2024]

# write to disk for reference in lake reports
save(chem_fld, file = "output/chem_fld.RDATA")
