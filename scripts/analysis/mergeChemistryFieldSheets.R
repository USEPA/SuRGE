# MERGING FIELD SHEETS (DATA TAB) AND CLEAN CHEMISTRY FILE (NO DUPS/BLANKS)
# only merging sonde data from field sheets for now

# Merge these two object
chemistry
fld_sheet

# fld_sheet in wide, but chemistry in long.  pivot fld_sheet
# to long before merge

# First, split out data fields not associated with a depth
fld_sheet_no_depth <- fld_sheet %>% 
  select(-eval_status,
         -contains("trap"),
         -contains("chamb"),
         -contains("air"),
         -contains("check"),
         -contains("_s"), # shallow sonde
         -contains("_d"), # deep sonde
         lake_id, site_id, lat, long, 
         site_depth, # character, 'shallow' 'deep'
         trap_deply_date) %>% # keep this to indicate sample_year
  rename(sample_date = trap_deply_date)
  
# pull out sonde data, pivot to long, then back to wider
fld_sheet_sonde <- fld_sheet %>% 
  select(-eval_status,
         -contains("trap"),
         -contains("chamb"),
         -contains("air"),
         -contains("check"),
         -lat, -long, -site_depth) %>%
  mutate(across(everything(), ~as.character(.x))) %>% # all columns must be same class
  pivot_longer(!(c(lake_id, site_id))) %>%
  # move depth info from column names into separate columns
  mutate(sample_depth = case_when(grepl("_s$", name) ~ "shallow", # values that end with _s (i.e. sample_depth_s)
                                  grepl("s_flag", name) ~ "shallow", # get shallow flag
                                  grepl("_s_comment", name) ~ "shallow", # get shallow comment
                                  grepl("_d$", name) ~ "deep", # values that end with _d (i.e. sample_depth_d)
                                  grepl("d_flag", name) ~ "deep", # get shallow flag
                                  grepl("_d_comment", name) ~ "deep", # get shallow comment
                                  TRUE ~ NA_character_),
         # remove sample depth info from column names
         name = case_when(grepl("_s$", name) ~ gsub("_s$", "", name), # Remove _s from end of value
                          grepl("s_flag", name) ~ gsub("s_flag", "flag", name), # Remove _s from _s_flag
                          grepl("s_comment", name) ~ gsub("s_comment", "comment", name), # Remove _s from _s_comment
                          grepl("_d$", name) ~ gsub("_d$", "", name), # Remove _d from end of value
                          grepl("d_flag", name) ~ gsub("d_flag", "flag", name), # Remove _d from _d_flag
                          grepl("d_comment", name) ~ gsub("d_comment", "comment", name)), # Remove _d from _d_comment
         #sample_depth is 'shallow' or 'deep'.  The actual depth of sample collection will be 'sample_depth_m'
         name = replace(name, name == "sample_depth", "sample_depth_m")) 

# Pivot back to wide, but now longer, just like chemistry
fld_sheet_sonde <- fld_sheet_sonde %>% 
  pivot_wider(names_from = name, values_from = value) %>%
  # convert numeric back to numeric
  mutate(across(.cols = c(site_id, sample_depth_m, temp, do_mg, sp_cond, ph, chl, turb), ~as.numeric(.x))) %>%
  # sort sonde parameters alphabetically
  select(sort(tidyselect::peek_vars())) %>%
  relocate(lake_id, site_id, sample_depth, sample_depth_m) # put identifiers first



# Now merge in non depth specific fields from fld_sheets
fld_sheet_sonde1 <- full_join(fld_sheet_sonde, fld_sheet_no_depth)
dim(fld_sheet_sonde) # 2060, 22 [6/2/2022]
dim(fld_sheet_no_depth) #1030, 6 [6/2/2022]
dim(fld_sheet_sonde1) #2060, 26 good [6/2/2022]

# Will merge on common names: lake_id and site_id
names(chemistry)[names(chemistry) %in% names(fld_sheet_sonde1)] #lake_id, site_id, sample_depth
names(fld_sheet_sonde1)[names(fld_sheet_sonde1) %in% names(chemistry)] #lake_id, site_id, sample_depth
class(chemistry$lake_id) == class(fld_sheet_sonde1$lake_id) # TRUE
class(chemistry$site_id) == class(fld_sheet_sonde1$site_id) # TRUE

# Check dimensions
dim(chemistry) # 153, 133 [5/31/2022]
dim(fld_sheet_sonde1) # 2060, 26 [5/31/2022]

# Check for correspondence among unique identifiers
# 7 lakes in chemistry, but not in field sheets.  These are all
# R10 lakes from 2018.  As of 5/31/2022, these field sheets
# haven't been read in yet.
chemistry %>% filter(!(lake_id %in% fld_sheet_sonde1$lake_id)) %>%
  select(lake_id) %>% distinct() %>% print(n=Inf)


# any lakes in fld_sheets, but not in chemistry? -NO
fld_sheet_sonde1 %>% filter(!(lake_id %in% chemistry$lake_id)) %>%
  select(lake_id) %>% distinct() %>% print(n=Inf)

chem_fld <- full_join(chemistry, fld_sheet_sonde1) %>%
  relocate(lake_id, site_id, lat, long, sample_date, site_depth, sample_depth, sample_depth_m)
dim(chem_fld) # 2090, 156 [5/31/2022]

# write to disk for reference in lake reports
save(chem_fld, file = "output/chem_fld.RDATA")
