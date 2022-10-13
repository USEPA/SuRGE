# NUTRIENT ANALYSIS ON LACHAT CONDUCTED IN AWBERC
# This script reads in the standard curves used for the 2021 AWBERC nutrient
# data.  The objective is to determine the low standard used for each analyte.
# We will define the reporting limit as the value of the lowest non-zero standard.

# SCRIPT TO READ IN COC FORMS--------------------
# TTEB data file contains data from many different project.  Here we read in
# the SuRGE CoC forms; we use these CoCs to filter out SuRGE data from TTEB
# data file.

# 1. Read in chain of custody file names
# sheet name is "water (original)", but sometimes there is a space before 
# "water".  str_detect looks for "water" in sheet name.
# https://stackoverflow.com/questions/61308709/r-use-regex-to-import-specific-sheets-from-multiple-excel-files
coc.list <- fs::dir_ls(path = paste0(userPath,  "data/chemistry/nutrients"), # get file names
                       regexp = "COC", # file names containing this pattern
                       recurse = FALSE) %>% # do not look in subdirectories
  .[!grepl("initials", .)] %>% # exclude template
  # map will read each file and select columns of interest
  purrr::map(~read_excel(.x, skip = 5, 
                         sheet = which(str_detect(excel_sheets(.x), "water"))) %>%
               select(contains("Lake"), contains("Site")) %>%
               mutate(across(everything(), ~as.character(.)))) # mix of character and double causes merge issues.  coerce all to character


# 2. in some COC the lake_id value is in the LakeID column, whereas in others it
# is in the SiteID... column.  specify correct columns
coc.vector <- coc.list %>%
  map_dfr(., # map_dfr will rbind list objects into df
          function(x) if(length(names(x)) > 1) { # if >1 column..
    select(x, LakeID) # grab LakeID
  } else {
    x # else, return the original df with one column
  }
  ) %>%
  pivot_longer(everything()) %>% # collapse into one column of values
  filter(!is.na(value)) %>% # remove NA
  select(value) %>% # pull out lake id values
  distinct(value) %>% # get rid of duplicates
  pull(value) # pull column into a vector



# SCRIPT TO READ IN WATER CHEM------------------


# function reads in data, filters to SuRGE data, and extracts analyte and run date (ddate)
get_awberc_data_ddate <- function(path, data, sheet) { 
  
  d <- read_excel(paste0(path, data), #
                  sheet = sheet, skip = 1, guess_max = 10000) %>%
  # d <- read_excel(paste0(cin.awberc.path,
  #                        "2021_ESF-EFWS_NutrientData_Updated01272022_AKB.xlsx"), #
  #                 sheet = "2021 Data", skip = 1) %>%
    janitor::clean_names() %>% # clean up names for rename and select, below
    janitor::remove_empty(which = c("rows", "cols")) %>% # remove empty rows
    rename(rdate = collection_date_cdate, #rename fields
           ddate = analyte_detection_date_ddate,
           finalConc = peak_concentration_corrected_for_dilution_factor,
           analyte = analyte_name_analy,
           tp_tn = tp_tn_adjusted_concentration_full_series_ug_p_l,
           lake_id = site_id_id,
           crossid = c_ross_id_pos, 
           site_id = long_id_subid,
           sample_type = type, 
           rep = rep_number) %>%
    mutate(nutrients_qual = if_else( # determine if holding time exceeded
      (as.Date(ddate) - as.Date(rdate)) > 28, TRUE, FALSE)) %>% # TRUE = hold time violation
    mutate(finalConc = ifelse( # correct TP and TN are in tp_tn
      analyte %in% c("TP", "TN"),
      tp_tn,
      finalConc)) %>%  
    filter(sample_type != "SPK") %>% # exclude matrix spike
    select(lake_id, site_id, crossid, sample_type, analyte, finalConc, nutrients_qual, rep, ddate) %>% # keep only needed fields
    mutate(analyte = str_to_lower(analyte)) %>% # make analyte names lowercase
    mutate(analyte = case_when( # change analyte names where necessary
      analyte == "trp" ~ "op",
      analyte == "tnh4" ~ "nh4",
      analyte == "tno2" ~ "no2",
      analyte == "tno2-3" ~ "no2_3",
      TRUE   ~ analyte)) %>%
    # strip character values from site_id, convert to numeric
    mutate(site_id =  as.numeric(gsub(".*?([0-9]+).*", "\\1", site_id))) %>%
    mutate(sample_type = case_when( # recode sample type identifiers
      grepl("dup", sample_type, ignore.case = TRUE) ~ "duplicate",
      grepl("ukn", sample_type, ignore.case = TRUE) ~ "unknown",
      grepl("blk", sample_type, ignore.case = TRUE) ~ "blank",
      TRUE ~ sample_type)) %>%
    # sample filtered or unfiltered
    mutate(filter = str_sub(crossid, 1, 1) %>% tolower(.),
           filter = case_when(
             filter == "d" ~ "filtered",
             filter == "t" ~ "unfiltered",
             TRUE ~ filter)) %>%
    # define sample depth
      mutate(sample_depth = str_sub(crossid, 3, nchar(crossid)),
             sample_depth = case_when(
               grepl("d", sample_depth, ignore.case = TRUE) ~ "deep",
               grepl("s", sample_depth, ignore.case = TRUE) ~ "shallow",
               TRUE ~ sample_depth)) %>%
    # filtered sample has really high NH4 (59.3), but unfiltered from same depth
    # has much lower (3.94).  Replacing suspicious value with lower value
    mutate(finalConc = replace(finalConc,
                               lake_id == "070 River" & sample_depth == "shallow" & 
                                 analyte == "nh4" & filter == "filtered", 
                               3.94)) %>%
    mutate() %>%
    # strip out unneeded analyses
    # exclude filtered samples run for totals
    filter(!(analyte %in% c("tp", "tn") & filter == "filtered")) %>%
    # exclude unfiltered samples run for inorganics
    filter(!(analyte %in% c("nh4", "op", "no2_3", "no2") & filter == "unfiltered")) %>%
    filter(!(analyte == "turea")) %>% # exclude urea
    #filter(grepl(pattern = c("CH4|069|070"), lake_id)) %>% # Filter SuRGE data
    filter(lake_id %in% coc.vector) %>% # Filter SuRGE data, see above
    mutate(lake_id = str_remove(lake_id, "CH4-")) %>% # standardize lake IDs
    mutate(lake_id = case_when( # # standardize lake ID sub-component names
      str_detect(lake_id, "LAC") ~ str_replace(lake_id, " LAC", "_lacustrine"),
      str_detect(lake_id, "River") ~ str_replace(lake_id, " River", "_riverine"),
      str_detect(lake_id, "Trans") ~ str_replace(lake_id, " Trans", "_transitional"),
      lake_id == "70" ~ "70_transitional",
      lake_id == "69" ~ "69_riverine",
      TRUE ~ lake_id)) %>%
    mutate(lake_id = case_when(
      str_detect(lake_id, "070") ~ str_replace(lake_id, "070", "70"), # need to replace 070 with 70
      str_detect(lake_id, "069") ~ str_replace(lake_id, "069", "69"), # need to replace 069 with 69
      str_detect(lake_id, "016") ~ str_replace(lake_id, "016", "16"), # need to replace 016 with 16
      str_detect(lake_id, "082") ~ str_replace(lake_id, "082", "82"), # need to replace 082 with 82
      TRUE ~ lake_id)) %>%
    mutate(finalConc = as.numeric(finalConc)) %>% # make analyte values numeric
    mutate(analyte_flag = case_when( # create the analyte_flag column
      analyte == "nh4" & finalConc < 6 ~ "<", 
      analyte == "no2" & finalConc < 6 ~ "<",
      analyte == "no2_3" & finalConc < 6 ~ "<",
      analyte == "op" & finalConc < 3 ~ "<",
      analyte == "tn" & finalConc < 25 ~ "<",
      analyte == "tp" & finalConc < 5 ~ "<",
      TRUE ~ "")) %>%
    mutate(finalConc = case_when( # create the analyte_flag column
      analyte == "nh4" & finalConc < 6 ~ 6, 
      analyte == "no2" & finalConc < 6 ~ 6,
      analyte == "no2_3" & finalConc < 6 ~ 6,
      analyte == "op" & finalConc < 3 ~ 3,
      analyte == "tn" & finalConc < 25 ~ 25,
      analyte == "tp" & finalConc < 5 ~ 5,
      TRUE ~ finalConc)) %>%
    mutate(units = case_when(
      analyte == "nh4"  ~ "ug_n_l",
      analyte == "no2"  ~ "ug_n_l",
      analyte == "no2_3"  ~ "ug_n_l",
      analyte == "op"  ~ "ug_p_l",
      analyte == "tn"  ~ "ug_n_l",
      analyte == "tp"  ~ "ug_p_l",
      TRUE ~ "")) %>%
    mutate(sample_type = case_when(
      sample_type == "duplicate" ~ "unknown",
      TRUE ~ sample_type)) %>%
    mutate(sample_depth = case_when(
      sample_type == "blank" ~ "blank", # see Wiki lake_id, site_id, and sample_depth formats
      TRUE ~ sample_depth)) %>%
    mutate(rep = case_when(
      rep == "A" ~ "1", # convert to number for dup_agg function
      rep == "B" ~ "2",
      TRUE ~ rep)) %>%
    select(-crossid)  %>% # no longer need crossid
    mutate(site_id = as.numeric(site_id)) %>% # convert to char to match other chem data 
    select(analyte, ddate) %>%
    distinct(analyte, ddate)
  
    
  return(d)
  
  
}




# function reads in standards
get_awberc_data_all <- function(path, data, sheet) { 
  
  d <- read_excel(paste0(path, data), #
                  sheet = sheet, skip = 1, guess_max = 10000) %>%
    # d <- read_excel(paste0(cin.awberc.path,
    #                        "2021_ESF-EFWS_NutrientData_Updated01272022_AKB.xlsx"), #
    #                 sheet = "2021 Data", skip = 1) %>%
    janitor::clean_names() %>% # clean up names for rename and select, below
    janitor::remove_empty(which = c("rows", "cols")) %>% # remove empty rows
    rename(rdate = collection_date_cdate, #rename fields
           ddate = analyte_detection_date_ddate,
           finalConc = peak_concentration_corrected_for_dilution_factor,
           analyte = analyte_name_analy,
           #tp_tn = tp_tn_adjusted_concentration_full_series_ug_p_l, # want known standard concentration, not calculated value
           lake_id = site_id_id,
           crossid = c_ross_id_pos, 
           site_id = long_id_subid,
           sample_type = type, 
           rep = rep_number) %>%
    select(lake_id, site_id, crossid, sample_type, analyte, finalConc, rep, ddate) %>% # keep only needed fields
    mutate(analyte = str_to_lower(analyte)) %>% # make analyte names lowercase
    mutate(analyte = case_when( # change analyte names where necessary
      analyte == "trp" ~ "op",
      analyte == "tnh4" ~ "nh4",
      analyte == "tno2" ~ "no2",
      analyte == "tno2-3" ~ "no2_3",
      TRUE   ~ analyte)) %>%
    mutate(finalConc = as.numeric(finalConc)) %>%
    select(site_id, ddate, finalConc, analyte, sample_type) %>%
    filter(site_id == "STD", sample_type == "CAL")  # get calibration standards
   
  
  
  return(d)
  
  
}

cin.awberc.path <- paste0(userPath, 
                          "data/chemistry/nutrients/")

# get run dates for SuRGE samples
chem21_ddate <- get_awberc_data_ddate(cin.awberc.path, 
                                      "2021_ESF-EFWS_NutrientData_Updated01272022_AKB.xlsx", 
                                      "2021 Data") 

# read standard curves
chem21_all <- get_awberc_data_all(cin.awberc.path, 
                                      "2021_ESF-EFWS_NutrientData_Updated01272022_AKB.xlsx", 
                                      "2021 Data") 

# chem21_all contains all standards used for standard curves in 2021.  Filter down
# to runs that included SuRGE samples.  Those dates are in chem21_ddate


chem21_all %>% 
  filter(ddate %in% chem21_ddate$ddate, analyte %in% chem21_ddate$analyte) %>%
  filter(finalConc != 0) %>% # remove 0 standard
  group_by(ddate, analyte) %>%
  summarise(minimum_std = min(finalConc)) %>%
  ggplot(., aes(ddate, minimum_std)) +
  geom_point() +
  facet_wrap(~analyte, scales = "free")

chem21_all %>% 
  filter(ddate %in% chem21_ddate$ddate, analyte %in% chem21_ddate$analyte) %>%
  filter(finalConc != 0, analyte == "tp") %>%
  arrange(ddate, finalConc)
