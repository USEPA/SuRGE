# NUTRIENT ANALYSIS ON LACHAT CONDUCTED IN AWBERC

# Original data can be found at: L:\Priv\Cin\ORD\Pegasus-ESF\Lachat Data\Nutrients
# Copy of original 2021 data file at SP: 
# ....data\chemistry\nutrients\2021_ESF-EFWS_NutrientData_Updated11082021_AKB.xlsx"

# see readChem.R in mulitResSurvey repo for an example of how to read data.

# Need to create a "nutrients_qual" column to indicate holding time violations.
# Value of "HOLD" if holding time violated, else blank.  Holding time should be
# calculated as difference between "analyte_detection_date" and "collection_date".
# use flag columns (i.e. srp_flag, no2_3_flag) to indicate censored values.  

# The lab quantified inorganics (no2, no2.3, oP (named RP in awberc data file),
# and nh4) in both filtered and unfiltered samples.  We only want inorganic data
# for filtered samples.  Filtered samples can be identified by the "D" proceeding
# the sample collection depth in the 'pos $char4.' column (e.g., D-Sh).

# The lab quantified total nutrients (tp and tn) for both filtered and unfiltered
# samples.  We only want tp and tn for unfiltered samples.  Unfiltered samples
# can be identified by the "T" proceeding the sample collection depth in the 
# 'pos $char4.' column (e.g., T-Sh).

# All analyte names begin with a "T" to indicate "total" (e.g. TNH4).  Ignore this
# and use the analyte names defined in github Wiki page ("Chemistry units,
# names, and data sources).

# SCRIPT TO READ IN COC FORMS
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
               select(contains("Lake"), contains("Site"))) 


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



# SCRIPT TO READ IN WATER CHEM

# The correction to 069 LAC will be addressed in update to nutrient data.  Will
# delete the relevant code below.
# problems with 298, 231, and 233 are being addressed by Andrea and Bill.  Check
# on these after new update has been issued.
# data from the following lakes has been reviewed as of 1/7/2022:
# 233, 237, 236, 144, 155, 275, 240, 069_lacustrine, 069_transitional, 070_lacustrine,
# 070_riverine, 288, 316, 79, 298, 75, 326, 327, 70_transitional, 231, 78, 232,
# 68, 69_riverine


# function reads in data, filters data, and creates all new columns
get_awberc_data <- function(path, data, sheet) { 
  
  d <- read_excel(paste0(path, data), #
                  sheet = sheet, skip = 1) %>%
  # d <- read_excel(paste0(cin.awberc.path,
  #                        "2021_ESF-EFWS_NutrientData_Updated11212021_AKB.xlsx"), #
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
    # No deep samples collected at 069 LAC.  The deep sample in the file is
    # actually shallow.
    mutate(crossid = case_when(lake_id == "069 LAC" & crossid == "T-Dp" ~ "T-Sh",
                               TRUE ~ crossid)) %>%
    mutate(nutrients_qual = if_else( # determine if holding time exceeded
      (as.Date(ddate) - as.Date(rdate)) > 28, TRUE, FALSE)) %>% # TRUE = hold time violation
    mutate(finalConc = ifelse( # correct TP and TN are in tp_tn
      analyte %in% c("TP", "TN"),
      tp_tn,
      finalConc)) %>%  
    filter(sample_type != "SPK") %>% # exclude matrix spike
    select(lake_id, site_id, crossid, sample_type, analyte, finalConc, nutrients_qual, rep) %>% # keep only needed fields
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
      sample_type == "UKN" ~ "unknown",
      sample_type == "DUP" ~ "duplicate",
      sample_type == "BLK" ~ "blank",
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
    mutate(finalConc = as.numeric(finalConc)) %>% # make analyte values numeric
    mutate(analyte_flag = case_when( # create the analyte_flag column
      analyte == "nh4" & finalConc < 7 ~ "<", 
      analyte == "no2" & finalConc < 8 ~ "<",
      analyte == "no2_3" & finalConc < 8 ~ "<",
      analyte == "op" & finalConc < 0.7 ~ "<",
      analyte == "tn" & finalConc < 8 ~ "<",
      analyte == "tp" & finalConc < 8 ~ "<",
      TRUE ~ "")) %>%
    mutate(finalConc = case_when( # create the analyte_flag column
      analyte == "nh4" & finalConc < 7 ~ 7, 
      analyte == "no2" & finalConc < 8 ~ 8,
      analyte == "no2_3" & finalConc < 8 ~ 8,
      analyte == "op" & finalConc < 0.7 ~ 0.7,
      analyte == "tn" & finalConc < 8 ~ 8,
      analyte == "tp" & finalConc < 8 ~ 8,
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
    select(-crossid) # no longer need crossid
    
  return(d)
  
  
}


# function aggregates dups, renames/sorts columns, and casts to wide
dup_agg <- function(data) { 

  # aggregate dups and convert analyte_flags to numeric (for summarize operations)
  e <- data %>%
    mutate(analyte_flag = if_else(str_detect(analyte_flag, "<"), 1, 0)) %>% # convert to numeric
    group_by(lake_id, site_id, analyte, sample_depth, 
             sample_type, rep, nutrients_qual, units) %>%
    summarize(value = mean(finalConc, na.rm = TRUE), # group and calculate means
              analyte_flag = mean(analyte_flag, na.rm = TRUE)) %>%
    mutate(sample_type = case_when(
      rep %in% c("B", 2) ~ "duplicate", TRUE ~ sample_type)) %>%
    select(-rep)



  # cast to wide, convert analyte flags to text, and convert any NaN to NA
  f <- e %>%
    pivot_wider(names_from = analyte, values_from = c(value, analyte_flag, units)) %>% # cast to wide
    mutate(across(contains("flag"), # convert all _flag values back to text (< or blank)
                  ~ if_else(.<1, "", "<"))) %>%
    mutate(across(starts_with(c("value", "analyte", "units")), # convert NaN values to NA
                  ~ ifelse(is.nan(.), NA, .))) 

  # rename functions to rename flag, units, and value columns
  flagnamer <- function(data) {str_c(str_remove(data, "analyte_flag_"), "_flag")}
  unitnamer <- function(data) {str_c(str_remove(data, "units_"), "_units")}
  valunamer <- function(data) {str_remove(data, "value_")}

  # apply rename functions to column names, then reorder columns
  g <- f %>%
    rename_with(flagnamer, .cols = contains("flag")) %>%
    rename_with(unitnamer, .cols = contains("units")) %>%
    rename_with(valunamer, .cols = contains("value")) %>%
    # mutate(duplicate = case_when(
    #   duplicate == 2 ~ "duplicate",
    #   duplicate == 1 ~ "not a duplicate",
    #   TRUE ~ "neither")) %>%
    select(order(colnames(.))) %>% # alphabetize column names
    select(lake_id, site_id, sample_depth, sample_type, nutrients_qual, everything()) %>%
    ungroup() %>%
    select(-rep)
    
  return(g)

  
}


cin.awberc.path <- paste0(userPath, 
                       "data/chemistry/nutrients/")
  
chem21 <- get_awberc_data(cin.awberc.path, 
                          "2021_ESF-EFWS_NutrientData_Updated11212021_AKB.xlsx", 
                          "2021 Data") 

chem21 %>% distinct(lake_id)


chem21 <- dup_agg(chem21) # final object, cast to wide with dups aggregated


# This data object contains laboratory dups that must be aggregated.  These
# data can be identified as having identical values for all fields except
# finalConc.  They have different values for the 'REP" field in the original data.
# This aggregation can be accomplished by grouping and summarizing.  I suspect
# you will want to write another function to do this.



