# NUTRIENT ANALYSIS ON LACHAT CONDUCTED IN AWBERC

# Original data can be found at: L:\Priv\Cin\ORD\Pegasus-ESF\Lachat Data\Nutrients

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

# data from the following lakes has been reviewed as of 9/6/2022:
# 233, 237, 236, 144, 155, 275, 240, 069_lacustrine, 069_transitional, 070_lacustrine,
# 070_riverine, 288, 316, 79, 298, 75, 326, 327, 70_transitional, 231, 78, 232,
# 68, 69_riverine, 16, 82


# function reads in data, filters data, and creates all new columns
get_awberc_data <- function(path, data, sheet) { 
  
  d <- read_excel(paste0(path, data), #
                  sheet = sheet, skip = 1) %>%  # guess_max = 10000
  # d <- read_excel(paste0(cin.awberc.path,
  #                        "2021_ESF-EFWS_NutrientData_Updated01272022_AKB.xlsx"), #
  #                 sheet = "2021 Data", skip = 1) %>%
  # d <- read_excel(paste0(cin.awberc.path,
  #                        "2022_ESF-EFWS_NutrientData_Updated08302022_AKB.xlsx"), #
  #                 sheet = "2022 Data", skip = 1) %>%
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
      (as.Date(ddate) - as.Date(rdate)) > 28, 
      "H", NA_character_)) %>% # TRUE = hold time violation 
    mutate(visit = if_else(lake_id %in% c("281", "250") & 
                             between(rdate, 
                                     as.Date("2022-08-15"), 
                                     as.Date("2022-09-15")),
                           2, 1, missing = 1)) %>%
    mutate(finalConc = ifelse( # correct TP and TN are in tp_tn
      analyte %in% c("TP", "TN"),
      tp_tn,
      finalConc)) %>%  
    filter(sample_type != "SPK", # exclude matrix spike
           sample_type != "CHK") %>% # exclude standard check
    select(lake_id, site_id, crossid, sample_type, analyte, 
           finalConc, nutrients_qual, rep, visit) %>% # keep only needed fields
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
      grepl("dup", sample_type, ignore.case = TRUE) ~ "duplicate", # laboratory duplicate
      grepl("ukn", sample_type, ignore.case = TRUE) ~ "unknown",
      grepl("blk", sample_type, ignore.case = TRUE) ~ "blank", # field blank
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
      analyte == "nh4" & finalConc < 6 ~ "ND", 
      analyte == "no2" & finalConc < 6 ~ "ND",
      analyte == "no2_3" & finalConc < 6 ~ "ND",
      analyte == "op" & finalConc < 3 ~ "ND",
      analyte == "tn" & finalConc < 25 ~ "ND",
      analyte == "tp" & finalConc < 5 ~ "ND",
      TRUE ~ "")) %>%
    mutate(finalConc = case_when( # create the finalConc column
      analyte == "nh4" & finalConc < 6 ~ 6, 
      analyte == "no2" & finalConc < 6 ~ 6,
      analyte == "no2_3" & finalConc < 6 ~ 6,
      analyte == "op" & finalConc < 3 ~ 3,
      analyte == "tn" & finalConc < 25 ~ 25,
      analyte == "tp" & finalConc < 5 ~ 5,
      TRUE ~ finalConc)) %>%
    # observations above the MDL, but below the lowest non-zero standard are flagged "L"
    mutate(analyte_flag = case_when( # create L values in the analyte_flag column
      analyte == "nh4" & finalConc > 6 & finalConc < 20 ~ "L",
      analyte == "no2" & finalConc > 6 & finalConc < 20 ~ "L",
      analyte == "no2_3" & finalConc > 6 & finalConc < 20 ~ "L",
      analyte == "op" & finalConc > 3 & finalConc < 5 ~ "L",
      analyte == "tn" & finalConc > 25 & finalConc < 30 ~ "L",
      analyte == "tp" & finalConc > 5 & finalConc < 5 ~ "L", # the MDL and lowest standard is 5
      TRUE ~ analyte_flag)) %>%
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
    mutate(rep = as.character(rep), # 2022 data should all be numeric, but convert to chr for consistency with 2021 and code below
           rep = case_when(
      rep == "A" ~ "1", # convert to number for dup_agg function
      rep == "B" ~ "2",
      TRUE ~ rep)) %>%
    select(-crossid)  %>% # no longer need crossid
    mutate(site_id = as.numeric(site_id)) # convert to numeric to match other chem data 
  
    
  return(d)
  
  
}


# function aggregates lab dups, renames/sorts columns, and casts to wide
dup_agg <- function(data) { 

  # aggregate dups and convert analyte_flags to numeric (for summarize operations)
  
  e <- data %>%
    mutate(analyte_flag = case_when(
      analyte_flag ==  "ND" ~ 0,
      analyte_flag == "L" ~ 100,
      analyte_flag ==  "" ~ NA_real_)) %>% # convert to numeric
    group_by(lake_id, site_id, analyte, sample_depth, 
             sample_type, rep, nutrients_qual, units, visit) %>%
    summarize(value = mean(finalConc, na.rm = TRUE), # group and calculate means
              analyte_flag = mean(analyte_flag, na.rm = TRUE)) %>% # any group with NA will return NA
    mutate(sample_type = case_when(
      rep %in% c("B", 2) ~ "duplicate", TRUE ~ sample_type)) %>%
    select(-rep)

  # cast to wide, convert analyte flags to text, and convert any NaN to NA
  f <- e %>%
    pivot_wider(names_from = analyte, values_from = c(value, analyte_flag, units, nutrients_qual)) %>% # cast to wide
    mutate(across(contains("flag"), # convert all _flag values back to text (< or blank)
                  ~ case_when(. == 0 ~ "ND",
                              . > 0 ~ "L",
                              is.na(.) ~ NA_character_,
                              is.nan(.) ~ NA_character_))) %>%
    mutate(across(starts_with(c("value", "analyte", "units", "nutrients")), # convert NaN values to NA
                  ~ ifelse(is.nan(.), NA, .))) 

  # rename functions to rename flag, units, and value columns
  flagnamer <- function(data) {str_c(str_remove(data, "analyte_flag_"), "_flag")}
  unitnamer <- function(data) {str_c(str_remove(data, "units_"), "_units")}
  qualnamer <- function(data) {str_c(str_remove(data, "nutrients_qual_"), "_qual")}
  valunamer <- function(data) {str_remove(data, "value_")}

  # apply rename functions to column names, then reorder columns
  g <- f %>%
    rename_with(flagnamer, .cols = contains("flag")) %>%
    rename_with(unitnamer, .cols = contains("units")) %>%
    rename_with(qualnamer, .cols = contains("qual")) %>% 
    rename_with(valunamer, .cols = contains("value")) %>%
    # mutate(duplicate = case_when(
    #   duplicate == 2 ~ "duplicate",
    #   duplicate == 1 ~ "not a duplicate",
    #   TRUE ~ "neither")) %>%
    select(order(colnames(.))) %>% # alphabetize column names
    select(lake_id, site_id, sample_depth, sample_type, everything()) %>%
    ungroup() %>%
    select(-rep)
    
  return(g)

  
}

flag_agg <- function(data) { # merge the flag columns for each analyte
  
  h <- data %>%
    # na.rm = TRUE, else NA gets converted to character and is included in new column
    unite("nh4_flags", nh4_flag, nh4_qual, sep = " ", na.rm = TRUE) %>% 
    unite("no2_flags", no2_flag, no2_qual, sep = " ", na.rm = TRUE) %>%
    unite("no2_3_flags", no2_3_flag, no2_3_qual, sep = " ", na.rm = TRUE) %>%
    unite("op_flags", op_flag, op_qual, sep = " ", na.rm = TRUE) %>%
    unite("tn_flags", tn_flag, tn_qual, sep = " ", na.rm = TRUE) %>%
    unite("tp_flags", tp_flag, tp_qual, sep = " ", na.rm = TRUE) %>%
    # In cases were _flag and _qual were NA, the above returns "".  Convert to NA
    mutate(across(contains("flags"), # 
                  ~if_else(. == "", NA_character_, .)))
    
    return(h)
  
  
}

# READ DATA-----------

cin.awberc.path <- paste0(userPath, 
                       "data/chemistry/nutrients/")
  
chem21 <- get_awberc_data(cin.awberc.path, 
                          "2021_ESF-EFWS_NutrientData_Updated01272022_AKB.xlsx", 
                          "2021 Data") 

chem22 <- get_awberc_data(cin.awberc.path, 
                          "2022_ESF-EFWS_NutrientData_Updated08302022_AKB.xlsx", 
                          "2022 Data") 

chemCinNutrients <- bind_rows(chem21, chem22) %>%
  dup_agg(.) %>% # final object, cast to wide with dups aggregated
  flag_agg(.)



# Sample inventory----------------------
# Are all collected samples included?

# Samples collected
chem21.inventory.expected <- chem.samples.foo %>% # see chemSampleList.R, [6/17/22] no 2022 lakes included yet
  filter(lab != "R10", # see readNutrientsR10_2018.R for 2018 R10 samples
         # See readNutrientsAda.R for R10 2020 nutrient data
         lab != "ADA", # Ada analyzed their own. readNutrientsAda.R  
         analyte_group == "nutrients", 
         sample_year >= 2021) %>% # earlier samples handled in other scripts
  select(-sample_year, -lab, -analyte_group)
  
  
# Samples analyzed  
chem21.inventory.analyzed <- chem21 %>% select(-site_id, -matches(c("flag|qual|units"))) %>%
  pivot_longer(!c(lake_id, sample_depth, sample_type), names_to = "analyte") %>%
  select(-value)

# all analyzed samples in collected list?
# yes
setdiff(chem21.inventory.analyzed, chem21.inventory.expected) %>% print(n=Inf)


# all collected samples in analyzed list?
setdiff(chem21.inventory.expected, chem21.inventory.analyzed) %>% 
  arrange(analyte, lake_id) %>% print(n=Inf)
# all accounted for!


