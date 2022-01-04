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
# and use the analyte names defined in github issue #10.
# SCRIPT TO READ IN WATER CHEM

get_awberc_data <- function(path, data, sheet) { 
  
  d <- read_excel(paste0(path, data), #
                  sheet = sheet, range = "A2:AZ12000") %>%
  # d <- read_excel(paste0(cin.awberc.path,
  #                        "2021_ESF-EFWS_NutrientData_Updated11082021_AKB.xlsx"), #
  #                 sheet = "2021 Data", range = "A2:AZ12000") %>%
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
           sample_type = type) %>%
    mutate(nutrients_qual = if_else( # determine if holding time exceeded
      (as.Date(ddate) - as.Date(rdate)) > 28, TRUE, FALSE)) %>% # TRUE = hold time violation
    mutate(finalConc = ifelse( # correct TP and TN are in tp_tn
      analyte %in% c("TP", "TN"),
      tp_tn,
      finalConc)) %>%  
    filter(sample_type != "SPK") %>% # exclude matrix spike
    select(lake_id, site_id, crossid, sample_type, analyte, finalConc, nutrients_qual) %>% # keep only needed fields
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
    # strip out unneeded analyses
    # exclude filtered samples run for totals
    filter(!(analyte %in% c("tp", "tn") & filter == "filtered")) %>%
    # exclude unfiltered samples run for inorganics
    filter(!(analyte %in% c("nh4", "op", "no2_3", "no2") & filter == "unfiltered")) %>%
    filter(!(analyte == "turea")) %>% # exclude urea
    filter(grepl(pattern = c("CH4|069|070"), lake_id)) %>% # Filter SuRGE data
    mutate(lake_id = str_remove(lake_id, "CH4-")) %>% # standardize lake IDs
    mutate(lake_id = case_when( # # standardize lake ID sub-component names
      str_detect(lake_id, "LAC") ~ str_replace(lake_id, " LAC", "_lacustrine"),
      str_detect(lake_id, "River") ~ str_replace(lake_id, " River", "_riverine"),
      str_detect(lake_id, "Trans") ~ str_replace(lake_id, " Trans", "_transitional"),
      TRUE ~ lake_id)) %>%
    mutate(finalConc = as.numeric(finalConc)) %>% # make analyte values numeric
    mutate(analyte_flag = case_when( # create the analyte_flag column
      analyte == "nh4" & finalConc < 7 ~ "<", # 1000 is a placeholder value
      analyte == "no2" & finalConc < 8 ~ "<",
      analyte == "no2_3" & finalConc < 8 ~ "<",
      analyte == "op" & finalConc < 0.7 ~ "<",
      analyte == "tn" & finalConc < 8 ~ "<",
      analyte == "tp" & finalConc < 8 ~ "<",
      TRUE ~ "")) %>%
    mutate(units = case_when(
      analyte == "nh4"  ~ "ug_n_l",
      analyte == "no2"  ~ "ug_n_l",
      analyte == "no2_3"  ~ "ug_n_l",
      analyte == "op"  ~ "ug_p_l",
      analyte == "tn"  ~ "ug_n_l",
      analyte == "tp"  ~ "ug_p_l",
      TRUE ~ "")) %>%
  select(-crossid) # no longer need crossid
    
  return(d)
  
  
}


lake_agg <- function(data) { 
  # 1/4/2022 problem with pivot_wider--there appear to be multiple values for some analytes? 
  # 1/4/2022 also need to implement duplicate aggregation
e <- data %>%
  mutate(lake_id = str_extract(lake_id, "\\d+")) %>% # strip suffixes from lake_ids
  group_by(lake_id, site_id, analyte, sample_type, sample_depth, nutrients_qual, analyte_flag, units) %>% 
  summarize(value = mean(finalConc, na.rm = TRUE)) %>% # group lakes together and calculate means
  pivot_wider(names_from = analyte, values_from = c(value, analyte_flag, units)) # %>% # cast to wide
  # mutate(across(starts_with(c("value", "analyte", "units")), # convert NULL values to NA
  #               ~ ifelse(is.null(.), NA, .))) # 1/4/2022 this isn't working--revisit
  #                                             # 1/4/2022 maybe because some values are lists?
return(e)


}


cin.awberc.path <- paste0(userPath, 
                       "data/chemistry/nutrients/")
  
chem21 <- get_awberc_data(cin.awberc.path, 
                          "2021_ESF-EFWS_NutrientData_Updated11082021_AKB.xlsx", 
                          "2021 Data") 

zz <- lake_agg(chem21) # temporary object so I can compare results

# This data object contains laboratory dups that must be aggregated.  These
# data can be identified as having identical values for all fields except
# finalConc.  They have different values for the 'REP" field in the original data.
# This aggregation can be accomplished by grouping and summarizing.  I suspect
# you will want to write another function to do this.



