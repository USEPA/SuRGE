# Script for reading chlorophyll samples collected by Region 10 in 2018.
# Samples distributed between analysis at AWBERC and a contract lab

# R10 REM CHLOROPHYLL SAMPLES ANALYZED AT AWBERC-----------------------------  

# Chain of Custody forms can be found at: 
# "L:\Priv\Cin\NRMRL\ReservoirEbullitionStudy\ebullition2017\projectDocuments\coc"
# Samples from SFR, MMR, LGR, PLR, and LVR were extracted and analyzed at AWBERC.
# The results can be found at: "L:\Priv\Cin\NRMRL\Chlorophyll\Results\chl_sample_log.csv"
# A copy of this file was moved to: 
# "...Environmental Protection Agency (EPA)\Herger, Lillian - REM_2018\data\chlorophyll\chl_sample_log.csv"
# AND
# "...\SuRGE Survey of Reservoir Greenhouse gas Emissions - Documents\data\algalIndicators\pigments\R10_2018_chl\chl_sample_log.csv"

# Please see project QAPP for Sample_ID conventions:
# "...\SuRGE Survey of Reservoir Greenhouse gas Emissions - Documents\projectDocuments\
#     QAPP, SOPs, manuals, HASP\
#     QAPP for an assessment of methane emissions from Region 10 reservoirs.docx"

# Please see Wiki page https://github.com/USEPA/SuRGE/wiki/Region-10-2018-sampling
# for crosswalk between 2018 lake_id values and SuRGE lake_id values.  Only read in
# data for sites specified in Wiki.

# Column R ("Chla_a_JH") contains the ug of chl a on the filter.  This value
# must be divided by the volume filtered to calculate chla in ug_l.  Volume filtered
# can be found at: "...\SuRGE Survey of Reservoir Greenhouse gas Emissions - Documents\
#                  data\algalIndicators\pigments\surgeFilteredVolumes.xlsx"

# the chla_qual column should reflect holding time violations.  Two separate holding
# times are assessed.  The first reflects the time from when the sample was collected
# to when it was extracted.  This quantity is the "Filter_Hold_Time" and is given 
# in column x.  A value <=60 is good, holding time >60 is a violation.  The second
# holding time is the "Extract_Hold-Time" (column Z) and reflects how long the
# extract was stored prior to analysis.  A value <= 330 is good, >330 is a violation.
# the chla_qual value should be 1 if either hold times are violated, otherwise "".

# The chla_flag column will have a value of "<" for chla levels below the detection
# limit of 9ug/L measured in the extract.  Extract concentration is calculated
# as (column R * 5)/column K.  Any sample for which this value is < 9 should be
# flagged.



# Function to read-in volume filtered data
get_volume_data <- function(path, data, sheet) { 
  
  d <- read_excel(paste0(path, data), #
                  sheet = sheet) %>%
    janitor::clean_names() %>% # clean up names 
    mutate(collection_date = as.Date(collection_date, format = "%m.%d.%Y")) %>%
    filter(collection_date >= as.Date("2018-01-01") & 
             collection_date <= as.Date("2018-12-31")) %>% # get 2018 only
    # pull out numeric component of site_id
    mutate(site_id = as.numeric(gsub(".*?([0-9]+).*", "\\1", site_id))) %>%
    mutate(sample_type = case_when( # convert sample_type to SuRGE format
      sample_type == "BLK" ~ "blank",
      sample_type == "DUP" ~ "duplicate",
      sample_type == "UKN" ~ "unknown",
      sample_type == "UNK" ~ "unknown",
      TRUE   ~ sample_type)) %>%
    mutate(volume_filtered_l = volume_filtered / 1000) %>% # convert from ml to L
    select(lake_id, site_id, sample_type, volume_filtered_l)
  
  return(d)
  
}

# Function to read-in chlorophyll results data
get_results_data <- function(path, data, sheet) { 
  
  e <- read_csv(paste0(path, data)) %>% #
    janitor::clean_names() %>% # clean up names 
    # convert all dates to same format and data type
    mutate(collection_date = as.Date(collection_date, format = "%m/%d/%Y")) %>%
    mutate(analysis_date = as.Date(analysis_date, format = "%m/%d/%Y")) %>%
    mutate(extraction_date = as.Date(extraction_date, format = "%m/%d/%Y")) %>%
    filter(collection_date >= as.Date("2018-01-01") & 
             collection_date <= as.Date("2018-12-31")) %>% # get 2018 only
    filter(project == "AEBRR") %>% # samples analyzed at AWBERC only
    mutate(lake_id = str_extract(sample_id, "SFR|MMR|LGR|PLR|LVR")) %>% # get lake_id
    mutate(lake_id = case_when( # convert lake_id to SuRGE format
      lake_id == "SFR" ~ "331",
      lake_id == "MMR" ~ "323",
      lake_id == "LGR" ~ "302",
      lake_id == "PLR" ~ "239",
      lake_id == "LVR" ~ "253",
      TRUE   ~ lake_id)) %>%
    filter(is.na(lake_id) == FALSE) %>% # keep only SuRGE lakes
    mutate(site_id = str_extract(sample_id, # create site_id   
                                 "U02|U22|U35|U10|U31|U05|U08|U33|U04|U10|U09|SU05|SU03|SU31|SU34"), # R10 2018 site_id values
           site_id = as.numeric(gsub(".*?([0-9]+).*", "\\1", site_id))) %>% # convert to numeric
    mutate(sample_type = str_sub(sample_id, -3)) %>% # get sample_type
    mutate(sample_type = case_when( # convert sample_type to SuRGE format
      sample_type == "BLK" ~ "blank",
      sample_type == "DUP" ~ "duplicate",
      sample_type == "UKN" ~ "unknown",
      sample_type == "UNK" ~ "unknown",
      TRUE   ~ sample_type)) %>%
    rename(chla = chl_a_jh) %>% # see Wiki for naming conventions
    mutate(sample_depth = case_when(sample_type == "blank" ~ "blank",
                                    TRUE ~ "shallow")) %>% # all samples were collected near a-w interface
    mutate(extract_conc = (5 * chla) / extract_volume_l) %>% # extracted concentration
    mutate(filter_hold_time = collection_date - extraction_date) %>% # get hold time
    mutate(extract_hold_time = analysis_date - extraction_date) %>% # get hold time
    mutate(chla_qual = case_when( # qual flag if either hold time exceeded
      filter_hold_time > 60 ~ "H",
      extract_hold_time > 300 ~ "H",
      TRUE   ~ "")) %>%
    mutate(chla_flag = case_when( # flag if conc is below detection limit 
      extract_conc < 9 ~ "ND",
      TRUE   ~ "")) %>%
    select(lake_id, site_id, sample_type, sample_depth, chla, extract_conc, 
           chla_flag, chla_qual) %>%
    unite("chla_flags", c(chla_flag, chla_qual), sep = " ", na.rm = TRUE) %>%
    mutate(chla_flags = if_else( # NA if there are no flags
      str_detect(chla_flags, "\\w"), chla_flags, NA_character_) %>%
        str_squish()) # remove any extra white spaces
  
  return(e)
  
}


# chlorophyll filter volumes data
cin.chl.volume.path <- paste0(userPath, 
                              "data/algalIndicators/pigments/")

chl18.volume <- get_volume_data(cin.chl.volume.path, 
                                "surgeFilteredVolumes.xlsx", 
                                "data") 

# chlorophyll results
cin.chl.results.path <- paste0(userPath, 
                               "data/algalIndicators/pigments/R10_2018_chl/")

chl18.results <- get_results_data(cin.chl.results.path, 
                                  "chl_sample_log.csv")

# join data to calculate volume of chlorophyll-a in ug/l
dim(chl18.results) # 18 observations (subset of 2018 samples run at AWBERC)
dim(chl18.volume) # 30 observations (all 2018 R10 samples)
chl18.awberc <- left_join(chl18.results, chl18.volume, by = c("lake_id", "site_id", "sample_type")) %>%
  mutate(chla = chla / volume_filtered_l) %>% # calculate chl-a in ug_l
  mutate(chla_units = "ug_l") %>% # add units column for chl-a
  select(-extract_conc, -volume_filtered_l) # no longer needed

dim(chl18.awberc) #18, all chla data retained



# SAMPLES ANALYZED AT BSA LABORATORIES-----------------------
# Samples collected from SFP, WPL, and BKL were extracted, but not run.  They were 
# driven to BSA laboratory by Dana Macke in September 2020.  I 
# provided filtered volumes for these samples and BSA reported the results in
# units of mg chla/m3.  Data are at:
#"...\SuRGE Survey of Reservoir Greenhouse gas Emissions - Documents\
# data\algalIndicators\pigments\R10_2018_chl\20201109 USEPA Chlorophyll Report 779.xlsx"
# OUTPUT-VOLUMETRIC worksheet

# We do not have the data to assess detection limit violations for these samples.
# Assume all values > MDL.  

# Filter hold time is not reported; assume a value of 60.  Extract hold time
# can be calculated as:
# "Analyzed_Date" (column E) - ["Sample Date" (column D) + 60 days]
# All of these samples will be flagged for holding time violation.



get_bsa_data <- function(path, data, sheet) { 
  
  f <- read_excel(paste0(path, data), #
                  sheet = sheet) %>%
    janitor::clean_names() %>% # clean up names 
    filter(study == "AEBRR") %>% # 
    rename(chla = chlorophyll_a_mg_m3) %>%
    mutate(sample_date = as.Date(sample_date, format = "%Y-%m-%d")) %>%
    mutate(analyzed_date = as.Date(analyzed_date, format = "%Y-%m-%d")) %>%
    mutate(extract_hold_time = analyzed_date - (sample_date + 60)) %>% # assume extraction occurred 60 days after collection
    mutate(lake_id = str_extract(location, "SFP|WPL|BKL")) %>% # get lake_id
    mutate(lake_id = case_when( # convert lake_id to SuRGE format
      lake_id == "SFP" ~ "263",
      lake_id == "WPL" ~ "308",
      lake_id == "BKL" ~ "999",
      TRUE   ~ lake_id)) %>%
    filter(is.na(lake_id) == FALSE) %>% # keep only SuRGE lakes
    mutate(site_id = str_extract(location, # create site_id
                                 "U01|U22|U35|U10|U31|U05|U06|U07|U08|U09|U12|U33|U04|U10|U09|SU05|SU03|SU31|SU34"), # R10 2018 site_id values
           site_id = as.numeric(gsub(".*?([0-9]+).*", "\\1", site_id))) %>%
    mutate(sample_type = str_extract(location, # get sample_type
                                     "BLK|DUP|UKN|UNK")) %>% 
    mutate(sample_type = case_when( # convert sample_type to SuRGE format
      sample_type == "BLK" ~ "blank",
      sample_type == "DUP" ~ "duplicate",
      sample_type == "UKN" ~ "unknown",
      sample_type == "UNK" ~ "unknown",
      TRUE   ~ sample_type)) %>%
    mutate(chla_qual = case_when( # qual flag if either hold time exceeded
      extract_hold_time > 330 ~ "H",
      TRUE   ~ "")) %>%
    mutate(sample_depth = case_when(sample_type == "blank" ~ "blank",
                                    TRUE ~ "shallow")) %>% # all samples were collected near a-w interface
    mutate(chla_flag = "") %>% # assume all are above mdl (data unavailable)
    mutate(chla_units = "ug_l") %>%
    select(lake_id, site_id, sample_type, sample_depth, chla, chla_flag, chla_qual, chla_units) %>%
    unite("chla_flags", c(chla_flag, chla_qual), sep = " ", na.rm = TRUE) %>%
    mutate(chla_flags = if_else( # NA if there are no flags
      str_detect(chla_flags, "\\w"), chla_flags, NA_character_) %>%
        str_squish()) # remove any extra white spaces

  return(f)

}


# read in chlorophyll results from BSA
chl.bsa.path <- paste0(userPath,
                       "data/algalIndicators/pigments/R10_2018_chl/")

chl18.bsa <- get_bsa_data(chl.bsa.path,
                          "20201109 USEPA Chlorophyll Report 779.xlsx",
                                  "OUTPUT - VOLUMETRIC")

# COMBINE AWBERC AND BSA DATA---------
# both have 7 identically formatted columns, good
dim(chl18.awberc)
dim(chl18.bsa)
chl18 <- rbind(chl18.awberc, chl18.bsa)



# SAMPLE INVENTORY------------------------
# REGION 10 chl samples collected, per master sample list (chemSampleList.R)
r10.chl.collected <- chem.samples.foo %>% 
  filter(lab == "R10", # R10
         sample_year == 2018,
         analyte == "chla") %>%
  select(lake_id, sample_type, sample_depth)


# R10 chl samples analyzed
r10.chl.analyzed <- chl18 %>% 
  select(lake_id, sample_type, sample_depth)

# Are all R10 2018 collected chl in R10 chl data?
# yes
setdiff(r10.chl.collected, r10.chl.analyzed) %>% print(n=Inf)

# Are all nutrient samples analyzed at ADA in list of samples sent to ADA?
# yes
setdiff(ada.oc.analyzed, ada.oc.collected) %>% print(n=Inf)





