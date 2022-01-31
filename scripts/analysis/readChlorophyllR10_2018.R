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

# Function to read-in volume filtered data
get_volume_data <- function(path, data, sheet) { 
  
  d <- read_excel(paste0(path, data), #
                       sheet = sheet) %>%
    janitor::clean_names() %>% # clean up names 
    mutate(collection_date = as.Date(collection_date, format = "%m.%d.%Y")) %>%
    filter(collection_date >= as.Date("2018-01-01") & 
             collection_date <= as.Date("2018-12-31")) # get 2018 only
  
  return(d)
  
}

# Function to read-in chlorophyll results data
get_results_data <- function(path, data, sheet) { 
  
  e <- read_csv(paste0(path, data)) %>% #
    janitor::clean_names() %>% # clean up names 
    mutate(collection_date = as.Date(collection_date, format = "%m/%d/%Y")) %>%
    mutate(analysis_date = as.Date(analysis_date, format = "%m/%d/%Y")) %>%
    mutate(extraction_date = as.Date(extraction_date, format = "%m/%d/%Y")) %>%
    filter(collection_date >= as.Date("2018-01-01") & 
             collection_date <= as.Date("2018-12-31")) %>% # get 2018 only
    rename(sample_type = type) %>%
    mutate(sample_type = case_when( # We'll probably want different names(???)
      sample_type == "Blank" ~ "blank",
      sample_type == "Control" ~ "control",
      sample_type == "Sample" ~ "sample",
      TRUE   ~ sample_type)) %>%
    select(lake_id, site_id, sample_type,
           collection_date, analysis_date, chl_a_jh)
  
  
  # TO DO: convert lake_id and site_id values (see Wiki page)
  # column R: chl_a_jh    column L: extract_volume_l
  # calculate hold time violations and chla_qual column:
    # create chla_qual column. There are 2 holding times:
    # holdtime 1: collection to extraction (filter_hold_time) >60 = violation
    # holdtime 2: extraction to analysis (extract_hold_time) >330 = violation.
    # the chla_qual = 1 if either hold times are violated, otherwise = "".
  # calculate mdl values and chla_flag column:
    # chla_flag = "<" for chla levels < 9ug/L (from extract)
    # Extract concentration = (column R * 5)/column K
  # create chla_units column?
  
  return(e)
  
}


# chlorophyll filter volumes data
cin.chl.volume.path <- paste0(userPath, 
                          "data/algalIndicators/pigments/")

chloro18.volume <- get_volume_data(cin.chl.volume.path, 
                          "surgeFilteredVolumes.xlsx", 
                          "data") 

# chlorophyll results
cin.chl.results.path <- paste0(userPath, 
                          "data/algalIndicators/pigments/R10_2018_chl/")

chloro18.results <- get_results_data(cin.chl.results.path, 
                          "chl_sample_log.csv")

# join data and calculate chlorophyll a
chloro18 <- left_join(chloro18.results, chloro18.volume, by = "????") %>%
  mutate(chlorophyll_a = chl_a_jh / volume_filtered) # calculate ug_l
