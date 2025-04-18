# READ DEPTH PROFILE DATA-----------


# 1. SURGE DATA FIRST----
# Create a list of file paths where the data are stored.  
labs <- c("ADA", "CIN", "DOE", "NAR", "R10", "RTP", "USGS", "PR")
paths <- paste0(userPath,  "data/", labs)


# Function for reading depth profile for SuRGE sites except Oahe (069) and Francis-Case 
# (070) lacustrine

get_depth_profile <- function(paths){
  #d <-  
    fs::dir_ls(path = paths, # see above
               regexp = 'DepthProfile', # file names containing this pattern
               ignore.case = TRUE, # DepthProfile or depthProfile, Depthprofile, etc
               recurse = TRUE) %>% # look in all subdirectories
    .[!grepl(c(".pdf|.docx|2016"), .)] %>% # remove pdf and .docx review files & 2016 profiles
    .[!grepl(c("Falls"), .)] %>% # Falls Lake worked up in fallsLakeCH4.proj
    #.[54] %>%
    # map will read each file in fs_path list generated above
    # imap passes the element name (here, the filename) to the function
    # purrr::imap(~excel_sheets(.x)) 
    
    purrr::imap(~read_excel(.x, sheet = "data", 
                            na = c("NA", "", "N/A", "n/a")) %>%
                  # Assign the filename to the visit column for now
                  mutate(visit = .y)) %>% # assign file name
    # remove empty dataframes.  Pegasus put empty Excel files in each lake
    # folder at beginning of season.  These files will be populated eventually,
    # but are causing issues with code below
    purrr::discard(~ nrow(.x) == 0) %>% 
    # format data
    map(., function(x) { 
      janitor::clean_names(x) %>%
        
        # janitor converts pH to p_h. fix here
        rename_with(~gsub("p_h", "ph", .), #specify sonde
                    contains("p_h")) %>%
        
        # clean chlorophyll names
        rename(chla_sonde = chl_a_ug_l) %>% # specify sonde
        rename(chla_sonde_flag = chla_flag) %>%
        rename(chla_sonde_comment = chla_comment) %>%
        
        # clean phycocyanin names. `any_of` won't throw error if column missing
        rename(any_of(c(phycocyanin_sonde = "phyc_ug_l",
                        phycocyanin_sonde_flag = "phyc_flag",
                        phycocyanin_sonde_comment = "phyc_comment"))) %>%
        
        # fix sp_cond
        rename(sp_cond_flag = cond_flag) %>%
        rename(sp_cond_comment = cond_comment) %>%
        
        # remove units from analyte names. Create units columns below
        rename(temp = temp_c) %>% #_c pattern below picks up _comment
        rename_with(~stringi::stri_replace_all_regex(.,
                                                     pattern = c("_m", "_mg_l", "_us_cm", "_ntu"),
                                                     replacement = ""), # remove units from name
                    matches(c("_m|_mg_l|_us_cm|_ntu"))) %>% # columns to apply function to
        # Assign value to visit based on the Excel file name
        mutate(visit = if_else(str_detect(visit, "visit2"),
                               2, 1, missing = 1), 
               # format lake_id and site_id.  See Wiki
               lake_id = as.character(lake_id) %>%
                 tolower(.) %>% # i.e. Lacustrine -> lacustrine
                 str_remove(., "ch4_") %>% # remove any ch4_ from lake_id
                 str_remove(., "^0+"), #remove leading zeroes i.e. 078->78
               site_id = as.numeric(gsub(".*?([0-9]+).*", "\\1", site_id)),
               # address data-class conflicts; make classes identical
               across(contains("comment"), ~ as.character(.)),
               across(contains("flag"), ~ as.character(.)),
               across(contains("depth"), ~round(.x, 1)), # round to nearest tenth of meter
               # create units column for each analyte
               across(c("sample_depth", "temp", "do", "sp_cond", "chla_sonde"), 
                             ~ case_when(
                               str_detect(paste(cur_column()), "depth") ~ "m",
                               str_detect(paste(cur_column()), "temp") ~ "c",
                               str_detect(paste(cur_column()), "do") ~ "mg_l",
                               str_detect(paste(cur_column()), "sp_cond") ~ "us_cm",
                               str_detect(paste(cur_column()), "chl") ~ "ug_l",
                               TRUE ~ "FLY UOU FOOLS"), # 
                             .names = "{col}_units"),
               phycocyanin_units = "ug_l" # create column even if wasnt' measured.
               ) 
    }) %>%
    map_dfr(., bind_rows) # rbinds into one df
}

# Get data
depth_profile_surge <- get_depth_profile(paths) 

# 2. OAHE (069) AND FRANCIS-CASE (070) LACUSTRINE DEPTH PROFILES-----
# data collected by USACE at same time we were sampling! Data delivered by
# John Hargrave on 10/8/2024 and a copy of the data put in 69_lacustrine
# and 70_lacustrine folders. Could read from either, arbitrarily chose
# 70 here.

depth_profile_69_70 <- read_excel(path = paste0(userPath, 
                                                # file contains both 69 and 70 data
                                                "data/CIN/CH4_070_francis_case_lacustrine/dataSheets/OAHFTR_LakeProfiles_2021_Jake.xlsx"),
                                  sheet = "data", skip = 9) %>%
  # fix names
  janitor::clean_names() %>%
  rename(ph = units_00400) %>%
  rename(sample_depth = depth_m) %>%
  rename(temp = deg_c_00010) %>%
  rename(do = mg_l_00299) %>%
  rename(sp_cond = umho_cm_00094) %>%
  rename(turbidity = ntu_00078) %>%
  
  # create units column for each analyte
  mutate(across(c("sample_depth", "temp", "do", "sp_cond", "turbidity"), 
                ~ case_when(
                  str_detect(paste(cur_column()), "depth") ~ "m",
                  str_detect(paste(cur_column()), "temp") ~ "c",
                  str_detect(paste(cur_column()), "do") ~ "mg_l",
                  str_detect(paste(cur_column()), "sp_cond") ~ "us_cm",
                  str_detect(paste(cur_column()), "turb") ~ "ntu",
                  TRUE ~ "FLY UOU FOOLS"), # 
                .names = "{col}_units"),
         site_id = case_when(station == "FTRLK0911DW" ~ 6, # U-23 is closest, but using index site
                             station == "OAHLK1153DW" ~ 4, # U-04 is closest to USACE monitoring site and is index site
                             TRUE ~ NA_integer_),
         lake_id = case_when(station == "FTRLK0911DW" ~ "69_lacustrine", 
                             station == "OAHLK1153DW" ~ "70_lacustrine", 
                             TRUE ~ NA_character_),
         visit = 1,
         date_time = as.POSIXct(date_time, format = "%Y%m%d, %H%M"),
         date = as.Date(date_time)) %>%
  filter(lake_id %in% c("69_lacustrine", "70_lacustrine") & # lakes of interest
           date %in% c(as.Date("2021/06/23"), as.Date("2021/06/24"))) %>% # dates, these perfectly align with SuRGE sampling
  filter(sample_depth != 0) %>% # depth 0 is for calibration check
  select(-station, -contains("date"), -x3_01, -m_v_00090, -m_00000, -ft_00062, -m_00077)


# 3. 2016 DEPTH PROFILES----
# data curated in mulitResSurvey repo and written to SuRGE SharePoint
depth_profile_2016 <- read_csv(paste0(userPath, "data/CIN/2016_survey/depthProfiles2016.csv")) %>%
  janitor::clean_names(replace = c("\u00b5" = "u")) %>% # ensure mu (micro) is converted properly
  filter(!grepl(c("July|Aug|Oct"), lake_name)) %>% # omit Acton Lake repeat visits
  
  # janitor converts pH to p_h. fix here
  rename(ph = p_h) %>%
  rename(sample_depth = sample_depth_m) %>%
  rename(temp = temp_c) %>%
  rename(do = do_mg_l) %>%
  rename(sp_cond = sp_cond_us_cm) %>%
  rename(chla_sonde = chl_a_ug_l) %>%
  
  # create units column for each analyte
  mutate(across(c("sample_depth", "temp", "do", "sp_cond", "chla_sonde"), 
         ~ case_when(
           str_detect(paste(cur_column()), "depth") ~ "m",
           str_detect(paste(cur_column()), "temp") ~ "c",
           str_detect(paste(cur_column()), "do") ~ "mg_l",
           str_detect(paste(cur_column()), "sp_cond") ~ "us_cm",
           str_detect(paste(cur_column()), "chl") ~ "ug_l",
           TRUE ~ "FLY UOU FOOLS"), # 
         .names = "{col}_units"),
         visit = 1,
         site_id = as.numeric(gsub(".*?([0-9]+).*", "\\1", site_id))) %>%
  # bring in SuRGE lake_id
  left_join(lake.list.2016 %>% select(lake_id, eval_status_code_comment), 
            by = c("lake_name" = "eval_status_code_comment")) %>%
  select(-lake_name, -sample_depth_ft, -orp_m_v)

  
 
# 4. FALLS LAKE DEPTH PROFILES----
depth_profile_falls <- read_csv(paste0(userPath, "data/RTP/CH4_1033_Falls_Lake/falls_lake_depth_profiles.csv"))



# 5. MERGE DEPTH PROFILES----
depth_profiles_all <- map(list(depth_profile_surge, depth_profile_2016, depth_profile_falls, depth_profile_69_70),
                          ~.x %>% mutate(lake_id = as.character(lake_id))) %>% # 69_lacustrine, etc
  map_dfr(., bind_rows) %>% # rbinds into one df
  relocate(lake_id, site_id, visit, sample_depth, sample_date)


# 6. GET SAMPLE DATES-----
depth_profile_dates <- read_csv("SuRGE_Sharepoint/data/depth_profile_dates.csv") %>%
  mutate(sample_date = as.Date(observation_date, format = "%m/%d/%Y")) %>%
  select(-observation_date)

 
# 7. MERGE SAMPLE DATES WITH DEPTH PROFILES
# depth_profiles_all contains a sample_date column from the 2016 data, but
# it contains NA for all other lakes. sample_dates for other lakes are in
# depth_profile_dates. use dplry::rows_update to replace NA with sample dates
# for all non 2016 lakes
depth_profiles_all <- depth_profiles_all %>%
  rows_update(., depth_profile_dates, by = c("lake_id", "site_id", "visit"))
