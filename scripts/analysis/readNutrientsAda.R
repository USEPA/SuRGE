# NUTRIENTS ANALYZED IN ADA GENERAL PARAMETERS LABORATORY

# FIRST SECTION READS IN NUTRIENT DATA FROM SAMPLES COLLECTED BY ADA IN 2021-------
# AND ANALYZED IN ADA LABORATORY.  FORMAT OF 2021 DATA REPORTS ARE UNIQUE TO
# 2021

# Function to extract data from Ada excel files
get_ada_data21 <- function(path, datasheet) { 
 
   # for 2020 data (and 2022 onward), as field_sample_id is in different format
   
  # toptable contains analyte names and MDL values
   toptable <- read_excel(paste0(path, datasheet), # get MDL & analyte names
                          sheet = "Data", range = "C8:N50", col_names = FALSE) %>% # up to 492 rows
      row_to_names(row_number = 1, remove_row = FALSE) %>% # column names = first row of data
      clean_names(case = "upper_camel") %>% # keep case consistent
      filter(Analytes %in% c("Analytes", "Unit", "MDL"), ignore.case=TRUE) %>% # # remove unneeded rows
      pivot_longer(cols = -Analytes, names_to = "variable")  %>% # transpose the tibble
      pivot_wider(names_from = Analytes, values_from = value) %>% # transpose the tibble
      select(-variable) %>% # no longer needed
      janitor::remove_empty("rows") %>% # remove empty rows
      filter(str_detect(Analytes, "nalytes") == FALSE) %>% # if necessary, removes superfluous row
      mutate(Analytes = str_c(Analytes, Unit)) %>% # concatenate analyte and unit, so unit is retained
      column_to_rownames(var = "Analytes") %>% # simpler to work w/ rownames in next chunk of code
      mutate(MDL = as.numeric(MDL)) # covert MDL values to numeric

analyte_names <- row.names(toptable) # pass analyte names to maintable, below

   # maintable combines toptable with results
   maintable <- read_excel(paste0(path, datasheet), # get the results
                           sheet = "Data", range = "A14:N500") %>% # up to 492 rows
      janitor::remove_empty("rows") %>% # remove empty rows
      janitor::clean_names() %>%
      mutate(lab_sample_id = toupper(lab_sample_id)) %>% # make uppercase since Ada isn't consistent
      mutate(labdup = if_else(str_detect(lab_sample_id, "LAB DUP"), "LAB DUP", "")) %>% # flag the dups
      select(field_sample_id, labdup, starts_with("dat")) %>% # remove unneeded columns
      rename_with(~paste0(analyte_names), .cols = starts_with("data")) %>% # rename using analyte names
      rename_with(~paste0(analyte_names, "_date_analyzed"), .cols = starts_with("date_a")) %>% # rename using analyte names
      mutate(across(ends_with("analyzed"), # compute holding time of analytes
                    ~ as.numeric(as.Date(., format = "%m/%d/%Y") - as.Date(date_collected, format = "%m/%d/%Y")))) %>%
      rename(sampleid = field_sample_id) %>% # temporary rename; changes later during text parsing
      filter(str_starts(sampleid, "\\(")) %>% # retain only rows where sampleid starts with '('
      select(sampleid, labdup, everything(), -date_collected) %>% # reorder columns for the following mutate()
      mutate(across(ends_with("/L"), # create new flag column if analyte not detected
                    ~ if_else(str_detect(., "ND"), "ND", ""),
                    .names = "{col}_flag")) %>%
      mutate(across(ends_with("/L"), # replace ND with the MDL value from toptable
                    ~ ifelse(str_detect(., "ND"), toptable[paste(cur_column()),2], .))) %>% # note this is base::ifelse
      mutate(sampleid = str_replace_all(sampleid, "(TN or DN)","")) %>% # clean-up sampleid field
      mutate(sampleid = str_replace_all(sampleid, "\\(\\)","")) %>% # clean-up sampleid field
      mutate(sampleid = str_replace_all(sampleid, " ", "")) %>% # remove any blank spaces inside string
      mutate(across(!ends_with(c("flag", "labdup", "sampleid")), # remove 'BQL', 'RPD' & other junk from data fields
                    ~ str_extract(., pattern = "\\-*\\d+\\.*\\d*"))) %>%
      mutate(across(!ends_with(c("flag", "labdup", "sampleid")), # make extracted data numeric
                    ~ as.numeric(.))) %>%
      mutate(sample_depth = str_sub(sampleid, 4, 4)) %>% # get sample depth from sampleid
      mutate(sample_type = str_sub(sampleid, 5, 5)) %>% # get sample type from sampleid
      mutate(sampleid = str_sub(sampleid, 1, 3)) %>% # make sampleid 3-digit numeric lake id only
      mutate(sample_depth = str_replace_all(sample_depth, c("D" = "deep", "S" = "shallow", "N" = "blank"))) %>%
      mutate(sample_type = str_replace_all(sample_type, c("B" = "blank", "U" =  "unknown", "D" =  "duplicate"))) %>%
      rename(lake_id = sampleid) %>% # change name to match chemCoc
      mutate(lake_id = as.character(as.numeric(lake_id))) %>% # consistent format for lake_id
      mutate(across(ends_with("analyzed"), # replace no. days w/ "HOLD" if holding time violated
                    ~ ifelse(.>28, "H", ""))) %>%
      mutate(site_id = "") # create empty column for site_id (id is populated later

  return(maintable)

}


# Function to convert units, rename columns, and add units columns
conv_units <- function(data, filename) {
   
# Flow: Series of non-nested 'if' conditions that evaluate file names;
   # conditions are mutually exclusive due to Ada lab file name conventions.
   # If TRUE, proceeds to convert units, rename columns, & add units columns.
   # Then, if the object includes no3 data, create new column for no3 qual. 

# filename <- toupper(filename) # use if case becomes an issue in file names;
      # note that 'oP' (in code below) contains a lowercase letter. 

   # AMMONIUM, NITRATE, NITRITE
   if (str_detect(paste(filename), "NH4")) 
      f <- data %>%
         mutate(across(ends_with("/L"), 
                       ~ case_when(
                          str_detect(paste(cur_column()), "mg/") ~ .*1000, 
                          TRUE ~ .*1))) %>%
         rename(nh4 = contains("NH4") & !ends_with(c("flag", "analyzed")), 
                no2_3 = contains("NO3") & contains("NO2") & !ends_with(c("flag", "analyzed")),
                no3 = contains("NO3") & !contains("NO2") & !ends_with(c("flag", "analyzed")),
                no2 = contains("NO2") & !contains("NO3") & !ends_with(c("flag", "analyzed")), 
                nh4_flag = contains("NH4") & ends_with("flag"),
                no2_3_flag = contains("NO3") & contains("NO2") & ends_with("flag"),
                no3_flag = contains("NO3") & !contains("NO2") & ends_with("flag"),
                no2_flag = contains("NO2") & !contains("NO3") & ends_with("flag"), 
                nh4_qual = contains("NH4") & ends_with("analyzed"), 
                no2_3_qual = contains("NO3") & contains("NO2") & ends_with("analyzed"),
                no2_qual = contains("NO2") & !contains("NO3") & ends_with("analyzed")) %>%
         mutate(across(ends_with(c("nh4", "no2_3", "no3", "no2")), 
                       ~ "ug_n_l",
                       .names = "{col}_units")) 
         
    # TOTAL PHOSPHORUS, TOTAL NITROGEN
   if (str_detect(paste(filename), "TNTP|TN,TP"))
      f <- data %>%
         mutate(across(ends_with("/L"), 
                       ~ case_when(
                          str_detect(paste(cur_column()), "mg/") ~ .*1000, 
                          TRUE ~ .*1))) %>%
         rename(tn = contains("TN") & !ends_with(c("flag", "analyzed")),
                tp = contains("TP") & !ends_with(c("flag", "analyzed")),
                tn_flag = contains("TN") & ends_with("flag"),
                tp_flag = contains("TP") & ends_with("flag"), 
                tn_qual = contains("TN") & ends_with("analyzed"), 
                tp_qual = contains("TP") & ends_with("analyzed")) %>%
         mutate(across(ends_with(c("tn")), 
                       ~ "ug_n_l",
                       .names = "{col}_units")) %>%
         mutate(across(ends_with(c("tp")), 
                       ~ "ug_p_l",
                       .names = "{col}_units"))
   
    # ORTHOPHOSPHATE
   if (str_detect(paste(filename), "oP"))
      f <- data %>%
         mutate(across(ends_with("/L"), 
                       ~ case_when(
                          str_detect(paste(cur_column()), "mg/") ~ .*1000, 
                          TRUE ~ .*1))) %>%
         rename(op = contains("oP") & !ends_with(c("flag", "analyzed")),
                op_flag = contains("oP") & ends_with("flag"),
                op_qual = contains("oP") & ends_with("analyzed")) %>%
         mutate(across(ends_with("op"), 
                       ~ "ug_p_l",
                       .names = "{col}_units")) 
  
    # Check for an no3 column, then create no3_qual flag column
   if ("no3" %in% colnames(f) & "no2_qual" %in% colnames(f)) 
     f <- f %>% 
        mutate(no3_qual = case_when( # check if no2 has qual flag
           no2_qual == "H" ~ "H",
           TRUE ~ ""))
   
   if ("no3" %in% colnames(f) & "no2_3_qual" %in% colnames(f)) 
     f <- f %>% 
       mutate(no3_qual = case_when( # check if no2_3 has qual flag
         no2_3_qual == "H" ~ "H",
         TRUE ~ ""))

   # select and order columns
  f <- f %>% 
     select(order(colnames(.))) %>% # alphabetize and reorder columns
     select(lake_id, site_id, sample_depth, sample_type, labdup, everything())
    
  return(f)
   
}


# Function to aggregate the lab duplicates
dup_agg21 <- function(data) {
   
   # first, convert all _flag columns to a numeric for summarize operations;
   # Must be performed in the event that a lab dup has a different flag than the sample.
   
   data <- data %>% 
      mutate(across(ends_with("flag"), 
                    ~ ifelse(str_detect(., "ND"), 1, 0))) 
   
   # carve out the _flag and _units columns so they can be re-joined later
   c <- data %>% select(ends_with(c("id","labdup", "type", "depth", "filter", "units", "qual")))
           
   d <- data %>%
      dplyr::group_by(sample_depth, sample_type) %>%
      # '!' to exclude columns we don't want to aggregate
      summarize(across(!ends_with(c("id","labdup", "type", "depth", "filter", "units", "qual")), 
                       ~ mean(., na.rm = TRUE))) 
   
   e <- left_join(d, c, by = c("sample_depth", "sample_type")) %>% # rejoin the data
      mutate(across(3:last_col(), # convert NaN to NA
                    ~ ifelse(is.nan(.), NA, .))) %>% # must use ifelse here (not if_else)
      select(order(colnames(.))) %>% # alphabetize column names
      select(lake_id, site_id, sample_depth, sample_type, sample_filter, labdup, everything()) %>% # put 'sampleid' first
      mutate(across(ends_with("flag"), # convert all _flag values back to text
                    ~ if_else(.<1, "", "ND"))) %>% 
      filter(labdup != "LAB DUP") %>% # remove the lab dup
      select(-labdup) # remove labdup column. JB 12/7/2021
   
   return(e)
   
}

flag_agg <- function(data) { # merge the flag columns for each analyte
  
  f <- data
  
  if ("nh4" %in% colnames(data))
    f <- f %>%
      unite("nh4_flags", nh4_flag, nh4_qual, sep = " ") 

  if ("no2_3" %in% colnames(data))
    f <- f %>%
      unite("no2_3_flags", no2_3_flag, no2_3_qual, sep = " ") 
  
  if ("no2" %in% colnames(data))
    f <- f %>%
      unite("no2_flags", no2_flag, no2_qual, sep = " ")
  
  if ("no3" %in% colnames(data))
    f <- f %>%
      unite("no3_flags", no3_flag, no3_qual, sep = " ")
  
  
  if ("tp" %in% colnames(data))
  f <- f %>%
  unite("tp_flags", tp_flag, tp_qual, sep = " ") %>%
  unite("tn_flags", tn_flag, tn_qual, sep = " ")
  
  if ("op" %in% colnames(data))
    f <- f %>%
    unite("op_flags", op_flag, op_qual, sep = " ") 

  
  return(f)
  
}


# create path for Lake Jean Neustadt
cin.ada.path <- paste0(userPath, 
                       "data/chemistry/nutrients/ADA/CH4_147_Lake Jean Neustadt/")

# apply get_ada_data and dup_agg functions to each spreadsheet for Lake Jean Neustadt

jea1 <- get_ada_data21(cin.ada.path, "EPAGPA054,SS#7773,AE2.6,Forshay,7-14-21,oP,GPKR.xls") %>%
  conv_units(filename = "EPAGPA054,SS#7773,AE2.6,Forshay,7-14-21,oP,GPKR.xls") %>%
  mutate(site_id = "1") %>% # add site_id, U-01
  mutate(sample_filter = "filtered") %>% # filtered or unfiltered, based on file name
  dup_agg21 %>% # aggregate lab duplicates (optional) 
  flag_agg # merge flag columns


jea2 <- get_ada_data21(cin.ada.path, "EPAGPA054SS#7773,AE2.6,Forshay,7-14-21,TNTPGPKR.xls") %>%
  conv_units("EPAGPA054SS#7773,AE2.6,Forshay,7-14-21,TNTPGPKR.xls") %>%
  mutate(site_id = "1") %>% # add site_id, U-01
  mutate(sample_filter = "unfiltered") %>% # filtered or unfiltered, based on file name
  dup_agg21 %>% # aggregate lab duplicates (optional)
  flag_agg # merge flag columns

jea3 <- get_ada_data21(cin.ada.path, "EPAGPA054SS#7773AE2.6Forshay,7-14-21,NO3NO2NH4.xlsx") %>%
  conv_units("EPAGPA054SS#7773AE2.6Forshay,7-14-21,NO3NO2NH4.xlsx") %>%
  mutate(site_id = "1") %>% # add site_id, U-01
  mutate(sample_filter = "filtered") %>% # filtered or unfiltered, based on file name
  dup_agg21 %>% # aggregate lab duplicates (optional)
  flag_agg # merge flag columns


# create path for Keystone Lake
cin.ada.path <- paste0(userPath, 
                       "data/chemistry/nutrients/ADA/CH4_148_Keystone Lake/")

# apply get_ada_data and dup_agg functions to each spreadsheet for Keystone Lake 
key1 <- get_ada_data21(cin.ada.path, "EPAGPA061,SS#7784,AE2.6,Forshay,8-17-21,oP,GPKR.xls") %>%
   conv_units("EPAGPA061,SS#7784,AE2.6,Forshay,8-17-21,oP,GPKR.xls") %>%
   mutate(site_id = "7") %>% # add site_id, U-07
   mutate(sample_filter = "filtered") %>% # filtered or unfiltered, based on file name
   dup_agg21 %>% # aggregate lab duplicates (optional)
   flag_agg # merge flag columns

key2 <- get_ada_data21(cin.ada.path, "EPAGPA061SS#7784,AE2.6,Forshay,8-17-21,TN,TP,GPKR.xls") %>%
   conv_units("EPAGPA061SS#7784,AE2.6,Forshay,8-17-21,TN,TP,GPKR.xls") %>%
   mutate(site_id = "7") %>% # add site_id, U-07
   mutate(sample_filter = "unfiltered") %>% # filtered or unfiltered, based on file name
   dup_agg21 %>% # aggregate lab duplicates (optional)
   flag_agg # merge flag columns

key3 <- get_ada_data21(cin.ada.path, "EPAGPA061SS#7784AE2.6Forshay,8-17-21NO3+NO2NH4NO2NO3GPMS.xlsx") %>%
   conv_units("EPAGPA061SS#7784AE2.6Forshay,8-17-21NO3+NO2NH4NO2NO3GPMS.xlsx") %>%
   mutate(site_id = "7") %>% # add site_id, U-07
   mutate(sample_filter = "filtered") %>% # filtered or unfiltered, based on file name
   dup_agg21 %>%# aggregate lab duplicates (optional)
   flag_agg # merge flag columns

# create path for Lake Overholser
cin.ada.path <- paste0(userPath, 
                       "data/chemistry/nutrients/ADA/CH4_167_Lake Overholser/")

# apply get_ada_data and dup_agg functions to each spreadsheet for Lake Overholser
ove1 <- get_ada_data21(cin.ada.path, "EPAGPA059,SS#7777,AE2.6,Forshay,7-27-21,oP,GPKR.xls") %>%
   conv_units("EPAGPA059,SS#7777,AE2.6,Forshay,7-27-21,oP,GPKR.xls") %>%
   mutate(site_id = "6") %>% # add site_id, U-06
   mutate(sample_filter = "filtered") %>% # filtered or unfiltered, based on file name
   dup_agg21 %>% # aggregate lab duplicates (optional)
   flag_agg # merge flag columns

ove2 <- get_ada_data21(cin.ada.path, "EPAGPA059SS#7777,AE2.6,Forshay,7-27-21,TN,TP,GPKR.xls") %>%
   conv_units("EPAGPA059SS#7777,AE2.6,Forshay,7-27-21,TN,TP,GPKR.xls") %>%
   mutate(site_id = "6") %>% # add site_id, U-06
   mutate(sample_filter = "unfiltered") %>% # filtered or unfiltered, based on file name
   dup_agg21 %>% # aggregate lab duplicates (optional)
   flag_agg # merge flag columns

# deleted "GPMS" from end of excel file name.  Full file name couldn't be read on Jake's laptop.
ove3 <- get_ada_data21(cin.ada.path, "EPAGPA059SS#7777AE2.6Forshay,7-27-21NO3+NO2NH4NO2NO3.xlsx") %>%
   conv_units("EPAGPA059SS#7777AE2.6Forshay,7-27-21NO3+NO2NH4NO2NO3.xlsx") %>%
   mutate(site_id = "6") %>% # add site_id, U-06
   mutate(sample_filter = "filtered") %>% # filtered or unfiltered, based on file name
   dup_agg21 %>% # aggregate lab duplicates (optional)
   flag_agg # merge flag columns

# 2020 NUTRIENT SAMPLES-----------------------------------------------------------
# Nutrient samples for the 2020 SuRGE field season were held in Cincinnati,
# then shipped to ADA in May 2021 for analysis.  The reports are formatted
# differently than the 2021 data reports.  We anticipate that the 2020 format
# will be used in 2022 and 2023.  Here we modify the functions defined above
# to accomodate format for 2020, 2022, and 2023.


get_ada_data <- function(path, datasheet) { 
   
# for 2020 data and 2022 onward, assuming field_sample_id format is the same
   
   # toptable contains analyte names and MDL values
   toptable <- read_excel(paste0(path, datasheet), # get MDL & analyte names
                          sheet = "Data", range = "C8:N50", col_names = FALSE) %>% # up to 492 rows
      row_to_names(row_number = 1, remove_row = FALSE) %>% # column names = first row of data
      clean_names(case = "upper_camel") %>% # keep case consistent
      filter(Analytes %in% c("Analytes", "Unit", "MDL"), ignore.case=TRUE) %>% # # remove unneeded rows
      pivot_longer(cols = -Analytes, names_to = "variable")  %>% # transpose the tibble
      pivot_wider(names_from = Analytes, values_from = value) %>% # transpose the tibble
      select(-variable) %>% # no longer needed
      janitor::remove_empty("rows") %>% # remove empty rows
      filter(str_detect(Analytes, "nalytes") == FALSE) %>% # if necessary, removes superfluous row
      mutate(Analytes = str_c(Analytes, Unit)) %>% # concatenate analyte and unit, so unit is retained
      column_to_rownames(var = "Analytes") %>% # simpler to work w/ rownames in next chunk of code
      mutate(MDL = str_replace(MDL, "\\**", "")) %>% # remove asterisks from MDL value
      mutate(MDL = as.numeric(MDL)) # covert MDL values to numeric
   analyte_names <- row.names(toptable) # pass analyte names to maintable, below
   
   #'maintable' combines 'toptable' with results
   maintable <- read_excel(paste0(path, datasheet), # get the results
                           sheet = "Data", range = "A14:N500", na = "-") %>% # up to 492 rows
      janitor::remove_empty("rows") %>% # remove empty rows
      janitor::clean_names() %>%
      mutate(lab_sample_id = toupper(lab_sample_id)) %>% # make uppercase since Ada isn't consistent
      mutate(labdup = if_else(str_detect(lab_sample_id, "LAB DUP"), "LAB DUP", "")) %>% # flag the dups
      select(field_sample_id, labdup, starts_with("dat")) %>% # remove unneeded columns
      rename_with(~paste0(analyte_names), .cols = starts_with("data")) %>% # rename using analyte names
      rename_with(~paste0(analyte_names, "_date_analyzed"), .cols = starts_with("date_a")) %>% # rename using analyte names
      mutate(across(ends_with("analyzed"), # compute holding time of analytes
                    ~ as.numeric(as.Date(., format = "%m/%d/%Y") - as.Date(date_collected, format = "%m/%d/%Y")))) %>%
      rename(sampleid = field_sample_id) %>% # temporary rename; changes later during text parsing
      filter(str_starts(sampleid, "TN\\d|DN\\d")) %>% # retain only rows where sampleid starts with TN or DN
      select(sampleid, labdup, everything(), -date_collected) %>% # reorder columns for the following mutate()
      mutate(across(ends_with("/L"), # create new flag column if analyte not detected
                    ~ if_else(str_detect(., "ND"), "ND", ""),
                    .names = "{col}_flag")) %>%
      mutate(across(ends_with("/L"), # replace ND with the MDL value from toptable
                    ~ ifelse(str_detect(., "ND"), toptable[paste(cur_column()),2], .))) %>% # note this is base::ifelse
      mutate(sampleid = str_replace_all(sampleid, "(TN or DN)","")) %>% # clean-up sampleid field
      mutate(sampleid = str_replace_all(sampleid, "\\(\\)","")) %>% # clean-up sampleid field
      mutate(sampleid = str_replace_all(sampleid, " ", "")) %>% # remove any blank spaces inside string
      mutate(across(!ends_with(c("flag", "labdup", "sampleid")), # remove 'BQL', 'RPD' & other junk from data fields
                    ~ str_extract(., pattern = "\\-*\\d+\\.*\\d*"))) %>%
      mutate(across(!ends_with(c("flag", "labdup", "sampleid")), # make extracted data numeric
                    ~ as.numeric(.))) %>%
      mutate(sample_depth = str_sub(sampleid, 6, 6)) %>% # get sample depth from sampleid
      mutate(sample_type = str_sub(sampleid, 7, 7)) %>% # get sample type from sampleid
      mutate(sample_filter = str_sub(sampleid, 1, 1)) %>% # get sample filter from sampleid
      mutate(sampleid = str_sub(sampleid, 3, 5)) %>% # make sampleid 3-digit numeric lake id only
      mutate(sample_depth = str_replace_all(sample_depth, c("D" = "deep", "S" = "shallow", "N" = "blank"))) %>%
      mutate(sample_type = str_replace_all(sample_type, c("B" = "blank", "U" =  "unknown", "D" =  "duplicate"))) %>%
      mutate(sample_filter = str_replace_all(sample_filter, c("D" =  "filtered", "N" = "nonfiltered"))) %>%
      rename(lake_id = sampleid) %>% # change name to match chemCoc
      mutate(lake_id = as.character(as.numeric(lake_id))) %>% # consistent format for lake_id
      mutate(across(ends_with("analyzed"), # replace no. days w/ "H" if holding time violated
                    ~ ifelse(.>28, "H", ""))) %>% 
      mutate(site_id = "") # create empty column for site_id (id is populated later)

   return(maintable)
   
}


# Function to aggregate the lab duplicates (2020)
dup_agg <- function(data) {
   
   # first, convert all _flag columns to a numeric for summarize operations;
   # Must be performed in the event that a lab dup has a different flag than the sample.
   
   data <- data %>% 
      mutate(across(ends_with("flag"), 
                    ~ ifelse(str_detect(., "ND"), 1, 0))) 
   
   # carve out the _flag and _units columns so they can be re-joined later
   c <- data %>% select(ends_with(c("id","labdup", "type", "depth", "filter", "units", "qual")))
   
   # group and summarize to obtain means                
   d <- data %>%
      dplyr::group_by(lake_id, sample_depth, sample_type) %>%
      # '!' to exclude columns we don't want to aggregate
      summarize(across(!ends_with(c("site_id","labdup", "type", "depth", "filter", "units", "qual")), 
                       ~ mean(., na.rm = TRUE)))
   
   e <- left_join(d, c, by = c("lake_id", "sample_depth", "sample_type")) %>% # rejoin the data
      mutate(across(4:last_col(), # convert NaN to NA
                    ~ ifelse(is.nan(.), NA, .))) %>% # must use ifelse here (not if_else)
      select(order(colnames(.))) %>% # alphabetize column names
      select(lake_id, site_id, sample_depth, sample_type, sample_filter, labdup, everything()) %>% # put 'sampleid' first
      mutate(across(ends_with("flag"), # convert all _flag values back to text
                    ~ if_else(.<1, "", "ND"))) %>% 
      filter(labdup != "LAB DUP") %>% # remove the lab dup
      select(-labdup) # remove labdup column. JB 12/7/2021

   return(e)
   
}

  
# 1. Read in root path for 2020 chemistry data analyzed in ADA.
cin.ada.path <- paste0(userPath, 
                           "data/chemistry/nutrients/2020cinSentToAda/")
# 2. Read in files containing unique sample IDs.
tot.id <- read_excel(paste0(cin.ada.path, "totalNutrientSampleIds.xlsx"))
diss.id <- read_excel(paste0(cin.ada.path, "dissolvedNutrientSampleIds.xlsx"))
id <- rbind(tot.id, diss.id) %>% select(lake_id, site_id) %>% distinct() %>%
   mutate(lake_id = as.numeric(lake_id) %>% as.character(),
          # extract number from site_id, convert to char to match other data
          site_id = as.character(gsub(".*?([0-9]+).*", "\\1", site_id)))

# 3. Read data
# op
lmp1 <- get_ada_data(cin.ada.path, "EPAGPA053SS#7759AE2.6ForshayLakeMethaneProject7-6-20oPRev1GPKR.xls") %>%
   filter(sample_filter == "filtered") %>% # op should only be filtered samples
   conv_units(filename = "EPAGPA053SS#7759AE2.6ForshayLakeMethaneProject7-6-20oPRev1GPKR.xls") %>%
   select(-site_id) %>%
   left_join(., id) %>% # add site_id
   dup_agg %>% # aggregate the lab duplicates (optional)
   flag_agg # merge flag columns

# TN TP
lmp2 <- get_ada_data(cin.ada.path, "EPAGPA053SS#7759,AE2.6,Forshay,LakeMethaneProject,7-6-20,TNTP,GPKR.xls") %>%
   filter(sample_filter == "T") %>% # T for total, that is what we want for TN and TP
   conv_units(filename = "EPAGPA053SS#7759,AE2.6,Forshay,LakeMethaneProject,7-6-20,TNTP,GPKR.xls") %>%
   select(-site_id) %>%
   left_join(., id) %>% # add site_id
   dup_agg %>% # aggregate the lab duplicates (optional)
  flag_agg # merge flag columns

# NO2, NO3, NO2+NO3, NH4
lmp3 <- get_ada_data(cin.ada.path, "EPAGPA053SS#7759ForshayLakeMethaneProject7-6-2020NO3+NO2NH4GPMS.xls") %>%
   filter(sample_filter == "filtered") %>% # inorganic N should only be filtered samples
   conv_units(filename = "EPAGPA053SS#7759ForshayLakeMethaneProject7-6-2020NO3+NO2NH4GPMS.xls") %>%
   select(-site_id) %>%
   left_join(., id) %>% # add site_id
   dup_agg %>% # aggregate the lab duplicates (optional)
   flag_agg # merge flag columns

lapply(list(lmp1, lmp2, lmp3), function(x) any(is.na(x$site_id))) # all records have site_id



# 2022 NUTRIENT SAMPLES-----------------------------------------------------------

get_ada_data22 <- function(path, datasheet) { 
  
  # toptable contains analyte names and MDL values
  toptable <- read_excel(paste0(path, datasheet), # get MDL & analyte names
                         sheet = "Data", range = "C8:N50", col_names = FALSE) %>% # up to 492 rows
    row_to_names(row_number = 1, remove_row = FALSE) %>% # column names = first row of data
    clean_names(case = "upper_camel") %>% # keep case consistent
    filter(Analytes %in% c("Analytes", "Unit", "MDL"), ignore.case=TRUE) %>% # # remove unneeded rows
    pivot_longer(cols = -Analytes, names_to = "variable")  %>% # transpose the tibble
    pivot_wider(names_from = Analytes, values_from = value) %>% # transpose the tibble
    select(-variable) %>% # no longer needed
    janitor::remove_empty("rows") %>% # remove empty rows
    filter(str_detect(Analytes, "nalytes") == FALSE) %>% # if necessary, removes superfluous row
    mutate(Analytes = str_c(Analytes, Unit)) %>% # concatenate analyte and unit, so unit is retained
    column_to_rownames(var = "Analytes") %>% # simpler to work w/ rownames in next chunk of code
    mutate(MDL = str_replace(MDL, "\\**", "")) %>% # remove asterisks from MDL value
    mutate(MDL = as.numeric(MDL)) # covert MDL values to numeric
  
  analyte_names <- row.names(toptable) # pass analyte names to maintable, below


  #'maintable' combines 'toptable' with results
  maintable <- read_excel(paste0(path, datasheet), # get the results
                          sheet = "Data", range = "A14:N500", na = "-") %>% # up to 492 rows
    janitor::remove_empty("rows") %>% # remove empty rows
    janitor::clean_names() %>%
    mutate(lab_sample_id = toupper(lab_sample_id)) %>% # make uppercase since Ada isn't consistent
    mutate(labdup = if_else(str_detect(lab_sample_id, "LAB DUP"), "LAB DUP", "")) %>% # flag the dups
    select(field_sample_id, labdup, starts_with("dat")) %>% # remove unneeded columns
    rename_with(~paste0(analyte_names), .cols = starts_with("data")) %>% # rename using analyte names
    rename_with(~paste0(analyte_names, "_date_analyzed"), .cols = starts_with("date_a")) %>% # rename using analyte names
    mutate(across(ends_with("analyzed"), # compute holding time of analytes
                  ~ as.numeric(as.Date(., format = "%m/%d/%Y") - as.Date(date_collected, format = "%m/%d/%Y"))))  %>%
  rename(sampleid = field_sample_id) %>% # temporary rename; changes later during text parsing
  filter(str_count(sampleid) == 5) %>% # retain only rows where sampleid is exactly 5 char long
    select(sampleid, labdup, everything(), -date_collected) %>% # reorder columns for the following mutate()
    mutate(across(ends_with("/L"), # create new flag column if analyte not detected
                  ~ if_else(str_detect(., "ND"), "ND", ""),
                  .names = "{col}_flag")) %>%
    mutate(across(ends_with("/L"), # replace ND with the MDL value from toptable
                  ~ ifelse(str_detect(., "ND"), toptable[paste(cur_column()),2], .))) %>% # note this is base::ifelse
    mutate(sampleid = str_replace_all(sampleid, "(TN or DN)","")) %>% # clean-up sampleid field
    mutate(sampleid = str_replace_all(sampleid, "\\(\\)","")) %>% # clean-up sampleid field
    mutate(sampleid = str_replace_all(sampleid, " ", "")) %>% # remove any blank spaces inside string
    mutate(across(!ends_with(c("flag", "labdup", "sampleid")), # remove 'BQL', 'RPD' & other junk from data fields
                  ~ str_extract(., pattern = "\\-*\\d+\\.*\\d*"))) %>%
    mutate(across(!ends_with(c("flag", "labdup", "sampleid")), # make extracted data numeric
                  ~ as.numeric(.))) %>%
    mutate(sample_depth = str_sub(sampleid, 4, 4)) %>% # get sample depth from sampleid
    mutate(sample_type = str_sub(sampleid, 5, 5)) %>% # get sample type from sampleid
    mutate(sampleid = str_sub(sampleid, 1, 3)) %>% # make sampleid 3-digit numeric lake id only
    mutate(sample_depth = str_replace_all(sample_depth, c("D" = "deep", "S" = "shallow", "N" = "blank"))) %>%
    mutate(sample_type = str_replace_all(sample_type, c("B" = "blank", "U" =  "unknown", "D" =  "duplicate"))) %>%
    rename(lake_id = sampleid) %>% # change name to match chemCoc
    mutate(lake_id = as.character(as.numeric(lake_id))) %>% # consistent format for lake_id
    mutate(across(ends_with("analyzed"), # replace no. days w/ "H" if holding time violated
                  ~ ifelse(.>28, "H", ""))) %>%
    # For 2022, SITE ID as follows: lakeid 184:3, lakeid 166:6, lakeid 146:4, lakeid 190:8
    mutate(site_id = case_when( # add site_id
      lake_id == "146" ~ "4",
      lake_id == "166" ~ "6",
      lake_id == "184" ~ "3",
      lake_id == "190" ~ "8",
      lake_id == "136" ~ "13",
      lake_id == "100" ~ "11",
      lake_id == "206" ~ "2",
      TRUE ~ "")) # blank if no match, but this will only occur if lake_id is missing/wrong

  return(maintable)

}



dup_agg22 <- function(data) {
  
  # Note: identical to dup_agg21 function (as of 19 Jul 2022)
  
  # first, convert all _flag columns to a numeric for summarize operations;
  # Must be performed in the event that a lab dup has a different flag than the sample.
  
  data <- data %>% 
    mutate(across(ends_with("flag"), 
                  ~ ifelse(str_detect(., "ND"), 1, 0))) 
  
  # carve out the _flag and _units columns so they can be re-joined later
  c <- data %>% select(ends_with(c("id","labdup", "type", "depth", "filter", "units", "qual")))
  
  d <- data %>%
    dplyr::group_by(lake_id, sample_depth, sample_type) %>%
    # '!' to exclude columns we don't want to aggregate
    summarize(across(!ends_with(c("id","labdup", "type", "depth", "filter", "units", "qual")), 
                     ~ mean(., na.rm = TRUE))) 
  
  e <- left_join(d, c, by = c("lake_id", "sample_depth", "sample_type")) %>% # rejoin the data
    mutate(across(3:last_col(), # convert NaN to NA
                  ~ ifelse(is.nan(.), NA, .))) %>% # must use ifelse here (not if_else)
    select(order(colnames(.))) %>% # alphabetize column names
    select(lake_id, site_id, sample_depth, sample_type, sample_filter, labdup, everything()) %>% # put 'sampleid' first
    mutate(across(ends_with("flag"), # convert all _flag values back to text
                  ~ if_else(.<1, "", "ND"))) %>% 
    filter(labdup != "LAB DUP") %>% # remove the lab dup
    select(-labdup) # remove labdup column. 
  
  return(e)
  
}
  
# Read in root path for 2022 chemistry data analyzed in ADA.
cin.ada.path <- paste0(userPath, 
                       "data/chemistry/nutrients/ADA/2022/")

# apply get_ada_data22 and dup_agg22 functions to each excel file
no2no3nh4.146190184166.22 <-
  get_ada_data22(cin.ada.path, "EPAGPA076_146_190_184_166_NO3+NO2NH4.xlsx") %>%
  conv_units(filename = "EPAGPA076_146_190_184_166_NO3+NO2NH4.xlsx") %>%
  mutate(sample_filter = "unfiltered") %>% # filtered or unfiltered, based on file name
  dup_agg22 %>% # aggregate lab duplicates (optional)
  flag_agg # merge flag columns for each analyte

tntp.146190184166.22 <- 
  get_ada_data22(cin.ada.path, "EPAGPA076_146_190_184_166_TN,TP.xls") %>%
  conv_units(filename = "EPAGPA076_146_190_184_166_TN,TP.xls") %>%
  mutate(sample_filter = "unfiltered") %>% # filtered or unfiltered, based on file name
  dup_agg22 %>% # aggregate lab duplicates (optional)
  flag_agg # merge flag columns for each analyte

op.146190.22 <- 
  get_ada_data22(cin.ada.path, "EPAGPA076_146_190_oP.xls") %>%
  conv_units(filename = "EPAGPA076_146_190_oP.xls") %>%
  mutate(sample_filter = "filtered") %>% # filtered or unfiltered, based on file name
  dup_agg22 %>% # aggregate lab duplicates (optional)
  flag_agg # merge flag columns for each analyte

op.166.22 <- 
  get_ada_data22(cin.ada.path, "EPAGPA076_166_oP.xls") %>%
  conv_units(filename = "EPAGPA076_166_oP.xls") %>%
  mutate(sample_filter = "filtered") %>% # filtered or unfiltered, based on file name
  dup_agg22  %>% # aggregate lab duplicates (optional)
  flag_agg # merge flag columns for each analyte

op.184.22 <- 
  get_ada_data22(cin.ada.path, "EPAGPA076_184_oP.xls") %>%
  conv_units(filename = "EPAGPA076_184_oP.xls") %>%
  mutate(sample_filter = "filtered") %>% # filtered or unfiltered, based on file name
  dup_agg22  %>% # aggregate lab duplicates (optional)
  flag_agg # merge flag columns for each analyte

no2no3nh4.136100206.22 <- 
  get_ada_data22(cin.ada.path, "EPAGPA081_136_100_206_NO3+NO2NH4.xlsx") %>%
  conv_units(filename = "EPAGPA081_136_100_206_NO3+NO2NH4.xlsx") %>%
  mutate(sample_filter = "unfiltered") %>% # filtered or unfiltered, based on file name
  dup_agg22  %>% # aggregate lab duplicates (optional)
  flag_agg # merge flag columns for each analyte

tntp.136100206.22 <- 
  get_ada_data22(cin.ada.path, "EPAGPA081_136_100_206_TN,TP.xls") %>%
  conv_units(filename = "EPAGPA081_136_100_206_TN,TP.xls") %>%
  mutate(sample_filter = "unfiltered") %>% # filtered or unfiltered, based on file name
  dup_agg22  %>% # aggregate lab duplicates (optional)
  flag_agg # merge flag columns for each analyte

op.136100206.22 <- 
  get_ada_data22(cin.ada.path, "EPAGPA081_136_100_206_oP.xls") %>%
  conv_units(filename = "EPAGPA081_136_100_206_oP.xls") %>%
  mutate(sample_filter = "filtered") %>% # filtered or unfiltered, based on file name
  dup_agg22  %>% # aggregate lab duplicates (optional)
  flag_agg # merge flag columns for each analyte




# JOIN ALL DATA OBJECTS------------------------------------------------------------

# Join all of the data objects
ada.nutrients <- list(jea = list(jea1, jea2, jea3), key = list(key1, key2, key3), 
                      ove = list(ove1, ove2, ove3), lmp = list(lmp1, lmp2, lmp3),
                      ada22 = list(op.146190.22, op.166.22, op.184.22, 
                                   op.136100206.22,
                                   no2no3nh4.146190184166.22,
                                   no2no3nh4.136100206.22, tntp.146190184166.22,
                                   tntp.136100206.22)) %>% 
   map_depth(2, ~select(., -sample_filter)) %>%
   map_depth(1, function(x) reduce(x, left_join)) %>%
   reduce(full_join) %>%
   mutate(site_id = as.numeric(site_id)) %>%
   arrange(lake_id) %>%
   ungroup()
   



# INSPECT FINAL MERGED DATA OBJECT-------------------------------------------
# Inspect final object for merge errors
# any duplicate rows for a given set of unique identifiers?  No, good!
ada.nutrients %>% select(lake_id, site_id, sample_depth, sample_type) %>%
   janitor::get_dupes()

# Did we preserve all combinations of unique identifiers in original data?
# 74 unique combinations of lake_id, site_id, sample_depth, and sample_type
# in original data.
list(jea1, jea2, jea3, key1, key2, key3, 
     ove1, ove2, ove3, lmp1, lmp2, lmp3) %>%
   map(~select(., lake_id, site_id, sample_depth, sample_type)) %>%
   map_dfr(~bind_rows(.)) %>% # bind all df by rows, creates one df
   distinct() %>% # condense to unique observations
   nrow(.) # 74 unique combinations

# 74 unique combinations in merged df.  Everything looks good
ada.nutrients %>% select(lake_id, site_id, sample_depth, sample_type) %>%
   distinct() %>% {nrow(.)}

# # Some unneeded code below to help resolved discrepancy between original and
# # merged data.
# 
# # concatenate unique identifiers in merged data into one column
# merged.obs <- ada.nutrients %>% select(lake_id, site_id, sample_depth, sample_type) %>%
#    distinct() %>%
#    unite("unique", everything()) # concatenate unique identifiers into string
# 
# # concatenate unique identifiers in original data into one column
# original.obs <- list(jea1, jea2, jea3, key1, key2, key3, 
#                      ove1, ove2, ove3, lmp1, lmp2, lmp3) %>%
#    map(~select(., lake_id, site_id, sample_depth, sample_type)) %>%
#    map_dfr(~bind_rows(.)) %>% # bind all df by rows, creates one df
#    distinct() %>% # pull our unique observations
#    unite("unique", everything()) # concatenate unique identifiers into string
# 
# # Which original data observation is missing from the merged data
# original.obs %>% filter(!(original.obs$unique %in% merged.obs$unique))


