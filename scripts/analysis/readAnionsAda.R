# ADA analyzing their own anions.  See ...data/chemistry/anions_ada_daniels/ADA/...

# See Wiki for analtye name and unique identifier conventions.

# ANIONS ADA 2021-----------------------------------------------------------------


get_ada_data <- function(path, datasheet) { 
  
  # for 2020 data (and 2022 onward), as field_sample_id is in different format
  
  # toptable contains analyte names and MDL values
  
  toptable <- read_excel(paste0(path, datasheet), # get MDL & analyte names
                         sheet = "Data", range = "C8:N50", 
                         col_names = FALSE) %>% # up to 492 rows
    row_to_names(row_number = 1, 
                 remove_row = FALSE) %>% # column names = first row of data
    clean_names(case = "upper_camel") %>% # keep case consistent
    filter(Analytes %in% c("Analytes", "Unit", "MDL"), 
           ignore.case = TRUE) %>% # # remove unneeded rows
    pivot_longer(cols = -Analytes, names_to = "variable")  %>% # transpose 
    pivot_wider(names_from = Analytes, values_from = value) %>% # transpose 
    select(-variable) %>% # no longer needed
    janitor::remove_empty("rows") %>% # remove empty rows
    filter(str_detect(Analytes, "nalytes") == FALSE) %>% # removes extra row
    mutate(Analytes = str_c(Analytes, Unit)) %>% # combine & retain units
    column_to_rownames(var = "Analytes") %>% # simpler to work w/ rownames 
    mutate(MDL = str_replace(MDL, "\\**", "")) %>% # remove asterisks from MDL 
    mutate(MDL = as.numeric(MDL)) # covert MDL values to numeric
  
  analyte_names <- row.names(toptable) # pass analyte names to maintable, below

  # maintable_1: combine toptable with results, make uppercase, id lab dups
  
  maintable_1 <- read_excel(paste0(path, datasheet), # get the results
                            sheet = "Data", 
                            range = "A14:N500") %>% # up to 492 rows
    janitor::remove_empty("rows") %>% # remove empty rows
    janitor::clean_names() %>%
    mutate(lab_sample_id = toupper(lab_sample_id)) %>% # make uppercase 
    mutate(labdup = if_else(str_detect(lab_sample_id, "LAB DUP"), 
                            "LAB DUP", "")) # flag the dups
  
  # maintable_2: select columns, rename remaining columns, 
  # calculate hold times
  
  maintable_2 <- maintable_1 %>%
    select(field_sample_id, labdup, starts_with("dat")) %>% # 
    rename_with(~paste0(analyte_names), 
                .cols = starts_with("data")) %>% # rename w/ analyte names
    rename_with(~paste0(analyte_names, "_date_analyzed"), 
                .cols = starts_with("date_a")) %>% # rename w/ analyte names
    mutate(across(ends_with("analyzed"), # select the latest date in range
                  ~ word(., -1) %>% 
                    as.Date(., tryFormats = c("%m/%d/%Y", "%Y-%m-%d")))) %>%
    mutate(across(ends_with("analyzed"), # compute holding time
                  ~ as.numeric(as.Date(., tryFormats = c("%m/%d/%Y", 
                                                         "%Y-%m-%d")) - 
                                 as.Date(date_collected, 
                                         format = "%m/%d/%Y")))) 
  
    # Special procedure for Ada anions data:
    # The Ada anions Excel file has 1 date analyzed column for all analytes.
    # We need copy this column across all analytes. 
  
  ada_anions_dates <- maintable_2 %>% # get the date data
    select(ends_with("analyzed")) %>%
    pull()
  
  maintable_2.5 <- maintable_2 %>%
    mutate(across(ends_with("/L"), # create date columns for all analytes
                  ~ ada_anions_dates, # copy the date data into all columns
                  .names = "{col}_date_analyzed"))
                
  # maintable_3: remove extra rows, create ND flag and apply MDL value, 
  # create L (i.e., BQL) flag
  
  maintable_3 <- maintable_2.5 %>%
    mutate(field_sample_id = # clean-up sampleid field
             str_remove_all(field_sample_id, "\\(|\\)|\\s")) %>% 
    filter(str_starts(field_sample_id, 
                      "TN\\d|DN\\d")|  # retain if ID starts with TN or DN, 
             str_count(field_sample_id) == 5) %>% # and retain if length = 5
    select(field_sample_id, labdup, everything(), 
           -date_collected) %>% # reorder columns to mutate()
    mutate(across(ends_with("/L"), # create new flag if analyte not detected
                  ~ if_else(str_detect(., "ND"), "ND", ""),
                  .names = "{col}_flag")) %>%
    mutate(across(ends_with("/L"), # replace ND with MDL value from toptable
                  ~ ifelse(str_detect(., "ND"), 
                           toptable[paste(cur_column()),2], .))) %>% 
    mutate(across(ends_with("/L"), # create new flag column for qual limit
                  ~ if_else(str_detect(., "BQL"), "L", ""),
                  .names = "{col}_bql")) 
  
  # maintable_4: remove extra characters, make numeric, parse sample IDs,  
  # format lake IDs, determine if hold time violated
  
  maintable_4 <- maintable_3 %>%
    mutate(field_sample_id = str_remove_all(
      field_sample_id, "TN|or|DN")) %>% # clean-up data
    mutate(across(!ends_with(c("flag", "analyzed", "bql", 
                               "labdup", "field_sample_id")), 
                  ~ str_extract(., pattern = "\\-*\\d+\\.*\\d*"))) %>% 
    mutate(across(!ends_with(c("flag", "analyzed","bql", # clean-up data
                               "labdup", "field_sample_id")), 
                  ~ as.numeric(.))) %>% # make extracted data numeric
    mutate(sample_depth = str_sub( # get sample depth 
      str_remove_all(field_sample_id, "\\d"), 1, 1)) %>% 
    mutate(sample_type = str_sub( # get sample type 
      str_remove_all(field_sample_id, "\\d"), 2, 2)) %>% 
    mutate(lake_id = parse_number(field_sample_id) %>% # get lake id
             as.character())  %>% # match chemCoc format
    mutate(sample_depth = str_replace_all(sample_depth, 
                                          c("D" = "deep", 
                                            "S" = "shallow", 
                                            "N" = "blank"))) %>%
    mutate(sample_type = str_replace_all(sample_type, 
                                         c("B" = "blank", 
                                           "U" =  "unknown", 
                                           "D" =  "duplicate"))) %>%
    mutate(across(ends_with("analyzed"), # check if hold time violated
                  ~ ifelse(.>28, "H", ""))) %>%
    select(-field_sample_id) # no longer needed
  
  return(maintable_4)
  
}
# Function to read-in and collate data from excel files 

# Function to convert units, rename columns, and add units columns
conv_units <- function(data, filename) {
  
  
  # filename <- toupper(filename) # use if case becomes an issue in file names.
  
  # Anions
  
  f <- data %>%
    mutate(across(ends_with("/L"), 
                  ~ case_when( # convert no2, no3, and op
                    str_detect(paste(cur_column()), "Nitr|Phos") ~ .*1000,
                    TRUE ~ .*1))) %>% # everything else should be correct as-is
    rename(f = contains("Fluo") & !ends_with(c("flag", "bql", "analyzed")),
           cl = contains("Chlo") & !ends_with(c("flag", "bql", "analyzed")),
           br = contains("Brom") & !ends_with(c("flag", "bql", "analyzed")),
           so4 = contains("Sulf") & !ends_with(c("flag", "bql", "analyzed")),
           no2 = contains("Nitrite") & !ends_with(c("flag", "bql", "analyzed")),
           no3 = contains("Nitrate") & !ends_with(c("flag", "bql", "analyzed")),
           op = contains("Phos") & !ends_with(c("flag", "bql", "analyzed")),
           f_flag = contains("Fluo") & ends_with("flag"),
           cl_flag = contains("Chlo") & ends_with("flag"),
           br_flag = contains("Brom") & ends_with("flag"),
           so4_flag = contains("Sulf") & ends_with("flag"),
           no2_flag = contains("Nitrite") & ends_with("flag"),
           no3_flag = contains("Nitrate") & ends_with("flag"),
           op_flag = contains("Phos") & ends_with("flag"),
           f_bql = contains("Fluo") & ends_with("bql"),
           cl_bql = contains("Chlo") & ends_with("bql"),
           br_bql = contains("Brom") & ends_with("bql"),
           so4_bql = contains("Sulf") & ends_with("bql"),
           no2_bql = contains("Nitrite") & ends_with("bql"),
           no3_bql = contains("Nitrate") & ends_with("bql"),
           op_bql = contains("Phos") & ends_with("bql"),
           f_qual = contains("Fluo") & ends_with("analyzed"),
           cl_qual = contains("Chlo") & ends_with("analyzed"), 
           br_qual = contains("Brom") & ends_with("analyzed"),
           so4_qual = contains("Sulf") & ends_with("analyzed"), 
           no2_qual = contains("Nitrite") & ends_with("analyzed"), 
           no3_qual = contains("Nitrate") & ends_with("analyzed"), 
           op_qual = contains("Phos") & ends_with("analyzed")) %>%
    mutate(across(ends_with(c("f", "cl", "br", "so4", "no2",
                              "no3", "op")), 
                  ~ case_when( # create units column for each analyte
                    str_detect(paste(cur_column()), "no2") ~ "ug_n_l",
                    str_detect(paste(cur_column()), "no3") ~ "ug_n_l",
                    str_detect(paste(cur_column()), "op") ~ "ug_p_l",
                    TRUE ~ "mg_l"), # everything else is mg_l
                  .names = "{col}_units")) 
  
  # select and order columns
  f <- f %>% 
    select(order(colnames(.))) %>% # alphabetize and reorder columns
    select(lake_id, sample_depth, sample_type, labdup, everything())
  
  return(f)
  
}

# Function to aggregate the lab duplicates

dup_agg <- function(data) {
  
  # first, convert all _flag columns to a numeric for summarize operations;
  # Must be performed in case lab dup has a different flag than the sample.
  
  data <- data %>% 
    mutate(across(ends_with(c("flag", "bql", "qual")), 
                  ~ ifelse(str_detect(., "ND|L|H"), 1, 0))) 
  
  
  # carve out the _flag and _units columns so they can be re-joined later
  
  c <- data %>% select(ends_with(c("id","labdup", "type", "depth", 
                                   "units")))
  
  # summarize means of analyte values and flags 
  
  d <- data %>%
    dplyr::group_by(lake_id, sample_depth, sample_type) %>%
    # '!' to exclude columns we don't want to aggregate
    summarize(across(!ends_with(c("site_id","labdup", "type", "depth", 
                                  "units")), 
                     ~ mean(., na.rm = TRUE))) 
  
  # rejoin data 
  
  e <- left_join(d, c, by = c("lake_id", "sample_depth", "sample_type")) %>% 
    mutate(across(4:last_col(), # convert NaN to NA
                  ~ ifelse(is.nan(.), NA, .))) %>% # must use 'ifelse' here 
    select(order(colnames(.))) %>% # alphabetize column names
    select(lake_id, site_id, sample_depth, sample_type, 
           labdup, everything()) %>% # put 'sampleid' first
    mutate(across(ends_with("flag"), # convert all _flag values back to text
                  ~ if_else(.<1, "", "ND"))) %>% 
    mutate(across(ends_with("bql"), # convert all _flag values back to text
                  ~ if_else(.<1, "", "L"))) %>% 
    mutate(across(ends_with("qual"), # convert all _flag values back to text
                  ~ if_else(.<1, "", "H"))) %>% 
    filter(labdup != "LAB DUP") %>% # remove the lab dup
    select(-labdup) %>% # remove labdup column. JB 12/7/2021
    ungroup()
  
  return(e)
  
}

# Function to aggregate all flags into a single column

flag_agg <- function(data) { # merge the flag columns for each analyte
  
  f <- data
  
  if ("f" %in% colnames(data))
    f <- f %>%
      unite("f_flags", f_flag, f_bql, f_qual, sep = " ") 
  
  if ("cl" %in% colnames(data))
    f <- f %>%
      unite("cl_flags", cl_flag, cl_bql, cl_qual, sep = " ") 
  
  if ("br" %in% colnames(data))
    f <- f %>%
      unite("br_flags", br_flag, br_bql, br_qual, sep = " ")
  
  if ("so4" %in% colnames(data))
    f <- f %>%
      unite("so4_flags", so4_flag, so4_bql, so4_qual, sep = " ")
  
  if ("no2" %in% colnames(data))
    f <- f %>%
      unite("no2_flags", no2_flag, no2_bql, no2_qual, sep = " ")
  
  if ("no3" %in% colnames(data))
    f <- f %>%
      unite("no3_flags", no3_flag, no3_bql, no3_qual, sep = " ")
  
  if ("op" %in% colnames(data))
    f <- f %>%
      unite("op_flags", op_flag, op_bql, op_qual, sep = " ")
  
  # If a sample and lab dup both have a flag (ND or L), retain only the L flag
  g <- f %>% 
    mutate(across(ends_with("flags"),
                  ~ if_else(
                    str_detect(., "ND L"), "L", .)) )
  
  # If there are no flags, enter NA in the _flags column
  h <- g %>%
    mutate(across(ends_with("flags"),
                  ~ if_else(str_detect(., "\\w"), ., NA_character_) %>%
                    str_squish(.))) # remove any extra white spaces
  
  return(h)
  
}

# create path for Lake Jean Neustadt
cin.ada.path <- paste0(
  userPath, 
  "data/chemistry/anions_ada_daniels/ADA/CH4_147_Lake Jean Neustadt/")

# apply get_ada_data21, conv_units, & dup_agg21 to Lake Jean Neustadt excel file
jea1 <- get_ada_data(
  cin.ada.path, 
  "EPAGPA054SS7773AE2.6Neustadt7-14-2021ClSO4BrFNO3NO2oPGPKR.xls") %>%
  mutate(site_id = "1") %>% # add site_id
  conv_units(
    filename = 
      "EPAGPA054SS7773AE2.6Neustadt7-14-2021ClSO4BrFNO3NO2oPGPKR.xls") %>%
  dup_agg %>% # aggregate lab duplicates (optional)
  flag_agg


# create path for Keystone Lake
# This generates some warnings, but the output is correct 
cin.ada.path <- paste0(userPath, 
                       "data/chemistry/anions_ada_daniels/ADA/CH4_148_Keystone Lake/")

# apply get_ada_data21, conv_units, & dup_agg21 to Keystone Lake excel file
key1 <- get_ada_data(cin.ada.path, 
  "EPAGPA061SS7784AE2.6Keystone8-17-2021ClSO4BrFNO3NO2GPKR.xls") %>%
  mutate(site_id = "7") %>% # add site_id
  conv_units(filename = 
      "EPAGPA061SS7784AE2.6Keystone8-17-2021ClSO4BrFNO3NO2GPKR.xls") %>%
  dup_agg %>% # aggregate lab duplicates (optional)
  flag_agg



# create path for Lake Overholser
cin.ada.path <- 
  paste0(userPath, 
         "data/chemistry/anions_ada_daniels/ADA/CH4_167_Lake Overholser/")

# apply get_ada_data21, conv_units, & dup_agg21 to Lake Overholser excel file
ove1 <- 
  get_ada_data(
    cin.ada.path, "EPAGPA059SS7777AE2.6Overh7-27-2021ClSO4BrFNO3NO2.xls") %>%
  mutate(site_id = "6") %>% # add site_id
  conv_units(
    filename = "EPAGPA059SS7777AE2.6Overh7-27-2021ClSO4BrFNO3NO2.xls") %>%
  dup_agg %>% # aggregate lab duplicates (optional)
  flag_agg




### ANIONS ADA 2022 ------------------------------------------------------------


# The 2022 Excel files contain multiple lakes in each file. site_id can be 
# assigned using the following function:

site_id_22 <- function(data) { 
  
  data <- data %>%
    mutate(site_id = case_when( # add site_id
      lake_id == "146" ~ "4",
      lake_id == "166" ~ "6",
      lake_id == "184" ~ "3",
      lake_id == "190" ~ "8",
      lake_id == "136" ~ "13",
      lake_id == "100" ~ "11",
      lake_id == "206" ~ "2",
      TRUE ~ "")) # blank if no match; will only occur if lake_id is missing
  
  return(data)
  
}

cin.ada.path <- paste0(userPath,
                       "data/chemistry/anions_ada_daniels/ADA/2022/")

anions_2022_146_190 <- 
  get_ada_data(cin.ada.path, "EPAGPA076_146_190_anions.xls") %>%
  conv_units(filename = "EPAGPA076_146_190_anions.xls") %>%
  site_id_22 %>%
  dup_agg %>% # aggregate lab duplicates (optional)
  flag_agg

anions_2022_184_166 <- 
  get_ada_data(cin.ada.path, "EPAGPA076_184_166_anions.xls") %>%
  conv_units(filename = "EPAGPA076_184_166_anions.xls") %>%
  site_id_22 %>%
  dup_agg %>% # aggregate lab duplicates (optional)
  flag_agg

anions_2022_136_100_206 <- 
  get_ada_data(cin.ada.path, "EPAGPA081_136_100_206_anions.xls") %>%
  conv_units(filename = "EPAGPA081_136_100_206_anions.xls") %>%
  site_id_22 %>%
  dup_agg %>% # aggregate lab duplicates (optional)
  flag_agg




### JOIN DATA OBJECTS------------------------------------------------------------

# Join all of the data objects
ada.anions <- list(jea = list(jea1), key = list(key1), 
                   ove = list(ove1), 
                   ada.22 = list(anions_2022_146_190), 
                   ada_22 = list(anions_2022_184_166), 
                      ada_22 = list(anions_2022_136_100_206)) %>% 
  map(~ reduce(.x, left_join)) %>% 
  reduce(full_join) %>% 
  select(-starts_with("no"), -starts_with("op")) %>% # remove no2, no3, and op
  arrange(lake_id) 

