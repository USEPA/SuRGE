# ADA analyzing their own anions.  See ...data/chemistry/anions_ada_daniels/ADA/...

# See Wiki for analtye name and unique identifier conventions.

# ANIONS ADA 2021-----------------------------------------------------------------


get_ada_data <- function(path, datasheet) { 
  
  # for 2020 data (and 2022 onward), as field_sample_id is in different format
  
  # toptable contains analyte names and MDL values
  
  toptable <- read_excel(paste0(path, datasheet), # get MDL & analyte names
                         sheet = "Data", range = "C8:S50", 
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
                            range = "A14:S500") %>% # up to 492 rows
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
    # rename_with(~paste0(analyte_names, "_date_analyzed"), 
    #             .cols = starts_with("date_a")) %>% # rename w/ analyte names
    mutate(across(ends_with("nalyzed"), # remove junk from date column(s)
                  ~ ifelse(
                    str_detect(., "/|&|-"), word(., -1), .))) %>% 
    mutate(across(
      ends_with("nalyzed"), # convert to date format
      ~ case_when(
        # handle Excel-formatted dates
        str_length(.) == 5 ~ lubridate::as_date(as.numeric(.),
                                                origin = "1899-12-30"),
        # handle dates in m/d/y format (this RegEx looks for any /year)
        str_detect(., "/\\d{4}") ~ lubridate::mdy(.),
        # otherwise the dates are already date-formatted (y-m-d)
        TRUE ~ lubridate::ymd(.)))) %>%
    # Compute holding time. 'date_analyzed' is now holding time (in days).
    mutate(across(ends_with("nalyzed"), 
                  ~  as.numeric(. - as.Date(date_collected,
                                            tryFormats = c("%m/%d/%Y",
                                                           "%Y-%m-%d")))))

    # Special procedure for Ada anions data:
    # The Ada anions Excel file has 1 date analyzed column for all analytes.
    # We need copy this column across all analytes. 
  ada_anions_dates <- maintable_2 %>% # get the date data
    select(ends_with("nalyzed")) %>%
    pull()


  maintable_2.5 <- maintable_2 %>%
    mutate(across(ends_with("/L"), # create date columns for all analytes
                  ~ ada_anions_dates, # copy the date data into all columns
                  .names = "{col}_date_analyzed"))

  # maintable_3: remove extra rows, create ND flag and apply MDL value,
  # create L (i.e., BQL) flag, create 'visit' column
  maintable_3 <- maintable_2.5 %>%
    mutate(field_sample_id = # remove "(TN or DN)" field_sample_id
             str_remove_all(field_sample_id, "\\s|\\(|\\)|TN or DN")) %>%
    filter(!str_detect(field_sample_id, # Keep only the data rows
                       "[a-z]")) %>% # remove any rows w/ lowercase letters
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
                  .names = "{col}_bql")) %>%
    mutate(visit = if_else(str_ends(field_sample_id, "2"), 2, 1))

  # maintable_4: remove extra characters, make numeric, parse sample IDs,
  # format lake IDs, determine if hold time violated
  maintable_4 <- maintable_3 %>%
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
    select(-field_sample_id, -date_analyzed) # no longer needed

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
  

  # summarize means of analyte values
  
  d <- data %>%
    dplyr::group_by(lake_id, site_id, sample_depth, sample_type) %>%
    # Summarize() to get means of all analyte values
    summarize(across(where(is.numeric), 
                     ~ mean(., na.rm = TRUE)),
              # First() gets the first value only for the character fields
              # By ignoring NA, this will return any flag
              across(ends_with(c("units", "flag", "bql", "qual")),
                     # 'order_by' sorts the column in question
                     ~ last(., na_rm = TRUE, order_by = .))) %>%
    ungroup()

  return(d)
  
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
                  ~ if_else(str_detect(., ".*\\S.*"), ., NA_character_) %>%
                    str_squish())) # remove any extra white spaces
  
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




### ANIONS ADA 2022-23 --------------------------------------------------------


# Excel files may contain multiple lakes in each file. site_id can be 
# assigned using the following function:

site_id_number <- function(data) { 
  
  data <- data %>%
    mutate(site_id = case_when( # add site_id
      lake_id == "3" ~ "20",
      lake_id == "4" ~ "3",
      lake_id == "11" ~ "3",
      lake_id == "18" ~ "26",
      lake_id == "99" ~ "16",
      lake_id == "100" ~ "11",
      lake_id == "136" ~ "13",
      lake_id == "146" ~ "4",
      lake_id == "147" ~ "1",
      lake_id == "148" ~ "7",
      lake_id == "166" ~ "6",
      lake_id == "184" ~ "3",
      lake_id == "186" ~ "8",
      lake_id == "190" ~ "8",
      lake_id == "206" ~ "2",
      TRUE ~ "")) # blank if no match; will only occur if lake_id is missing
  
  return(data)
  
}

# Read in root path for anion data analyzed in ADA.

cin.ada.path <- paste0(userPath,
                       "data/chemistry/anions_ada_daniels/ADA/")

# 2022 anion data

anions_2022_146_190 <- 
  get_ada_data(cin.ada.path, "2022/EPAGPA076_146_190_anions.xls") %>%
  conv_units(filename = "EPAGPA076_146_190_anions.xls") %>%
  site_id_number %>%
  dup_agg %>% # aggregate lab duplicates (optional)
  flag_agg

anions_2022_184_166 <- 
  get_ada_data(cin.ada.path, "2022/EPAGPA076_184_166_anions.xls") %>%
  conv_units(filename = "EPAGPA076_184_166_anions.xls") %>%
  site_id_number %>%
  dup_agg %>% # aggregate lab duplicates (optional)
  flag_agg

anions_2022_136_100_206 <- 
  get_ada_data(cin.ada.path, "2022/EPAGPA081_136_100_206_anions.xls") %>%
  conv_units(filename = "EPAGPA081_136_100_206_anions.xls") %>%
  site_id_number %>%
  dup_agg %>% # aggregate lab duplicates (optional)
  flag_agg

anions_2022_011_003 <- 
  get_ada_data(cin.ada.path, "2022/EPAGPA081_011_003_anions.xls") %>%
  conv_units(filename = "EPAGPA081_011_003_anions.xls") %>%
  site_id_number %>%
  dup_agg %>% # aggregate lab duplicates (optional)
  flag_agg

# 2023 anion data

anions_2023_099_004 <- 
  get_ada_data(cin.ada.path, "2023/EPAGPA100_099_004_anions.xlsx") %>%
  conv_units(filename = "EPAGPA100_099_004_anions.xlsx") %>%
  site_id_number %>%
  dup_agg %>% # aggregate lab duplicates (optional)
  flag_agg

anions_2023_186_018 <- 
  get_ada_data(cin.ada.path, "2023/EPAGPA106_186_018_anions.xlsx") %>%
  conv_units(filename = "EPAGPA106_186_018_anions.xlsx") %>%
  site_id_number %>%
  dup_agg %>% # aggregate lab duplicates (optional)
  flag_agg

anions_2023_148_147 <- 
  get_ada_data(cin.ada.path, "2023/EPAGPA108_148_147_anions.xlsx") %>%
  conv_units(filename = "EPAGPA108_148_147_anions.xlsx") %>%
  site_id_number %>%
  dup_agg %>% # aggregate lab duplicates (optional)
  flag_agg


### JOIN DATA OBJECTS------------------------------------------------------------

# Join all of the data objects
ada.anions <- list(jea = list(jea1), key = list(key1), 
                   ove = list(ove1), 
                   ada_22_1 = list(anions_2022_146_190), 
                   ada_22_2 = list(anions_2022_184_166), 
                   ada_22_3 = list(anions_2022_136_100_206), 
                   ada_22_4 = list(anions_2022_011_003), 
                   ada_23_1 = list(anions_2023_099_004),
                   ada_23_2 = list(anions_2023_186_018),
                   ada_23_3 = list(anions_2023_148_147)) %>% 
  map(~ reduce(.x, left_join)) %>% 
  reduce(full_join) %>% 
  select(-starts_with("no"), -starts_with("op")) %>% # remove no2, no3, and op
  mutate(site_id = as.numeric(site_id)) %>%
  arrange(lake_id) 


### SAMPLE INVENTORY------------------------------------------------------------
# ADA anion samples collected, per master sample list (chemSampleList.R)
ada.anions.collected <- chem.samples.foo %>% 
  filter(lab == "ADA",
         analyte_group == "anions") %>%
  select(lake_id, sample_type, sample_depth, analyte)
  

# ADA anion samples analyzed
ada.anions.analyzed <- ada.anions %>% 
  select(lake_id, sample_type, sample_depth, br, cl, f, so4) %>%
  pivot_longer(cols = !c(lake_id, sample_type, sample_depth),
               names_to = "analyte") %>%
  select(-value)

# Are all collected ADA anion samples in ADA data?
# yes
setdiff(ada.anions.collected, ada.anions.analyzed) %>% print(n=Inf)

# Are all analyzed ADA anion samples in list of ADA collected samples?
setdiff(ada.anions.analyzed, ada.anions.collected) %>% print(n=Inf)

