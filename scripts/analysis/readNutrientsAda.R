# NUTRIENTS ANALYZED IN ADA GENERAL PARAMETERS LABORATORY

# FUNCTIONS TO EXTRACT DATA FROM ADA EXCEL FILES--------------------------------

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
  
  # maintable_1: combine toptable with results, make uppercase, flag lab dups
  
  maintable_1 <- read_excel(paste0(path, datasheet), # get the results
                            sheet = "Data", 
                            range = "A14:N500") %>% # up to 492 rows
    janitor::remove_empty("rows") %>% # remove empty rows
    janitor::clean_names() %>%
    mutate(lab_sample_id = toupper(lab_sample_id)) %>% # make uppercase 
    mutate(labdup = if_else(str_detect(lab_sample_id, "LAB DUP"), 
                            "LAB DUP", "")) # identify the dups
  
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
  
  
  # maintable_3: remove extra rows, create ND flag and apply MDL value, 
  # create L (i.e., BQL) flag
  
  maintable_3 <- maintable_2 %>%
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

# Function to convert units, rename columns, and add units columns

conv_units <- function(data, filename) {
   
# Flow: Series of non-nested 'if' conditions that evaluate file names;
   # conditions are mutually exclusive due to Ada lab file name conventions.
   # If TRUE, proceeds to convert units, rename columns, & add units columns.
   # Then, if the object includes no3 data, create new column for no3 qual. 

   # AMMONIUM, NITRATE, NITRITE
  if (str_detect(paste(filename), "NH4")) 
    f <- data %>%
      mutate(across(ends_with("/L"), 
                    ~ case_when(
                      str_detect(paste(cur_column()), "mg/") ~ .*1000, 
                      TRUE ~ .*1))) %>%
      rename(nh4 = contains("NH4") & !ends_with(c("flag", "bql", "analyzed")), 
             no2_3 = contains("NO3") & contains("NO2") & 
               !ends_with(c("flag", "bql", "analyzed")),
             no3 = contains("NO3") & !contains("NO2") & 
               !ends_with(c("flag", "bql", "analyzed")),
             no2 = contains("NO2") & !contains("NO3") & 
               !ends_with(c("flag", "bql", "analyzed")), 
             nh4_flag = contains("NH4") & ends_with("flag"),
             no2_3_flag = contains("NO3") & contains("NO2") & 
               ends_with("flag"),
             no3_flag = contains("NO3") & !contains("NO2") & 
               ends_with("flag"),
             no2_flag = contains("NO2") & !contains("NO3") & 
               ends_with("flag"), 
             nh4_qual = contains("NH4") & ends_with("analyzed"), 
             no2_3_qual = contains("NO3") & contains("NO2") & 
               ends_with("analyzed"),
             no2_qual = contains("NO2") & !contains("NO3") & 
               ends_with("analyzed"), 
             nh4_bql = contains("NH4") & ends_with("bql"), 
             no2_3_bql = contains("NO3") & contains("NO2") & 
               ends_with("bql"),
             no3_bql = contains("NO3") & !contains("NO2") & 
               ends_with("bql"),
             no2_bql = contains("NO2") & !contains("NO3") & 
               ends_with("bql")) %>%
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
      rename(tn = contains("TN") & !ends_with(c("flag", "bql", "analyzed")),
             tp = contains("TP") & !ends_with(c("flag", "bql", "analyzed")),
             tn_flag = contains("TN") & ends_with("flag"),
             tp_flag = contains("TP") & ends_with("flag"), 
             tn_bql = contains("TN") & ends_with("bql"),
             tp_bql = contains("TP") & ends_with("bql"),
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
      rename(op = contains("oP") & !ends_with(c("flag", "bql", "analyzed")),
             op_flag = contains("oP") & ends_with("flag"),
             op_bql = contains("oP") & ends_with("bql"),
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
    select(lake_id, sample_depth, sample_type, labdup, everything())
  
  return(f)
   
}

# Function to aggregate the lab duplicates

dup_agg <- function(data) {
  
  # filter out any observations where value is NA
  
  data <- data %>% filter(!if_any(where(is.numeric), is.na))
  
  # first, convert all _flag columns to a numeric for summarize operations;
  # Must be performed in case lab dup has a different flag than the sample.
  
  data <- data %>% 
    mutate(across(ends_with(c("flag", "bql", "qual")), 
                  ~ if_else(str_detect(., "ND|L|H"), 1, 0))) 
  
  
  # carve out the _flag and _units columns so they can be re-joined later
  
  c <- data %>% select(ends_with(c("id","labdup", "type", "depth", 
                                   "filter", "units")))
  
  # summarize means of analyte values and flags 
  
  d <- data %>%
    dplyr::group_by(lake_id, sample_depth, sample_type) %>%
    # '!' to exclude columns we don't want to aggregate
    summarize(across(!ends_with(c("site_id","labdup", "type", "depth", 
                                  "filter", "units")), 
                     ~ mean(., na.rm = TRUE))) 
  
  # rejoin data 
  
  e <- left_join(d, c, by = c("lake_id", "sample_depth", "sample_type")) %>% 
    mutate(across(4:last_col(), # convert NaN to NA
                  ~ ifelse(is.nan(.), NA, .))) %>% # must use 'ifelse' here 
    select(order(colnames(.))) %>% # alphabetize column names
    select(lake_id, site_id, sample_depth, sample_type, 
           sample_filter, labdup, everything()) %>% # put 'sampleid' first
    # if both sample and dup had a flag, value = 1. If only one had a flag, 
    # value = 0.5. In both cases, flag is retained in aggregated observation.
    mutate(across(ends_with("flag"), # convert all _flag values back to text
                  ~ if_else(.< 0.5, "", "ND"))) %>% 
    mutate(across(ends_with("bql"), # convert all _flag values back to text
                  ~ if_else(.< 0.5, "", "L"))) %>% 
    mutate(across(ends_with("qual"), # convert all _flag values back to text
                  ~ if_else(.< 0.5, "", "H"))) %>% 
    filter(labdup != "LAB DUP") %>% # remove the lab dup
    select(-labdup) %>% # remove labdup column. JB 12/7/2021
    ungroup()
  
  return(e)
  
}

# Function to aggregate all flags into a single column

flag_agg <- function(data) { # merge the flag columns for each analyte
  
  f <- data
  
  if ("nh4" %in% colnames(data))
    f <- f %>%
      unite("nh4_flags", nh4_flag, nh4_bql, nh4_qual, sep = " ") 
  
  if ("no2_3" %in% colnames(data))
    f <- f %>%
      unite("no2_3_flags", no2_3_flag, no2_3_bql, no2_3_qual, sep = " ") 
  
  if ("no2" %in% colnames(data))
    f <- f %>%
      unite("no2_flags", no2_flag, no2_bql, no2_qual, sep = " ")
  
  if ("no3" %in% colnames(data))
    f <- f %>%
      unite("no3_flags", no3_flag, no3_bql, no3_qual, sep = " ")
  
  
  if ("tp" %in% colnames(data))
    f <- f %>%
      unite("tp_flags", tp_flag, tp_bql, tp_qual, sep = " ") %>%
      unite("tn_flags", tn_flag, tn_bql, tn_qual, sep = " ")
  
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

# 2021 NUTRIENT SAMPLES---------------------------------------------------------

# ANALYZED IN ADA LABORATORY.  FORMAT OF 2021 DATA REPORTS ARE UNIQUE TO
# 2021, but the functions above can be used across all years.

# create path for Lake Jean Neustadt

cin.ada.path <- paste0(userPath, 
                       "data/chemistry/nutrients/ADA/CH4_147_Lake Jean Neustadt/")

jea1 <- get_ada_data(cin.ada.path, 
                     "EPAGPA054,SS#7773,AE2.6,Forshay,7-14-21,oP,GPKR.xls") %>%
  conv_units(filename = 
               "EPAGPA054,SS#7773,AE2.6,Forshay,7-14-21,oP,GPKR.xls") %>%
  mutate(site_id = "1") %>% # add site_id, U-01
  mutate(sample_filter = "filtered") %>% # op is filtered
  dup_agg %>% # aggregate lab duplicates (optional) 
  flag_agg # merge flag columns


jea2 <- get_ada_data(cin.ada.path, 
                     "EPAGPA054SS#7773,AE2.6,Forshay,7-14-21,TNTPGPKR.xls") %>%
  conv_units("EPAGPA054SS#7773,AE2.6,Forshay,7-14-21,TNTPGPKR.xls") %>%
  mutate(site_id = "1") %>% # add site_id, U-01
  mutate(sample_filter = "unfiltered") %>% # tn tp is unfiltered
  dup_agg %>% # aggregate lab duplicates (optional)
  flag_agg # merge flag columns

jea3 <- get_ada_data(cin.ada.path, 
                     "EPAGPA054SS#7773AE2.6Forshay,7-14-21,NO3NO2NH4.xlsx") %>%
  conv_units("EPAGPA054SS#7773AE2.6Forshay,7-14-21,NO3NO2NH4.xlsx") %>%
  mutate(site_id = "1") %>% # add site_id, U-01
  mutate(sample_filter = "filtered") %>% # no2 no3 nh4 is filtered
  dup_agg %>% # aggregate lab duplicates (optional)
  flag_agg # merge flag columns

# create path for Keystone Lake

cin.ada.path <- paste0(userPath, 
                       "data/chemistry/nutrients/ADA/CH4_148_Keystone Lake/")

key1 <- get_ada_data(cin.ada.path, 
                     "EPAGPA061,SS#7784,AE2.6,Forshay,8-17-21,oP,GPKR.xls") %>%
  conv_units("EPAGPA061,SS#7784,AE2.6,Forshay,8-17-21,oP,GPKR.xls") %>%
  mutate(site_id = "7") %>% # add site_id, U-07
  mutate(sample_filter = "filtered") %>% # op is filtered
  dup_agg %>% # aggregate lab duplicates (optional)
  flag_agg # merge flag columns

key2 <- get_ada_data(cin.ada.path, 
                     "EPAGPA061SS#7784,AE2.6,Forshay,8-17-21,TN,TP,GPKR.xls") %>%
  conv_units("EPAGPA061SS#7784,AE2.6,Forshay,8-17-21,TN,TP,GPKR.xls") %>%
  mutate(site_id = "7") %>% # add site_id, U-07
  mutate(sample_filter = "unfiltered") %>% # tn tp is unfiltered
  dup_agg %>% # aggregate lab duplicates (optional)
  flag_agg # merge flag columns

key3 <- get_ada_data(cin.ada.path, 
                     "EPAGPA061SS#7784AE2.6Forshay,8-17-21NO3+NO2NH4NO2NO3GPMS.xlsx") %>%
  conv_units("EPAGPA061SS#7784AE2.6Forshay,8-17-21NO3+NO2NH4NO2NO3GPMS.xlsx") %>%
  mutate(site_id = "7") %>% # add site_id, U-07
  mutate(sample_filter = "filtered") %>% # no2 no3 nh4 is filtered
  dup_agg %>%# aggregate lab duplicates (optional)
  flag_agg # merge flag columns

# create path for Lake Overholser

cin.ada.path <- paste0(userPath, 
                       "data/chemistry/nutrients/ADA/CH4_167_Lake Overholser/")

ove1 <- get_ada_data(cin.ada.path, 
                     "EPAGPA059,SS#7777,AE2.6,Forshay,7-27-21,oP,GPKR.xls") %>%
  conv_units("EPAGPA059,SS#7777,AE2.6,Forshay,7-27-21,oP,GPKR.xls") %>%
  mutate(site_id = "6") %>% # add site_id, U-06
  mutate(sample_filter = "filtered") %>% # op is filtered
  dup_agg %>% # aggregate lab duplicates (optional)
  flag_agg # merge flag columns

ove2 <- get_ada_data(cin.ada.path, 
                     "EPAGPA059SS#7777,AE2.6,Forshay,7-27-21,TN,TP,GPKR.xls") %>%
  conv_units("EPAGPA059SS#7777,AE2.6,Forshay,7-27-21,TN,TP,GPKR.xls") %>%
  mutate(site_id = "6") %>% # add site_id, U-06
  mutate(sample_filter = "unfiltered") %>% # tn tp is unfiltered
  dup_agg %>% # aggregate lab duplicates (optional)
  flag_agg # merge flag columns

# deleted "GPMS" from end of file name.  
# Full file name couldn't be read on Jake's laptop.

ove3 <- get_ada_data(cin.ada.path, 
                     "EPAGPA059SS#7777AE2.6Forshay,7-27-21NO3+NO2NH4NO2NO3.xlsx") %>%
  conv_units("EPAGPA059SS#7777AE2.6Forshay,7-27-21NO3+NO2NH4NO2NO3.xlsx") %>%
  mutate(site_id = "6") %>% # add site_id, U-06
  mutate(sample_filter = "filtered") %>% # no2 no3 nh4 is filtered
  dup_agg %>% # aggregate lab duplicates (optional)
  flag_agg # merge flag columns

# 2020 NUTRIENT SAMPLES-----------------------------------------------------------

# Nutrient samples for the 2020 SuRGE field season were held in Cincinnati,
# then shipped to ADA in May 2021 for analysis.  The reports are formatted
# differently than the 2021 data reports, but we have added an additional 
# function below to handle this data format. 

# 1. Read in root path for 2020 chemistry data analyzed in ADA.

cin.ada.path <- paste0(userPath, 
                       "data/chemistry/nutrients/2020cinSentToAda/")

# 2. Read in files containing unique sample IDs.

tot.id <- read_excel(paste0(cin.ada.path, "totalNutrientSampleIds.xlsx"))
diss.id <- read_excel(paste0(cin.ada.path, "dissolvedNutrientSampleIds.xlsx"))
id <- rbind(tot.id, diss.id) %>% select(lake_id, site_id) %>% distinct() %>%
  mutate(lake_id = as.numeric(lake_id) %>% as.character(),
         # extract number from site_id, convert to char to match other data
         site_id = as.character(abs(parse_number(site_id))))

# 3. Read data

# OP

lmp1 <- get_ada_data(cin.ada.path, 
                     "EPAGPA053SS#7759AE2.6ForshayLakeMethaneProject7-6-20oPRev1GPKR.xls") %>%
  conv_units(filename = "EPAGPA053SS#7759AE2.6ForshayLakeMethaneProject7-6-20oPRev1GPKR.xls") %>%
  left_join(., id) %>% # add site_id
  mutate(sample_filter = "filtered") %>% # op is filtered
  dup_agg %>% # aggregate the lab duplicates (optional)
  flag_agg # merge flag columns

# TN TP

# 2020 Ada TN TP data are a special case: the data contain both D (dissolved) 
# and T (total) samples. We want T (i.e., nonfiltered) samples only. 

# Function to extract "T" or "D" from sample ID field in 2020 TN TP data.
# Note that this adds a sample_filter column in a row-wise fashion; we can do 
# so since the order of the rows does not change until dup_agg function.

find_t_rows <- function(path, datasheet) {
  
  rows <- read_excel(paste0(path, datasheet), # get the results
                     sheet = "Data", range = "A14:N500", na = "-") %>% # up to 492 rows
    janitor::remove_empty("rows") %>%
    select(field_sample_id = 1) %>% # keep only the sample id column
    filter(str_starts(field_sample_id, "TN|DN")) %>% # keep only data rows
    dplyr::transmute(sample_filter = str_sub(
      field_sample_id, 1, 1)) %>% # keep only first letter 
    mutate(sample_filter = str_replace_all(
      sample_filter, c("D" =  "filtered", "T" = "unfiltered")))

  return(rows)
    
}

lmp2 <- get_ada_data(cin.ada.path, 
                     "EPAGPA053SS#7759,AE2.6,Forshay,LakeMethaneProject,7-6-20,TNTP,GPKR.xls") %>%
  conv_units(filename = 
               "EPAGPA053SS#7759,AE2.6,Forshay,LakeMethaneProject,7-6-20,TNTP,GPKR.xls") %>%
  left_join(., id) %>% # add site_id
  mutate(find_t_rows( #  apply function to get sample_filter
    cin.ada.path, 
    "EPAGPA053SS#7759,AE2.6,Forshay,LakeMethaneProject,7-6-20,TNTP,GPKR.xls")) %>% 
  filter(sample_filter == "unfiltered") %>% # keep only nonfiltered TN TP samples
  dup_agg %>% # aggregate the lab duplicates (optional)
  flag_agg # merge flag columns

# NO2, NO3, NO2+NO3, NH4

lmp3 <- get_ada_data(cin.ada.path, 
                     "EPAGPA053SS#7759ForshayLakeMethaneProject7-6-2020NO3+NO2NH4GPMS.xls") %>%
  conv_units(filename = 
               "EPAGPA053SS#7759ForshayLakeMethaneProject7-6-2020NO3+NO2NH4GPMS.xls") %>%
  left_join(., id) %>% # add site_id
  mutate(sample_filter = "filtered") %>% # no2 no3 nh4 is filtered
  dup_agg %>% # aggregate the lab duplicates (optional)
  flag_agg # merge flag columns

# Check if all records have site_id

lapply(list(lmp1, lmp2, lmp3), function(x) any(is.na(x$site_id))) 



# 2022 NUTRIENT SAMPLES---------------------------------------------------------

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
    TRUE ~ "")) # blank if no match; this will only occur if lake_id is missing
  
  return(data)
  
}

# Read in root path for 2022 chemistry data analyzed in ADA.

cin.ada.path <- paste0(userPath, 
                       "data/chemistry/nutrients/ADA/2022/")

no2no3nh4_2022_146_190_184_166 <-
  get_ada_data(cin.ada.path, "EPAGPA076_146_190_184_166_NO3+NO2NH4.xlsx") %>%
  conv_units(filename = "EPAGPA076_146_190_184_166_NO3+NO2NH4.xlsx") %>%
  site_id_22 %>% # add site_id for 2022 samples
  mutate(sample_filter = "filtered") %>% # no2 no3 nh4 is filtered
  dup_agg %>% # aggregate lab duplicates (optional)
  flag_agg # merge flag columns for each analyte

tntp_2022_146_190_184_166 <- 
  get_ada_data(cin.ada.path, "EPAGPA076_146_190_184_166_TN,TP.xls") %>%
  conv_units(filename = "EPAGPA076_146_190_184_166_TN,TP.xls") %>%
  site_id_22 %>% # add site_id for 2022 samples
  mutate(sample_filter = "unfiltered") %>% # tn tp is unfiltered
  dup_agg %>% # aggregate lab duplicates (optional)
  flag_agg # merge flag columns for each analyte

op_2022_146_190 <- 
  get_ada_data(cin.ada.path, "EPAGPA076_146_190_oP.xls") %>%
  conv_units(filename = "EPAGPA076_146_190_oP.xls") %>%
  site_id_22 %>% # add site_id for 2022 samples
  mutate(sample_filter = "filtered") %>% # op is filtered
  dup_agg %>% # aggregate lab duplicates (optional)
  flag_agg # merge flag columns for each analyte

op_2022_166 <- 
  get_ada_data(cin.ada.path, "EPAGPA076_166_oP.xls") %>%
  conv_units(filename = "EPAGPA076_166_oP.xls") %>%
  site_id_22 %>% # add site_id for 2022 samples
  mutate(sample_filter = "filtered") %>% # op is filtered
  dup_agg  %>% # aggregate lab duplicates (optional)
  flag_agg # merge flag columns for each analyte

op_2022_184 <- 
  get_ada_data(cin.ada.path, "EPAGPA076_184_oP.xls") %>%
  conv_units(filename = "EPAGPA076_184_oP.xls") %>%
  site_id_22 %>% # add site_id for 2022 samples
  mutate(sample_filter = "filtered") %>% # op is filtered
  dup_agg  %>% # aggregate lab duplicates (optional)
  flag_agg # merge flag columns for each analyte

no2no3nh4_2022_136_100_206 <- 
  get_ada_data(cin.ada.path, "EPAGPA081_136_100_206_NO3+NO2NH4.xlsx") %>%
  conv_units(filename = "EPAGPA081_136_100_206_NO3+NO2NH4.xlsx") %>%
  site_id_22 %>% # add site_id for 2022 samples
  mutate(sample_filter = "filtered") %>% # # no2 no3 nh4 is filtered
  dup_agg  %>% # aggregate lab duplicates (optional)
  flag_agg # merge flag columns for each analyte

tntp_2022_136_100_206 <- 
  get_ada_data(cin.ada.path, "EPAGPA081_136_100_206_TN,TP.xls") %>%
  conv_units(filename = "EPAGPA081_136_100_206_TN,TP.xls") %>%
  site_id_22 %>% # add site_id for 2022 samples
  mutate(sample_filter = "unfiltered") %>% # tn tp is unfiltered
  dup_agg  %>% # aggregate lab duplicates (optional)
  flag_agg # merge flag columns for each analyte

op_2022_136_100_206 <- 
  get_ada_data(cin.ada.path, "EPAGPA081_136_100_206_oP.xls") %>%
  conv_units(filename = "EPAGPA081_136_100_206_oP.xls") %>%
  site_id_22 %>% # add site_id for 2022 samples
  mutate(sample_filter = "filtered") %>% # op is filtered
  dup_agg  %>% # aggregate lab duplicates (optional)
  flag_agg # merge flag columns for each analyte




# JOIN ALL DATA OBJECTS------------------------------------------------------------

# Join all of the data objects
ada.nutrients <- list(jea = list(jea1, jea2, jea3), 
                      key = list(key1, key2, key3), 
                      ove = list(ove1, ove2, ove3), 
                      lmp = list(lmp1, lmp2, lmp3),
                      ada22_1 = list(no2no3nh4_2022_146_190_184_166, 
                                     tntp_2022_146_190_184_166, 
                                     full_join(op_2022_146_190, 
                                     full_join(op_2022_166, op_2022_184))), 
                      ada22_2 = list(no2no3nh4_2022_136_100_206,
                                     tntp_2022_136_100_206,
                                     op_2022_136_100_206)) %>% 
  map_depth(2, ~ select(.x, -sample_filter)) %>%
  map(~ reduce(.x, left_join)) %>%
  reduce(full_join) %>%
  mutate(site_id = as.numeric(site_id)) %>%
  arrange(lake_id) 




# INSPECT FINAL MERGED DATA OBJECT-------------------------------------------

# Inspect final object for merge errors
# any duplicate rows for a given set of unique identifiers?  No, good!
ada.nutrients %>% select(lake_id, site_id, sample_depth, sample_type) %>%
   janitor::get_dupes()

# Did we preserve all combinations of unique identifiers in original data?
# 92 unique combinations of lake_id, site_id, sample_depth, and sample_type
# in original data.
list(jea1, jea2, jea3, key1, key2, key3, 
     ove1, ove2, ove3, lmp1, lmp2, lmp3, 
     op_2022_146_190, 
     op_2022_166, 
     op_2022_184, 
     op_2022_136_100_206, 
     tntp_2022_146_190_184_166,
     tntp_2022_136_100_206,
     no2no3nh4_2022_146_190_184_166,
     no2no3nh4_2022_136_100_206) %>%
  map(~select(., lake_id, site_id, sample_depth, sample_type)) %>%
  map_dfr(~bind_rows(.)) %>% # bind all df by rows, creates one df
  distinct() %>% # condense to unique observations
  nrow(.) # 92 unique combinations

# 92 unique combinations in merged df.  Everything looks good
ada.nutrients %>% select(lake_id, site_id, sample_depth, sample_type) %>%
  distinct() %>% {nrow(.)}


# Inspect flags for any unusual formatting
ada.nutrients %>% select(contains("flags")) %>% print(n=Inf)

# Inspect units
unique(ada.nutrients$op_units) # good
unique(ada.nutrients$tn_units) # good
unique(ada.nutrients$tp_units) # good
unique(ada.nutrients$nh4_units) # good
unique(ada.nutrients$no2_3_units) # good
unique(ada.nutrients$no2_units) # includes NA?
unique(ada.nutrients$no3_units) # includes NA

# Random inspection
sample_frac(ada.nutrients, 0.1) %>% select(!contains("units")) %>% print(n=Inf)


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


