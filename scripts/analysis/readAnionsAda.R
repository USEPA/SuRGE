# ADA analyzing their own anions.  See ...data/chemistry/anions_ada_daniels/ADA/...

# See Wiki for analtye name and unique identifier conventions.



# Function to read-in and collate data from excel files 
get_ada_data21 <- function(path, datasheet) { 
  
  #'toptable' contains analyte names and MDL values
  toptable <- read_excel(paste0(path, datasheet), # get MDL & analyte names
                         sheet = "Data", range = "c8:S500") %>% # up to 492 rows
    janitor::remove_empty("rows") %>% # remove empty rows
    select(-(starts_with("."))) %>% # get rid of empty/unneeded columns
    rownames_to_column() %>% # transpose the tibble 
    pivot_longer(-rowname, 'variable', 'value') %>% # transpose the tibble
    pivot_wider(variable, rowname) %>% # transpose the tibble
    row_to_names(1) %>% # transpose the tibble
    select(starts_with("Analytes"), MDL, starts_with("Unit")) %>% # select only columns w/ analyte names, units, & MDL
    rename(Analytes = starts_with("Analytes")) %>%
    filter(str_detect(Analytes, "Analyte", negate = TRUE)) %>% # filter out superfluous "Analytes..."
    mutate(Analytes = str_c(Analytes, Unit)) %>% # concatenate analyte and unit, so unit is retained
    column_to_rownames(var = "Analytes") %>% # simpler to work w/ rownames in next chunk of code
    mutate(MDL = as.numeric(MDL)) # covert MDL values to numeric
  
  analyte_names <- row.names(toptable) # pass analyte names to maintable, below

  #'maintable' combines 'toptable' with results
  maintable <- read_excel(paste0(path, datasheet), # get the results
                          sheet = "Data", range = "A14:S500") %>% # up to 492 rows
    janitor::remove_empty("rows") %>% # remove empty rows
    janitor::clean_names() %>%
    mutate(lab_sample_id = toupper(lab_sample_id)) %>% # make uppercase since Ada isn't consistent
    mutate(labdup = if_else(str_detect(lab_sample_id, "LAB DUP"), "LAB DUP", "")) %>% # flag the dups
    select(field_sample_id, labdup, starts_with("dat")) %>% # remove unneeded columns
    rename_with(~paste0(analyte_names), .cols = starts_with("data")) %>% # rename using analyte names
    # Note: unlike other data files, all anions will have the same date analyzed
    mutate(date_analyzed = word(date_analyzed, -1)) %>% # extract last date from range of dates
    mutate(across(starts_with("date"), # convert all dates to as.Date format
                  ~ ifelse(str_detect(., "/"), as.Date(., format = "%m/%d/%Y"), as.Date(.)))) %>% 
    mutate(across(ends_with("/L"), # create qual column for all remaining analytes
                  ~ as.numeric(date_analyzed - date_collected),
                  .names = "{col}__date_analyzed")) %>%
    rename(sampleid = field_sample_id) %>% # temporary rename; changes later during text parsing
    filter(str_starts(sampleid, "\\(")) %>% # retain only rows where sampleid starts with '('
    select(sampleid, labdup, everything(),
           -date_collected, -date_analyzed) %>% # reorder columns for the following mutate()
    mutate(across(ends_with("/L"), # create new flag column if analyte not detected
                  ~ if_else(str_detect(., "ND"), "<", ""),
                  .names = "{col}_flag")) %>%
    mutate(across(ends_with("/L"), # replace ND with the MDL value from toptable
                  ~ ifelse(str_detect(., "ND"), toptable[paste(cur_column()),1], .))) %>% # note this is base::ifelse
    mutate(sampleid = str_replace_all(sampleid, "(TN or DN)","")) %>% # clean-up sampleid field
    mutate(sampleid = str_replace_all(sampleid, "\\(\\)","")) %>% # clean-up sampleid field
    mutate(sampleid = str_replace_all(sampleid, " ", "")) %>% # remove any blank spaces inside string
    mutate(across(!ends_with(c("flag", "labdup", "sampleid", "analyzed")), # remove 'BQL', 'RPD' & other junk from data fields
                  ~ str_extract(., pattern = "\\-*\\d+\\.*\\d*"))) %>%
    mutate(across(!ends_with(c("flag", "labdup", "sampleid", "analyzed")), # make extracted data numeric
                  ~ as.numeric(.))) %>%
    mutate(sample_depth = str_sub(sampleid, 4, 4)) %>% # get sample depth from sampleid
    mutate(sample_type = str_sub(sampleid, 5, 5)) %>% # get sample type from sampleid
    mutate(sampleid = str_sub(sampleid, 1, 3)) %>% # make sampleid 3-digit numeric lake id only
    mutate(sample_depth = str_replace_all(sample_depth, c("D" = "deep", "S" = "shallow", "N" = "blank"))) %>%
    mutate(sample_type = str_replace_all(sample_type, c("B" = "blank", "U" =  "unknown", "D" =  "duplicate"))) %>%
    rename(lake_id = sampleid) %>% # enforce project formatting.  See Wiki
    mutate(lake_id = as.character(as.numeric(lake_id))) %>% # consistent format for lake_id
    mutate(across(ends_with("analyzed"), # determine if holding time exceeded
                  ~ if_else(.>28, TRUE, FALSE))) %>%  # TRUE = hold time violation
    mutate(site_id = "") # create empty column for site_id (id is populated later)

  return(maintable)
  
}


# Function to convert units, rename columns, and add units columns
conv_units <- function(data, filename) {
  
  
  # filename <- toupper(filename) # use if case becomes an issue in file names.
  
  # Anions
  
  f <- data %>%
    mutate(across(ends_with("/L"), 
                  ~ case_when( # convert no2, no3, and op
                    str_detect(paste(cur_column()), "Nitr|Phos") ~ .*1000,
                    TRUE ~ .*1))) %>% # everything else should be correct as-is
    rename(f = contains("Fluo") & !ends_with(c("flag", "analyzed")),
           cl = contains("Chlo") & !ends_with(c("flag", "analyzed")),
           br = contains("Brom") & !ends_with(c("flag", "analyzed")),
           so4 = contains("Sulf") & !ends_with(c("flag", "analyzed")),
           no2 = contains("Nitrite") & !ends_with(c("flag", "analyzed")),
           no3 = contains("Nitrate") & !ends_with(c("flag", "analyzed")),
           op = contains("Phos") & !ends_with(c("flag", "analyzed")),
           f_flag = contains("Fluo") & ends_with("flag"),
           cl_flag = contains("Chlo") & ends_with("flag"),
           br_flag = contains("Brom") & ends_with("flag"),
           so4_flag = contains("Sulf") & ends_with("flag"),
           no2_flag = contains("Nitrite") & ends_with("flag"),
           no3_flag = contains("Nitrate") & ends_with("flag"),
           op_flag = contains("Phos") & ends_with("flag"),
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
    select(lake_id, site_id, sample_depth, sample_type, labdup, everything())
  
  return(f)
  
}


# Function to aggregate the lab duplicates
dup_agg21 <- function(data) {
  
  # first, convert all _flag columns to a numeric for summarize operations;
  # Must be performed in the event that a lab dup has a different flag than the sample.
  
  data <- data %>% 
    mutate(across(ends_with("flag"), 
                  ~ ifelse(str_detect(., "<"), 1, 0))) 
  
  # carve out the _flag and _units columns so they can be re-joined later
  c <- data %>% select(ends_with(c("id","labdup", "type", "depth", "units", "qual")))
  
  d <- data %>%
    dplyr::group_by(sample_depth, sample_type) %>%
    # '!' to exclude columns we don't want to aggregate
    summarize(across(!ends_with(c("id","labdup", "type", "depth", "units", "qual")), 
                     ~ mean(., na.rm = TRUE))) 
  
  e <- left_join(d, c, by = c("sample_depth", "sample_type")) %>% # rejoin the data
    mutate(across(3:last_col(), # convert NaN to NA
                  ~ ifelse(is.nan(.), NA, .))) %>% # must use ifelse here (not if_else)
    select(order(colnames(.))) %>% # alphabetize column names
    select(lake_id, site_id, sample_depth, sample_type, labdup, everything()) %>% # put 'sampleid' first
    mutate(across(ends_with("flag"), # convert all _flag values back to text
                  ~ if_else(.<1, "", "<"))) %>% 
    filter(labdup != "LAB DUP") %>% # remove the lab dup
    select(-labdup) # remove labdup column. 
  
  return(e)
  
}

# create path for Lake Jean Neustadt
cin.ada.path <- paste0(userPath, 
                       "data/chemistry/anions_ada_daniels/ADA/CH4_147_Lake Jean Neustadt/")

# apply get_ada_data21, conv_units, & dup_agg21 to Lake Jean Neustadt excel file
jea1 <- get_ada_data21(cin.ada.path, "EPAGPA054SS7773AE2.6Neustadt7-14-2021ClSO4BrFNO3NO2oPGPKR.xls") %>%
  conv_units(filename = "EPAGPA054SS7773AE2.6Neustadt7-14-2021ClSO4BrFNO3NO2oPGPKR.xls") %>%
  mutate(site_id = 1) %>% # add site_id
  dup_agg21 # aggregate lab duplicates (optional)


# create path for Keystone Lake
# This generates some warnings, but the output is correct 
cin.ada.path <- paste0(userPath, 
                       "data/chemistry/anions_ada_daniels/ADA/CH4_148_Keystone Lake/")

# apply get_ada_data21, conv_units, & dup_agg21 to Keystone Lake excel file
key1 <- get_ada_data21(cin.ada.path, "EPAGPA061SS7784AE2.6Keystone8-17-2021ClSO4BrFNO3NO2GPKR.xls") %>%
  conv_units(filename = "EPAGPA061SS7784AE2.6Keystone8-17-2021ClSO4BrFNO3NO2GPKR.xls") %>%
  mutate(site_id = 7) %>% # add site_id
  dup_agg21 # aggregate lab duplicates (optional)


# create path for Lake Overholser
cin.ada.path <- paste0(userPath, 
                       "data/chemistry/anions_ada_daniels/ADA/CH4_167_Lake Overholser/")

# apply get_ada_data21, conv_units, & dup_agg21 to Lake Overholser excel file
ove1 <- get_ada_data21(cin.ada.path, "EPAGPA059SS7777AE2.6Overh7-27-2021ClSO4BrFNO3NO2.xls") %>%
  conv_units(filename = "EPAGPA059SS7777AE2.6Overh7-27-2021ClSO4BrFNO3NO2.xls") %>%
  mutate(site_id = 6) %>% # add site_id
  dup_agg21 # aggregate lab duplicates (optional)


# Join all of the data objects
ada.anions <- list(jea = list(jea1), key = list(key1), 
                   ove = list(ove1)) %>% 
  map_depth(1, function(x) reduce(x, left_join)) %>% 
  reduce(full_join) %>% 
  select(-starts_with("no"), -starts_with("op")) %>% # remove no2, no3, and op
  arrange(lake_id)

