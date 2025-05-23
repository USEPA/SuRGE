# File for reading doc and toc from ADA.
# See ...data/chemistry/oc_ada_masi/ADA/.... for data.

# See Wiki for analyte name and unique identifier conventions.

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
    # rename_with(~paste0(analyte_names, "_date_analyzed"), 
    #             .cols = starts_with("date_a")) %>% # rename w/ analyte names
    mutate(across(ends_with("analyzed"), # select the latest date in range
                  ~ word(., -1) %>% 
                    as.Date(., tryFormats = c("%m/%d/%Y", "%Y-%m-%d")))) %>%
    mutate(across(ends_with("analyzed"), # compute holding time
                  ~ as.numeric(as.Date(., tryFormats = c("%m/%d/%Y", 
                                                         "%Y-%m-%d")) - 
                                 as.Date(date_collected, 
                                         format = "%m/%d/%Y")))) 
  
  
  # Pull column of date data (it doesn't matter which, since they're identical)
  analyzed_dates <- maintable_1 %>% select(contains("date_analyzed")) %>% pull()
  
  maintable_2.5 <- maintable_2 %>%
    mutate(across(ends_with("/L"), # create date columns for all analytes
                  ~ analyzed_dates, # copy the date data into all columns
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
    select(-field_sample_id) # no longer needed
  
  return(maintable_4)
  
}

# Function to convert units, rename columns, and add units columns

conv_units <- function(data, filename) {
  
  # TOC/DOC
  
  # note that no unit conversions were required for TOC/DOC data
  d <- data %>%
    mutate(across(ends_with("/L"), 
                  ~ case_when( # no conversion needed. 
                    str_detect(paste(cur_column()), "mg/") ~ . * 1, 
                    TRUE ~ . * 1))) %>%
    rename(toc = contains("NPOC") & !ends_with(c("flag", "bql", "analyzed")),
           doc = contains("NPDOC") & !ends_with(c("flag", "bql", "analyzed")),
           toc_flag = contains("NPOC") & ends_with("flag"),
           doc_flag = contains("NPDOC") & ends_with("flag"),
           toc_qual = contains("NPOC") & ends_with("analyzed"),
           doc_qual = contains("NPDOC") & ends_with("analyzed"), 
           toc_bql = contains("NPOC") & ends_with("bql"),
           doc_bql = contains("NPDOC") & ends_with("bql")) %>%
    mutate(across(ends_with("oc"), 
                  ~ "mg_c_l",
                  .names = "{col}_units")) 
  
  # select and order columns
  e <- d %>% 
    select(!starts_with("date_analyzed")) %>% # shouldn't need these
    select(order(colnames(.))) %>% # alphabetize and reorder columns
    select(lake_id, site_id, sample_depth, sample_type, labdup, everything())
  
  return(e)
  
}

# Function to aggregate the lab duplicates

dup_agg <- function(data) {
  
  # filter out any observations where value is NA
  
  data <- data %>% filter(!if_any(where(is.numeric), is.na))
  
  
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
  
  if ("doc" %in% colnames(data))
    f <- f %>%
      unite("doc_flags", doc_flag, doc_bql, doc_qual, sep = " ") 
  
  if ("toc" %in% colnames(data))
    f <- f %>%
      unite("toc_flags", toc_flag, toc_bql, toc_qual, sep = " ") 
  
  # If a sample and lab dup both have a flag (ND or L), retain only the L flag
  g <- f %>% 
    mutate(across(ends_with("flags"),
                  ~ if_else(
                    str_detect(., "ND L"), "L", .)) )
  
  # If there are no flags, enter NA in the _flags column
  h <- g %>%
    mutate(across(ends_with("flags"),
                  ~ if_else(str_detect(., "\\w"), ., NA_character_) %>%
                    str_squish())) # remove any extra white spaces
  
  return(h)
  
}

# ADA OC 2021-------------------------------------------------------------------

# create path for Lake Jean Neustadt
cin.ada.path <- 
  paste0(userPath,
         "data/chemistry/oc_ada_masi/ADA/CH4_147_Lake Jean Neustadt/")

jea1 <- 
  get_ada_data(
    cin.ada.path, "EPAGPA054SS7773AE2.6Neustadt7-14-21NPOCNPDOCGPMS.xlsx") %>%
  mutate(site_id = "1") %>%
  conv_units(
    filename = "EPAGPA054SS7773AE2.6Neustadt7-14-21NPOCNPDOCGPMS.xlsx") %>%
  # add site_id
  dup_agg %>% # aggregate lab duplicates (optional)
  flag_agg

# create path for Keystone Lake
cin.ada.path <- paste0(userPath, 
                       "data/chemistry/oc_ada_masi/ADA/CH4_148_Keystone Lake/")

key1 <- 
  get_ada_data(
    cin.ada.path, "EPAGPA061SS7784AE2.6Keystone8-17-21NPOCNPDOCGPMS.xlsx") %>%
  mutate(site_id = "7") %>% # add site_id
  conv_units(
    filename = "EPAGPA061SS7784AE2.6Keystone8-17-21NPOCNPDOCGPMS.xlsx") %>%
  dup_agg %>% # aggregate lab duplicates (optional)
  flag_agg

# create path for Lake Overholser
cin.ada.path <- 
  paste0(userPath, "data/chemistry/oc_ada_masi/ADA/CH4_167_Lake Overholser/")

ove1 <- 
  get_ada_data(cin.ada.path, 
              "EPAGPA059SS7777AE2.6Overholser7-27-21NPOCNPDOCGPMS.xlsx") %>%
  mutate(site_id = "6") %>% # add site_id
  conv_units(
    filename = "EPAGPA059SS7777AE2.6Overholser7-27-21NPOCNPDOCGPMS.xlsx") %>%
  dup_agg %>% # aggregate lab duplicates (optional)
  flag_agg

# ADA OC 2022-2023-------------------------------------------------------------

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

# Read in root path for oc data analyzed in ADA.

cin.ada.path <- paste0(userPath, 
                       "data/chemistry/oc_ada_masi/ADA/")

# 2022 oc data

oc_2022_146_190 <- 
  get_ada_data(cin.ada.path, "2022/EPAGPA076_146_190_NPOCNPDOC.xlsx") %>%
  site_id_number %>% # add site_id for 2022 samples
  conv_units(filename = "EPAGPA076_146_190_NPOCNPDOC.xlsx") %>%
  dup_agg %>% # aggregate lab duplicates (optional)
  flag_agg

oc_2022_166 <- 
  get_ada_data(cin.ada.path, "2022/EPAGPA076_166_NPOCNPDOC.xlsx") %>%
  site_id_number %>% # add site_id for 2022 samples
  conv_units(filename = "EPAGPA076_166_NPOCNPDOC.xlsx") %>%
  dup_agg %>% # aggregate lab duplicates (optional)
  flag_agg

oc_2022_184 <- 
  get_ada_data(cin.ada.path, "2022/EPAGPA076_184_NPOCNPDOC.xlsx") %>%
  site_id_number %>% # add site_id for 2022 samples
  conv_units(filename = "EPAGPA076_184_NPOCNPDOC.xlsx") %>%
  dup_agg %>% # aggregate lab duplicates (optional)
  flag_agg

oc_2022_136_100 <- 
  get_ada_data(cin.ada.path, "2022/EPAGPA081_136_100_NPOCNPDOC.xlsx") %>%
  site_id_number %>% # add site_id for 2022 samples
  conv_units(filename = "EPAGPA081_136_100_NPOCNPDOC.xlsx") %>%
  dup_agg %>% # aggregate lab duplicates (optional)
  flag_agg

oc_2022_206 <- 
  get_ada_data(cin.ada.path, "2022/EPAGPA081_206_NPOCNPDOC.xlsx") %>%
  site_id_number %>% # add site_id for 2022 samples
  conv_units(filename = "EPAGPA081_206_NPOCNPDOC.xlsx") %>%
  dup_agg %>% # aggregate lab duplicates (optional)
  flag_agg

oc_2022_011_003 <- 
  get_ada_data(cin.ada.path, "2022/EPAGPA081_011_003_NPOCNPDOC.xlsx") %>%
  site_id_number %>% # add site_id for 2022 samples
  conv_units(filename = "EPAGPA081_011_003_NPOCNPDOC.xlsx") %>%
  dup_agg %>% # aggregate lab duplicates (optional)
  flag_agg

# 2023 oc data

oc_2023_099_004 <- 
  get_ada_data(cin.ada.path, "2023/EPAGPA100_099_004_NPOCNPDOC.xlsx") %>%
  site_id_number %>% # add site_id for 2022 samples
  conv_units(filename = "EPAGPA100_099_004_NPOCNPDOC.xlsx") %>%
  dup_agg %>% # aggregate lab duplicates (optional)
  flag_agg

oc_2023_018 <- 
  get_ada_data(cin.ada.path, "2023/EPAGPA106_018_NPOCNPDOC.xlsx") %>%
  site_id_number %>% # add site_id for 2022 samples
  conv_units(filename = "EPAGPA106_018_NPOCNPDOC.xlsx") %>%
  dup_agg %>% # aggregate lab duplicates (optional)
  flag_agg

oc_2023_186 <- 
  get_ada_data(cin.ada.path, "2023/EPAGPA106_186_NPOCNPDOC.xlsx") %>%
  site_id_number %>% # add site_id for 2022 samples
  conv_units(filename = "EPAGPA106_186_NPOCNPDOC.xlsx") %>%
  dup_agg %>% # aggregate lab duplicates (optional)
  flag_agg

oc_2023_147 <- 
  get_ada_data(cin.ada.path, "2023/EPAGPA108_147_NPOCNPDOC.xlsx") %>%
  site_id_number %>% # add site_id for 2022 samples
  conv_units(filename = "EPAGPA108_147_NPOCNPDOC.xlsx") %>%
  dup_agg %>% # aggregate lab duplicates (optional)
  flag_agg

oc_2023_148 <- 
  get_ada_data(cin.ada.path, "2023/EPAGPA108_148_NPOCNPDOC.xlsx") %>%
  site_id_number %>% # add site_id for 2022 samples
  conv_units(filename = "EPAGPA108_148_NPOCNPDOC.xlsx") %>%
  dup_agg %>% # aggregate lab duplicates (optional)
  flag_agg





# JOIN DATA OBJECTS-------------------------------------------------------------

# Join all of the data objects
ada.oc <- list(
  jea1, 
  key1, 
  ove1, 
  oc_2022_146_190, 
  oc_2022_166, 
  oc_2022_184,
  oc_2022_136_100, 
  oc_2022_206,
  oc_2022_011_003, 
  oc_2023_099_004, 
  oc_2023_018, 
  oc_2023_186, 
  oc_2023_147, 
  oc_2023_148) %>% 
  reduce(full_join) %>%
  arrange(lake_id) %>%
  mutate(site_id = as.numeric(site_id)) %>% # make site id numeric
  ungroup()


# SAMPLE INVENTORY------------------------------------------------------------
# ADA OC samples collected, per master sample list (chemSampleList.R)
ada.oc.collected <- chem.samples.foo %>% 
  filter(lab == "ADA", # all 2021 nutrients stored and analyzed at ADA
         analyte_group == "organics") %>%
  select(lake_id, sample_type, sample_depth, analyte)


# ADA OC samples analyzed
ada.oc.analyzed <- ada.oc %>% 
  select(lake_id, sample_type, sample_depth, doc, toc) %>%
  pivot_longer(cols = !c(lake_id, sample_type, sample_depth),
               names_to = "analyte") %>%
  select(-value)

# Are all ADA collected OC in ADA data?
# yes
setdiff(ada.oc.collected, ada.oc.analyzed) %>% print(n=Inf)

# Are all nutrient samples analyzed at ADA in list of samples sent to ADA?
# yes
setdiff(ada.oc.analyzed, ada.oc.collected) %>% print(n=Inf)

