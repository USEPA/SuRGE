# File for reading doc and toc from ADA.
# See ...data/chemistry/TOC.DOC/ADA/.... for data.
# See issue 8 for key to sample id values.
# See issue 10 for analyte names.

get_ada_data21 <- function(path, datasheet) { 
  
  # for 2020 data (and 2022 onward), as field_sample_id is in different format
  
  #'toptable' contains analyte names and MDL values
  toptable <- read_excel(paste0(path, datasheet), # get MDL & analyte names
                         sheet = "Data", range = "c8:N500") %>% # up to 492 rows
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
                  ~ if_else(str_detect(., "ND"), "<", ""),
                  .names = "{col}_flag")) %>%
    mutate(across(ends_with("/L"), # replace ND with the MDL value from toptable
                  ~ ifelse(str_detect(., "ND"), toptable[paste(cur_column()),1], .))) %>% # note this is base::ifelse
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
                  ~ if_else(.>28, TRUE, FALSE))) %>% 
    mutate(site_id = "") # create empty column for site_id (id is populated later)
  
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
      mutate(across(ends_with(c("tn", "tp")), 
                    ~ "ug_n_l",
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
                    ~ "ug_n_l",
                    .names = "{col}_units")) 
  
  # Check for an no3 column, then create no3_qual flag column
  if ("no3" %in% colnames(f))
    f <- f %>% 
      mutate(no3_qual = case_when( # check if either no2 or no2_3 has qual flag
        no2_qual == TRUE ~ TRUE,
        no2_3_qual == TRUE ~ TRUE, 
        TRUE ~ FALSE))
  
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
                  ~ if_else(.<1, "", "<"))) %>% 
    filter(labdup != "LAB DUP") %>% # remove the lab dup
    select(-labdup) # remove labdup column. JB 12/7/2021
  
  return(e)
  
}

# create path for Lake Jean Neustadt
cin.ada.path <- paste0(userPath, 
                       "data/chemistry/TOC.DOC/ADA/CH4_147_Lake Jean Neustadt")

# apply get_ada_data and dup_agg functions to each spreadsheet for Lake Jean Neustadt

# file names are too long. will need to rename
jea1 <- get_ada_data21(cin.ada.path, "EPAGPA054SS#7773AE2.6ForshayLakeMethaneProject7-14-21NPOCNPDOCGPMS.xlsx") %>%
  conv_units(filename = "EPAGPA054SS#7773AE2.6ForshayLakeMethaneProject7-14-21NPOCNPDOCGPMS.xlsx") %>%
  # mutate(site_id = 1) %>% # add site_id 
  # mutate(sample_filter = "filtered") %>% # filtered or unfiltered, based on file name
  dup_agg21 # aggregate lab duplicates (optional)