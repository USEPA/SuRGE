# File for reading doc and toc from ADA.
# See ...data/chemistry/oc_ada_masi/ADA/.... for data.

# See Wiki for analyte name and unique identifier conventions.


# Function to read-in and collate data from excel files 
get_ada_data21 <- function(path, datasheet) { 
  
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

  # # maintable combines toptable with results
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
                  ~ if_else(str_detect(., "ND"), "<", NA_character_),
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
    mutate(across(ends_with("analyzed"), # determine if holding time exceeded
                  ~ if_else(.>28, TRUE, FALSE))) %>% # TRUE = hold time violation
    mutate(site_id = "") # create empty column for site_id (id is populated later)

  return(maintable)

}


# Function to convert units, rename columns, and add units columns
conv_units <- function(data, filename) {
  
  
  # filename <- toupper(filename) # use if case becomes an issue in file names.

  # TOC/DOC

  # note that no unit conversions were required for TOC/DOC data
    f <- data %>%
      mutate(across(ends_with("/L"), 
                    ~ case_when( # results are identical; no conversion needed. 
                      str_detect(paste(cur_column()), "mg/") ~ .*1, 
                      TRUE ~ .*1))) %>%
      rename(toc = contains("NPOC") & !ends_with(c("flag", "analyzed")),
             doc = contains("NPDOC") & !ends_with(c("flag", "analyzed")),
             toc_flag = contains("NPOC") & ends_with("flag"),
             doc_flag = contains("NPDOC") & ends_with("flag"),
             toc_qual = contains("NPOC") & ends_with("analyzed"),
             doc_qual = contains("NPDOC") & ends_with("analyzed")) %>%
      mutate(across(ends_with("oc"), 
                    ~ "mg_c_l",
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
                  ~ if_else(.<1, NA_character_, "<"))) %>% 
    filter(labdup != "LAB DUP") %>% # remove the lab dup
    select(-labdup) # remove labdup column. JB 12/7/2021
  
  return(e)
  
}

# create path for Lake Jean Neustadt
cin.ada.path <- paste0(userPath, 
                       "data/chemistry/oc_ada_masi/ADA/CH4_147_Lake Jean Neustadt/")

# apply get_ada_data21, conv_units, & dup_agg21 to Lake Jean Neustadt excel file
jea1 <- get_ada_data21(cin.ada.path, "EPAGPA054SS7773AE2.6Neustadt7-14-21NPOCNPDOCGPMS.xlsx") %>%
  conv_units(filename = "EPAGPA054SS7773AE2.6Neustadt7-14-21NPOCNPDOCGPMS.xlsx") %>%
  mutate(site_id = 1) %>% # add site_id
  dup_agg21 # aggregate lab duplicates (optional)


# create path for Keystone Lake
cin.ada.path <- paste0(userPath, 
                       "data/chemistry/oc_ada_masi/ADA/CH4_148_Keystone Lake/")

# apply get_ada_data21, conv_units, & dup_agg21 to Keystone Lake excel file
key1 <- get_ada_data21(cin.ada.path, "EPAGPA061SS7784AE2.6Keystone8-17-21NPOCNPDOCGPMS.xlsx") %>%
  conv_units(filename = "EPAGPA061SS7784AE2.6Keystone8-17-21NPOCNPDOCGPMS.xlsx") %>%
  mutate(site_id = 7) %>% # add site_id
  dup_agg21 # aggregate lab duplicates (optional)


# create path for Lake Overholser
cin.ada.path <- paste0(userPath, 
                       "data/chemistry/oc_ada_masi/ADA/CH4_167_Lake Overholser/")

# apply get_ada_data21, conv_units, & dup_agg21 to Lake Overholser excel file
ove1 <- get_ada_data21(cin.ada.path, "EPAGPA059SS7777AE2.6Overholser7-27-21NPOCNPDOCGPMS.xlsx") %>%
  conv_units(filename = "EPAGPA059SS7777AE2.6Overholser7-27-21NPOCNPDOCGPMS.xlsx") %>%
  mutate(site_id = 6) %>% # add site_id
  dup_agg21 # aggregate lab duplicates (optional)


# Join all of the data objects
ada.oc <- list(jea = list(jea1), key = list(key1), 
                      ove = list(ove1)) %>% 
  map_depth(1, function(x) reduce(x, left_join)) %>%
  reduce(full_join) %>%
  arrange(lake_id) %>%
  mutate(site_id = as.numeric(site_id)) %>% # make site id numeric
  ungroup()

