


# NUTRIENTS-----------------------
# DO NOT FLAG BQL RESULTS FROM ADA

# 1NOV2021: commented out; superseded by new code below. 
# cin.ada.path <- paste0(userPath, 
#                            "data/chemistry/nutrients/ADA/CH4_147_Lake Jean Neustadt/")
# 
# cin.ada.Neustadt2021.op <- read_excel(paste0(cin.ada.path, 
#                                    "EPAGPA054,SS#7773,AE2.6,Forshay,7-14-21,oP,GPKR.xls"),
#                                    sheet = "Data", range = "A14:G19") %>%
#   janitor::clean_names() 
# 
# cin.ada.Neustadt2021.tntp <- read_excel(paste0(cin.ada.path,
#                                   "EPAGPA054SS#7773,AE2.6,Forshay,7-14-21,TNTPGPKR.xls"),
#                                   sheet = "Data", range = "A14:J19") %>%
#   janitor::clean_names() 
# 
# cin.ada.Neustadt2021.no3.n02.nh4 <- read_excel(paste0(cin.ada.path,
#                                   "EPAGPA054SS#7773AE2.6Forshay,7-14-21,NO3NO2NH4.xlsx"),
#                                    sheet = "Data", range = "A14:N19") %>%
#   janitor::clean_names()

# Function to extract data from Ada excel files
get_ada_data <- function(path, datasheet) { 
 

  #'toptable' contains analyte names and MDL values
 toptable <- read_excel(paste0(path, datasheet), # get MDL & analyte names
                    sheet = "Data", range = "c8:N19") %>%
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
   
z <<- (toptable)
 #'maintable' combines 'toptable' with results
 maintable <- read_excel(paste0(path, datasheet), # get the results
                         sheet = "Data", range = "A14:N19") %>%
   janitor::clean_names() %>%
   mutate(lab_sample_id = toupper(lab_sample_id)) %>% # make uppercase since Ada isn't consistent
   mutate(labdup = if_else(str_detect(lab_sample_id, "LAB DUP"), "LAB DUP", "")) %>% # flag the dups
   select(field_sample_id, labdup, starts_with("data")) %>% # remove unneeded columns
   rename_with(~row.names(toptable), .cols = starts_with("data")) %>% # rename using analyte names
   rename(sampleid = field_sample_id) %>% # temporary rename; changes later during text parsing
   select(sampleid, labdup, everything()) %>% # reorder columns for the following mutate() 
   mutate(across(3:last_col(), # create new flag column if analyte not detected
                 ~ if_else(str_detect(., "ND"), "<", ""),
                 .names = "{col}_flag")) %>%
   mutate(across(3:last_col(), # replace ND with the MDL value from toptable
                 ~ ifelse(str_detect(., "ND"), toptable[paste(cur_column()),1], .))) %>% # note this is base::ifelse
   mutate(sampleid = str_replace_all(sampleid, "(TN or DN)","")) %>% # clean-up sampleid field
   mutate(sampleid = str_replace_all(sampleid, "\\(\\)","")) %>% # clean-up sampleid field
   mutate(sampleid = str_replace_all(sampleid, " ", "")) %>% # remove any blank spaces inside string
   mutate(across(!ends_with(c("flag", "labdup", "sampleid")), # remove 'BQL', 'RPD' & other junk from data fields
                 ~ str_extract(., pattern = "\\-*\\d+\\.*\\d*"))) %>%
   mutate(across(!ends_with(c("flag", "labdup", "sampleid")), # make extracted data numeric
                 ~ as.numeric(.))) %>%
   #janitor::clean_names()  %>% # this is causing a problem by changing mu symbol to 'm'
   mutate(sample_depth = str_sub(sampleid, 4, 4)) %>% # get sample depth from sampleid
   mutate(sample_type = str_sub(sampleid, 5, 5)) %>% # get sample type from sampleid
   mutate(sampleid = str_sub(sampleid, 1, 3)) %>% # make sampleid 3-digit numeric lake id only
   mutate(sample_depth = str_replace_all(sample_depth, c("D" = "deep", "S" = "shallow", "N" = "nutrient blank"))) %>%
   mutate(sample_type = str_replace_all(sample_type, c("B" = "nutrient blank", "U" =  "unknown", "D" =  "duplicate"))) %>%
   dplyr::rename(lake_id = sampleid) %>% # change name to match chemCoc
   mutate(site_id = "") %>% # create empty column for site_id (id is populated later)
   select(order(colnames(.))) %>% # alphabetize column names
   select(lake_id, site_id, sample_depth, sample_type, labdup, everything()) # put id fields first

  return(maintable)

}

# Function to aggregate the lab dups from the data 
# This is a separate function, since we won't always want to aggregate.
dup_agg <- function(data) {
   
   # carve out the _flag columns so they can be re-joined later
   c <- data %>% select(ends_with(c("id","labdup", "type", "depth", "flag", "filter")))
   
   # group and summarize to obtain means                
   d <- data %>%
      dplyr::group_by(sample_depth, sample_type) %>%
      summarize(across(!ends_with(c("id","labdup", "type", "depth", "flag", "filter")), 
                       ~ mean(., na.rm = TRUE)))
   
   e <- left_join(d, c, by = c("sample_depth", "sample_type")) %>% # rejoin the data
      mutate(across(3:last_col(),
                    ~ ifelse(is.nan(.), NA, .))) %>% # must use ifelse here (not if_else)
      select(order(colnames(.))) %>% # alphabetize column names
      select(lake_id, site_id, sample_depth, sample_type, labdup, everything()) %>% # put 'sampleid' first
      filter(labdup != "LAB DUP") # remove the lab dup; we may want to revisit this.
   # note that the LAB DUP and the original now have identical values, but the < flags may differ
 
return(e)

}

# Function to convert units
conv_units <- function(data) {
   
   # Step: identify which columns & units are incorrect
      # 1Dec: passed units to output tibble in analyte names
         # parse column names into analyte name and units
         # try using dplyr::cur_column   
   # Step: convert units
      # nh4, no2_3, no2, no3, tn: ug_n_l
      # tp, op: ug_p_l
      # toc, doc: mg_c_l
   # Step: rename columns

   
   d <- data %>%
      
      mutate(across(ends_with("/L"), 
                    ~ case_when(
                       str_detect(paste(cur_column()), "mg/") ~ .*1000, 
                       TRUE ~ .*1)))

   # May be able to use same format to rename() analytes. 
   

  return(d)
   
}



# create path for Lake Jean Neustadt
cin.ada.path <- paste0(userPath, 
                       "data/chemistry/nutrients/ADA/CH4_147_Lake Jean Neustadt/")

# apply get_ada_data and dup_agg functions to each spreadsheet for Lake Jean Neustadt

jea1 <- get_ada_data(cin.ada.path, "EPAGPA054,SS#7773,AE2.6,Forshay,7-14-21,oP,GPKR.xls") %>%
   mutate(site_id = "U-01") %>% # add site_id
   mutate(sample_filter = "filtered") %>% # filtered or unfiltered, based on file name
   dup_agg

jea2 <- get_ada_data(cin.ada.path, "EPAGPA054SS#7773,AE2.6,Forshay,7-14-21,TNTPGPKR.xls") %>%
   mutate(site_id = "U-01") %>% # add site_id
   mutate(sample_filter = "unfiltered") %>% # filtered or unfiltered, based on file name
   dup_agg

jea3 <- get_ada_data(cin.ada.path, "EPAGPA054SS#7773AE2.6Forshay,7-14-21,NO3NO2NH4.xlsx") %>%
   mutate(sample_filter = "filtered") %>% # filtered or unfiltered, based on file name
   dup_agg
   


# create path for Keystone Lake
cin.ada.path <- paste0(userPath, 
                       "data/chemistry/nutrients/ADA/CH4_148_Keystone Lake/")

# apply get_ada_data and dup_agg functions to each spreadsheet for Keystone Lake 
key1 <- get_ada_data(cin.ada.path, "EPAGPA061,SS#7784,AE2.6,Forshay,8-17-21,oP,GPKR.xls") %>%
   mutate(site_id = "U-07") %>% # add site_id
   mutate(sample_filter = "filtered") %>% # filtered or unfiltered, based on file name
   dup_agg

key2 <- get_ada_data(cin.ada.path, "EPAGPA061SS#7784,AE2.6,Forshay,8-17-21,TN,TP,GPKR.xls") %>%
   mutate(site_id = "U-07") %>% # add site_id
   mutate(sample_filter = "unfiltered") %>% # filtered or unfiltered, based on file name
   dup_agg

key3 <- get_ada_data(cin.ada.path, "EPAGPA061SS#7784AE2.6Forshay,8-17-21NO3+NO2NH4NO2NO3GPMS.xlsx") %>%
   mutate(site_id = "U-07") %>% # add site_id
   mutate(sample_filter = "filtered") %>% # filtered or unfiltered, based on file name
   dup_agg


# create path for Lake Overholser
cin.ada.path <- paste0(userPath, 
                       "data/chemistry/nutrients/ADA/CH4_167_Lake Overholser/")

# apply get_ada_data and dup_agg functions to each spreadsheet for Lake Overholser
ove1 <- get_ada_data(cin.ada.path, "EPAGPA059,SS#7777,AE2.6,Forshay,7-27-21,oP,GPKR.xls") %>%
   mutate(site_id = "U-06") %>% # add site_id
   mutate(sample_filter = "filtered") %>% # filtered or unfiltered, based on file name
   dup_agg 

ove2 <- get_ada_data(cin.ada.path, "EPAGPA059SS#7777,AE2.6,Forshay,7-27-21,TN,TP,GPKR.xls") %>%
   mutate(site_id = "U-06") %>% # add site_id
   mutate(sample_filter = "unfiltered") %>% # filtered or unfiltered, based on file name
   dup_agg

ove3 <- get_ada_data(cin.ada.path, "EPAGPA059SS#7777AE2.6Forshay,7-27-21NO3+NO2NH4NO2NO3GPMS.xlsx") %>%
   mutate(site_id = "U-06") %>% # add site_id
   mutate(sample_filter = "filtered") %>% # filtered or unfiltered, based on file name
   dup_agg


zzz <- left_join(jea1, jea2, by = "lake_id") 
# this object name is just a placeholder




# Nutrient samples for the 2020 SuRGE field season were held in Cincinnati,
# then shipped to ADA in May 2021 for analysis.
# 11Nov21: JB not yet reviewed

# 1. Read in chain of custody forms
cin.ada.coc.path <- paste0(userPath, 
                           "data/chemistry/nutrients/2020cinSentToAda/")
cin.ada.coc <- rbind(read_excel(paste0(cin.ada.coc.path,
                                       "dissolvedNutrientSampleIds.xlsx"),
                                sheet = "data"),
                     read_excel(paste0(cin.ada.coc.path,
                                       "totalNutrientSampleIds.xlsx"),
                                sheet = "data")) %>%
  janitor::clean_names()

# 2. Read in ORP data
cin.ada.orp <- read_excel(paste0(cin.ada.coc.path,
                                 "EPAGPA053,SS#7759,AE2.6,Forshay,LakeMethaneProject7-6-20,oP,GPKR.xls"),
                          sheet = "Data", range = "A14:G81") %>%
  janitor::clean_names() %>%
  rename(ortho_p = data)

# 3. Add coc info to ORP data
nrow(cin.ada.orp) #67 records
cin.ada.orp <- left_join(cin.ada.orp, cin.ada.coc, by = c("field_sample_id" = "lab_id"))
nrow(cin.ada.orp) #67 records
cin.ada.orp %>% filter(is.na(lake_id)) # good, all were matched.

# 4. Read in TN and TP data 
cin.ada.total <- read_excel(paste0(cin.ada.coc.path,
                                   "EPAGPA053SS#7759,AE2.6,Forshay,LakeMethaneProject,7-6-20,TN,TP,GPKR.xls"),
                            sheet = "Data", range = "A14:J154") %>% # records above A85 are for dissolved samples!
  janitor::clean_names() %>%
  filter(!grepl("DN", field_sample_id)) %>% # remove dissolved nutrients.  somehow they were run for totals.
  rename_with(~gsub(c("_5|_6|_7"), replacement = "_tn", x = .x)) %>%
  rename_with(~gsub(c("_8|_9|_10"), replacement = "_tp", x = .x)) %>%
  rename_with(~gsub("data_", "", .x)) %>%
  janitor::clean_names() 

#5.  Add coc info to TN and TP data
nrow(cin.ada.total) #70 records
cin.ada.no3.nh4 
nrow(cin.ada.total) #70 records
cin.ada.total %>% filter(is.na(lake_id)) # good, all were matched.

#6.  Read in no3.no2
cin.ada.no3.nh4 <- read_excel(paste0(cin.ada.coc.path,
                                   "EPAGPA053SS#7759ForshayLakeMethaneProject7-6-2020NO3+NO2NH4GPMS.xls"),
                            sheet = "Data", range = "A14:I88") %>% 
  janitor::clean_names() %>%
  rename_with(~gsub(c("_5|_6"), replacement = "_no2_3", x = .x)) %>%
  rename_with(~gsub(c("_8|_9"), replacement = "_nh4", x = .x)) %>%
  rename_with(~gsub("data_", "", .x))

#7.  Add coc info to no3.no2 data
cin.ada.no3.nh4 <- left_join(cin.ada.no3.nh4, cin.ada.coc, by = c("field_sample_id" = "lab_id")) 
cin.ada.no3.nh4 %>% filter(is.na(lake_id)) # good, all were matched.



