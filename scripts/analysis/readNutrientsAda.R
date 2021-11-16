


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
   select(starts_with("Analytes"), MDL) %>% # select only the columns w/ analyte names and MDL
   rename(Analytes = starts_with("Analytes")) %>%
   filter(str_detect(Analytes, "Analyte", negate = TRUE)) %>% # filter out superfluous "Analytes..."
   column_to_rownames(var = "Analytes") # simpler to work w/ rownames in next chunk of code
 
 #'maintable' combines 'toptable' with results
 maintable <- read_excel(paste0(path, datasheet), # get the results
                         sheet = "Data", range = "A14:N19") %>%
   janitor::clean_names() %>%
   mutate(labdup = if_else(str_detect(lab_sample_id, "LAB DUP"), "LAB DUP", "")) %>% # flag the dups
   select(field_sample_id, labdup, starts_with("data")) %>% # remove unneeded columns
   rename_with(~row.names(toptable), .cols = starts_with("data")) %>% # rename using analyte names
   rename(sampleid = field_sample_id) %>%
   select(sampleid, labdup, everything()) %>% # reorder columns for the following mutate() 
   mutate(across(3:last_col(), # create new flag column if analyte not detected
                 ~ if_else(str_detect(., "ND"), "<", ""),
                 .names = "{col}_flag")) %>%
   mutate(across(3:last_col(), # replace ND with the MDL value from toptable
                 ~ ifelse(str_detect(., "ND"), toptable[paste(cur_column()),1], .))) %>% # note this is base::ifelse
   mutate(sampleid = str_replace_all(sampleid, "[(TN or DN)]","")) %>% # clean-up sampleid field
   janitor::clean_names() %>%
   mutate(across(!ends_with(c("flag", "labdup", "sampleid")), # remove 'BQL', 'RPD' & other junk from data fields
                 ~ str_extract(., pattern = "\\D\\d\\d+"))) %>%
   mutate(across(!ends_with(c("flag", "labdup", "sampleid")), # make extracted data numeric
                 ~ as.numeric(.))) %>%
   select(order(colnames(.))) %>% # alphabetize column names
   select(sampleid, labdup, everything()) # put 'sampleid' first
  return(maintable)

}

# Function to aggregate the lab dups from the data. 
# This is a separate function, since we won't always want to aggregate.
dup_agg <- function(data) {
   
   # carve out the _flag columns so they can be re-joined later
   c <- data %>% select(ends_with(c("sampleid","labdup", "flag")))
   
   # group and summarize to obtain means                
   d <- data %>%
      dplyr::group_by(sampleid) %>%
      summarize(across(!ends_with(c("labdup", "sampleid", "flag")), 
                       ~ mean(., na.rm = TRUE)))
   
   e <- left_join(d, c, by = 'sampleid') %>% # rejoin the data
      mutate(across(3:last_col(), 
                    ~ ifelse(is.nan(.), NA, .))) %>% # must use ifelse here (not if_else)
      select(order(colnames(.))) %>% # alphabetize column names
      select(sampleid, labdup, everything()) %>% # put 'sampleid' first
      filter(labdup != "LAB DUP") # remove the lab dup; we may want to revisit this.
      # note that the LAB DUP and the original now have identical values, but the < flags may differ
   
return(e)

}


# 1NOV21 next steps:
# join resulting data objects into a single tibble
# fix variable names?

#11NOV21:
# The Sample ID values in the spreadsheet are a bit confusing.  they are a five
# digit alphanumeric code where the first three digits correspond to lake_id, the
# fourth digit indicates sample depth (D=deep, S=shallow, N= no data (used for blanks)),
# and the fifth digit indicates dissolved or unfiltered (U=unfiltered, D=dissolved,
# B=blank).  We only want to read in dissolved + blanks from the NO3NO2NH4 and
# oP spreadsheets.  We only want unfiltered and blanks from the TNTP spreadsheets.
# After the correct rows have been read in, we need to parse the sampleID into 
# the fields needed to uniquely identify each sample (lake_id, site_id, sample_depth,
# and sample_type).  See chemCoc for how these columns should be formatted.  The ADA
# spreadsheets do not contain data for site_id field.  This information will need
# to be manually hardcoded from sheet F1 of the field data sheets (...\SuRGE Survey 
# of Reservoir Greenhouse gas Emissions - Documents\data\ADA\").

# I think data from the Jean Neustadt NO3NO2NH4 file aren't being read in correctly. 
# The sampleid values in jea1 don't seem to match those in the spreadsheet.  While
# I generally prefer to read in the data as formatted by the lab, this might be 
# a case where it makes more sense to add a tab to each excel file and transcribe
# the data in a more workable format.  We still read data from each excel file, 
# but we read the formatted data from the new tab.  What do you think?


# create path for Lake Jean Neustadt
cin.ada.path <- paste0(userPath, 
                       "data/chemistry/nutrients/ADA/CH4_147_Lake Jean Neustadt/")

# apply get_ada_data function to each spreadsheet for Lake Jean Neustadt
# also aggregate LAB DUPs with dup_agg
jea1 <- get_ada_data(cin.ada.path, "EPAGPA054SS#7773AE2.6Forshay,7-14-21,NO3NO2NH4.xlsx") %>%
   dup_agg
jea2 <- get_ada_data(cin.ada.path, "EPAGPA054SS#7773,AE2.6,Forshay,7-14-21,TNTPGPKR.xls") %>%
   dup_agg
jea3 <- get_ada_data(cin.ada.path, "EPAGPA054,SS#7773,AE2.6,Forshay,7-14-21,oP,GPKR.xls") %>%
   dup_agg


# create path for Keystone Lake
cin.ada.path <- paste0(userPath, 
                       "data/chemistry/nutrients/ADA/CH4_148_Keystone Lake/")

# apply get_ada_data function to each spreadsheet for Keystone Lake 
key1 <- get_ada_data(cin.ada.path, "EPAGPA061,SS#7784,AE2.6,Forshay,8-17-21,oP,GPKR.xls")
key2 <- get_ada_data(cin.ada.path, "EPAGPA061SS#7784,AE2.6,Forshay,8-17-21,TN,TP,GPKR.xls")             
key3 <- get_ada_data(cin.ada.path, "EPAGPA061SS#7784AE2.6Forshay,8-17-21NO3+NO2NH4NO2NO3GPMS.xlsx")

# create path for Lake Overholser
cin.ada.path <- paste0(userPath, 
                       "data/chemistry/nutrients/ADA/CH4_167_Lake Overholser/")

# apply get_ada_data function to each spreadsheet for for Lake Overholser
ove1 <- get_ada_data(cin.ada.path, "EPAGPA059,SS#7777,AE2.6,Forshay,7-27-21,oP,GPKR.xls")
ove2 <- get_ada_data(cin.ada.path, "EPAGPA059SS#7777,AE2.6,Forshay,7-27-21,TN,TP,GPKR.xls")
ove3 <- get_ada_data(cin.ada.path, "EPAGPA059SS#7777AE2.6Forshay,7-27-21NO3+NO2NH4NO2NO3GPMS.xlsx")           

zzz <- left_join(jea1, jea2, by = "sampleid") # 4NOV21: What do we do with LAB DUPs?
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



