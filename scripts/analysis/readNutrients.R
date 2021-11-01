


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
   rownames_to_column() %>% # these 4 lines of code transpose the dataframe 
   pivot_longer(-rowname, 'variable', 'value') %>%
   pivot_wider(variable, rowname) %>%
   row_to_names(1) %>%
   select(starts_with("Analytes"), MDL) %>% # select only the columns w/ analyte names and MDL
   rename(Analytes = starts_with("Analytes")) %>%
   filter(str_detect(Analytes, "Analyte", negate = TRUE)) # filter out superfluous "Analytes..."
 
 #'maintable' combines 'toptable' with results
 maintable <- read_excel(paste0(path, datasheet), # get the results
                         sheet = "Data", range = "A14:N19") %>%
   janitor::clean_names() %>%
   select(field_sample_id, starts_with("data")) %>% # remove unneeded columns
   rename_with(~toptable$Analytes, .cols = starts_with("data")) %>% # rename using analyte names
   rename(sampleid = field_sample_id) %>%
   mutate(across(2:last_col(), # create new flag column if analyte not detected
                 ~ if_else(. == "ND", "<", ""), 
                 .names = "{col}_flag"))
   # mutate(across(2:last_col(),
   #               ~ if_else(. == "ND", -1, 555))) %>%

   #janitor::clean_names() 
   
  zz <<- toptable
  return(maintable)

}

# 1NOV21 next steps:
# join all 3 resulting data objects into a single dataframe (maybe?)
# add flags if ND: need to add str_detect due to superfluous text
# add MDL value if ND: figure out (maybe pivot 'toptable' and join?)
# clean up the sampleid field (remove 'TN or DN')
# clean up the variable names

# create path for Lake Jean Neustadt
cin.ada.path <- paste0(userPath, 
                       "data/chemistry/nutrients/ADA/CH4_147_Lake Jean Neustadt/")

# apply get_ada_data function to each spreadsheet for Lake Jean Neustadt
z1 <- get_ada_data(cin.ada.path, "EPAGPA054SS#7773AE2.6Forshay,7-14-21,NO3NO2NH4.xlsx")
z2 <- get_ada_data(cin.ada.path, "EPAGPA054SS#7773,AE2.6,Forshay,7-14-21,TNTPGPKR.xls")
z3 <- get_ada_data(cin.ada.path, "EPAGPA054,SS#7773,AE2.6,Forshay,7-14-21,oP,GPKR.xls")

# join all 3 in a single object (IN WORK)

# create path for Keystone Lake
cin.ada.path <- paste0(userPath, 
                       "data/chemistry/nutrients/ADA/CH4_148_Keystone Lake/")

# apply get_ada_data function to each spreadsheet for Lakes Keystone and Bluestem
get_ada_data(cin.ada.path, "EPAGPA061,SS#7784,AE2.6,Forshay,8-17-21,oP,GPKR.xls")
get_ada_data(cin.ada.path, "EPAGPA061SS#7784,AE2.6,Forshay,8-17-21,TN,TP,GPKR.xls")             
get_ada_data(cin.ada.path, "EPAGPA061SS#7784AE2.6Forshay,8-17-21NO3+NO2NH4NO2NO3GPMS.xlsx")

# create path for Lake Overholser
cin.ada.path <- paste0(userPath, 
                       "data/chemistry/nutrients/ADA/CH4_167_Lake Overholser/")

# apply get_ada_data function to each spreadsheet for for Lake Overholser
get_ada_data(cin.ada.path, "EPAGPA059,SS#7777,AE2.6,Forshay,7-27-21,oP,GPKR.xls")
get_ada_data(cin.ada.path, "EPAGPA059SS#7777,AE2.6,Forshay,7-27-21,TN,TP,GPKR.xls")
get_ada_data(cin.ada.path, "EPAGPA059SS#7777AE2.6Forshay,7-27-21NO3+NO2NH4NO2NO3GPMS.xlsx")           




# Nutrient samples for the 2020 SuRGE field season were held in Cincinnati,
# then shipped to ADA in May 2021 for analysis.

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



