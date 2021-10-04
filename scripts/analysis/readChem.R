
# METALS----------------------------------
# Metals samples were submitted to TTEB by Pegasus.
# every submission should be documented with a chain of custody
# form mapping TTEB sample id to the surge unique identifiers.
# The chain of custody (.pdf) from should be in the chemistry folder.
# mapping between sample id and surge identifiers has been transcribed
# onto ttebSampleIds.xlsx"

# Samples collected during the 2020 field season were held until the lab
# opened and submitted in march 2021.  Those data are read in below.
metals1 <- read_excel(paste0(userPath, 
                             "data/chemistry/metals/winter_2021_submision/",
                             "SURGE_20210316.xlsx")) %>%
  rename(lab_id = ...1) %>%
  mutate(file_metals = "SURGE_20210316.xlsx") %>%
  janitor::clean_names(.)


nrow(metals1) #42 records in data

# merge with chain of custody
metals1 <- left_join(metals1, chemCoc) # keep all analytical records 
nrow(metals1) #42, good
metals1 %>% filter(is.na(lake_id)) # good all records matched
metals1


# TOC/DOC-------------------
# During the 2020 field season, TOC samples were sent to MASI contract
# laboratory for analysis.  No DOC analysis was conducted in 2020.

# remove 01 flag.
toc.masi <- read_excel(paste0(userPath, 
                           "data/chemistry/TOC.DOC/MASI_TOC/masiTocData.xlsx"), 
                    sheet = "data")


# During 2021 field season, DOC and TOC were submitted to Maily's lab.  Results
# contain samples from other groups too.
toc.1 <- read_excel(paste0(userPath, 
                           "data/chemistry/TOC.DOC/TOC_Results_20210621_30.xlsx")) 
toc.2 <- read_excel(paste0(userPath, 
                           "data/chemistry/TOC.DOC/TOC_Results_20210712_25.xlsx")) 
toc.3 <- read_excel(paste0(userPath, 
                           "data/chemistry/TOC.DOC/TOC_Results_20210730_823.xlsx")) 
toc.epa <- rbind(toc.1, toc.2, toc.3) %>% janitor::clean_names()

# Need to clean up toc.epa and restrict lab_id field to numeric values, then
# join with chemCoc.

  #left_join(., chemCoc, by = c("sample_id" = "lab_id"))

# NUTRIENTS-----------------------
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



