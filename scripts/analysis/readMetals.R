##UPDATE TO READ IN FROM FINAL DBF FILES (BEAULIEU AND SURGE)
# Rename flag to qual.  Import values reported in final data file.  Add
# 'flag` column.  This will be < or blank, indicating censored observations.
# filter out old observations from the BEAULIEU... file.


metals.BEAULIEU <- read_excel(paste0(userPath, 
                           "data/chemistry/BEAULIEU_10_01_2021_update.xlsx")) 

metals.SURGE <- read_excel(paste0(userPath, 
                             "data/chemistry/SURGE 2021_10_07_2021_update.xlsx"))


metals.epa <- bind_rows(metals.BEAULIEU, metals.SURGE) %>% 
  janitor::clean_names() %>%
  filter(labid>200000) %>% # remove pre-2020 data
  select(-tn, -toc_comb, -colldate, -studyid) %>% # remove unneeded columns
  rename(sampleid = labid, qual = flag) %>% 
  mutate(across(al_aes:zn_aes, # replace lab's placeholder numbers with 'NA'
                ~ na_if(., 9999999999999990.000))) %>%
  # create 'flag' columns for every analyte to flag observations < det. limit
  mutate(across(al_aes:zn_aes, 
                ~ if_else(. < 0 , "<", ""), 
                .names = "{col}_flag")) %>%
  mutate(across(al_aes:zn_aes, # make all values positive
                ~ abs(.))) %>%
  select(order(colnames(.))) %>% # alphabetize column names
  select(sampleid, sampid, qual, comment, everything()) # put these columns first

  

  
  



peek# METALS
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