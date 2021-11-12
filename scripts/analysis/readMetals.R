##UPDATE TO READ IN FROM FINAL DBF FILES (BEAULIEU AND SURGE)

# Rename flag to qual. 'flag` column will be < or blank, indicating 
# censored observations. Filter out old observations from the BEAULIEU... file.


metals.BEAULIEU <- read_excel(paste0(userPath, 
                           "data/chemistry/BEAULIEU_10_01_2021_update.xlsx")) 

metals.SURGE <- read_excel(paste0(userPath, 
                             "data/chemistry/SURGE 2021_10_07_2021_update.xlsx"))


metals.epa <- bind_rows(metals.BEAULIEU, metals.SURGE) %>% 
  janitor::clean_names() %>%
  
  
  # as of 11/10/2021 the files only contain metals data for SuRGE, but the
  # Beaulieu file contains TN and TOC from older studies.  Remove other 
  # analytes for now, but might make sense to expand this code to process other
  # analytes (TOC and DOC) when the file is updated again.
  # 12NOV2021: to include TN and TOC, 
  # remove the following filter() and replace the select() w/ this line of code:  
  # select(-colldate, -studyid) %>% # remove unneeded columns
  
  filter(labid>200000) %>% # filter out pre-2020 data
  select(-tn, -toc_comb, -colldate, -studyid) %>% # remove unneeded columns
  
  rename(lab_id = labid, metals.qual = flag) %>% # this should be 'lab_id' to match COC
  # a value of 9999999999999990.000 indicates no data for that sample/analyte.
  # this sometimes occurs when the analyte was outside of the standard curve
  # and was rerun, but the summary file wasn't updated with re-run value.
  # This is the case for labid's 203013 and 203014.  Maily indicated these
  # would be updated in the future (see 10/26/2021 email).  Replace with NA
  # for now.
  mutate(across(al_aes:zn_aes, # replace lab's placeholder numbers with 'NA'
                ~ na_if(., 9999999999999990.000))) %>%
  # create 'flag' columns for every analyte to flag observations < det. limit
  mutate(across(al_aes:zn_aes, # nice code Joe!
                ~ if_else(. < 0 , "<", ""), 
                .names = "{col}_flag")) %>%
  mutate(across(al_aes:zn_aes, # make all values positive.  # nice Joe!
                ~ abs(.))) %>%
  select(order(colnames(.))) %>% # alphabetize column names
  select(lab_id, sampid, metals.qual, comment, everything()) # put these columns first

  


# merge with chain of custody

metals.epa <- left_join(metals.epa, chemCoc %>% select(-analyte)) # keep all analytical records 
nrow(metals.epa) #68 records
metals.epa %>% filter(is.na(metals.epa)) # good, all records matched
metals.epa
