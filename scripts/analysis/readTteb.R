##UPDATE TO READ IN FROM FINAL DBF FILES (BEAULIEU AND SURGE)

# Rename flag to qual. 'flag` column will be < or blank, indicating 
# censored observations. Filter out old observations from the BEAULIEU... file.


metals.BEAULIEU <- read_excel(paste0(userPath, 
                           "data/chemistry/BEAULIEU_10_01_2021_update.xlsx")) 

metals.SURGE <- read_excel(paste0(userPath, 
                             "data/chemistry/SURGE 2021_10_07_2021_update.xlsx"))


metals.epa <- bind_rows(metals.BEAULIEU, metals.SURGE) %>% 
  janitor::clean_names() %>%
  rename_with(.cols = contains("_aes"), ~gsub("_aes", "", .)) %>% # remove aes from variable name
  
  
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
  mutate(across(al:zn, # replace lab's placeholder numbers with 'NA'
                ~ na_if(., 9999999999999990.000))) %>%
  # create 'flag' columns for every analyte to flag observations < det. limit
  mutate(across(al:zn, # nice code Joe!
                ~ if_else(. < 0 , "<", ""), #bd reported as -detection limit
                .names = "{col}_flag")) %>%
  mutate(across(al:zn, # make all values positive. absolute value of - detection limit
                ~ abs(.))) %>%
  select(order(colnames(.))) %>% # alphabetize column names
  select(lab_id, sampid, metals.qual, comment, everything()) # put these columns first

  


# merge with chain of custody

metals.epa <- left_join(metals.epa, chemCoc %>% select(-analyte)) # keep all analytical records 
nrow(metals.epa) #68 records
metals.epa %>% filter(is.na(metals.epa)) # good, all records matched
metals.epa


# Compare list of submitted samples to comprehensive sample list
tteb.samples <- read_excel(paste0(userPath, 
                                  "data/chemistry/ttebSampleIds.xlsx"))

# print rows in d.anions not in chem.samples.
# only 247 and unk in ttebSampleIds, but not in comprehensive list.  247 was
# not sampled.
setdiff(tteb.samples[c("lake_id", "sample_depth", "sample_type", "analyte")],
        chem.samples.foo %>% 
          filter(analyte_group %in% c("organics", "metals"), #tteb does organics and metals
                 sample_year >= 2020, # no 2018 samples sent to TTEB
                 !(lab == "ADA" & analyte_group == "organics"), # ADA does own organics
                 !(sample_year == 2020 & analyte_group == "organics"))  %>% # 2020 doc/toc sent to MASI
          mutate(analyte = replace(analyte, analyte_group == "metals", "metals")) %>%
          distinct() %>%
          select(lake_id, sample_depth, sample_type, analyte)) %>%
  arrange(lake_id)

# 6. Have all tteb sampled in comprehensive sample list been analyzed?
# Print rows from comprehensive sample list not in tteb list.
# Missing a bunch, but I see nar sample receipt list doesn't contain any 2020
# samples.  Have NAR update file, then revisit.  I inspected the 2021 samples
# not in NAR inventory and communicated to Stephen Shivers.  Stephen confirmed
# that he analyzed the samples and will update the Excel file.
setdiff(chem.samples.foo %>% 
          filter(analyte_group %in% c("organics", "metals"), #tteb does organics and metals
                 sample_year >= 2020, # no 2018 samples sent to TTEB
                 !(lab == "ADA" & analyte_group == "organics"), # ADA does own organics
                 !(sample_year == 2020 & analyte_group == "organics"))  %>% # 2020 doc/toc sent to MASI
          mutate(analyte = replace(analyte, analyte_group == "metals", "metals")) %>%
          distinct() %>%
          select(lake_id, sample_depth, sample_type, analyte) ,
        tteb.samples[c("lake_id", "sample_depth", "sample_type", "analyte")]) %>%
  arrange(lake_id) 

# 147
# ttebMetals19July2021.pdf contains three shallow metals samples for lake 147.  
# None of three lines report whether they are unknowns, duplicates, or blanks.  
# All three are entered as unknown in ttebSampleIds, but we will need to change 
# this when we get the data.

# 155
# 23june has 6 toc doc samples for 155. No blanks though.  could not find sample 
# tracking form to check if blank was collected

# 275
# 01July2021 sample tracking form has a shallow duplicate and two shallow unknowns 
# for metals.  I suspect one of the unknowns is the blank.  Will have to sort out 
# after  we get data from TTEB.
# I don't see anything that could have been the doc blank.

# 298
# One of the shallow unknown metals that was submitted (203558, 203580) is likely
# the missing blank.  Will sort out after we get tteb data.

# 68
# Two DOC blanks submitted (204165, 204166).  One of them is likely the TOC blank.
# will sort out after we get tteb data.



