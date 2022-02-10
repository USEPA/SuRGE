# READ ANALYTICAL DATA FROM TTEB LABORATORY

# In 2018, TTEB ran TOC and TN, but not metals, on R10 samples.  These data are in 
# BEAULIEU_01_20_2022_update.xlsx.  We will use TN from AWBERC nutrient chemistry,
# not TTEB analysis.

# In March 2021, TTEB ran metals (TOC ran by MASI contract lab) on R10 and CIN 
# SuRGE samples collected in 2020.  Data are in `SURGE_2021_01_20_2022_update.xlsx`.

# During the summer of 2021, TTEB ran metals, TOC, and DOC on SuRGE samples
# multiple locations.  Data are in `SURGE_2022_01_20_2022_update.xlsx`.

# As of 1/25/2022, the `SURGE_2022_01_20_2022_update.xlsx` file contains a
# subset of the data.  We are waiting for an update from TTEB.

# 1. READ CHEMISTRY DATA--------------
# Files contain samples from SuRGE + other studies.  Filter below.
tteb.BEAULIEU <- read_excel(paste0(userPath, 
                           "data/chemistry/tteb/BEAULIEU_01_20_2022_update.xlsx")) 

tteb.SURGE <- read_excel(paste0(userPath, 
                             "data/chemistry/tteb/SURGE_2021_01_20_2022_update.xlsx"))


tteb <- bind_rows(tteb.BEAULIEU, tteb.SURGE) %>% 
  janitor::clean_names() %>%
  rename_with(.cols = contains("_aes"), ~gsub("_aes", "", .)) %>% # remove aes from variable name
   select(-colldate, -studyid, -tn, -flag) %>% # remove unneeded columns
  rename(lab_id = labid,
         toc = toc_comb) %>%
  
  # a value of 9999999999999990.000 indicates no data for that sample/analyte.
  # This often occurs if a summary file contains samples with different
  # requested analytes.  For example, the Beaulieu file contains samples that did
  # not request metals (e.g. Falls Lake (FL), 2018 SuRGE samples (LVR, PLR))
  # and samples that did (e.g. 2020 SuRGE samples).  Samples that did not request
  # metals have values of 9999999999999990.000 for all metals analytes.
  # However, a value of 9999999999999990.000 may also indicate that the analyte
  # was outside of the standard curve and was rerun, but the summary file wasn't
  # updated with re-run value.  This is the case for labid 203173.
  mutate(across(everything(), # replace lab's placeholder numbers with 'NA'
                ~ na_if(., 9999999999999990.000))) %>%
  # create 'flag' columns for every analyte to flag observations < det. limit
  mutate(across(al:zn, # nice code Joe!
                ~ if_else(. < 0 , "<", NA_character_), # bd reported as -detection limit
                .names = "{col}_flag")) %>%
  # create 'units' columns. All units will be converted to ug/L below; see Wiki page 
  mutate(across(al:zn, # 
                ~ "ug/l", 
                .names = "{col}_units")) %>%
  mutate(toc_units = "mg_c_l") %>% # TOC is the exception
  mutate(across(al:zn, # make all values positive. absolute value of - detection limit
                ~ abs(.))) %>%
  mutate(across(c(al:zn, -toc), # convert values from mg/l to ug/l; see Wiki page
                ~ .*1000)) %>%
  select(order(colnames(.))) %>% # alphabetize column names
  select(lab_id, sampid, everything()) # put these columns first

  

# 2. READ CHAIN OF CUSTODY----------------
# Read in chain on custody data for SuRGE samples submitted to TTEB
# Need to add R10 2018 samples to ttebSampleIds.xlsx
ttebCoc <- read_excel(paste0(userPath, 
                             "data/chemistry/tteb/ttebSampleIds.xlsx")) %>%
  clean_names(.)

janitor::get_dupes(ttebCoc, lab_id) # no duplicates


# 3. REVIEW CHAIN OF CUSTODY-------------

# Compare list of submitted samples to comprehensive sample list
# print rows in ttebSampleIds not in chem.samples.
# [Jan. 21, 2022] No extra samples, all good 
setdiff(ttebCoc[c("lake_id", "sample_depth", "sample_type", "analyte")],
        chem.samples.foo %>% 
          filter(analyte_group %in% c("organics", "metals"), #tteb does organics and metals
                 !(lab == "ADA" & analyte_group == "organics"), # ADA does own organics
                 !(sample_year == 2020 & analyte_group == "organics"))  %>% # 2020 doc/toc sent to MASI
          mutate(analyte = replace(analyte, analyte_group == "metals", "metals")) %>%
          distinct() %>%
          select(lake_id, sample_depth, sample_type, analyte)) %>%
  arrange(lake_id)

# Have all tteb samples in comprehensive sample list been submitted?
# Print rows from comprehensive sample list not in tteb coc.
# Missing some from 147, 275, 298, and 68
setdiff(chem.samples.foo %>% 
          filter(analyte_group %in% c("organics", "metals"), #tteb does organics and metals
                 #sample_year >= 2020, # no 2018 samples sent to TTEB
                 !(lab == "ADA" & analyte_group == "organics"), # ADA does own organics
                 !(sample_year == 2020 & analyte_group == "organics"))  %>% # 2020 doc/toc sent to MASI
          mutate(analyte = replace(analyte, # convert long list of metals into "metals"
                                   analyte_group == "metals", 
                                   "metals")) %>%
          distinct() %>% # remove duplicate "metals"
          select(lake_id, sample_depth, sample_type, analyte),
        ttebCoc[c("lake_id", "sample_depth", "sample_type", "analyte")]) %>%
  arrange(lake_id)

# 147
# ttebMetals19July2021.pdf contains three shallow metals samples for lake 147.  
# None of three lines report whether they are unknowns, duplicates, or blanks.  
# All three are entered as unknown in ttebSampleIds, but we will need to change 
# this when we get the data.

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


# 4. JOIN TTEB DATA WITH CoC--------------
# inner_join will keep all matched samples.  Since
# we are matching with SuRGE CoC, only SuRGE samples will be retained.
# tteb contains data from other studies too (i.e. Falls Lake dat)
tteb.all <- inner_join(ttebCoc %>% select(-analyte), tteb)
nrow(tteb.all) # 95 records

# Are all submitted samples in chemistry data?
# no, missing a ton.  Waiting for update from Maily.
ttebCoc %>% filter(!(lab_id %in% tteb.all$lab_id))
# list of missing metals samples to send to Maily [1/25/2022]
ttebCoc %>% filter(!(lab_id %in% tteb.all$lab_id)) %>%
  filter(analyte == "metals") %>%
  write.table(paste0(userPath, "data/chemistry/tteb/missingMetals01252022.txt"), row.names = FALSE)


# clean up final object
tteb.all <- tteb.all %>% select(-lab_id, -coc, -notes, -sampid) %>%
  rename(cin_shipping_notes = shipping_notes) %>%
  mutate(site_id = as.numeric(gsub(".*?([0-9]+).*", "\\1", site_id))) %>%  # remove non numeric chars
  # rename the toc fields to enable a clean join with other objects containing
  # TOC data (i.e. oc.ada, masi.toc).  See mergeChemistry.R
  rename(tteb.toc = toc, # rename these fields for the full_join in the merge script
         tteb.toc_units = toc_units, 
         tteb.toc_flag = toc_flag) 

