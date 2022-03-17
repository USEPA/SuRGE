# READ ANALYTICAL DATA FROM TTEB LABORATORY

# In 2018, TTEB ran TOC and TN, but not metals, on R10 samples.  These data are in 
# BEAULIEU_01_20_2022_update.xlsx.  We will use TN from AWBERC nutrient chemistry,
# not TTEB analysis.

# In March 2021, TTEB ran metals (TOC ran by MASI contract lab) on R10 and CIN 
# SuRGE samples collected in 2020.  Data are in `SURGE_2021_01_20_2022_update.xlsx`.

# During the summer of 2021, TTEB ran metals, TOC, and DOC on SuRGE samples
# multiple locations.  Data are in `SURGE_2021_03_10_2022_update.xlsx`.

# As of 3/10/2022, `SURGE_2021_03_10_2022_update.xlsx` and `SURGE_03_10_2022_update.xlsx`
# files contains a subset of the data.  We are waiting for an update from TTEB.

# As of 3/17/2022, the tteb sharedrive (L:\Public\CESER-PUB\IPCB) contains 
# SURGE.dbf.  The records in this file are duplicates of those in SURGE 2021.dbf

# 1. READ CHEMISTRY DATA--------------
# Files contain samples from SuRGE + other studies.  Filter below.

tteb.BEAULIEU <- read_excel(paste0(userPath, 
                                   "data/chemistry/tteb/BEAULIEU_01_20_2022_update.xlsx")) 

tteb.SURGE <- read_excel(paste0(userPath, 
                                "data/chemistry/tteb/SURGE_2021_03_10_2022_update.xlsx"))


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
  # create 'units' columns. Most units in mg/L 
  mutate(across(al:zn, # 
                ~ "mg_l", 
                .names = "{col}_units")) %>%
  mutate(toc_units = "mg_c_l") %>% # TOC is the exception
  mutate(across(al:zn, # make all values positive. absolute value of - detection limit
                ~ abs(.))) %>%
  select(order(colnames(.))) %>% # alphabetize column names
  select(lab_id, sampid, everything()) # put these columns first



# 2. READ CHAIN OF CUSTODY----------------
# Read in chain on custody data for SuRGE samples submitted to TTEB
# Need to add R10 2018 samples to ttebSampleIds.xlsx
ttebCoc <- read_excel(paste0(userPath, 
                             "data/chemistry/tteb/ttebSampleIds.xlsx")) %>%
  clean_names(.) %>%
  mutate(site_id = as.numeric(gsub(".*?([0-9]+).*", "\\1", site_id)))

janitor::get_dupes(ttebCoc, lab_id) # no duplicates


# 3. REVIEW CHAIN OF CUSTODY-------------

# Compare list of submitted samples to comprehensive sample list
# print rows in ttebSampleIds not in chem.samples.
# [Mar. 10, 2022] No extra samples, all good 
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
# All samples accounted for
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



# 4. JOIN TTEB DATA WITH CoC--------------
# inner_join will keep all matched samples.  Since
# we are matching with SuRGE CoC, only SuRGE samples will be retained.
# tteb contains data from other studies too (i.e. Falls Lake dat)
tteb.all <- inner_join(ttebCoc, tteb)
nrow(tteb.all) # 319 records [3/17/2022] 


# 5. DOC AND TOC ARE SUBMITTED TO TTEB AS TOC.   FIX HERE.
tteb.all <- tteb.all %>%
  mutate(doc = case_when(analyte == "doc" ~ toc,
                         TRUE ~ NA_real_),
         doc_units = "mg_c_l",
         doc_flag = case_when(analyte == "doc" ~ toc_flag,
                              TRUE ~ NA_character_)) %>%
  mutate(toc = case_when(analyte == "doc" ~ NA_real_,
                         TRUE ~ toc))

# 6. SAMPLE INVENTORY REVIEW
# Are all submitted samples in chemistry data?
# missing 33 samples.  Waiting for update from Maily. [3/17/2022]
ttebCoc %>% filter(!(lab_id %in% tteb.all$lab_id))
# list of missing samples to send to Maily [3/17/2022]
ttebCoc %>% filter(!(lab_id %in% tteb.all$lab_id)) %>%
  write.table(paste0(userPath, "data/chemistry/tteb/missingTteb03102022.txt"), row.names = FALSE)


# 7. UNIQUE IDs ARE DUPLICATED FOR EACH ANALYTE
# any combination of lake_id, site_id, sample_depth, and sample_type could
# be repeated for metals, doc, and toc.  To eliminate replicates of rows
# that share unique IDs, split by analyte, select columns that contain data
# for the analyte, then merge by unique ID.
tteb.all <- tteb.all %>%
  group_split(analyte) %>% # split by analyte
  map(., function(x) 
    if (unique(x$analyte == "doc")) { # if contains doc
      x %>% select(lake_id, site_id, sample_depth, sample_type, contains("doc")) # select doc stuff
    } else if (unique(x$analyte == "toc")) { # if contains toc
      x %>% select(lake_id, site_id, sample_depth, sample_type, contains("toc")) # select toc stuff
    } else if (unique(x$analyte == "metals")) { # if contains metals
      x %>% select(lake_id, site_id, sample_depth, sample_type, 
                   ni, ni_flag, ni_units, # if ni in matches, also grabs units (i.e. doc_units)
                   s, s_flag, s_units, # if s in matches, grabs too many variable
                   matches("(al|as|ba|be|ca|cd|cr|cu|fe|k|li|mg|
                         mn|na|p|pb|sb|si|sn|sr|v|zn)")) # select metals stuff
    }) %>%
  reduce(., full_join) # merge on lake_id, site_id, sample_depth, sample_type

dim(tteb.all) #82 rows.  Good, reduced from 352 to 82.


# 7. CLEAN UP FINAL OBJECT
tteb.all <- tteb.all %>% 
  select(-analyte, -sampid) %>%
  mutate(site_id = as.numeric(gsub(".*?([0-9]+).*", "\\1", site_id))) %>%  # remove non numeric chars
  # rename the toc and doc fields to enable a clean join with other objects containing
  # TOC data (i.e. oc.ada, masi.toc). DOC data (ada.oc). See mergeChemistry.R
  rename(tteb.toc = toc, # rename these fields for the full_join in the merge script
         tteb.toc_units = toc_units, 
         tteb.toc_flag = toc_flag,
         tteb.doc = doc,
         tteb.doc_units = doc_units,
         tteb.doc_flag = doc_flag) 

janitor::get_dupes(tteb.all %>% select(lake_id, site_id, sample_type, sample_depth))


