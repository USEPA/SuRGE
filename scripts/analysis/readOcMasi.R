# TOC/DOC-------------------
# During the 2020 field season, several batches of TOC samples were sent to MASI 
# contract laboratory for analysis.  The first handful of batches contained a 
# TOC vial per sample.  At the request of MASI, subsequent batches contained 
# two vials per sample.  No DOC analysis was conducted in 2020.

# See the repo Wiki page for information on toc_qual and toc_flag.

# read data
toc.masi <- read_excel(paste0(userPath, 
                              "data/chemistry/TOC.DOC/MASI_TOC/masiTocData.xlsx"), 
                       sheet = "data") %>%
  rename(toc_units = units,
         toc_qual = qual,
         toc_flag = flag) %>%
  mutate(toc_units = tolower(toc_units),  # L ->l
         toc_units =  sub("/", "_", toc_units), # / -> _
         toc_units =  paste0(substr(toc_units, start = 1, stop = 3), # mg_
                             "c_", # squeeze this in between mg_ and l
                             substr(toc_units, start = 4, stop = 4)), # l
         #toc_qual should be TRUE or FALSE
         toc_qual = case_when(is.na(toc_qual) ~ FALSE, # if NA, then FALSE (no holding time violation)
                          TRUE ~ TRUE), # if not NA, then TRUE (holding time violation)
         # sample depth for blanks entered as N/A.  Change to 'blank'
         sample_depth = case_when(sample_depth == "N/A" ~ "blank",
                                  TRUE ~ sample_depth),
         lake_id = as.numeric(lake_id) %>% as.character(),
         # extract number from site_id, convert to numeric
         site_id = as.numeric(gsub(".*?([0-9]+).*", "\\1", site_id))) %>%
  select(-lab_id)
                
  
# check data object
# any duplicated observations for unique combination of sample IDs?
# no dups, good!
toc.masi %>% select(matches("id|sample")) %>% 
  janitor::get_dupes()


# sample inventory.  All 2020 TOC samples were sent to MASI.
# print rows in toc.masi not in chem.samples
# only samples showing up are duplicate TOC samples that were requested by MASI,
# but collected at non qa.qc lakes.  No problem.
setdiff(toc.masi[,c("lake_id", "sample_depth", "sample_type")],
        chem.samples.foo[c("lake_id", "sample_depth", "sample_type")]) %>% print(n=Inf)


# Have all 2020 toc samples comprehensive sample list been analyzed by MASI?
# Print rows from comprehensive sample list not in MASI data.
# Good, all samples accounted for.
setdiff(chem.samples.foo %>% filter(analyte == "toc", # only toc sent to MASI
                                    sample_year == 2020) %>% # only 2020 sent to MASI
          select(lake_id, sample_depth, sample_type),
        toc.masi[c("lake_id", "sample_depth", "sample_type")]) %>%
  arrange(lake_id) %>% print(n=Inf)


# ###DEPRECATED
# # CODE BELOW READS IN PRELIMINARY DATA REPORTS FROM TTEB LAB.  FINAL DATA
# # WILL BE DELIVERED IN VERY DIFFERENT FORMAT.  LETS NOT WASTE TIME TWEAKING
# # THIS CODE WHEN DATA WILL ULTIMATELY BE READ IN FROM DIFFERENT FILES
# # During 2021 field season, DOC and TOC were submitted to Maily's lab.  Results
# # contain samples from other groups too.
# toc.1 <- read_excel(paste0(userPath, 
#                            "data/chemistry/TOC.DOC/TOC_Results_20210621_30.xlsx")) 
# toc.2 <- read_excel(paste0(userPath, 
#                            "data/chemistry/TOC.DOC/TOC_Results_20210712_25.xlsx")) 
# toc.3 <- read_excel(paste0(userPath, 
#                            "data/chemistry/TOC.DOC/TOC_Results_20210730_823.xlsx")) 
# toc.epa <- rbind(toc.1, toc.2, toc.3) %>% janitor::clean_names()
# 
# # Need to clean up toc.epa and restrict lab_id field to numeric values, then
# # join with chemCoc.
# 
# # Fix sample_id field and ID/average duplicates:
# # str_length + str_squish revealed no leading or trailing spaces in toc.epa
# 
# 
# # Integrated Jake/Joe version (18 Oct 2021) working version. 
# toc.epa <- rbind(toc.1, toc.2, toc.3) %>% 
#   janitor::clean_names() %>%
#   # extract lab ID and put it in a new column:
#   mutate(sample_id1 = as.numeric(str_split_fixed(sample_id, pattern = "\\D", n=2)[,1])) %>%
#   filter(is.na(sample_id1) == FALSE) %>%  # filter out anything without a lab id
#   mutate(lab_qa = str_extract(sample_id, pattern = "SPK|DUP|d2")) %>% # id the dupes
#   select(-sample_id) %>%
#   relocate(sample_id1) %>%
#   #203613 was run on 7/24 and noted "will be re-analyzed this week"
#   #it was rerun on 7/31/21 with the sampleID 203613rerun.  I believe it was rerun
#   #because it was below the lowest value in the standard curve.  The two values
#   #are similar, so lets treat as lab DUPS.
#   mutate(notes = replace(notes, sample_id1 == "203613", "rerun because initial results below std curve"))
# # 18 Oct 2021: don't filter anything else yet.  TTEB will clean up most of this
# # in their final data report.
# # filter(dup_sample == "DUP" | is.na(dup_sample)==TRUE) # filter out the d2 and SPK
# 
# # add-in the following to combine the duplicates. 
# # revisit after we get updated report from TTEB
# # group_by(unit, lab_id) %>%  # PROBLEM: we lose some columns this way.
# # summarize(npoc_result = mean(npoc_result))
