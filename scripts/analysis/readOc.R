# Rename flag to qual.  Import values reported in final data file.  Add
# 'flag` column.  This will be < or blank, indicating censored observations.


# TOC/DOC-------------------
# During the 2020 field season, TOC samples were sent to MASI contract
# laboratory for analysis.  No DOC analysis was conducted in 2020.

# remove 01 flag.
toc.masi <- read_excel(paste0(userPath, 
                              "data/chemistry/TOC.DOC/MASI_TOC/masiTocData.xlsx"), 
                       sheet = "data")

toc.masi1 <- toc.masi %>% # created new df so I can compare w/ toc.masi 
  rename(sampleid = lab_id) %>%
  select(sampleid, everything()) %>% # put sampleid column first
  select(-lake_id, -site_id) %>% # remove unneeded columns??
  mutate(qual = str_split_fixed(qual, pattern = ",", n=2)) # remove '01' from qual
  
  
  

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

# Fix sample_id field and ID/average duplicates:
# str_length + str_squish revealed no leading or trailing spaces in toc.epa


# Integrated Jake/Joe version (18 Oct 2021) working version. 
toc.epa <- rbind(toc.1, toc.2, toc.3) %>% 
  janitor::clean_names() %>%
  # extract lab ID and put it in a new column:
  mutate(sample_id1 = as.numeric(str_split_fixed(sample_id, pattern = "\\D", n=2)[,1])) %>%
  filter(is.na(sample_id1) == FALSE) %>%  # filter out anything without a lab id
  mutate(lab_qa = str_extract(sample_id, pattern = "SPK|DUP|d2")) %>% # id the dupes
  select(-sample_id) %>%
  relocate(sample_id1) %>%
  #203613 was run on 7/24 and noted "will be re-analyzed this week"
  #it was rerun on 7/31/21 with the sampleID 203613rerun.  I believe it was rerun
  #because it was below the lowest value in the standard curve.  The two values
  #are similar, so lets treat as lab DUPS.
  mutate(notes = replace(notes, sample_id1 == "203613", "rerun because initial results below std curve"))
# 18 Oct 2021: don't filter anything else yet.  TTEB will clean up most of this
# in their final data report.
# filter(dup_sample == "DUP" | is.na(dup_sample)==TRUE) # filter out the d2 and SPK

# add-in the following to combine the duplicates. 
# revisit after we get updated report from TTEB
# group_by(unit, lab_id) %>%  # PROBLEM: we lose some columns this way.
# summarize(npoc_result = mean(npoc_result))


# need to add toc.masi to object
toc_joined <- left_join(toc.epa, chemCoc, by = c("sample_id1" = "lab_id"))
