# TOC/DOC-------------------
# During the 2020 field season, several batches of TOC samples were sent to MASI 
# contract laboratory for analysis.  The first handful of batches contained a 
# TOC vial per sample.  At the request of MASI, subsequent batches contained 
# two vials per sample.  No DOC analysis was conducted in 2020.

# See the repo Wiki page for information on toc_qual and toc_flag.

# read data
toc.masi <- read_excel(paste0(userPath, 
                              "data/chemistry/oc_ada_masi/MASI_TOC/masiTocData.xlsx"), 
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
         toc_qual = case_when(is.na(toc_qual) ~ "", 
                              # if NA, then FALSE (no holding time violation)
                              TRUE ~ "H"), # if not NA, then TRUE (holding time violation)
         # sample depth for blanks entered as N/A.  Change to 'blank'
         sample_depth = case_when(sample_depth == "N/A" ~ "blank",
                                  TRUE ~ sample_depth),
         lake_id = as.numeric(lake_id) %>% as.character(),
         # extract number from site_id, convert to numeric
         site_id = as.numeric(gsub(".*?([0-9]+).*", "\\1", site_id))) %>%
  select(-lab_id) %>%
  # Unite the flag columns
  mutate(toc_flag = if_else(toc_flag == "<", "ND", "")) %>%
  unite("toc_flags", toc_flag, toc_qual, sep = " ", na.rm = TRUE) %>%
  mutate(toc_flags = if_else( # NA if there are no flags
    str_detect(toc_flags, "\\w"), toc_flags, NA_character_) %>%
      str_squish()) # remove any extra white spaces
                
  
# check data object
# any duplicated observations for unique combination of sample IDs?
# no dups, good!
toc.masi %>% select(matches("id|sample")) %>% 
  janitor::get_dupes()


# INSPECT LABORATORY DUPLICATES ----------------- 
# These are extra samples collected per MASI's
# internal qa.qc requirements.  No other qa.qc collected at these sites. Treat
# as laboratory duplicates to prevent downstream merging headaches.
labDupIds <- toc.masi %>% 
  filter(sample_type == "lab_duplicate") %>%
  mutate(id = paste0(lake_id, sample_depth))

labDups <- toc.masi %>% mutate(id = paste0(lake_id, sample_depth)) %>%
  filter(id %in% labDupIds$id)

# one of the 102 shallow samples is way too high (>25mg/l).  
ggplot(labDups, aes(id, toc)) + 
  geom_point() + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))

toc.masi <- toc.masi %>% filter(toc<25) # delete outlier


# aggregate across remaining laboratory duplicates
toc.masi <- toc.masi %>%
  pivot_wider(names_from = sample_type, values_from = toc) %>% # cast to wide
  mutate(unknown = case_when(is.na(lab_duplicate) ~ unknown, # if no lab dup, then unknown
                             TRUE ~ (unknown + lab_duplicate)/2)) %>% # if lab dup, then aggregate
  select(-lab_duplicate) %>% # remove lab_duplicate
  pivot_longer(c(unknown, blank, duplicate), # pivot back to long
               names_to = "sample_type", 
               values_to = "toc") %>%
  filter(!is.na(toc)) # remove extra records created from pivoting.

# SAMPLE INVENTORY ANALYSIS---------------
# All 2020 TOC samples were sent to MASI.
# print rows in toc.masi not in chem.samples
# All samples sent to MASI were expected, good.
setdiff(toc.masi[,c("lake_id", "sample_depth", "sample_type")],
        chem.samples.foo[c("lake_id", "sample_depth", "sample_type")])



# Have all 2020 toc samples comprehensive sample list been analyzed by MASI?
# Print rows from comprehensive sample list not in MASI data.
# Good, all samples accounted for.
setdiff(chem.samples.foo %>% filter(analyte == "toc", # only toc sent to MASI
                                    sample_year == 2020) %>% # only 2020 sent to MASI
          select(lake_id, sample_depth, sample_type),
        toc.masi[c("lake_id", "sample_depth", "sample_type")]) %>%
  arrange(lake_id) %>% print(n=Inf)

