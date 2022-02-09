# Script for reading anions analyzed at AWBERC by Kit Daniels for CIN, RTP, and
# USGS samples collected in 2021.

# see ...data/chemistry/anions_ada_daniels/Daniels_Anions_2021.xlsx for data
# see issue 10 for analyte naming conventions

# 1. Read data
d.anions <- read_excel(paste0(userPath,
                              "data/chemistry/anions_ada_daniels/",
                              "Daniels_Anions_2021.xls"),
                       sheet = "2021_SURGE_anions",
                       na = "n.a.") %>%
  janitor::clean_names() %>%
  select(-matches(("no2|no3|po4"))) %>% # these analytes are taken from Lachat
  # rename(fluoride_mg_l = f_mg_l) %>% # rename to be consistent with github Wiki
  mutate(sample_depth = case_when(grepl("blank", sample_id, ignore.case = TRUE) ~ "blank",
                                  grepl("shallow", sample_id, ignore.case = TRUE) ~ "shallow",
                                  grepl("deep", sample_id, ignore.case = TRUE) ~ "deep",
                                  TRUE ~ "oops"),
         sample_type = case_when(grepl("dup", sample_id, ignore.case = TRUE) ~ "duplicate",
                                 grepl("blank", sample_id, ignore.case = TRUE) ~ "blank",
                                 TRUE ~ "unknown"),
         #https://stackoverflow.com/questions/35403491/r-regex-extracting-a-string-between-a-dash-and-a-period
         lake_id = gsub("^[^-]*-([^-]+).*", "\\1", sample_id) %>% 
           as.numeric() %>% as.character(),
         d_anion_analysis_date = as.Date(gsub( " .*$", "", analysis_date), format = "%m/%d/%Y")) %>%
  mutate(lake_id = case_when(str_detect(sample_id, "LAC") ~ paste0(lake_id, "_lacustrine"),
                             str_detect(sample_id, "TRAN") ~ paste0(lake_id, "_transitional"),
                             str_detect(sample_id, "RIV") ~ paste0(lake_id, "_riverine"),
                             lake_id == "69" ~ "69_riverine",
                             TRUE ~ lake_id))

# 2. Extract units and analyte names
analytes.units <- d.anions %>% 
  select(contains("_l")) %>% 
  colnames(.) %>%
  tibble(analyte = str_extract(., pattern = "[^_]+"),
         #https://statisticsglobe.com/extract-substring-before-or-after-pattern-in-r
         unit = sub(".*?_", "", .)) %>% #? ensures first occurence of _ is indexed
  select(-.) %>%
  #pivot_longer(analyte) %>%
  pivot_wider(names_from = analyte, values_from = unit) %>%
  rename_with(.cols = everything(), ~paste0(., "_units"))

# 3. merge data with units
d.anions <- d.anions %>%
  # rename columns to analyte name, remove units
  rename_with(., 
              .cols = contains("_l"), # rename columns with this pattern
              ~sub("_.*", "", .x) # extract before first occurrence of _
              ) %>% 
  cbind(., analytes.units) %>% # add units to data.
  mutate(site_id = as.numeric(site_id)) %>% # make site id numeric
  select(-no, -sample_id, -analysis_date, -d_anion_analysis_date, -comments)

# Sample Inventory Audit.-#-#-##-#-##-#-##-#-##-#-##-#-#

# 5. Compare samples analyzed to those in comprehensive sample list.
# [1/25/2022] all samples in comprehensive list. Good

# print rows in d.anions not in chem.samples.
setdiff(d.anions[c("lake_id", "sample_depth", "sample_type")],
        chem.samples.foo %>% 
          filter(analyte_group == "anions",
                 sample_year >= 2020,
                 lab != "ADA") %>%
          select(lake_id, sample_depth, sample_type)) 

# 6. # Have all Daniels anion samples in comprehensive sample list been analyzed?
# Print rows from comprehensive sample list not in Daniels anion data.
# Missing a bunch
# setdiff(chem.samples.foo %>% filter(analyte_group == "anions", 
#                                     sample_year == 2021, # only 2021 samples to daniels
#                                     lab != "ADA") %>% # ADA ran their own anions 
#           select(lake_id, sample_depth, sample_type),
#         d.anions[,c("lake_id", "sample_depth", "sample_type")]) %>%
#   arrange(lake_id) %>%
#   write.table(file = paste0(userPath, "data/chemistry/anions_ada_daniels/danielsMissingAnions.txt"), row.names = FALSE)



