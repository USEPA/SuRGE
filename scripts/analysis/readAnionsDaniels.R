# Script for reading anions analyzed at AWBERC by Kit Daniels for CIN, RTP, and
# USGS samples collected in 2021.

# see ...data/chemistry/anions_ada_daniels/Daniels_Anions_2021.xlsx for data
# see issue 10 for analyte naming conventions

# 1. Read data
d.anions <- read_excel(paste0(userPath,
                              "data/chemistry/anions_ada_daniels/",
                              "Daniels_Anions_2021.xlsx"),
                       sheet = "2021_SURGE_anions",
                       na = "n.a.") %>%
  janitor::clean_names() %>%
  select(-matches(("no2|no3|po4"))) %>% # these analytes are taken from Lachat
  rename(fluoride_mg_l = f_mg_l) # rename to be consistent with github issue #10

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
  cbind(., analytes.units) # add units to data.

# Sample Inventory Audit.-#-#-##-#-##-#-##-#-##-#-##-#-#

# 5. Compare samples analyzed to those in comprehensive sample list.
# All analyzed samples are in comprehensive sample list, good.

# print rows in d.anions not in chem.samples.
setdiff(d.anions[c("lake_id", "sample_depth", "sample_type")],
        chem.samples.foo %>% 
          filter(analyte_group == "anions",
                 sample_year >= 2020,
                 lab != "ADA") %>%
          select(lake_id, sample_depth, sample_type)) 

# 6. # Have all Daniels anion samples in comprehensive sample list been analyzed?
# Print rows from comprehensive sample list not in Daniels anion data.
# Missing a bunch, but I see nar sample receipt list doesn't contain any 2020
# samples.  Have NAR update file, then revisit.  I inspected the 2021 samples
# not in NAR inventory and communicated to Stephen Shivers.  Stephen confirmed
# that he analyzed the samples and will update the Excel file.
setdiff(chem.samples.foo %>% filter(analyte_group == "anions", 
                                    sample_year == 2021, # only 2021 samples to daniels
                                    lab != "ADA") %>% # ADA ran their own anions 
          select(lake_id, sample_depth, sample_type),
        d.anions[,c("lake_id", "sample_depth", "sample_type")]) %>%
  arrange(lake_id) %>%
  write.table(file = "clipboard")



