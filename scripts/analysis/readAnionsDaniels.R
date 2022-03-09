# Script for reading anions analyzed at AWBERC by Kit Daniels for CIN, RTP, and
# USGS samples collected in 2021.

# see ...data/chemistry/anions_ada_daniels/Daniels_Anions_2021.xlsx for data
# see issue 10 for analyte naming conventions

# 1. Function to read daniels anion data---------------
path <- paste0(userPath,
               "data/chemistry/anions_ada_daniels/",
               "Daniels_Anions_2021_2_import.xls")

sheet <- "2021_SURGE_anions"

get_daniels <- function(path, sheet){
  top_table <- read_excel(path = path, sheet = sheet, na = "n.a.") %>%
    janitor::clean_names() %>%
    select(-matches(("no2|no3|po4|full|dilution"))) %>% # these analytes are taken from Lachat
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
           d_anion_analysis_date = as.Date(gsub( " .*$", "", injection_date_time), format = "%m/%d/%Y")) %>%
    mutate(lake_id = case_when(str_detect(sample_id, "LAC") ~ paste0(lake_id, "_lacustrine"),
                               str_detect(sample_id, "TRAN") ~ paste0(lake_id, "_transitional"),
                               str_detect(sample_id, "RIV") ~ paste0(lake_id, "_riverine"),
                               lake_id == "69" ~ "69_riverine",
                               TRUE ~ lake_id)) %>%
    # create the analyte_flag column
    mutate(f_flag = case_when(is.na(f_mg_l) ~ "<", TRUE ~ NA_character_)) %>% 
    mutate(cl_flag = case_when(is.na(cl_mg_l) ~ "<", TRUE ~ NA_character_)) %>% 
    mutate(br_flag = case_when(is.na(br_mg_l) ~ "<", TRUE ~ NA_character_)) %>% 
    mutate(so4_flag = case_when(is.na(so4_mg_l) ~ "<", TRUE ~ NA_character_)) %>% 
    # sub mdl for NA
    mutate(f_mg_l = case_when(is.na(f_mg_l) ~ 0.005, TRUE ~ f_mg_l),
           cl_mg_l = case_when(is.na(cl_mg_l) ~ 0.03, TRUE ~ cl_mg_l),
           br_mg_l = case_when(is.na(br_mg_l) ~ 0.02, TRUE ~ br_mg_l),
           so4_mg_l = case_when(is.na(so4_mg_l) ~ 0.025, TRUE ~ so4_mg_l))

# 2. Extract units and analyte names
analytes.units <- top_table %>% 
  select(contains("_l")) %>% 
  colnames(.) %>%
  tibble(analyte = str_extract(., pattern = "[^_]+"),
         #https://statisticsglobe.com/extract-substring-before-or-after-pattern-in-r
         unit = sub(".*?_", "", .)) %>% #? ensures first occurrence of _ is indexed
  select(-.) %>%
  #pivot_longer(analyte) %>%
  pivot_wider(names_from = analyte, values_from = unit) %>%
  rename_with(.cols = everything(), ~paste0(., "_units"))

# 3. merge data with units
top_table <- top_table %>%
  # rename columns to analyte name, remove units
  rename_with(., 
              .cols = contains("_l"), # rename columns with this pattern
              ~sub("_.*", "", .x) # extract before first occurrence of _
              ) %>% 
  cbind(., analytes.units) %>% # add units to data.
  mutate(site_id = as.numeric(site_id)) %>% # make site id numeric
  select(-sample_id, -injection_date_time) %>%
  as_tibble()

top_table
}

# 2. Use function to read data-------------
d.anions <- get_daniels(path, sheet)

# 3. Merge with sample collection date and calculate qual_flag-------------------

# Get sample collection date from fld_sheet.  This date not entered anywhere, but
# it was either day 1 or 2 at lake.  Conservatively assume day 1.
sample_date <- fld_sheet %>%
  select(lake_id, trap_deply_date) %>% # day 1
  filter(!is.na(trap_deply_date)) %>%
  group_by(lake_id) %>%
  summarize(sample_col_date = min(trap_deply_date))

# join date with d.anions
dim(d.anions) # 147 rows
d.anions <- left_join(d.anions, sample_date) 
dim(d.anions) # 147 rows


# calculate holding time violation (-qual)
d.anions <- d.anions %>%
  add_column(f_qual = NA,
         cl_qual = NA,
         br_qual = NA,
         so4_qual = NA) %>%
  mutate(across(contains("qual"), 
                ~ if_else((d_anion_analysis_date - sample_col_date) > 28, TRUE, FALSE))) %>%  # TRUE = hold time violation)
  select(-contains("date"))
    

# 4. Merge lake_id, site_id, sample_depth, sample_type  
# see issue #32 for details




janitor::get_dupes(d.anions %>% # no dups
                     select(lake_id, site_id, sample_depth, sample_type)

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



