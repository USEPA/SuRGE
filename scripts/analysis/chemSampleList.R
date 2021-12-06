# THIS SCRIPT CREATES A DATAFRAME OF ALL CHEM SAMPLES THAT SHOULD
# HAVE BEEN COLLECTED.  CHECK THIS LIST AGAINST REPORTED DATA
# AND/OR CHAIN OF CUSTODY FORMS TO ENSURE ALL SAMPLES WERE RECEIVED
# AND ANALYZED




# CREATE LIST OF CHEM SAMPLES
# 1.  Expand the rows for Oahe and Francis Case to reflect the separate sampling 
# at the lacustrine, transitional, and riverine zones.
oahe_FrancisCase <- lake.list %>% 
  filter(lake_id %in% 69:70) %>% # extract these two lakes
  slice(rep(1:n(), each = 3)) %>% # create three replicates of each
  mutate(lake_id = paste(lake_id, # change lake_id to reflect reservoir zone
                         c("lacustrine", "transitional", "riverine")))

# replace 69 and 70 in lake.list with oahe_FrancisCase
lake.list.chem <- lake.list %>% # see readSurgeLakes.R
  filter(!(lake_id %in% 69:70)) %>% # exclude oahe and Francis Case
  mutate(lake_id = as.character(lake_id)) %>% # convert to ch to allow join
  full_join(., oahe_FrancisCase) # join expanded Oahe and FC with lake.list

# 2. filter comprehensive lake list to lakes that have been sampled
lake.list.chem <- lake.list.chem %>% 
  filter(sample_year <= 2021, # lakes sampled in or before 2021
         !grepl(c("PI|LD|TR"), sample_year)) # exclude inacessible lakes (may not be necessary)

# 3. qa.qc lakes had blanks and dups collected. This must be reflected in final
# df of collected samples.  Read list of qa.qc lakes.
qa.qc <- readxl::read_excel(paste0(userPath, 
                                   "data/QAQCsamplesCollected.xlsx")) %>%
  janitor::clean_names() %>%
  mutate(qa_qc = 1) # column indicating qa.qc samples collected

# merge qa.qc with lake.list.chem
nrow(lake.list.chem) # 62
lake.list.chem <- left_join(lake.list.chem, qa.qc) # keep all 
nrow(lake.list.chem) # 62

##############eliminate _aes from names!!!!!!!!!!!!!!!!!!!!#######################
# 4.  create df of chem samples collected from qa.qc lakes
# R10 and 2020 sampling did not include doc, anions.  Adjust for this below.
qa.qc.samples <- expand.grid(lake_id = lake.list.chem %>% # lake_id for all sampled lakes
                               filter(qa_qc == 1) %>% # filter to qa.qc lakes
                               select(lake_id) %>% # pull lake_id
                               pull(), # extract lake_id to vector
                             sample_type = c("duplicate", "blank"),
                             analyte = c("nh4", "no2_3", "no3", "no2", "tn", "tp", "op",  # nutrients
                                         "fluoride", "cl", "br", "so4", # anions
                                         "doc", "toc", # organics
                                         "al_aes",  "as_aes", # metals
                                         "ba_aes", "be_aes", "ca_aes",  "cd_aes",   "cr_aes", "cu_aes",  "fe_aes",
                                         "k_aes",   "li_aes",  "mg_aes", "mn_aes", "na_as",  "ni_aes",  "pb_aes",
                                         "p_aes",   "sb_aes",  "si_aes",  "sn_aes",  "sr_aes",  "s_aes",   "v_aes", 
                                         "zn_aes",
                                         "microcystin", "phycocyanin", "chla") # algal indicators
                             sample_depth = "shallow") %>% # qa.qc only collected from shallow depth
  arrange(lake_id)

# 5. create df of samples collected from all lakes
# R10 and 2020 sampling did not include doc, anions, taxonomy, physiology.  Adjust for this below.
unknown.samples <- expand.grid(lake_id = lake.list.chem$lake_id, # lake_id for all sampled lakes
                               sample_type = "unknown", # unknowns collected at all lakes
                               analyte = c("nh4", "no2_3", "no3", "no2", "tn", "tp", "op",  # nutrients
                                           "fluoride", "cl", "br", "so4", # anions
                                           "doc", "toc", # organics
                                           "al_aes",  "as_aes", # metals
                                           "ba_aes", "be_aes", "ca_aes",  "cd_aes",   "cr_aes", "cu_aes",  "fe_aes",
                                           "k_aes",   "li_aes",  "mg_aes", "mn_aes", "na_as",  "ni_aes",  "pb_aes",
                                           "p_aes",   "sb_aes",  "si_aes",  "sn_aes",  "sr_aes",  "s_aes",   "v_aes", 
                                           "zn_aes",
                                           "microcystin", "phycocyanin", "chla", # algal indicators
                                           "taxonomy", "physiology"),
                               sample_depth = c("shallow", "deep")) %>%
  # algal indicator samples not collected at depth.  Filter out
  filter(!(sample_depth == "deep" & analyte %in% c("microcystin", "phycocyanin", "chla", # algal indicators
                                                 "taxonomy", "physiology"))) %>%
  arrange(lake_id)

# 6. Combine df of qa.qc and df of unknowns.  Merge 'lab' and 'sample_year' fields
# from lake.list.
# R10 and 2020 sampling did not include doc, anions, taxonomy, physiology.  Adjust for this below.
chem.samples <- rbind(qa.qc.samples, unknown.samples) %>%
  full_join(., select(lake.list.chem, lake_id, lab, sample_year))

# 7. Add analyte_group field to df
nutrients <- c("nh4", "no2_3", "no3", "no2", "tn", "tp", "op")
anions <- c("fluoride", "cl", "br", "so4")
organics <- c("doc", "toc")
metals <- c("al_aes",  "as_aes", "ba_aes", "be_aes", "ca_aes",  
                             "cd_aes",   "cr_aes", "cu_aes",  "fe_aes",
                             "k_aes",   "li_aes",  "mg_aes", "mn_aes", "na_as",
                             "ni_aes",  "pb_aes", "p_aes",   "sb_aes", "si_aes",
                             "sn_aes",  "sr_aes",  "s_aes",   "v_aes",  "zn_aes")
algae.nar <- c("microcystin", "phycocyanin", "chla")
algae.gb <- c("taxonomy", "physiology")

chem.samples <- chem.samples %>%
  mutate(analyte_group = ifelse(analyte %in% nutrients,
                                "nutrients",
                                ifelse(analyte %in% anions,
                                       "anions",
                                       ifelse(analyte %in% organics,
                                              "organics",
                                              ifelse(analyte %in% metals,
                                                     "metals",
                                                     ifelse(analyte %in% algae.nar,
                                                            "algae.nar",
                                                            ifelse(analyte %in% algae.gb,
                                                                   "algae.gb",
                                                                   "oops")))))))
unique(chem.samples$analyte_group) # no 'oops', good

# 8. R10 and 2020 sampling did not include doc, anions, taxonomy, physiology.  
# Adjust for this here.
chem.samples.foo <- chem.samples %>%
  filter(!(lab == "R10" & (analyte %in% c("microcystin", "phycocyanin", "doc") |
                             analyte_group %in% c("algae.gb", "anions"))))
         
         
         
         
         
         
         