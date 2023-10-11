# THIS SCRIPT CREATES A DATAFRAME OF ALL CHEM SAMPLES THAT SHOULD
# HAVE BEEN COLLECTED.  CHECK THIS LIST AGAINST REPORTED DATA
# AND/OR CHAIN OF CUSTODY FORMS TO ENSURE ALL SAMPLES WERE RECEIVED
# AND ANALYZED




# CREATE LIST OF CHEM SAMPLES
# 1.  Expand the rows for Oahe and Francis Case to reflect the separate sampling 
# at the lacustrine, transitional, and riverine zones.
oahe_FrancisCase <- lake.list %>% # see readSurgeLakes.R
  filter(lake_id %in% 69:70) %>% # extract these two lakes
  slice(rep(1:n(), each = 3)) %>% # create three replicates of each
  mutate(lake_id = paste(lake_id, # change lake_id to reflect reservoir zone
                         c("lacustrine", "transitional", "riverine"),
                         sep = "_"))

# replace 69 and 70 in lake.list with oahe_FrancisCase
lake.list.chem <- lake.list %>% # see readSurgeLakes.R
  filter(!(lake_id %in% 69:70)) %>% # exclude oahe and Francis Case
  mutate(lake_id = as.character(lake_id)) %>% # convert to ch to allow join
  full_join(., oahe_FrancisCase) # join expanded Oahe and FC with lake.list

# 2. filter comprehensive lake list to lakes that have been sampled
lake.list.chem <- lake.list.chem %>% 
  filter(sample_year <= 2023, # excludes lakes with NA for sample_year
    !grepl(c("PI|LD|TR"), eval_status_code), # exclude inaccessible lakes
         !(lake_id == "250" & visit == 1), # chem samples from 250 visit 1 were lost
         !(lake_id == "281" & visit == 1)) # chem samples from 281 visit 1 were lost

# 3. qa.qc lakes had blanks and dups collected. This must be reflected in final
# df of collected samples.  Read list of qa.qc lakes.
qa.qc <- readxl::read_excel(paste0(userPath, 
                                   "data/QAQCsamplesCollected.xlsx")) %>%
  janitor::clean_names() %>%
  mutate(qa_qc = 1) # column indicating qa.qc samples collected

# merge qa.qc with lake.list.chem
nrow(lake.list.chem) # 99
lake.list.chem <- left_join(lake.list.chem, qa.qc) # keep all 
nrow(lake.list.chem) # 99

# 4. create vectors of analyte groups.
nutrients <- c("nh4", "no2_3", "no2", "tn", "tp", "op")
anions <- c("f", "cl", "br", "so4")
organics <- c("doc", "toc")
metals <- c("al", "as", "ba", "be", "ca",  
            "cd", "cr", "cu", "fe",
            "k", "li",  "mg", "mn", "na",
            "ni", "pb", "p", "sb", "si",
            "sn", "sr", "s", "v", "zn")
algae.nar <- c("microcystin", "phycocyanin", "chla")
algae.gb <- c("taxonomy", "physiology")

# 5. create df of chem samples collected from qa.qc lakes.
# R10 and 2020 sampling did not include doc, anions.  Include here, but
# filter out in step 8.
qa.qc.samples <- expand.grid(lake_id = lake.list.chem %>% # lake_id for all sampled lakes
                               filter(qa_qc == 1) %>% # filter to qa.qc lakes
                               select(lake_id) %>% # pull lake_id
                               pull(), # extract lake_id to vector
                             sample_type = c("duplicate", "blank"),
                             analyte = c(nutrients, anions, organics, 
                                         metals, algae.nar), # no qa.qc for algae.gb analytes
                             sample_depth = "shallow", # qa.qc only collected from shallow depth
                             stringsAsFactors = FALSE, KEEP.OUT.ATTRS = FALSE) %>%
  mutate(sample_depth = replace(sample_depth, sample_type == "blank", "blank"), # blank depth = blank
         visit = case_when(lake_id == 281 ~ 2, # qa.qc collected from 281 on visit == 2
                           lake_id == 148 ~2, # qa.qc collected from 148 on visit == 2
                           TRUE ~ 1)) %>% # all others collected on visit == 1
  arrange(lake_id)

# 6. create df of samples collected from all lakes
# 2018 (R10) and 2020 sampling (CIN, RTP, R10) did not include doc, anions, 
# taxonomy, physiology.  Adjust for this below.

# lake_id for all sampled lakes.
unknown.samples <- expand.grid(lake_id = lake.list.chem$lake_id, 
                               sample_type = "unknown", # unknowns collected at all lakes
                               analyte = c(nutrients, anions, organics, 
                                           metals, algae.nar, algae.gb),
                               sample_depth = c("shallow", "deep"),
                               stringsAsFactors = FALSE, KEEP.OUT.ATTRS = FALSE) %>%
  mutate(visit = case_when(lake_id %in% c("250", "281") ~ 2, # visit == 1 samples were lost, only visit == 2 submitted
                           TRUE ~ 1)) %>% # all other sites unchanged (147 and 148 should have visits 1 and 2, see below)
  # algal indicator samples not collected at depth.  Filter out
  filter(!(sample_depth == "deep" & analyte %in% c(algae.nar, algae.gb))) %>%
  arrange(lake_id)

unknown.samples <- unknown.samples %>%
  filter(lake_id == 147 | lake_id == 148) %>% # pull out rows for 147, and 148
  mutate(visit = 2) %>% # change visit to vist == 2
  bind_rows(unknown.samples) # add two new rows to original df

# 7. Combine df of qa.qc and df of unknowns.  Merge 'lab' and 'sample_year' fields
# from lake.list.
# R10 and 2020 sampling did not include doc, anions, taxonomy, physiology.  Filter
# out in step 8.
chem.samples <- rbind(qa.qc.samples, unknown.samples) %>%
  full_join(., select(lake.list.chem, lake_id, lab, sample_year)) %>% 
  arrange(sample_year, lab, lake_id, sample_type, sample_depth) %>%
  relocate(sample_year, lab, lake_id, sample_type, sample_depth, analyte)

# 7. Add analyte_group field to df
chem.samples <- chem.samples %>%
  mutate(analyte_group = case_when(analyte %in% nutrients ~ "nutrients",
                                   analyte %in% anions ~ "anions",
                                   analyte %in% organics ~ "organics",
                                   analyte %in% metals ~ "metals",
                                   analyte %in% algae.nar ~ "algae.nar",
                                   analyte %in% algae.gb ~"algae.gb",
                                   TRUE ~ "oops"))

unique(chem.samples$analyte_group) # no 'oops', good

# 8. Filter out samples that were not collected in some years.
chem.samples.foo <- chem.samples %>%
  # use ! to exclude any samples that meet these criteria
  # R10 in 2018 did not include doc, anions, taxonomy, physiology, or metals. 
  filter(!((lab == "R10" & sample_year == 2018) & # for R10 sampling in 2018
             (analyte %in% c("microcystin", "phycocyanin", "doc") | # that contain these
                analyte_group %in% c("algae.gb", "anions", "metals")))) %>% 
  # R10 only collected at shallow depth in 2018
  filter(!((lab == "R10" & sample_year == 2018) & # for R10 sampling in 2018
             sample_depth == "deep")) %>%
  # 2020 samples (CIN, RTP, R10) did not include doc, anions, taxonomy, or physiology.
  filter(!((sample_year == 2020) & # for R10, RTP, and CIN samples in 2020
             (analyte == "doc" | # that contain these
                analyte_group %in% c("algae.gb", "anions")))) %>% # or these
  # chla and phycocyanin blanks not collected at lake_id == 67
  filter(!(lake_id == "67" & analyte %in% c("chla", "phycocyanin") & sample_type == "blank")) %>%
  # no deep chemistry at 69_lacustrine (forgot van dorn)
  filter(!(lake_id == "69_lacustrine" & sample_depth == "deep")) %>% 
  # blanks collected at 238, but no duplicates
  filter(!(lake_id == "238" & sample_type == "duplicate")) %>%
  # duplicates collected at 238, but no blanks
  filter(!(lake_id == "265" & sample_type == "blank")) %>% 
  # toc and doc blanks not collected at 155 (as best we can tell)
  filter(!(lake_id == "155" & sample_type == "blank" & analyte %in% (c("doc", "toc")))) %>%
  # toc and doc blanks not collected from 67.  No DI brought to field
  filter(!(lake_id == "67" & sample_type == "blank" & analyte %in% c("doc", "toc"))) %>%
  # chl unknown filter tore during sample prep at 148.  no replacement available
  filter(!(lake_id == "148" & sample_type == "unknown" & analyte == "chla")) %>%
  # no DOC blank collected at 275
  filter(!(lake_id == "275" & sample_type == "blank" & analyte == "doc")) %>%
  # no DOC blank collected at 64
  filter(!(lake_id == "64" & sample_type == "blank" & analyte == "doc")) %>%
  # no DOC blank collected at 65
  filter(!(lake_id == "65" & sample_type == "blank" & analyte == "doc")) %>%
  # no anion deep collected at 65 (chemistry065NARTtoCIN06September2022.pdf)
  filter(!(lake_id == "65" & sample_type == "unknown" & sample_depth == "deep" & analyte_group == "anions")) %>%
  # this sample lost in lab.  See See 2/21/2023 email from Maily Pham
  filter(!(lake_id == "240" & sample_type == "unknown" & analyte == "doc" & sample_depth == "deep")) %>%
  # shallow water put in deep toc and doc shallow.  These vials were discarded at AWBERC.
  filter(!(lake_id == "053" & analyte %in% c("doc", "toc") & sample_depth == "deep")) %>%
  arrange(sample_year, lab, lake_id, sample_type, analyte_group, sample_depth)
         
         
# 9.  chem sampling for R10 2018 was done at trib and open_water site.  Blanks 
# and dups mostly at trib site, but we will only read in open_water site data
# to be consistent with SuRGE 2020-2023.  R10 collected one blank and one dup
# at open_water site:
# LGR, 302, site 10, has field duplicate of shallow sample: TOC, nutrients, chla
# WPL (308) has blank at site 1. TOC, nutrients, chla
# create small dataframes containing the qa.qc samples for these lakes.
lgr <- chem.samples.foo %>% filter(lake_id == "302") %>%
  mutate(sample_type = "duplicate" )
wpl <- chem.samples.foo %>% filter(lake_id == "308") %>%
  mutate(sample_type = "blank", sample_depth = "blank")  

# Bind these dfs to main df
chem.samples.foo <- rbind(chem.samples.foo, lgr, wpl) %>% as_tibble()
                                     