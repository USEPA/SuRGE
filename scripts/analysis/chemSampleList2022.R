# 2022 sample list

# CREATE LIST OF CHEM SAMPLES
# 1.  filter comprehensive lake list to 2022 lakes
lake.list.2022 <- lake.list %>% 
  filter(sample_year == 2022, # 2022 lakes
         !grepl(c("PI|LD|TR"), sample_year)) %>% # exclude inaccessible lakes (may not be necessary)
  select(lake_id, sample_year, lab)


# 2. Assume half of lakes sampled by each lab gets qa.qc 
# foo is number of lakes getting qa.qc.  funky code accounts for odd number of
# lakes, in which calculating half isn't straightforward
foo <- lake.list.2022 %>% group_by(lab) %>%
  summarize(n = n()) %>% 
  mutate(qa.qc.n = ifelse(((n/2) %% 2 == 0) | n == 2,
                          n/2,
                          (n-1)/2)
         )

# hardcode qa.qc indicator.  Don't know which lakes will get qa.qc,
# so assigning randomly
lake.list.2022 <- lake.list.2022 %>%
  mutate(qa_qc = c(1, NA, # RTP, 2 lakes, one qa.qc
                   1,1, 1, NA, NA, NA, # NAR, 6 lakes, three qa.qc
                   1, 1, NA, NA, # DOE, 4 lakes, 2 qa.qc
         rep(1,7), rep(NA, 8), #CIN, 15 lakes, 7 qa.qc
                   rep(1,3), rep(NA, 4))) # ADA 7 lakes, 3 qa.qc


# 4. create vectors of analyte groups.
nutrients <- c("nh4", "no2_3", "no3", "no2", "tn", "tp", "op")
anions <- c("fluoride", "cl", "br", "so4")
organics <- c("doc", "toc")
metals <- c("al", "as", "ba", "be", "ca",  
            "cd", "cr", "cu", "fe",
            "k", "li",  "mg", "mn", "na",
            "ni", "pb", "p", "sb", "si",
            "sn", "sr", "s", "v", "zn")
algae.nar <- c("microcystin", "phycocyanin", "chla")
algae.gb <- c("taxonomy", "physiology")
suva <- "suva"

# 5. create df of chem samples collected from qa.qc lakes.
qa.qc.samples <- expand.grid(lake_id = lake.list.2022 %>% # lake_id for all sampled lakes
                               filter(qa_qc == 1) %>% # filter to qa.qc lakes
                               select(lake_id) %>% # pull lake_id
                               pull(), # extract lake_id to vector
                             sample_type = c("duplicate", "blank"),
                             analyte = c(nutrients, anions, organics, 
                                         metals, algae.nar, suva), # no qa.qc for algae.gb analytes
                             sample_depth = "shallow", # qa.qc only collected from shallow depth
                             stringsAsFactors = FALSE, KEEP.OUT.ATTRS = FALSE) %>%
  as_tibble() %>%
  mutate(sample_depth = replace(sample_depth, sample_type == "blank", "blank")) %>% # blank depth = blank
  arrange(lake_id)

# 6. create df of samples collected from all lakes
unknown.samples <- expand.grid(lake_id = lake.list.2022$lake_id, # lake_id for all sampled lakes
                               sample_type = "unknown", # unknowns collected at all lakes
                               analyte = c(nutrients, anions, organics, 
                                           metals, algae.nar, algae.gb, suva),
                               sample_depth = c("shallow", "deep"),
                               stringsAsFactors = FALSE, KEEP.OUT.ATTRS = FALSE) %>%
  as_tibble() %>%
  # algal indicator and suva samples not collected at depth.  Filter out
  filter(!(sample_depth == "deep" & analyte %in% c(algae.nar, algae.gb, suva))) %>%
  arrange(lake_id)

# 7. Combine df of qa.qc and df of unknowns.  Merge 'lab' and 'sample_year' fields
# from lake.list.
chem.samples.2022 <- rbind(qa.qc.samples, unknown.samples) %>%
  full_join(., select(lake.list.2022, lake_id, lab, sample_year)) %>% 
  arrange(sample_year, lab, lake_id, sample_type, sample_depth) %>%
  relocate(sample_year, lab, lake_id, sample_type, sample_depth, analyte)

# 7. Add analyte_group field to df
chem.samples.2022 <- chem.samples.2022 %>%
  mutate(analyte_group = case_when(analyte %in% nutrients ~ "nutrients",
                                   analyte %in% anions ~ "anions",
                                   analyte %in% organics ~ "organics",
                                   analyte %in% metals ~ "metals",
                                   analyte %in% algae.nar ~ "algae.nar",
                                   analyte %in% algae.gb ~ "algae.gb",
                                   analyte %in% suva ~ "suva",
                                   TRUE ~ "oops"))

unique(chem.samples$analyte_group) # no 'oops', good

# 8. Work out total number of samples
# mutate analyte list to reflect individual samples
chem.samples.2022 %>% 
  mutate(analyte = case_when(analyte %in% nutrients ~ "nutrients",
                             analyte %in% anions ~ "anions",
                             # analyte %in% organics ~ "organics", # need to retain separate doc/toc
                             analyte %in% metals ~ "metals",
                             # analyte %in% algae.nar ~ "algae.nar", # separate chla, micro, phyco
                             # analyte %in% algae.gb ~"algae.gb", # separate tax and physio
                             TRUE ~ analyte)) %>%
  select(-analyte_group) %>% # get rid of group
  distinct() %>% # condense to distinct
  count(analyte)











