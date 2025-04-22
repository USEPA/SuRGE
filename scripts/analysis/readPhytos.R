## Read in phytoplankton community composition info for SuRGE Lakes
## Received from Avery Tatters on 3/14/2025

phyto_data <-  read_excel(paste0(userPath,
                                 "data/algalIndicators/SuRGE Taxonomy 2021-23 v4.xlsx"), 
                          sheet = "SuRGE Taxonomy- 2021-23") %>%
  janitor::clean_names() %>%
  select(site_id, year_col, algal_group, phylum,class, order, family, genus, density) %>%
  # distinct(site_id) # no lacustrine...
  rename(lake_id = site_id) %>%
  mutate(lake_id = str_extract(lake_id, "(\\d+$)") %>% # extract numeric part of lake_id
           as.numeric(), # convert lake_id to numeric
         visit = case_when(lake_id == 147 & year_col == 2021 ~ 1,
                           lake_id == 147 & year_col == 2023 ~ 2,
                           lake_id == 148 & year_col == 2021 ~ 1,
                           lake_id == 148 & year_col == 2023 ~ 2,
                           lake_id == 250 ~ 2, # samples from visit 1 lost
                           lake_id == 281 ~ 2, # samples from visit 1 lost
                           TRUE ~ 1), # visit 1 for all others
         density_units = "cells_ml") %>%
  select(-year_col) %>%
  relocate(lake_id, visit) %>%
  filter(!is.na(lake_id))


# Are all phyto_data lake_id values in lake.list?
phyto_data %>% filter(!(lake_id %in% lake.list$lake_id)) # yes!

# are all sampled lakes in phyto data?
# No, asked Avery 4/22/2025
lake.list.all %>% 
  filter(eval_status_code == "S", sample_year >= 2021) %>%
  filter(!(lake_id %in% as.character(phyto_data$lake_id))) %>% # pyto_data doesn't contain any 69 or 70 data
  arrange(sample_year) %>%
  select(lake_id, lab, sample_year) %>%
  print(n=Inf) %>%
  write_csv(., "output/missing_phyto.csv")

# CONDENSED VERSION FOR MERGE_PREDICTORS----------
phyto_data_link <- phyto_data %>%
  filter(algal_group == "CYANOBACTERIA") %>%
  group_by(lake_id, visit) %>%
  summarize(density = sum(density),
            density_units = "cells_ml")
  group_by