# this script reads in the SuRGE survey design, including site weights.

lake.list <- readxl::read_excel(paste0(userPath, 
                                       "surgeDsn/SuRGE_design_20191206_eval_status.xlsx"), na = c("", "NA")) %>%
  janitor::clean_names() %>%
  dplyr::rename(lake_id = site_id) %>%
  dplyr::rename(nla17_site_id = site_id_2) %>%
  dplyr::rename(nla_unique_id = unique_id) %>%
  mutate(lake_id = str_extract(lake_id, "(\\d+$)") %>% # extract numeric part of lake_id
           as.numeric(), # convert lake_id to numeric
         visit = 1) %>% # all lakes except 147, 148, 250, and 281 were only visited once.  see below
  filter(lake_id <= 1000) # 999 and 1000 are handpicked R10 site and PR, respectively.  1001 - 1032 are the 2016 multireservoir survey lakes + Falls Lake

# Add visit 2 for 250 and 281 (CIN sites with lost cooler) and 147/148 (ADA sites revisited for quality)
lake.list <- lake.list %>% 
  filter(lake_id == 250 | lake_id == 281 | lake_id == 147 | lake_id == 148) %>% # pull out rows for 250, 281, 147, and 148
  mutate(sample_year = case_when(lake_id %in% c(147, 148) ~ 2023, # ADA resampled in 2023, originally sampled in 2021
                                 TRUE ~ sample_year), # CIN (lakes 250/281) conducted visits 1 and 2 in same year 
         visit = 2) %>% # change visit to vist == 2
  bind_rows(lake.list) # add two new rows to original df


lake.list.2016 <- readxl::read_excel(paste0(userPath, 
                                       "surgeDsn/SuRGE_design_20191206_eval_status.xlsx"), na = c("", "NA")) %>%
  janitor::clean_names() %>%
  dplyr::rename(lake_id = site_id) %>%
  dplyr::rename(nla17_site_id = site_id_2) %>%
  dplyr::rename(nla_unique_id = unique_id) %>%
  mutate(lake_id = str_extract(lake_id, "(\\d+$)") %>% # extract numeric part of lake_id
           as.numeric(), # convert lake_id to numeric
         visit = 1) %>% # all lakes except 147, 148, 250, and 281 were only visited once.  see below
  filter(lake_id > 1000) # 1001 - 1032 are the 2016 multireservoir survey lakes + Falls Lake
