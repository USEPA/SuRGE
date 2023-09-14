# this script reads in the SuRGE survey design, including site weights.

lake.list <- readxl::read_excel(paste0(userPath, 
                                       "surgeDsn/SuRGE_design_20191206_eval_status.xlsx")) %>%
  janitor::clean_names() %>%
  dplyr::rename(lake_id = site_id) %>%
  dplyr::rename(nla_id = site_id_2) %>%
  mutate(lake_id = substr(lake_id, 5, 8) %>% # extract numeric part of lake_id
           as.numeric(), #convert lake_id to numeric
         visit = 1) # all lakes except 250 and 281 were only visited once.  250/281 handled below

# Add visit 2 for 250 and 281 (CIN sites with lost cooler) and 147/148 (ADA sites revisited for quality)
lake.list <- lake.list %>% 
  filter(lake_id == 250 | lake_id == 281 | lake_id == 147 | lake_id == 148) %>% # pull out rows for 250, 281, 147, and 148
  mutate(visit = 2) %>% # change visit to vist == 2
  bind_rows(lake.list) # add two new rows to original df
