# this script reads in the SuRGE survey design, including site weights.

lake.list <- readxl::read_excel(paste0(userPath, 
                                       "surgeDsn/SuRGE_design_20191206_eval_status.xlsx")) %>%
  janitor::clean_names() %>%
  dplyr::rename(lake_id = site_id) %>%
  mutate(lake_id = substr(lake_id, 5, 8) %>% # extract numeric part of lake_id
           as.numeric()) #convert lake_id to numeric

