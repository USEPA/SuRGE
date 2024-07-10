# Read NLA 2017 data

# Chemistry
nla17_chem <- readr::read_csv(paste0(userPath, 
                                "data/nla17/nla_2017_water_chemistry_chla-data.csv"),
                              na = c("", " ")) %>%
  janitor::clean_names() %>%
  select(site_id, visit_no, analyte, result, result_units) %>% # select needed values
  #dplyr::rename(id = site_id) %>% # set up for below
  dplyr::rename_with(~paste0("nla17_", .)) %>% # append nla17_ to all columns names
  # missing units on this observation causing problems when pivoting to wide
  # replace NA for units with correct units
  mutate(across(nla17_result_units, 
                ~replace(., nla17_site_id == "NLA17_WI-10001" & nla17_analyte == "ALUMINUM", "MG/L"))) %>%
  # Need to aggregate across visits 1 and 2.  Group by nla_id and analyte
  dplyr::group_by(nla17_site_id, nla17_analyte) %>% # group to aggregate visits 1 and 2
  summarize(nla17_result = mean(nla17_result, na.rm = TRUE), # aggregate results across visits
            nla17_result_units = unique(nla17_result_units)) %>% # retain units
  mutate(across(nla17_result, ~replace(., is.nan(.), NA))) %>%
  ungroup() 

# pivot to wide
nla17_chem <- nla17_chem %>%
  pivot_wider(names_from = nla17_analyte, values_from = c(nla17_result, nla17_result_units)) %>%
  rename_with(., ~ gsub("_result", "", .x)) %>% # remove "-result" from column names
  janitor::clean_names()

