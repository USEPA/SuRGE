# Read NLA 2017 data

# Chemistry
nla17.chem <- readr::read_csv(paste0(userPath, 
                                "data/nla17/nla_2017_water_chemistry_chla-data.csv"),
                              na = c("", " ")) %>%
  janitor::clean_names() %>%
  select(site_id, visit_no, analyte, result, result_units) %>% # select needed values
  dplyr::rename(id = site_id) %>% # set up for below
  dplyr::rename_with(~paste0("nla_", .)) %>% # append nla_ to all columns names
  # missing units on this observation causing problems when pivoting to wide
  # replace NA for units with correct units
  mutate(across(nla_result_units, 
                ~replace(., nla_id == "NLA17_WI-10001" & nla_analyte == "ALUMINUM", "MG/L"))) %>%
  # Need to aggregate across visits 1 and 2.  Group by nla_id and analyte
  dplyr::group_by(nla_id, nla_analyte) %>% # group to aggregate visits 1 and 2
  summarize(nla_result = mean(nla_result, na.rm = TRUE), # aggregate results across visits
            nla_result_units = unique(nla_result_units)) %>% # retain units
  mutate(across(nla_result, ~replace(., is.nan(.), NA))) %>%
  ungroup() 

# pivot to wide
nla17.chem <- nla17.chem %>%
  pivot_wider(names_from = nla_analyte, values_from = c(nla_result, nla_result_units)) %>%
  rename_with(., ~ gsub("_result", "", .x)) # remove "-result" from column names

