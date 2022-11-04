# CREATE A DATAFRAME CONTAINING WATER CHEMISTRY SAMPLE DATES FOR HOLDING TIME CALCULATIONS

# Date of water chem sample collection was recorded on sample bottles, but not on data sheets.
# Some labs recorded the date on the bottle and provided that information in the data files (ADA),
# but others did not (NAR algal pigments).  Here we select the earliest sampling date for each
# lake.  It is likely that water chem samples were actually collected on the second day at some
# of these lakes, but this approach is conservative.

sample_date <- fld_sheet %>% # this object contains dates for trap deployment, retrieval, and floating chamber
  select(lake_id, trap_deply_date) %>% # pull out trap deployment date, nearly always day 1
  distinct() %>% # duplicate values across the sites within each lake.  boil down to distinct values
  na.omit() %>% # remove records where lake_id or deployment data is NA
  rename(sample_date = trap_deply_date) %>%
  # Need to create visit column.  visit = 1 for all sites except revisit at 250 and 281
  mutate(visit = case_when(
    lake_id %in% c("281", "250") & sample_date > as.Date("2022-08-20") ~ 2, # 2 for revisit
    TRUE ~ 1)) %>% #all else 1
  # some records have multiple trap deployment dates (when work was ditributed over several days)
  # here we extract earliest date
  group_by(lake_id, visit) %>%
  arrange(sample_date) %>%
  slice(1L) # pulls first row within each group (earliest sampling date)



