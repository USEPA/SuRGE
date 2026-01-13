# The data paper files contain stratification indices aggregated across
# reservoir zones (riverine, transitional, lacustrine) in lakes 69 and 70.
# For the statistical analysis, it would be better to have the data by zone. 
# Here we write out zone specific values for use in the surge_stats repo.

inner_join(
  # stratification indices by lake, no site_id
  strat_link %>%
    filter(grepl(c("lacustrine|transitional|riverine"), lake_id)),
  
  # site_id of depth profile
  depth_profile_dates %>% 
    filter(grepl(c("lacustrine|transitional|riverine"), lake_id)) %>%
    distinct(lake_id, site_id, visit)
  
) %>%
  mutate(
    # move zone to site_id
    site_id = case_when(
      grepl("riverine", lake_id) ~ paste0(site_id, "_riverine"),
      grepl("transitional", lake_id) ~ paste0(site_id, "_transitional"),
      grepl("lacustrine", lake_id) ~ paste0(site_id, "_lacustrine")
    ),
    lake_id = case_when(
      grepl("69_", lake_id) ~ "69",
      grepl("70_", lake_id) ~ "70"
    ),
    thermdep2 = case_when(
      is.nan(thermdep2) ~ NA,
      TRUE ~ thermdep2
    ),
    buoyf_units = "s-2",
    thermdep2_units = "m"
  ) %>%
  arrange(lake_id) %>%
  write_csv(., "output/69_70_stratification_indices.csv")

