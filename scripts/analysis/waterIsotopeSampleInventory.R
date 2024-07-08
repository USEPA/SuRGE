wis_inventory <- read_excel(paste0(userPath, "data/siteDescriptors/SuRGE Inventory.xlsx"), na = "NA") %>%
  janitor::clean_names() %>%
  select(-analyte) %>%
  mutate(sample_depth = case_when(is.na(sample_depth) ~ "shallow",
                                  TRUE ~ sample_depth),
         across(contains("sample"), tolower)) %>%
# Filter out blanks and those with 0 volume remaining
  filter(sample_type != "Blank", volume != 0)

# 12 shallow filtered
# 14 deep filtered
wis_inventory %>% 
  select(-notes, -contains("volume")) %>%
  distinct(lake_id, filtered, sample_depth) %>%
  summarize(n_shallow_filtered = sum(sample_depth == "shallow" & filtered == TRUE),
            n_shallow_unfiltered = sum(sample_depth == "shallow" & filtered == FALSE),
            n_deep_filtered = sum(sample_depth == "deep" & filtered == TRUE),
            n_deep_unfiltered = sum(sample_depth == "deep" & filtered == FALSE))

# Break into groups to assess sample inventory
filtered_shallow <- wis_inventory %>%
  filter(sample_depth == "shallow" & filtered == TRUE) %>%
  select(lake_id) %>%
  distinct

filtered_deep <- wis_inventory %>%
  filter(sample_depth == "deep" & filtered == TRUE) %>%
  select(lake_id) %>%
  distinct

unfiltered_shallow <- wis_inventory %>%
  filter(sample_depth == "shallow" & filtered == FALSE) %>%
  select(lake_id) %>%
  distinct

unfiltered_deep <- wis_inventory %>%
  filter(sample_depth == "deep" & filtered == FALSE) %>%
  select(lake_id) %>%
  distinct

# what sites have filtered deep but not filtered shallow?
# 13 and 214
filtered_deep$lake_id[!(filtered_deep$lake_id %in% filtered_shallow$lake_id)]

# how about unfiltered shallow but not filtered shallow?
# 13 and 214
unfiltered_shallow$lake_id[!(unfiltered_shallow$lake_id %in% filtered_shallow$lake_id)] #13 and 214

# Any unfiltered samples from lakes that don't have shallow filtered
unfiltered_shallow$lake_id[!(unfiltered_shallow$lake_id %in% filtered_shallow$lake_id)] #13 and 214
unfiltered_deep$lake_id[!(unfiltered_deep$lake_id %in% filtered_shallow$lake_id)] # 165, 214, 13



