data <- tibble(lake_id = rep(68, 5),
               site_id = "SU-4", 
               sample_depth = c("deep", rep("shallow", 4)),
               filter = "filter",
               rep = c(1,1,2,1,2),
               sample_type = c("unknown", "blank", "duplicate", "unknown", "unknown"),
               analyte = "nh4",
               finalConc = c(535, 3.3, 3, 8.3, 4.4))

# sample_type == duplicate defines a laboratory dup.  It will have the same rep value
# as the corresponding unknown.  Convert duplicate to unknown
data <- data %>% mutate(sample_type = case_when(sample_type == "duplicate" ~ "unknown",
                                                TRUE ~ sample_type))

# Now we can group by lake_id, site_id, sample_depth, rep, analyte, and sample_type to aggregate the lab dup,
# while preserving the field duplicate
data <- data %>% group_by(lake_id, site_id, sample_depth, sample_type, rep, analyte) %>% summarize(finalConc = mean(finalConc, na.rm = T))

# Now we can recode sample type to identify field duplicate.  It will have a rep value of 2 or B
data <- data %>% mutate(sample_type = case_when(rep %in% c(2, "B") ~ "duplicate",
                                                TRUE ~ sample_type)) %>%
  select(-rep)

data

