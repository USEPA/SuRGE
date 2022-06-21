# SCRIPT FOR AGGREGATING FIELD DUPLICATES AND OMITTING BLANKS FROM CHEMISTRY

# # Dummy example with flag, units, qual, and numeric column-----------
# # 147 has two <.  Lake 137 has a single <.  Lake 155 has no <.  Test all
# # conditions.
# chemistry.l <- chemistry %>% filter(lake_id == "155") %>%
#   group_by(lake_id, site_id, sample_depth) %>%
#   select(sample_type, contains("nh4")) %>%
#   mutate(nh4_flag = replace(nh4_flag, nh4_flag == "", NA))
# 
# 
# # looks good.  Why isn't case_when working?
# chemistry.m <- chemistry.l %>% 
#   filter(!(sample_type == "blank")) %>% 
#   mutate(m_nh4 = mean(nh4)) %>% 
#   # mutate(nh4_flag = case_when(
#   #   all(is.na(nh4_flag)) ~ NA, # if all nh4_flag values are NA, then NA
#   #   all(!is.na(nh4_flag)) ~ "<", # if both nh4_flag are <, then <
#   #   TRUE ~ "")) # if only one is <, then NA
#   mutate(nh4_flag = ifelse(all(is.na(nh4_flag)), NA, # if all nh4_flag values are NA, then NA
#                            ifelse(all(!is.na(nh4_flag)), "<",  # if both nh4_flag are <, then <
#                                   NA)), # if only one is <, then NA
#          nh4_units = unique(nh4_units),
#          nh4_qual = ifelse(all(nh4_qual == TRUE), TRUE, # if all nh4_qual are TRUE, the TRUE
#                            ifelse(all(nh4_qual == FALSE), FALSE, # if both qual fields are F, then F
#                                   FALSE))) %>% # if T and F, report F
#   filter(!(sample_type == "duplicate")) %>%
#   select(-sample_type) # no longer needed
  



# Generalize across Dummy example with flag, units, qual, and numeric column-----------
# chemistry.l <- chemistry %>% 
#   filter(lake_id %in% c("147", "137")) %>%
#   mutate(across(contains("flag"), ~ replace(., . == "", NA))) %>% # see issue 36
#   group_by(lake_id, site_id, sample_depth)
# 
# 
# # looks good.  
# chemistry.m <- chemistry.l %>% 
#   filter(!(sample_type == "blank")) %>% 
#   # site_id is numeric, but ignored below because it is a grouping variable.
#   mutate(across(where(is.numeric), mean, na.rm = TRUE),
#          across(contains("flag"), 
#                 ~ ifelse(all(is.na(.)), NA, # if all _flag values are NA, then NA
#                          ifelse(all(!is.na(.)), "<",  # if both _flag values are <, then <
#                                 NA))), # if only one is <, then NA
#          across(contains("qual"),
#                 ~ ifelse(all(. == TRUE), TRUE, # if all _qual are TRUE, the TRUE
#                          ifelse(all(. == FALSE), FALSE, # if both qual fields are F, then F
#                                 FALSE))),  # if T and F, report F
#          across(contains("units"), unique)) %>% # identical units for all observations within a group
#   filter(!(sample_type == "duplicate")) %>%
#   select(-sample_type) # no longer needed



# Generalize to function----------------
clean_chem <- function(data) {
  data %>% 
    rename(no23 = no2_3, no23_flag = no2_3_flag) %>% # temporarily rename the columns w/ 2 underscores
    mutate(across(contains("flag"), ~ # convert all NA flags to "no value" if corresponding analyte is NA
                    ifelse(is.na(get(word(paste(cur_column(), .), sep = "_"))) == TRUE, 
                           "no value", .))) %>%
    group_by(lake_id, site_id, sample_depth) %>%
    filter(!(sample_type == "blank")) %>% 
    # site_id is numeric, but ignored below because it is a grouping variable.
    mutate(across(where(is.numeric), mean, na.rm = TRUE),
           across(contains("flag"), 
                  ~ ifelse(any(is.na(.) == TRUE), NA, # if any _flag values are NA, then NA
                           ifelse(any(. == "<"), "<",  # if any _flag values are <, then <
                                  "no value"))), # any remaining cases = "no value" (i.e., corresponding analytes are NA)
           across(contains("qual"),
                  ~ ifelse(all(. == TRUE), TRUE, # if all _qual values are TRUE, the TRUE
                           ifelse(all(. == FALSE), FALSE, # if both qual fields are F, then F
                                  FALSE))),  # if T and F, report F
           across(contains("units"),
                  ~ ifelse(any(str_detect(., "_") == TRUE), first(str_sort(.)), # if any group has units ("_" detected), use same units  
                                  .))) %>% # if no units in group, no change (i.e., NA)
    filter(!(sample_type == "duplicate")) %>% # now we can remove dups
    rename(no2_3 = no23, no2_3_flag = no23_flag) %>% # changes columns back to original names in wiki
    select(-sample_type) %>% # no longer need sample_type (all unknowns)
    ungroup() %>% # remove grouping
    mutate(across(contains("flag"), # "no value" text no longer needed; convert to NA
                  ~ ifelse(. == "no value", NA, .)))
  
} 

chemistry <- clean_chem(chemistry_all)
dim(chemistry_all) # 210, 131 [3/30/2022]
dim(chemistry) # 154, 130 [3/30/2022], good fewer rows, but one fewer column (-sample_type)

names(chemistry_all)[!names(chemistry_all) %in% names(chemistry)] # sample_type removed, good


