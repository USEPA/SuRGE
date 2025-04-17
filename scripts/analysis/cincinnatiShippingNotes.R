# SAMPLES SHIPPED TO CINCINNATI ARE CHECKED FOR CONDITION WHEN THEY ARRIVE.
# CONDITION IS DOCUMENTED IN EXCEL.  SAMPLES IN POOR CONDITION WHEN RECEIVED
# MUST BE FLAGGED WITH AN S.

# Add shipping flags----

# Read in shipping notes data
shipping_data <-
  read_excel(
    paste0(
      userPath, 
      "data/sampleTrackingSheets/conditionOfSamplesWhenReceivedInCincinnati.xlsx"))

# Some information captured in this spreadsheet does not merit a shipping
# flag.  Manipulate object to contain only observations that necessitate a
# flag.
shipping_data <- shipping_data %>%
  # chemistry samples from two USGS lakes were warm when received 
  filter(lake_id %in% c("288", "316")) %>% # [12/8/22] only lakes that require a flag
  mutate(flag = "S") %>%
  select(lake_id, visit, flag, analyte_group)

  
  
# Add the shipping notes to chemistry_all and add "S" flags as needed
chemistry_all <- 
  chemistry_all %>%
  left_join(., shipping_data, # join to get shipping notes
            by = c("lake_id", "visit")) %>% 
  mutate(across(
    # this code isn't generalized to all possible shipping issues.  As of 12/8/2022,
    # only chemistry samples affected by shipping issues (not algae pigments, microcystin,
    # taxonomy, etc).  Also, metals are acidified and stored at room temperature and
    # are therefore unaffected by warming during shipping.  Code below adds a shipping
    # flag to chemistry analytes for affected samples.  In future, be wary of S
    # flags for pigment samples received at CIN, that then are flagged again when
    # received at NAR (they warmed en route to NAR).  This could result in two S
    # flags for one sample, which we probably don't want.
    paste(c("nh4", "no2_3", "no2", "tn", "tp", "op", "f", "cl", "br", "so4"), "flags", sep = "_"), # mutate all flag columns for chemistry analytes
    ~ case_when(is.na(flag) ~ ., # if no shipping issue, flags do not change
                is.na(.) ~ "S", # if there is a shipping issue, but no other flag, add "S" to flag column
                TRUE ~ str_c(., "S", sep = " ")))) %>% # Otherwise add "S" to existing flags
  select(-flag) # remove flag column inherited from shipping_data




# Arrange columns----
chemistry_all <- chemistry_all %>%
  select(-analyte_group) %>% # inherited from shipping_data, not needed
  relocate(lake_id, site_id, sample_depth, sample_type, visit, 
           sort(colnames(.))) # others arranged alphabetically
