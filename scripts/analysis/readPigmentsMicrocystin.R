# script for reading chlorophyll, phycocyanin, and microcystin measured
# at Narragansett laboratory.


# Read list of samples received by NAR.
nar.samples <- read_excel(paste0(userPath,
                                 "data/sampleTrackingSheets//NAR algal indicator//",
                                 "narSampleReceiptList.xlsx"))

# Were all samples received expected?  Compare list of received samples to 
# those expected [10/14/2022]
setdiff(nar.samples[c("lake_id", "analyte", "sample_type")],
        chem.samples.foo[c("lake_id", "analyte", "sample_type")]) %>% print(n=Inf)


# Have all NAR algae samples in comprehensive sample list been delivered to NAR?
# Missing lake 204 samples.  Confirmed missing by NAR.  Samples cannot be located 
setdiff(chem.samples.foo %>% filter(analyte_group == "algae.nar", 
                                    sample_year != 2018) %>% # 2018 R10 not sent to NAR
          select(lake_id, analyte, sample_type),
        nar.samples[c("lake_id", "analyte", "sample_type")]) %>%
  arrange(lake_id) %>% print(n=Inf)

# Jeff Hollister: the values are the concentration in the sample 
# (i.e. filter volume has been accounted for)
get_chla_data <- function(path, data, sheet) { 
  
  d <- read_csv(paste0(path, data)) %>% #
    janitor::clean_names() %>% # 
    mutate(site_id = as.numeric(gsub(".*?([0-9]+).*", "\\1", site))) %>% # convert to numeric
    mutate(sample_depth = "shallow") %>% # all samples were collected near a-w interface
    mutate(units = "ug_l") %>% # to match naming conventions
    
    # PLACEHOLDERS until data available 
    mutate(chla_qual = "H") %>% # qual (holding time) flag
    mutate(chla_ship = "S") %>% # shipping flag 
    
    rename(sample_type = reps, 
           lake_id = waterbody, 
           chla_units = units, 
           chla = value)  %>%
    # mutate(hold_time = collection_date - extraction_date) %>% # get hold time
    # mutate(chla_qual = case_when( # flag with "H" if hold time exceeded
    #   hold_time > 60 ~ "H",
    #   TRUE   ~ "")) %>%
    # No detection limits reported for 2020-2021 chlorophyll data
    select(lake_id, site_id, sample_type, sample_depth, 
           chla, chla_qual, chla_ship) %>%
    unite("chla_flags", chla_qual, chla_ship, sep = " ") # merge all flag columns
    # This data currently has placeholder flags (as of 9/26/2022)
    # Until data are available, the following 4 lines are commented out:
  # %>%
  #   mutate(chla_flags = if_else( # NA if there are no flags
  #     str_detect(chla_flags, "\\w"), chla_flags, NA_character_) %>%
  #       str_squish()) # remove any extra white spaces
  
  return(d)
  
}

get_phyco_data <- function(path, data, sheet) { 
  
  d <- read_csv(paste0(path, data)) %>% #
    janitor::clean_names() %>% # 
    mutate(site_id = as.numeric(gsub(".*?([0-9]+).*", "\\1", site))) %>% # convert to numeric
    mutate(sample_depth = "shallow") %>% # all samples were collected near a-w interface
    mutate(units = "ug_l") %>% # to match naming conventions
    
    # PLACEHOLDERS until data available 
    # See https://github.com/jhollist/surge_algal/issues
    mutate(phycocyanin_qual = "H") %>% # qual (holding time) flag
    mutate(phycocyanin_ship = "S") %>% # shipping flag 
    
    rename(sample_type = reps, 
           lake_id = waterbody, 
           phycocyanin_units = units, 
           phycocyanin = value)  %>%
    # mutate(hold_time = collection_date - extraction_date) %>% # get hold time
    # mutate(chla_qual = case_when( # flag with "H" if hold time exceeded
    #   hold_time > 60 ~ "H",
    #   TRUE   ~ "")) %>%
    # No detection limits reported for 2020-2021 phycocyanin data
    select(lake_id, site_id, sample_type, sample_depth, 
           phycocyanin, phycocyanin_qual, phycocyanin_ship) %>%
    unite("phycocyanin_flags", phycocyanin_qual, 
          phycocyanin_ship, sep = " ") # merge all flag columns
  # This data currently has placeholder flags (as of 9/26/2022)
  # Until data are available, the following 5 lines are commented out:
  # %>%
  #   mutate(phycocyanin_flags = if_else( # NA if there are no flags
  #     str_detect(phycocyanin_flags, "\\w"), 
  # phycocyanin_flags, NA_character_) %>%
  #       str_squish()) # remove any extra white spaces

  
  return(d)
  
}


# chlorophyll, phycocyanin, and microcystin results
cin.pig.path <- paste0(userPath,
                       "data/algalIndicators/pigments/")


chla_20_21 <- get_chla_data(cin.pig.path,
                               "surge_chla_all_2020_2021.csv") 

phycocyanin_20_21 <- get_phyco_data(cin.pig.path,
                            "surge_phyco_all_2020_2021.csv")


pigments_20_21 <- left_join(chla_20_21, phycocyanin_20_21, 
                            by = c("lake_id", "site_id", 
                                   "sample_depth", "sample_type")) %>%
  select(-chla_flags, -phycocyanin_flags) # exclude these fields for now. 3/29/2022

