# script for reading chlorophyll, phycocyanin, and microcystin measured
# at Narragansett laboratory.

# 11 Jan 2023: Flags already added to data by Jeff Hollister. 
# Commenting out all code related to determining flags. (jwc) 


# Jeff Hollister: the values are the concentration in the sample 
# (i.e. filter volume has been accounted for)
get_chla_data <- function(path, data, sheet) { 
  
  d <- read_csv(paste0(path, data)) %>% #
    janitor::clean_names() %>% # 
    mutate(site_id = as.numeric(gsub(".*?([0-9]+).*", "\\1", site))) %>% # convert to numeric
    mutate(sample_depth = "shallow") %>% # all samples were collected near a-w interface
    mutate(units = "ug_l") %>% # to match naming conventions

    # mutate(chla_qual = "H") %>% # qual (holding time) flag
    # mutate(chla_ship = "S") %>% # shipping flag 
    
    rename(sample_type = field_dups, 
           lake_id = waterbody, 
           chla_units = units, 
           chla = value)  %>%
    
    # mutate(hold_time = collection_date - extraction_date) %>% # get hold time
    # mutate(chla_qual = case_when( # flag with "H" if hold time exceeded
    #   hold_time > 60 ~ "H",
    #   TRUE   ~ "")) %>%
    
    # Create visit field
    mutate(visit = case_when(lake_id %in% c("281", "250") &
                               between(date %>% as.Date(format = "%m/%d/%Y"),
                                       as.Date("2022-08-15"),
                                       as.Date("2022-09-15")) ~ 2,
                             lake_id %in% c("147", "148") &
                               between(date %>% as.Date(format = "%m/%d/%Y"),
                                       as.Date("2023-08-01"),
                                       as.Date("2023-08-30")) ~ 2,
                             TRUE ~ 1)) %>%
    select(lake_id, site_id, sample_type, sample_depth, visit,
           chla, chla_units, chla_flag) 

    # unite("chla_flags", chla_qual, chla_ship, sep = " ") %>% 
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
    
    # mutate(phycocyanin_qual = "H") %>% # qual (holding time) flag
    # mutate(phycocyanin_ship = "S") %>% # shipping flag 
    
    rename(sample_type = field_dups, 
           lake_id = waterbody, 
           phycocyanin_units = units, 
           phycocyanin = value)  %>%
    
    # mutate(hold_time = collection_date - extraction_date) %>% # get hold time
    # mutate(chla_qual = case_when( # flag with "H" if hold time exceeded
    #   hold_time > 60 ~ "H",
    #   TRUE   ~ "")) %>%
    
    # Create visit field
    mutate(visit = case_when(lake_id %in% c("281", "250") &
                             between(date %>% as.Date(format = "%m/%d/%Y"),
                                     as.Date("2022-08-15"),
                                     as.Date("2022-09-15")) ~ 2,
                             lake_id %in% c("147", "148") &
                               between(date %>% as.Date(format = "%m/%d/%Y"),
                                       as.Date("2023-08-01"),
                                       as.Date("2023-08-30")) ~ 2,
                             TRUE ~ 1)) %>%
    
    select(lake_id, site_id, sample_type, sample_depth, visit, 
           phycocyanin, phycocyanin_units, phyco_flag) %>%
    # variable names must be same for aggregateFieldDupsStripFieldBlanks.R
    rename(phycocyanin_flag = phyco_flag)
    
    # unite("phycocyanin_flags", phycocyanin_qual, 
    #       phycocyanin_ship, sep = " ") %>% # merge all flag columns:
    #   mutate(phycocyanin_flags = if_else( # NA if there are no flags
    #     str_detect(phycocyanin_flags, "\\w"), 
    # phycocyanin_flags, NA_character_) %>%
    #       str_squish()) # remove any extra white spaces
    
  
  return(d)
  
}


# chlorophyll, phycocyanin, and microcystin results
cin.peg.path <- paste0(userPath,
                       "data/algalIndicators/pigments/")

chla <- get_chla_data(cin.peg.path,
                      "surge_chl_all_years.csv")

phycocyanin <- get_phyco_data(cin.peg.path,
                              "surge_phyco_all_years.csv")

# chla_20_21_22 <- get_chla_data(cin.pig.path,
#                                "surge_chla_all_2020_2021_2022.csv") 
# 
# phycocyanin_20_21_22 <- get_phyco_data(cin.pig.path,
#                             "surge_phyco_all_2020_2021_2022.csv")

nrow(chla) #160
nrow(phycocyanin) #161
pigments <- full_join(chla, phycocyanin, 
                            by = c("lake_id", "site_id", 
                                   "sample_depth", "sample_type", "visit")) 



# SAMPLE INVENTORY------------------------
# Read list of samples received by NAR.
nar.samples <- read_excel(paste0(userPath,
                                 "data/sampleTrackingSheets//NAR algal indicator//",
                                 "narSampleReceiptList.xlsx"))


# Were all samples received expected?  Compare list of received samples to
# those expected. YES. [4/2/2024]
setdiff(nar.samples[c("lake_id", "visit", "analyte", "sample_type")],
        chem.samples.foo[c("lake_id", "visit", "analyte", "sample_type")]) %>%
  print(n=Inf)


# Have all NAR algae samples in comprehensive sample list been delivered to NAR?
# Missing lake 204 samples.  Confirmed missing by NAR.  Sample found on lab floor
# and framed in Jeff's office!
# but are we also missing lake 204 microcystin?
setdiff(chem.samples.foo %>%
          filter(analyte_group == "algae.nar",
                 sample_year != 2018) %>% # 2018 R10 not sent to NAR
          select(lake_id, analyte, sample_type),
        nar.samples[c("lake_id", "analyte", "sample_type")]) %>%
  arrange(lake_id) %>% print(n=Inf)


# Now lets make sure that all samples were analyzed
pigments_analyzed <- pigments %>%
  pivot_longer(cols = c(chla, phycocyanin),
                names_to = "analyte",
              values_to = "concentration") %>%
  select(lake_id, sample_type, sample_depth, visit, analyte)

pigments_collected <- chem.samples.foo %>% 
  filter(analyte %in% c("phycocyanin", "chla"),
         sample_year != 2018, # 2018 R10 not sent to NAR
          #sample_year < 2023, #
         sample_type != "blank") %>% # NAR not sure how to handle blanks.  Active issue in hollister's repo
  select(lake_id, sample_type, sample_depth, visit, analyte)

# Have all collected NAR pigment samples been analyzed?
# 204 was known to be lost, see above
# See issue #6 at https://github.com/jhollist/surge_algal/issues/6
setdiff(pigments_collected, pigments_analyzed) %>% print(n=Inf)

# Are all pigment samples samples analyzed at NAR in list of samples sent to NAR?
# 148 shallow unknown.  Filter tore, no replacement available.  No sample
# shipped to NAR.  This shows up as sample because chla value was populated
# with NA during join with phyco.  ALL GOOD!
setdiff(pigments_analyzed, pigments_collected) %>% print(n=Inf)

