# script for reading microcystin measured at Narragansett laboratory.

# 11 Jan 2023: Flags already added to data by Jeff Hollister. 
# Commenting out all code related to determining flags. (jwc) 

get_microcystin_data <- function(path) { 
  
  d <- read_csv(path, na = "NA") %>% #
    janitor::clean_names() %>% # 
    mutate(site_id = as.numeric(gsub(".*?([0-9]+).*", "\\1", site)), # convert to numeric
           sample_depth = "shallow", # all samples were collected near a-w interface
           lake_id = str_remove(waterbody, "^0+"), # remove leading 0 from character string
           
           # Create visit field
           visit = case_when(lake_id %in% c("281", "250") &
                               between(date %>% as.Date(format = "%m/%d/%Y"),
                                       as.Date("2022-08-15"),
                                       as.Date("2022-09-23")) ~ 2,
                             lake_id %in% c("147", "148") &
                               between(date %>% as.Date(format = "%m/%d/%Y"),
                                       as.Date("2023-08-01"),
                                       as.Date("2023-08-30")) ~ 2,
                             TRUE ~ 1),
           # deal with instances of "<0.000", which causes the field to be character
           value = case_when(value == "<0.000" ~ 0,
                             TRUE ~ as.numeric(value))) %>%
    rename(sample_type = field_dups,
           microcystin = value,
           microcystin_units = units,
           microcystin_flags = flag) %>%
    
    
    select(lake_id, site_id, sample_type, sample_depth, visit,
           microcystin, microcystin_units, microcystin_flags) 
  
  
  return(d)
  
}

path <- paste0(userPath, "data/algalIndicators/surge_microcystin.csv")
microcystin <- get_microcystin_data(path)

# check for dups
# 20 on 4/16/2025. Asked Jeff to investigate. Will strip out for now
microcystin %>% janitor::get_dupes(lake_id, lake_id, site_id, sample_depth, sample_type, visit)

# remove dups for now
microcystin <- microcystin[!duplicated(microcystin[c("lake_id", "lake_id", "site_id", "sample_depth", "sample_type", "visit")]), ]

# confirm no dups
microcystin %>% janitor::get_dupes(lake_id, lake_id, site_id, sample_depth, sample_type, visit)

# lakes 69 and 70 do not have _lac, _riv, _trans, etc. 
# a few lakes with incorrect site_id value
# Strip until Jeff fixes. Issue # 9 at surge_algal repo
microcystin <- microcystin %>%
  filter(!(lake_id %in% c("69", "70")),
         !(lake_id == 230 & site_id == 0 & visit == 1), # site_id 0?
         !(lake_id == 44 & site_id == 77 & visit == 1), # site_id == 77?
         !(lake_id == 76 & site_id == 20 & visit == 1) # site_id == 20?
  )
