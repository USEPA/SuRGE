# script for reading microcystin measured at Narragansett laboratory.

# 11 Jan 2023: Flags already added to data by Jeff Hollister. 
# Commenting out all code related to determining flags. (jwc) 

get_microcystin_data <- function(path) { 
  
  d <- read_csv(path) %>% #
    janitor::clean_names() %>% # 
    mutate(site_id = as.numeric(gsub(".*?([0-9]+).*", "\\1", site_id)), # convert to numeric
           sample_depth = "shallow", # all samples were collected near a-w interface
           lake_id = str_remove(waterbody, "^0+")) %>% # remove leading 0 from character string
    rename(sample_type = field_dups,
           microcystin = value,
           microcystin_units = units) %>%

    # Create visit field
    mutate(visit = case_when(lake_id %in% c("281", "250") &
                               between(date %>% as.Date(format = "%m/%d/%Y"),
                                       as.Date("2022-08-15"),
                                       as.Date("2022-09-23")) ~ 2,
                             lake_id %in% c("147", "148") &
                               between(date %>% as.Date(format = "%m/%d/%Y"),
                                       as.Date("2023-08-01"),
                                       as.Date("2023-08-30")) ~ 2,
                             TRUE ~ 1)) %>%
    select(lake_id, site_id, sample_type, sample_depth, visit,
           microcystin, microcystin_units) %>%
    filter(sample_type != "blank")

  
  return(d)
  
}

path <- paste0(userPath, "data/algalIndicators/surge_microcystin.csv")
microcystin <- get_microcystin_data(path)
