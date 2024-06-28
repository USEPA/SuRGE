# NID DATA FOR SuRGE SITES
# PROVIDED BY DEEMER 11/28/2023, MISSING LAKES SUBSEQUENTLY UPDATED BY PEGASUS
nid_link <- read_csv(paste0(userPath, "data/siteDescriptors/nid_links_and_dam_age.csv")) %>%
  janitor::clean_names() %>%
  rename(lake_id = site_id) %>%
  group_by(lake_id) %>%
  slice_min(year_completed) %>% # dups for multiple dams on same lake constructed at same year
  distinct(year_completed)

dim(nid_link_min) #147
  
#Jake--Do we need this?  Do you want me to add script to create the link internally?
# nid <- read_csv("https://nid.sec.usace.army.mil/api/nation/csv", skip = 1) %>%
#   janitor::clean_names()
