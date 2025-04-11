# NID DATA FOR SuRGE SITES
# PROVIDED BY DEEMER 11/28/2023, MISSING LAKES SUBSEQUENTLY UPDATED BY PEGASUS IN SPRING 2024
nid_link <- read_csv(paste0(userPath, "data/siteDescriptors/nid_links_and_dam_age.csv")) %>%
  janitor::clean_names() %>%
  rename(lake_id = site_id) %>%
  group_by(lake_id, nid_id) %>%
  slice_min(year_completed) %>% # dups for multiple dams on same lake constructed at same year
  distinct(year_completed)

dim(nid_link) #147

# Pull Dam Latitudes and Longitudes
# data originally downloaded on 4/11/2025, then written to disk
# read_csv("https://nid.sec.usace.army.mil/api/nation/csv", skip = 1) %>%
#   janitor::clean_names() %>%
#   select(federal_id, name, latitude, longitude) %>% 
#   write.csv(nid, paste0(userPath, "data/siteDescriptors/nid.csv"), row.names = FALSE)
#   nid <- read_csv(paste0(userPath, "data/siteDescriptors/nid.csv"))

# read and format local copy
nid <- read_csv(paste0(userPath, "data/siteDescriptors/nid.csv")) %>%
  rename(nid_id = federal_id,
         dam_name = name,
         dam_latitude = latitude,
         dam_longitude = longitude)
  

# merge nid data with nid_link
nid_link <- left_join(nid_link, nid, by= "nid_id")
nid_link$lake_id <- as.numeric(nid_link$lake_id)
nid_link <- ungroup(nid_link)
