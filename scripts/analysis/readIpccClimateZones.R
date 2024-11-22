# Read IPCC climate zones
climate <- sf::st_read(paste0(userPath, "data/spatial/climateMap"), 
                       layer = "ipcc_zones_2017_names_dissolve") %>%
   select(fldd_zn) %>% # pull out Flooded Lands climate zone
  rename(climate = fldd_zn) %>%
  st_make_valid()



# All sites, spatial
surge_sf <- dplyr::bind_rows(lake.list, # no "lacustrine", "transitional", "riverine" (same climate zone)
                             lake.list.2016) %>% 
  select(lake_id, sample_year, contains("dd83")) %>% # exclude visit
  filter(!is.na(sample_year)) %>% # omit lakes not sampled
  select(-sample_year) %>% # different values for ADA revisits 147 and 148
  distinct %>% # values repeated for revisits (149 lakes)
  st_as_sf(coords = c("lon_dd83", "lat_dd83"), crs = 4326) %>%
  st_make_valid()

# Assign climate zone
surge_climate <- st_intersection(surge_sf, climate) %>%
  st_drop_geometry %>%
  select(lake_id, climate)


