
site_descriptors_data %>% 
  as_tibble %>%
  slice(2301:2367) %>% # subset to not overwhelm API. 100 works, 150 is too big 2367
  mutate(
    lat = as.numeric(lat),
    long = as.numeric(long),
    time_zone = tz_lookup_coords(
      lat = lat, 
      lon = long,
      method = "accurate"),
    elevation = ""
  ) %>% 
  select(
    lat, long, elevation, time_zone, sample_start, sample_end) %>% 
  write.table(., file = "clipboard-16384", row.names = FALSE, 
              col.names = FALSE, sep = ",", quote=FALSE)
