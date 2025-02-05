
# READ POLYGONS
# whole lake polygon with major ecological divisions
whole_069 <- st_read(paste0(userPath, "lakeDsn/CIN/CH4-069/merc069.shp")) # Oahe
whole_070 <- st_read(paste0(userPath, "lakeDsn/CIN/CH4-070/merc070.shp")) # Francis Case 

# Aggregate whole reservoir polygons by zone (aggregate sections within zones)
# and calculate area of each zone.
zones <- imap(list("69" = whole_069, "70" = whole_070),
              ~.x %>%
                janitor::clean_names() %>%
                group_by(zone) %>%
                summarize %>%
                st_transform(5070) %>% # albers equal area. Better for calculating area?
                mutate(area_m2 = as.numeric(st_area(.)),
                       lake_id = .y) %>%
                st_drop_geometry %>%
                select(area_m2, lake_id, zone)) %>%
  bind_rows(.) 

# calculate total area of reservoir         
zone_area <- zones %>% 
  group_by(lake_id) %>%
  summarize(area_m2 = sum(area_m2))

# propotion of total area in each zone
zones <- zones %>%
  mutate(prop = case_when(lake_id == "69" ~ (area_m2 / zone_area %>% filter(lake_id == "69") %>% pull(area_m2)),
                          lake_id == "70" ~ (area_m2 / zone_area %>% filter(lake_id == "70") %>% pull(area_m2)),
                          TRUE ~ 999999999999)
         ) %>%
  select(-area_m2)

