# PREP MINIMUM DATA FOR G-RES

str(morpho) # 147 lakes
unique(morpho$lake_id) # numeric

morpho %>%
  janitor::get_dupes(lake_id)


str(surge_lakes) # sf object, 114 lakes
unique(surge_lakes$lake_id) # numeric
str(lakes_2016) # sf object, 33 lakes
unique(lakes_2016$lake_id) # numeric

lakes_foo <- c(unique(surge_lakes$lake_id),
                   unique(lakes_2016$lake_id))

morpho %>% filter(!(lake_id %in% lakes_foo)) # all lakes with morpho estimates have a corresponding polygon

lakes_foo[!(lakes_foo %in% morpho$lake_id)] # all lake polygons have morpho values

## POLYGONS----
bind_rows(list(surge_lakes, lakes_2016)) %>% # merge polygons
  st_make_valid() %>%
  left_join(morpho) %>%
  st_write(., file.path( "../../../lakeDsn", paste0("all_lakes_gres_", Sys.Date(), ".gpkg")), # write to .gpkg
           layer = "all_lakes",
           append = FALSE)
