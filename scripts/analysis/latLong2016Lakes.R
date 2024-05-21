# BRIDGET WANT A LAT LON FOR ALL LAKES IN THE 2016 SURVEY.  THESE WILL
# BE USED TO HELP HER IDENTIFY A NID RECORD

# Read in the lake shapefiles
lake.2016 <- fs::dir_ls(path = "../../../lakeDsn/2016_survey", regexp = ".shp", recurse = TRUE) %>%
  .[!grepl("xml", .)] %>% # exclude xml files
  purrr::map(read_sf) %>% # read .shp
  purrr::map(~st_transform(., 4326)) %>% # decimal degrees
  purrr::map(st_union) # dissolve polygon sections


# Plot a point on lake polygon.  st_centroid often generates points outside
# of polygon, wheras st_point_on_surface contains point within polygon.
# this is just a check to make sure the calculated point is reasonable.
for (i in 1:length(lake.2016)) {
plot(st_geometry(lake.2016[[i]])) # plot lake polygon
#plot(st_geometry(st_centroid(lake.2016[[i]])), add=TRUE)
plot(st_geometry(st_point_on_surface(lake.2016[[i]])), add=TRUE, col="red")  # add point
line <- readline(prompt="Press [enter] to continue") # pause to review plot, hit enter to proceed
}

# create table of lat/lon points
out <- 
  lake.2016 %>% # list of lake polygons
  purrr::map_df(st_point_on_surface) %>%  # convert to point on polygon
  map(~st_coordinates(.) %>% as.data.frame(.)) %>% # extract lat lon, convert to df
  imap(., ~mutate(.x, lake = .y)) %>% # add lake name.  .y is list element name
  map(~mutate(., lake = gsub(pattern = "../../../lakeDsn/2016_survey/", # clean up lake name
                             replacement = "", 
                             x = .$lake))) %>%
  dplyr::bind_rows(.) # collapse to df
  


write.csv(out, file = "output/2016_lake_point.csv")
