# SOLAR IRRADIANCE DATA

# 1. Write coordinates, start, and stop time to clipboard.
# 2. Paste data into query window at web, download .csv
# 3. Read and format downloaded data
# 4. Merge data with SuRGE identifiers (lake_id....)

# 1. GET SOLAR IRRADIANCE DATA----
# Code below used to generate data that was input to 
# open-meteo.com to extract `shortwave radiation sum` data.
# The website can only handle 100 requests at at time,
# so had to slice into 100 row increments, paste into
# website, then download data to: output/solarIrradianceData/
# Data downloaded on 2/6/2025

site_descriptors_data %>%
  as_tibble %>%
  # subset to not overwhelm API. 100 works, 150 is too big
  # iteratively write 100 observations to clipboard, paste
  # into website, download data, repeat.
  slice(2201:2300) %>% 
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

# 2. READ AND FORMAT SOLAR IRRADIANCE DATA----
# Read solar irradiance data pulled from open_meteo.com
solar <- fs::dir_ls(path = "output/solarIrradianceData") %>%
  .[!grepl("shortwave_radiation.csv", .)] %>% # exclude output file (see final line of code)
  # read data
  map(
    readr::read_csv, 
    id = "filename", # add column containing source file name for troubleshooting
    guess_max = 10000 # guess correct column types
  ) %>% 
  map(
    ., # data is list of dataframes
    # id and irradiance data are stored in spearate sections of csv
    # split each data type into separate list element
    # create splitting group
    . %>% 
      mutate(
        group = case_when(is.na(timezone_abbreviation) ~ "solar", # solar data section
                                   TRUE ~ "id") # id data section
        ) %>%
      # split into two list elements
      group_by(group) %>%
      group_split() %>%
      # mutate list element 1 which contains unique identifiers
      map_at(
        ., # list of 2 elements as input (solar and id elements)
        .at = 1, # operate on first element (id element)
        function(x){
          x %>%
            # convert lat and lon to numeric
            mutate(across(c(latitude, longitude), as.numeric))
        }
      ) %>%
      # clean list element 2 which contains irradiance data
      map_at(
        ., # list of 2 elements as input (solar and id elements)
        .at = 2, # operate on 2nd element (solar element)
        function(x) {
          x %>%
            select(location_id, latitude, longitude) %>%
            janitor::row_to_names(., row_number = 1) %>%
            janitor::clean_names(.) %>%
            mutate(
              shortwave_radiation_sum_mj_m2 = as.numeric(shortwave_radiation_sum_mj_m2),
              time = as.Date(time) # convert to date class
              ) %>%
            # mean radiation, start date, and end date by location_id
            group_by(location_id) %>%
            summarise(
              shortwave_radiation_sum_mj_m2 = mean(shortwave_radiation_sum_mj_m2),
              # time column contains all dates between sample_start and sample_stop
              #
              sample_start = min(time),
              sample_end = max(time))
        }
      ) %>% # close map_at
      # merge 2 list elements
      Reduce(merge, .) %>%
      # clean df
      select(filename, sample_start, sample_end, shortwave_radiation_sum_mj_m2,
             # enforce SuRGE conventions
             lat = latitude, long = longitude)
  ) %>%
  # the above outputs a list with one element for each of the original
  # data files. Merge them together here.
  bind_rows %>%
  as_tibble

# Next we need to merge identifiers (lake_id, site_id, visit)
# with the solar data.
nrow(solar) # 2367 rows
nrow(site_descriptors_data) # 2367

# OK, this merge is complicated because the lat lon returned by open-meteo.com do
# not match the coordinates of the site. I assume it returns coordinates
# of grid cell mid point, or something like that. 


# Make map of 1) SuRGE site coordinates and 2) coordinates of downloaded solar data 
# Yeah, many fewer distinct lat/long in solar object than site_descriptors
tmap_mode("view") # specify interactive map

# bind SuRGE and solar coordinates
bind_rows(
  # SuRGE coordinates
site_descriptors_data %>%
  as_tibble %>%
  select(lat, long) %>%
  mutate(
    across(c(lat, long), 
           as.numeric),
    source = "descriptors"
  ),
# solar coordinates
solar %>%
  select(lat, long) %>%
  mutate(source = "solar")
) %>%
  # convert to spatial
  st_as_sf(., coords = c("long", "lat"), crs = 4326) %>%
  # make map
  tm_shape(.) +
  tm_dots(col = "source",
          palette = "tol.rainbow" # Choose a color palette
          ) +
  tm_basemap(server = "OpenStreetMap")


# Can we match SuRGE sites to corresponding solar points by how close the 
# points are, plus sample_start date?

# create unique identifier for each lat/long in solar object
# A single solar data coordinate is repeated for all SuRGE sites where
# that are closest to that solar coordinate than any other solar coordinate
solar <- solar %>%
  group_by(lat, long) %>%
  # cur_group_id() is fancy function for assigning unique id
  # to each unique combination of current grouping variable.
  mutate(solar_location_id = cur_group_id()) %>%
  ungroup

# sf object containing the unique points in the solar dataset
solar_points <- solar %>%
  select(lat, long, solar_location_id) %>%
  distinct() %>%
  st_as_sf(., coords = c("long", "lat"), crs = 4326)

# sf object containing the SuRGE coordinates
descriptors <- site_descriptors_data %>%
  as_tibble %>%
  select(lake_id, site_id, visit, sample_start, sample_end, lat, long) %>%
  st_as_sf(., coords = c("long", "lat"), crs = 4326)


# Get the row index from solar that is closest to each site location in descriptors
nearest_indices <- st_nearest_feature(descriptors, solar_points)

# Now append SuRGE sites with corresponding solar station
descriptors_with_solar_location_id <- bind_cols(
  # spatial object with SuRGE identifiers
  descriptors %>%
    st_drop_geometry(), 
  
  # solar_points object arranged to match order of descriptors object
  solar_points %>%
    st_drop_geometry(.) %>%
    .[nearest_indices, "solar_location_id"]
)

# OK, now we can join solar data with descriptors data that has been
# amended with solar_location_id from solar data.
# Shit, not working because of dups in solar. These dups are the result of
# `st_nearest_feature` creating associations between SuRGE sites and
# solar irradiance sites that don't agree with the associations used
# by open-meteo.com. These mismatches only occur among sites in the same
# lakes. Mismatch is minor, just aggregate across them.
solar_data <- left_join(
  # prep site_descriptors_data for join
  descriptors_with_solar_location_id,
  solar %>% 
    select(-filename) %>%
    distinct %>%
    group_by(sample_start, sample_end, lat, long, solar_location_id) %>%
    # see note immediately above for whey this aggregation is required
    summarise(
      shortwave_radiation_sum_mj_m2 = mean(shortwave_radiation_sum_mj_m2)
    )
)

# correlation with latitude? Nope.
ggplot(solar_data, aes(lat, shortwave_radiation_sum_mj_m2)) +
  geom_point()

# # clean up and write file for use in surge_stats repo
# solar_data %>%
#   select(lake_id, site_id, visit, shortwave_radiation_sum_mj_m2) %>%
#   write_csv(., "output/solarIrradianceData/shortwave_radiation.csv")

