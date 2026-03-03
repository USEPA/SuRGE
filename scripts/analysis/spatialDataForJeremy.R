# Merge chamber deployment times, trap retrieval times, and trap deployment
# times with point spatial data for Jeremy. This will be used to grab
# met data from ERA5.


# 1. COMPILE TRAP DEPLOYMENT RETRIEVAL DATA-----
trap_date_time <- bind_rows(
  # 2016 and SuRGE data
  dat %>% 
  select(lake_id, site_id, visit, trap_deply_date_time, trap_rtrvl_date_time) %>%
    mutate(trap_deply_date_time_units = "UTC",
           trap_rtrvl_date_time_units = "UTC"),
  
  # Falls Lake 
  # times were defined based on local time zone, then converted to UTC in 
  # fallsLakeCH4 RStudio project (readFieldSheets.R)
  readRDS(paste0(userPath, "data/RTP/CH4_1033_Falls_Lake/falls_lake_fld_sheet.rds")) %>%
    select(lake_id, site_id, visit, trap_deply_date_time, trap_rtrvl_date_time) %>%
    mutate(lake_id = as.numeric(lake_id),
           site_id = as.character(site_id),
           trap_deply_date_time_units = "UTC",
           trap_rtrvl_date_time_units = "UTC")
) %>%
  as_tibble

# # check distribution of time stamps
# trap_date_time %>%
#   ggplot(aes(lake_id, trap_deply_date_time)) +
#   geom_point()

# 2. COMPILE CHAMBER DEPLOYMENT DATA----
chamb_date_time <- bind_rows(
  
  # Falls Lake 
  # times were defined based on local time zone, then converted to UTC in 
  # fallsLakeCH4 RStudio project (readFieldSheets.R)
  readRDS(paste0(userPath, "data/RTP/CH4_1033_Falls_Lake/falls_lake_fld_sheet.rds")) %>%
    select(lake_id, site_id, visit, chamb_deply_date_time) %>%
    mutate(lake_id = as.numeric(lake_id),
           site_id = as.character(site_id),
           chamb_deply_date_time_units = "UTC"),
  
  # SuRGE
  gga_3 %>%
    # deal with lacustrine etc from Missouri river
    mutate( # move transitional, lacustrine, riverine from lake_id to site_id
      site_id = case_when(grepl("lacustrine", lake_id) ~ paste0(site_id, "_lacustrine"),
                          grepl("transitional", lake_id) ~ paste0(site_id, "_transitional"),
                          grepl("riverine", lake_id) ~ paste0(site_id, "_riverine"),
                          TRUE ~ as.character(site_id)),
      # remove transitional, lacustrine, riverine from lake_id
      # retain character class initially, then convert to numeric.
      lake_id = case_when(lake_id %in% c("69_lacustrine", "69_riverine", "69_transitional") ~ "69",
                          lake_id %in% c("70_lacustrine", "70_riverine", "70_transitional") ~ "70",
                          TRUE ~ lake_id),
      lake_id = as.numeric(lake_id)) %>%
    select(lake_id, site_id, visit, ch4DeplyDtTm) %>%
    rename(chamb_deply_date_time = ch4DeplyDtTm) %>% # we are focusing on CH4
    # time zones arbitrarily defined as UTC in readLgr.R, but are eastern for all 
    # lakes except Region 10 where LGR clock was set to Pacific. Here we 1) split
    # the R10 data into one list element and all other data into another, 2) redefine
    # time zone as Pacific or Eastern, 3) recast as UTC, 4) recombine data.
    mutate(tz = case_when(lake_id %in% c(238, 239, 249, 253, 263, 265, 287, 302,
                                         308, 323, 331, 999) ~ "America/Los_Angeles", # all R10 are Pacific
                          TRUE ~ "America/New_York")) %>% # all others eastern
    group_split(tz) %>% # split by time zone
    # R can't support different time zones in one column. split eastern and pacific
    # into separate list elements, define local time zone, cast to UTC, then join
    # back to df
    map_dfr(~.x %>% mutate(
      # enforce time zone used in LGR, then cast to UTC
      chamb_deply_date_time = case_when(tz == "America/Los_Angeles" ~ 
                                          force_tz(chamb_deply_date_time, "America/Los_Angeles") %>%
                                          with_tz(., tzone = "UTC"),
                                        tz == "America/New_York" ~ 
                                          force_tz(chamb_deply_date_time, "America/New_York") %>%
                                          with_tz(., tzone = "UTC"),
                                        # error code
                                        TRUE ~ as.POSIXct("1900-01-01 01:30:00", "%Y-%m-%d %H:%M:%S", tz = "UTC")))
    ) %>%
    select(-tz) %>% # no longer need tz field
    # time series repeated for each deployment. Filter down to unique values for each site.
    distinct %>%
    mutate(chamb_deply_date_time_units = "UTC"),
  
  # 2016
  dat_2016 %>%
    select(lake_id, site_id, visit, chamb_deply_date_time) %>%
    mutate(lake_id = as.numeric(lake_id), site_id = as.character(site_id),
           chamb_deply_date_time_units = "UTC")
)

# check error code
# none, good
chamb_date_time %>% 
  filter(
    chamb_deply_date_time == as.POSIXct("1900-01-01 01:30:00", "%Y-%m-%d %H:%M:%S", tz = "UTC")
    )

dim(trap_date_time) # 2816
dim(chamb_date_time) # 2783

# # check distribution of time stamps
# chamb_date_time %>%
#   ggplot(aes(lake_id, chamb_deply_date_time)) +
#   geom_point()

# 3. WRITE NEW GEOPACKAGE WITH POINT AND POLYGON DATA FOR JEREMY----
# bind trap/chamber date_time data with point spatial point and write to .gpkg
full_join(
  # get spatial data from existing .gpkg rather than reading shapefiles again
  # this file includes Falls Lake
  # see writeSuRGElakesToGPKG.R
  st_read(
  file.path(userPath, "/lakeDsn/all_lakes_2026-03-02.gpkg"), # update as needed
  layer = "points"
  ),
  
  # Add chamber deployment times
  chamb_date_time
) %>%
  full_join(
    # add trap deployment times
    trap_date_time
  ) %>%
  # convert UTC time to character to prevent any shananigans!
  mutate(
    across(
      contains("date_time") & !contains("units"), \(x) format(x,"%Y-%m-%d %H:%M:%S") 
    ) # close across
  ) %>% # close mutate
  st_write(., 
           file.path(userPath, "/data/siteDescriptors/RTP_gridded_data/",
                     paste0("all_lakes_for_met_", Sys.Date(), ".gpkg")), 
           layer = "points",
           append = FALSE)

# 4. ADD LAKE POLYGONS TO GEOPACKAGE----
# lake polygons are already compiled in writeSuRGElakesToGPKG.R. Adding
# to this new .gpkg for convenience.

st_read(
  file.path(userPath, "/lakeDsn/all_lakes_2026-03-02.gpkg"), # update as needed
  layer = "all_lakes"
) %>%
  st_write(., 
           file.path(userPath, "/data/siteDescriptors/RTP_gridded_data/",
                     paste0("all_lakes_for_met_", Sys.Date(), ".gpkg")), 
           layer = "polygons",
           append = FALSE)


# 5. INSPECT FILES FOR ISSUES----
new_met_points <- st_read( 
  file.path(userPath, "/data/siteDescriptors/RTP_gridded_data/",
            "all_lakes_for_met_2026-03-03.gpkg"), 
  layer = "points"
)

dim(new_met_points) #2816 points

# no dups
new_met_points %>%
  st_drop_geometry %>%
  select(lake_id, site_id, visit) %>%
  janitor::get_dupes()

# lake_id numeric, site_id character, visit numeric
# date_time character. good
map(new_met_points, class)

# 6. COMPARE NEW AND OLD MET FILES----
# Jeremy's original met data query was based on time stamps in: 
old_met_points <- st_read( 
  file.path(userPath, "/lakeDsn/all_lakes_2025-04-24.gpkg"), # update as needed
  layer = "points"
) %>%
  # st_read will assign local time zone to posixct, so specify UTC,
  # then convert to character for comparison to new file
  mutate(across(contains("date_time"), \(x) with_tz(x, tzone = "UTC")),
         across(contains("date_time"), \(x) format(x, "%Y-%m-%d %H:%M:%S")))

dim(old_met_points) # 3044
dim(new_met_points) # 2816

# why more points in old than new?
# what points in new are missing from old?
# hmm, all are in old?
anti_join(
  new_met_points %>%
    st_drop_geometry %>%
    select(lake_id, site_id, visit),
  old_met_points %>%
    st_drop_geometry %>%
    select(lake_id, site_id, visit)
)


# dups in old file?
# yes!
old_met_points %>%
  st_drop_geometry %>%
  janitor::get_dupes(lake_id, site_id, visit) 


# what date_time values changed? Should only be trap, not chamber
check_time_change <- inner_join(
  new_met_points %>% st_drop_geometry %>% select(-contains("units")),
  old_met_points %>% st_drop_geometry %>% select(-contains("units"), -site_wgt, -site_depth),
  by = join_by(lake_id, site_id, visit),
  suffix = c("_new", "_old"))  %>%
  as_tibble %>%
  select(lake_id, site_id, visit, 
          chamb_deply_date_time_old, chamb_deply_date_time_new,
          trap_deply_date_time_old, trap_deply_date_time_new,
          trap_rtrvl_date_time_old, trap_rtrvl_date_time_new,
         everything()) %>%
  mutate(
    same_chamb = case_when(
      chamb_deply_date_time_old == chamb_deply_date_time_new ~ "same",
      TRUE ~ "updated"
    ),
    same_trap_rtrvl = case_when(
      trap_rtrvl_date_time_old == trap_rtrvl_date_time_new ~ "same",
      TRUE ~ "updated"
    ),
    same_trap_deply = case_when(
      trap_deply_date_time_old == trap_deply_date_time_new ~ "same",
      TRUE ~ "updated"
    )
  ) 

# any chamber time stamps change?
# only picking up NA, so no change, good
check_time_change %>%
  filter(same_chamb == "updated") %>%
  select(lake_id, site_id, visit, contains("chamb"), -contains("units")) %>%
  print(n=Inf)

# any trap deply time stamps change?
# only picking 9 changes, these are the forced time zone observations
# at Missouri River where cell phone time zone was uncertain due to tz 
# boundary running down center of river.
check_time_change %>%
  filter(same_trap_deply == "updated") %>%
  select(lake_id, site_id, visit, contains("trap_deply"), -contains("units")) %>%
  print(n=Inf)


# any trap retrieval time stamps change?
# only picking up the same 9 changes observed above. EXPECTED MORE CHANGES.
check_time_change %>%
  filter(same_trap_rtrvl == "updated") %>%
  select(lake_id, site_id, visit, contains("trap_rtrvl"), -contains("units")) %>%
  print(n=Inf)



# manually inspect some trap deployment times from multiple times zones
# Atlantic, site 1000, standard -4, daylight -4
# eastern, 98, standard -5, daylight -4
# central, 191, standard -6, daylight -5
# mountain, 328, standard -7, daylight -6
# pacific, 297, standard - -8, daylight -7
# southwest, 275, standard -7, daylight -7



new_met_points %>%
  filter(lake_id %in% c(1000, 98, 191, 328,
                        297, 275)) %>%
  group_by(lake_id) %>%
  arrange(site_id) %>%
  slice_head %>%
  select(lake_id, site_id, visit, trap_deply_date_time, trap_rtrvl_date_time)

