# THIS SCRIPT COLLECTS LAKE POLYGONS AND SAMPLING POINTS
# FOR USE IN lakeMorpho AND JEREMY SCHROEDER'S WORK
# ON GRIDDED DATA


# COLLECT LAKE POLYGONS------------------------

## SuRGE and R10 2018 polygons--------
# 1. CREATE A LIST FILE PATHS WHERE THE SURGE LAKE POLYGONS ARE STORED.  
labs <- c("ADA", "CIN", "DOE", "NAR", "R10", "RTP", "USGS", "PR")
paths <- paste0(userPath,  "lakeDsn/", labs)
# paths <- paste0("C:/Users/JBEAULIE/Environmental Protection Agency (EPA)/SuRGE Survey of Reservoir Greenhouse gas Emissions - Documents/lakeDsn/", labs)

# 2. LIST OF .gdb TO READ
gdb_list <- lake.list %>%
  filter(eval_status_code == "S") %>%
  select(lake_id) %>%
  mutate(lake_id = as.character(lake_id),
         lake_id = case_when(lake_id =="326" ~ "merc326high",
                             lake_id =="287" ~ "originalMerc287",
                             lake_id =="265" ~ "lowMerc265",
                             lake_id =="249" ~ "midMerc249",
                             lake_id == "326" ~ "merc326high",
                             lake_id == "68" ~ "merc068", # need to exclude /CH4-068/2013 Lake Tschida Heart Butte Reservoir Sedimentation Survey.gdb"
                             lake_id == "13" ~ "merc013", # need to remove "13" so we don't grab "2013 Lake Tschida Heart Butte Reservoir Sedimentation Survey.gdb"
                             lake_id == "69" ~ NA_character_, # .shp of entire lake read below
                             lake_id == "70" ~ NA_character_, # .shp of entire lake read below
                             nchar(lake_id) == 1 ~ paste0("00", lake_id),
                             nchar(lake_id) == 2 ~ paste0("0", lake_id),
                             TRUE ~ lake_id)) %>%
  filter(!is.na(lake_id)) %>% #69 and 70 set to NA (see above)
  distinct # 4 revisits 
  

nrow(gdb_list) # 112 observations, lake.list includes 118, but this includes 4 revisits and two NA (69 and 70), remove those to get 112

# collapse to vector
gdb_list <- gdb_list %>%
  pull %>% paste(., collapse = "|")



# 2018 R10 and 2020-2023 SuRGE data are formatted similarly
get_surge <- function(paths){
# 1. GET LIST OF .gdb FOR 2018 SURGE LAKES
# Final lake polygons were written to a .gdb.  In most cases, they are also
# available as shapefile (e.g. eqArea003.shp).  I chose to work with .gdb here.

# Read .gdb names
fs_paths <- fs::dir_ls(path = paths, # see above
           regexp = '.gdb', # file names containing this pattern
           recurse = TRUE, # look in all subdirectories
           type = "file") %>% # only retain file names, not directory names   
  sub('\\/[^\\/]*$', '',.) %>% # extract characters before final /
  .[!grepl("bathymetry", .)] %>% # omit bathymetry files
  unique(.) %>% # names of .gdb e.g. merc297.gdb
  .[grepl(gdb_list, .)] # extract only desired .gdb

# 2. GET NAME OF LAKE POLYGON LAYER IN EACH .gdb
layers <- purrr::map(fs_paths, ~st_layers(.)) %>% # Read layers in each .gdb
  # each element contains a bunch of layers. 
  # Omit layers containing point, buffers, or depth contours
  map(., function(x){ # apply to each list element
    x$name[!grepl("site|buffer|contour", # exclude layer names containing ...
                  x$name, # names of all layers in each .gdb in list
                  ignore.case = TRUE)]
  }
  )

# 3. READ AND FORMAT LAKE POLYGONS
# with map2, the first vector/list will be supplied as the first argument to the 
# named function and the second vector/list will be supplied as the second argument.
# here dsn = fs_paths, and layer = layers.
surge_lakes <- map2(fs_paths, layers,  st_read, stringsAsFactors = FALSE) %>% # read lake polygon layer
  #.[68] %>% # code development
  # format lake_name and lake_id
  map(~rename(., lake_id = lakeSiteID) %>% 
        rename(lake_name = lakeName) %>% 
        mutate(lake_id = gsub("ch4-", "", lake_id, ignore.case = TRUE) %>% # remove "ch4-"
                 str_remove("^0+") %>% # remove leading zeroes
                 as.numeric) |> # no lacustrine, etc
        st_set_geometry("geom")) %>% # make sure geometry column name is geom (it is Shape, geometry, .... across objects)
  
  # dissolve sections and strata (e.g., trib, open_water), but keep
  # lakeName and lakeSiteID attributes.  st_union and st_combine will do the
  # dissolve bit, but drops attributes.  This slick dplyr code does the trick
  #https://github.com/r-spatial/sf/issues/290
  map(function(x) x %>% group_by(lake_name, lake_id) %>% 
        summarise() %>% # this does the dissolved
        ungroup() %>% # remove grouping
        st_make_valid()) %>% # clean up objects

map(~if(.$lake_id == "317") { #  .shp has two separate polygons, only one was sampled.  omit one not sampled
  st_cast(., "POLYGON") %>% # breaks single multipolygon into two separate polygons
    mutate(area_m = st_area(.)) %>% # calculate area of each polygon
    top_n(1, area_m) %>% # pick largest polygon
    select(-area_m) # no longer need area.m field
  } else { # if not lake 317
    . # just return shapefile
  }) %>%
  bind_rows() # collapse to one sf object
 

# Missouri river impoundments split into riverine, transitional, and lacustrine.
# Here we read in .shp for entire reservoir to be used for morpho and gridded
# data
missouri_river <- map(list(paste0(userPath, "lakeDsn/CIN/CH4-070/merc070dissolve.shp"),
                           paste0(userPath, "lakeDsn/CIN/CH4-069/merc069dissolve.shp")),
                      st_read) %>%
  bind_rows() %>% 
  mutate(lake_id = as.numeric(lake_id)) %>% # no lacustrine, etc
  rename(geom = geometry)


return(bind_rows(surge_lakes, missouri_river))
} # close get_surge function

# get SuRGE lakes (2018, 2020-2023)
surge_lakes <- get_surge(paths)
# warning OK:  Warning message:
# In st_cast.sf(., "POLYGON") :
#   repeating attributes for all sub-geometries for which they may not be constant

## 2016 AND FALLS LAKE POLYGONS------------------------

# 1. file paths where the 2016 lake polygons are stored.  
paths <- paste0(userPath,  "lakeDsn/", "2016_survey")

get_2016 <- function(paths){
  
  # 1. READ IN FILE NAMES FOR 2016 RESERVOIR SURVEY AND FALLS LAKE POLYGONS
  #d <- 
  fs::dir_ls(path = paths, 
             regexp = '..shp', # file names containing this pattern
             recurse = 1, # one level into subdirectories (avoid bathymetry files)
             type = "file") %>% # only retain file names, not directory names  
    .[!(grepl("xml|lock|basin", .))] %>% # omit .shp, .xml, .lock, and basin shapefiles
    # I couldn't dissolve intra-reservoir boundaries for these lakes in R. dissolved in Pro. Ignore original .shp
    # and read in dissolved polygons. Specified lake name below.
    .[!(grepl("fallsLakeSitesEqArea|fallsLakeEqArea.shp|miltonEqArea.shp|senecavilleEqArea.shp|caesarCreekEqArea.shp", .))] %>%
    .[!grepl("brookville_bb.shp", .)] %>% # omit brookeville bounding box used for data paper figure
    #.[15] %>% # subset for code development
    #imap: .x is object piped into imap, .y is object index (name of list element)
    imap(~st_read(.x, stringsAsFactors = FALSE) %>% # read shapefiles
           #st_cast(., "POLYGON") %>%
           st_make_valid() %>% # fix any spatial issues
           st_transform(., 3857) |>  # web meractor, consistent with surge_lakes
           st_set_geometry("geom") %>% # make sure geometry column name is geom (it is Shape, geometry, .... across objects)
           
           # Each .shp must have a lake_name attribute.  All but 5 do.  Below
           # we address the 5 that need the attribute
           mutate( # first mutate creates a lake_name attribute if not already present
             # and populates it with the list element name (.y).  I tried 
             # if_else and case_when, but they broke when encountering list elements
             # that didn't contain the lake_name attribute.  This if else combo works.
             lake_name = if(!any("Lake_Name" %in% names(.))) {.y} else {unique(Lake_Name)},
             # second mutate replaces the lake_name, taken from .y, with the appropriate lake_name
             # for the 5 lakes.
             lake_name = case_when(grepl("brookville", lake_name, ignore.case = TRUE) ~ "Brookville Lake",
                                   grepl("buckhorn", lake_name, ignore.case = TRUE) ~ "Buckhorn Lake",
                                   grepl("carr", lake_name, ignore.case = TRUE) ~ "Carr Fork Lake",
                                   grepl("cave", lake_name, ignore.case = TRUE) ~ "Cave Run Lake",
                                   grepl("falls", lake_name, ignore.case = TRUE) ~ "Falls Lake",
                                   grepl("senecaville", lake_name, ignore.case = TRUE) ~ "Senecaville Lake",
                                   grepl("milton", lake_name, ignore.case = TRUE) ~ "Lake Milton",
                                   grepl("caesar", lake_name, ignore.case = TRUE) ~ "Caesar Creek Lake",
                                   TRUE ~ lake_name))) %>%
    # This dissolves intra reservoir strata and/or sections
    map(function(x) x %>% group_by(lake_name) %>%
          summarise() %>% # this does the dissolve
          ungroup() %>% # remove grouping
          st_make_valid()) %>% # clean up objects
    #this bit assigns lake_id values (e.g. 1000)
    #d[1] %>%
    map(function(x) {
      x.lake_name <- x$lake_name # get the Lake_Name
      # find corresponding lakeSiteID from lake.list.2016 (see readSurgeLakes.R)
      x.lake_id <- lake.list.2016[lake.list.2016$eval_status_code_comment %in% x.lake_name, "lake_id"] %>% pull
      x %>% mutate(lake_id = x.lake_id)
    }) %>%
    bind_rows() # collapse to one sf object with multiple polygons
}

lakes_2016 <- get_2016(paths)
dim(lakes_2016) # 33 lakes (32 from 2016 + Falls Lake)


# GET POINTS-----------------
## SuRGE sites---------
dat_surge_sf <- fld_sheet %>%
  filter(eval_status == "TS", # only sampled sites
         !(is.na(long)|is.na(lat))) %>% # only sites where lat and long were recorded
  
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
  st_as_sf(., coords = c("long", "lat")) %>%
  `st_crs<-` (4326) %>% # latitude and longitude
  st_transform(., 3857) %>% # web meractor, consistent with surge_lakes
  st_set_geometry("geom") %>% # ensure consistent geometry column names across all sf objects
  select(lake_id, site_id, visit)



dim(dat_surge_sf) #1869



## 2016 data----------
# dat_2016 loaded via read2016data.R --> estimateDepth2016.R
dat_2016

# Format and coerce to spatial object
# data in correct UTC time zone. See issue 134
dat_2016_sf <- dat_2016 %>%
  filter(!is.na(lat), !is.na(long)) %>%
  st_as_sf(., coords = c("long","lat"), crs = "EPSG:4326") %>% # lat/long
  st_transform(., 3857) %>% # web meractor, consistent with surge_lakes
  st_set_geometry("geom") %>%  # ensure consistent geometry column names across all sf objects
  # exclude sites with no trap or chamber deployment
  filter(
    !is.na(trap_deply_date_time)|!is.na(trap_rtrvl_date_time)|!is.na(chamb_deply_date_time)
    ) %>% 
  select(lake_id, site_id, visit) %>%
 relocate(lake_id, site_id) %>%
  mutate(site_id = as.character(site_id))

dim(dat_2016_sf) #498


## Falls lake-----------
# data maintained at: "C:\Users\JBEAULIE\OneDrive - Environmental Protection Agency (EPA)\gitRepository\fallsLakeCH4"
# 1. create sf object for sites 991 and 992 that were sampled one. These sites
#    are not included in official survey design.
# 2. merge above with official survey design sf object
# 3. merge sf object above with deployment and retrieval times

# sf object of sites
dat_falls_lake_sf <- rbind(
  # sf object for sites 991 and 992. these were sampled once but not in survey design file (?)
  tribble(
    ~lat, ~lon, ~site_id,
    36.07039, -78.79043, 991,
    36.07016, -78.78858, 992) %>%
    st_as_sf(coords = c("lon", "lat"), crs = 4326) %>%
    st_set_geometry("geom") %>%  # ensure consistent geometry column names across all sf objects
    st_transform(3857), # web meractor, consistent with surge_lakes
  
  # read in and merge .shp containing survey design
  st_read(paste0(userPath,  "lakeDsn/2016_survey/fallsLake/FallsLakeSitesEqArea.shp")) %>%
    st_transform(., 3857) %>% # web mercator, consistent with surge_lakes
    st_set_geometry("geom") %>%  # ensure consistent geometry column names across all sf objects
    select(siteID) %>%
    rename(site_id = siteID) %>%
    mutate(site_id = substr(site_id, 4,5) %>% as.numeric)
) %>%
  # join with visit number from fallsLakeCH4 RStudio project (readFieldSheets.R)
  #  RStudio project (readFieldSheets.R)
  right_join(
    readRDS(paste0(userPath, "data/RTP/CH4_1033_Falls_Lake/falls_lake_fld_sheet.rds")) %>%
      select(lake_id, site_id, visit) 
  ) %>%
  st_make_valid() %>%
  mutate(site_id = as.character(site_id),
         lake_id = as.numeric(lake_id))


# WRITE POLYGONS AND POINTS TO DISK-----------
## POLYGONS----
# general geopackage for collaborators, including Falls Lakes for Jeremy
bind_rows(list(surge_lakes, lakes_2016)) %>% # merge polygons
  st_make_valid() %>%
  st_write(., file.path(userPath, "/lakeDsn", paste0("all_lakes_", Sys.Date(), ".gpkg")), # write to .gpkg
           layer = "all_lakes",
           append = FALSE)

# geopackage for data paper
bind_rows( # merge polygons
  list(
    surge_lakes, 
    lakes_2016 %>% filter(lake_id != 1033) # exclude Falls Lake 
    ) # close list
  ) %>% # close bind_rows
  select(-lake_name) %>%
  st_make_valid() %>%
  st_write(., file.path( 
    "communications/manuscript/data_paper/", 
    "2_lake_polygons.gpkg"), # write to .gpkg
    layer = "lake_polygons",
    append = FALSE)

dim(surge_lakes) #114
dim(lakes_2016 %>% filter(lake_id != 1033)) #32
32+114 #146

## POINTS----
# merge 2016, SuRGE, and Falls Lake data
# general geopackage for collaborators, add to all_lakes.gpkg
# see scripts/analysis/spatialDataForJeremy.R for addition of deply/retr
# date_times
bind_rows(
  list(
    dat_2016_sf, # 2016 points
    dat_surge_sf, # surge points
    dat_falls_lake_sf # Falls Lake points
    )
  ) %>% # merge points
  st_write(., file.path(userPath, "/lakeDsn", paste0("all_lakes_", Sys.Date(), ".gpkg")), # write to .gpkg
           layer = "points",
           append = FALSE)

# geopackage for data paper
bind_rows(list(dat_2016_sf, dat_surge_sf)) %>% # merge points
  st_write(., file.path(
    "communications/manuscript/data_paper/", 
    "1_sample_points.gpkg"), # write to .gpkg
    layer = "sample_points",
    append = FALSE)

bind_rows(list(dat_2016_sf, dat_surge_sf)) %>% dim #2367 observations
