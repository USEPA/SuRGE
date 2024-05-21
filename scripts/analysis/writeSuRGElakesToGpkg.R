# THIS SCRIPT COLLECTS LAKE POLYGONS AND SAMPLING POINTS
# FOR USE IN lakeMorpho AND JEREMY SCHROEDER'S WORK
# ON GRIDDED DATA


# COLLECT LAKE POLYGONS------------------------

## SuRGE, including R10 2018, polygons
# 1. CREATE A LIST FILE PATHS WHERE THE SURGE LAKE POLYGONS ARE STORED.  
labs <- c("ADA", "CIN", "DOE", "NAR", "R10", "RTP", "USGS", "PR")
paths <- paste0(userPath,  "lakeDsn/", labs)

# 2. LIST OF .gdb TO IGNORE
omit_gdb <- paste(c("2013 Lake Tschida Heart Butte Reservoir Sedimentation Survey.gdb",
                    "069nhd.gdb", "randomPoints.gdb", "randomPoints.gdb", "annotation.gdb",
                    "mercxxx.gdb", "WGSxxx.gdb", "mercharsha.gdb", "homeMerc.gdb",
                    "homeWGS.gdb", "nasheville.gdb", "mercxxx.gdb", "path1.gdb",
                    "merc318.gdb", # not sampled, dry 
                    "lowMerc287.gdb", # low points not used
                    "highMerc265.gdb", "originalMerc265.gdb", # low point set used
                    "highMerc249", "lowMerc249", "originalMerc249", # used mid point set
                    "merc188", # used high points
                    "merc326low", # used high points
                    "merc070lacustrine", # using polygon for entire reservoir
                    "merc070riverine",
                    "merc070transitional",
                    "merc069lacustrine", # using polygon for entire reservoir
                    "merc069riverine",
                    "merc069transitional"),                    
                  collapse = "|")

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
  unique(.) %>% # names of .gdb e.g. merc297.gdb
  .[!(grepl(omit_gdb, .))] # omit random .gdb not needed here


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
  # format lake_name and lake_id
  map(~rename(., lake_id = lakeSiteID) %>% 
        rename(lake_name = lakeName) %>% 
        mutate(lake_id = gsub("ch4-", "", lake_id, ignore.case = TRUE) %>% # remove "ch4-"
                 str_remove("^0+"))) %>% # remove leading zeroes
  # dissolve sections and strata (e.g., trib, open_water), but keep
  # lakeName and lakeSiteID attributes.  st_union and st_combine will do the
  # dissolve bit, but drops attributes.  This slick dplyr code does the trick
  #https://github.com/r-spatial/sf/issues/290
  map(function(x) x %>% group_by(lake_name, lake_id) %>% 
        summarise() %>% # this does the dissolved
        ungroup() %>% # remove grouping
        st_make_valid()) %>% # clean up objects

map(~if(.$lake_id == "317") { #  .shp has two separate polygons, only one was sampled.  omit one not sampled
  st_cast(., "POLYGON") %>% # breaks single multipolygon into two separate ploygons
    mutate(area_m = st_area(.)) %>% # calculate area of each polygon
    top_n(1, area_m) %>% # pick largest polygon
    select(-area_m) # no longer need area.m field
  } else { # if not lake 317
    . # just return shapefile
  }) %>%
  bind_rows() # collapse to one sf object
 

# Missouri rive impoundments split into riverine, transitional, and lacustrine.
# Here we read in .shp for entire reservoir to be used for  morpho and gridded
# data
missouri_river <- map(list(paste0(userPath, "lakeDsn/CIN/CH4-070/merc070dissolve.shp"),
                           paste0(userPath, "lakeDsn/CIN/CH4-069/merc069dissolve.shp")),
                      st_read) %>%
  bind_rows() %>% rename(geom = geometry)


return(bind_rows(surge_lakes, missouri_river))
}

# get SuRGE lakes (2018, 2020-2023)
surge_lakes <- get_surge(paths)
# warning OK:  Warning message:
# In st_cast.sf(., "POLYGON") :
#   repeating attributes for all sub-geometries for which they may not be constant

# 2016 AND FALLS LAKE POLYGONS

# 1. file paths where the 2016 lake polygons are stored.  
paths <- paste0(userPath,  "lakeDsn/", "2016_survey")

get_2016 <- function(paths){

  # 1. READ IN FILE NAMES FOR 2016 RESERVOIR SURVEY AND FALLS LAKE POLYGONS
  # d <- 
  fs::dir_ls(path = paths, 
             regexp = '..shp', # file names containing this pattern
             recurse = TRUE, # look in all subdirectories
             type = "file") %>% # only retain file names, not directory names  
    .[!(grepl("xml|lock", .))] %>% # omit .shp.xml and .lock files
    #.[8] %>% # subset for code development
    #imap: .x is object piped into impap, .y is object index (name of list element)
    imap(~st_read(.x, stringsAsFactors = FALSE) %>% # read shapefiles
           st_transform(., 3857) %>%  # web meractor, consistent with surge_lakes
           # Each .shp must have a lake_name attribute.  All but 5 do.  Below
           # we address the 5 that need the attribute
           mutate( # first mutate creates a lake_name attribute if not already present
             # and populates it with the list element name (.y).  I tried 
             # if_else and case_when, but they broke when encountering list elements
             # that didn't contain the lake_name attribute.  This if else combo works.
             lake_name = if(!any("Lake_Name" %in% names(.))) {.y} else {unique(Lake_Name)},
             # second mutate replaces the lake_name, taken from .y, with the appropriate lake_lame
             # for the 5 lakes.
             lake_name = case_when(grepl("brookville", lake_name, ignore.case = TRUE) ~ "Brookville Lake",
                                   grepl("buckhorn", lake_name, ignore.case = TRUE) ~ "Buckhorn Lake",
                                   grepl("carr", lake_name, ignore.case = TRUE) ~ "Carr Fork Lake",
                                   grepl("cave", lake_name, ignore.case = TRUE) ~ "Cave Run Lake",
                                   grepl("falls", lake_name, ignore.case = TRUE) ~ "Falls Lake",
                                   TRUE ~ lake_name))) %>%
    # This dissolves intra reservoir strata and/or sections
    map(function(x) x %>% group_by(lake_name) %>% 
          summarise() %>% # this does the dissolve
          ungroup() %>% # remove grouping
          st_make_valid()) %>% # clean up objects
    # this bit assigns lake_id values (e.g. 1000)
    map(function(x) {
      x.lake_name <- x$lake_name # get the Lake_Name
      # find corresponding lakeSiteID from lake.list.2016 (see readSurgeLakes.R)
      x.lake_id <- lake.list.2016[lake.list.2016$eval_status_code_comment %in% x.lake_name, "lake_id"]
      x %>% mutate(lake_id = gsub("CH4-", "", x.lake_id))
    }) %>%
    bind_rows() # collapse to one sf object with multiple polygons
}

lakes_2016 <- get_2016(paths)
  

# GET POINTS AND TRAP DEPOLYMENT/RETRIEVAL TIMES-----------------
## SuRGE sites
dat.surge.sf <- fld_sheet %>%
  filter(eval_status == "TS", # only sampled sites
         !(is.na(long)|is.na(lat)), # only sites where lat and long were recorded
         !(is.na(trap_deply_date_time)|is.na(trap_rtrvl_date_time))) %>% # only rows with trap data
  st_as_sf(., coords = c("long", "lat")) %>%
  `st_crs<-` (4326) %>% # latitude and longitude
  st_transform(., 3857) %>% # web meractor, consistent with surge_lakes
  select(lake_id, site_id, (contains("trap") & contains("date_time")))

dim(dat.surge.sf) #1819


## 2016 data
# load 2016 data
load(paste0(userPath, "data/CIN/2016_survey/eqAreaData.RData")) # loads eqAreaData

dat.2016 <- eqAreaData # rename to dat.2016
remove(eqAreaData) # remove original object

# Format and coerce to spatial object

dat.2016.sf <- st_as_sf(dat.2016, coords = c("xcoord", "ycoord")) %>% 
  `st_crs<-`("ESRI:102008") %>% # original 2016 data in Conus Albers. (5070)
  st_transform(., 3857) %>% # web meractor, consistent with surge_lakes
  select(Lake_Name, siteID, (contains("trap") & contains("DtTm"))) %>%
  filter(!(is.na(trapDeplyDtTm)|is.na(trapRtrvDtTm))) %>% # exclude sites with no trap deployment
  rename(trap_deply_date_time = trapDeplyDtTm, trap_rtrvl_date_time = trapRtrvDtTm) %>% # change name to SuRGE convention
  # multiple deployments at Acton Lake have different names (e.g. Acton Aug).  Change them
  # all to Acton Lake
  mutate(Lake_Name = case_when(grepl("acton", Lake_Name, ignore.case = TRUE) ~ "Acton Lake",
                               TRUE ~ Lake_Name)) %>%
  left_join(lake.list.2016 %>% select(lake_id, eval_status_code_comment), 
            by = c("Lake_Name" = "eval_status_code_comment")) %>%
  select(-Lake_Name) %>% # lake_name no longer needed
  rename(site_id = siteID) %>%
  mutate(site_id = as.numeric(gsub(".*?([0-9]+).*", "\\1", site_id)), # format site_id
         lake_id = as.character(lake_id)) %>%
  relocate(lake_id, site_id) 

dim(dat.2016) #1531
dim(dat.2016.sf) #539, lots of rows with no trap data (e.g. oversample sites)


# WRITE POLYGONS AND POINTS TO DISK-----------
## POLYGONS
# bind_rows(list(surge_lakes, lakes_2016)) %>% # merge polygons
#   st_write(., file.path( "../../../lakeDsn", "all_lakes.gpkg"), # write to .gpkg
#            append = FALSE)

dim(surge_lakes) #123
dim(lakes_2016) #33
33+123 #156

## POINTS
# merge 2016 and SuRGE data
# [5/13/2024] missing Falls Lake
# add point to all_lakes.gpkg
# bind_rows(list(dat.2016.sf, dat.surge.sf)) %>% # merge polygons
#   st_write(., file.path( "../../../lakeDsn", "all_lakes.gpkg"), # write to .gpkg
#            layer = "points",
#            append = FALSE)

