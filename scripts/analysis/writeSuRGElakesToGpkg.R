
#### READ IN SuRGE LAKE POLYGONS FIRST


# 1. create a list file paths where the surge lake polygons are stored.  
labs <- c("ADA", "CIN", "DOE", "NAR", "R10", "RTP", "USGS", "PR")
paths <- paste0(userPath,  "lakeDsn/", labs)

# 2. # list of .gdb to ignore
omit_gdb <- paste(c("2013 Lake Tschida Heart Butte Reservoir Sedimentation Survey.gdb",
                    "069nhd.gdb", "randomPoints.gdb", "randomPoints.gdb", "annotation.gdb",
                    "mercxxx.gdb", "WGSxxx.gdb", "mercharsha.gdb", "homeMerc.gdb",
                    "homeWGS.gdb", "nasheville.gdb", "mercxxx.gdb", "path1.gdb",
                    "merc318.gdb", # not sampled, dry 
                    "lowMerc287.gdb", # low points not used
                    "highMerc265.gdb", "originalMerc265.gdb", # low point set used
                    "highMerc249", "lowMerc249", "originalMerc249", # used mid point set
                    "merc188", # used high points
                    "merc326low"), # used high points
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

# 3. READ LAKE POLYGONS, FORMAT, WRITE TO DISK
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
 
return(surge_lakes)
}

# get SuRGE lakes (2018, 2020-2023)
surge_lakes <- get_surge(paths)

#######################################################
###### GET 2016 LAKES  ################################
#######################################################

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
           st_transform(., 3857) %>%  # consistent with surge_lakes
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
  

#######################################################
###### MERGE 2016 AND SURGE LAKES  ####################
#######################################################
# WRITE POLYGONS TO SINGLE .gpkg
bind_rows(list(surge_lakes, lakes_2016)) %>% # merge polygons
  st_write(., file.path( "../../../lakeDsn", "all_lakes.gpkg"), # write to .gpkg
           append = FALSE)
