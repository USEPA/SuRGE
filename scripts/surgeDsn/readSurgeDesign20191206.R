# CREATE GEOPACKAGE SHOWING SITES YET TO BE SAMPLED, PLUS OVERSAMPLE SITE
# DOESN'T SHOW SITE WE CAN'T SAMPLE


# TONY OLSEN GENERATED SuRGE_design_20191206.shp as the master design file for SuRGE.
# I CONVERTED THE .dbf FILE TO EXCEL AND ADDED A VARIETY OF COLUMNS TO FACILITATE 
# EXECUTION OF THE DESIGN (i.e. evalStatus, lab, etc).  THIS SCRIPT IS USED TO READ IN
# THE UPDATED EXCEL FILE, FILTER OUT SITE THAT CANNOT BE SAMPLED OR HAVE ALREADY BEEN SAMPLED,
# THEN CONVERT TO GEOPACKAGE (retains full column names, whereas .shp
# abbreviates names) FOR MAPPING IN arcGIS.  GEOPACKAGE MUST BE CONVERTED TO .GDB IN GIS PRIOR
# TO 'OVERWRITE WEB LAYER' IN WEB MAP.

surgeDsn <- readxl::read_xlsx("../../../surgeDsn/SuRGE_design_20191206_eval_status.xlsx")
str(surgeDsn) 

# Prep for writing
surgeDsn <- surgeDsn %>% select(-xcoord_1, -ycoord_1) %>% # remove xcoord and ycoord, holdover from Tony's .shp
  # arcGIS Pro cannot handle "." in geopackage column names. probably can't handle " " either.
  # just remove.
  rename_all(~ gsub(" ", "", .)) %>% # replace " " in column names with ""
  # remove site deemed unsampleable
  filter(!EvalStatusCode %in% c("LD", "PI", "TR")) %>%
  # Pull out sites that are yet to be sampled, or don't have a sample year yet assigned
  # this includes oversample sites that do not have year assigned
  filter(SampleYear > 2020 | is.na(SampleYear)) 
         

# convert to sf object, then write to disk for mapping in GIS

surgeDsn.sf <- st_as_sf(surgeDsn, coords = c("LON_DD83", "LAT_DD83"), 
                        crs = 4269) %>% # NAD83
  st_transform(., crs = 3857) # web mercator

st_crs(surgeDsn.sf)
plot(surgeDsn.sf$geometry)

st_write(obj = surgeDsn.sf, 
         dsn = file.path( "../../../surgeDsn", "SuRGE_design_20191206_eval_status.gpkg"), 
         layer = "SuRGE_design_20191206", # package appends 'main.' to layer name?
         append=FALSE, # this overwrites existing layer
         driver = "GPKG")


unique(surgeDsn.sf$lab)




# sampled 14 in 2020
# total CIN sites for 2021/2022/2023
# 48-14 = 38 =~ 12/year
