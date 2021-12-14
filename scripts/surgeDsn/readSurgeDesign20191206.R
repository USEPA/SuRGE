# CREATE GEOPACKAGE SHOWING SITES YET TO BE SAMPLED, PLUS OVERSAMPLE SITE
# DOESN'T SHOW SITE WE CAN'T SAMPLE


# TONY OLSEN GENERATED SuRGE_design_20191206.shp as the master design file for SuRGE.
# I CONVERTED THE .dbf FILE TO EXCEL AND ADDED A VARIETY OF COLUMNS TO FACILITATE 
# EXECUTION OF THE DESIGN (i.e. evalStatus, lab, etc).  THIS SCRIPT IS USED TO READ IN
# THE UPDATED EXCEL FILE, FILTER OUT SITE THAT CANNOT BE SAMPLED OR HAVE ALREADY BEEN SAMPLED,
# THEN CONVERT TO GEOPACKAGE (retains full column names, whereas .shp
# abbreviates names) FOR MAPPING IN arcGIS.  GEOPACKAGE MUST BE CONVERTED TO .GDB IN GIS PRIOR
# TO 'OVERWRITE WEB LAYER' IN WEB MAP.

# Read in data
surgeDsn <- readxl::read_xlsx("../../../surgeDsn/SuRGE_design_20191206_eval_status.xlsx") %>%
  select(-xcoord_1, -ycoord_1) %>% # remove xcoord and ycoord, holdover from Tony's .shp
  janitor::clean_names() # GIS is picky about names

unique(surgeDsn$lab)

# Convert to sf object
surgeDsn.sf <- st_as_sf(surgeDsn, coords = c("lon_dd83", "lat_dd83"), 
                        crs = 4269) %>% # NAD83
  st_transform(., crs = 3857) # web mercator

st_crs(surgeDsn.sf) # confirm CRS
plot(surgeDsn.sf$geometry) # preview plot


# Filter to sites to be sampled in 2022 - 2023, plus all unused oversample sites
surgeDsn22_23 <- surgeDsn.sf %>%
  # remove site deemed unsampleable
  filter(!eval_status_code %in% c("LD", "PI", "TR")) %>%
  # Pull out sites that are yet to be sampled, or don't have a sample year yet assigned
  # this includes oversample sites that do not have year assigned
  filter(sample_year > 2021 | is.na(sample_year)) 

surgeDsn22_23 %>% print(n=Inf)
         

# Filter to sites sampled in 2018, 2020, 2021, and those to be sampled in 2022 - 2023,
# plus oversample sites
surgeDsnSampled <- surgeDsn.sf %>%
  # remove site deemed unsampleable
  filter(!eval_status_code %in% c("LD", "PI", "TR"))

surgeDsnSampled %>% print(n=Inf)

# write 2022 - 2023 sites to disk
st_write(obj = surgeDsn22_23, 
         dsn = file.path( "../../../surgeDsn", "SuRGE_design_20191206_eval_status.gpkg"), 
         layer = "2022_2023_sites", # package appends 'main.' to layer name?
         append=FALSE, # this overwrites existing layer
         driver = "GPKG")


# write all sampled sites to disk
st_write(obj = surgeDsnSampled, 
         dsn = file.path( "../../../surgeDsn", "SuRGE_design_20191206_eval_status.gpkg"), 
         layer = "sampled_sites", # package appends 'main.' to layer name?
         append=FALSE, # this overwrites existing layer
         driver = "GPKG")

# Quick look at what remains to be sampled
surgeDsnSampled %>% 
  filter(sample_year >= 2022 | is.na(sample_year)) %>% # only those remaining
  {table(.$lab)} # only 16 for CIN


# CIN sampled in 2021
surgeDsnSampled %>% 
  filter(sample_year == 2021, lab == "CIN")

# should be 114 sites total (108 for SuRGE plus 6 extra from R10 2018)
addmargins(table(surgeDsnSampled$lab)) # yes


