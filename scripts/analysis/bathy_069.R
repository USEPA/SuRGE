bathy_69 <- read.table(paste0(userPath, "lakeDsn/CIN/CH4-069/bathymetry/Jake_OaheGIS/JGH_Oahe_Bathy/pf.txt")) %>%
  as_tibble() %>%
  rename(x=V1, y=V2, z=V3) %>%
  st_as_sf(., coords = c("x","y"), crs = 4326)
  
library(tmap)
tmap_mode("view")
tm_shape(bathy_69) +
  tm_dots()

summary(bathy_69)

st_write(bathy_69, dsn = paste0(userPath, "lakeDsn/CIN/CH4-069/bathymetry/Jake_OaheGIS/JGH_Oahe_Bathy/pf.shp"))

# GET RAW DEPTH SOUNDINGS AND LAKE POLYGON----------
# get boundary polygon
boundary_69 <- st_read(paste0(userPath, "lakeDsn/CIN/CH4-069/eqArea069_Merge.shp")) %>%
  # not certain if "source" is needed, following blog post
  # setting depth = 0 at boundary
  transmute(source = "boundary", depth = 0) %>%
  st_transform(st_crs(measured_depths_69)) # same CRS as depth soundings


C:\Users\JBEAULIE\Environmental Protection Agency (EPA)\SuRGE Survey of Reservoir Greenhouse gas Emissions - Documents\lakeDsn\CIN\CH4-069\bathymetry\Jake_OaheGIS\JGH_Oahe_Bathy\pf.shp
# Get depth soundings
measured_depths_69 <- st_read(paste0(userPath, "lakeDsn/CIN/CH4-069/bathymetry/Jake_OaheGIS/JHG_Oahe_bathy/pf.shp")) %>%
  st_zm() %>% # remove z geometry
  #st_transform(6358) %>% # convert to crs that tmap likes
  janitor::clean_names(.) %>%
  transmute(depth = 1351 - field3,
            source = "measured")