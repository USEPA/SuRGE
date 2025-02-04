# BATHYMETRY MODELING 070,FRANCIS CASE
# Roughly following this post: 
# https://dewey.dunnington.ca/post/2019/bathymetry-lake-volume-estimation-using-r/
library(tidyverse)
library(janitor)
library(terra)
library(raster)
library(sf)
library(interp)
library(tictoc)
library(tmap)


library(conflicted)
conflicted::conflict_scout()
conflict_prefer("select", "dplyr") # select() will call dplyr::select()
conflict_prefer("filter", "dplyr") # filter() will call dplyr::filter()
conflict_prefer("rename", "dplyr") # filter() will call dplyr::rename()

# GET RAW DEPTH SOUNDINGS AND LAKE POLYGON----------
# get boundary polygon
boundary_70 <- st_read("~/private/bathymetry/70/eqArea070_Merge.shp") %>%
  # not certain if "source" is needed, following blog post
  # setting depth = 0 at boundary
  transmute(source = "boundary", depth = 0) %>%
  st_transform(st_crs(measured_depths)) # same CRS as depth soundings


# Get depth soundings
# SuRGE sampling was 6/25/2021. Gage height at station 06442996 was 55.5 ft.
# Gage datum is 1,295.56', therefore water surface was 1,295.56 + 55.5 = 1351'
measured_depths_70 <- st_read("~/private/bathymetry/70/Lake_Francis_Case_Hydro_1000_5_fall2022_NAVD88.shp") %>%
  st_zm() %>% # remove z geometry
  janitor::clean_names(.) %>%
  transmute(depth = 1351 - field3,
            source = "measured")

# any missing values
measured_depths_70 %>% filter(if_any(everything(), is.na)) # no missing values

# negative depths?
measured_depths_70 %>% filter(depth<0) # yes, 26,092

# where are negative depths located?
# mostly on perimeter of river, that is probably OK and will just assign
# a value of 0. 
# There is a cluster along the main channel a bit south of Chamberlain,
# however, that is problematic. The online map (https://fishing-app.gpsnauticalcharts.com/
# i-boating-fishing-web-app/fishing-marine-charts-navigation.html?title=Lake+
# Francis+Case+boating+app#12.74/43.6651/-99.3987) shows ~50' of water here. Probably
# best to delete these.
tmap_mode("view")
tm_shape(boundary_70) +
  tm_polygons() +
tm_shape(measured_depths_70 %>% filter(depth<0)) +
  tm_dots()


# replace depths < 0 with 0
measured_depths_70 <- measured_depths_70 %>%
  mutate(depth = case_when(depth < 0 ~ 0,
                           TRUE ~ depth))

# plot raw depth soundings and bounding polygon
tm_shape(boundary_70) +
  tm_polygons() +
  tm_shape(measured_depths_70) +
  tm_dots()
  

# CREATE BOUNDARY POINTS FROM POLYGON---------
# cast from polygon to points
boundary_points_70 <- st_cast(boundary_70, "POINT")

# inspect for missing values
boundary_points_70 %>% filter(if_any(everything(), is.na)) # kinda weird, 667 features with no depth

# plot missing values
# all near hwy 44 bridge?
tm_shape(boundary_points_70 %>% filter(if_any(everything(), is.na))) +
  tm_dots()

# fix missing values
boundary_points_70 <- boundary_points_70 %>%
  replace_na(list(source = "boundary", depth = 0))

# confirm values are fixed
boundary_points_70 %>% filter(if_any(everything(), is.na)) # no missing values, good 

# MERGE MEASURED DEPTHS AND BOUNDARY POINTS------------
depths_70 <- rbind(boundary_points_70, measured_depths_70) %>%
  cbind(., st_coordinates(.)) # add coordinates, some methods need them

# any missing values?
depths_70 %>% filter(if_any(everything(), is.na)) # no

# any identical points?
# 8571 total, 551 boundary points and 8020 measured
depths_70 %>% janitor::get_dupes(geometry) %>% {table(.$source)} # 8571 points with identical locations

# map dupes - widely distributed
tm_shape(depths_70 %>% janitor::get_dupes(geometry)) +
  tm_dots()

# aggregate dupes
# 549,087 points with dupes. Some are duplicated twice, other 3 times, etc
# 544,746 after removing duplicated values
# 549,087 - 544,746 = 4,341
depths_70 <- depths_70 %>% distinct()

# verify all dupes are removed
depths_70 %>% janitor::get_dupes(geometry) # 0, good
depths_70 %>% janitor::get_dupes(X, Y, depth) # 0, good


# PREPARE INTERPOLATION GRID--------
# Make grid for raster
grid_sf_70 <- st_make_grid(depths_70, 
                        cellsize = c(500, 500), # 10m x 10m cell size
                        what = "centers") %>%
  st_as_sf() 

# Identify intersecting grid cells
# Retain grid cells located with bounding polygon
# https://www.google.com/search?q=R+sf+filter+grid+cells+within+polygon
# &sca_esv=9535a44266e54a5f&rlz=1C1GCEA_enUS1052US1052&ei=aoebZ_vSAoaw5NoP36i-
# 4A8&ved=0ahUKEwj7r4f4zp2LAxUGGFkFHV-UD_wQ4dUDCBA&uact=5&oq=R+sf+filter+grid+
# cells+within+polygon&gs_lp=Egxnd3Mtd2l6LXNlcnAiJVIgc2YgZmlsdGVyIGdyaWQgY2Vsb
# HMgd2l0aGluIHBvbHlnb24yCBAhGKABGMMESLE8UPgiWJ05cAF4AJABAJgBeaABhQiqAQQxMS4xu
# AEDyAEA-AEBmAILoAKfB8ICChAAGLADGNYEGEfCAggQABgIGA0YHsICCxAAGIAEGIYDGIoFwgIIE
# AAYgAQYogTCAgoQIRigARjDBBgKmAMAiAYBkAYIkgcDOS4yoAezMg&sclient=gws-wiz-serp
intersections_70 <- st_intersects(grid_sf_70, boundary_70)

# Filter grid cells within polygon
filtered_grid_70 <- grid_sf_70[which(sapply(intersections_70, length) > 0), ]

# Visualize grid and bounding polygon
tm_shape(boundary_70) +
  tm_polygons() +
  tm_shape(filtered_grid_70) +
  tm_dots()


# add coordinates to grid
filtered_grid_70 <- bind_cols(filtered_grid_70, 
                           st_coordinates(filtered_grid_70)) 

# check for dups
filtered_grid_70 %>% janitor::get_dupes(X, Y) # 0, good

# TIN INTERPOLATION---------------
# triangular irregular network surface
tic()
fit_TIN <- interp::interpp(
  x = depths_70$X,
  y = depths_70$Y,
  z = depths_70$depth,
  xo = filtered_grid_70$X,
  yo = filtered_grid_70$Y,
  duplicate = "remove" # why isn't this stripping dups?
)
toc()

# Extract interpolated depth ("Z") from model and add to the grid
filtered_grid_70$TIN <- fit_TIN$z


# CONTOURING AND RASTER-----------
depth_raster <- filtered_grid_70 %>% 
  st_set_geometry(NULL) %>% 
  select(X, Y, TIN) %>% 
  raster::rasterFromXYZ(crs = raster::crs(measured_depths_70))

depth_contours <- depth_raster %>% 
  raster::rasterToContour(levels = seq(from = 0, to = 40, by = 5)) %>% 
  st_as_sf()

# PLOTTTING--------
ggplot(filtered_grid_70) +
  geom_sf(data = boundary_70) +
  geom_raster(aes(X, Y, fill = TIN)) +
  geom_sf(data = depth_contours) +
  scale_fill_viridis_c() +
  labs(x = NULL, y = NULL, fill = "Depth (ft)")

# WRITE RESULTS TO DISK-----------
st_write(depth_contours, 
         dsn = paste0(userPath, 
                      "lakeDsn/CIN/CH4-070/bathymetry/",
                      "epa_interpolation/147_bathy_epa_contours.shp"),
         append = FALSE)

writeRaster(depth_raster, 
            # writing as ascii file type (ESRI Ascii)
            filename = paste0(userPath, 
                              "lakeDsn/CIN/CH4-070/bathymetry/",
                              "epa_interpolation/147_bathy_epa.asc"),
            prj = TRUE, overwrite = TRUE)

