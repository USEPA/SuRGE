# BATHYMETRY MODELING DEMO
# Roughly following this post: 
# https://dewey.dunnington.ca/post/2019/bathymetry-lake-volume-estimation-using-r/

library(tidyverse)
library(janitor)
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
# Get depth soundings

# Using Jean Neustadt (147), 1km2, for this demo
measured_depths_147 <- st_read(dsn = "~/private/bathymetry/147/Jean_Neustadt_Collected_Points.shp") %>%
  janitor::clean_names(.) %>%
  # convert depth to depth below the water surface, positive values
  # include `source` to help with troubleshooting.
  transmute(depth = depth * -1,
         source = "measured")

measured_depths_147

# get boundary polygon
boundary_147 <- st_read("~/private/bathymetry/147/eqArea147.shp") %>%
  # include `source` to help with troubleshooting.
  # setting depth = 0 at boundary
  transmute(source = "boundary", depth = 0) %>%
  st_transform(st_crs(measured_depths_147)) # same CRS as depth soundings

# plot raw depth soundings and bounding polygon
tmap_mode("view")
tm_shape(boundary_147) +
  tm_polygons() +
  tm_shape(measured_depths_147) +
  tm_dots()



# CREATE BOUNDARY POINTS FROM POLYGON---------
boundary_points_147 <- st_cast(boundary_147, "POINT")


# MERGE MEASURED DEPTHS AND BOUNDARY POINTS------------
depths_147 <- rbind(boundary_points_147, measured_depths_147) %>%
  cbind(., st_coordinates(.)) # add coordinates

depths_147



# PREPARE INTERPOLATION GRID--------
# Make grid for raster
grid_sf_147 <- st_make_grid(depths_147, 
                     cellsize = c(10, 10), # 10m x 10m cell size
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
intersections_147 <- st_intersects(grid_sf_147, boundary_147)

# Filter grid cells within bounding polygon
filtered_grid_147 <- grid_sf_147[which(sapply(intersections_147, length) > 0), ]

# Visualize grid and bounding polygon
tm_shape(boundary_147) +
  tm_polygons() +
  tm_shape(filtered_grid_147) +
  tm_dots()

# add coordinates to grid
filtered_grid_147 <- bind_cols(filtered_grid_147, 
                           st_coordinates(filtered_grid_147)) 


# TIN INTERPOLATION---------------
# triangular irregular network surface
tic() # 1337 sec with 10m grid
fit_TIN_147 <- interp::interpp(
  x = depths_147$X,
  y = depths_147$Y,
  z = depths_147$depth,
  xo = filtered_grid_147$X,
  yo = filtered_grid_147$Y,
  duplicate = "strip"
)
toc()

# Extract interpolated depth ("Z") from model and add to the grid
filtered_grid_147$TIN <- fit_TIN_147$z


# CONTOURING AND RASTER-----------
depth_raster <- filtered_grid_147 %>% 
  st_set_geometry(NULL) %>% 
  select(X, Y, TIN) %>% 
  raster::rasterFromXYZ(crs = raster::crs(measured_depths_147))

depth_contours <- depth_raster_147 %>% 
  raster::rasterToContour(levels = seq(from = 0, to = 40, by = 5)) %>% # can customize countours
  st_as_sf()

# PLOTTTING--------
ggplot(filtered_grid_147) +
  geom_sf(data = boundary_147) +
  geom_raster(aes(X, Y, fill = TIN)) +
  geom_sf(data = depth_contours) +
  scale_fill_viridis_c() +
  labs(x = NULL, y = NULL, fill = "Depth (ft)")

# WRITE RESULTS TO DISK-----------
st_write(depth_contours, 
         dsn = paste0(userPath, 
                      "lakeDsn/ADA/CH4-147/JeanNeustadt_bathy/",
                      "epa_interpolation/147_bathy_epa_contours.shp"),
         append = FALSE)
        
writeRaster(depth_raster, 
            # writing as ascii file type (ESRI Ascii)
            filename = paste0(userPath, 
                              "lakeDsn/ADA/CH4-147/JeanNeustadt_bathy/",
                              "epa_interpolation/147_bathy_epa.asc"),
            prj = TRUE, overwrite = TRUE)


