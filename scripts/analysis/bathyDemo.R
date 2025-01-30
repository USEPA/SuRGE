# BATHYMETRY MODELING DEMO
# Roughly following this post: 
# https://dewey.dunnington.ca/post/2019/bathymetry-lake-volume-estimation-using-r/


# GET RAW DEPTH SOUNDINGS AND LAKE POLYGON----------
# Get depth soundings

# Using Jean Neustadt (147), 1km2, for this demo
measured_depths <- st_read(paste0(userPath, 
                                  "lakeDsn/ADA/CH4-147/JeanNeustadt_bathy/",
                                  "Jean_Neustadt_Collected_Points.shp")) %>%
  janitor::clean_names(.) %>%
  transmute(depth = depth * -1,
         source = "measured")

measured_depths

# get boundary polygon
boundary <- st_read(paste0(userPath, 
                           "lakeDsn/ADA/CH4-147/eqArea147.shp")) %>%
  # not certain if "source" is needed, following blog post
  # setting depth = 0 at boundary
  transmute(source = "boundary", depth = 0) %>%
  st_transform(st_crs(measured_depths)) # same CRS as depth soundings

# plot raw depth soundings and bounding polygon
ggplot() +
  geom_sf(data = boundary) +
  geom_sf_text(aes(label = depth), data = measured_depths, size = 2.5) 

# create boundary points
boundary_points <- st_cast(boundary, "POINT")
depths <- rbind(boundary_points, measured_depths) %>%
  cbind(., st_coordinates(.)) # add coordinates, some methods need them

depths

# PREPARE INTERPOLATION GRID--------
# Make grid for raster
grid_sf <- st_make_grid(depths, 
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
intersections <- st_intersects(grid_sf, boundary)

# Filter grid cells within polygon
filtered_grid <- grid_sf[which(sapply(intersections, length) > 0), ]

# Visualize (optional)
plot(st_geometry(boundary))
plot(filtered_grid, add = TRUE, col = "red")

# add coordinates to grid, needed by some interpolation methods
filtered_grid <- bind_cols(filtered_grid, 
                           st_coordinates(filtered_grid)) 


# TIN INTERPOLATION---------------
# triangular irregular network surface
library(interp)
fit_TIN <- interp::interpp(
  x = depths$X,
  y = depths$Y,
  z = depths$depth,
  xo = filtered_grid$X,
  yo = filtered_grid$Y,
  duplicate = "strip"
)

# Extract interpolated depth ("Z") from model and add to the grid
filtered_grid$TIN <- fit_TIN$z


# CONTOURING AND RASTER-----------
depth_raster <- filtered_grid %>% 
  st_set_geometry(NULL) %>% 
  select(X, Y, TIN) %>% 
  raster::rasterFromXYZ(crs = raster::crs(measured_depths))

depth_contours <- depth_raster %>% 
  raster::rasterToContour(levels = seq(from = 0, to = 40, by = 5)) %>% 
  st_as_sf()

# PLOTTTING--------
ggplot(filtered_grid) +
  geom_sf(data = boundary) +
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


