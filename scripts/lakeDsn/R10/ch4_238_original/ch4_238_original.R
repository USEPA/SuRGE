# STRATIFIED, UNEQUAL PROBABILITY GRTS DESIGN

##Design for Beulah Reservoir based on NHD lake polygon and bathymetry from 
##Portland State University.  This lake polygon is much larger than in recent
##satelite imagery.  Will create second design based on recent imagery.

## WHEN MODIFIYING FOR ANOTHER LAKE MAKE THE FOLLOWING CHANGES: 
##  MODIFY THE GRTS DESIGN LIST FOR THE NUMBER OF MAIN AND OVERSAMPLE SITES WANTED 
##  FOR EACH STRATA AND EACH SECTION, NOMINALLY:
##    OPEN WATER MAINSITES = 10
##            SECTION A (NORTH) = X
##            SECTION B (SOUTH) = 10-X
##    OPEN WATER OVER SAMPLE = 20
##    TRIBUTARY MAIN SITES = 5
##    TRIBUTARY OVER SAMPLE = 10
##  CHANGE THE ZOOM FACTOR ON LINE 179
##  FIND AND REPLACE ALL INSTANCES OF THE LAKE NAME


# BEULAH RESERVOIR 
# LARGE LAKE (7.2 km^2) WITH TWO TRIB ARMS (NW AND NE). NW TRIB IS THE MALHEUR
# RIVER AND APPEARS TO DRAIN MUCH LARGER AREA THAN NE ARM.
# ~100FT DEEP AT DAM

# A STRATIFIED-UNEQUAL PROBABILITY GRTS DESIGN
## UNEQUAL PROBABILITY USED IN AN ATTEMPT TO MAKE SAMPLING EASIER ON THE CREW
## THIS GRTS DESIGN HAS ALSO BEEN UPDATED TO HAVE THE EXPANDED ATTRIBUTE TABLE IN THE
## "EQAREA" VERSION OF THE SITE SHAPE FILE

# TRIB STRATA EXTENDS TO A DEPTH 8M (26 FT).  THIS IS THE CUTOFF USED FOR
# HARSHA LAKE.  BASING BATHYMETRY ON PORTLAND STATUE UNIVERSITY MAP.  APPEARS
# MORE ACCURATE THAN MODELED CONTOURS.


# WORKING DIRECTORY IS THE DIRECTORY CONTAINING THE .rproj FILE.  DATA IS KEPT THREE DIRECTORIES HIGHER IN SuRGE DOCUMENTS
# LIBRARY.  CAN USE RELATIVE PATHS TO READ IN DATA.---------
# dir("../../../") # two dots brings you up one level in data hierarchy.  This code brings you up three levels into SuRGE.

# LIBRARIES------
# library(tidyverse) # load from masterLibrary.R
# library(spsurvey) # load from masterLibrary.R
# library(leaflet) # load from masterLibrary.R
# library(mapview) # load from masterLibrary.R


# READ POLYGON SHAPEFILE-----
# USED IN grts() CALL
originalEqArea238 <- st_read(dsn = "../../../lakeDsn/R10/ch4_238 beulah/ch4_238_original",
                             layer = "beulahEqAreaOriginal")  # shapefile name
plot(originalEqArea238$geometry) # visualize polygon

# Check CRS, must be equal area for grts function
# no EPSG code, but arcGIS reports a Geographic Coordinate system of GCS North American 1983 (WKID 4269) and a Projected
# Coordinate System of NAD 1983 Albers (WKID = 0)
st_crs(originalEqArea238) 
originalEqArea238 <- st_transform(originalEqArea238, 5070) # convert to NAD_1983_Contiguous_USA_Albers
st_crs(originalEqArea238) # 5070
# clean up attributes
originalEqArea238 <- originalEqArea238 %>%
  mutate(lakeName = "Beulah",
         lakeSiteID = "ch4-238") %>%
  select(lakeName, lakeSiteID, Area_km2, strata, section)



# ATTRIBUTES----------
head(originalEqArea238) # review attributes

# summarize frame by section
temp <- with(originalEqArea238, tapply(Area_km2, section, sum))
temp <- round(addmargins(temp), 2)
temp


# SET UP FOR GRTS FUNCTION---------
# Call the set.seed function so that the survey designs can be replicateD
set.seed(4447864)

# Create the design list
### We decided to set the number of main sites in the tributary area to 5, since it is a relatively small area (0.4 sq km)
### the unequal probability splits the open water part of the lake into two sections of almost equal area
original238Dsgn <- list("open_water" = list(panel=c(mainSites=10),
                                            seltype="Unequal",
                                            caty.n=c("north" = 6, 
                                                     "south" = 4), 
                                            over=20),
                        "trib"=list(panel=c(mainSites=5), #5.2sites/km2
                                    seltype="Equal",
                                    over=10))
# create SpatialDesign object
originalSites238 <- grts(design=original238Dsgn,
                        DesignID="SU", # SU for stratified, unequal
                        type.frame="area",
                        src.frame="sf.object",
                        sf.object=originalEqArea238,
                        stratum="strata",
                        mdcaty="section",
                        shapefile=FALSE) # convert design object to sf, define prj, then write to .shp

# Print the initial six lines of the survey design
head(originalSites238)


# Print the survey design summary
summary(originalSites238)


# CRS--------------
# can specify grts to write out .shp.  if so, use this in grts function:
# out.shape="../../../lakeDsn/R10/ch4_238 beulah/ch4_238_original/originalSitesEqArea238")

# I tested this and shapefile is written out without any coordinate reference system (no .prj file)
# It might be cleaner to convert SpatialDesign object to sf, define CRS, then write to disk.
# Waiting for guidance from Marc Weber on how to define CRS of object.

class(originalSites238) # SpatialDesign object
originalSitesEqArea238 <- st_as_sf(originalSites238) # convert to sf object
st_crs(originalSitesEqArea238) # no coordinate reference system?
st_crs(originalSitesEqArea238) = 5070 # I'm pretty sure it should inherit CRS from parent object

# project to WGS84 for plotting with leaflet, ArcPad, and writing out lat/long table  (5070 works fine for mapview)
originalWGS238 <- originalEqArea238 %>% st_transform("+proj=longlat +datum=WGS84") # tried 4269 and 4238, but leaflet asked for this
originalSitesWGS238 <-  originalSitesEqArea238 %>% st_transform("+proj=longlat +datum=WGS84") # # tried 4269 and 4238, but leaflet asked for this 


# BUFFER
# Create a 15m radius buffer around each sampling point
originalSitesEqArea238buffer <- st_buffer(originalSitesEqArea238, 15) # radius of 15m, diameter of 30m?
originalSitesWGS238buffer <- st_transform(originalSitesEqArea238buffer, crs = "+proj=longlat +datum=WGS84")



# MAPS---------------------
# Approach 1: generate static maps to paste in .doc or .rmd (see ch4_238_original.html).  html or .doc can be printed for use in field.
# - ggmap requires an API key from google to provide satellite images.  Credit card required.  No thanks.
# - decided to make interactive map with leaflet, but capture image in .png with mapview::mapshot()

# Approach 2: knit .rmd to html with interactive maps.  These can be printed by field crews or viewed on computer.
# - getting some weird errors about path being too long in ch4_238_originalInteractive.rmd
# - keeping maps and mapshot below until problem solved.  Hopefully can come back and delete code below.


# Interactive below, export to .png using mapview::mapshot()
# # MAPVIEW APPROACH
# # mapview approach gets close, but I can't quite tweak everything the way I like
# # mapview can project on fly.  OK to use crs 5070
# # make map
# m <- mapview(originalEqArea238, zcol = "section", map.types = "Esri.WorldImagery") +
#   mapview(originalSitesEqArea238.sf, zcol = "panel", col.regions = c("red", "black")) 
# 
# 
# # define default zoom level # USE THIS APPROACH FOR MAPVIEW
# cntr_crds <- data.frame(lon = mean(st_coordinates(originalSitesEqArea238.sf)[,1]), # mean long
#                         lat = mean(st_coordinates(originalSitesEqArea238.sf)[,2])) %>% # mean lat
#   st_as_sf(., coords = c("lon", "lat"), crs = 5070) %>% # convert df to sf, inherits CRS from parent, 5070
#   st_transform(., 4269) # project to 5070 (WGS84 lat lon) for 'centering' using leaflet function
# 
# 
# m@map %>% leaflet::setView(st_coordinates(cntr_crds)[1], st_coordinates(cntr_crds)[2], zoom = 14)


# LEAFLET APPROACH
# Leaflet allows much more control, requires WGS84 CRS.
# define default zoom level, used with setView function
# setView requires manual fiddling with zoom level.  fitBounds() is cleaner
# cntr_crds <- data.frame(lon = mean(st_coordinates(originalSitesWGS238.sf)[,1]), # mean long
#                         lat = mean(st_coordinates(originalSitesWGS238.sf)[,2])) %>% # mean lat
#   st_as_sf(., coords = c("lon", "lat"), crs = "+proj=longlat +datum=WGS84") # convert df to sf, inherits CRS from parent, WGS84

  
# mapshot() call seems sensitive to length of file name (sometimes?).  Try to keep simple.
# ch4_238_originalAll.png works, but ch4_238_originalAllSites.png does not.


# leaflet colors
factpal <- colorFactor(topo.colors(3), originalWGS238$section)
factpal.points <- colorFactor(palette = c("red", "black"), domain = originalSitesWGS238$panel)

# All points
m <- leaflet() %>% 
  addProviderTiles(providers$Esri.WorldImagery) %>%
  #setView(st_coordinates(cntr_crds)[1], st_coordinates(cntr_crds)[2], zoom = 15) %>%
  fitBounds(lng1 = min(st_coordinates(originalSitesWGS238)[,1]),
            lng2 = max(st_coordinates(originalSitesWGS238)[,1]),
            lat1 = min(st_coordinates(originalSitesWGS238)[,2]),
            lat2 = max(st_coordinates(originalSitesWGS238)[,2])) %>%
  addPolygons(data = originalWGS238,
              color = "black", weight = 1,
              fillColor = ~factpal(section)) %>%
  addCircleMarkers(data = originalSitesWGS238,
                   fillColor = ~factpal.points(panel),
                   fillOpacity = 1,
                   stroke = FALSE,
                   radius = ~ifelse(panel == "mainSites", 10, 4),
                   label = ~siteID,
                   labelOptions = labelOptions(noHide = T, textOnly = TRUE,
                                               style = list(
                                                 color = "white",
                                                 "font-size" = "15px",
                                                 "font-family" = "serif"))) %>%
  addLegend(position = "bottomright", pal = factpal.points, 
            values = as.character(originalSitesWGS238$panel),
            title = "Sample sites") %>%
  addLegend(position = "bottomright", pal = factpal, 
            values = as.character(originalWGS238$section),
            title = "sections") %>%
  addScaleBar()

mapview::mapshot(m, file = "../../../lakeDsn/R10/ch4_238 beulah/ch4_238_original/238_originalAll.png", 
                 remove_controls = NULL)


# Main points
m <- leaflet() %>% 
  addProviderTiles(providers$Esri.WorldImagery) %>%
  fitBounds(lng1 = min(st_coordinates(originalSitesWGS238)[,1]),
            lng2 = max(st_coordinates(originalSitesWGS238)[,1]),
            lat1 = min(st_coordinates(originalSitesWGS238)[,2]),
            lat2 = max(st_coordinates(originalSitesWGS238)[,2])) %>%
  addPolygons(data = originalWGS238,
              color = "black", weight = 1,
              fillColor = ~factpal(section)) %>%
  addCircleMarkers(data = filter(originalSitesWGS238, panel == "mainSites"),
                   fillColor = "red", # manual color specification
                   fillOpacity = 1,
                   stroke = FALSE,
                   radius = 6,
                   label = ~siteID,
                   labelOptions = labelOptions(noHide = T, textOnly = TRUE,
                                               style = list(
                                                 color = "white",
                                                 "font-size" = "15px",
                                                 "font-family" = "serif"))) %>%
  # Rather than using pal and values, you can explicitly pass in colors and labels
  addLegend(position = "bottomright", 
            color = "red", # manual 'color' specification
            labels = "mainSites", # manual 'labels' specification
            title = "Sample sites") %>%
  addLegend(position = "bottomright", pal = factpal, 
            values = as.character(originalWGS238$section),
            title = "sections") %>%
  addScaleBar()

mapview::mapshot(m, file = "../../../lakeDsn/R10/ch4_238 beulah/ch4_238_original/238_originalMain.png", 
                 remove_controls = NULL)


# OverSample points
m <- leaflet() %>% 
  addProviderTiles(providers$Esri.WorldImagery) %>%
  fitBounds(lng1 = min(st_coordinates(originalSitesWGS238)[,1]),
            lng2 = max(st_coordinates(originalSitesWGS238)[,1]),
            lat1 = min(st_coordinates(originalSitesWGS238)[,2]),
            lat2 = max(st_coordinates(originalSitesWGS238)[,2])) %>%
  addPolygons(data = originalWGS238,
              color = "black", weight = 1,
              fillColor = ~factpal(section)) %>%
  addCircleMarkers(data = filter(originalSitesWGS238, panel == "OverSamp"),
                   fillColor = "black", # manual color specification
                   fillOpacity = 1,
                   stroke = FALSE,
                   radius = 6,
                   label = ~siteID,
                   labelOptions = labelOptions(noHide = T, textOnly = TRUE,
                                               style = list(
                                                 color = "white",
                                                 "font-size" = "15px",
                                                 "font-family" = "serif"))) %>%
  # Rather than using pal and values, you can explicitly pass in colors and labels
  addLegend(position = "bottomright", 
            color = "black", # manual 'color' specification
            labels = "OverSamp", # manual 'labels' specification
            title = "Sample sites") %>%
  addLegend(position = "bottomright", pal = factpal, 
            values = as.character(originalWGS238$section),
            title = "sections") %>%
  addScaleBar()

mapview::mapshot(m, file = "../../../lakeDsn/R10/ch4_238 beulah/ch4_238_original/238_originalOver.png", 
                 remove_controls = NULL)



# WRITE OBJECTS TO DISK-----------------
# write out table of sample sites for reference in field
write.table(originalSitesWGS238 %>%
              select(panel, siteID, stratum, section) %>%
              arrange(panel, stratum, section, siteID),
            file = "../../../lakeDsn/R10/ch4_238 beulah/ch4_238_original/ch4_238_originalSites.txt",
            row.names = FALSE, sep="\t")


#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_
# geopackage
# can write all layers to a geopackage, which behaves much like a geodatabase in ArcGIS.

# write out all sites
st_write(obj = originalSitesWGS238, 
         dsn = file.path( "../../../lakeDsn/R10/ch4_238 beulah/ch4_238_original", "originalWGS238.gpkg"), 
         layer = "originalAllSitesWGS238", # package appends 'main.' to layer name?
         layer_options = 'OVERWRITE=YES', # overwrite layer if already present
         #update = TRUE, actually not sure how this differs from 'OVERWRITE=YES'
         driver = "GPKG")

# write out all buffers
st_write(obj = originalSitesWGS238buffer, 
         dsn = file.path( "../../../lakeDsn/R10/ch4_238 beulah/ch4_238_original", "originalWGS238.gpkg"), 
         layer = "originalSitesWGS238buffer", # package appends 'main.' to layer name?
         layer_options = 'OVERWRITE=YES', # overwrite layer if already present
         #update = TRUE, actually not sure how this differs from 'OVERWRITE=YES'
         driver = "GPKG")



# write out main sites
st_write(obj = filter(originalSitesWGS238, panel == "mainSites"),
         dsn = file.path( "../../../lakeDsn/R10/ch4_238 beulah/ch4_238_original", "originalWGS238.gpkg"), 
         layer = "originalMainSitesWGS238",
         layer_options = 'OVERWRITE=YES', # overwrite layer if already present
         driver = "GPKG")

# write out main site buffers
st_write(obj = filter(originalSitesWGS238buffer, panel == "mainSites"),
         dsn = file.path( "../../../lakeDsn/R10/ch4_238 beulah/ch4_238_original", "originalWGS238.gpkg"), 
         layer = "originalMainSitesWGS238buffer",
         layer_options = 'OVERWRITE=YES', # overwrite layer if already present
         driver = "GPKG")

# write out oversample sites
st_write(obj = filter(originalSitesWGS238, panel == "OverSamp"),
         dsn = file.path( "../../../lakeDsn/R10/ch4_238 beulah/ch4_238_original", "originalWGS238.gpkg"), 
         layer = "originalOverSampSitesWGS238",
         layer_options = 'OVERWRITE=YES', # overwrite layer if already present,
         driver = "GPKG")

# write out oversample site buffers
st_write(obj = filter(originalSitesWGS238buffer, panel == "OverSamp"),
         dsn = file.path( "../../../lakeDsn/R10/ch4_238 beulah/ch4_238_original", "originalWGS238.gpkg"), 
         layer = "originalOverSampSitesWGS238buffer",
         layer_options = 'OVERWRITE=YES', # overwrite layer if already present,
         driver = "GPKG")


# write out lake polygon
st_write(obj = originalWGS238,
         dsn = file.path( "../../../lakeDsn/R10/ch4_238 beulah/ch4_238_original", "originalWGS238.gpkg"), 
         layer = "originalWGS238",
         layer_options = 'OVERWRITE=YES', # overwrite layer if already present,
         driver = "GPKG")


st_layers(file.path( "../../../lakeDsn/R10/ch4_238 beulah/ch4_238_original", "originalWGS238.gpkg"))


#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_
# to .shp 

# write out main sites
st_write(obj = filter(originalSitesWGS238, panel == "mainSites"), 
         dsn = "../../../lakeDsn/R10/ch4_238 beulah/ch4_238_original", 
         layer = "originalMainSitesWGS238", 
         update = TRUE,
         delete_layer = TRUE, # True to overwrite existing layer
         driver = "ESRI Shapefile")


# write out OverSamp sites
st_write(obj = filter(originalSitesWGS238, panel == "OverSamp"), 
         dsn = "../../../lakeDsn/R10/ch4_238 beulah/ch4_238_original", 
         layer = "originalOverSampSitesWGS238", 
         update = TRUE,
         delete_layer = TRUE, # True to overwrite existing layer
         driver = "ESRI Shapefile")

# write out all sites
st_write(obj = originalSitesWGS238, 
         dsn = "../../../lakeDsn/R10/ch4_238 beulah/ch4_238_original", 
         layer = "originalAllSitesWGS238", 
         update = TRUE,
         delete_layer = TRUE, # True to overwrite existing layer
         driver = "ESRI Shapefile")

# write out lake polygon
st_write(obj = originalWGS238, 
         dsn = "../../../lakeDsn/R10/ch4_238 beulah/ch4_238_original", 
         layer = "originalWGS238", 
         update = TRUE,
         delete_layer = TRUE, # True to overwrite existing layer
         driver = "ESRI Shapefile")


