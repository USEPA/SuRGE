# STRATIFIED, UNEQUAL PROBABILITY GRTS DESIGN

##Design for Beulah Reservoir based on NHD lake polygon modified to reflect
##"average" conditions based on Google Earth time series averages.  Polygon
##modifications executed by R10.  Alternative design for larger polygon area
##in beulahAverage.R.  Bathymetry from Portland State University.  


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
# LARGE LAKE (4.8 km^2) WITH TWO TRIB ARMS (NW AND NE). NW TRIB IS THE MALHEUR
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
averageEqArea238 <- st_read(dsn = "../../../lakeDsn/R10/ch4_238 beulah/ch4_238_average",
                             layer = "beulahEqAreaAverage")  # shapefile name
plot(averageEqArea238$geometry) # visualize polygon

# Check CRS, must be equal area for grts function
# no EPSG code, but arcGIS reports a Geographic Coordinate system of GCS North American 1983 (WKID 4269) and a Projected
# Coordinate System of NAD 1983 Albers (WKID = 0)
st_crs(averageEqArea238) 
averageEqArea238 <- st_transform(averageEqArea238, 5070) # convert to NAD_1983_Contiguous_USA_Albers
st_crs(averageEqArea238) # 5070
# clean up attributes
averageEqArea238 <- averageEqArea238 %>%
  mutate(lakeName = "Beulah",
         lakeSiteID = "ch4-238") %>%
  select(lakeName, lakeSiteID, Area_km2, strata, section)



# ATTRIBUTES----------
head(averageEqArea238) # review attributes

# summarize frame by section
temp <- with(averageEqArea238, tapply(Area_km2, section, sum))
temp <- round(addmargins(temp), 2)
temp


# SET UP FOR GRTS FUNCTION---------
# Call the set.seed function so that the survey designs can be replicateD
set.seed(4447864)

# Create the design list
### Five main sites in the tributary.
### the unequal probability splits the open water part of the lake into two sections of almost equal area
average238Dsgn <- list("open_water" = list(panel=c(mainSites=10),
                                            seltype="Unequal",
                                            caty.n=c("north" = 5, #2.7 sites/km2
                                                     "south" = 5), #2.6 sites/km2
                                            over=20),
                        "trib"=list(panel=c(mainSites=5), #5.2sites/km2
                                    seltype="Equal",
                                    over=10))
# create SpatialDesign object
averageSites238 <- grts(design=average238Dsgn,
                        DesignID="SU", # SU for stratified, unequal
                        type.frame="area",
                        src.frame="sf.object",
                        sf.object=averageEqArea238,
                        stratum="strata",
                        mdcaty="section",
                        shapefile=FALSE) # convert design object to sf, define prj, then write to .shp

# Print the initial six lines of the survey design
head(averageSites238)


# Print the survey design summary
summary(averageSites238)


# CRS--------------
# can specify grts to write out .shp.  if so, use this in grts function:
# out.shape="../../../lakeDsn/R10/ch4_238 beulah/ch4_238_average/averageSitesEqArea238")

# I tested this and shapefile is written out without any coordinate reference system (no .prj file)
# It might be cleaner to convert SpatialDesign object to sf, define CRS, then write to disk.
# Waiting for guidance from Marc Weber on how to define CRS of object.

class(averageSites238) # SpatialDesign object
averageSitesEqArea238 <- st_as_sf(averageSites238) # convert to sf object
st_crs(averageSitesEqArea238) # no coordinate reference system?
st_crs(averageSitesEqArea238) = 5070 # I'm pretty sure it should inherit CRS from parent object

# project to WGS84 for plotting with leaflet, ArcPad, and writing out lat/long table  (5070 works fine for mapview)
averageWGS238 <- averageEqArea238 %>% st_transform("+proj=longlat +datum=WGS84") # tried 4269 and 4238, but leaflet asked for this
averageSitesWGS238 <-  averageSitesEqArea238 %>% st_transform("+proj=longlat +datum=WGS84") # # tried 4269 and 4238, but leaflet asked for this 



# MAPS---------------------
# Approach 1: generate static maps to paste in .doc or .rmd (see ch4_238_average.html).  html or .doc can be printed for use in field.
# - ggmap requires an API key from google to provide satellite images.  Credit card required.  No thanks.
# - decided to make interactive map with leaflet, but capture image in .png with mapview::mapshot()

# Approach 2: knit .rmd to html with interactive maps.  These can be printed by field crews or viewed on computer.
# - getting some weird errors about path being too long in ch4_238_averageInteractive.rmd
# - keeping maps and mapshot below until problem solved.  Hopefully can come back and delete code below.


# Interactive below, export to .png using mapview::mapshot()
# # MAPVIEW APPROACH
# # mapview approach gets close, but I can't quite tweak everything the way I like
# # mapview can project on fly.  OK to use crs 5070
# # make map
# m <- mapview(averageEqArea238, zcol = "section", map.types = "Esri.WorldImagery") +
#   mapview(averageSitesEqArea238.sf, zcol = "panel", col.regions = c("red", "black")) 
# 
# 
# # define default zoom level # USE THIS APPROACH FOR MAPVIEW
# cntr_crds <- data.frame(lon = mean(st_coordinates(averageSitesEqArea238.sf)[,1]), # mean long
#                         lat = mean(st_coordinates(averageSitesEqArea238.sf)[,2])) %>% # mean lat
#   st_as_sf(., coords = c("lon", "lat"), crs = 5070) %>% # convert df to sf, inherits CRS from parent, 5070
#   st_transform(., 4269) # project to 5070 (WGS84 lat lon) for 'centering' using leaflet function
# 
# 
# m@map %>% leaflet::setView(st_coordinates(cntr_crds)[1], st_coordinates(cntr_crds)[2], zoom = 14)


# LEAFLET APPROACH
# Leaflet allows much more control, requires WGS84 CRS.
# define default zoom level, used with setView function
# setView requires manual fiddling with zoom level.  fitBounds() is cleaner
# cntr_crds <- data.frame(lon = mean(st_coordinates(averageSitesWGS238.sf)[,1]), # mean long
#                         lat = mean(st_coordinates(averageSitesWGS238.sf)[,2])) %>% # mean lat
#   st_as_sf(., coords = c("lon", "lat"), crs = "+proj=longlat +datum=WGS84") # convert df to sf, inherits CRS from parent, WGS84

  
# mapshot() call seems sensitive to length of file name (sometimes?).  Try to keep simple.
# ch4_238_averageAll.png works, but ch4_238_averageAllSites.png does not.


# leaflet colors
factpal <- colorFactor(topo.colors(3), averageWGS238$section)
factpal.points <- colorFactor(palette = c("red", "black"), domain = averageSitesWGS238$panel)

# All points
m <- leaflet() %>% 
  addProviderTiles(providers$Esri.WorldImagery) %>%
  #setView(st_coordinates(cntr_crds)[1], st_coordinates(cntr_crds)[2], zoom = 15) %>%
  fitBounds(lng1 = min(st_coordinates(averageSitesWGS238)[,1]),
            lng2 = max(st_coordinates(averageSitesWGS238)[,1]),
            lat1 = min(st_coordinates(averageSitesWGS238)[,2]),
            lat2 = max(st_coordinates(averageSitesWGS238)[,2])) %>%
  addPolygons(data = averageWGS238,
              color = "black", weight = 1,
              fillColor = ~factpal(section)) %>%
  addCircleMarkers(data = averageSitesWGS238,
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
            values = as.character(averageSitesWGS238$panel),
            title = "Sample sites") %>%
  addLegend(position = "bottomright", pal = factpal, 
            values = as.character(averageWGS238$section),
            title = "sections") %>%
  addScaleBar()

mapview::mapshot(m, file = "../../../lakeDsn/R10/ch4_238 beulah/ch4_238_average/238_averageAll.png", 
                 remove_controls = NULL)


# Main points
m <- leaflet() %>% 
  addProviderTiles(providers$Esri.WorldImagery) %>%
  fitBounds(lng1 = min(st_coordinates(averageSitesWGS238)[,1]),
            lng2 = max(st_coordinates(averageSitesWGS238)[,1]),
            lat1 = min(st_coordinates(averageSitesWGS238)[,2]),
            lat2 = max(st_coordinates(averageSitesWGS238)[,2])) %>%
  addPolygons(data = averageWGS238,
              color = "black", weight = 1,
              fillColor = ~factpal(section)) %>%
  addCircleMarkers(data = filter(averageSitesWGS238, panel == "mainSites"),
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
            values = as.character(averageWGS238$section),
            title = "sections") %>%
  addScaleBar()

mapview::mapshot(m, file = "../../../lakeDsn/R10/ch4_238 beulah/ch4_238_average/238_averageMain.png", 
                 remove_controls = NULL)


# OverSample points
m <- leaflet() %>% 
  addProviderTiles(providers$Esri.WorldImagery) %>%
  fitBounds(lng1 = min(st_coordinates(averageSitesWGS238)[,1]),
            lng2 = max(st_coordinates(averageSitesWGS238)[,1]),
            lat1 = min(st_coordinates(averageSitesWGS238)[,2]),
            lat2 = max(st_coordinates(averageSitesWGS238)[,2])) %>%
  addPolygons(data = averageWGS238,
              color = "black", weight = 1,
              fillColor = ~factpal(section)) %>%
  addCircleMarkers(data = filter(averageSitesWGS238, panel == "OverSamp"),
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
            values = as.character(averageWGS238$section),
            title = "sections") %>%
  addScaleBar()

mapview::mapshot(m, file = "../../../lakeDsn/R10/ch4_238 beulah/ch4_238_average/238_averageOver.png", 
                 remove_controls = NULL)



# WRITE OBJECTS TO DISK-----------------
# write out table of sample sites for reference in field
write.table(averageSitesWGS238 %>%
              select(panel, siteID, stratum, section) %>%
              arrange(panel, stratum, section, siteID),
            file = "../../../lakeDsn/R10/ch4_238 beulah/ch4_238_average/ch4_238_averageSites.txt",
            row.names = FALSE, sep="\t")

# write out main sites
st_write(obj = filter(averageSitesWGS238, panel == "mainSites"), 
         dsn = "../../../lakeDsn/R10/ch4_238 beulah/ch4_238_average", 
         layer = "averageMainSitesWGS238", 
         update = TRUE,
         delete_layer = TRUE, # True to overwrite existing layer
         driver = "ESRI Shapefile")


# write out OverSamp sites
st_write(obj = filter(averageSitesWGS238, panel == "OverSamp"), 
         dsn = "../../../lakeDsn/R10/ch4_238 beulah/ch4_238_average", 
         layer = "averageOverSampSitesWGS238", 
         update = TRUE,
         delete_layer = TRUE, # True to overwrite existing layer
         driver = "ESRI Shapefile")

# write out all sites
st_write(obj = averageSitesWGS238, 
         dsn = "../../../lakeDsn/R10/ch4_238 beulah/ch4_238_average", 
         layer = "averageAllSitesWGS238", 
         update = TRUE,
         delete_layer = TRUE, # True to overwrite existing layer
         driver = "ESRI Shapefile")

# write out lake polygon
st_write(obj = averageWGS238, 
         dsn = "../../../lakeDsn/R10/ch4_238 beulah/ch4_238_average", 
         layer = "averageWGS238", 
         update = TRUE,
         delete_layer = TRUE, # True to overwrite existing layer
         driver = "ESRI Shapefile")


