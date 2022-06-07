# UNSTRATIFIED EQUAL PROBABILITY GRTS DESIGN

# PRELIMINARIES-------------
# When modifying for another lake: 
#  Modify design list for number of main/oversample sites wanted 
#  for each strata and section, nominally:
#    open water main sites = 10
#            section A (north) = X
#            section B (south) = 10-X
#    open water oversample = 20
#    trib main sites = 5
#    trib oversample sites = 10
#  find and replace all instances of lake name


# WORKING DIRECTORY 
# Working directory contains .rproj file.  Data is kept three directories higher in SuRGE documents
# librarys.  Use relative paths to read/write data.
# dir("../../../") # two dots brings you up one level in data hierarchy.  This code brings you up three levels into SuRGE.

# LIBRARIES
# library(tidyverse) # load from masterLibrary.R
# library(spsurvey) # load from masterLibrary.R
# library(leaflet) # load from masterLibrary.R
# library(mapview) # load from masterLibrary.R

# PROJECTIONS
# grts function requires Albers (5070)
# leaflet requires WGS84 ()
# lat/long table requires WGS84
# ArcGIS Pro/Geoplatform requires Web Mercator (3857)


# JOLLY POND (ch4-084)--------
# Design for CH4-084, Jolly Pond, based NHD lake polygon edited
# to reflect shoreline in ESRI imagery.
# Very small (0.18 km2), using equal area design.

# Read polygon shapefile
eqArea084 <- st_read(dsn = "../../../lakeDsn/DOE/CH4-084",
                             layer = "eqArea084")  # shapefile name
plot(eqArea084$geometry) # visualize polygon

# Check CRS, must be equal area for grts function
# EPSG code hard to decipher from output, CRS clearly reported as 'NAD_1983_Albers'
# Coordinate System of NAD 1983 Albers (WKID = 0)
st_crs(eqArea084) 
st_crs(eqArea084) == st_crs(5070) # True

# clean up attributes
eqArea084 <- eqArea084 %>%
  mutate(lakeName = "jolly pond",
         lakeSiteID = "ch4-084") %>%
  select(lakeName, lakeSiteID, Area_km2)


# ATTRIBUTES----------
head(eqArea084) # review attributes



# SET UP FOR GRTS FUNCTION---------
# Call the set.seed function so that the survey designs can be replicateD
set.seed(4447864)

# Create the design list
dsgn084 <- list(None = list(panel=c(mainSites=15),
                                            seltype="Equal",
                                            over=20))
# create SpatialDesign object
sites084 <- grts(design=dsgn084,
                        DesignID="U", # U for unstratified
                        type.frame="area",
                        src.frame="sf.object",
                        sf.object=eqArea084,
                        shapefile=FALSE) # convert design object to sf, define prj, then write to .shp

# Print the initial six lines of the survey design
head(sites084)


# Print the survey design summary
summary(sites084)


# CRS--------------
class(sites084) # SpatialDesign object
sitesEqArea084 <- st_as_sf(sites084) # convert to sf object
st_crs(sitesEqArea084) # no coordinate reference system?
st_crs(sitesEqArea084) = 5070 # inherits from parent object, per Weber.

# project to WGS84 for plotting with leaflet, ArcPad, and writing out lat/long table  (5070 works fine for mapview)
WGS084 <- eqArea084 %>% st_transform(4326) # 4326 is WGS84, whereas 4269 is NAD83
sitesWGS084 <-  sitesEqArea084 %>% st_transform(4326) # 4326 is WGS84, whereas 4269 is NAD83 

# project to Web Mercator (3857) for use in ArcGIS Pro/geoplatform
merc084 <- eqArea084 %>% st_transform(3857) 
sitesMerc084 <-  sitesEqArea084 %>% st_transform(3857) 

# BUFFER
# Create a 15m radius buffer around each sampling point
# Per ESRI, buffer should be created in Web Mercator, an equidistant projection, not Albers
sitesMerc084buffer <- st_buffer(sitesMerc084, 15) # radius of 15m, diameter of 30m. radius = 45ft = 2 boat lengths
sitesWGS084buffer <- sitesMerc084buffer %>% st_transform(4326) # for use in ArcPad


# MAPS---------------------
# Approach: generate static maps to paste in .rmd.  Knitted html can be printed for use in field.
# - ggmap requires an API key from google to provide satellite images.  Credit card required.  No thanks.
# - decided to make interactive map with leaflet, but capture image in .png with mapview::mapshot()
# - per Lil, field crews won't make use of interactive maps


# LEAFLET
# Mapview approach gets close, but I can't quite tweak everything the way I like
# Leaflet allows much more control, requires WGS84 CRS.
# setView for zoo level requires manual fiddling with zoom level.  fitBounds() is cleaner
# mapshot() call seems sensitive to length of file name (sometimes?).  Try to keep simple.
# ch4_084_All.png works, but ch4_084_AllSites.png does not.


# leaflet colors
factpal.points <- colorFactor(palette = c("red", "black"), domain = sitesWGS084$panel)

# All points
m <- leaflet() %>% 
  addProviderTiles(providers$Esri.WorldImagery) %>%
  #setView(st_coordinates(cntr_crds)[1], st_coordinates(cntr_crds)[2], zoom = 15) %>%
  fitBounds(lng1 = min(st_coordinates(sitesWGS084)[,1]),
            lng2 = max(st_coordinates(sitesWGS084)[,1]),
            lat1 = min(st_coordinates(sitesWGS084)[,2]),
            lat2 = max(st_coordinates(sitesWGS084)[,2])) %>%
  addPolygons(data = st_zm(WGS084),  # removes z and m values, if present
              color = "black", weight = 1,
              fillOpacity = 1, fillColor = "blue") %>%
  addCircleMarkers(data = sitesWGS084,
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
            opacity = 1,
            values = as.character(sitesWGS084$panel),
            title = "Sample sites") %>%
  addScaleBar()

mapview::mapshot(m, file = "../../../lakeDsn/DOE/CH4-084/084_All.png", 
                 remove_controls = NULL)


# Main points
m <- leaflet() %>% 
  addProviderTiles(providers$Esri.WorldImagery) %>%
  fitBounds(lng1 = min(st_coordinates(sitesWGS084)[,1]),
            lng2 = max(st_coordinates(sitesWGS084)[,1]),
            lat1 = min(st_coordinates(sitesWGS084)[,2]),
            lat2 = max(st_coordinates(sitesWGS084)[,2])) %>%
  addPolygons(data = st_zm(WGS084), # removes z and m values, if present
              color = "black", weight = 1,
              fillOpacity = 1, fillColor = "blue") %>%
  addCircleMarkers(data = filter(sitesWGS084, panel == "mainSites"),
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
            opacity = 1,
            labels = "mainSites", # manual 'labels' specification
            title = "Sample sites") %>%
  addScaleBar()

mapview::mapshot(m, file = "../../../lakeDsn/DOE/CH4-084/084_Main.png", 
                 remove_controls = NULL)


# OverSample points
m <- leaflet() %>% 
  addProviderTiles(providers$Esri.WorldImagery) %>%
  fitBounds(lng1 = min(st_coordinates(sitesWGS084)[,1]),
            lng2 = max(st_coordinates(sitesWGS084)[,1]),
            lat1 = min(st_coordinates(sitesWGS084)[,2]),
            lat2 = max(st_coordinates(sitesWGS084)[,2])) %>%
  addPolygons(data = st_zm(WGS084), # removes z and m values, if present
              color = "black", weight = 1,
              fillOpacity = 1, fillColor = "blue") %>%
  addCircleMarkers(data = filter(sitesWGS084, panel == "OverSamp"),
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
            opacity = 1,
            labels = "OverSamp", # manual 'labels' specification
            title = "Sample sites") %>%
  addScaleBar()

mapview::mapshot(m, file = "../../../lakeDsn/DOE/CH4-084/084_Over.png", 
                 remove_controls = NULL)



# WRITE OBJECTS TO DISK-----------------
# write out table of sample sites for reference in field.  Must be WGS
write.table(sitesWGS084 %>%
              select(panel, siteID) %>%
              arrange(panel, siteID),
            file = "../../../lakeDsn/DOE/CH4-084/ch4_084Sites.txt",
            row.names = FALSE, sep="\t")


# geopackage
# can write all layers to a geopackage, which behaves much like a geodatabase in ArcGIS.

# Web Mercator for Web Map first

# write out all sites
st_write(obj = sitesMerc084, 
         dsn = file.path( "../../../lakeDsn/DOE/CH4-084", "merc084.gpkg"), 
         layer = "AllSitesMerc084", # package appends 'main.' to layer name?
         append=FALSE, # this overwrites existing layer
         driver = "GPKG")

# write out all buffers
st_write(obj = sitesMerc084buffer, 
         dsn = file.path( "../../../lakeDsn/DOE/CH4-084", "merc084.gpkg"), 
         layer = "AllSitesMerc084buffer", # package appends 'main.' to layer name?
         append=FALSE, # this overwrites existing layer
         driver = "GPKG")

# write out main sites
st_write(obj = filter(sitesMerc084, panel == "mainSites"),
         dsn = file.path( "../../../lakeDsn/DOE/CH4-084", "merc084.gpkg"), 
         layer = "MainSitesMerc084",
         append=FALSE, # this overwrites existing layer
         driver = "GPKG")

# write out main site buffers
st_write(obj = filter(sitesMerc084buffer, panel == "mainSites"),
         dsn = file.path( "../../../lakeDsn/DOE/CH4-084", "merc084.gpkg"), 
         layer = "MainSitesMerc084buffer",
         append=FALSE, # this overwrites existing layer
         driver = "GPKG")

# write out oversample sites
st_write(obj = filter(sitesMerc084, panel == "OverSamp"),
         dsn = file.path( "../../../lakeDsn/DOE/CH4-084", "merc084.gpkg"), 
         layer = "OverSampSitesMerc084",
         append=FALSE, # this overwrites existing layer,
         driver = "GPKG")

# write out oversample site buffers
st_write(obj = filter(sitesMerc084buffer, panel == "OverSamp"),
         dsn = file.path( "../../../lakeDsn/DOE/CH4-084", "merc084.gpkg"), 
         layer = "OverSampSitesMerc084buffer",
         append=FALSE, # this overwrites existing layer,
         driver = "GPKG")


# write out lake polygon
st_write(obj = merc084,
         dsn = file.path( "../../../lakeDsn/DOE/CH4-084", "merc084.gpkg"), 
         layer = "merc084",
         append=FALSE, # this overwrites existing layer,
         driver = "GPKG")


st_layers(file.path( "../../../lakeDsn/DOE/CH4-084", "merc084.gpkg"))


# WGS for ArcPad next

# write out all sites
st_write(obj = sitesWGS084, 
         dsn = file.path( "../../../lakeDsn/DOE/CH4-084", "WGS084.gpkg"), 
         layer = "AllSitesWGS084", # package appends 'main.' to layer name?
         append=FALSE, # this overwrites existing layer
         driver = "GPKG")

# write out all buffers
st_write(obj = sitesWGS084buffer, 
         dsn = file.path( "../../../lakeDsn/DOE/CH4-084", "WGS084.gpkg"), 
         layer = "sitesWGS084buffer", # package appends 'main.' to layer name?
         append=FALSE, # this overwrites existing layer
         driver = "GPKG")

# write out main sites
st_write(obj = filter(sitesWGS084, panel == "mainSites"),
         dsn = file.path( "../../../lakeDsn/DOE/CH4-084", "WGS084.gpkg"), 
         layer = "MainSitesWGS084",
         append=FALSE, # this overwrites existing layer
         driver = "GPKG")

# write out main site buffers
st_write(obj = filter(sitesWGS084buffer, panel == "mainSites"),
         dsn = file.path( "../../../lakeDsn/DOE/CH4-084", "WGS084.gpkg"), 
         layer = "MainSitesWGS084buffer",
         append=FALSE, # this overwrites existing layer
         driver = "GPKG")

# write out oversample sites
st_write(obj = filter(sitesWGS084, panel == "OverSamp"),
         dsn = file.path( "../../../lakeDsn/DOE/CH4-084", "WGS084.gpkg"), 
         layer = "OverSampSitesWGS084",
         append=FALSE, # this overwrites existing layer,
         driver = "GPKG")

# write out oversample site buffers
st_write(obj = filter(sitesWGS084buffer, panel == "OverSamp"),
         dsn = file.path( "../../../lakeDsn/DOE/CH4-084", "WGS084.gpkg"), 
         layer = "OverSampSitesWGS084buffer",
         append=FALSE, # this overwrites existing layer,
         driver = "GPKG")


# write out lake polygon
st_write(obj = WGS084,
         dsn = file.path( "../../../lakeDsn/DOE/CH4-084", "WGS084.gpkg"), 
         layer = "WGS084",
         append=FALSE, # this overwrites existing layer,
         driver = "GPKG")


st_layers(file.path( "../../../lakeDsn/DOE/CH4-084", "WGS084.gpkg"))

