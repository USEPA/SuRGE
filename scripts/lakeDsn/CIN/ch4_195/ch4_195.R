# UNSTRATIFIED EQUAL PROBABILITY GRTS DESIGN


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


# MONEE RESERVOIR (ch4-195)--------
# Design for CH4-195, Monee Reservoir, based NHD lake polygon edited
# to reflect shoreline in ESRI imagery.
# Very small (0.14 km2), using equal area design.

# Read polygon shapefile
eqArea195 <- st_read(dsn = "../../../lakeDsn/CIN/CH4-195",
                             layer = "eqArea195")  # shapefile name
plot(eqArea195$geometry) # visualize polygon

# Check CRS, must be equal area for grts function
# EPSG code hard to decipher from output, CRS clearly reported as 'NAD_1983_Albers'
# Coordinate System of NAD 1983 Albers (WKID = 0)
st_crs(eqArea195) 
st_crs(eqArea195) == st_crs(5070) # True

# clean up attributes
eqArea195 <- eqArea195 %>%
  mutate(lakeName = "Monee Reservoir",
         lakeSiteID = "ch4-195") %>%
  select(lakeName, lakeSiteID, Area_km2)


# ATTRIBUTES----------
head(eqArea195) # review attributes



# SET UP FOR GRTS FUNCTION---------
# Call the set.seed function so that the survey designs can be replicated
set.seed(4447864)

# Create the design list
dsgn195 <- list(None = list(panel=c(mainSites=15),
                                            seltype="Equal",
                                            over=20))
# create SpatialDesign object
sites195 <- grts(design=dsgn195,
                        DesignID="U", # U for unstratified
                        type.frame="area",
                        src.frame="sf.object",
                        sf.object=eqArea195,
                        shapefile=FALSE) # convert design object to sf, define prj, then write to .shp

# Print the initial six lines of the survey design
head(sites195)


# Print the survey design summary
summary(sites195)


# CRS--------------
class(sites195) # SpatialDesign object
siteseqArea195 <- st_as_sf(sites195) # convert to sf object
st_crs(siteseqArea195) # no coordinate reference system?
st_crs(siteseqArea195) = 5070 # inherits from parent object, per Weber.

# project to WGS84 for plotting with leaflet, ArcPad, and writing out lat/long table  (5070 works fine for mapview)
WGS195 <- eqArea195 %>% st_transform(4326) # 4326 is WGS84, whereas 4269 is NAD83
sitesWGS195 <-  siteseqArea195 %>% st_transform(4326) # 4326 is WGS84, whereas 4269 is NAD83 

# project to Web Mercator (3857) for use in ArcGIS Pro/geoplatform
merc195 <- eqArea195 %>% st_transform(3857) 
sitesMerc195 <-  siteseqArea195 %>% st_transform(3857) 

# BUFFER
# Create a 15m radius buffer around each sampling point
# Per ESRI, buffer should be created in Web Mercator, an equidistant projection, not Albers
sitesMerc195buffer <- st_buffer(sitesMerc195, 15) # radius of 15m, diameter of 30m. radius = 45ft = 2 boat lengths
sitesWGS195buffer <- sitesMerc195buffer %>% st_transform(4326) # for use in ArcPad


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
# ch4_195_All.png works, but ch4_195_AllSites.png does not.


# leaflet colors
factpal.points <- colorFactor(palette = c("red", "black"), domain = sitesWGS195$panel)

# All points
m <- leaflet() %>% 
  addProviderTiles(providers$Esri.WorldImagery) %>%
  #setView(st_coordinates(cntr_crds)[1], st_coordinates(cntr_crds)[2], zoom = 15) %>%
  fitBounds(lng1 = min(st_coordinates(sitesWGS195)[,1]),
            lng2 = max(st_coordinates(sitesWGS195)[,1]),
            lat1 = min(st_coordinates(sitesWGS195)[,2]),
            lat2 = max(st_coordinates(sitesWGS195)[,2])) %>%
  addPolygons(data = st_zm(WGS195),  # removes z and m values, if present
              color = "black", weight = 1,
              fillOpacity = 1, fillColor = "blue") %>%
  addCircleMarkers(data = sitesWGS195,
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
            values = as.character(sitesWGS195$panel),
            title = "Sample sites") %>%
  addScaleBar()

mapview::mapshot(m, file = "../../../lakeDsn/CIN/CH4-195/195_All.png", 
                 remove_controls = NULL)


# Main points
m <- leaflet() %>% 
  addProviderTiles(providers$Esri.WorldImagery) %>%
  fitBounds(lng1 = min(st_coordinates(sitesWGS195)[,1]),
            lng2 = max(st_coordinates(sitesWGS195)[,1]),
            lat1 = min(st_coordinates(sitesWGS195)[,2]),
            lat2 = max(st_coordinates(sitesWGS195)[,2])) %>%
  addPolygons(data = st_zm(WGS195), # removes z and m values, if present
              color = "black", weight = 1,
              fillOpacity = 1, fillColor = "blue") %>%
  addCircleMarkers(data = filter(sitesWGS195, panel == "mainSites"),
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

mapview::mapshot(m, file = "../../../lakeDsn/CIN/CH4-195/195_Main.png", 
                 remove_controls = NULL)


# OverSample points
m <- leaflet() %>% 
  addProviderTiles(providers$Esri.WorldImagery) %>%
  fitBounds(lng1 = min(st_coordinates(sitesWGS195)[,1]),
            lng2 = max(st_coordinates(sitesWGS195)[,1]),
            lat1 = min(st_coordinates(sitesWGS195)[,2]),
            lat2 = max(st_coordinates(sitesWGS195)[,2])) %>%
  addPolygons(data = st_zm(WGS195), # removes z and m values, if present
              color = "black", weight = 1,
              fillOpacity = 1, fillColor = "blue") %>%
  addCircleMarkers(data = filter(sitesWGS195, panel == "OverSamp"),
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

mapview::mapshot(m, file = "../../../lakeDsn/CIN/CH4-195/195_Over.png", 
                 remove_controls = NULL)



# WRITE OBJECTS TO DISK-----------------
# write out table of sample sites for reference in field.  Must be WGS
write.table(sitesWGS195 %>%
              select(panel, siteID) %>%
              arrange(panel, siteID),
            file = "../../../lakeDsn/CIN/CH4-195/ch4_195Sites.txt",
            row.names = FALSE, sep="\t")


# geopackage
# can write all layers to a geopackage, which behaves much like a geodatabase in ArcGIS.

# Web Mercator for Web Map first

# write out all sites
st_write(obj = sitesMerc195, 
         dsn = file.path( "../../../lakeDsn/CIN/CH4-195", "merc195.gpkg"), 
         layer = "AllSitesMerc195", # package appends 'main.' to layer name?
         append=FALSE, # this overwrites existing layer
         driver = "GPKG")

# write out all buffers
st_write(obj = sitesMerc195buffer, 
         dsn = file.path( "../../../lakeDsn/CIN/CH4-195", "merc195.gpkg"), 
         layer = "AllSitesMerc195buffer", # package appends 'main.' to layer name?
         append=FALSE, # this overwrites existing layer
         driver = "GPKG")

# write out main sites
st_write(obj = filter(sitesMerc195, panel == "mainSites"),
         dsn = file.path( "../../../lakeDsn/CIN/CH4-195", "merc195.gpkg"), 
         layer = "MainSitesMerc195",
         append=FALSE, # this overwrites existing layer
         driver = "GPKG")

# write out main site buffers
st_write(obj = filter(sitesMerc195buffer, panel == "mainSites"),
         dsn = file.path( "../../../lakeDsn/CIN/CH4-195", "merc195.gpkg"), 
         layer = "MainSitesMerc195buffer",
         append=FALSE, # this overwrites existing layer
         driver = "GPKG")

# write out oversample sites
st_write(obj = filter(sitesMerc195, panel == "OverSamp"),
         dsn = file.path( "../../../lakeDsn/CIN/CH4-195", "merc195.gpkg"), 
         layer = "OverSampSitesMerc195",
         append=FALSE, # this overwrites existing layer,
         driver = "GPKG")

# write out oversample site buffers
st_write(obj = filter(sitesMerc195buffer, panel == "OverSamp"),
         dsn = file.path( "../../../lakeDsn/CIN/CH4-195", "merc195.gpkg"), 
         layer = "OverSampSitesMerc195buffer",
         append=FALSE, # this overwrites existing layer,
         driver = "GPKG")


# write out lake polygon
st_write(obj = merc195,
         dsn = file.path( "../../../lakeDsn/CIN/CH4-195", "merc195.gpkg"), 
         layer = "merc195",
         append=FALSE, # this overwrites existing layer,
         driver = "GPKG")


st_layers(file.path( "../../../lakeDsn/CIN/CH4-195", "merc195.gpkg"))


# WGS for ArcPad next

# write out all sites
st_write(obj = sitesWGS195, 
         dsn = file.path( "../../../lakeDsn/CIN/CH4-195", "WGS195.gpkg"), 
         layer = "AllSitesWGS195", # package appends 'main.' to layer name?
         append=FALSE, # this overwrites existing layer
         driver = "GPKG")

# write out all buffers
st_write(obj = sitesWGS195buffer, 
         dsn = file.path( "../../../lakeDsn/CIN/CH4-195", "WGS195.gpkg"), 
         layer = "sitesWGS195buffer", # package appends 'main.' to layer name?
         append=FALSE, # this overwrites existing layer
         driver = "GPKG")

# write out main sites
st_write(obj = filter(sitesWGS195, panel == "mainSites"),
         dsn = file.path( "../../../lakeDsn/CIN/CH4-195", "WGS195.gpkg"), 
         layer = "MainSitesWGS195",
         append=FALSE, # this overwrites existing layer
         driver = "GPKG")

# write out main site buffers
st_write(obj = filter(sitesWGS195buffer, panel == "mainSites"),
         dsn = file.path( "../../../lakeDsn/CIN/CH4-195", "WGS195.gpkg"), 
         layer = "MainSitesWGS195buffer",
         append=FALSE, # this overwrites existing layer
         driver = "GPKG")

# write out oversample sites
st_write(obj = filter(sitesWGS195, panel == "OverSamp"),
         dsn = file.path( "../../../lakeDsn/CIN/CH4-195", "WGS195.gpkg"), 
         layer = "OverSampSitesWGS195",
         append=FALSE, # this overwrites existing layer,
         driver = "GPKG")

# write out oversample site buffers
st_write(obj = filter(sitesWGS195buffer, panel == "OverSamp"),
         dsn = file.path( "../../../lakeDsn/CIN/CH4-195", "WGS195.gpkg"), 
         layer = "OverSampSitesWGS195buffer",
         append=FALSE, # this overwrites existing layer,
         driver = "GPKG")


# write out lake polygon
st_write(obj = WGS195,
         dsn = file.path( "../../../lakeDsn/CIN/CH4-195", "WGS195.gpkg"), 
         layer = "WGS195",
         append=FALSE, # this overwrites existing layer,
         driver = "GPKG")


st_layers(file.path( "../../../lakeDsn/CIN/CH4-195", "WGS195.gpkg"))

