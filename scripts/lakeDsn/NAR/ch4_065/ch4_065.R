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


# Weatherhead Hollow (ch4-065)--------
# Design for CH4-065, Weatherhead Hollow, based NHD lake polygon edited
# to reflect shoreline in ESRI imagery and checked agains recent Landsat imagery.
# Very small (0.1 km2), using equal area design.

# Read polygon shapefile
eqArea065 <- st_read(dsn = "../../../lakeDsn/NAR/CH4-065",
                             layer = "eqArea065")  # shapefile name
plot(eqArea065$geometry) # visualize polygon

# Check CRS, must be equal area for grts function
# EPSG code hard to decipher from output, CRS clearly reported as 'NAD_1983_Albers'
# Coordinate System of NAD 1983 Albers (WKID = 0)
st_crs(eqArea065) 
st_crs(eqArea065) == st_crs(5070) # True

# clean up attributes
eqArea065 <- eqArea065 %>%
  mutate(lakeName = "jolly pond",
         lakeSiteID = "ch4-065") %>%
  select(lakeName, lakeSiteID, Area_km2)


# ATTRIBUTES----------
head(eqArea065) # review attributes



# SET UP FOR GRTS FUNCTION---------
# Call the set.seed function so that the survey designs can be replicateD
set.seed(4447864)

# Create the design list
dsgn065 <- list(None = list(panel=c(mainSites=15),
                                            seltype="Equal",
                                            over=20))
# create SpatialDesign object
sites065 <- grts(design=dsgn065,
                        DesignID="U", # U for unstratified
                        type.frame="area",
                        src.frame="sf.object",
                        sf.object=eqArea065,
                        shapefile=FALSE) # convert design object to sf, define prj, then write to .shp

# Print the initial six lines of the survey design
head(sites065)


# Print the survey design summary
summary(sites065)


# CRS--------------
class(sites065) # SpatialDesign object
sitesEqArea065 <- st_as_sf(sites065) # convert to sf object
st_crs(sitesEqArea065) # no coordinate reference system?
st_crs(sitesEqArea065) = 5070 # inherits from parent object, per Weber.

# project to WGS84 for plotting with leaflet, ArcPad, and writing out lat/long table  (5070 works fine for mapview)
WGS065 <- eqArea065 %>% st_transform(4326) # 4326 is WGS84, whereas 4269 is NAD83
sitesWGS065 <-  sitesEqArea065 %>% st_transform(4326) # 4326 is WGS84, whereas 4269 is NAD83 

# project to Web Mercator (3857) for use in ArcGIS Pro/geoplatform
merc065 <- eqArea065 %>% st_transform(3857) 
sitesMerc065 <-  sitesEqArea065 %>% st_transform(3857) 

# BUFFER
# Create a 15m radius buffer around each sampling point
# Per ESRI, buffer should be created in Web Mercator, an equidistant projection, not Albers
sitesMerc065buffer <- st_buffer(sitesMerc065, 15) # radius of 15m, diameter of 30m. radius = 45ft = 2 boat lengths
sitesWGS065buffer <- sitesMerc065buffer %>% st_transform(4326) # for use in ArcPad


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
# ch4_065_All.png works, but ch4_065_AllSites.png does not.


# leaflet colors
factpal.points <- colorFactor(palette = c("red", "black"), domain = sitesWGS065$panel)

# All points
m <- leaflet() %>% 
  addProviderTiles(providers$Esri.WorldImagery) %>%
  #setView(st_coordinates(cntr_crds)[1], st_coordinates(cntr_crds)[2], zoom = 15) %>%
  fitBounds(lng1 = min(st_coordinates(sitesWGS065)[,1]),
            lng2 = max(st_coordinates(sitesWGS065)[,1]),
            lat1 = min(st_coordinates(sitesWGS065)[,2]),
            lat2 = max(st_coordinates(sitesWGS065)[,2])) %>%
  addPolygons(data = st_zm(WGS065),  # removes z and m values, if present
              color = "black", weight = 1,
              fillOpacity = 1, fillColor = "blue") %>%
  addCircleMarkers(data = sitesWGS065,
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
            values = as.character(sitesWGS065$panel),
            title = "Sample sites") %>%
  addScaleBar()

mapview::mapshot(m, file = "../../../lakeDsn/NAR/CH4-065/065_All.png", 
                 remove_controls = NULL)


# Main points
m <- leaflet() %>% 
  addProviderTiles(providers$Esri.WorldImagery) %>%
  fitBounds(lng1 = min(st_coordinates(sitesWGS065)[,1]),
            lng2 = max(st_coordinates(sitesWGS065)[,1]),
            lat1 = min(st_coordinates(sitesWGS065)[,2]),
            lat2 = max(st_coordinates(sitesWGS065)[,2])) %>%
  addPolygons(data = st_zm(WGS065), # removes z and m values, if present
              color = "black", weight = 1,
              fillOpacity = 1, fillColor = "blue") %>%
  addCircleMarkers(data = filter(sitesWGS065, panel == "mainSites"),
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

mapview::mapshot(m, file = "../../../lakeDsn/NAR/CH4-065/065_Main.png", 
                 remove_controls = NULL)


# OverSample points
m <- leaflet() %>% 
  addProviderTiles(providers$Esri.WorldImagery) %>%
  fitBounds(lng1 = min(st_coordinates(sitesWGS065)[,1]),
            lng2 = max(st_coordinates(sitesWGS065)[,1]),
            lat1 = min(st_coordinates(sitesWGS065)[,2]),
            lat2 = max(st_coordinates(sitesWGS065)[,2])) %>%
  addPolygons(data = st_zm(WGS065), # removes z and m values, if present
              color = "black", weight = 1,
              fillOpacity = 1, fillColor = "blue") %>%
  addCircleMarkers(data = filter(sitesWGS065, panel == "OverSamp"),
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

mapview::mapshot(m, file = "../../../lakeDsn/NAR/CH4-065/065_Over.png", 
                 remove_controls = NULL)



# WRITE OBJECTS TO DISK-----------------
# write out table of sample sites for reference in field.  Must be WGS
write.table(sitesWGS065 %>%
              select(panel, siteID) %>%
              arrange(panel, siteID),
            file = "../../../lakeDsn/NAR/CH4-065/ch4_065Sites.txt",
            row.names = FALSE, sep="\t")


# geopackage
# can write all layers to a geopackage, which behaves much like a geodatabase in ArcGIS.

# Web Mercator for Web Map first

# write out all sites
st_write(obj = sitesMerc065, 
         dsn = file.path( "../../../lakeDsn/NAR/CH4-065", "merc065.gpkg"), 
         layer = "AllSitesMerc065", # package appends 'main.' to layer name?
         append=FALSE, # this overwrites existing layer
         driver = "GPKG")

# write out all buffers
st_write(obj = sitesMerc065buffer, 
         dsn = file.path( "../../../lakeDsn/NAR/CH4-065", "merc065.gpkg"), 
         layer = "AllSitesMerc065buffer", # package appends 'main.' to layer name?
         append=FALSE, # this overwrites existing layer
         driver = "GPKG")

# write out main sites
st_write(obj = filter(sitesMerc065, panel == "mainSites"),
         dsn = file.path( "../../../lakeDsn/NAR/CH4-065", "merc065.gpkg"), 
         layer = "MainSitesMerc065",
         append=FALSE, # this overwrites existing layer
         driver = "GPKG")

# write out main site buffers
st_write(obj = filter(sitesMerc065buffer, panel == "mainSites"),
         dsn = file.path( "../../../lakeDsn/NAR/CH4-065", "merc065.gpkg"), 
         layer = "MainSitesMerc065buffer",
         append=FALSE, # this overwrites existing layer
         driver = "GPKG")

# write out oversample sites
st_write(obj = filter(sitesMerc065, panel == "OverSamp"),
         dsn = file.path( "../../../lakeDsn/NAR/CH4-065", "merc065.gpkg"), 
         layer = "OverSampSitesMerc065",
         append=FALSE, # this overwrites existing layer,
         driver = "GPKG")

# write out oversample site buffers
st_write(obj = filter(sitesMerc065buffer, panel == "OverSamp"),
         dsn = file.path( "../../../lakeDsn/NAR/CH4-065", "merc065.gpkg"), 
         layer = "OverSampSitesMerc065buffer",
         append=FALSE, # this overwrites existing layer,
         driver = "GPKG")


# write out lake polygon
st_write(obj = merc065,
         dsn = file.path( "../../../lakeDsn/NAR/CH4-065", "merc065.gpkg"), 
         layer = "merc065",
         append=FALSE, # this overwrites existing layer,
         driver = "GPKG")


st_layers(file.path( "../../../lakeDsn/NAR/CH4-065", "merc065.gpkg"))


# WGS for ArcPad next

# write out all sites
st_write(obj = sitesWGS065, 
         dsn = file.path( "../../../lakeDsn/NAR/CH4-065", "WGS065.gpkg"), 
         layer = "AllSitesWGS065", # package appends 'main.' to layer name?
         append=FALSE, # this overwrites existing layer
         driver = "GPKG")

# write out all buffers
st_write(obj = sitesWGS065buffer, 
         dsn = file.path( "../../../lakeDsn/NAR/CH4-065", "WGS065.gpkg"), 
         layer = "sitesWGS065buffer", # package appends 'main.' to layer name?
         append=FALSE, # this overwrites existing layer
         driver = "GPKG")

# write out main sites
st_write(obj = filter(sitesWGS065, panel == "mainSites"),
         dsn = file.path( "../../../lakeDsn/NAR/CH4-065", "WGS065.gpkg"), 
         layer = "MainSitesWGS065",
         append=FALSE, # this overwrites existing layer
         driver = "GPKG")

# write out main site buffers
st_write(obj = filter(sitesWGS065buffer, panel == "mainSites"),
         dsn = file.path( "../../../lakeDsn/NAR/CH4-065", "WGS065.gpkg"), 
         layer = "MainSitesWGS065buffer",
         append=FALSE, # this overwrites existing layer
         driver = "GPKG")

# write out oversample sites
st_write(obj = filter(sitesWGS065, panel == "OverSamp"),
         dsn = file.path( "../../../lakeDsn/NAR/CH4-065", "WGS065.gpkg"), 
         layer = "OverSampSitesWGS065",
         append=FALSE, # this overwrites existing layer,
         driver = "GPKG")

# write out oversample site buffers
st_write(obj = filter(sitesWGS065buffer, panel == "OverSamp"),
         dsn = file.path( "../../../lakeDsn/NAR/CH4-065", "WGS065.gpkg"), 
         layer = "OverSampSitesWGS065buffer",
         append=FALSE, # this overwrites existing layer,
         driver = "GPKG")


# write out lake polygon
st_write(obj = WGS065,
         dsn = file.path( "../../../lakeDsn/NAR/CH4-065", "WGS065.gpkg"), 
         layer = "WGS065",
         append=FALSE, # this overwrites existing layer,
         driver = "GPKG")


st_layers(file.path( "../../../lakeDsn/NAR/CH4-065", "WGS065.gpkg"))

