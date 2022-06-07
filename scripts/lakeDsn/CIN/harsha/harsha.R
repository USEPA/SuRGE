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


# harsha--------
# Design for Harsha Lake.  This design will not be implemented.

# Read polygon shapefile
eqAreaharsha <- st_read(dsn = "../../../lakeDsn/CIN/harsha",
                             layer = "eqAreaharsha") %>% # shapefile name
  st_transform(5070)
plot(eqAreaharsha$geometry) # visualize polygon

# Check CRS, must be equal area for grts function
# EPSG code hard to decipher from output, CRS clearly reported as 'NAD_1983_Albers'
# Coordinate System of NAD 1983 Albers (WKID = 0)
st_crs(eqAreaharsha) 
st_crs(eqAreaharsha) == st_crs(5070) # True

# clean up attributes
eqAreaharsha <- eqAreaharsha %>%
  mutate(lakeName = "Harsha Lake",
         lakeSiteID = "ch4-harsha") %>%
  select(lakeName, lakeSiteID, Area_km2)


# ATTRIBUTES----------
head(eqAreaharsha) # review attributes



# SET UP FOR GRTS FUNCTION---------
# Call the set.seed function so that the survey designs can be replicateD
set.seed(4447864)

# Create the design list
dsgnharsha <- list(None = list(panel=c(mainSites=15),
                                            seltype="Equal",
                                            over=20))
# create SpatialDesign object
sitesharsha <- grts(design=dsgnharsha,
                        DesignID="U", # U for unstratified
                        type.frame="area",
                        src.frame="sf.object",
                        sf.object=eqAreaharsha,
                        shapefile=FALSE) # convert design object to sf, define prj, then write to .shp

# Print the initial six lines of the survey design
head(sitesharsha)


# Print the survey design summary
summary(sitesharsha)


# CRS--------------
class(sitesharsha) # SpatialDesign object
sitesEqAreaharsha <- st_as_sf(sitesharsha) # convert to sf object
st_crs(sitesEqAreaharsha) # no coordinate reference system?
st_crs(sitesEqAreaharsha) = 5070 # inherits from parent object, per Weber.

# project to WGS84 for plotting with leaflet, ArcPad, and writing out lat/long table  (5070 works fine for mapview)
WGSharsha <- eqAreaharsha %>% st_transform(4326) # 4326 is WGS84, whereas 4269 is NAD83
sitesWGSharsha <-  sitesEqAreaharsha %>% st_transform(4326) # 4326 is WGS84, whereas 4269 is NAD83 

# project to Web Mercator (3857) for use in ArcGIS Pro/geoplatform
mercharsha <- eqAreaharsha %>% st_transform(3857) 
sitesMercharsha <-  sitesEqAreaharsha %>% st_transform(3857) 

# BUFFER
# Create a 15m radius buffer around each sampling point
# Per ESRI, buffer should be created in Web Mercator, an equidistant projection, not Albers
sitesMercharshabuffer <- st_buffer(sitesMercharsha, 15) # radius of 15m, diameter of 30m. radius = 45ft = 2 boat lengths
sitesWGSharshabuffer <- sitesMercharshabuffer %>% st_transform(4326) # for use in ArcPad


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
# ch4_harsha_All.png works, but ch4_harsha_AllSites.png does not.


# leaflet colors
factpal.points <- colorFactor(palette = c("red", "black"), domain = sitesWGSharsha$panel)

# All points
m <- leaflet() %>% 
  addProviderTiles(providers$Esri.WorldImagery) %>%
  #setView(st_coordinates(cntr_crds)[1], st_coordinates(cntr_crds)[2], zoom = 15) %>%
  fitBounds(lng1 = min(st_coordinates(sitesWGSharsha)[,1]),
            lng2 = max(st_coordinates(sitesWGSharsha)[,1]),
            lat1 = min(st_coordinates(sitesWGSharsha)[,2]),
            lat2 = max(st_coordinates(sitesWGSharsha)[,2])) %>%
  addPolygons(data = st_zm(WGSharsha),  # removes z and m values, if present
              color = "black", weight = 1,
              fillOpacity = 1, fillColor = "blue") %>%
  addCircleMarkers(data = sitesWGSharsha,
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
            values = as.character(sitesWGSharsha$panel),
            title = "Sample sites") %>%
  addScaleBar()

mapview::mapshot(m, file = "../../../lakeDsn/CIN/harsha/harsha_All.png", 
                 remove_controls = NULL)


# Main points
m <- leaflet() %>% 
  addProviderTiles(providers$Esri.WorldImagery) %>%
  fitBounds(lng1 = min(st_coordinates(sitesWGSharsha)[,1]),
            lng2 = max(st_coordinates(sitesWGSharsha)[,1]),
            lat1 = min(st_coordinates(sitesWGSharsha)[,2]),
            lat2 = max(st_coordinates(sitesWGSharsha)[,2])) %>%
  addPolygons(data = st_zm(WGSharsha), # removes z and m values, if present
              color = "black", weight = 1,
              fillOpacity = 1, fillColor = "blue") %>%
  addCircleMarkers(data = filter(sitesWGSharsha, panel == "mainSites"),
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

mapview::mapshot(m, file = "../../../lakeDsn/CIN/harsha/harsha_Main.png", 
                 remove_controls = NULL)


# OverSample points
m <- leaflet() %>% 
  addProviderTiles(providers$Esri.WorldImagery) %>%
  fitBounds(lng1 = min(st_coordinates(sitesWGSharsha)[,1]),
            lng2 = max(st_coordinates(sitesWGSharsha)[,1]),
            lat1 = min(st_coordinates(sitesWGSharsha)[,2]),
            lat2 = max(st_coordinates(sitesWGSharsha)[,2])) %>%
  addPolygons(data = st_zm(WGSharsha), # removes z and m values, if present
              color = "black", weight = 1,
              fillOpacity = 1, fillColor = "blue") %>%
  addCircleMarkers(data = filter(sitesWGSharsha, panel == "OverSamp"),
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

mapview::mapshot(m, file = "../../../lakeDsn/CIN/harsha/harsha_Over.png", 
                 remove_controls = NULL)



# WRITE OBJECTS TO DISK-----------------
# write out table of sample sites for reference in field.  Must be WGS
write.table(sitesWGSharsha %>%
              select(panel, siteID) %>%
              arrange(panel, siteID),
            file = "../../../lakeDsn/CIN/harsha/ch4_harshaSites.txt",
            row.names = FALSE, sep="\t")


# geopackage
# can write all layers to a geopackage, which behaves much like a geodatabase in ArcGIS.

# Web Mercator for Web Map first

# write out all sites
st_write(obj = sitesMercharsha, 
         dsn = file.path( "../../../lakeDsn/CIN/harsha", "mercharsha.gpkg"), 
         layer = "AllSitesMercharsha", # package appends 'main.' to layer name?
         append=FALSE, # this overwrites existing layer
         driver = "GPKG")

# write out all buffers
st_write(obj = sitesMercharshabuffer, 
         dsn = file.path( "../../../lakeDsn/CIN/harsha", "mercharsha.gpkg"), 
         layer = "AllSitesMercharshabuffer", # package appends 'main.' to layer name?
         append=FALSE, # this overwrites existing layer
         driver = "GPKG")

# write out main sites
st_write(obj = filter(sitesMercharsha, panel == "mainSites"),
         dsn = file.path( "../../../lakeDsn/CIN/harsha", "mercharsha.gpkg"), 
         layer = "MainSitesMercharsha",
         append=FALSE, # this overwrites existing layer
         driver = "GPKG")

# write out main site buffers
st_write(obj = filter(sitesMercharshabuffer, panel == "mainSites"),
         dsn = file.path( "../../../lakeDsn/CIN/harsha", "mercharsha.gpkg"), 
         layer = "MainSitesMercharshabuffer",
         append=FALSE, # this overwrites existing layer
         driver = "GPKG")

# write out oversample sites
st_write(obj = filter(sitesMercharsha, panel == "OverSamp"),
         dsn = file.path( "../../../lakeDsn/CIN/harsha", "mercharsha.gpkg"), 
         layer = "OverSampSitesMercharsha",
         append=FALSE, # this overwrites existing layer,
         driver = "GPKG")

# write out oversample site buffers
st_write(obj = filter(sitesMercharshabuffer, panel == "OverSamp"),
         dsn = file.path( "../../../lakeDsn/CIN/harsha", "mercharsha.gpkg"), 
         layer = "OverSampSitesMercharshabuffer",
         append=FALSE, # this overwrites existing layer,
         driver = "GPKG")


# write out lake polygon
st_write(obj = mercharsha,
         dsn = file.path( "../../../lakeDsn/CIN/harsha", "mercharsha.gpkg"), 
         layer = "mercharsha",
         append=FALSE, # this overwrites existing layer,
         driver = "GPKG")


st_layers(file.path( "../../../lakeDsn/CIN/harsha", "mercharsha.gpkg"))


# WGS for ArcPad next

# write out all sites
st_write(obj = sitesWGSharsha, 
         dsn = file.path( "../../../lakeDsn/CIN/harsha", "WGSharsha.gpkg"), 
         layer = "AllSitesWGSharsha", # package appends 'main.' to layer name?
         append=FALSE, # this overwrites existing layer
         driver = "GPKG")

# write out all buffers
st_write(obj = sitesWGSharshabuffer, 
         dsn = file.path( "../../../lakeDsn/CIN/harsha", "WGSharsha.gpkg"), 
         layer = "sitesWGSharshabuffer", # package appends 'main.' to layer name?
         append=FALSE, # this overwrites existing layer
         driver = "GPKG")

# write out main sites
st_write(obj = filter(sitesWGSharsha, panel == "mainSites"),
         dsn = file.path( "../../../lakeDsn/CIN/harsha", "WGSharsha.gpkg"), 
         layer = "MainSitesWGSharsha",
         append=FALSE, # this overwrites existing layer
         driver = "GPKG")

# write out main site buffers
st_write(obj = filter(sitesWGSharshabuffer, panel == "mainSites"),
         dsn = file.path( "../../../lakeDsn/CIN/harsha", "WGSharsha.gpkg"), 
         layer = "MainSitesWGSharshabuffer",
         append=FALSE, # this overwrites existing layer
         driver = "GPKG")

# write out oversample sites
st_write(obj = filter(sitesWGSharsha, panel == "OverSamp"),
         dsn = file.path( "../../../lakeDsn/CIN/harsha", "WGSharsha.gpkg"), 
         layer = "OverSampSitesWGSharsha",
         append=FALSE, # this overwrites existing layer,
         driver = "GPKG")

# write out oversample site buffers
st_write(obj = filter(sitesWGSharshabuffer, panel == "OverSamp"),
         dsn = file.path( "../../../lakeDsn/CIN/harsha", "WGSharsha.gpkg"), 
         layer = "OverSampSitesWGSharshabuffer",
         append=FALSE, # this overwrites existing layer,
         driver = "GPKG")


# write out lake polygon
st_write(obj = WGSharsha,
         dsn = file.path( "../../../lakeDsn/CIN/harsha", "WGSharsha.gpkg"), 
         layer = "WGSharsha",
         append=FALSE, # this overwrites existing layer,
         driver = "GPKG")


st_layers(file.path( "../../../lakeDsn/CIN/harsha", "WGSharsha.gpkg"))

