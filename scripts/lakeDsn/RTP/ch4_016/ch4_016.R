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


# JOLLY POND (ch4-016)--------
# Design for CH4-016, Jolly Pond, based NHD lake polygon edited
# to reflect shoreline in ESRI imagery.
# Very small (0.18 km2), using equal area design.

# Read polygon shapefile
eqArea016 <- st_read(dsn = "../../../lakeDsn/RTP/CH4-016",
                             layer = "eqArea016")  # shapefile name
plot(eqArea016$geometry) # visualize polygon

# Check CRS, must be equal area for grts function
# EPSG code hard to decipher from output, CRS clearly reported as 'NAD_1983_Albers'
# Coordinate System of NAD 1983 Albers (WKID = 0)
st_crs(eqArea016) 
st_crs(eqArea016) == st_crs(5070) # True

# clean up attributes
eqArea016 <- eqArea016 %>%
  mutate(lakeName = "jolly pond",
         lakeSiteID = "ch4-016") %>%
  select(lakeName, lakeSiteID, Area_km2)


# ATTRIBUTES----------
head(eqArea016) # review attributes



# SET UP FOR GRTS FUNCTION---------
# Call the set.seed function so that the survey designs can be replicateD
set.seed(4447864)

# Create the design list
dsgn016 <- list(None = list(panel=c(mainSites=15),
                                            seltype="Equal",
                                            over=20))
# create SpatialDesign object
sites016 <- grts(design=dsgn016,
                        DesignID="U", # U for unstratified
                        type.frame="area",
                        src.frame="sf.object",
                        sf.object=eqArea016,
                        shapefile=FALSE) # convert design object to sf, define prj, then write to .shp

# Print the initial six lines of the survey design
head(sites016)


# Print the survey design summary
summary(sites016)


# CRS--------------
class(sites016) # SpatialDesign object
sitesEqArea016 <- st_as_sf(sites016) # convert to sf object
st_crs(sitesEqArea016) # no coordinate reference system?
st_crs(sitesEqArea016) = 5070 # inherits from parent object, per Weber.

# project to WGS84 for plotting with leaflet, ArcPad, and writing out lat/long table  (5070 works fine for mapview)
WGS016 <- eqArea016 %>% st_transform(4326) # 4326 is WGS84, whereas 4269 is NAD83
sitesWGS016 <-  sitesEqArea016 %>% st_transform(4326) # 4326 is WGS84, whereas 4269 is NAD83 

# project to Web Mercator (3857) for use in ArcGIS Pro/geoplatform
merc016 <- eqArea016 %>% st_transform(3857) 
sitesMerc016 <-  sitesEqArea016 %>% st_transform(3857) 

# BUFFER
# Create a 15m radius buffer around each sampling point
# Per ESRI, buffer should be created in Web Mercator, an equidistant projection, not Albers
sitesMerc016buffer <- st_buffer(sitesMerc016, 15) # radius of 15m, diameter of 30m. radius = 45ft = 2 boat lengths
sitesWGS016buffer <- sitesMerc016buffer %>% st_transform(4326) # for use in ArcPad


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
# ch4_016_All.png works, but ch4_016_AllSites.png does not.


# leaflet colors
factpal.points <- colorFactor(palette = c("red", "black"), domain = sitesWGS016$panel)

# All points
m <- leaflet() %>% 
  addProviderTiles(providers$Esri.WorldImagery) %>%
  #setView(st_coordinates(cntr_crds)[1], st_coordinates(cntr_crds)[2], zoom = 15) %>%
  fitBounds(lng1 = min(st_coordinates(sitesWGS016)[,1]),
            lng2 = max(st_coordinates(sitesWGS016)[,1]),
            lat1 = min(st_coordinates(sitesWGS016)[,2]),
            lat2 = max(st_coordinates(sitesWGS016)[,2])) %>%
  addPolygons(data = st_zm(WGS016),  # removes z and m values, if present
              color = "black", weight = 1,
              fillOpacity = 1, fillColor = "blue") %>%
  addCircleMarkers(data = sitesWGS016,
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
            values = as.character(sitesWGS016$panel),
            title = "Sample sites") %>%
  addScaleBar()

mapview::mapshot(m, file = "../../../lakeDsn/RTP/CH4-016/016_All.png", 
                 remove_controls = NULL)


# Main points
m <- leaflet() %>% 
  addProviderTiles(providers$Esri.WorldImagery) %>%
  fitBounds(lng1 = min(st_coordinates(sitesWGS016)[,1]),
            lng2 = max(st_coordinates(sitesWGS016)[,1]),
            lat1 = min(st_coordinates(sitesWGS016)[,2]),
            lat2 = max(st_coordinates(sitesWGS016)[,2])) %>%
  addPolygons(data = st_zm(WGS016), # removes z and m values, if present
              color = "black", weight = 1,
              fillOpacity = 1, fillColor = "blue") %>%
  addCircleMarkers(data = filter(sitesWGS016, panel == "mainSites"),
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
  addScaleBar()

mapview::mapshot(m, file = "../../../lakeDsn/RTP/CH4-016/016_Main.png", 
                 remove_controls = NULL)


# OverSample points
m <- leaflet() %>% 
  addProviderTiles(providers$Esri.WorldImagery) %>%
  fitBounds(lng1 = min(st_coordinates(sitesWGS016)[,1]),
            lng2 = max(st_coordinates(sitesWGS016)[,1]),
            lat1 = min(st_coordinates(sitesWGS016)[,2]),
            lat2 = max(st_coordinates(sitesWGS016)[,2])) %>%
  addPolygons(data = st_zm(WGS016), # removes z and m values, if present
              color = "black", weight = 1,
              fillOpacity = 1, fillColor = "blue") %>%
  addCircleMarkers(data = filter(sitesWGS016, panel == "OverSamp"),
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
  addScaleBar()

mapview::mapshot(m, file = "../../../lakeDsn/RTP/CH4-016/016_Over.png", 
                 remove_controls = NULL)



# WRITE OBJECTS TO DISK-----------------
# write out table of sample sites for reference in field.  Must be WGS
write.table(sitesWGS016 %>%
              select(panel, siteID) %>%
              arrange(panel, siteID),
            file = "../../../lakeDsn/RTP/CH4-016/ch4_016_sites.txt",
            row.names = FALSE, sep="\t")


# geopackage
# can write all layers to a geopackage, which behaves much like a geodatabase in ArcGIS.

# Web Mercator for Web Map first

# write out all sites
st_write(obj = sitesMerc016, 
         dsn = file.path( "../../../lakeDsn/RTP/CH4-016", "merc016.gpkg"), 
         layer = "AllSitesMerc016", # package appends 'main.' to layer name?
         append=FALSE, # this overwrites existing layer
         driver = "GPKG")

# write out all buffers
st_write(obj = sitesMerc016buffer, 
         dsn = file.path( "../../../lakeDsn/RTP/CH4-016", "merc016.gpkg"), 
         layer = "AllSitesMerc016buffer", # package appends 'main.' to layer name?
         append=FALSE, # this overwrites existing layer
         driver = "GPKG")

# write out main sites
st_write(obj = filter(sitesMerc016, panel == "mainSites"),
         dsn = file.path( "../../../lakeDsn/RTP/CH4-016", "merc016.gpkg"), 
         layer = "MainSitesMerc016",
         append=FALSE, # this overwrites existing layer
         driver = "GPKG")

# write out main site buffers
st_write(obj = filter(sitesMerc016buffer, panel == "mainSites"),
         dsn = file.path( "../../../lakeDsn/RTP/CH4-016", "merc016.gpkg"), 
         layer = "MainSitesMerc016buffer",
         append=FALSE, # this overwrites existing layer
         driver = "GPKG")

# write out oversample sites
st_write(obj = filter(sitesMerc016, panel == "OverSamp"),
         dsn = file.path( "../../../lakeDsn/RTP/CH4-016", "merc016.gpkg"), 
         layer = "OverSampSitesMerc016",
         append=FALSE, # this overwrites existing layer,
         driver = "GPKG")

# write out oversample site buffers
st_write(obj = filter(sitesMerc016buffer, panel == "OverSamp"),
         dsn = file.path( "../../../lakeDsn/RTP/CH4-016", "merc016.gpkg"), 
         layer = "OverSampSitesMerc016buffer",
         append=FALSE, # this overwrites existing layer,
         driver = "GPKG")


# write out lake polygon
st_write(obj = merc016,
         dsn = file.path( "../../../lakeDsn/RTP/CH4-016", "merc016.gpkg"), 
         layer = "merc016",
         append=FALSE, # this overwrites existing layer,
         driver = "GPKG")


st_layers(file.path( "../../../lakeDsn/RTP/CH4-016", "merc016.gpkg"))


# WGS for ArcPad next

# write out all sites
st_write(obj = sitesWGS016, 
         dsn = file.path( "../../../lakeDsn/RTP/CH4-016", "WGS016.gpkg"), 
         layer = "AllSitesWGS016", # package appends 'main.' to layer name?
         append=FALSE, # this overwrites existing layer
         driver = "GPKG")

# write out all buffers
st_write(obj = sitesWGS016buffer, 
         dsn = file.path( "../../../lakeDsn/RTP/CH4-016", "WGS016.gpkg"), 
         layer = "sitesWGS016buffer", # package appends 'main.' to layer name?
         append=FALSE, # this overwrites existing layer
         driver = "GPKG")

# write out main sites
st_write(obj = filter(sitesWGS016, panel == "mainSites"),
         dsn = file.path( "../../../lakeDsn/RTP/CH4-016", "WGS016.gpkg"), 
         layer = "MainSitesWGS016",
         append=FALSE, # this overwrites existing layer
         driver = "GPKG")

# write out main site buffers
st_write(obj = filter(sitesWGS016buffer, panel == "mainSites"),
         dsn = file.path( "../../../lakeDsn/RTP/CH4-016", "WGS016.gpkg"), 
         layer = "MainSitesWGS016buffer",
         append=FALSE, # this overwrites existing layer
         driver = "GPKG")

# write out oversample sites
st_write(obj = filter(sitesWGS016, panel == "OverSamp"),
         dsn = file.path( "../../../lakeDsn/RTP/CH4-016", "WGS016.gpkg"), 
         layer = "OverSampSitesWGS016",
         append=FALSE, # this overwrites existing layer,
         driver = "GPKG")

# write out oversample site buffers
st_write(obj = filter(sitesWGS016buffer, panel == "OverSamp"),
         dsn = file.path( "../../../lakeDsn/RTP/CH4-016", "WGS016.gpkg"), 
         layer = "OverSampSitesWGS016buffer",
         append=FALSE, # this overwrites existing layer,
         driver = "GPKG")


# write out lake polygon
st_write(obj = WGS016,
         dsn = file.path( "../../../lakeDsn/RTP/CH4-016", "WGS016.gpkg"), 
         layer = "WGS016",
         append=FALSE, # this overwrites existing layer,
         driver = "GPKG")


st_layers(file.path( "../../../lakeDsn/RTP/CH4-016", "WGS016.gpkg"))

