# UNSTRATIFIED, UNEQUAL PROBABILITY GRTS DESIGN

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


# ATAGAHI LAKE (ch4-102)--------
# Design for CH4-102, Atagahi Lake, based on NHD lake polygon

# Read polygon shapefile
eqArea102 <- st_read(dsn = "../../../lakeDsn/RTP/CH4-102",
                             layer = "eqArea102")  # shapefile name
plot(eqArea102$geometry) # visualize polygon

# Check CRS, must be equal area for grts function
# EPSG code hard to decipher from output, CRS clearly reported as 'NAD_1983_Albers'
# Coordinate System of NAD 1983 Albers (WKID = 0)
st_crs(eqArea102) 
st_crs(eqArea102) == st_crs(5070) # True

# clean up attributes
eqArea102 <- eqArea102 %>%
  mutate(lakeName = "Atagahi",
         lakeSiteID = "ch4-102") %>%
  select(lakeName, lakeSiteID, Area_km2, section)


# ATTRIBUTES----------
head(eqArea102) # review attributes

# summarize frame by section
temp <- with(eqArea102, tapply(Area_km2, section, sum))
temp <- round(addmargins(temp), 2)
temp


# SET UP FOR GRTS FUNCTION---------
# Call the set.seed function so that the survey designs can be replicateD
set.seed(4447864)

# Create the design list
### We decided to set the number of main sites in the tributary area to 5, since it is a relatively small area (0.4 sq km)
### the unequal probability splits the open water part of the lake into two sections of almost equal area
dsgn102 <- list(None = list(panel=c(mainSites=15),
                            seltype="Unequal",
                            caty.n=c("north" = 13, # grts gives 5, = 5.4 sites/km2
                                     "south" = 2), # grts gives 7, 5.9 sites/km2
                            over=20))
# create SpatialDesign object
sites102 <- grts(design=dsgn102,
                 DesignID="U", # U for unstratified, unequal
                 type.frame="area",
                 src.frame="sf.object",
                 sf.object=eqArea102,
                 mdcaty="section",
                 shapefile=FALSE) # convert design object to sf, define prj, then write to .shp

# Print the initial six lines of the survey design
head(sites102)


# Print the survey design summary
summary(sites102)


# CRS--------------
class(sites102) # SpatialDesign object
sites102@data <- sites102@data %>% rename(section = mdcaty) # rename
sitesEqArea102 <- st_as_sf(sites102) # convert to sf object
st_crs(sitesEqArea102) # no coordinate reference system?
st_crs(sitesEqArea102) = 5070 # inherits from parent object, per Weber.

# project to WGS84 for plotting with leaflet, ArcPad, and writing out lat/long table  (5070 works fine for mapview)
WGS102 <- eqArea102 %>% st_transform(4326) # 4326 is WGS84, whereas 4269 is NAD83
sitesWGS102 <-  sitesEqArea102 %>% st_transform(4326) # 4326 is WGS84, whereas 4269 is NAD83 

# project to Web Mercator (3857) for use in ArcGIS Pro/geoplatform
merc102 <- eqArea102 %>% st_transform(3857) 
sitesMerc102 <-  sitesEqArea102 %>% st_transform(3857) 

# BUFFER
# Create a 15m radius buffer around each sampling point
# Per ESRI, buffer should be created in Web Mercator, an equidistant projection, not Albers
sitesMerc102buffer <- st_buffer(sitesMerc102, 15) # radius of 15m, diameter of 30m. radius = 45ft = 2 boat lengths
sitesWGS102buffer <- sitesMerc102buffer %>% st_transform(4326) # for use in ArcPad


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
# ch4_102_lowAll.png works, but ch4_102_lowAllSites.png does not.


# leaflet colors
factpal <- colorFactor(topo.colors(3), WGS102$section)
factpal.points <- colorFactor(palette = c("red", "black"), domain = sitesWGS102$panel)

# All points
m <- leaflet() %>% 
  addProviderTiles(providers$Esri.WorldImagery) %>%
  #setView(st_coordinates(cntr_crds)[1], st_coordinates(cntr_crds)[2], zoom = 15) %>%
  fitBounds(lng1 = min(st_coordinates(sitesWGS102)[,1]),
            lng2 = max(st_coordinates(sitesWGS102)[,1]),
            lat1 = min(st_coordinates(sitesWGS102)[,2]),
            lat2 = max(st_coordinates(sitesWGS102)[,2])) %>%
  addPolygons(data = st_zm(WGS102), # removes z and m values, if present
              color = "black", weight = 1,
              fillOpacity = 1, fillColor = ~factpal(section)) %>%
  addCircleMarkers(data = sitesWGS102,
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
            values = as.character(sitesWGS102$panel),
            title = "Sample sites") %>%
  addLegend(position = "bottomright", pal = factpal, 
            opacity = 1,
            values = as.character(WGS102$section),
            title = "sections") %>%
  addScaleBar()

mapview::mapshot(m, file = "../../../lakeDsn/RTP/CH4-102/102_All.png", 
                 remove_controls = NULL)


# Main points
m <- leaflet() %>% 
  addProviderTiles(providers$Esri.WorldImagery) %>%
  fitBounds(lng1 = min(st_coordinates(sitesWGS102)[,1]),
            lng2 = max(st_coordinates(sitesWGS102)[,1]),
            lat1 = min(st_coordinates(sitesWGS102)[,2]),
            lat2 = max(st_coordinates(sitesWGS102)[,2])) %>%
  addPolygons(data = st_zm(WGS102), # removes z and m values, if present
              color = "black", weight = 1,
              fillOpacity = 1, fillColor = ~factpal(section)) %>%
  addCircleMarkers(data = filter(sitesWGS102, panel == "mainSites"),
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
  addLegend(position = "bottomright", pal = factpal, 
            opacity = 1,
            values = as.character(WGS102$section),
            title = "sections") %>%
  addScaleBar()

mapview::mapshot(m, file = "../../../lakeDsn/RTP/CH4-102/102_Main.png", 
                 remove_controls = NULL)


# OverSample points
m <- leaflet() %>% 
  addProviderTiles(providers$Esri.WorldImagery) %>%
  fitBounds(lng1 = min(st_coordinates(sitesWGS102)[,1]),
            lng2 = max(st_coordinates(sitesWGS102)[,1]),
            lat1 = min(st_coordinates(sitesWGS102)[,2]),
            lat2 = max(st_coordinates(sitesWGS102)[,2])) %>%
  addPolygons(data = st_zm(WGS102),  # removes z and m values, if present
              color = "black", weight = 1,
              fillOpacity = 1, fillColor = ~factpal(section)) %>%
  addCircleMarkers(data = filter(sitesWGS102, panel == "OverSamp"),
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
  addLegend(position = "bottomright", pal = factpal, 
            opacity = 1,
            values = as.character(WGS102$section),
            title = "sections") %>%
  addScaleBar()

mapview::mapshot(m, file = "../../../lakeDsn/RTP/CH4-102/102_Over.png", 
                 remove_controls = NULL)



# WRITE OBJECTS TO DISK-----------------
# write out table of sample sites for reference in field.  Must be WGS
write.table(sitesWGS102 %>%
              select(panel, siteID, section) %>%
              arrange(panel, section, siteID),
            file = "../../../lakeDsn/RTP/CH4-102/ch4_102Sites.txt",
            row.names = FALSE, sep="\t")


# geopackage
# can write all layers to a geopackage, which behaves much like a geodatabase in ArcGIS.

# Web Mercator for Web Map first

# write out all sites
st_write(obj = sitesMerc102, 
         dsn = file.path( "../../../lakeDsn/RTP/CH4-102", "merc102.gpkg"), 
         layer = "allSitesMerc102", # package appends 'main.' to layer name?
         append=FALSE, # this overwrites existing layer
         driver = "GPKG")

# write out all buffers
st_write(obj = sitesMerc102buffer, 
         dsn = file.path( "../../../lakeDsn/RTP/CH4-102", "merc102.gpkg"), 
         layer = "allSitesMerc102buffer", # package appends 'main.' to layer name?
         append=FALSE, # this overwrites existing layer
         driver = "GPKG")

# write out main sites
st_write(obj = filter(sitesMerc102, panel == "mainSites"),
         dsn = file.path( "../../../lakeDsn/RTP/CH4-102", "merc102.gpkg"), 
         layer = "mainSitesMerc102",
         append=FALSE, # this overwrites existing layer
         driver = "GPKG")

# write out main site buffers
st_write(obj = filter(sitesMerc102buffer, panel == "mainSites"),
         dsn = file.path( "../../../lakeDsn/RTP/CH4-102", "merc102.gpkg"), 
         layer = "mainSitesMerc102buffer",
         append=FALSE, # this overwrites existing layer
         driver = "GPKG")

# write out oversample sites
st_write(obj = filter(sitesMerc102, panel == "OverSamp"),
         dsn = file.path( "../../../lakeDsn/RTP/CH4-102", "merc102.gpkg"), 
         layer = "overSampSitesMerc102",
         append=FALSE, # this overwrites existing layer,
         driver = "GPKG")

# write out oversample site buffers
st_write(obj = filter(sitesMerc102buffer, panel == "OverSamp"),
         dsn = file.path( "../../../lakeDsn/RTP/CH4-102", "merc102.gpkg"), 
         layer = "overSampSitesMerc102buffer",
         append=FALSE, # this overwrites existing layer,
         driver = "GPKG")


# write out lake polygon
st_write(obj = merc102,
         dsn = file.path( "../../../lakeDsn/RTP/CH4-102", "merc102.gpkg"), 
         layer = "merc102",
         append=FALSE, # this overwrites existing layer,
         driver = "GPKG")


st_layers(file.path( "../../../lakeDsn/RTP/CH4-102", "merc102.gpkg"))


# WGS for ArcPad next

# write out all sites
st_write(obj = sitesWGS102, 
         dsn = file.path( "../../../lakeDsn/RTP/CH4-102", "WGS102.gpkg"), 
         layer = "allSitesWGS102", # package appends 'main.' to layer name?
         append=FALSE, # this overwrites existing layer
         driver = "GPKG")

# write out all buffers
st_write(obj = sitesWGS102buffer, 
         dsn = file.path( "../../../lakeDsn/RTP/CH4-102", "WGS102.gpkg"), 
         layer = "sitesWGS102buffer", # package appends 'main.' to layer name?
         append=FALSE, # this overwrites existing layer
         driver = "GPKG")

# write out main sites
st_write(obj = filter(sitesWGS102, panel == "mainSites"),
         dsn = file.path( "../../../lakeDsn/RTP/CH4-102", "WGS102.gpkg"), 
         layer = "mainSitesWGS102",
         append=FALSE, # this overwrites existing layer
         driver = "GPKG")

# write out main site buffers
st_write(obj = filter(sitesWGS102buffer, panel == "mainSites"),
         dsn = file.path( "../../../lakeDsn/RTP/CH4-102", "WGS102.gpkg"), 
         layer = "mainSitesWGS102buffer",
         append=FALSE, # this overwrites existing layer
         driver = "GPKG")

# write out oversample sites
st_write(obj = filter(sitesWGS102, panel == "OverSamp"),
         dsn = file.path( "../../../lakeDsn/RTP/CH4-102", "WGS102.gpkg"), 
         layer = "overSampSitesWGS102",
         append=FALSE, # this overwrites existing layer,
         driver = "GPKG")

# write out oversample site buffers
st_write(obj = filter(sitesWGS102buffer, panel == "OverSamp"),
         dsn = file.path( "../../../lakeDsn/RTP/CH4-102", "WGS102.gpkg"), 
         layer = "overSampSitesWGS102buffer",
         append=FALSE, # this overwrites existing layer,
         driver = "GPKG")


# write out lake polygon
st_write(obj = WGS102,
         dsn = file.path( "../../../lakeDsn/RTP/CH4-102", "WGS102.gpkg"), 
         layer = "WGS102",
         append=FALSE, # this overwrites existing layer,
         driver = "GPKG")


st_layers(file.path( "../../../lakeDsn/RTP/CH4-102", "WGS102.gpkg"))

