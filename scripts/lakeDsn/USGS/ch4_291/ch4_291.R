# UNSTRATIFIED, UNEQUAL PROBABILITY GRTS DESIGN


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


# Upper San Leandro Reservoir (ch4-291)--------
# Design for CH4-291, Upper San Leandro Reservoir, based on July 2017 Landsat image.
# Local managers felt it was a good approximation of expected August 2021 conditions.

# Read polygon shapefile
eqArea291 <- st_read(dsn = "../../../lakeDsn/USGS/CH4-291",
                             layer = "eqArea291")  # shapefile name
plot(eqArea291$geometry) # visualize polygon

# Check CRS, must be equal area for grts function
# EPSG code hard to decipher from output, CRS clearly reported as 'NAD_1983_Albers'
# Coordinate System of NAD 1983 Albers (WKID = 0)
st_crs(eqArea291) 
st_crs(eqArea291) == st_crs(5070) # True

# clean up attributes
eqArea291 <- eqArea291 %>%
  mutate(lakeName = "Upper San Leandro Reservoir",
         lakeSiteID = "ch4-291") %>%
  select(lakeName, lakeSiteID, Area_km2, section)


# ATTRIBUTES----------
head(eqArea291) # review attributes

# summarize frame by section
temp <- with(eqArea291, tapply(Area_km2, section, sum))
temp <- round(addmargins(temp), 2)
temp


# SET UP FOR GRTS FUNCTION---------
# Call the set.seed function so that the survey designs can be replicateD
set.seed(4447864)

# Create the design list
### We decided to set the number of main sites in the tributary area to 5, since it is a relatively small area (0.4 sq km)
### the unequal probability splits the open water part of the lake into two sections of almost equal area
dsgn291 <- list(None = list(panel=c(mainSites=15),
                            seltype="Unequal",
                            caty.n=c("north" = 9, # 
                                     "south" = 6), # 
                            over=20))
# create SpatialDesign object
sites291 <- grts(design=dsgn291,
                 DesignID="U", # U for unstratified, unequal
                 type.frame="area",
                 src.frame="sf.object",
                 sf.object=eqArea291,
                 mdcaty="section",
                 shapefile=FALSE) # convert design object to sf, define prj, then write to .shp

# Print the initial six lines of the survey design
head(sites291)


# Print the survey design summary
summary(sites291)


# CRS--------------
class(sites291) # SpatialDesign object
sites291@data <- sites291@data %>% rename(section = mdcaty) # rename
sitesEqArea291 <- st_as_sf(sites291) # convert to sf object
st_crs(sitesEqArea291) # no coordinate reference system?
st_crs(sitesEqArea291) = 5070 # inherits from parent object, per Weber.

# project to WGS84 for plotting with leaflet, ArcPad, and writing out lat/long table  (5070 works fine for mapview)
WGS291 <- eqArea291 %>% st_transform(4326) # 4326 is WGS84, whereas 4269 is NAD83
sitesWGS291 <-  sitesEqArea291 %>% st_transform(4326) # 4326 is WGS84, whereas 4269 is NAD83 

# project to Web Mercator (3857) for use in ArcGIS Pro/geoplatform
merc291 <- eqArea291 %>% st_transform(3857) 
sitesMerc291 <-  sitesEqArea291 %>% st_transform(3857) 

# BUFFER
# Create a 15m radius buffer around each sampling point
# Per ESRI, buffer should be created in Web Mercator, an equidistant projection, not Albers
sitesMerc291buffer <- st_buffer(sitesMerc291, 15) # radius of 15m, diameter of 30m. radius = 45ft = 2 boat lengths
sitesWGS291buffer <- sitesMerc291buffer %>% st_transform(4326) # for use in ArcPad


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
# ch4_291_lowAll.png works, but ch4_291_lowAllSites.png does not.


# leaflet colors
factpal <- colorFactor(topo.colors(3), WGS291$section)
factpal.points <- colorFactor(palette = c("red", "black"), domain = sitesWGS291$panel)

# All points
m <- leaflet() %>% 
  addProviderTiles(providers$Esri.WorldImagery) %>%
  #setView(st_coordinates(cntr_crds)[1], st_coordinates(cntr_crds)[2], zoom = 15) %>%
  fitBounds(lng1 = min(st_coordinates(sitesWGS291)[,1]),
            lng2 = max(st_coordinates(sitesWGS291)[,1]),
            lat1 = min(st_coordinates(sitesWGS291)[,2]),
            lat2 = max(st_coordinates(sitesWGS291)[,2])) %>%
  addPolygons(data = st_zm(WGS291), # removes z and m values, if present
              color = "black", weight = 1,
              fillOpacity = 1, fillColor = ~factpal(section)) %>%
  addCircleMarkers(data = sitesWGS291,
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
            values = as.character(sitesWGS291$panel),
            title = "Sample sites") %>%
  addLegend(position = "bottomright", pal = factpal, 
            opacity = 1,
            values = as.character(WGS291$section),
            title = "sections") %>%
  addScaleBar()

mapview::mapshot(m, file = "../../../lakeDsn/USGS/CH4-291/291_All.png", 
                 remove_controls = NULL)


# Main points
m <- leaflet() %>% 
  addProviderTiles(providers$Esri.WorldImagery) %>%
  fitBounds(lng1 = min(st_coordinates(sitesWGS291)[,1]),
            lng2 = max(st_coordinates(sitesWGS291)[,1]),
            lat1 = min(st_coordinates(sitesWGS291)[,2]),
            lat2 = max(st_coordinates(sitesWGS291)[,2])) %>%
  addPolygons(data = st_zm(WGS291), # removes z and m values, if present
              color = "black", weight = 1,
              fillOpacity = 1, fillColor = ~factpal(section)) %>%
  addCircleMarkers(data = filter(sitesWGS291, panel == "mainSites"),
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
            values = as.character(WGS291$section),
            title = "sections") %>%
  addScaleBar()

mapview::mapshot(m, file = "../../../lakeDsn/USGS/CH4-291/291_Main.png", 
                 remove_controls = NULL)


# OverSample points
m <- leaflet() %>% 
  addProviderTiles(providers$Esri.WorldImagery) %>%
  fitBounds(lng1 = min(st_coordinates(sitesWGS291)[,1]),
            lng2 = max(st_coordinates(sitesWGS291)[,1]),
            lat1 = min(st_coordinates(sitesWGS291)[,2]),
            lat2 = max(st_coordinates(sitesWGS291)[,2])) %>%
  addPolygons(data = st_zm(WGS291),  # removes z and m values, if present
              color = "black", weight = 1,
              fillOpacity = 1, fillColor = ~factpal(section)) %>%
  addCircleMarkers(data = filter(sitesWGS291, panel == "OverSamp"),
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
            values = as.character(WGS291$section),
            title = "sections") %>%
  addScaleBar()

mapview::mapshot(m, file = "../../../lakeDsn/USGS/CH4-291/291_Over.png", 
                 remove_controls = NULL)



# WRITE OBJECTS TO DISK-----------------
# write out table of sample sites for reference in field.  Must be WGS
write.table(sitesWGS291 %>%
              select(panel, siteID, section) %>%
              arrange(panel, section, siteID),
            file = "../../../lakeDsn/USGS/CH4-291/ch4_291Sites.txt",
            row.names = FALSE, sep="\t")


# geopackage
# can write all layers to a geopackage, which behaves much like a geodatabase in ArcGIS.

# Web Mercator for Web Map first

# write out all sites
st_write(obj = sitesMerc291, 
         dsn = file.path( "../../../lakeDsn/USGS/CH4-291", "merc291.gpkg"), 
         layer = "allSitesMerc291", # package appends 'main.' to layer name?
         append=FALSE, # this overwrites existing layer
         driver = "GPKG")

# write out all buffers
st_write(obj = sitesMerc291buffer, 
         dsn = file.path( "../../../lakeDsn/USGS/CH4-291", "merc291.gpkg"), 
         layer = "allSitesMerc291buffer", # package appends 'main.' to layer name?
         append=FALSE, # this overwrites existing layer
         driver = "GPKG")

# write out main sites
st_write(obj = filter(sitesMerc291, panel == "mainSites"),
         dsn = file.path( "../../../lakeDsn/USGS/CH4-291", "merc291.gpkg"), 
         layer = "mainSitesMerc291",
         append=FALSE, # this overwrites existing layer
         driver = "GPKG")

# write out main site buffers
st_write(obj = filter(sitesMerc291buffer, panel == "mainSites"),
         dsn = file.path( "../../../lakeDsn/USGS/CH4-291", "merc291.gpkg"), 
         layer = "mainSitesMerc291buffer",
         append=FALSE, # this overwrites existing layer
         driver = "GPKG")

# write out oversample sites
st_write(obj = filter(sitesMerc291, panel == "OverSamp"),
         dsn = file.path( "../../../lakeDsn/USGS/CH4-291", "merc291.gpkg"), 
         layer = "overSampSitesMerc291",
         append=FALSE, # this overwrites existing layer,
         driver = "GPKG")

# write out oversample site buffers
st_write(obj = filter(sitesMerc291buffer, panel == "OverSamp"),
         dsn = file.path( "../../../lakeDsn/USGS/CH4-291", "merc291.gpkg"), 
         layer = "overSampSitesMerc291buffer",
         append=FALSE, # this overwrites existing layer,
         driver = "GPKG")


# write out lake polygon
st_write(obj = merc291,
         dsn = file.path( "../../../lakeDsn/USGS/CH4-291", "merc291.gpkg"), 
         layer = "merc291",
         append=FALSE, # this overwrites existing layer,
         driver = "GPKG")


st_layers(file.path( "../../../lakeDsn/USGS/CH4-291", "merc291.gpkg"))


# WGS for ArcPad next

# write out all sites
st_write(obj = sitesWGS291, 
         dsn = file.path( "../../../lakeDsn/USGS/CH4-291", "WGS291.gpkg"), 
         layer = "allSitesWGS291", # package appends 'main.' to layer name?
         append=FALSE, # this overwrites existing layer
         driver = "GPKG")

# write out all buffers
st_write(obj = sitesWGS291buffer, 
         dsn = file.path( "../../../lakeDsn/USGS/CH4-291", "WGS291.gpkg"), 
         layer = "sitesWGS291buffer", # package appends 'main.' to layer name?
         append=FALSE, # this overwrites existing layer
         driver = "GPKG")

# write out main sites
st_write(obj = filter(sitesWGS291, panel == "mainSites"),
         dsn = file.path( "../../../lakeDsn/USGS/CH4-291", "WGS291.gpkg"), 
         layer = "mainSitesWGS291",
         append=FALSE, # this overwrites existing layer
         driver = "GPKG")

# write out main site buffers
st_write(obj = filter(sitesWGS291buffer, panel == "mainSites"),
         dsn = file.path( "../../../lakeDsn/USGS/CH4-291", "WGS291.gpkg"), 
         layer = "mainSitesWGS291buffer",
         append=FALSE, # this overwrites existing layer
         driver = "GPKG")

# write out oversample sites
st_write(obj = filter(sitesWGS291, panel == "OverSamp"),
         dsn = file.path( "../../../lakeDsn/USGS/CH4-291", "WGS291.gpkg"), 
         layer = "overSampSitesWGS291",
         append=FALSE, # this overwrites existing layer,
         driver = "GPKG")

# write out oversample site buffers
st_write(obj = filter(sitesWGS291buffer, panel == "OverSamp"),
         dsn = file.path( "../../../lakeDsn/USGS/CH4-291", "WGS291.gpkg"), 
         layer = "overSampSitesWGS291buffer",
         append=FALSE, # this overwrites existing layer,
         driver = "GPKG")


# write out lake polygon
st_write(obj = WGS291,
         dsn = file.path( "../../../lakeDsn/USGS/CH4-291", "WGS291.gpkg"), 
         layer = "WGS291",
         append=FALSE, # this overwrites existing layer,
         driver = "GPKG")


st_layers(file.path( "../../../lakeDsn/USGS/CH4-291", "WGS291.gpkg"))

