# STRATIFIED, UNEQUAL PROBABILITY GRTS DESIGN

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


# LAKE OAHE transitional ZONE (CH4-069)--------
# Design for CH4-069, Oahe Lake, based on NHD lake polygon reflecting subsection of transitional zone

# Read polygon shapefile
eqArea069transitional <- st_read(dsn = "../../../lakeDsn/CIN/CH4-069",
                             layer = "eqArea069transitionalSF")  # shapefile name
plot(eqArea069transitional$geometry) # visualize polygon

# Check CRS, must be equal area for grts function
# EPSG code hard to decipher from output, CRS clearly reported as 'NAD_1983_Albers'
# Coordinate System of NAD 1983 Albers (WKID = 0)
st_crs(eqArea069transitional) 
st_crs(eqArea069transitional) == st_crs(5070) # True

# clean up attributes
eqArea069transitional <- eqArea069transitional %>%
  mutate(lakeName = "Oahe transitional",
         lakeSiteID = "CH4-069") %>%
  select(lakeName, lakeSiteID, Area_km2, section)


# ATTRIBUTES----------
head(eqArea069transitional) # review attributes

# summarize frame by section
temp <- with(eqArea069transitional, tapply(Area_km2, section, sum))
temp <- round(addmargins(temp), 2)
temp


# SET UP FOR GRTS FUNCTION---------
# Call the set.seed function so that the survey designs can be replicateD
set.seed(4447864)

# Create the design list
### We decided to set the number of main sites in the tributary area to 5, since it is a relatively small area (0.4 sq km)
### the unequal probability splits the open water part of the lake into two sections of almost equal area
dsgn069transitional <- list(None = list(panel=c(mainSites=15),
                            seltype="Unequal",
                            caty.n=c("north" = 5, # 
                                     "south" = 5, # 
                                     "mid" = 5),
                            over=20))
# create SpatialDesign object
sites069transitional <- grts(design=dsgn069transitional,
                 DesignID="U", # SU for stratified, unequal
                 type.frame="area",
                 src.frame="sf.object",
                 sf.object=eqArea069transitional,
                 mdcaty="section",
                 shapefile=FALSE) # convert design object to sf, define prj, then write to .shp

# Print the initial six lines of the survey design
head(sites069transitional)


# Print the survey design summary
summary(sites069transitional)


# CRS--------------
class(sites069transitional) # SpatialDesign object
sites069transitional@data <- sites069transitional@data %>% rename(section = mdcaty) # rename
siteseqArea069transitional <- st_as_sf(sites069transitional) # convert to sf object
st_crs(siteseqArea069transitional) # no coordinate reference system?
st_crs(siteseqArea069transitional) = 5070 # inherits from parent object, per Weber.

# project to WGS84 for plotting with leaflet, ArcPad, and writing out lat/long table  (5070 works fine for mapview)
WGS069transitional <- eqArea069transitional %>% st_transform(4326) # 4326 is WGS84, whereas 4269 is NAD83
sitesWGS069transitional <-  siteseqArea069transitional %>% st_transform(4326) # 4326 is WGS84, whereas 4269 is NAD83 

# project to Web Mercator (3857) for use in ArcGIS Pro/geoplatform
merc069transitional <- eqArea069transitional %>% st_transform(3857) 
sitesMerc069transitional <-  siteseqArea069transitional %>% st_transform(3857) 

# BUFFER
# Create a 15m radius buffer around each sampling point
# Per ESRI, buffer should be created in Web Mercator, an equidistant projection, not Albers
sitesMerc069transitionalbuffer <- st_buffer(sitesMerc069transitional, 15) # radius of 15m, diameter of 30m. radius = 45ft = 2 boat lengths
sitesWGS069transitionalbuffer <- sitesMerc069transitionalbuffer %>% st_transform(4326) # for use in ArcPad


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
# ch4_069transitional_lowAll.png works, but ch4_069transitional_lowAllSites.png does not.


# leaflet colors
factpal <- colorFactor(topo.colors(3), WGS069transitional$section)
factpal.points <- colorFactor(palette = c("red", "black"), domain = sitesWGS069transitional$panel)

# All points
m <- leaflet() %>% 
  addProviderTiles(providers$Esri.WorldImagery) %>%
  #setView(st_coordinates(cntr_crds)[1], st_coordinates(cntr_crds)[2], zoom = 15) %>%
  fitBounds(lng1 = min(st_coordinates(sitesWGS069transitional)[,1]),
            lng2 = max(st_coordinates(sitesWGS069transitional)[,1]),
            lat1 = min(st_coordinates(sitesWGS069transitional)[,2]),
            lat2 = max(st_coordinates(sitesWGS069transitional)[,2])) %>%
  addPolygons(data = st_zm(WGS069transitional), # removes z and m values, if present
              color = "black", weight = 1,
              fillOpacity = 1, fillColor = ~factpal(section)) %>%
  addCircleMarkers(data = sitesWGS069transitional,
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
            values = as.character(sitesWGS069transitional$panel),
            title = "Sample sites") %>%
  addLegend(position = "bottomright", pal = factpal, 
            opacity = 1,
            values = as.character(WGS069transitional$section),
            title = "sections") %>%
  addScaleBar()

mapview::mapshot(m, file = "../../../lakeDsn/CIN/CH4-069/069transitional_All.png", 
                 remove_controls = NULL)


# Main points
m <- leaflet() %>% 
  addProviderTiles(providers$Esri.WorldImagery) %>%
  fitBounds(lng1 = min(st_coordinates(sitesWGS069transitional)[,1]),
            lng2 = max(st_coordinates(sitesWGS069transitional)[,1]),
            lat1 = min(st_coordinates(sitesWGS069transitional)[,2]),
            lat2 = max(st_coordinates(sitesWGS069transitional)[,2])) %>%
  addPolygons(data = st_zm(WGS069transitional), # removes z and m values, if present
              color = "black", weight = 1,
              fillOpacity = 1, fillColor = ~factpal(section)) %>%
  addCircleMarkers(data = filter(sitesWGS069transitional, panel == "mainSites"),
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
            values = as.character(WGS069transitional$section),
            title = "sections") %>%
  addScaleBar()

mapview::mapshot(m, file = "../../../lakeDsn/CIN/CH4-069/069transitional_Main.png", 
                 remove_controls = NULL)


# OverSample points
m <- leaflet() %>% 
  addProviderTiles(providers$Esri.WorldImagery) %>%
  fitBounds(lng1 = min(st_coordinates(sitesWGS069transitional)[,1]),
            lng2 = max(st_coordinates(sitesWGS069transitional)[,1]),
            lat1 = min(st_coordinates(sitesWGS069transitional)[,2]),
            lat2 = max(st_coordinates(sitesWGS069transitional)[,2])) %>%
  addPolygons(data = st_zm(WGS069transitional),  # removes z and m values, if present
              color = "black", weight = 1,
              fillOpacity = 1, fillColor = ~factpal(section)) %>%
  addCircleMarkers(data = filter(sitesWGS069transitional, panel == "OverSamp"),
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
            values = as.character(WGS069transitional$section),
            title = "sections") %>%
  addScaleBar()

mapview::mapshot(m, file = "../../../lakeDsn/CIN/CH4-069/069transitional_Over.png", 
                 remove_controls = NULL)



# WRITE OBJECTS TO DISK-----------------
# write out table of sample sites for reference in field.  Must be WGS
write.table(sitesWGS069transitional %>%
              select(panel, siteID, section) %>%
              arrange(panel, section, siteID),
            file = "../../../lakeDsn/CIN/CH4-069/ch4_069transitionalSites.txt",
            row.names = FALSE, sep="\t")


# geopackage
# can write all layers to a geopackage, which behaves much like a geodatabase in ArcGIS.

# Web Mercator for Web Map first

# write out all sites
st_write(obj = sitesMerc069transitional, 
         dsn = file.path( "../../../lakeDsn/CIN/CH4-069", "merc069transitional.gpkg"), 
         layer = "allSitesMerc069transitional", # package appends 'main.' to layer name?
         append=FALSE, # this overwrites existing layer
         driver = "GPKG")

# write out all buffers
st_write(obj = sitesMerc069transitionalbuffer, 
         dsn = file.path( "../../../lakeDsn/CIN/CH4-069", "merc069transitional.gpkg"), 
         layer = "allSitesMerc069transitionalbuffer", # package appends 'main.' to layer name?
         append=FALSE, # this overwrites existing layer
         driver = "GPKG")

# write out main sites
st_write(obj = filter(sitesMerc069transitional, panel == "mainSites"),
         dsn = file.path( "../../../lakeDsn/CIN/CH4-069", "merc069transitional.gpkg"), 
         layer = "mainSitesMerc069transitional",
         append=FALSE, # this overwrites existing layer
         driver = "GPKG")

# write out main site buffers
st_write(obj = filter(sitesMerc069transitionalbuffer, panel == "mainSites"),
         dsn = file.path( "../../../lakeDsn/CIN/CH4-069", "merc069transitional.gpkg"), 
         layer = "mainSitesMerc069transitionalbuffer",
         append=FALSE, # this overwrites existing layer
         driver = "GPKG")

# write out oversample sites
st_write(obj = filter(sitesMerc069transitional, panel == "OverSamp"),
         dsn = file.path( "../../../lakeDsn/CIN/CH4-069", "merc069transitional.gpkg"), 
         layer = "overSampSitesMerc069transitional",
         append=FALSE, # this overwrites existing layer,
         driver = "GPKG")

# write out oversample site buffers
st_write(obj = filter(sitesMerc069transitionalbuffer, panel == "OverSamp"),
         dsn = file.path( "../../../lakeDsn/CIN/CH4-069", "merc069transitional.gpkg"), 
         layer = "overSampSitesMerc069transitionalbuffer",
         append=FALSE, # this overwrites existing layer,
         driver = "GPKG")


# write out lake polygon
st_write(obj = merc069transitional,
         dsn = file.path( "../../../lakeDsn/CIN/CH4-069", "merc069transitional.gpkg"), 
         layer = "merc069transitional",
         append=FALSE, # this overwrites existing layer,
         driver = "GPKG")


st_layers(file.path( "../../../lakeDsn/CIN/CH4-069", "merc069transitional.gpkg"))


# WGS for ArcPad next
# 
# # write out all sites
# st_write(obj = sitesWGS069transitional, 
#          dsn = file.path( "../../../lakeDsn/CIN/CH4-069", "WGS069transitional.gpkg"), 
#          layer = "allSitesWGS069transitional", # package appends 'main.' to layer name?
#          append=FALSE, # this overwrites existing layer
#          driver = "GPKG")
# 
# # write out all buffers
# st_write(obj = sitesWGS069transitionalbuffer, 
#          dsn = file.path( "../../../lakeDsn/CIN/CH4-069", "WGS069transitional.gpkg"), 
#          layer = "sitesWGS069transitionalbuffer", # package appends 'main.' to layer name?
#          append=FALSE, # this overwrites existing layer
#          driver = "GPKG")
# 
# # write out main sites
# st_write(obj = filter(sitesWGS069transitional, panel == "mainSites"),
#          dsn = file.path( "../../../lakeDsn/CIN/CH4-069", "WGS069transitional.gpkg"), 
#          layer = "mainSitesWGS069transitional",
#          append=FALSE, # this overwrites existing layer
#          driver = "GPKG")
# 
# # write out main site buffers
# st_write(obj = filter(sitesWGS069transitionalbuffer, panel == "mainSites"),
#          dsn = file.path( "../../../lakeDsn/CIN/CH4-069", "WGS069transitional.gpkg"), 
#          layer = "mainSitesWGS069transitionalbuffer",
#          append=FALSE, # this overwrites existing layer
#          driver = "GPKG")
# 
# # write out oversample sites
# st_write(obj = filter(sitesWGS069transitional, panel == "OverSamp"),
#          dsn = file.path( "../../../lakeDsn/CIN/CH4-069", "WGS069transitional.gpkg"), 
#          layer = "overSampSitesWGS069transitional",
#          append=FALSE, # this overwrites existing layer,
#          driver = "GPKG")
# 
# # write out oversample site buffers
# st_write(obj = filter(sitesWGS069transitionalbuffer, panel == "OverSamp"),
#          dsn = file.path( "../../../lakeDsn/CIN/CH4-069", "WGS069transitional.gpkg"), 
#          layer = "overSampSitesWGS069transitionalbuffer",
#          append=FALSE, # this overwrites existing layer,
#          driver = "GPKG")
# 
# 
# # write out lake polygon
# st_write(obj = WGS069transitional,
#          dsn = file.path( "../../../lakeDsn/CIN/CH4-069", "WGS069transitional.gpkg"), 
#          layer = "WGS069transitional",
#          append=FALSE, # this overwrites existing layer,
#          driver = "GPKG")
# 
# 
# st_layers(file.path( "../../../lakeDsn/CIN/CH4-069", "WGS069transitional.gpkg"))

