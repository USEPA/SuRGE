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


# Francis Case LACUSTRINE ZONE (CH4-070)--------
# Design for CH4-070, Francis Case, based on NHD lake polygon reflecting subsection of lacustrine zone

# Read polygon shapefile
eqArea070lacustrine <- st_read(dsn = "../../../lakeDsn/CIN/CH4-070",
                             layer = "eqArea070lacustrineSF")  # shapefile name
plot(eqArea070lacustrine$geometry) # visualize polygon

# Check CRS, must be equal area for grts function
# EPSG code hard to decipher from output, CRS clearly reported as 'NAD_1983_Albers'
# Coordinate System of NAD 1983 Albers (WKID = 0)
st_crs(eqArea070lacustrine) 
st_crs(eqArea070lacustrine) == st_crs(5070) # True

# clean up attributes
eqArea070lacustrine <- eqArea070lacustrine %>%
  mutate(lakeName = "Francis Case lacustrine",
         lakeSiteID = "CH4-070") %>%
  select(lakeName, lakeSiteID, Area_km2, section)


# ATTRIBUTES----------
head(eqArea070lacustrine) # review attributes

# summarize frame by section
temp <- with(eqArea070lacustrine, tapply(Area_km2, section, sum))
temp <- round(addmargins(temp), 2)
temp


# SET UP FOR GRTS FUNCTION---------
# Call the set.seed function so that the survey designs can be replicateD
set.seed(4447864)

# Create the design list
### We decided to set the number of main sites in the tributary area to 5, since it is a relatively small area (0.4 sq km)
### the unequal probability splits the open water part of the lake into two sections of almost equal area
dsgn070lacustrine <- list(None = list(panel=c(mainSites=15),
                            seltype="Unequal",
                            caty.n=c("1" = 4, # 
                                     "2" = 4, # 
                                     "3" = 4,
                                     "4" = 3),
                            over=20))

# create SpatialDesign object
sites070lacustrine <- grts(design=dsgn070lacustrine,
                 DesignID="U", # SU for stratified, unequal
                 type.frame="area",
                 src.frame="sf.object",
                 sf.object=eqArea070lacustrine,
                 mdcaty="section",
                 shapefile=FALSE) # convert design object to sf, define prj, then write to .shp

# Print the initial six lines of the survey design
head(sites070lacustrine)


# Print the survey design summary
summary(sites070lacustrine)


# CRS--------------
class(sites070lacustrine) # SpatialDesign object
sites070lacustrine@data <- sites070lacustrine@data %>% rename(section = mdcaty) # rename
siteseqArea070lacustrine <- st_as_sf(sites070lacustrine) # convert to sf object
st_crs(siteseqArea070lacustrine) # no coordinate reference system?
st_crs(siteseqArea070lacustrine) = 5070 # inherits from parent object, per Weber.

# project to WGS84 for plotting with leaflet, ArcPad, and writing out lat/long table  (5070 works fine for mapview)
WGS070lacustrine <- eqArea070lacustrine %>% st_transform(4326) # 4326 is WGS84, whereas 4269 is NAD83
sitesWGS070lacustrine <-  siteseqArea070lacustrine %>% st_transform(4326) # 4326 is WGS84, whereas 4269 is NAD83 

# project to Web Mercator (3857) for use in ArcGIS Pro/geoplatform
merc070lacustrine <- eqArea070lacustrine %>% st_transform(3857) 
sitesMerc070lacustrine <-  siteseqArea070lacustrine %>% st_transform(3857) 

# BUFFER
# Create a 15m radius buffer around each sampling point
# Per ESRI, buffer should be created in Web Mercator, an equidistant projection, not Albers
sitesMerc070lacustrinebuffer <- st_buffer(sitesMerc070lacustrine, 15) # radius of 15m, diameter of 30m. radius = 45ft = 2 boat lengths
sitesWGS070lacustrinebuffer <- sitesMerc070lacustrinebuffer %>% st_transform(4326) # for use in ArcPad


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
# ch4_070lacustrine_lowAll.png works, but ch4_070lacustrine_lowAllSites.png does not.


# leaflet colors
factpal <- colorFactor(topo.colors(3), WGS070lacustrine$section)
factpal.points <- colorFactor(palette = c("red", "black"), domain = sitesWGS070lacustrine$panel)

# All points
m <- leaflet() %>% 
  addProviderTiles(providers$Esri.WorldImagery) %>%
  #setView(st_coordinates(cntr_crds)[1], st_coordinates(cntr_crds)[2], zoom = 15) %>%
  fitBounds(lng1 = min(st_coordinates(sitesWGS070lacustrine)[,1]),
            lng2 = max(st_coordinates(sitesWGS070lacustrine)[,1]),
            lat1 = min(st_coordinates(sitesWGS070lacustrine)[,2]),
            lat2 = max(st_coordinates(sitesWGS070lacustrine)[,2])) %>%
  addPolygons(data = st_zm(WGS070lacustrine), # removes z and m values, if present
              color = "black", weight = 1,
              fillOpacity = 1, fillColor = ~factpal(section)) %>%
  addCircleMarkers(data = sitesWGS070lacustrine,
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
            values = as.character(sitesWGS070lacustrine$panel),
            title = "Sample sites") %>%
  addLegend(position = "bottomright", pal = factpal, 
            opacity = 1,
            values = as.character(WGS070lacustrine$section),
            title = "sections") %>%
  addScaleBar()

mapview::mapshot(m, file = "../../../lakeDsn/CIN/CH4-070/070lacustrine_All.png", 
                 remove_controls = NULL)


# Main points
m <- leaflet() %>% 
  addProviderTiles(providers$Esri.WorldImagery) %>%
  fitBounds(lng1 = min(st_coordinates(sitesWGS070lacustrine)[,1]),
            lng2 = max(st_coordinates(sitesWGS070lacustrine)[,1]),
            lat1 = min(st_coordinates(sitesWGS070lacustrine)[,2]),
            lat2 = max(st_coordinates(sitesWGS070lacustrine)[,2])) %>%
  addPolygons(data = st_zm(WGS070lacustrine), # removes z and m values, if present
              color = "black", weight = 1,
              fillOpacity = 1, fillColor = ~factpal(section)) %>%
  addCircleMarkers(data = filter(sitesWGS070lacustrine, panel == "mainSites"),
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
            values = as.character(WGS070lacustrine$section),
            title = "sections") %>%
  addScaleBar()

mapview::mapshot(m, file = "../../../lakeDsn/CIN/CH4-070/070lacustrine_Main.png", 
                 remove_controls = NULL)


# OverSample points
m <- leaflet() %>% 
  addProviderTiles(providers$Esri.WorldImagery) %>%
  fitBounds(lng1 = min(st_coordinates(sitesWGS070lacustrine)[,1]),
            lng2 = max(st_coordinates(sitesWGS070lacustrine)[,1]),
            lat1 = min(st_coordinates(sitesWGS070lacustrine)[,2]),
            lat2 = max(st_coordinates(sitesWGS070lacustrine)[,2])) %>%
  addPolygons(data = st_zm(WGS070lacustrine),  # removes z and m values, if present
              color = "black", weight = 1,
              fillOpacity = 1, fillColor = ~factpal(section)) %>%
  addCircleMarkers(data = filter(sitesWGS070lacustrine, panel == "OverSamp"),
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
            values = as.character(WGS070lacustrine$section),
            title = "sections") %>%
  addScaleBar()

mapview::mapshot(m, file = "../../../lakeDsn/CIN/CH4-070/070lacustrine_Over.png", 
                 remove_controls = NULL)



# WRITE OBJECTS TO DISK-----------------
# write out table of sample sites for reference in field.  Must be WGS
write.table(sitesWGS070lacustrine %>%
              select(panel, siteID, section) %>%
              arrange(panel, section, siteID),
            file = "../../../lakeDsn/CIN/CH4-070/ch4_070lacustrineSites.txt",
            row.names = FALSE, sep="\t")


# geopackage
# can write all layers to a geopackage, which behaves much like a geodatabase in ArcGIS.

# Web Mercator for Web Map first

# write out all sites
st_write(obj = sitesMerc070lacustrine, 
         dsn = file.path( "../../../lakeDsn/CIN/CH4-070", "merc070lacustrine.gpkg"), 
         layer = "allSitesMerc070lacustrine", # package appends 'main.' to layer name?
         append=FALSE, # this overwrites existing layer
         driver = "GPKG")

# write out all buffers
st_write(obj = sitesMerc070lacustrinebuffer, 
         dsn = file.path( "../../../lakeDsn/CIN/CH4-070", "merc070lacustrine.gpkg"), 
         layer = "allSitesMerc070lacustrinebuffer", # package appends 'main.' to layer name?
         append=FALSE, # this overwrites existing layer
         driver = "GPKG")

# write out main sites
st_write(obj = filter(sitesMerc070lacustrine, panel == "mainSites"),
         dsn = file.path( "../../../lakeDsn/CIN/CH4-070", "merc070lacustrine.gpkg"), 
         layer = "mainSitesMerc070lacustrine",
         append=FALSE, # this overwrites existing layer
         driver = "GPKG")

# write out main site buffers
st_write(obj = filter(sitesMerc070lacustrinebuffer, panel == "mainSites"),
         dsn = file.path( "../../../lakeDsn/CIN/CH4-070", "merc070lacustrine.gpkg"), 
         layer = "mainSitesMerc070lacustrinebuffer",
         append=FALSE, # this overwrites existing layer
         driver = "GPKG")

# write out oversample sites
st_write(obj = filter(sitesMerc070lacustrine, panel == "OverSamp"),
         dsn = file.path( "../../../lakeDsn/CIN/CH4-070", "merc070lacustrine.gpkg"), 
         layer = "overSampSitesMerc070lacustrine",
         append=FALSE, # this overwrites existing layer,
         driver = "GPKG")

# write out oversample site buffers
st_write(obj = filter(sitesMerc070lacustrinebuffer, panel == "OverSamp"),
         dsn = file.path( "../../../lakeDsn/CIN/CH4-070", "merc070lacustrine.gpkg"), 
         layer = "overSampSitesMerc070lacustrinebuffer",
         append=FALSE, # this overwrites existing layer,
         driver = "GPKG")


# write out lake polygon
st_write(obj = merc070lacustrine,
         dsn = file.path( "../../../lakeDsn/CIN/CH4-070", "merc070lacustrine.gpkg"), 
         layer = "merc070lacustrine",
         append=FALSE, # this overwrites existing layer,
         driver = "GPKG")


st_layers(file.path( "../../../lakeDsn/CIN/CH4-070", "merc070lacustrine.gpkg"))


# WGS for ArcPad next
# 
# # write out all sites
# st_write(obj = sitesWGS070lacustrine, 
#          dsn = file.path( "../../../lakeDsn/CIN/CH4-070", "WGS070lacustrine.gpkg"), 
#          layer = "allSitesWGS070lacustrine", # package appends 'main.' to layer name?
#          append=FALSE, # this overwrites existing layer
#          driver = "GPKG")
# 
# # write out all buffers
# st_write(obj = sitesWGS070lacustrinebuffer, 
#          dsn = file.path( "../../../lakeDsn/CIN/CH4-070", "WGS070lacustrine.gpkg"), 
#          layer = "sitesWGS070lacustrinebuffer", # package appends 'main.' to layer name?
#          append=FALSE, # this overwrites existing layer
#          driver = "GPKG")
# 
# # write out main sites
# st_write(obj = filter(sitesWGS070lacustrine, panel == "mainSites"),
#          dsn = file.path( "../../../lakeDsn/CIN/CH4-070", "WGS070lacustrine.gpkg"), 
#          layer = "mainSitesWGS070lacustrine",
#          append=FALSE, # this overwrites existing layer
#          driver = "GPKG")
# 
# # write out main site buffers
# st_write(obj = filter(sitesWGS070lacustrinebuffer, panel == "mainSites"),
#          dsn = file.path( "../../../lakeDsn/CIN/CH4-070", "WGS070lacustrine.gpkg"), 
#          layer = "mainSitesWGS070lacustrinebuffer",
#          append=FALSE, # this overwrites existing layer
#          driver = "GPKG")
# 
# # write out oversample sites
# st_write(obj = filter(sitesWGS070lacustrine, panel == "OverSamp"),
#          dsn = file.path( "../../../lakeDsn/CIN/CH4-070", "WGS070lacustrine.gpkg"), 
#          layer = "overSampSitesWGS070lacustrine",
#          append=FALSE, # this overwrites existing layer,
#          driver = "GPKG")
# 
# # write out oversample site buffers
# st_write(obj = filter(sitesWGS070lacustrinebuffer, panel == "OverSamp"),
#          dsn = file.path( "../../../lakeDsn/CIN/CH4-070", "WGS070lacustrine.gpkg"), 
#          layer = "overSampSitesWGS070lacustrinebuffer",
#          append=FALSE, # this overwrites existing layer,
#          driver = "GPKG")
# 
# 
# # write out lake polygon
# st_write(obj = WGS070lacustrine,
#          dsn = file.path( "../../../lakeDsn/CIN/CH4-070", "WGS070lacustrine.gpkg"), 
#          layer = "WGS070lacustrine",
#          append=FALSE, # this overwrites existing layer,
#          driver = "GPKG")
# 
# 
# st_layers(file.path( "../../../lakeDsn/CIN/CH4-070", "WGS070lacustrine.gpkg"))

