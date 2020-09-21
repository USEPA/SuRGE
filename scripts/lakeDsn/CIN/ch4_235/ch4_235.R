# STRATIFIED, EQUAL PROBABILITY GRTS DESIGN

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


# SPENCER LAKE (ch4-235)--------
# Design for CH4-235, sPENCER Lake, based on NHD lake polygon
# modified to reflect arial images.
# Read polygon shapefile
eqArea235 <- st_read(dsn = "../../../lakeDsn/CIN/CH4-235",
                             layer = "eqArea235")  # shapefile name
plot(eqArea235$geometry) # visualize polygon

# Check CRS, must be equal area for grts function
# EPSG code hard to decipher from output, CRS clearly reported as 'NAD_1983_Albers'
# Coordinate System of NAD 1983 Albers (WKID = 0)
st_crs(eqArea235) 
st_crs(eqArea235) == st_crs(5070) # True

# clean up attributes
eqArea235 <- eqArea235 %>%
  mutate(lakeName = "Sylvan Lake",
         lakeSiteID = "ch4-235") %>%
  select(lakeName, lakeSiteID, Area_km2, strata)


# ATTRIBUTES----------
head(eqArea235) # review attributes

# summarize frame by strata
temp <- with(eqArea235, tapply(Area_km2, strata, sum))
temp <- round(addmargins(temp), 2)
temp


# SET UP FOR GRTS FUNCTION---------
# Call the set.seed function so that the survey designs can be replicated
set.seed(4447864)

# Create the design list
dsgn235 <- list("west" = list(panel=c(mainSites=4),
                                            seltype="Equal",
                                            over=10),
                        "east"=list(panel=c(mainSites=11), 
                                    seltype="Equal",
                                    over=10))
# create SpatialDesign object
sites235 <- grts(design=dsgn235,
                 DesignID="S", # S for stratified
                 type.frame="area",
                 src.frame="sf.object",
                 sf.object=eqArea235,
                 stratum="strata",
                 shapefile=FALSE) # convert design object to sf, define prj, then write to .shp

# Print the initial six lines of the survey design
head(sites235)


# Print the survey design summary
summary(sites235)


# CRS--------------
class(sites235) # SpatialDesign object
sitesEqArea235 <- st_as_sf(sites235) # convert to sf object
st_crs(sitesEqArea235) # no coordinate reference system?
st_crs(sitesEqArea235) = 5070 # inherits from parent object, per Weber.

# project to WGS84 for plotting with leaflet, ArcPad, and writing out lat/long table  (5070 works fine for mapview)
WGS235 <- eqArea235 %>% st_transform(4326) # 4326 is WGS84, whereas 4269 is NAD83
sitesWGS235 <-  sitesEqArea235 %>% st_transform(4326) # 4326 is WGS84, whereas 4269 is NAD83 

# project to Web Mercator (3857) for use in ArcGIS Pro/geoplatform
merc235 <- eqArea235 %>% st_transform(3857) 
sitesMerc235 <-  sitesEqArea235 %>% st_transform(3857) 

# BUFFER
# Create a 15m radius buffer around each sampling point
# Per ESRI, buffer should be created in Web Mercator, an equidistant projection, not Albers
sitesMerc235buffer <- st_buffer(sitesMerc235, 15) # radius of 15m, diameter of 30m. radius = 45ft = 2 boat lengths
sitesWGS235buffer <- sitesMerc235buffer %>% st_transform(4326) # for use in ArcPad


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
# CH4-235All.png works, but CH4-235AllSites.png does not.


# leaflet colors
factpal <- colorFactor(topo.colors(2), WGS235$strata)
factpal.points <- colorFactor(palette = c("red", "black"), domain = sitesWGS235$panel)

# All points
m <- leaflet() %>% 
  addProviderTiles(providers$Esri.WorldImagery) %>%
  #setView(st_coordinates(cntr_crds)[1], st_coordinates(cntr_crds)[2], zoom = 15) %>%
  fitBounds(lng1 = min(st_coordinates(sitesWGS235)[,1]),
            lng2 = max(st_coordinates(sitesWGS235)[,1]),
            lat1 = min(st_coordinates(sitesWGS235)[,2]),
            lat2 = max(st_coordinates(sitesWGS235)[,2])) %>%
  addPolygons(data = st_zm(WGS235), # removes z and m values, if present
              color = "black", weight = 1,
              fillOpacity = 1, fillColor = ~factpal(strata)) %>%
  addCircleMarkers(data = sitesWGS235,
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
            values = as.character(sitesWGS235$panel),
            title = "Sample sites") %>%
  addLegend(position = "bottomright", pal = factpal, 
            opacity = 1,
            values = as.character(WGS235$strata),
            title = "strata") %>%
  addScaleBar()

mapview::mapshot(m, file = "../../../lakeDsn/CIN/CH4-235/235_All.png", 
                 remove_controls = NULL)


# Main points
m <- leaflet() %>% 
  addProviderTiles(providers$Esri.WorldImagery) %>%
  fitBounds(lng1 = min(st_coordinates(sitesWGS235)[,1]),
            lng2 = max(st_coordinates(sitesWGS235)[,1]),
            lat1 = min(st_coordinates(sitesWGS235)[,2]),
            lat2 = max(st_coordinates(sitesWGS235)[,2])) %>%
  addPolygons(data = st_zm(WGS235), # removes z and m values, if present
              color = "black", weight = 1,
              fillOpacity = 1, fillColor = ~factpal(strata)) %>%
  addCircleMarkers(data = filter(sitesWGS235, panel == "mainSites"),
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
            values = as.character(WGS235$strata),
            title = "strata") %>%
  addScaleBar()

mapview::mapshot(m, file = "../../../lakeDsn/CIN/CH4-235/235_Main.png", 
                 remove_controls = NULL)


# OverSample points
m <- leaflet() %>% 
  addProviderTiles(providers$Esri.WorldImagery) %>%
  fitBounds(lng1 = min(st_coordinates(sitesWGS235)[,1]),
            lng2 = max(st_coordinates(sitesWGS235)[,1]),
            lat1 = min(st_coordinates(sitesWGS235)[,2]),
            lat2 = max(st_coordinates(sitesWGS235)[,2])) %>%
  addPolygons(data = st_zm(WGS235),  # removes z and m values, if present
              color = "black", weight = 1,
              fillOpacity = 1, fillColor = ~factpal(strata)) %>%
  addCircleMarkers(data = filter(sitesWGS235, panel == "OverSamp"),
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
            values = as.character(WGS235$strata),
            title = "strata") %>%
  addScaleBar()

mapview::mapshot(m, file = "../../../lakeDsn/CIN/CH4-235/235_Over.png", 
                 remove_controls = NULL)



# WRITE OBJECTS TO DISK-----------------
# write out table of sample sites for reference in field.  Must be WGS
write.table(sitesWGS235 %>%
              select(panel, siteID, stratum) %>%
              arrange(panel, stratum, siteID),
            file = "../../../lakeDsn/CIN/CH4-235/ch4-235Sites.txt",
            row.names = FALSE, sep="\t")


# geopackage
# can write all layers to a geopackage, which behaves much like a geodatabase in ArcGIS.

# Web Mercator for Web Map first

# write out all sites
st_write(obj = sitesMerc235, 
         dsn = file.path( "../../../lakeDsn/CIN/CH4-235", "merc235.gpkg"), 
         layer = "allSitesMerc235", # package appends 'main.' to layer name?
         append=FALSE, # this overwrites existing layer
         driver = "GPKG")

# write out all buffers
st_write(obj = sitesMerc235buffer, 
         dsn = file.path( "../../../lakeDsn/CIN/CH4-235", "merc235.gpkg"), 
         layer = "allSitesMerc235buffer", # package appends 'main.' to layer name?
         append=FALSE, # this overwrites existing layer
         driver = "GPKG")

# write out main sites
st_write(obj = filter(sitesMerc235, panel == "mainSites"),
         dsn = file.path( "../../../lakeDsn/CIN/CH4-235", "merc235.gpkg"), 
         layer = "mainSitesMerc235",
         append=FALSE, # this overwrites existing layer
         driver = "GPKG")

# write out main site buffers
st_write(obj = filter(sitesMerc235buffer, panel == "mainSites"),
         dsn = file.path( "../../../lakeDsn/CIN/CH4-235", "merc235.gpkg"), 
         layer = "mainSitesMerc235buffer",
         append=FALSE, # this overwrites existing layer
         driver = "GPKG")

# write out oversample sites
st_write(obj = filter(sitesMerc235, panel == "OverSamp"),
         dsn = file.path( "../../../lakeDsn/CIN/CH4-235", "merc235.gpkg"), 
         layer = "overSampSitesMerc235",
         append=FALSE, # this overwrites existing layer,
         driver = "GPKG")

# write out oversample site buffers
st_write(obj = filter(sitesMerc235buffer, panel == "OverSamp"),
         dsn = file.path( "../../../lakeDsn/CIN/CH4-235", "merc235.gpkg"), 
         layer = "overSampSitesMerc235buffer",
         append=FALSE, # this overwrites existing layer,
         driver = "GPKG")


# write out lake polygon
st_write(obj = merc235,
         dsn = file.path( "../../../lakeDsn/CIN/CH4-235", "merc235.gpkg"), 
         layer = "merc235",
         append=FALSE, # this overwrites existing layer,
         driver = "GPKG")


st_layers(file.path( "../../../lakeDsn/CIN/CH4-235", "merc235.gpkg"))


# WGS for ArcPad next

# # write out all sites
# st_write(obj = sitesWGS235, 
#          dsn = file.path( "../../../lakeDsn/CIN/CH4-235", "WGS235.gpkg"), 
#          layer = "allSitesWGS235", # package appends 'main.' to layer name?
#          append=FALSE, # this overwrites existing layer
#          driver = "GPKG")
# 
# # write out all buffers
# st_write(obj = sitesWGS235buffer, 
#          dsn = file.path( "../../../lakeDsn/CIN/CH4-235", "WGS235.gpkg"), 
#          layer = "sitesWGS235buffer", # package appends 'main.' to layer name?
#          append=FALSE, # this overwrites existing layer
#          driver = "GPKG")

# write out main sites
st_write(obj = filter(sitesWGS235, panel == "mainSites"),
         dsn = file.path( "../../../lakeDsn/CIN/CH4-235", "WGS235.gpkg"), 
         layer = "mainSitesWGS235",
         append=FALSE, # this overwrites existing layer
         driver = "GPKG")

# write out main site buffers
st_write(obj = filter(sitesWGS235buffer, panel == "mainSites"),
         dsn = file.path( "../../../lakeDsn/CIN/CH4-235", "WGS235.gpkg"), 
         layer = "mainSitesWGS235buffer",
         append=FALSE, # this overwrites existing layer
         driver = "GPKG")

# write out oversample sites
st_write(obj = filter(sitesWGS235, panel == "OverSamp"),
         dsn = file.path( "../../../lakeDsn/CIN/CH4-235", "WGS235.gpkg"), 
         layer = "overSampSitesWGS235",
         append=FALSE, # this overwrites existing layer,
         driver = "GPKG")

# write out oversample site buffers
st_write(obj = filter(sitesWGS235buffer, panel == "OverSamp"),
         dsn = file.path( "../../../lakeDsn/CIN/CH4-235", "WGS235.gpkg"), 
         layer = "overSampSitesWGS235buffer",
         append=FALSE, # this overwrites existing layer,
         driver = "GPKG")


# write out lake polygon
st_write(obj = WGS235,
         dsn = file.path( "../../../lakeDsn/CIN/CH4-235", "WGS235.gpkg"), 
         layer = "WGS235",
         append=FALSE, # this overwrites existing layer,
         driver = "GPKG")


st_layers(file.path( "../../../lakeDsn/CIN/CH4-235", "WGS235.gpkg"))

