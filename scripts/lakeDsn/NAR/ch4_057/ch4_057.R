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


# SLATERSVILLE RESERVOIR (ch4-057)--------
# Design for CH4-057, Slatersville Reservoir, based on NHD lake polygon
# modified to reflect arial images.
# Read polygon shapefile
eqArea057 <- st_read(dsn = "../../../lakeDsn/NAR/CH4-057",
                             layer = "eqArea057")  # shapefile name
plot(eqArea057$geometry) # visualize polygon

# Check CRS, must be equal area for grts function
# EPSG code hard to decipher from output, CRS clearly reported as 'NAD_1983_Albers'
# Coordinate System of NAD 1983 Albers (WKID = 0)
st_crs(eqArea057) 
st_crs(eqArea057) == st_crs(5070) # True

# clean up attributes
eqArea057 <- eqArea057 %>%
  mutate(lakeName = "Slatersville Reservoir",
         lakeSiteID = "ch4-057") %>%
  select(lakeName, lakeSiteID, Area_km2, strata)


# ATTRIBUTES----------
head(eqArea057) # review attributes

# summarize frame by strata
temp <- with(eqArea057, tapply(Area_km2, strata, sum))
temp <- round(addmargins(temp), 2)
temp


# SET UP FOR GRTS FUNCTION---------
# Call the set.seed function so that the survey designs can be replicated
set.seed(4447864)

# Create the design list
dsgn057 <- list("north" = list(panel=c(mainSites=6),
                                            seltype="Equal",
                                            over=10),
                        "south"=list(panel=c(mainSites=9), # 
                                    seltype="Equal",
                                    over=10))
# create SpatialDesign object
sites057 <- grts(design=dsgn057,
                 DesignID="S", # S for stratified
                 type.frame="area",
                 src.frame="sf.object",
                 sf.object=eqArea057,
                 stratum="strata",
                 shapefile=FALSE) # convert design object to sf, define prj, then write to .shp

# Print the initial six lines of the survey design
head(sites057)


# Print the survey design summary
summary(sites057)


# CRS--------------
class(sites057) # SpatialDesign object
sitesEqArea057 <- st_as_sf(sites057) # convert to sf object
st_crs(sitesEqArea057) # no coordinate reference system?
st_crs(sitesEqArea057) = 5070 # inherits from parent object, per Weber.

# project to WGS84 for plotting with leaflet, ArcPad, and writing out lat/long table  (5070 works fine for mapview)
WGS057 <- eqArea057 %>% st_transform(4326) # 4326 is WGS84, whereas 4269 is NAD83
sitesWGS057 <-  sitesEqArea057 %>% st_transform(4326) # 4326 is WGS84, whereas 4269 is NAD83 

# project to Web Mercator (3857) for use in ArcGIS Pro/geoplatform
merc057 <- eqArea057 %>% st_transform(3857) 
sitesMerc057 <-  sitesEqArea057 %>% st_transform(3857) 

# BUFFER
# Create a 15m radius buffer around each sampling point
# Per ESRI, buffer should be created in Web Mercator, an equidistant projection, not Albers
sitesMerc057buffer <- st_buffer(sitesMerc057, 15) # radius of 15m, diameter of 30m. radius = 45ft = 2 boat lengths
sitesWGS057buffer <- sitesMerc057buffer %>% st_transform(4326) # for use in ArcPad


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
# CH4-057All.png works, but CH4-057AllSites.png does not.


# leaflet colors
factpal <- colorFactor(topo.colors(2), WGS057$strata)
factpal.points <- colorFactor(palette = c("red", "black"), domain = sitesWGS057$panel)

# All points
m <- leaflet() %>% 
  addProviderTiles(providers$Esri.WorldImagery) %>%
  #setView(st_coordinates(cntr_crds)[1], st_coordinates(cntr_crds)[2], zoom = 15) %>%
  fitBounds(lng1 = min(st_coordinates(sitesWGS057)[,1]),
            lng2 = max(st_coordinates(sitesWGS057)[,1]),
            lat1 = min(st_coordinates(sitesWGS057)[,2]),
            lat2 = max(st_coordinates(sitesWGS057)[,2])) %>%
  addPolygons(data = st_zm(WGS057), # removes z and m values, if present
              color = "black", weight = 1,
              fillOpacity = 1, fillColor = ~factpal(strata)) %>%
  addCircleMarkers(data = sitesWGS057,
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
            values = as.character(sitesWGS057$panel),
            title = "Sample sites") %>%
  addLegend(position = "bottomright", pal = factpal, 
            opacity = 1,
            values = as.character(WGS057$strata),
            title = "strata") %>%
  addScaleBar()

mapview::mapshot(m, file = "../../../lakeDsn/NAR/CH4-057/057_All.png", 
                 remove_controls = NULL)


# Main points
m <- leaflet() %>% 
  addProviderTiles(providers$Esri.WorldImagery) %>%
  fitBounds(lng1 = min(st_coordinates(sitesWGS057)[,1]),
            lng2 = max(st_coordinates(sitesWGS057)[,1]),
            lat1 = min(st_coordinates(sitesWGS057)[,2]),
            lat2 = max(st_coordinates(sitesWGS057)[,2])) %>%
  addPolygons(data = st_zm(WGS057), # removes z and m values, if present
              color = "black", weight = 1,
              fillOpacity = 1, fillColor = ~factpal(strata)) %>%
  addCircleMarkers(data = filter(sitesWGS057, panel == "mainSites"),
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
            values = as.character(WGS057$strata),
            title = "strata") %>%
  addScaleBar()

mapview::mapshot(m, file = "../../../lakeDsn/NAR/CH4-057/057_Main.png", 
                 remove_controls = NULL)


# OverSample points
m <- leaflet() %>% 
  addProviderTiles(providers$Esri.WorldImagery) %>%
  fitBounds(lng1 = min(st_coordinates(sitesWGS057)[,1]),
            lng2 = max(st_coordinates(sitesWGS057)[,1]),
            lat1 = min(st_coordinates(sitesWGS057)[,2]),
            lat2 = max(st_coordinates(sitesWGS057)[,2])) %>%
  addPolygons(data = st_zm(WGS057),  # removes z and m values, if present
              color = "black", weight = 1,
              fillOpacity = 1, fillColor = ~factpal(strata)) %>%
  addCircleMarkers(data = filter(sitesWGS057, panel == "OverSamp"),
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
            values = as.character(WGS057$strata),
            title = "strata") %>%
  addScaleBar()

mapview::mapshot(m, file = "../../../lakeDsn/NAR/CH4-057/057_Over.png", 
                 remove_controls = NULL)



# WRITE OBJECTS TO DISK-----------------
# write out table of sample sites for reference in field.  Must be WGS
write.table(sitesWGS057 %>%
              select(panel, siteID, stratum) %>%
              arrange(panel, stratum, siteID),
            file = "../../../lakeDsn/NAR/CH4-057/ch4_057Sites.txt",
            row.names = FALSE, sep="\t")


# geopackage
# can write all layers to a geopackage, which behaves much like a geodatabase in ArcGIS.

# Web Mercator for Web Map first

# write out all sites
st_write(obj = sitesMerc057, 
         dsn = file.path( "../../../lakeDsn/NAR/CH4-057", "merc057.gpkg"), 
         layer = "allSitesMerc057", # package appends 'main.' to layer name?
         append=FALSE, # this overwrites existing layer
         driver = "GPKG")

# write out all buffers
st_write(obj = sitesMerc057buffer, 
         dsn = file.path( "../../../lakeDsn/NAR/CH4-057", "merc057.gpkg"), 
         layer = "allSitesMerc057buffer", # package appends 'main.' to layer name?
         append=FALSE, # this overwrites existing layer
         driver = "GPKG")

# write out main sites
st_write(obj = filter(sitesMerc057, panel == "mainSites"),
         dsn = file.path( "../../../lakeDsn/NAR/CH4-057", "merc057.gpkg"), 
         layer = "mainSitesMerc057",
         append=FALSE, # this overwrites existing layer
         driver = "GPKG")

# write out main site buffers
st_write(obj = filter(sitesMerc057buffer, panel == "mainSites"),
         dsn = file.path( "../../../lakeDsn/NAR/CH4-057", "merc057.gpkg"), 
         layer = "mainSitesMerc057buffer",
         append=FALSE, # this overwrites existing layer
         driver = "GPKG")

# write out oversample sites
st_write(obj = filter(sitesMerc057, panel == "OverSamp"),
         dsn = file.path( "../../../lakeDsn/NAR/CH4-057", "merc057.gpkg"), 
         layer = "overSampSitesMerc057",
         append=FALSE, # this overwrites existing layer,
         driver = "GPKG")

# write out oversample site buffers
st_write(obj = filter(sitesMerc057buffer, panel == "OverSamp"),
         dsn = file.path( "../../../lakeDsn/NAR/CH4-057", "merc057.gpkg"), 
         layer = "overSampSitesMerc057buffer",
         append=FALSE, # this overwrites existing layer,
         driver = "GPKG")


# write out lake polygon
st_write(obj = merc057,
         dsn = file.path( "../../../lakeDsn/NAR/CH4-057", "merc057.gpkg"), 
         layer = "merc057",
         append=FALSE, # this overwrites existing layer,
         driver = "GPKG")


st_layers(file.path( "../../../lakeDsn/NAR/CH4-057", "merc057.gpkg"))


# WGS for ArcPad next

# # write out all sites
# st_write(obj = sitesWGS057, 
#          dsn = file.path( "../../../lakeDsn/NAR/CH4-057", "WGS057.gpkg"), 
#          layer = "allSitesWGS057", # package appends 'main.' to layer name?
#          append=FALSE, # this overwrites existing layer
#          driver = "GPKG")
# 
# # write out all buffers
# st_write(obj = sitesWGS057buffer, 
#          dsn = file.path( "../../../lakeDsn/NAR/CH4-057", "WGS057.gpkg"), 
#          layer = "sitesWGS057buffer", # package appends 'main.' to layer name?
#          append=FALSE, # this overwrites existing layer
#          driver = "GPKG")

# write out main sites
st_write(obj = filter(sitesWGS057, panel == "mainSites"),
         dsn = file.path( "../../../lakeDsn/NAR/CH4-057", "WGS057.gpkg"), 
         layer = "mainSitesWGS057",
         append=FALSE, # this overwrites existing layer
         driver = "GPKG")

# write out main site buffers
st_write(obj = filter(sitesWGS057buffer, panel == "mainSites"),
         dsn = file.path( "../../../lakeDsn/NAR/CH4-057", "WGS057.gpkg"), 
         layer = "mainSitesWGS057buffer",
         append=FALSE, # this overwrites existing layer
         driver = "GPKG")

# write out oversample sites
st_write(obj = filter(sitesWGS057, panel == "OverSamp"),
         dsn = file.path( "../../../lakeDsn/NAR/CH4-057", "WGS057.gpkg"), 
         layer = "overSampSitesWGS057",
         append=FALSE, # this overwrites existing layer,
         driver = "GPKG")

# write out oversample site buffers
st_write(obj = filter(sitesWGS057buffer, panel == "OverSamp"),
         dsn = file.path( "../../../lakeDsn/NAR/CH4-057", "WGS057.gpkg"), 
         layer = "overSampSitesWGS057buffer",
         append=FALSE, # this overwrites existing layer,
         driver = "GPKG")


# write out lake polygon
st_write(obj = WGS057,
         dsn = file.path( "../../../lakeDsn/NAR/CH4-057", "WGS057.gpkg"), 
         layer = "WGS057",
         append=FALSE, # this overwrites existing layer,
         driver = "GPKG")


st_layers(file.path( "../../../lakeDsn/NAR/CH4-057", "WGS057.gpkg"))

