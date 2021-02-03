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


# TSCHIDA LAKE (ch4-068)--------
# Design for CH4-068, Tschida Lake, based on NHD lake polygon
# modified to relfect areal imagery.

# Read polygon shapefile
eqArea068 <- st_read(dsn = "../../../lakeDsn/CIN/CH4-068",
                             layer = "eqArea068")  # shapefile name
plot(eqArea068$geometry) # visualize polygon

# Check CRS, must be equal area for grts function
# EPSG code hard to decipher from output, CRS clearly reported as 'NAD_1983_Albers'
# Coordinate System of NAD 1983 Albers (WKID = 0)
st_crs(eqArea068) 
st_crs(eqArea068) == st_crs(5070) # True

# clean up attributes
eqArea068 <- eqArea068 %>%
  mutate(lakeName = "Tschida Lake",
         lakeSiteID = "ch4-068") %>%
  select(lakeName, lakeSiteID, Area_km2, strata, section)


# ATTRIBUTES----------
head(eqArea068) # review attributes

# summarize frame by section
temp <- with(eqArea068, tapply(Area_km2, section, sum))
temp <- round(addmargins(temp), 2)
temp


# SET UP FOR GRTS FUNCTION---------
# Call the set.seed function so that the survey designs can be replicateD
set.seed(4447864)

# Create the design list
### We decided to set the number of main sites in the tributary area to 5, since it is a relatively small area (0.4 sq km)
### the unequal probability splits the open water part of the lake into two sections of almost equal area
dsgn068 <- list("open_water" = list(panel=c(mainSites=11),
                                            seltype="Unequal",
                                            caty.n=c("west" = 4, # 
                                                     "central" = 3,
                                                     "east" = 4), # 
                                            over=20),
                        "trib"=list(panel=c(mainSites=4), # 
                                    seltype="Equal",
                                    over=20))
# create SpatialDesign object
sites068 <- grts(design=dsgn068,
                        DesignID="SU", # SU for stratified, unequal
                        type.frame="area",
                        src.frame="sf.object",
                        sf.object=eqArea068,
                        stratum="strata",
                        mdcaty="section",
                        shapefile=FALSE) # convert design object to sf, define prj, then write to .shp

# Print the initial six lines of the survey design
head(sites068)


# Print the survey design summary
summary(sites068)


# CRS--------------
class(sites068) # SpatialDesign object
sitesEqArea068 <- st_as_sf(sites068) # convert to sf object
st_crs(sitesEqArea068) # no coordinate reference system?
st_crs(sitesEqArea068) = 5070 # inherits from parent object, per Weber.

# project to WGS84 for plotting with leaflet, ArcPad, and writing out lat/long table  (5070 works fine for mapview)
WGS068 <- eqArea068 %>% st_transform(4326) # 4326 is WGS84, whereas 4269 is NAD83
sitesWGS068 <-  sitesEqArea068 %>% st_transform(4326) # 4326 is WGS84, whereas 4269 is NAD83 

# project to Web Mercator (3857) for use in ArcGIS Pro/geoplatform
merc068 <- eqArea068 %>% st_transform(3857) 
sitesMerc068 <-  sitesEqArea068 %>% st_transform(3857) 

# BUFFER
# Create a 15m radius buffer around each sampling point
# Per ESRI, buffer should be created in Web Mercator, an equidistant projection, not Albers
sitesMerc068buffer <- st_buffer(sitesMerc068, 15) # radius of 15m, diameter of 30m. radius = 45ft = 2 boat lengths
sitesWGS068buffer <- sitesMerc068buffer %>% st_transform(4326) # for use in ArcPad


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
# CH4-068All.png works, but CH4-068AllSites.png does not.


# leaflet colors
factpal <- colorFactor(topo.colors(3), WGS068$section)
factpal.points <- colorFactor(palette = c("red", "black"), domain = sitesWGS068$panel)

# All points
m <- leaflet() %>% 
  addProviderTiles(providers$Esri.WorldImagery) %>%
  #setView(st_coordinates(cntr_crds)[1], st_coordinates(cntr_crds)[2], zoom = 15) %>%
  fitBounds(lng1 = min(st_coordinates(sitesWGS068)[,1]),
            lng2 = max(st_coordinates(sitesWGS068)[,1]),
            lat1 = min(st_coordinates(sitesWGS068)[,2]),
            lat2 = max(st_coordinates(sitesWGS068)[,2])) %>%
  addPolygons(data = st_zm(WGS068), # removes z and m values, if present
              color = "black", weight = 1,
              fillOpacity = 1, fillColor = ~factpal(section)) %>%
  addCircleMarkers(data = sitesWGS068,
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
            values = as.character(sitesWGS068$panel),
            title = "Sample sites") %>%
  addLegend(position = "bottomright", pal = factpal, 
            opacity = 1,
            values = as.character(WGS068$section),
            title = "sections") %>%
  addScaleBar()

mapview::mapshot(m, file = "../../../lakeDsn/CIN/CH4-068/068_All.png", 
                 remove_controls = NULL)


# Main points
m <- leaflet() %>% 
  addProviderTiles(providers$Esri.WorldImagery) %>%
  fitBounds(lng1 = min(st_coordinates(sitesWGS068)[,1]),
            lng2 = max(st_coordinates(sitesWGS068)[,1]),
            lat1 = min(st_coordinates(sitesWGS068)[,2]),
            lat2 = max(st_coordinates(sitesWGS068)[,2])) %>%
  addPolygons(data = st_zm(WGS068), # removes z and m values, if present
              color = "black", weight = 1,
              fillOpacity = 1, fillColor = ~factpal(section)) %>%
  addCircleMarkers(data = filter(sitesWGS068, panel == "mainSites"),
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
            values = as.character(WGS068$section),
            title = "sections") %>%
  addScaleBar()

mapview::mapshot(m, file = "../../../lakeDsn/CIN/CH4-068/068_Main.png", 
                 remove_controls = NULL)


# OverSample points
m <- leaflet() %>% 
  addProviderTiles(providers$Esri.WorldImagery) %>%
  fitBounds(lng1 = min(st_coordinates(sitesWGS068)[,1]),
            lng2 = max(st_coordinates(sitesWGS068)[,1]),
            lat1 = min(st_coordinates(sitesWGS068)[,2]),
            lat2 = max(st_coordinates(sitesWGS068)[,2])) %>%
  addPolygons(data = st_zm(WGS068),  # removes z and m values, if present
              color = "black", weight = 1,
              fillOpacity = 1, fillColor = ~factpal(section)) %>%
  addCircleMarkers(data = filter(sitesWGS068, panel == "OverSamp"),
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
            values = as.character(WGS068$section),
            title = "sections") %>%
  addScaleBar()

mapview::mapshot(m, file = "../../../lakeDsn/CIN/CH4-068/068_Over.png", 
                 remove_controls = NULL)



# WRITE OBJECTS TO DISK-----------------
# write out table of sample sites for reference in field.  Must be WGS
write.table(sitesWGS068 %>%
              select(panel, siteID, section) %>%
              arrange(panel, section, siteID),
            file = "../../../lakeDsn/CIN/CH4-068/ch4_068Sites.txt",
            row.names = FALSE, sep="\t")


# geopackage
# can write all layers to a geopackage, which behaves much like a geodatabase in ArcGIS.

# Web Mercator for Web Map first

# write out all sites
st_write(obj = sitesMerc068, 
         dsn = file.path( "../../../lakeDsn/CIN/CH4-068", "merc068.gpkg"), 
         layer = "allSitesMerc068", # package appends 'main.' to layer name?
         append=FALSE, # this overwrites existing layer
         driver = "GPKG")

# write out all buffers
st_write(obj = sitesMerc068buffer, 
         dsn = file.path( "../../../lakeDsn/CIN/CH4-068", "merc068.gpkg"), 
         layer = "allSitesMerc068buffer", # package appends 'main.' to layer name?
         append=FALSE, # this overwrites existing layer
         driver = "GPKG")

# write out main sites
st_write(obj = filter(sitesMerc068, panel == "mainSites"),
         dsn = file.path( "../../../lakeDsn/CIN/CH4-068", "merc068.gpkg"), 
         layer = "mainSitesMerc068",
         append=FALSE, # this overwrites existing layer
         driver = "GPKG")

# write out main site buffers
st_write(obj = filter(sitesMerc068buffer, panel == "mainSites"),
         dsn = file.path( "../../../lakeDsn/CIN/CH4-068", "merc068.gpkg"), 
         layer = "mainSitesMerc068buffer",
         append=FALSE, # this overwrites existing layer
         driver = "GPKG")

# write out oversample sites
st_write(obj = filter(sitesMerc068, panel == "OverSamp"),
         dsn = file.path( "../../../lakeDsn/CIN/CH4-068", "merc068.gpkg"), 
         layer = "overSampSitesMerc068",
         append=FALSE, # this overwrites existing layer,
         driver = "GPKG")

# write out oversample site buffers
st_write(obj = filter(sitesMerc068buffer, panel == "OverSamp"),
         dsn = file.path( "../../../lakeDsn/CIN/CH4-068", "merc068.gpkg"), 
         layer = "overSampSitesMerc068buffer",
         append=FALSE, # this overwrites existing layer,
         driver = "GPKG")


# write out lake polygon
st_write(obj = merc068,
         dsn = file.path( "../../../lakeDsn/CIN/CH4-068", "merc068.gpkg"), 
         layer = "merc068",
         append=FALSE, # this overwrites existing layer,
         driver = "GPKG")


st_layers(file.path( "../../../lakeDsn/CIN/CH4-068", "merc068.gpkg"))


# WGS for ArcPad next

# write out all sites
st_write(obj = sitesWGS068, 
         dsn = file.path( "../../../lakeDsn/CIN/CH4-068", "WGS068.gpkg"), 
         layer = "allSitesWGS068", # package appends 'main.' to layer name?
         append=FALSE, # this overwrites existing layer
         driver = "GPKG")

# write out all buffers
st_write(obj = sitesWGS068buffer, 
         dsn = file.path( "../../../lakeDsn/CIN/CH4-068", "WGS068.gpkg"), 
         layer = "sitesWGS068buffer", # package appends 'main.' to layer name?
         append=FALSE, # this overwrites existing layer
         driver = "GPKG")

# write out main sites
st_write(obj = filter(sitesWGS068, panel == "mainSites"),
         dsn = file.path( "../../../lakeDsn/CIN/CH4-068", "WGS068.gpkg"), 
         layer = "mainSitesWGS068",
         append=FALSE, # this overwrites existing layer
         driver = "GPKG")

# write out main site buffers
st_write(obj = filter(sitesWGS068buffer, panel == "mainSites"),
         dsn = file.path( "../../../lakeDsn/CIN/CH4-068", "WGS068.gpkg"), 
         layer = "mainSitesWGS068buffer",
         append=FALSE, # this overwrites existing layer
         driver = "GPKG")

# write out oversample sites
st_write(obj = filter(sitesWGS068, panel == "OverSamp"),
         dsn = file.path( "../../../lakeDsn/CIN/CH4-068", "WGS068.gpkg"), 
         layer = "overSampSitesWGS068",
         append=FALSE, # this overwrites existing layer,
         driver = "GPKG")

# write out oversample site buffers
st_write(obj = filter(sitesWGS068buffer, panel == "OverSamp"),
         dsn = file.path( "../../../lakeDsn/CIN/CH4-068", "WGS068.gpkg"), 
         layer = "overSampSitesWGS068buffer",
         append=FALSE, # this overwrites existing layer,
         driver = "GPKG")


# write out lake polygon
st_write(obj = WGS068,
         dsn = file.path( "../../../lakeDsn/CIN/CH4-068", "WGS068.gpkg"), 
         layer = "WGS068",
         append=FALSE, # this overwrites existing layer,
         driver = "GPKG")


st_layers(file.path( "../../../lakeDsn/CIN/CH4-068", "WGS068.gpkg"))

