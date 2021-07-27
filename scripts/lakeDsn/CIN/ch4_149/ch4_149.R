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


# SHERMAN RESERVOIR (ch4-149)--------
# Design for CH4-149, Sherman Reservoir, based on NHD lake polygon


# Read polygon shapefile
eqArea149 <- st_read(dsn = "../../../lakeDsn/CIN/CH4-149",
                             layer = "eqArea149")  # shapefile name
plot(eqArea149$geometry) # visualize polygon

# Check CRS, must be equal area for grts function
# EPSG code hard to decipher from output, CRS clearly reported as 'NAD_1983_Albers'
# Coordinate System of NAD 1983 Albers (WKID = 0)
st_crs(eqArea149) 
st_crs(eqArea149) == st_crs(5070) # True

# clean up attributes
eqArea149 <- eqArea149 %>%
  mutate(lakeName = "Sherman Reservoir",
         lakeSiteID = "ch4-149") %>%
  select(lakeName, lakeSiteID, Area_km2, strata, section)


# ATTRIBUTES----------
head(eqArea149) # review attributes

# summarize frame by section
temp <- with(eqArea149, tapply(Area_km2, section, sum))
temp <- round(addmargins(temp), 2)
temp


# SET UP FOR GRTS FUNCTION---------
# Call the set.seed function so that the survey designs can be replicateD
set.seed(4447864)

# Create the design list
### We decided to set the number of main sites in the tributary area to 5, since it is a relatively small area (0.4 sq km)
### the unequal probability splits the open water part of the lake into two sections of almost equal area
dsgn149 <- list("open_water" = list(panel=c(mainSites=10),
                                            seltype="Unequal",
                                            caty.n=c("east" = 3, # grts gives 4, =  3 sites/km2
                                                     "north" = 3, # grts gives 3, =  1 sites/km2
                                                     "dam" = 4), # grts gives 2,  0.4 sites/km2
                                            over=20),
                        "trib"=list(panel=c(mainSites=5), #  8 sites/km2
                                    seltype="Equal",
                                    over=5))
# create SpatialDesign object
sites149 <- grts(design=dsgn149,
                        DesignID="SU", # SU for stratified, unequal
                        type.frame="area",
                        src.frame="sf.object",
                        sf.object=eqArea149,
                        stratum="strata",
                        mdcaty="section",
                        shapefile=FALSE) # convert design object to sf, define prj, then write to .shp

# Print the initial six lines of the survey design
head(sites149)


# Print the survey design summary
summary(sites149)


# CRS--------------
class(sites149) # SpatialDesign object
sitesEqArea149 <- st_as_sf(sites149) # convert to sf object
st_crs(sitesEqArea149) # no coordinate reference system?
st_crs(sitesEqArea149) = 5070 # inherits from parent object, per Weber.

# project to WGS84 for plotting with leaflet, ArcPad, and writing out lat/long table  (5070 works fine for mapview)
WGS149 <- eqArea149 %>% st_transform(4326) # 4326 is WGS84, whereas 4269 is NAD83
sitesWGS149 <-  sitesEqArea149 %>% st_transform(4326) # 4326 is WGS84, whereas 4269 is NAD83 

# project to Web Mercator (3857) for use in ArcGIS Pro/geoplatform
merc149 <- eqArea149 %>% st_transform(3857) 
sitesMerc149 <-  sitesEqArea149 %>% st_transform(3857) 

# BUFFER
# Create a 15m radius buffer around each sampling point
# Per ESRI, buffer should be created in Web Mercator, an equidistant projection, not Albers
sitesMerc149buffer <- st_buffer(sitesMerc149, 15) # radius of 15m, diameter of 30m. radius = 45ft = 2 boat lengths
sitesWGS149buffer <- sitesMerc149buffer %>% st_transform(4326) # for use in ArcPad


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
# CH4-149All.png works, but CH4-149AllSites.png does not.


# leaflet colors
factpal <- colorFactor(topo.colors(3), WGS149$section)
factpal.points <- colorFactor(palette = c("red", "black"), domain = sitesWGS149$panel)

# All points
m <- leaflet() %>% 
  addProviderTiles(providers$Esri.WorldImagery) %>%
  #setView(st_coordinates(cntr_crds)[1], st_coordinates(cntr_crds)[2], zoom = 15) %>%
  fitBounds(lng1 = min(st_coordinates(sitesWGS149)[,1]),
            lng2 = max(st_coordinates(sitesWGS149)[,1]),
            lat1 = min(st_coordinates(sitesWGS149)[,2]),
            lat2 = max(st_coordinates(sitesWGS149)[,2])) %>%
  addPolygons(data = st_zm(WGS149), # removes z and m values, if present
              color = "black", weight = 1,
              fillOpacity = 1, fillColor = ~factpal(section)) %>%
  addCircleMarkers(data = sitesWGS149,
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
            values = as.character(sitesWGS149$panel),
            title = "Sample sites") %>%
  addLegend(position = "bottomright", pal = factpal, 
            opacity = 1,
            values = as.character(WGS149$section),
            title = "sections") %>%
  addScaleBar()

mapview::mapshot(m, file = "../../../lakeDsn/CIN/CH4-149/149_All.png", 
                 remove_controls = NULL)


# Main points
m <- leaflet() %>% 
  addProviderTiles(providers$Esri.WorldImagery) %>%
  fitBounds(lng1 = min(st_coordinates(sitesWGS149)[,1]),
            lng2 = max(st_coordinates(sitesWGS149)[,1]),
            lat1 = min(st_coordinates(sitesWGS149)[,2]),
            lat2 = max(st_coordinates(sitesWGS149)[,2])) %>%
  addPolygons(data = st_zm(WGS149), # removes z and m values, if present
              color = "black", weight = 1,
              fillOpacity = 1, fillColor = ~factpal(section)) %>%
  addCircleMarkers(data = filter(sitesWGS149, panel == "mainSites"),
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
            values = as.character(WGS149$section),
            title = "sections") %>%
  addScaleBar()

mapview::mapshot(m, file = "../../../lakeDsn/CIN/CH4-149/149_Main.png", 
                 remove_controls = NULL)


# OverSample points
m <- leaflet() %>% 
  addProviderTiles(providers$Esri.WorldImagery) %>%
  fitBounds(lng1 = min(st_coordinates(sitesWGS149)[,1]),
            lng2 = max(st_coordinates(sitesWGS149)[,1]),
            lat1 = min(st_coordinates(sitesWGS149)[,2]),
            lat2 = max(st_coordinates(sitesWGS149)[,2])) %>%
  addPolygons(data = st_zm(WGS149),  # removes z and m values, if present
              color = "black", weight = 1,
              fillOpacity = 1, fillColor = ~factpal(section)) %>%
  addCircleMarkers(data = filter(sitesWGS149, panel == "OverSamp"),
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
            values = as.character(WGS149$section),
            title = "sections") %>%
  addScaleBar()

mapview::mapshot(m, file = "../../../lakeDsn/CIN/CH4-149/149_Over.png", 
                 remove_controls = NULL)



# WRITE OBJECTS TO DISK-----------------
# write out table of sample sites for reference in field.  Must be WGS
write.table(sitesWGS149 %>%
              select(panel, siteID, section) %>%
              arrange(panel, section, siteID),
            file = "../../../lakeDsn/CIN/CH4-149/ch4_149Sites.txt",
            row.names = FALSE, sep="\t")


# geopackage
# can write all layers to a geopackage, which behaves much like a geodatabase in ArcGIS.

# Web Mercator for Web Map first

# write out all sites
st_write(obj = sitesMerc149, 
         dsn = file.path( "../../../lakeDsn/CIN/CH4-149", "merc149.gpkg"), 
         layer = "allSitesMerc149", # package appends 'main.' to layer name?
         append=FALSE, # this overwrites existing layer
         driver = "GPKG")

# write out all buffers
st_write(obj = sitesMerc149buffer, 
         dsn = file.path( "../../../lakeDsn/CIN/CH4-149", "merc149.gpkg"), 
         layer = "allSitesMerc149buffer", # package appends 'main.' to layer name?
         append=FALSE, # this overwrites existing layer
         driver = "GPKG")

# write out main sites
st_write(obj = filter(sitesMerc149, panel == "mainSites"),
         dsn = file.path( "../../../lakeDsn/CIN/CH4-149", "merc149.gpkg"), 
         layer = "mainSitesMerc149",
         append=FALSE, # this overwrites existing layer
         driver = "GPKG")

# write out main site buffers
st_write(obj = filter(sitesMerc149buffer, panel == "mainSites"),
         dsn = file.path( "../../../lakeDsn/CIN/CH4-149", "merc149.gpkg"), 
         layer = "mainSitesMerc149buffer",
         append=FALSE, # this overwrites existing layer
         driver = "GPKG")

# write out oversample sites
st_write(obj = filter(sitesMerc149, panel == "OverSamp"),
         dsn = file.path( "../../../lakeDsn/CIN/CH4-149", "merc149.gpkg"), 
         layer = "overSampSitesMerc149",
         append=FALSE, # this overwrites existing layer,
         driver = "GPKG")

# write out oversample site buffers
st_write(obj = filter(sitesMerc149buffer, panel == "OverSamp"),
         dsn = file.path( "../../../lakeDsn/CIN/CH4-149", "merc149.gpkg"), 
         layer = "overSampSitesMerc149buffer",
         append=FALSE, # this overwrites existing layer,
         driver = "GPKG")


# write out lake polygon
st_write(obj = merc149,
         dsn = file.path( "../../../lakeDsn/CIN/CH4-149", "merc149.gpkg"), 
         layer = "merc149",
         append=FALSE, # this overwrites existing layer,
         driver = "GPKG")


st_layers(file.path( "../../../lakeDsn/CIN/CH4-149", "merc149.gpkg"))


# WGS for ArcPad next

# write out all sites
st_write(obj = sitesWGS149, 
         dsn = file.path( "../../../lakeDsn/CIN/CH4-149", "WGS149.gpkg"), 
         layer = "allSitesWGS149", # package appends 'main.' to layer name?
         append=FALSE, # this overwrites existing layer
         driver = "GPKG")

# write out all buffers
st_write(obj = sitesWGS149buffer, 
         dsn = file.path( "../../../lakeDsn/CIN/CH4-149", "WGS149.gpkg"), 
         layer = "sitesWGS149buffer", # package appends 'main.' to layer name?
         append=FALSE, # this overwrites existing layer
         driver = "GPKG")

# write out main sites
st_write(obj = filter(sitesWGS149, panel == "mainSites"),
         dsn = file.path( "../../../lakeDsn/CIN/CH4-149", "WGS149.gpkg"), 
         layer = "mainSitesWGS149",
         append=FALSE, # this overwrites existing layer
         driver = "GPKG")

# write out main site buffers
st_write(obj = filter(sitesWGS149buffer, panel == "mainSites"),
         dsn = file.path( "../../../lakeDsn/CIN/CH4-149", "WGS149.gpkg"), 
         layer = "mainSitesWGS149buffer",
         append=FALSE, # this overwrites existing layer
         driver = "GPKG")

# write out oversample sites
st_write(obj = filter(sitesWGS149, panel == "OverSamp"),
         dsn = file.path( "../../../lakeDsn/CIN/CH4-149", "WGS149.gpkg"), 
         layer = "overSampSitesWGS149",
         append=FALSE, # this overwrites existing layer,
         driver = "GPKG")

# write out oversample site buffers
st_write(obj = filter(sitesWGS149buffer, panel == "OverSamp"),
         dsn = file.path( "../../../lakeDsn/CIN/CH4-149", "WGS149.gpkg"), 
         layer = "overSampSitesWGS149buffer",
         append=FALSE, # this overwrites existing layer,
         driver = "GPKG")


# write out lake polygon
st_write(obj = WGS149,
         dsn = file.path( "../../../lakeDsn/CIN/CH4-149", "WGS149.gpkg"), 
         layer = "WGS149",
         append=FALSE, # this overwrites existing layer,
         driver = "GPKG")


st_layers(file.path( "../../../lakeDsn/CIN/CH4-149", "WGS149.gpkg"))

