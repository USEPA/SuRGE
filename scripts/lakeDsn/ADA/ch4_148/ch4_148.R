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


# KEYSTONE LAKE (ch4-148)--------
# Design for CH4-148, Keystone Lake, based on NHD lake polygon

# Read polygon shapefile
eqArea148 <- st_read(dsn = "../../../lakeDsn/ADA/CH4-148",
                             layer = "eqArea148")  # shapefile name
plot(eqArea148$geometry) # visualize polygon

# Check CRS, must be equal area for grts function
# EPSG code hard to decipher from output, CRS clearly reported as 'NAD_1983_Albers'
# Coordinate System of NAD 1983 Albers (WKID = 0)
st_crs(eqArea148) 
st_crs(eqArea148) == st_crs(5070) # True

# clean up attributes
eqArea148 <- eqArea148 %>%
  mutate(lakeName = "Keystone",
         lakeSiteID = "ch4-148") %>%
  select(lakeName, lakeSiteID, Area_km2, section)


# ATTRIBUTES----------
head(eqArea148) # review attributes

# summarize frame by section
temp <- with(eqArea148, tapply(Area_km2, section, sum))
temp <- round(addmargins(temp), 2)
temp


# SET UP FOR GRTS FUNCTION---------
# Call the set.seed function so that the survey designs can be replicateD
set.seed(4447864)

# Create the design list
### We decided to set the number of main sites in the tributary area to 5, since it is a relatively small area (0.4 sq km)
### the unequal probability splits the open water part of the lake into two sections of almost equal area
dsgn148 <- list(None = list(panel=c(mainSites=15),
                            seltype="Unequal",
                            caty.n=c("northwest" = 3, # ggrts gives 7, 5.9 sites/km2
                                     "keystone" = 4,
                                     "cimarron" = 2,
                                     "south" = 1,
                                     "prue" = 5),
                            over=25))
# create SpatialDesign object
sites148 <- grts(design=dsgn148,
                 DesignID="U", # U for unstratified, unequal
                 type.frame="area",
                 src.frame="sf.object",
                 sf.object=eqArea148,
                 mdcaty="section",
                 shapefile=FALSE) # convert design object to sf, define prj, then write to .shp

# Print the initial six lines of the survey design
head(sites148)


# Print the survey design summary
summary(sites148)


# CRS--------------
class(sites148) # SpatialDesign object
sites148@data <- sites148@data %>% rename(section = mdcaty) # rename
sitesEqArea148 <- st_as_sf(sites148) # convert to sf object
st_crs(sitesEqArea148) # no coordinate reference system?
st_crs(sitesEqArea148) = 5070 # inherits from parent object, per Weber.

# project to WGS84 for plotting with leaflet, ArcPad, and writing out lat/long table  (5070 works fine for mapview)
WGS148 <- eqArea148 %>% st_transform(4326) # 4326 is WGS84, whereas 4269 is NAD83
sitesWGS148 <-  sitesEqArea148 %>% st_transform(4326) # 4326 is WGS84, whereas 4269 is NAD83 

# project to Web Mercator (3857) for use in ArcGIS Pro/geoplatform
merc148 <- eqArea148 %>% st_transform(3857) 
sitesMerc148 <-  sitesEqArea148 %>% st_transform(3857) 

# BUFFER
# Create a 15m radius buffer around each sampling point
# Per ESRI, buffer should be created in Web Mercator, an equidistant projection, not Albers
sitesMerc148buffer <- st_buffer(sitesMerc148, 15) # radius of 15m, diameter of 30m. radius = 45ft = 2 boat lengths
sitesWGS148buffer <- sitesMerc148buffer %>% st_transform(4326) # for use in ArcPad


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
# ch4_148_lowAll.png works, but ch4_148_lowAllSites.png does not.


# leaflet colors
factpal <- colorFactor(topo.colors(3), WGS148$section)
factpal.points <- colorFactor(palette = c("red", "black"), domain = sitesWGS148$panel)

# All points
m <- leaflet() %>% 
  addProviderTiles(providers$Esri.WorldImagery) %>%
  #setView(st_coordinates(cntr_crds)[1], st_coordinates(cntr_crds)[2], zoom = 15) %>%
  fitBounds(lng1 = min(st_coordinates(sitesWGS148)[,1]),
            lng2 = max(st_coordinates(sitesWGS148)[,1]),
            lat1 = min(st_coordinates(sitesWGS148)[,2]),
            lat2 = max(st_coordinates(sitesWGS148)[,2])) %>%
  addPolygons(data = st_zm(WGS148), # removes z and m values, if present
              color = "black", weight = 1,
              fillOpacity = 1, fillColor = ~factpal(section)) %>%
  addCircleMarkers(data = sitesWGS148,
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
            values = as.character(sitesWGS148$panel),
            title = "Sample sites") %>%
  addLegend(position = "bottomright", pal = factpal, 
            opacity = 1,
            values = as.character(WGS148$section),
            title = "sections") %>%
  addScaleBar()

mapview::mapshot(m, file = "../../../lakeDsn/ADA/CH4-148/148_All.png", 
                 remove_controls = NULL)


# Main points
m <- leaflet() %>% 
  addProviderTiles(providers$Esri.WorldImagery) %>%
  fitBounds(lng1 = min(st_coordinates(sitesWGS148)[,1]),
            lng2 = max(st_coordinates(sitesWGS148)[,1]),
            lat1 = min(st_coordinates(sitesWGS148)[,2]),
            lat2 = max(st_coordinates(sitesWGS148)[,2])) %>%
  addPolygons(data = st_zm(WGS148), # removes z and m values, if present
              color = "black", weight = 1,
              fillOpacity = 1, fillColor = ~factpal(section)) %>%
  addCircleMarkers(data = filter(sitesWGS148, panel == "mainSites"),
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
            values = as.character(WGS148$section),
            title = "sections") %>%
  addScaleBar()

mapview::mapshot(m, file = "../../../lakeDsn/ADA/CH4-148/148_Main.png", 
                 remove_controls = NULL)


# OverSample points
m <- leaflet() %>% 
  addProviderTiles(providers$Esri.WorldImagery) %>%
  fitBounds(lng1 = min(st_coordinates(sitesWGS148)[,1]),
            lng2 = max(st_coordinates(sitesWGS148)[,1]),
            lat1 = min(st_coordinates(sitesWGS148)[,2]),
            lat2 = max(st_coordinates(sitesWGS148)[,2])) %>%
  addPolygons(data = st_zm(WGS148),  # removes z and m values, if present
              color = "black", weight = 1,
              fillOpacity = 1, fillColor = ~factpal(section)) %>%
  addCircleMarkers(data = filter(sitesWGS148, panel == "OverSamp"),
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
            values = as.character(WGS148$section),
            title = "sections") %>%
  addScaleBar()

mapview::mapshot(m, file = "../../../lakeDsn/ADA/CH4-148/148_Over.png", 
                 remove_controls = NULL)



# WRITE OBJECTS TO DISK-----------------
# write out table of sample sites for reference in field.  Must be WGS
write.table(sitesWGS148 %>%
              select(panel, siteID, section) %>%
              arrange(panel, section, siteID),
            file = "../../../lakeDsn/ADA/CH4-148/ch4_148Sites.txt",
            row.names = FALSE, sep="\t")


# geopackage
# can write all layers to a geopackage, which behaves much like a geodatabase in ArcGIS.

# Web Mercator for Web Map first

# write out all sites
st_write(obj = sitesMerc148, 
         dsn = file.path( "../../../lakeDsn/ADA/CH4-148", "merc148.gpkg"), 
         layer = "allSitesMerc148", # package appends 'main.' to layer name?
         append=FALSE, # this overwrites existing layer
         driver = "GPKG")

# write out all buffers
st_write(obj = sitesMerc148buffer, 
         dsn = file.path( "../../../lakeDsn/ADA/CH4-148", "merc148.gpkg"), 
         layer = "allSitesMerc148buffer", # package appends 'main.' to layer name?
         append=FALSE, # this overwrites existing layer
         driver = "GPKG")

# write out main sites
st_write(obj = filter(sitesMerc148, panel == "mainSites"),
         dsn = file.path( "../../../lakeDsn/ADA/CH4-148", "merc148.gpkg"), 
         layer = "mainSitesMerc148",
         append=FALSE, # this overwrites existing layer
         driver = "GPKG")

# write out main site buffers
st_write(obj = filter(sitesMerc148buffer, panel == "mainSites"),
         dsn = file.path( "../../../lakeDsn/ADA/CH4-148", "merc148.gpkg"), 
         layer = "mainSitesMerc148buffer",
         append=FALSE, # this overwrites existing layer
         driver = "GPKG")

# write out oversample sites
st_write(obj = filter(sitesMerc148, panel == "OverSamp"),
         dsn = file.path( "../../../lakeDsn/ADA/CH4-148", "merc148.gpkg"), 
         layer = "overSampSitesMerc148",
         append=FALSE, # this overwrites existing layer,
         driver = "GPKG")

# write out oversample site buffers
st_write(obj = filter(sitesMerc148buffer, panel == "OverSamp"),
         dsn = file.path( "../../../lakeDsn/ADA/CH4-148", "merc148.gpkg"), 
         layer = "overSampSitesMerc148buffer",
         append=FALSE, # this overwrites existing layer,
         driver = "GPKG")


# write out lake polygon
st_write(obj = merc148,
         dsn = file.path( "../../../lakeDsn/ADA/CH4-148", "merc148.gpkg"), 
         layer = "merc148",
         append=FALSE, # this overwrites existing layer,
         driver = "GPKG")


st_layers(file.path( "../../../lakeDsn/ADA/CH4-148", "merc148.gpkg"))


# WGS for ArcPad next

# write out all sites
st_write(obj = sitesWGS148, 
         dsn = file.path( "../../../lakeDsn/ADA/CH4-148", "WGS148.gpkg"), 
         layer = "allSitesWGS148", # package appends 'main.' to layer name?
         append=FALSE, # this overwrites existing layer
         driver = "GPKG")

# write out all buffers
st_write(obj = sitesWGS148buffer, 
         dsn = file.path( "../../../lakeDsn/ADA/CH4-148", "WGS148.gpkg"), 
         layer = "sitesWGS148buffer", # package appends 'main.' to layer name?
         append=FALSE, # this overwrites existing layer
         driver = "GPKG")

# write out main sites
st_write(obj = filter(sitesWGS148, panel == "mainSites"),
         dsn = file.path( "../../../lakeDsn/ADA/CH4-148", "WGS148.gpkg"), 
         layer = "mainSitesWGS148",
         append=FALSE, # this overwrites existing layer
         driver = "GPKG")

# write out main site buffers
st_write(obj = filter(sitesWGS148buffer, panel == "mainSites"),
         dsn = file.path( "../../../lakeDsn/ADA/CH4-148", "WGS148.gpkg"), 
         layer = "mainSitesWGS148buffer",
         append=FALSE, # this overwrites existing layer
         driver = "GPKG")

# write out oversample sites
st_write(obj = filter(sitesWGS148, panel == "OverSamp"),
         dsn = file.path( "../../../lakeDsn/ADA/CH4-148", "WGS148.gpkg"), 
         layer = "overSampSitesWGS148",
         append=FALSE, # this overwrites existing layer,
         driver = "GPKG")

# write out oversample site buffers
st_write(obj = filter(sitesWGS148buffer, panel == "OverSamp"),
         dsn = file.path( "../../../lakeDsn/ADA/CH4-148", "WGS148.gpkg"), 
         layer = "overSampSitesWGS148buffer",
         append=FALSE, # this overwrites existing layer,
         driver = "GPKG")


# write out lake polygon
st_write(obj = WGS148,
         dsn = file.path( "../../../lakeDsn/ADA/CH4-148", "WGS148.gpkg"), 
         layer = "WGS148",
         append=FALSE, # this overwrites existing layer,
         driver = "GPKG")


st_layers(file.path( "../../../lakeDsn/ADA/CH4-148", "WGS148.gpkg"))

