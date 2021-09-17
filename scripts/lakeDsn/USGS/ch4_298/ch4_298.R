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


# WILD HORSE RESERVOIR (ch4-298)--------
# Design for CH4-298, Wild Horse Reservoir.
# Polygon based on six historic images from Google Earth reflecting a fairly broad 
# range of reservoir heights.  We reviewed these with the local reservoir manager.  
# Based on current water levels, current snow pack in the mountains, and anticipated 
# irrigation demand, he felt that the ch4_298_Oct2006.jpg image is a good approximation 
# of the conditions we will encounter this summer.  This is supported by April 17, 2021
# Landsat imagery.  At that water level we will 
# be able to navigate our boat under the bridge leading to the middle arm.  
# The other two arms of the reservoir will remain unaccessible.

# Read polygon shapefile
eqArea298 <- st_read(dsn = "../../../lakeDsn/USGS/CH4-298",
                             layer = "eqArea298")  # shapefile name
plot(eqArea298$geometry) # visualize polygon

# Check CRS, must be equal area for grts function
# EPSG code hard to decipher from output, CRS clearly reported as 'NAD_1983_Albers'
# Coordinate System of NAD 1983 Albers (WKID = 0)
st_crs(eqArea298) 
st_crs(eqArea298) == st_crs(5070) # True

# clean up attributes
eqArea298 <- eqArea298 %>%
  mutate(lakeName = "Wild Horse Reservoir",
         lakeSiteID = "ch4-298") %>%
  select(lakeName, lakeSiteID, Area_km2, section)


# ATTRIBUTES----------
head(eqArea298) # review attributes

# summarize frame by section
temp <- with(eqArea298, tapply(Area_km2, section, sum))
temp <- round(addmargins(temp), 2)
temp


# SET UP FOR GRTS FUNCTION---------
# Call the set.seed function so that the survey designs can be replicateD
set.seed(4447864)

# Create the design list
### We decided to set the number of main sites in the tributary area to 5, since it is a relatively small area (0.4 sq km)
### the unequal probability splits the open water part of the lake into two sections of almost equal area
dsgn298 <- list(None = list(panel=c(mainSites=15),
                            seltype="Unequal",
                            caty.n=c("north" = 1, 
                                     "NW" = 7,
                                     "SE" = 7), 
                            over=30))
# create SpatialDesign object
sites298 <- grts(design=dsgn298,
                 DesignID="U", # U for unstratified, unequal
                 type.frame="area",
                 src.frame="sf.object",
                 sf.object=eqArea298,
                 mdcaty="section",
                 shapefile=FALSE) # convert design object to sf, define prj, then write to .shp

# Print the initial six lines of the survey design
head(sites298)


# Print the survey design summary
summary(sites298)


# CRS--------------
class(sites298) # SpatialDesign object
sites298@data <- sites298@data %>% rename(section = mdcaty) # rename
sitesEqArea298 <- st_as_sf(sites298) # convert to sf object
st_crs(sitesEqArea298) # no coordinate reference system?
st_crs(sitesEqArea298) = 5070 # inherits from parent object, per Weber.

# project to WGS84 for plotting with leaflet, ArcPad, and writing out lat/long table  (5070 works fine for mapview)
WGS298 <- eqArea298 %>% st_transform(4326) # 4326 is WGS84, whereas 4269 is NAD83
sitesWGS298 <-  sitesEqArea298 %>% st_transform(4326) # 4326 is WGS84, whereas 4269 is NAD83 

# project to Web Mercator (3857) for use in ArcGIS Pro/geoplatform
merc298 <- eqArea298 %>% st_transform(3857) 
sitesMerc298 <-  sitesEqArea298 %>% st_transform(3857) 

# BUFFER
# Create a 15m radius buffer around each sampling point
# Per ESRI, buffer should be created in Web Mercator, an equidistant projection, not Albers
sitesMerc298buffer <- st_buffer(sitesMerc298, 15) # radius of 15m, diameter of 30m. radius = 45ft = 2 boat lengths
sitesWGS298buffer <- sitesMerc298buffer %>% st_transform(4326) # for use in ArcPad


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
# ch4_298_lowAll.png works, but ch4_298_lowAllSites.png does not.


# leaflet colors
factpal <- colorFactor(topo.colors(3), WGS298$section)
factpal.points <- colorFactor(palette = c("red", "black"), domain = sitesWGS298$panel)

# All points
m <- leaflet() %>% 
  addProviderTiles(providers$Esri.WorldImagery) %>%
  #setView(st_coordinates(cntr_crds)[1], st_coordinates(cntr_crds)[2], zoom = 15) %>%
  fitBounds(lng1 = min(st_coordinates(sitesWGS298)[,1]),
            lng2 = max(st_coordinates(sitesWGS298)[,1]),
            lat1 = min(st_coordinates(sitesWGS298)[,2]),
            lat2 = max(st_coordinates(sitesWGS298)[,2])) %>%
  addPolygons(data = st_zm(WGS298), # removes z and m values, if present
              color = "black", weight = 1,
              fillOpacity = 1, fillColor = ~factpal(section)) %>%
  addCircleMarkers(data = sitesWGS298,
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
            values = as.character(sitesWGS298$panel),
            title = "Sample sites") %>%
  addLegend(position = "bottomright", pal = factpal, 
            opacity = 1,
            values = as.character(WGS298$section),
            title = "sections") %>%
  addScaleBar()

mapview::mapshot(m, file = "../../../lakeDsn/USGS/CH4-298/298_All.png", 
                 remove_controls = NULL)


# Main points
m <- leaflet() %>% 
  addProviderTiles(providers$Esri.WorldImagery) %>%
  fitBounds(lng1 = min(st_coordinates(sitesWGS298)[,1]),
            lng2 = max(st_coordinates(sitesWGS298)[,1]),
            lat1 = min(st_coordinates(sitesWGS298)[,2]),
            lat2 = max(st_coordinates(sitesWGS298)[,2])) %>%
  addPolygons(data = st_zm(WGS298), # removes z and m values, if present
              color = "black", weight = 1,
              fillOpacity = 1, fillColor = ~factpal(section)) %>%
  addCircleMarkers(data = filter(sitesWGS298, panel == "mainSites"),
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
            values = as.character(WGS298$section),
            title = "sections") %>%
  addScaleBar()

mapview::mapshot(m, file = "../../../lakeDsn/USGS/CH4-298/298_Main.png", 
                 remove_controls = NULL)


# OverSample points
m <- leaflet() %>% 
  addProviderTiles(providers$Esri.WorldImagery) %>%
  fitBounds(lng1 = min(st_coordinates(sitesWGS298)[,1]),
            lng2 = max(st_coordinates(sitesWGS298)[,1]),
            lat1 = min(st_coordinates(sitesWGS298)[,2]),
            lat2 = max(st_coordinates(sitesWGS298)[,2])) %>%
  addPolygons(data = st_zm(WGS298),  # removes z and m values, if present
              color = "black", weight = 1,
              fillOpacity = 1, fillColor = ~factpal(section)) %>%
  addCircleMarkers(data = filter(sitesWGS298, panel == "OverSamp"),
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
            values = as.character(WGS298$section),
            title = "sections") %>%
  addScaleBar()

mapview::mapshot(m, file = "../../../lakeDsn/USGS/CH4-298/298_Over.png", 
                 remove_controls = NULL)



# WRITE OBJECTS TO DISK-----------------
# write out table of sample sites for reference in field.  Must be WGS
write.table(sitesWGS298 %>%
              select(panel, siteID, section) %>%
              arrange(panel, section, siteID),
            file = "../../../lakeDsn/USGS/CH4-298/ch4_298Sites.txt",
            row.names = FALSE, sep="\t")


# geopackage
# can write all layers to a geopackage, which behaves much like a geodatabase in ArcGIS.

# Web Mercator for Web Map first

# write out all sites
st_write(obj = sitesMerc298, 
         dsn = file.path( "../../../lakeDsn/USGS/CH4-298", "merc298.gpkg"), 
         layer = "allSitesMerc298", # package appends 'main.' to layer name?
         append=FALSE, # this overwrites existing layer
         driver = "GPKG")

# write out all buffers
st_write(obj = sitesMerc298buffer, 
         dsn = file.path( "../../../lakeDsn/USGS/CH4-298", "merc298.gpkg"), 
         layer = "allSitesMerc298buffer", # package appends 'main.' to layer name?
         append=FALSE, # this overwrites existing layer
         driver = "GPKG")

# write out main sites
st_write(obj = filter(sitesMerc298, panel == "mainSites"),
         dsn = file.path( "../../../lakeDsn/USGS/CH4-298", "merc298.gpkg"), 
         layer = "mainSitesMerc298",
         append=FALSE, # this overwrites existing layer
         driver = "GPKG")

# write out main site buffers
st_write(obj = filter(sitesMerc298buffer, panel == "mainSites"),
         dsn = file.path( "../../../lakeDsn/USGS/CH4-298", "merc298.gpkg"), 
         layer = "mainSitesMerc298buffer",
         append=FALSE, # this overwrites existing layer
         driver = "GPKG")

# write out oversample sites
st_write(obj = filter(sitesMerc298, panel == "OverSamp"),
         dsn = file.path( "../../../lakeDsn/USGS/CH4-298", "merc298.gpkg"), 
         layer = "overSampSitesMerc298",
         append=FALSE, # this overwrites existing layer,
         driver = "GPKG")

# write out oversample site buffers
st_write(obj = filter(sitesMerc298buffer, panel == "OverSamp"),
         dsn = file.path( "../../../lakeDsn/USGS/CH4-298", "merc298.gpkg"), 
         layer = "overSampSitesMerc298buffer",
         append=FALSE, # this overwrites existing layer,
         driver = "GPKG")


# write out lake polygon
st_write(obj = merc298,
         dsn = file.path( "../../../lakeDsn/USGS/CH4-298", "merc298.gpkg"), 
         layer = "merc298",
         append=FALSE, # this overwrites existing layer,
         driver = "GPKG")


st_layers(file.path( "../../../lakeDsn/USGS/CH4-298", "merc298.gpkg"))


# WGS for ArcPad next

# write out all sites
st_write(obj = sitesWGS298, 
         dsn = file.path( "../../../lakeDsn/USGS/CH4-298", "WGS298.gpkg"), 
         layer = "allSitesWGS298", # package appends 'main.' to layer name?
         append=FALSE, # this overwrites existing layer
         driver = "GPKG")

# write out all buffers
st_write(obj = sitesWGS298buffer, 
         dsn = file.path( "../../../lakeDsn/USGS/CH4-298", "WGS298.gpkg"), 
         layer = "sitesWGS298buffer", # package appends 'main.' to layer name?
         append=FALSE, # this overwrites existing layer
         driver = "GPKG")

# write out main sites
st_write(obj = filter(sitesWGS298, panel == "mainSites"),
         dsn = file.path( "../../../lakeDsn/USGS/CH4-298", "WGS298.gpkg"), 
         layer = "mainSitesWGS298",
         append=FALSE, # this overwrites existing layer
         driver = "GPKG")

# write out main site buffers
st_write(obj = filter(sitesWGS298buffer, panel == "mainSites"),
         dsn = file.path( "../../../lakeDsn/USGS/CH4-298", "WGS298.gpkg"), 
         layer = "mainSitesWGS298buffer",
         append=FALSE, # this overwrites existing layer
         driver = "GPKG")

# write out oversample sites
st_write(obj = filter(sitesWGS298, panel == "OverSamp"),
         dsn = file.path( "../../../lakeDsn/USGS/CH4-298", "WGS298.gpkg"), 
         layer = "overSampSitesWGS298",
         append=FALSE, # this overwrites existing layer,
         driver = "GPKG")

# write out oversample site buffers
st_write(obj = filter(sitesWGS298buffer, panel == "OverSamp"),
         dsn = file.path( "../../../lakeDsn/USGS/CH4-298", "WGS298.gpkg"), 
         layer = "overSampSitesWGS298buffer",
         append=FALSE, # this overwrites existing layer,
         driver = "GPKG")


# write out lake polygon
st_write(obj = WGS298,
         dsn = file.path( "../../../lakeDsn/USGS/CH4-298", "WGS298.gpkg"), 
         layer = "WGS298",
         append=FALSE, # this overwrites existing layer,
         driver = "GPKG")


st_layers(file.path( "../../../lakeDsn/USGS/CH4-298", "WGS298.gpkg"))

