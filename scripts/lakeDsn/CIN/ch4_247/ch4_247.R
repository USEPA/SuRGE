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


# Heron Lake (ch4-247)--------
# Design for CH4-247, Heron Reservoir, based on March 2022 Landsat

# Read polygon shapefile
eqArea247 <- st_read(dsn = "../../../lakeDsn/CIN/CH4-247",
                             layer = "eqArea247")  # shapefile name
plot(eqArea247$geometry) # visualize polygon

# Check CRS, must be equal area for grts function
# EPSG code hard to decipher from output, CRS clearly reported as 'NAD_1983_Albers'
# Coordinate System of NAD 1983 Albers (WKID = 0)
st_crs(eqArea247) 
st_crs(eqArea247) == st_crs(5070) # True

# clean up attributes
eqArea247 <- eqArea247 %>%
  mutate(lakeName = "Heron Reservoir",
         lakeSiteID = "ch4-247") %>%
  select(lakeName, lakeSiteID, Area_km2, section)


# ATTRIBUTES----------
head(eqArea247) # review attributes

# summarize frame by section
temp <- with(eqArea247, tapply(Area_km2, section, sum))
temp <- round(addmargins(temp), 2)
temp


# SET UP FOR GRTS FUNCTION---------
# Call the set.seed function so that the survey designs can be replicateD
set.seed(4447864)

# Create the design list
### We decided to set the number of main sites in the tributary area to 5, since it is a relatively small area (0.4 sq km)
### the unequal probability splits the open water part of the lake into two sections of almost equal area
dsgn247 <- list(None = list(panel=c(mainSites=15),
                            seltype="Unequal",
                            caty.n=c("west" = 5, # grts gives 5, = 5.4 sites/km2
                                     "east" = 6, # grts gives 7, 5.9 sites/km2
                                     "northeast" = 2,
                                     "north" = 2),
                            over=20))
# create SpatialDesign object
sites247 <- grts(design=dsgn247,
                 DesignID="U", # U for unstratified, unequal
                 type.frame="area",
                 src.frame="sf.object",
                 sf.object=eqArea247,
                 mdcaty="section",
                 shapefile=FALSE) # convert design object to sf, define prj, then write to .shp

# Print the initial six lines of the survey design
head(sites247)


# Print the survey design summary
summary(sites247)


# CRS--------------
class(sites247) # SpatialDesign object
sites247@data <- sites247@data %>% rename(section = mdcaty) # rename
sitesEqArea247 <- st_as_sf(sites247) # convert to sf object
st_crs(sitesEqArea247) # no coordinate reference system?
st_crs(sitesEqArea247) = 5070 # inherits from parent object, per Weber.

# project to WGS84 for plotting with leaflet, ArcPad, and writing out lat/long table  (5070 works fine for mapview)
WGS247 <- eqArea247 %>% st_transform(4326) # 4326 is WGS84, whereas 4269 is NAD83
sitesWGS247 <-  sitesEqArea247 %>% st_transform(4326) # 4326 is WGS84, whereas 4269 is NAD83 

# project to Web Mercator (3857) for use in ArcGIS Pro/geoplatform
merc247 <- eqArea247 %>% st_transform(3857) 
sitesMerc247 <-  sitesEqArea247 %>% st_transform(3857) 

# BUFFER
# Create a 15m radius buffer around each sampling point
# Per ESRI, buffer should be created in Web Mercator, an equidistant projection, not Albers
sitesMerc247buffer <- st_buffer(sitesMerc247, 15) # radius of 15m, diameter of 30m. radius = 45ft = 2 boat lengths
sitesWGS247buffer <- sitesMerc247buffer %>% st_transform(4326) # for use in ArcPad


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
# ch4_247_lowAll.png works, but ch4_247_lowAllSites.png does not.


# leaflet colors
factpal <- colorFactor(topo.colors(3), WGS247$section)
factpal.points <- colorFactor(palette = c("red", "black"), domain = sitesWGS247$panel)

# All points
m <- leaflet() %>% 
  addProviderTiles(providers$Esri.WorldImagery) %>%
  #setView(st_coordinates(cntr_crds)[1], st_coordinates(cntr_crds)[2], zoom = 15) %>%
  fitBounds(lng1 = min(st_coordinates(sitesWGS247)[,1]),
            lng2 = max(st_coordinates(sitesWGS247)[,1]),
            lat1 = min(st_coordinates(sitesWGS247)[,2]),
            lat2 = max(st_coordinates(sitesWGS247)[,2])) %>%
  addPolygons(data = st_zm(WGS247), # removes z and m values, if present
              color = "black", weight = 1,
              fillOpacity = 1, fillColor = ~factpal(section)) %>%
  addCircleMarkers(data = sitesWGS247,
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
            values = as.character(sitesWGS247$panel),
            title = "Sample sites") %>%
  addLegend(position = "bottomright", pal = factpal, 
            opacity = 1,
            values = as.character(WGS247$section),
            title = "sections") %>%
  addScaleBar()

mapview::mapshot(m, file = "../../../lakeDsn/CIN/CH4-247/247_All.png", 
                 remove_controls = NULL)


# Main points
m <- leaflet() %>% 
  addProviderTiles(providers$Esri.WorldImagery) %>%
  fitBounds(lng1 = min(st_coordinates(sitesWGS247)[,1]),
            lng2 = max(st_coordinates(sitesWGS247)[,1]),
            lat1 = min(st_coordinates(sitesWGS247)[,2]),
            lat2 = max(st_coordinates(sitesWGS247)[,2])) %>%
  addPolygons(data = st_zm(WGS247), # removes z and m values, if present
              color = "black", weight = 1,
              fillOpacity = 1, fillColor = ~factpal(section)) %>%
  addCircleMarkers(data = filter(sitesWGS247, panel == "mainSites"),
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
            values = as.character(WGS247$section),
            title = "sections") %>%
  addScaleBar()

mapview::mapshot(m, file = "../../../lakeDsn/CIN/CH4-247/247_Main.png", 
                 remove_controls = NULL)


# OverSample points
m <- leaflet() %>% 
  addProviderTiles(providers$Esri.WorldImagery) %>%
  fitBounds(lng1 = min(st_coordinates(sitesWGS247)[,1]),
            lng2 = max(st_coordinates(sitesWGS247)[,1]),
            lat1 = min(st_coordinates(sitesWGS247)[,2]),
            lat2 = max(st_coordinates(sitesWGS247)[,2])) %>%
  addPolygons(data = st_zm(WGS247),  # removes z and m values, if present
              color = "black", weight = 1,
              fillOpacity = 1, fillColor = ~factpal(section)) %>%
  addCircleMarkers(data = filter(sitesWGS247, panel == "OverSamp"),
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
            values = as.character(WGS247$section),
            title = "sections") %>%
  addScaleBar()

mapview::mapshot(m, file = "../../../lakeDsn/CIN/CH4-247/247_Over.png", 
                 remove_controls = NULL)



# WRITE OBJECTS TO DISK-----------------
# write out table of sample sites for reference in field.  Must be WGS
write.table(sitesWGS247 %>%
              select(panel, siteID, section) %>%
              arrange(panel, section, siteID),
            file = "../../../lakeDsn/CIN/CH4-247/ch4_247Sites.txt",
            row.names = FALSE, sep="\t")


# geopackage
# can write all layers to a geopackage, which behaves much like a geodatabase in ArcGIS.

# Web Mercator for Web Map first

# write out all sites
st_write(obj = sitesMerc247, 
         dsn = file.path( "../../../lakeDsn/CIN/CH4-247", "merc247.gpkg"), 
         layer = "allSitesMerc247", # package appends 'main.' to layer name?
         append=FALSE, # this overwrites existing layer
         driver = "GPKG")

# write out all buffers
st_write(obj = sitesMerc247buffer, 
         dsn = file.path( "../../../lakeDsn/CIN/CH4-247", "merc247.gpkg"), 
         layer = "allSitesMerc247buffer", # package appends 'main.' to layer name?
         append=FALSE, # this overwrites existing layer
         driver = "GPKG")

# write out main sites
st_write(obj = filter(sitesMerc247, panel == "mainSites"),
         dsn = file.path( "../../../lakeDsn/CIN/CH4-247", "merc247.gpkg"), 
         layer = "mainSitesMerc247",
         append=FALSE, # this overwrites existing layer
         driver = "GPKG")

# write out main site buffers
st_write(obj = filter(sitesMerc247buffer, panel == "mainSites"),
         dsn = file.path( "../../../lakeDsn/CIN/CH4-247", "merc247.gpkg"), 
         layer = "mainSitesMerc247buffer",
         append=FALSE, # this overwrites existing layer
         driver = "GPKG")

# write out oversample sites
st_write(obj = filter(sitesMerc247, panel == "OverSamp"),
         dsn = file.path( "../../../lakeDsn/CIN/CH4-247", "merc247.gpkg"), 
         layer = "overSampSitesMerc247",
         append=FALSE, # this overwrites existing layer,
         driver = "GPKG")

# write out oversample site buffers
st_write(obj = filter(sitesMerc247buffer, panel == "OverSamp"),
         dsn = file.path( "../../../lakeDsn/CIN/CH4-247", "merc247.gpkg"), 
         layer = "overSampSitesMerc247buffer",
         append=FALSE, # this overwrites existing layer,
         driver = "GPKG")


# write out lake polygon
st_write(obj = merc247,
         dsn = file.path( "../../../lakeDsn/CIN/CH4-247", "merc247.gpkg"), 
         layer = "merc247",
         append=FALSE, # this overwrites existing layer,
         driver = "GPKG")


st_layers(file.path( "../../../lakeDsn/CIN/CH4-247", "merc247.gpkg"))


# WGS for ArcPad next

# write out all sites
st_write(obj = sitesWGS247, 
         dsn = file.path( "../../../lakeDsn/CIN/CH4-247", "WGS247.gpkg"), 
         layer = "allSitesWGS247", # package appends 'main.' to layer name?
         append=FALSE, # this overwrites existing layer
         driver = "GPKG")

# write out all buffers
st_write(obj = sitesWGS247buffer, 
         dsn = file.path( "../../../lakeDsn/CIN/CH4-247", "WGS247.gpkg"), 
         layer = "sitesWGS247buffer", # package appends 'main.' to layer name?
         append=FALSE, # this overwrites existing layer
         driver = "GPKG")

# write out main sites
st_write(obj = filter(sitesWGS247, panel == "mainSites"),
         dsn = file.path( "../../../lakeDsn/CIN/CH4-247", "WGS247.gpkg"), 
         layer = "mainSitesWGS247",
         append=FALSE, # this overwrites existing layer
         driver = "GPKG")

# write out main site buffers
st_write(obj = filter(sitesWGS247buffer, panel == "mainSites"),
         dsn = file.path( "../../../lakeDsn/CIN/CH4-247", "WGS247.gpkg"), 
         layer = "mainSitesWGS247buffer",
         append=FALSE, # this overwrites existing layer
         driver = "GPKG")

# write out oversample sites
st_write(obj = filter(sitesWGS247, panel == "OverSamp"),
         dsn = file.path( "../../../lakeDsn/CIN/CH4-247", "WGS247.gpkg"), 
         layer = "overSampSitesWGS247",
         append=FALSE, # this overwrites existing layer,
         driver = "GPKG")

# write out oversample site buffers
st_write(obj = filter(sitesWGS247buffer, panel == "OverSamp"),
         dsn = file.path( "../../../lakeDsn/CIN/CH4-247", "WGS247.gpkg"), 
         layer = "overSampSitesWGS247buffer",
         append=FALSE, # this overwrites existing layer,
         driver = "GPKG")


# write out lake polygon
st_write(obj = WGS247,
         dsn = file.path( "../../../lakeDsn/CIN/CH4-247", "WGS247.gpkg"), 
         layer = "WGS247",
         append=FALSE, # this overwrites existing layer,
         driver = "GPKG")


st_layers(file.path( "../../../lakeDsn/CIN/CH4-247", "WGS247.gpkg"))

