# STRATIFIED, EQUAL PROBABILITY GRTS DESIGN

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


# EMIGRANT LAKE (ch4-249)--------
# Design for CH4-249, Emigrant Reservoir.  Doing four designs for this lake
# based on anticipated water levels derived from Google Earth historic imagery:
# Original, high, low, and low.  This script is for the 'low' scenario.
# Two tribs on the south (Emigrant Cr. on SE and Hill Cr. on SW).
# ~100ft deep at dam.  Trib strata extends to a depth of 8M (26 FT).

# Read polygon shapefile
lowEqArea249 <- st_read(dsn = "../../../lakeDsn/R10/ch4_249 emigrant/ch4_249_low",
                             layer = "lowEqArea249")  # shapefile name
plot(lowEqArea249$geometry) # visualize polygon

# Check CRS, must be equal area for grts function
# EPSG code hard to decipher from output, CRS clearly reported as 'NAD_1983_Albers'
# Coordinate System of NAD 1983 Albers (WKID = 0)
st_crs(lowEqArea249) 
st_crs(lowEqArea249) == st_crs(5070) # True

# clean up attributes
lowEqArea249 <- lowEqArea249 %>%
  mutate(lakeName = "Emigrant",
         lakeSiteID = "ch4-249") %>%
  select(lakeName, lakeSiteID, Area_km2, strata, section)


# ATTRIBUTES----------
head(lowEqArea249) # review attributes

# summarize frame by section
temp <- with(lowEqArea249, tapply(Area_km2, section, sum))
temp <- round(addmargins(temp), 2)
temp


# SET UP FOR GRTS FUNCTION---------
# Call the set.seed function so that the survey designs can be replicateD
set.seed(4447864)

# Create the design list
### We decided to set the number of main sites in the tributary area to 5, since it is a relatively small area (0.4 sq km)
### the unequal probability splits the open water part of the lake into two sections of almost equal area
low249Dsgn <- list("open_water" = list(panel=c(mainSites=10),
                                       seltype="Equal",
                                       over=20),
                   "trib"=list(panel=c(mainSites=5), # 37.5 sites/km2
                               seltype="Equal",
                               over=5))
# create SpatialDesign object
lowSites249 <- grts(design=low249Dsgn,
                        DesignID="S", # SU for stratified, unequal
                        type.frame="area",
                        src.frame="sf.object",
                        sf.object=lowEqArea249,
                        stratum="strata",
                        shapefile=FALSE) # convert design object to sf, define prj, then write to .shp

# Print the initial six lines of the survey design
head(lowSites249)


# Print the survey design summary
summary(lowSites249)


# CRS--------------
class(lowSites249) # SpatialDesign object
lowSitesEqArea249 <- st_as_sf(lowSites249) # convert to sf object
st_crs(lowSitesEqArea249) # no coordinate reference system?
st_crs(lowSitesEqArea249) = 5070 # inherits from parent object, per Weber.

# project to WGS84 for plotting with leaflet, ArcPad, and writing out lat/long table  (5070 works fine for mapview)
lowWGS249 <- lowEqArea249 %>% st_transform(4326) # 4326 is WGS84, whereas 4269 is NAD83
lowSitesWGS249 <-  lowSitesEqArea249 %>% st_transform(4326) # 4326 is WGS84, whereas 4269 is NAD83 

# project to Web Mercator (3857) for use in ArcGIS Pro/geoplatform
lowMerc249 <- lowEqArea249 %>% st_transform(3857) 
lowSitesMerc249 <-  lowSitesEqArea249 %>% st_transform(3857) 

# BUFFER
# Create a 15m radius buffer around each sampling point
# Per ESRI, buffer should be created in Web Mercator, an equidistant projection, not Albers
lowSitesMerc249buffer <- st_buffer(lowSitesMerc249, 15) # radius of 15m, diameter of 30m. radius = 45ft = 2 boat lengths
lowSitesWGS249buffer <- lowSitesMerc249buffer %>% st_transform(4326) # for use in ArcPad


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
# ch4_249_lowAll.png works, but ch4_249_lowAllSites.png does not.


# leaflet colors
factpal <- colorFactor(topo.colors(3), lowWGS249$section)
factpal.points <- colorFactor(palette = c("red", "black"), domain = lowSitesWGS249$panel)

# All points
m <- leaflet() %>% 
  addProviderTiles(providers$Esri.WorldImagery) %>%
  #setView(st_coordinates(cntr_crds)[1], st_coordinates(cntr_crds)[2], zoom = 15) %>%
  fitBounds(lng1 = min(st_coordinates(lowSitesWGS249)[,1]),
            lng2 = max(st_coordinates(lowSitesWGS249)[,1]),
            lat1 = min(st_coordinates(lowSitesWGS249)[,2]),
            lat2 = max(st_coordinates(lowSitesWGS249)[,2])) %>%
  addPolygons(data = lowWGS249,
              color = "black", weight = 1,
              fillColor = ~factpal(section)) %>%
  addCircleMarkers(data = lowSitesWGS249,
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
            values = as.character(lowSitesWGS249$panel),
            title = "Sample sites") %>%
  addLegend(position = "bottomright", pal = factpal, 
            values = as.character(lowWGS249$section),
            title = "sections") %>%
  addScaleBar()

mapview::mapshot(m, file = "../../../lakeDsn/R10/ch4_249 emigrant/ch4_249_low/249_lowAll.png", 
                 remove_controls = NULL)


# Main points
m <- leaflet() %>% 
  addProviderTiles(providers$Esri.WorldImagery) %>%
  fitBounds(lng1 = min(st_coordinates(lowSitesWGS249)[,1]),
            lng2 = max(st_coordinates(lowSitesWGS249)[,1]),
            lat1 = min(st_coordinates(lowSitesWGS249)[,2]),
            lat2 = max(st_coordinates(lowSitesWGS249)[,2])) %>%
  addPolygons(data = lowWGS249,
              color = "black", weight = 1,
              fillColor = ~factpal(section)) %>%
  addCircleMarkers(data = filter(lowSitesWGS249, panel == "mainSites"),
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
            labels = "mainSites", # manual 'labels' specification
            title = "Sample sites") %>%
  addLegend(position = "bottomright", pal = factpal, 
            values = as.character(lowWGS249$section),
            title = "sections") %>%
  addScaleBar()

mapview::mapshot(m, file = "../../../lakeDsn/R10/ch4_249 emigrant/ch4_249_low/249_lowMain.png", 
                 remove_controls = NULL)


# OverSample points
m <- leaflet() %>% 
  addProviderTiles(providers$Esri.WorldImagery) %>%
  fitBounds(lng1 = min(st_coordinates(lowSitesWGS249)[,1]),
            lng2 = max(st_coordinates(lowSitesWGS249)[,1]),
            lat1 = min(st_coordinates(lowSitesWGS249)[,2]),
            lat2 = max(st_coordinates(lowSitesWGS249)[,2])) %>%
  addPolygons(data = lowWGS249,
              color = "black", weight = 1,
              fillColor = ~factpal(section)) %>%
  addCircleMarkers(data = filter(lowSitesWGS249, panel == "OverSamp"),
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
            labels = "OverSamp", # manual 'labels' specification
            title = "Sample sites") %>%
  addLegend(position = "bottomright", pal = factpal, 
            values = as.character(lowWGS249$section),
            title = "sections") %>%
  addScaleBar()

mapview::mapshot(m, file = "../../../lakeDsn/R10/ch4_249 emigrant/ch4_249_low/249_lowOver.png", 
                 remove_controls = NULL)



# WRITE OBJECTS TO DISK-----------------
# write out table of sample sites for reference in field.  Must be WGS
write.table(lowSitesWGS249 %>%
              select(panel, siteID, section) %>%
              arrange(panel, section, siteID),
            file = "../../../lakeDsn/R10/ch4_249 emigrant/ch4_249_low/ch4_249_lowSites.txt",
            row.names = FALSE, sep="\t")


# geopackage
# can write all layers to a geopackage, which behaves much like a geodatabase in ArcGIS.

# Web Mercator for Web Map first

# write out all sites
st_write(obj = lowSitesMerc249, 
         dsn = file.path( "../../../lakeDsn/R10/ch4_249 emigrant/ch4_249_low", "lowMerc249.gpkg"), 
         layer = "lowAllSitesMerc249", # package appends 'main.' to layer name?
         append=FALSE, # this overwrites existing layer
         driver = "GPKG")

# write out all buffers
st_write(obj = lowSitesMerc249buffer, 
         dsn = file.path( "../../../lakeDsn/R10/ch4_249 emigrant/ch4_249_low", "lowMerc249.gpkg"), 
         layer = "lowAllSitesMerc249buffer", # package appends 'main.' to layer name?
         append=FALSE, # this overwrites existing layer
         driver = "GPKG")

# write out main sites
st_write(obj = filter(lowSitesMerc249, panel == "mainSites"),
         dsn = file.path( "../../../lakeDsn/R10/ch4_249 emigrant/ch4_249_low", "lowMerc249.gpkg"), 
         layer = "lowMainSitesMerc249",
         append=FALSE, # this overwrites existing layer
         driver = "GPKG")

# write out main site buffers
st_write(obj = filter(lowSitesMerc249buffer, panel == "mainSites"),
         dsn = file.path( "../../../lakeDsn/R10/ch4_249 emigrant/ch4_249_low", "lowMerc249.gpkg"), 
         layer = "lowMainSitesMerc249buffer",
         append=FALSE, # this overwrites existing layer
         driver = "GPKG")

# write out oversample sites
st_write(obj = filter(lowSitesMerc249, panel == "OverSamp"),
         dsn = file.path( "../../../lakeDsn/R10/ch4_249 emigrant/ch4_249_low", "lowMerc249.gpkg"), 
         layer = "lowOverSampSitesMerc249",
         append=FALSE, # this overwrites existing layer
         driver = "GPKG")

# write out oversample site buffers
st_write(obj = filter(lowSitesMerc249buffer, panel == "OverSamp"),
         dsn = file.path( "../../../lakeDsn/R10/ch4_249 emigrant/ch4_249_low", "lowMerc249.gpkg"), 
         layer = "lowOverSampSitesMerc249buffer",
         append=FALSE, # this overwrites existing layer
         driver = "GPKG")


# write out lake polygon
st_write(obj = lowMerc249,
         dsn = file.path( "../../../lakeDsn/R10/ch4_249 emigrant/ch4_249_low", "lowMerc249.gpkg"), 
         layer = "lowMerc249",
         append=FALSE, # this overwrites existing layer
         driver = "GPKG")


st_layers(file.path( "../../../lakeDsn/R10/ch4_249 emigrant/ch4_249_low", "lowMerc249.gpkg"))


# WGS for ArcPad next

# write out all sites
st_write(obj = lowSitesWGS249, 
         dsn = file.path( "../../../lakeDsn/R10/ch4_249 emigrant/ch4_249_low", "lowWGS249.gpkg"), 
         layer = "lowAllSitesWGS249", # package appends 'main.' to layer name?
         append=FALSE, # this overwrites existing layer
         driver = "GPKG")

# write out all buffers
st_write(obj = lowSitesWGS249buffer, 
         dsn = file.path( "../../../lakeDsn/R10/ch4_249 emigrant/ch4_249_low", "lowWGS249.gpkg"), 
         layer = "lowSitesWGS249buffer", # package appends 'main.' to layer name?
         append=FALSE, # this overwrites existing layer
         driver = "GPKG")

# write out main sites
st_write(obj = filter(lowSitesWGS249, panel == "mainSites"),
         dsn = file.path( "../../../lakeDsn/R10/ch4_249 emigrant/ch4_249_low", "lowWGS249.gpkg"), 
         layer = "lowMainSitesWGS249",
         append=FALSE, # this overwrites existing layer
         driver = "GPKG")

# write out main site buffers
st_write(obj = filter(lowSitesWGS249buffer, panel == "mainSites"),
         dsn = file.path( "../../../lakeDsn/R10/ch4_249 emigrant/ch4_249_low", "lowWGS249.gpkg"), 
         layer = "lowMainSitesWGS249buffer",
         append=FALSE, # this overwrites existing layer
         driver = "GPKG")

# write out oversample sites
st_write(obj = filter(lowSitesWGS249, panel == "OverSamp"),
         dsn = file.path( "../../../lakeDsn/R10/ch4_249 emigrant/ch4_249_low", "lowWGS249.gpkg"), 
         layer = "lowOverSampSitesWGS249",
         append=FALSE, # this overwrites existing layer
         driver = "GPKG")

# write out oversample site buffers
st_write(obj = filter(lowSitesWGS249buffer, panel == "OverSamp"),
         dsn = file.path( "../../../lakeDsn/R10/ch4_249 emigrant/ch4_249_low", "lowWGS249.gpkg"), 
         layer = "lowOverSampSitesWGS249buffer",
         append=FALSE, # this overwrites existing layer
         driver = "GPKG")


# write out lake polygon
st_write(obj = lowWGS249,
         dsn = file.path( "../../../lakeDsn/R10/ch4_249 emigrant/ch4_249_low", "lowWGS249.gpkg"), 
         layer = "lowWGS249",
         append=FALSE, # this overwrites existing layer
         driver = "GPKG")


st_layers(file.path( "../../../lakeDsn/R10/ch4_249 emigrant/ch4_249_low", "lowWGS249.gpkg"))

