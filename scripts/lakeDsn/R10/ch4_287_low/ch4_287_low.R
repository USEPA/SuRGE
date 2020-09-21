# UNSTRATIFIED UNEQUAL PROBABILITY GRTS DESIGN

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


# OWHYEE LAKE (ch4-287)--------
# Design for CH4-287, Owhyee Lake, based on NHD lake polygon
# modified to reflect lower water levels commonly encountered during summer
# Very long (~30 miles).  Unstratified, but broken into 5 sections to facilitate oversample sites.

# Read polygon shapefile
lowEqArea287 <- st_read(dsn = "../../../lakeDsn/R10/ch4_287 owyhee/ch4_287_low",
                             layer = "lowEqArea287")  # shapefile name
plot(lowEqArea287$geometry) # visualize polygon

# Check CRS, must be equal area for grts function
# EPSG code hard to decipher from output, CRS clearly reported as 'NAD_1983_Albers'
# Coordinate System of NAD 1983 Albers (WKID = 0)
st_crs(lowEqArea287) 
st_crs(lowEqArea287) == st_crs(5070) # True

# clean up attributes
lowEqArea287 <- lowEqArea287 %>%
  mutate(lakeName = "owyhee",
         lakeSiteID = "ch4-287") %>%
  select(lakeName, lakeSiteID, Area_km2, section)


# ATTRIBUTES----------
head(lowEqArea287) # review attributes



# SET UP FOR GRTS FUNCTION---------
# Call the set.seed function so that the survey designs can be replicateD
set.seed(4447864)

# Create the design list
low287Dsgn <- list(None = list(panel=c(mainSites=15),
                               seltype="Unequal",
                               caty.n =c("1" = 3,
                                         "2" = 3,
                                         "3" = 3,
                                         "4" = 3,
                                         "5" = 3),
                               over=20))
# create SpatialDesign object
lowSites287 <- grts(design=low287Dsgn,
                    DesignID="U", # U for unstratified
                    type.frame="area",
                    src.frame="sf.object",
                    sf.object=lowEqArea287,
                    mdcaty = "section",
                    shapefile=FALSE) # convert design object to sf, define prj, then write to .shp

# Print the initial six lines of the survey design
head(lowSites287)


# Print the survey design summary
summary(lowSites287)


# CRS--------------
class(lowSites287) # SpatialDesign object
lowSitesEqArea287 <- st_as_sf(lowSites287) # convert to sf object
st_crs(lowSitesEqArea287) # no coordinate reference system?
st_crs(lowSitesEqArea287) = 5070 # inherits from parent object, per Weber.

# project to WGS84 for plotting with leaflet, ArcPad, and writing out lat/long table  (5070 works fine for mapview)
lowWGS287 <- lowEqArea287 %>% st_transform(4326) # 4326 is WGS84, whereas 4269 is NAD83
lowSitesWGS287 <-  lowSitesEqArea287 %>% st_transform(4326) # 4326 is WGS84, whereas 4269 is NAD83 

# project to Web Mercator (3857) for use in ArcGIS Pro/geoplatform
lowMerc287 <- lowEqArea287 %>% st_transform(3857) 
lowSitesMerc287 <-  lowSitesEqArea287 %>% st_transform(3857) 

# BUFFER
# Create a 15m radius buffer around each sampling point
# Per ESRI, buffer should be created in Web Mercator, an equidistant projection, not Albers
lowSitesMerc287buffer <- st_buffer(lowSitesMerc287, 15) # radius of 15m, diameter of 30m. radius = 45ft = 2 boat lengths
lowSitesWGS287buffer <- lowSitesMerc287buffer %>% st_transform(4326) # for use in ArcPad


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
# ch4_287_lowAll.png works, but ch4_287_lowAllSites.png does not.


# leaflet colors
factpal <- colorFactor(topo.colors(5), lowWGS287$section)
factpal.points <- colorFactor(palette = c("red", "black"), domain = lowSitesWGS287$panel)

# All points
m <- leaflet() %>% 
  addProviderTiles(providers$Esri.WorldImagery) %>%
  #setView(st_coordinates(cntr_crds)[1], st_coordinates(cntr_crds)[2], zoom = 15) %>%
  fitBounds(lng1 = min(st_coordinates(lowSitesWGS287)[,1]),
            lng2 = max(st_coordinates(lowSitesWGS287)[,1]),
            lat1 = min(st_coordinates(lowSitesWGS287)[,2]),
            lat2 = max(st_coordinates(lowSitesWGS287)[,2])) %>%
  addPolygons(data = st_zm(lowWGS287),  # removes z and m values, if present
              color = "black", weight = 1,
              fillOpacity = 1, fillColor = ~factpal(section)) %>%
  addCircleMarkers(data = lowSitesWGS287,
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
            values = as.character(lowSitesWGS287$panel),
            title = "Sample sites") %>%
  addLegend(position = "bottomright", pal = factpal, 
            values = as.character(lowWGS287$section),
            title = "sections") %>%
  addScaleBar()

mapview::mapshot(m, file = "../../../lakeDsn/R10/ch4_287 owyhee/ch4_287_low/287_lowAll.png", 
                 remove_controls = NULL)


# Main points
m <- leaflet() %>% 
  addProviderTiles(providers$Esri.WorldImagery) %>%
  fitBounds(lng1 = min(st_coordinates(lowSitesWGS287)[,1]),
            lng2 = max(st_coordinates(lowSitesWGS287)[,1]),
            lat1 = min(st_coordinates(lowSitesWGS287)[,2]),
            lat2 = max(st_coordinates(lowSitesWGS287)[,2])) %>%
  addPolygons(data = st_zm(lowWGS287), # removes z and m values, if present
              color = "black", weight = 1,
              fillOpacity = 1, fillColor = ~factpal(section)) %>%
  addCircleMarkers(data = filter(lowSitesWGS287, panel == "mainSites"),
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
            values = as.character(lowWGS287$section),
            title = "sections") %>%
  addScaleBar()

mapview::mapshot(m, file = "../../../lakeDsn/R10/ch4_287 owyhee/ch4_287_low/287_lowMain.png", 
                 remove_controls = NULL)


# OverSample points
m <- leaflet() %>% 
  addProviderTiles(providers$Esri.WorldImagery) %>%
  fitBounds(lng1 = min(st_coordinates(lowSitesWGS287)[,1]),
            lng2 = max(st_coordinates(lowSitesWGS287)[,1]),
            lat1 = min(st_coordinates(lowSitesWGS287)[,2]),
            lat2 = max(st_coordinates(lowSitesWGS287)[,2])) %>%
  addPolygons(data = st_zm(lowWGS287), # removes z and m values, if present
              color = "black", weight = 1,
              fillOpacity = 1, fillColor = ~factpal(section)) %>%
  addCircleMarkers(data = filter(lowSitesWGS287, panel == "OverSamp"),
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
            values = as.character(lowWGS287$section),
            title = "sections") %>%
  addScaleBar()

mapview::mapshot(m, file = "../../../lakeDsn/R10/ch4_287 owyhee/ch4_287_low/287_lowOver.png", 
                 remove_controls = NULL)



# WRITE OBJECTS TO DISK-----------------
# write out table of sample sites for reference in field.  Must be WGS
write.table(lowSitesWGS287 %>%
              select(panel, siteID) %>%
              arrange(panel, siteID),
            file = "../../../lakeDsn/R10/ch4_287 owyhee/ch4_287_low/ch4_287_lowSites.txt",
            row.names = FALSE, sep="\t")


# geopackage
# can write all layers to a geopackage, which behaves much like a geodatabase in ArcGIS.

# Web Mercator for Web Map first

# write out all sites
st_write(obj = lowSitesMerc287, 
         dsn = file.path( "../../../lakeDsn/R10/ch4_287 owyhee/ch4_287_low", "lowMerc287.gpkg"), 
         layer = "lowAllSitesMerc287", # package appends 'main.' to layer name?
         append=FALSE, # this overwrites existing layer
         driver = "GPKG")

# write out all buffers
st_write(obj = lowSitesMerc287buffer, 
         dsn = file.path( "../../../lakeDsn/R10/ch4_287 owyhee/ch4_287_low", "lowMerc287.gpkg"), 
         layer = "lowAllSitesMerc287buffer", # package appends 'main.' to layer name?
         append=FALSE, # this overwrites existing layer
         driver = "GPKG")

# write out main sites
st_write(obj = filter(lowSitesMerc287, panel == "mainSites"),
         dsn = file.path( "../../../lakeDsn/R10/ch4_287 owyhee/ch4_287_low", "lowMerc287.gpkg"), 
         layer = "lowMainSitesMerc287",
         append=FALSE, # this overwrites existing layer
         driver = "GPKG")

# write out main site buffers
st_write(obj = filter(lowSitesMerc287buffer, panel == "mainSites"),
         dsn = file.path( "../../../lakeDsn/R10/ch4_287 owyhee/ch4_287_low", "lowMerc287.gpkg"), 
         layer = "lowMainSitesMerc287buffer",
         append=FALSE, # this overwrites existing layer
         driver = "GPKG")

# write out oversample sites
st_write(obj = filter(lowSitesMerc287, panel == "OverSamp"),
         dsn = file.path( "../../../lakeDsn/R10/ch4_287 owyhee/ch4_287_low", "lowMerc287.gpkg"), 
         layer = "lowOverSampSitesMerc287",
         append=FALSE, # this overwrites existing layer,
         driver = "GPKG")

# write out oversample site buffers
st_write(obj = filter(lowSitesMerc287buffer, panel == "OverSamp"),
         dsn = file.path( "../../../lakeDsn/R10/ch4_287 owyhee/ch4_287_low", "lowMerc287.gpkg"), 
         layer = "lowOverSampSitesMerc287buffer",
         append=FALSE, # this overwrites existing layer,
         driver = "GPKG")


# write out lake polygon
st_write(obj = lowMerc287,
         dsn = file.path( "../../../lakeDsn/R10/ch4_287 owyhee/ch4_287_low", "lowMerc287.gpkg"), 
         layer = "lowMerc287",
         append=FALSE, # this overwrites existing layer,
         driver = "GPKG")


st_layers(file.path( "../../../lakeDsn/R10/ch4_287 owyhee/ch4_287_low", "lowMerc287.gpkg"))


# WGS for ArcPad next

# write out all sites
st_write(obj = lowSitesWGS287, 
         dsn = file.path( "../../../lakeDsn/R10/ch4_287 owyhee/ch4_287_low", "lowWGS287.gpkg"), 
         layer = "lowAllSitesWGS287", # package appends 'main.' to layer name?
         append=FALSE, # this overwrites existing layer
         driver = "GPKG")

# write out all buffers
st_write(obj = lowSitesWGS287buffer, 
         dsn = file.path( "../../../lakeDsn/R10/ch4_287 owyhee/ch4_287_low", "lowWGS287.gpkg"), 
         layer = "lowSitesWGS287buffer", # package appends 'main.' to layer name?
         append=FALSE, # this overwrites existing layer
         driver = "GPKG")

# write out main sites
st_write(obj = filter(lowSitesWGS287, panel == "mainSites"),
         dsn = file.path( "../../../lakeDsn/R10/ch4_287 owyhee/ch4_287_low", "lowWGS287.gpkg"), 
         layer = "lowMainSitesWGS287",
         append=FALSE, # this overwrites existing layer
         driver = "GPKG")

# write out main site buffers
st_write(obj = filter(lowSitesWGS287buffer, panel == "mainSites"),
         dsn = file.path( "../../../lakeDsn/R10/ch4_287 owyhee/ch4_287_low", "lowWGS287.gpkg"), 
         layer = "lowMainSitesWGS287buffer",
         append=FALSE, # this overwrites existing layer
         driver = "GPKG")

# write out oversample sites
st_write(obj = filter(lowSitesWGS287, panel == "OverSamp"),
         dsn = file.path( "../../../lakeDsn/R10/ch4_287 owyhee/ch4_287_low", "lowWGS287.gpkg"), 
         layer = "lowOverSampSitesWGS287",
         append=FALSE, # this overwrites existing layer,
         driver = "GPKG")

# write out oversample site buffers
st_write(obj = filter(lowSitesWGS287buffer, panel == "OverSamp"),
         dsn = file.path( "../../../lakeDsn/R10/ch4_287 owyhee/ch4_287_low", "lowWGS287.gpkg"), 
         layer = "lowOverSampSitesWGS287buffer",
         append=FALSE, # this overwrites existing layer,
         driver = "GPKG")


# write out lake polygon
st_write(obj = lowWGS287,
         dsn = file.path( "../../../lakeDsn/R10/ch4_287 owyhee/ch4_287_low", "lowWGS287.gpkg"), 
         layer = "lowWGS287",
         append=FALSE, # this overwrites existing layer,
         driver = "GPKG")


st_layers(file.path( "../../../lakeDsn/R10/ch4_287 owyhee/ch4_287_low", "lowWGS287.gpkg"))

