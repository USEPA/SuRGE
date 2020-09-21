# STRATIFIED, UNEQUAL PROBABILITY GRTS DESIGN

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


# BEULAH RESERVOIR----- 
# Design for Beulah Reservoir (ch4-238) per the original NHDPlusV2 polygon.  
# 7.2 km^2 with two trib arms (NW AND NE). NW trib is Malheur river and drains larger area
# that NE arm.  100' deep at dam.  Trib strata extends to a depth of 8M (26 FT). Basing
# bathymetry on Portland State University map.  Appears more accurate than modeled contours.

# Read polygon shapefile
originalEqArea238 <- st_read(dsn = "../../../lakeDsn/R10/ch4_238 beulah/ch4_238_original",
                             layer = "originalEqArea238")  # shapefile name
plot(originalEqArea238$geometry) # visualize polygon

# Check CRS, must be equal area for grts function
# EPSG code hard to decipher from output, CRS clearly reported as 'NAD_1983_Albers'
# Coordinate System of NAD 1983 Albers (WKID = 0)
st_crs(originalEqArea238) 
st_crs(originalEqArea238) == st_crs(5070) # True

# clean up attributes
originalEqArea238 <- originalEqArea238 %>%
  mutate(lakeName = "beulah",
         lakeSiteID = "ch4-238") %>%
  select(lakeName, lakeSiteID, Area_km2, strata, section)


# ATTRIBUTES----------
head(originalEqArea238) # review attributes

# summarize frame by section
temp <- with(originalEqArea238, tapply(Area_km2, section, sum))
temp <- round(addmargins(temp), 2)
temp


# SET UP FOR GRTS FUNCTION---------
# Call the set.seed function so that the survey designs can be replicateD
set.seed(4447864)

# Create the design list
original238Dsgn <- list("open_water" = list(panel=c(mainSites=10),
                                            seltype="Unequal",
                                            caty.n=c("north" = 6, # 
                                                     "south" = 4), # 
                                            over=20),
                        "trib"=list(panel=c(mainSites=5), # 
                                    seltype="Equal",
                                    over=10))
# create SpatialDesign object
originalSites238 <- grts(design=original238Dsgn,
                        DesignID="SU", # SU for stratified, unequal
                        type.frame="area",
                        src.frame="sf.object",
                        sf.object=originalEqArea238,
                        stratum="strata",
                        mdcaty="section",
                        shapefile=FALSE) # convert design object to sf, define prj, then write to .shp

# Print the initial six lines of the survey design
head(originalSites238)


# Print the survey design summary
summary(originalSites238)


# CRS--------------
class(originalSites238) # SpatialDesign object
originalSitesEqArea238 <- st_as_sf(originalSites238) # convert to sf object
st_crs(originalSitesEqArea238) # no coordinate reference system?
st_crs(originalSitesEqArea238) = 5070 # inherits from parent object, per Weber.

# project to WGS84 for plotting with leaflet, ArcPad, and writing out lat/long table  (5070 works fine for mapview)
originalWGS238 <- originalEqArea238 %>% st_transform(4326) # 4326 is WGS84, whereas 4269 is NAD83
originalSitesWGS238 <-  originalSitesEqArea238 %>% st_transform(4326) # 4326 is WGS84, whereas 4269 is NAD83 

# project to Web Mercator (3857) for use in ArcGIS Pro/geoplatform
originalMerc238 <- originalEqArea238 %>% st_transform(3857) 
originalSitesMerc238 <-  originalSitesEqArea238 %>% st_transform(3857) 

# BUFFER
# Create a 15m radius buffer around each sampling point
# Per ESRI, buffer should be created in Web Mercator, an equidistant projection, not Albers
originalSitesMerc238buffer <- st_buffer(originalSitesMerc238, 15) # radius of 15m, diameter of 30m. radius = 45ft = 2 boat lengths
originalSitesWGS238buffer <- originalSitesMerc38buffer %>% st_transform(4326) # for use in ArcPad


# MAPS---------------------
# Approach: generate static maps to paste in .rmd.  Knitted html can be printed for use in field.
# - ggmap requires an API key from google to provide satellite images.  Credit card required.  No thanks.
# - decided to make interactive map with leaflet, but capture image in .png with mapview::mapshot()
# - per Lil, field crews won't make use of interactive maps


# LEAFLET
# Mapview approach gets close, but I can't quite tweak everything the way I like
# Leaflet aloriginals much more control, requires WGS84 CRS.
# setView for zoo level requires manual fiddling with zoom level.  fitBounds() is cleaner
# mapshot() call seems sensitive to length of file name (sometimes?).  Try to keep simple.
# ch4_238_originalAll.png works, but ch4_238_originalAllSites.png does not.


# leaflet colors
factpal <- colorFactor(topo.colors(3), originalWGS238$section)
factpal.points <- colorFactor(palette = c("red", "black"), domain = originalSitesWGS238$panel)

# All points
m <- leaflet() %>% 
  addProviderTiles(providers$Esri.WorldImagery) %>%
  #setView(st_coordinates(cntr_crds)[1], st_coordinates(cntr_crds)[2], zoom = 15) %>%
  fitBounds(lng1 = min(st_coordinates(originalSitesWGS238)[,1]),
            lng2 = max(st_coordinates(originalSitesWGS238)[,1]),
            lat1 = min(st_coordinates(originalSitesWGS238)[,2]),
            lat2 = max(st_coordinates(originalSitesWGS238)[,2])) %>%
  addPolygons(data = originalWGS238,
              color = "black", weight = 1,
              fillColor = ~factpal(section)) %>%
  addCircleMarkers(data = originalSitesWGS238,
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
            values = as.character(originalSitesWGS238$panel),
            title = "Sample sites") %>%
  addLegend(position = "bottomright", pal = factpal, 
            values = as.character(originalWGS238$section),
            title = "sections") %>%
  addScaleBar()

mapview::mapshot(m, file = "../../../lakeDsn/R10/ch4_238 beulah/ch4_238_original/238_originalAll.png", 
                 remove_controls = NULL)


# Main points
m <- leaflet() %>% 
  addProviderTiles(providers$Esri.WorldImagery) %>%
  fitBounds(lng1 = min(st_coordinates(originalSitesWGS238)[,1]),
            lng2 = max(st_coordinates(originalSitesWGS238)[,1]),
            lat1 = min(st_coordinates(originalSitesWGS238)[,2]),
            lat2 = max(st_coordinates(originalSitesWGS238)[,2])) %>%
  addPolygons(data = originalWGS238,
              color = "black", weight = 1,
              fillColor = ~factpal(section)) %>%
  addCircleMarkers(data = filter(originalSitesWGS238, panel == "mainSites"),
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
            values = as.character(originalWGS238$section),
            title = "sections") %>%
  addScaleBar()

mapview::mapshot(m, file = "../../../lakeDsn/R10/ch4_238 beulah/ch4_238_original/238_originalMain.png", 
                 remove_controls = NULL)


# OverSample points
m <- leaflet() %>% 
  addProviderTiles(providers$Esri.WorldImagery) %>%
  fitBounds(lng1 = min(st_coordinates(originalSitesWGS238)[,1]),
            lng2 = max(st_coordinates(originalSitesWGS238)[,1]),
            lat1 = min(st_coordinates(originalSitesWGS238)[,2]),
            lat2 = max(st_coordinates(originalSitesWGS238)[,2])) %>%
  addPolygons(data = originalWGS238,
              color = "black", weight = 1,
              fillColor = ~factpal(section)) %>%
  addCircleMarkers(data = filter(originalSitesWGS238, panel == "OverSamp"),
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
            values = as.character(originalWGS238$section),
            title = "sections") %>%
  addScaleBar()

mapview::mapshot(m, file = "../../../lakeDsn/R10/ch4_238 beulah/ch4_238_original/238_originalOver.png", 
                 remove_controls = NULL)



# WRITE OBJECTS TO DISK-----------------
# write out table of sample sites for reference in field.  Must be WGS
write.table(originalSitesWGS238 %>%
              select(panel, siteID, section) %>%
              arrange(panel, section, siteID),
            file = "../../../lakeDsn/R10/ch4_238 beulah/ch4_238_original/ch4_238_originalSites.txt",
            row.names = FALSE, sep="\t")


# geopackage
# can write all layers to a geopackage, which behaves much like a geodatabase in ArcGIS.

# Web Mercator for Web Map first

# write out all sites
st_write(obj = originalSitesMerc238, 
         dsn = file.path( "../../../lakeDsn/R10/ch4_238 beulah/ch4_238_original", "originalMerc238.gpkg"), 
         layer = "originalAllSitesMerc238", # package appends 'main.' to layer name?
         append=FALSE, # this overwrites existing layer
         driver = "GPKG")

# write out all buffers
st_write(obj = originalSitesMerc238buffer, 
         dsn = file.path( "../../../lakeDsn/R10/ch4_238 beulah/ch4_238_original", "originalMerc238.gpkg"), 
         layer = "originalAllSitesMerc238buffer", # package appends 'main.' to layer name?
         append=FALSE, # this overwrites existing layer
         driver = "GPKG")

# write out main sites
st_write(obj = filter(originalSitesMerc238, panel == "mainSites"),
         dsn = file.path( "../../../lakeDsn/R10/ch4_238 beulah/ch4_238_original", "originalMerc238.gpkg"), 
         layer = "originalMainSitesMerc238",
         append=FALSE, # this overwrites existing layer
         driver = "GPKG")

# write out main site buffers
st_write(obj = filter(originalSitesMerc238buffer, panel == "mainSites"),
         dsn = file.path( "../../../lakeDsn/R10/ch4_238 beulah/ch4_238_original", "originalMerc238.gpkg"), 
         layer = "originalMainSitesMerc238buffer",
         append=FALSE, # this overwrites existing layer
         driver = "GPKG")

# write out oversample sites
st_write(obj = filter(originalSitesMerc238, panel == "OverSamp"),
         dsn = file.path( "../../../lakeDsn/R10/ch4_238 beulah/ch4_238_original", "originalMerc238.gpkg"), 
         layer = "originalOverSampSitesMerc238",
         append=FALSE, # this overwrites existing layer,
         driver = "GPKG")

# write out oversample site buffers
st_write(obj = filter(originalSitesMerc238buffer, panel == "OverSamp"),
         dsn = file.path( "../../../lakeDsn/R10/ch4_238 beulah/ch4_238_original", "originalMerc238.gpkg"), 
         layer = "originalOverSampSitesMerc238buffer",
         append=FALSE, # this overwrites existing layer,
         driver = "GPKG")


# write out lake polygon
st_write(obj = originalMerc238,
         dsn = file.path( "../../../lakeDsn/R10/ch4_238 beulah/ch4_238_original", "originalMerc238.gpkg"), 
         layer = "originalMerc238",
         append=FALSE, # this overwrites existing layer,
         driver = "GPKG")


st_layers(file.path( "../../../lakeDsn/R10/ch4_238 beulah/ch4_238_original", "originalMerc238.gpkg"))


# WGS for ArcPad next

# write out all sites
st_write(obj = originalSitesWGS238, 
         dsn = file.path( "../../../lakeDsn/R10/ch4_238 beulah/ch4_238_original", "originalWGS238.gpkg"), 
         layer = "originalAllSitesWGS238", # package appends 'main.' to layer name?
         append=FALSE, # this overwrites existing layer
         driver = "GPKG")

# write out all buffers
st_write(obj = originalSitesWGS238buffer, 
         dsn = file.path( "../../../lakeDsn/R10/ch4_238 beulah/ch4_238_original", "originalWGS238.gpkg"), 
         layer = "originalSitesWGS238buffer", # package appends 'main.' to layer name?
         append=FALSE, # this overwrites existing layer
         driver = "GPKG")

# write out main sites
st_write(obj = filter(originalSitesWGS238, panel == "mainSites"),
         dsn = file.path( "../../../lakeDsn/R10/ch4_238 beulah/ch4_238_original", "originalWGS238.gpkg"), 
         layer = "originalMainSitesWGS238",
         append=FALSE, # this overwrites existing layer
         driver = "GPKG")

# write out main site buffers
st_write(obj = filter(originalSitesWGS238buffer, panel == "mainSites"),
         dsn = file.path( "../../../lakeDsn/R10/ch4_238 beulah/ch4_238_original", "originalWGS238.gpkg"), 
         layer = "originalMainSitesWGS238buffer",
         append=FALSE, # this overwrites existing layer
         driver = "GPKG")

# write out oversample sites
st_write(obj = filter(originalSitesWGS238, panel == "OverSamp"),
         dsn = file.path( "../../../lakeDsn/R10/ch4_238 beulah/ch4_238_original", "originalWGS238.gpkg"), 
         layer = "originalOverSampSitesWGS238",
         append=FALSE, # this overwrites existing layer,
         driver = "GPKG")

# write out oversample site buffers
st_write(obj = filter(originalSitesWGS238buffer, panel == "OverSamp"),
         dsn = file.path( "../../../lakeDsn/R10/ch4_238 beulah/ch4_238_original", "originalWGS238.gpkg"), 
         layer = "originalOverSampSitesWGS238buffer",
         append=FALSE, # this overwrites existing layer,
         driver = "GPKG")


# write out lake polygon
st_write(obj = originalWGS238,
         dsn = file.path( "../../../lakeDsn/R10/ch4_238 beulah/ch4_238_original", "originalWGS238.gpkg"), 
         layer = "originalWGS238",
         append=FALSE, # this overwrites existing layer,
         driver = "GPKG")


st_layers(file.path( "../../../lakeDsn/R10/ch4_238 beulah/ch4_238_original", "originalWGS238.gpkg"))

