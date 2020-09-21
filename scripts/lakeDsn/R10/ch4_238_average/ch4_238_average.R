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
# Design for Beulah Reservoir (ch4-238) per the average NHDPlusV2 polygon.  
# 7.2 km^2 with two trib arms (NW AND NE). NW trib is Malheur river and drains larger area
# that NE arm.  100' deep at dam.  Trib strata extends to a depth of 8M (26 FT). Basing
# bathymetry on Portland State University map.  Appears more accurate than modeled contours.

# Read polygon shapefile
averageEqArea238 <- st_read(dsn = "../../../lakeDsn/R10/ch4_238 beulah/ch4_238_average",
                             layer = "averageEqArea238")  # shapefile name
plot(averageEqArea238$geometry) # visualize polygon

# Check CRS, must be equal area for grts function
# EPSG code hard to decipher from output, CRS clearly reported as 'NAD_1983_Albers'
# Coordinate System of NAD 1983 Albers (WKID = 0)
st_crs(averageEqArea238) 
st_crs(averageEqArea238) == st_crs(5070) # True

# clean up attributes
averageEqArea238 <- averageEqArea238 %>%
  mutate(lakeName = "beulah",
         lakeSiteID = "ch4-238") %>%
  select(lakeName, lakeSiteID, Area_km2, strata, section)


# ATTRIBUTES----------
head(averageEqArea238) # review attributes

# summarize frame by section
temp <- with(averageEqArea238, tapply(Area_km2, section, sum))
temp <- round(addmargins(temp), 2)
temp


# SET UP FOR GRTS FUNCTION---------
# Call the set.seed function so that the survey designs can be replicateD
set.seed(4447864)

# Create the design list
average238Dsgn <- list("open_water" = list(panel=c(mainSites=10),
                                            seltype="Unequal",
                                            caty.n=c("north" = 5, # 
                                                     "south" = 5), # 
                                            over=20),
                        "trib"=list(panel=c(mainSites=5), # 
                                    seltype="Equal",
                                    over=10))
# create SpatialDesign object
averageSites238 <- grts(design=average238Dsgn,
                        DesignID="SU", # SU for stratified, unequal
                        type.frame="area",
                        src.frame="sf.object",
                        sf.object=averageEqArea238,
                        stratum="strata",
                        mdcaty="section",
                        shapefile=FALSE) # convert design object to sf, define prj, then write to .shp

# Print the initial six lines of the survey design
head(averageSites238)


# Print the survey design summary
summary(averageSites238)


# CRS--------------
class(averageSites238) # SpatialDesign object
averageSitesEqArea238 <- st_as_sf(averageSites238) # convert to sf object
st_crs(averageSitesEqArea238) # no coordinate reference system?
st_crs(averageSitesEqArea238) = 5070 # inherits from parent object, per Weber.

# project to WGS84 for plotting with leaflet, ArcPad, and writing out lat/long table  (5070 works fine for mapview)
averageWGS238 <- averageEqArea238 %>% st_transform(4326) # 4326 is WGS84, whereas 4269 is NAD83
averageSitesWGS238 <-  averageSitesEqArea238 %>% st_transform(4326) # 4326 is WGS84, whereas 4269 is NAD83 

# project to Web Mercator (3857) for use in ArcGIS Pro/geoplatform
averageMerc238 <- averageEqArea238 %>% st_transform(3857) 
averageSitesMerc238 <-  averageSitesEqArea238 %>% st_transform(3857) 

# BUFFER
# Create a 15m radius buffer around each sampling point
# Per ESRI, buffer should be created in Web Mercator, an equidistant projection, not Albers
averageSitesMerc238buffer <- st_buffer(averageSitesMerc238, 15) # radius of 15m, diameter of 30m. radius = 45ft = 2 boat lengths
averageSitesWGS238buffer <- averageSitesWGS238buffer %>% st_transform(4326) # for use in ArcPad


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
# ch4_238_averageAll.png works, but ch4_238_averageAllSites.png does not.


# leaflet colors
factpal <- colorFactor(topo.colors(3), averageWGS238$section)
factpal.points <- colorFactor(palette = c("red", "black"), domain = averageSitesWGS238$panel)

# All points
m <- leaflet() %>% 
  addProviderTiles(providers$Esri.WorldImagery) %>%
  #setView(st_coordinates(cntr_crds)[1], st_coordinates(cntr_crds)[2], zoom = 15) %>%
  fitBounds(lng1 = min(st_coordinates(averageSitesWGS238)[,1]),
            lng2 = max(st_coordinates(averageSitesWGS238)[,1]),
            lat1 = min(st_coordinates(averageSitesWGS238)[,2]),
            lat2 = max(st_coordinates(averageSitesWGS238)[,2])) %>%
  addPolygons(data = averageWGS238,
              color = "black", weight = 1,
              fillColor = ~factpal(section)) %>%
  addCircleMarkers(data = averageSitesWGS238,
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
            values = as.character(averageSitesWGS238$panel),
            title = "Sample sites") %>%
  addLegend(position = "bottomright", pal = factpal, 
            values = as.character(averageWGS238$section),
            title = "sections") %>%
  addScaleBar()

mapview::mapshot(m, file = "../../../lakeDsn/R10/ch4_238 beulah/ch4_238_average/238_averageAll.png", 
                 remove_controls = NULL)


# Main points
m <- leaflet() %>% 
  addProviderTiles(providers$Esri.WorldImagery) %>%
  fitBounds(lng1 = min(st_coordinates(averageSitesWGS238)[,1]),
            lng2 = max(st_coordinates(averageSitesWGS238)[,1]),
            lat1 = min(st_coordinates(averageSitesWGS238)[,2]),
            lat2 = max(st_coordinates(averageSitesWGS238)[,2])) %>%
  addPolygons(data = averageWGS238,
              color = "black", weight = 1,
              fillColor = ~factpal(section)) %>%
  addCircleMarkers(data = filter(averageSitesWGS238, panel == "mainSites"),
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
            values = as.character(averageWGS238$section),
            title = "sections") %>%
  addScaleBar()

mapview::mapshot(m, file = "../../../lakeDsn/R10/ch4_238 beulah/ch4_238_average/238_averageMain.png", 
                 remove_controls = NULL)


# OverSample points
m <- leaflet() %>% 
  addProviderTiles(providers$Esri.WorldImagery) %>%
  fitBounds(lng1 = min(st_coordinates(averageSitesWGS238)[,1]),
            lng2 = max(st_coordinates(averageSitesWGS238)[,1]),
            lat1 = min(st_coordinates(averageSitesWGS238)[,2]),
            lat2 = max(st_coordinates(averageSitesWGS238)[,2])) %>%
  addPolygons(data = averageWGS238,
              color = "black", weight = 1,
              fillColor = ~factpal(section)) %>%
  addCircleMarkers(data = filter(averageSitesWGS238, panel == "OverSamp"),
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
            values = as.character(averageWGS238$section),
            title = "sections") %>%
  addScaleBar()

mapview::mapshot(m, file = "../../../lakeDsn/R10/ch4_238 beulah/ch4_238_average/238_averageOver.png", 
                 remove_controls = NULL)



# WRITE OBJECTS TO DISK-----------------
# write out table of sample sites for reference in field.  Must be WGS
write.table(averageSitesWGS238 %>%
              select(panel, siteID, section) %>%
              arrange(panel, section, siteID),
            file = "../../../lakeDsn/R10/ch4_238 beulah/ch4_238_average/ch4_238_averageSites.txt",
            row.names = FALSE, sep="\t")


# geopackage
# can write all layers to a geopackage, which behaves much like a geodatabase in ArcGIS.

# Web Mercator for Web Map first

# write out all sites
st_write(obj = averageSitesMerc238, 
         dsn = file.path( "../../../lakeDsn/R10/ch4_238 beulah/ch4_238_average", "averageMerc238.gpkg"), 
         layer = "averageAllSitesMerc238", # package appends 'main.' to layer name?
         append=FALSE, # this overwrites existing layer
         driver = "GPKG")

# write out all buffers
st_write(obj = averageSitesMerc238buffer, 
         dsn = file.path( "../../../lakeDsn/R10/ch4_238 beulah/ch4_238_average", "averageMerc238.gpkg"), 
         layer = "averageAllSitesMerc238buffer", # package appends 'main.' to layer name?
         append=FALSE, # this overwrites existing layer
         driver = "GPKG")

# write out main sites
st_write(obj = filter(averageSitesMerc238, panel == "mainSites"),
         dsn = file.path( "../../../lakeDsn/R10/ch4_238 beulah/ch4_238_average", "averageMerc238.gpkg"), 
         layer = "averageMainSitesMerc238",
         append=FALSE, # this overwrites existing layer
         driver = "GPKG")

# write out main site buffers
st_write(obj = filter(averageSitesMerc238buffer, panel == "mainSites"),
         dsn = file.path( "../../../lakeDsn/R10/ch4_238 beulah/ch4_238_average", "averageMerc238.gpkg"), 
         layer = "averageMainSitesMerc238buffer",
         append=FALSE, # this overwrites existing layer
         driver = "GPKG")

# write out oversample sites
st_write(obj = filter(averageSitesMerc238, panel == "OverSamp"),
         dsn = file.path( "../../../lakeDsn/R10/ch4_238 beulah/ch4_238_average", "averageMerc238.gpkg"), 
         layer = "averageOverSampSitesMerc238",
         append=FALSE, # this overwrites existing layer,
         driver = "GPKG")

# write out oversample site buffers
st_write(obj = filter(averageSitesMerc238buffer, panel == "OverSamp"),
         dsn = file.path( "../../../lakeDsn/R10/ch4_238 beulah/ch4_238_average", "averageMerc238.gpkg"), 
         layer = "averageOverSampSitesMerc238buffer",
         append=FALSE, # this overwrites existing layer,
         driver = "GPKG")


# write out lake polygon
st_write(obj = averageMerc238,
         dsn = file.path( "../../../lakeDsn/R10/ch4_238 beulah/ch4_238_average", "averageMerc238.gpkg"), 
         layer = "averageMerc238",
         append=FALSE, # this overwrites existing layer,
         driver = "GPKG")


st_layers(file.path( "../../../lakeDsn/R10/ch4_238 beulah/ch4_238_average", "averageMerc238.gpkg"))


# WGS for ArcPad next

# write out all sites
st_write(obj = averageSitesWGS238, 
         dsn = file.path( "../../../lakeDsn/R10/ch4_238 beulah/ch4_238_average", "averageWGS238.gpkg"), 
         layer = "averageAllSitesWGS238", # package appends 'main.' to layer name?
         append=FALSE, # this overwrites existing layer
         driver = "GPKG")

# write out all buffers
st_write(obj = averageSitesWGS238buffer, 
         dsn = file.path( "../../../lakeDsn/R10/ch4_238 beulah/ch4_238_average", "averageWGS238.gpkg"), 
         layer = "averageSitesWGS238buffer", # package appends 'main.' to layer name?
         append=FALSE, # this overwrites existing layer
         driver = "GPKG")

# write out main sites
st_write(obj = filter(averageSitesWGS238, panel == "mainSites"),
         dsn = file.path( "../../../lakeDsn/R10/ch4_238 beulah/ch4_238_average", "averageWGS238.gpkg"), 
         layer = "averageMainSitesWGS238",
         append=FALSE, # this overwrites existing layer
         driver = "GPKG")

# write out main site buffers
st_write(obj = filter(averageSitesWGS238buffer, panel == "mainSites"),
         dsn = file.path( "../../../lakeDsn/R10/ch4_238 beulah/ch4_238_average", "averageWGS238.gpkg"), 
         layer = "averageMainSitesWGS238buffer",
         append=FALSE, # this overwrites existing layer
         driver = "GPKG")

# write out oversample sites
st_write(obj = filter(averageSitesWGS238, panel == "OverSamp"),
         dsn = file.path( "../../../lakeDsn/R10/ch4_238 beulah/ch4_238_average", "averageWGS238.gpkg"), 
         layer = "averageOverSampSitesWGS238",
         append=FALSE, # this overwrites existing layer,
         driver = "GPKG")

# write out oversample site buffers
st_write(obj = filter(averageSitesWGS238buffer, panel == "OverSamp"),
         dsn = file.path( "../../../lakeDsn/R10/ch4_238 beulah/ch4_238_average", "averageWGS238.gpkg"), 
         layer = "averageOverSampSitesWGS238buffer",
         append=FALSE, # this overwrites existing layer,
         driver = "GPKG")


# write out lake polygon
st_write(obj = averageWGS238,
         dsn = file.path( "../../../lakeDsn/R10/ch4_238 beulah/ch4_238_average", "averageWGS238.gpkg"), 
         layer = "averageWGS238",
         append=FALSE, # this overwrites existing layer,
         driver = "GPKG")


st_layers(file.path( "../../../lakeDsn/R10/ch4_238 beulah/ch4_238_average", "averageWGS238.gpkg"))

