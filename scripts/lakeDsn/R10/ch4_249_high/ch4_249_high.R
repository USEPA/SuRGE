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


# EMIGRANT LAKE (ch4-249)--------
# Design for CH4-249, Emigrant Reservoir.  Doing four designs for this lake
# based on anticipated water levels derived from Google Earth historic imagery:
# Original, high, mid, and low.  This script is for the 'High' scenario, which
# is actually a smaller lake polygon than the original NHD polygon.
# Two tribs on the south (Emigrant Cr. on SE and Hill Cr. on SW).
# ~100ft deep at dam.  Trib strata extends to a depth of 8M (26 FT).

# Read polygon shapefile
highEqArea249 <- st_read(dsn = "../../../lakeDsn/R10/ch4_249 emigrant/ch4_249_high",
                             layer = "highEqArea249")  # shapefile name
plot(highEqArea249$geometry) # visualize polygon

# Check CRS, must be equal area for grts function
# EPSG code hard to decipher from output, CRS clearly reported as 'NAD_1983_Albers'
# Coordinate System of NAD 1983 Albers (WKID = 0)
st_crs(highEqArea249) 
st_crs(highEqArea249) == st_crs(5070) # True

# clean up attributes
highEqArea249 <- highEqArea249 %>%
  mutate(lakeName = "Emigrant",
         lakeSiteID = "ch4-249") %>%
  select(lakeName, lakeSiteID, Area_km2, strata, section)


# ATTRIBUTES----------
head(highEqArea249) # review attributes

# summarize frame by section
temp <- with(highEqArea249, tapply(Area_km2, section, sum))
temp <- round(addmargins(temp), 2)
temp


# SET UP FOR GRTS FUNCTION---------
# Call the set.seed function so that the survey designs can be replicateD
set.seed(4447864)

# Create the design list
### We decided to set the number of main sites in the tributary area to 5, since it is a relatively small area (0.4 sq km)
### the unequal probability splits the open water part of the lake into two sections of almost equal area
high249Dsgn <- list("open_water" = list(panel=c(mainSites=12),
                                            seltype="Unequal",
                                            caty.n=c("east" = 6, # grts gives 5, = 5.4 sites/km2
                                                     "west" = 6), # grts gives 7, 5.9 sites/km2
                                            over=20),
                        "trib"=list(panel=c(mainSites=3), # 30 sites/km2
                                    seltype="Equal",
                                    over=5))
# create SpatialDesign object
highSites249 <- grts(design=high249Dsgn,
                        DesignID="SU", # SU for stratified, unequal
                        type.frame="area",
                        src.frame="sf.object",
                        sf.object=highEqArea249,
                        stratum="strata",
                        mdcaty="section",
                        shapefile=FALSE) # convert design object to sf, define prj, then write to .shp

# Print the initial six lines of the survey design
head(highSites249)


# Print the survey design summary
summary(highSites249)


# CRS--------------
class(highSites249) # SpatialDesign object
highSitesEqArea249 <- st_as_sf(highSites249) # convert to sf object
st_crs(highSitesEqArea249) # no coordinate reference system?
st_crs(highSitesEqArea249) = 5070 # inherits from parent object, per Weber.

# project to WGS84 for plotting with leaflet, ArcPad, and writing out lat/long table  (5070 works fine for mapview)
highWGS249 <- highEqArea249 %>% st_transform(4326) # 4326 is WGS84, whereas 4269 is NAD83
highSitesWGS249 <-  highSitesEqArea249 %>% st_transform(4326) # 4326 is WGS84, whereas 4269 is NAD83 

# project to Web Mercator (3857) for use in ArcGIS Pro/geoplatform
highMerc249 <- highEqArea249 %>% st_transform(3857) 
highSitesMerc249 <-  highSitesEqArea249 %>% st_transform(3857) 

# BUFFER
# Create a 15m radius buffer around each sampling point
# Per ESRI, buffer should be created in Web Mercator, an equidistant projection, not Albers
highSitesMerc249buffer <- st_buffer(highSitesMerc249, 15) # radius of 15m, diameter of 30m. radius = 45ft = 2 boat lengths
highSitesWGS249buffer <- highSitesMerc249buffer %>% st_transform(4326) # for use in ArcPad


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
# ch4_249_highAll.png works, but ch4_249_highAllSites.png does not.


# leaflet colors
factpal <- colorFactor(topo.colors(3), highWGS249$section)
factpal.points <- colorFactor(palette = c("red", "black"), domain = highSitesWGS249$panel)

# All points
m <- leaflet() %>% 
  addProviderTiles(providers$Esri.WorldImagery) %>%
  #setView(st_coordinates(cntr_crds)[1], st_coordinates(cntr_crds)[2], zoom = 15) %>%
  fitBounds(lng1 = min(st_coordinates(highSitesWGS249)[,1]),
            lng2 = max(st_coordinates(highSitesWGS249)[,1]),
            lat1 = min(st_coordinates(highSitesWGS249)[,2]),
            lat2 = max(st_coordinates(highSitesWGS249)[,2])) %>%
  addPolygons(data = highWGS249,
              color = "black", weight = 1,
              fillColor = ~factpal(section)) %>%
  addCircleMarkers(data = highSitesWGS249,
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
            values = as.character(highSitesWGS249$panel),
            title = "Sample sites") %>%
  addLegend(position = "bottomright", pal = factpal, 
            values = as.character(highWGS249$section),
            title = "sections") %>%
  addScaleBar()

mapview::mapshot(m, file = "../../../lakeDsn/R10/ch4_249 emigrant/ch4_249_high/249_highAll.png", 
                 remove_controls = NULL)


# Main points
m <- leaflet() %>% 
  addProviderTiles(providers$Esri.WorldImagery) %>%
  fitBounds(lng1 = min(st_coordinates(highSitesWGS249)[,1]),
            lng2 = max(st_coordinates(highSitesWGS249)[,1]),
            lat1 = min(st_coordinates(highSitesWGS249)[,2]),
            lat2 = max(st_coordinates(highSitesWGS249)[,2])) %>%
  addPolygons(data = highWGS249,
              color = "black", weight = 1,
              fillColor = ~factpal(section)) %>%
  addCircleMarkers(data = filter(highSitesWGS249, panel == "mainSites"),
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
            values = as.character(highWGS249$section),
            title = "sections") %>%
  addScaleBar()

mapview::mapshot(m, file = "../../../lakeDsn/R10/ch4_249 emigrant/ch4_249_high/249_highMain.png", 
                 remove_controls = NULL)


# OverSample points
m <- leaflet() %>% 
  addProviderTiles(providers$Esri.WorldImagery) %>%
  fitBounds(lng1 = min(st_coordinates(highSitesWGS249)[,1]),
            lng2 = max(st_coordinates(highSitesWGS249)[,1]),
            lat1 = min(st_coordinates(highSitesWGS249)[,2]),
            lat2 = max(st_coordinates(highSitesWGS249)[,2])) %>%
  addPolygons(data = highWGS249,
              color = "black", weight = 1,
              fillColor = ~factpal(section)) %>%
  addCircleMarkers(data = filter(highSitesWGS249, panel == "OverSamp"),
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
            values = as.character(highWGS249$section),
            title = "sections") %>%
  addScaleBar()

mapview::mapshot(m, file = "../../../lakeDsn/R10/ch4_249 emigrant/ch4_249_high/249_highOver.png", 
                 remove_controls = NULL)



# WRITE OBJECTS TO DISK-----------------
# write out table of sample sites for reference in field.  Must be WGS
write.table(highSitesWGS249 %>%
              select(panel, siteID, section) %>%
              arrange(panel, section, siteID),
            file = "../../../lakeDsn/R10/ch4_249 emigrant/ch4_249_high/ch4_249_highSites.txt",
            row.names = FALSE, sep="\t")


# geopackage
# can write all layers to a geopackage, which behaves much like a geodatabase in ArcGIS.

# Web Mercator for Web Map first

# write out all sites
st_write(obj = highSitesMerc249, 
         dsn = file.path( "../../../lakeDsn/R10/ch4_249 emigrant/ch4_249_high", "highMerc249.gpkg"), 
         layer = "highAllSitesMerc249", # package appends 'main.' to layer name?
         append=FALSE, # this overwrites existing layer
         driver = "GPKG")

# write out all buffers
st_write(obj = highSitesMerc249buffer, 
         dsn = file.path( "../../../lakeDsn/R10/ch4_249 emigrant/ch4_249_high", "highMerc249.gpkg"), 
         layer = "highAllSitesMerc249buffer", # package appends 'main.' to layer name?
         append=FALSE, # this overwrites existing layer
         driver = "GPKG")

# write out main sites
st_write(obj = filter(highSitesMerc249, panel == "mainSites"),
         dsn = file.path( "../../../lakeDsn/R10/ch4_249 emigrant/ch4_249_high", "highMerc249.gpkg"), 
         layer = "highMainSitesMerc249",
         append=FALSE, # this overwrites existing layer
         driver = "GPKG")

# write out main site buffers
st_write(obj = filter(highSitesMerc249buffer, panel == "mainSites"),
         dsn = file.path( "../../../lakeDsn/R10/ch4_249 emigrant/ch4_249_high", "highMerc249.gpkg"), 
         layer = "highMainSitesMerc249buffer",
         append=FALSE, # this overwrites existing layer
         driver = "GPKG")

# write out oversample sites
st_write(obj = filter(highSitesMerc249, panel == "OverSamp"),
         dsn = file.path( "../../../lakeDsn/R10/ch4_249 emigrant/ch4_249_high", "highMerc249.gpkg"), 
         layer = "highOverSampSitesMerc249",
         append=FALSE, # this overwrites existing layer
         driver = "GPKG")

# write out oversample site buffers
st_write(obj = filter(highSitesMerc249buffer, panel == "OverSamp"),
         dsn = file.path( "../../../lakeDsn/R10/ch4_249 emigrant/ch4_249_high", "highMerc249.gpkg"), 
         layer = "highOverSampSitesMerc249buffer",
         append=FALSE, # this overwrites existing layer
         driver = "GPKG")


# write out lake polygon
st_write(obj = highMerc249,
         dsn = file.path( "../../../lakeDsn/R10/ch4_249 emigrant/ch4_249_high", "highMerc249.gpkg"), 
         layer = "highMerc249",
         append=FALSE, # this overwrites existing layer
         driver = "GPKG")


st_layers(file.path( "../../../lakeDsn/R10/ch4_249 emigrant/ch4_249_high", "highMerc249.gpkg"))


# WGS for ArcPad next

# write out all sites
st_write(obj = highSitesWGS249, 
         dsn = file.path( "../../../lakeDsn/R10/ch4_249 emigrant/ch4_249_high", "highWGS249.gpkg"), 
         layer = "highAllSitesWGS249", # package appends 'main.' to layer name?
         append=FALSE, # this overwrites existing layer
         driver = "GPKG")

# write out all buffers
st_write(obj = highSitesWGS249buffer, 
         dsn = file.path( "../../../lakeDsn/R10/ch4_249 emigrant/ch4_249_high", "highWGS249.gpkg"), 
         layer = "highSitesWGS249buffer", # package appends 'main.' to layer name?
         append=FALSE, # this overwrites existing layer
         driver = "GPKG")

# write out main sites
st_write(obj = filter(highSitesWGS249, panel == "mainSites"),
         dsn = file.path( "../../../lakeDsn/R10/ch4_249 emigrant/ch4_249_high", "highWGS249.gpkg"), 
         layer = "highMainSitesWGS249",
         append=FALSE, # this overwrites existing layer
         driver = "GPKG")

# write out main site buffers
st_write(obj = filter(highSitesWGS249buffer, panel == "mainSites"),
         dsn = file.path( "../../../lakeDsn/R10/ch4_249 emigrant/ch4_249_high", "highWGS249.gpkg"), 
         layer = "highMainSitesWGS249buffer",
         append=FALSE, # this overwrites existing layer
         driver = "GPKG")

# write out oversample sites
st_write(obj = filter(highSitesWGS249, panel == "OverSamp"),
         dsn = file.path( "../../../lakeDsn/R10/ch4_249 emigrant/ch4_249_high", "highWGS249.gpkg"), 
         layer = "highOverSampSitesWGS249",
         append=FALSE, # this overwrites existing layer
         driver = "GPKG")

# write out oversample site buffers
st_write(obj = filter(highSitesWGS249buffer, panel == "OverSamp"),
         dsn = file.path( "../../../lakeDsn/R10/ch4_249 emigrant/ch4_249_high", "highWGS249.gpkg"), 
         layer = "highOverSampSitesWGS249buffer",
         append=FALSE, # this overwrites existing layer
         driver = "GPKG")


# write out lake polygon
st_write(obj = highWGS249,
         dsn = file.path( "../../../lakeDsn/R10/ch4_249 emigrant/ch4_249_high", "highWGS249.gpkg"), 
         layer = "highWGS249",
         append=FALSE, # this overwrites existing layer
         driver = "GPKG")


st_layers(file.path( "../../../lakeDsn/R10/ch4_249 emigrant/ch4_249_high", "highWGS249.gpkg"))

