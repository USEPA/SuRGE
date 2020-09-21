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
# Original, high, mid, and low.  This script is for the 'mid' scenario.
# Two tribs on the south (Emigrant Cr. on SE and Hill Cr. on SW).
# ~100ft deep at dam.  Trib strata extends to a depth of 8M (26 FT).

# Read polygon shapefile
midEqArea249 <- st_read(dsn = "../../../lakeDsn/R10/ch4_249 emigrant/ch4_249_mid",
                             layer = "midEqArea249")  # shapefile name
plot(midEqArea249$geometry) # visualize polygon

# Check CRS, must be equal area for grts function
# EPSG code hard to decipher from output, CRS clearly reported as 'NAD_1983_Albers'
# Coordinate System of NAD 1983 Albers (WKID = 0)
st_crs(midEqArea249) 
st_crs(midEqArea249) == st_crs(5070) # True

# clean up attributes
midEqArea249 <- midEqArea249 %>%
  mutate(lakeName = "Emigrant",
         lakeSiteID = "ch4-249") %>%
  select(lakeName, lakeSiteID, Area_km2, strata, section)


# ATTRIBUTES----------
head(midEqArea249) # review attributes

# summarize frame by section
temp <- with(midEqArea249, tapply(Area_km2, section, sum))
temp <- round(addmargins(temp), 2)
temp


# SET UP FOR GRTS FUNCTION---------
# Call the set.seed function so that the survey designs can be replicateD
set.seed(4447864)

# Create the design list
### We decided to set the number of main sites in the tributary area to 5, since it is a relatively small area (0.4 sq km)
### the unequal probability splits the open water part of the lake into two sections of almost equal area
mid249Dsgn <- list("open_water" = list(panel=c(mainSites=12),
                                            seltype="Unequal",
                                            caty.n=c("east" = 5, # grts gives 4, = 5.1 sites/km2
                                                     "west" = 7), # grts gives 8, 8 sites/km2
                                            over=20),
                        "trib"=list(panel=c(mainSites=3), # 37.5 sites/km2
                                    seltype="Equal",
                                    over=5))
# create SpatialDesign object
midSites249 <- grts(design=mid249Dsgn,
                        DesignID="SU", # SU for stratified, unequal
                        type.frame="area",
                        src.frame="sf.object",
                        sf.object=midEqArea249,
                        stratum="strata",
                        mdcaty="section",
                        shapefile=FALSE) # convert design object to sf, define prj, then write to .shp

# Print the initial six lines of the survey design
head(midSites249)


# Print the survey design summary
summary(midSites249)


# CRS--------------
class(midSites249) # SpatialDesign object
midSitesEqArea249 <- st_as_sf(midSites249) # convert to sf object
st_crs(midSitesEqArea249) # no coordinate reference system?
st_crs(midSitesEqArea249) = 5070 # inherits from parent object, per Weber.

# project to WGS84 for plotting with leaflet, ArcPad, and writing out lat/long table  (5070 works fine for mapview)
midWGS249 <- midEqArea249 %>% st_transform(4326) # 4326 is WGS84, whereas 4269 is NAD83
midSitesWGS249 <-  midSitesEqArea249 %>% st_transform(4326) # 4326 is WGS84, whereas 4269 is NAD83 

# project to Web Mercator (3857) for use in ArcGIS Pro/geoplatform
midMerc249 <- midEqArea249 %>% st_transform(3857) 
midSitesMerc249 <-  midSitesEqArea249 %>% st_transform(3857) 

# BUFFER
# Create a 15m radius buffer around each sampling point
# Per ESRI, buffer should be created in Web Mercator, an equidistant projection, not Albers
midSitesMerc249buffer <- st_buffer(midSitesMerc249, 15) # radius of 15m, diameter of 30m. radius = 45ft = 2 boat lengths
midSitesWGS249buffer <- midSitesMerc249buffer %>% st_transform(4326) # for use in ArcPad


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
# ch4_249_midAll.png works, but ch4_249_midAllSites.png does not.


# leaflet colors
factpal <- colorFactor(topo.colors(3), midWGS249$section)
factpal.points <- colorFactor(palette = c("red", "black"), domain = midSitesWGS249$panel)

# All points
m <- leaflet() %>% 
  addProviderTiles(providers$Esri.WorldImagery) %>%
  #setView(st_coordinates(cntr_crds)[1], st_coordinates(cntr_crds)[2], zoom = 15) %>%
  fitBounds(lng1 = min(st_coordinates(midSitesWGS249)[,1]),
            lng2 = max(st_coordinates(midSitesWGS249)[,1]),
            lat1 = min(st_coordinates(midSitesWGS249)[,2]),
            lat2 = max(st_coordinates(midSitesWGS249)[,2])) %>%
  addPolygons(data = midWGS249,
              color = "black", weight = 1,
              fillColor = ~factpal(section)) %>%
  addCircleMarkers(data = midSitesWGS249,
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
            values = as.character(midSitesWGS249$panel),
            title = "Sample sites") %>%
  addLegend(position = "bottomright", pal = factpal, 
            values = as.character(midWGS249$section),
            title = "sections") %>%
  addScaleBar()

mapview::mapshot(m, file = "../../../lakeDsn/R10/ch4_249 emigrant/ch4_249_mid/249_midAll.png", 
                 remove_controls = NULL)


# Main points
m <- leaflet() %>% 
  addProviderTiles(providers$Esri.WorldImagery) %>%
  fitBounds(lng1 = min(st_coordinates(midSitesWGS249)[,1]),
            lng2 = max(st_coordinates(midSitesWGS249)[,1]),
            lat1 = min(st_coordinates(midSitesWGS249)[,2]),
            lat2 = max(st_coordinates(midSitesWGS249)[,2])) %>%
  addPolygons(data = midWGS249,
              color = "black", weight = 1,
              fillColor = ~factpal(section)) %>%
  addCircleMarkers(data = filter(midSitesWGS249, panel == "mainSites"),
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
            values = as.character(midWGS249$section),
            title = "sections") %>%
  addScaleBar()

mapview::mapshot(m, file = "../../../lakeDsn/R10/ch4_249 emigrant/ch4_249_mid/249_midMain.png", 
                 remove_controls = NULL)


# OverSample points
m <- leaflet() %>% 
  addProviderTiles(providers$Esri.WorldImagery) %>%
  fitBounds(lng1 = min(st_coordinates(midSitesWGS249)[,1]),
            lng2 = max(st_coordinates(midSitesWGS249)[,1]),
            lat1 = min(st_coordinates(midSitesWGS249)[,2]),
            lat2 = max(st_coordinates(midSitesWGS249)[,2])) %>%
  addPolygons(data = midWGS249,
              color = "black", weight = 1,
              fillColor = ~factpal(section)) %>%
  addCircleMarkers(data = filter(midSitesWGS249, panel == "OverSamp"),
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
            values = as.character(midWGS249$section),
            title = "sections") %>%
  addScaleBar()

mapview::mapshot(m, file = "../../../lakeDsn/R10/ch4_249 emigrant/ch4_249_mid/249_midOver.png", 
                 remove_controls = NULL)



# WRITE OBJECTS TO DISK-----------------
# write out table of sample sites for reference in field.  Must be WGS
write.table(midSitesWGS249 %>%
              select(panel, siteID, section) %>%
              arrange(panel, section, siteID),
            file = "../../../lakeDsn/R10/ch4_249 emigrant/ch4_249_mid/ch4_249_midSites.txt",
            row.names = FALSE, sep="\t")


# geopackage
# can write all layers to a geopackage, which behaves much like a geodatabase in ArcGIS.

# Web Mercator for Web Map first

# write out all sites
st_write(obj = midSitesMerc249, 
         dsn = file.path( "../../../lakeDsn/R10/ch4_249 emigrant/ch4_249_mid", "midMerc249.gpkg"), 
         layer = "midAllSitesMerc249", # package appends 'main.' to layer name?
         append=FALSE, # this overwrites existing layer
         driver = "GPKG")

# write out all buffers
st_write(obj = midSitesMerc249buffer, 
         dsn = file.path( "../../../lakeDsn/R10/ch4_249 emigrant/ch4_249_mid", "midMerc249.gpkg"), 
         layer = "midAllSitesMerc249buffer", # package appends 'main.' to layer name?
         append=FALSE, # this overwrites existing layer
         driver = "GPKG")

# write out main sites
st_write(obj = filter(midSitesMerc249, panel == "mainSites"),
         dsn = file.path( "../../../lakeDsn/R10/ch4_249 emigrant/ch4_249_mid", "midMerc249.gpkg"), 
         layer = "midMainSitesMerc249",
         append=FALSE, # this overwrites existing layer
         driver = "GPKG")

# write out main site buffers
st_write(obj = filter(midSitesMerc249buffer, panel == "mainSites"),
         dsn = file.path( "../../../lakeDsn/R10/ch4_249 emigrant/ch4_249_mid", "midMerc249.gpkg"), 
         layer = "midMainSitesMerc249buffer",
         append=FALSE, # this overwrites existing layer
         driver = "GPKG")

# write out oversample sites
st_write(obj = filter(midSitesMerc249, panel == "OverSamp"),
         dsn = file.path( "../../../lakeDsn/R10/ch4_249 emigrant/ch4_249_mid", "midMerc249.gpkg"), 
         layer = "midOverSampSitesMerc249",
         append=FALSE, # this overwrites existing layer
         driver = "GPKG")

# write out oversample site buffers
st_write(obj = filter(midSitesMerc249buffer, panel == "OverSamp"),
         dsn = file.path( "../../../lakeDsn/R10/ch4_249 emigrant/ch4_249_mid", "midMerc249.gpkg"), 
         layer = "midOverSampSitesMerc249buffer",
         append=FALSE, # this overwrites existing layer
         driver = "GPKG")


# write out lake polygon
st_write(obj = midMerc249,
         dsn = file.path( "../../../lakeDsn/R10/ch4_249 emigrant/ch4_249_mid", "midMerc249.gpkg"), 
         layer = "midMerc249",
         append=FALSE, # this overwrites existing layer
         driver = "GPKG")


st_layers(file.path( "../../../lakeDsn/R10/ch4_249 emigrant/ch4_249_mid", "midMerc249.gpkg"))


# WGS for ArcPad next

# write out all sites
st_write(obj = midSitesWGS249, 
         dsn = file.path( "../../../lakeDsn/R10/ch4_249 emigrant/ch4_249_mid", "midWGS249.gpkg"), 
         layer = "midAllSitesWGS249", # package appends 'main.' to layer name?
         append=FALSE, # this overwrites existing layer
         driver = "GPKG")

# write out all buffers
st_write(obj = midSitesWGS249buffer, 
         dsn = file.path( "../../../lakeDsn/R10/ch4_249 emigrant/ch4_249_mid", "midWGS249.gpkg"), 
         layer = "midSitesWGS249buffer", # package appends 'main.' to layer name?
         append=FALSE, # this overwrites existing layer
         driver = "GPKG")

# write out main sites
st_write(obj = filter(midSitesWGS249, panel == "mainSites"),
         dsn = file.path( "../../../lakeDsn/R10/ch4_249 emigrant/ch4_249_mid", "midWGS249.gpkg"), 
         layer = "midMainSitesWGS249",
         append=FALSE, # this overwrites existing layer
         driver = "GPKG")

# write out main site buffers
st_write(obj = filter(midSitesWGS249buffer, panel == "mainSites"),
         dsn = file.path( "../../../lakeDsn/R10/ch4_249 emigrant/ch4_249_mid", "midWGS249.gpkg"), 
         layer = "midMainSitesWGS249buffer",
         append=FALSE, # this overwrites existing layer
         driver = "GPKG")

# write out oversample sites
st_write(obj = filter(midSitesWGS249, panel == "OverSamp"),
         dsn = file.path( "../../../lakeDsn/R10/ch4_249 emigrant/ch4_249_mid", "midWGS249.gpkg"), 
         layer = "midOverSampSitesWGS249",
         append=FALSE, # this overwrites existing layer
         driver = "GPKG")

# write out oversample site buffers
st_write(obj = filter(midSitesWGS249buffer, panel == "OverSamp"),
         dsn = file.path( "../../../lakeDsn/R10/ch4_249 emigrant/ch4_249_mid", "midWGS249.gpkg"), 
         layer = "midOverSampSitesWGS249buffer",
         append=FALSE, # this overwrites existing layer
         driver = "GPKG")


# write out lake polygon
st_write(obj = midWGS249,
         dsn = file.path( "../../../lakeDsn/R10/ch4_249 emigrant/ch4_249_mid", "midWGS249.gpkg"), 
         layer = "midWGS249",
         append=FALSE, # this overwrites existing layer
         driver = "GPKG")


st_layers(file.path( "../../../lakeDsn/R10/ch4_249 emigrant/ch4_249_mid", "midWGS249.gpkg"))

