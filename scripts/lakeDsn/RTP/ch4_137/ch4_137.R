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


# BURNSVILLE LAKE (ch4-137)--------
# Design for CH4-137, Burnsville Lake, based on NHD lake polygon
# modified to exlcude areas below Route $ where navigation will be difficult.
# Little Kanawha River drains in from south and is the largest tributary.

# Read polygon shapefile
eqArea137 <- st_read(dsn = "../../../lakeDsn/RTP/CH4-137",
                             layer = "eqArea137")  # shapefile name
plot(eqArea137$geometry) # visualize polygon

# Check CRS, must be equal area for grts function
# EPSG code hard to decipher from output, CRS clearly reported as 'NAD_1983_Albers'
# Coordinate System of NAD 1983 Albers (WKID = 0)
st_crs(eqArea137) 
st_crs(eqArea137) == st_crs(5070) # True

# clean up attributes
eqArea137 <- eqArea137 %>%
  mutate(lakeName = "Burnsville Lake",
         lakeSiteID = "ch4-137") %>%
  select(lakeName, lakeSiteID, Area_km2, strata, section)


# ATTRIBUTES----------
head(eqArea137) # review attributes

# summarize frame by section
temp <- with(eqArea137, tapply(Area_km2, section, sum))
temp <- round(addmargins(temp), 2)
temp


# SET UP FOR GRTS FUNCTION---------
# Call the set.seed function so that the survey designs can be replicateD
set.seed(4447864)

# Create the design list
### We decided to set the number of main sites in the tributary area to 5, since it is a relatively small area (0.4 sq km)
### the unequal probability splits the open water part of the lake into two sections of almost equal area
dsgn137 <- list("open_water" = list(panel=c(mainSites=12),
                                            seltype="Unequal",
                                            caty.n=c("main" = 6, # grts gives 5, = 5.4 sites/km2
                                                     "NE" = 3,
                                                     "central" = 3), # grts gives 7, 5.9 sites/km2
                                            over=20),
                        "trib"=list(panel=c(mainSites=3), # 30 sites/km2
                                    seltype="Equal",
                                    over=5))
# create SpatialDesign object
sites137 <- grts(design=dsgn137,
                        DesignID="SU", # SU for stratified, unequal
                        type.frame="area",
                        src.frame="sf.object",
                        sf.object=eqArea137,
                        stratum="strata",
                        mdcaty="section",
                        shapefile=FALSE) # convert design object to sf, define prj, then write to .shp

# Print the initial six lines of the survey design
head(sites137)


# Print the survey design summary
summary(sites137)


# CRS--------------
class(sites137) # SpatialDesign object
sitesEqArea137 <- st_as_sf(sites137) # convert to sf object
st_crs(sitesEqArea137) # no coordinate reference system?
st_crs(sitesEqArea137) = 5070 # inherits from parent object, per Weber.

# project to WGS84 for plotting with leaflet, ArcPad, and writing out lat/long table  (5070 works fine for mapview)
WGS137 <- eqArea137 %>% st_transform(4326) # 4326 is WGS84, whereas 4269 is NAD83
sitesWGS137 <-  sitesEqArea137 %>% st_transform(4326) # 4326 is WGS84, whereas 4269 is NAD83 

# project to Web Mercator (3857) for use in ArcGIS Pro/geoplatform
merc137 <- eqArea137 %>% st_transform(3857) 
sitesMerc137 <-  sitesEqArea137 %>% st_transform(3857) 

# BUFFER
# Create a 15m radius buffer around each sampling point
# Per ESRI, buffer should be created in Web Mercator, an equidistant projection, not Albers
sitesMerc137buffer <- st_buffer(sitesMerc137, 15) # radius of 15m, diameter of 30m. radius = 45ft = 2 boat lengths
sitesWGS137buffer <- sitesMerc137buffer %>% st_transform(4326) # for use in ArcPad


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
# CH4-137All.png works, but CH4-137AllSites.png does not.


# leaflet colors
factpal <- colorFactor(topo.colors(3), WGS137$section)
factpal.points <- colorFactor(palette = c("red", "black"), domain = sitesWGS137$panel)

# All points
m <- leaflet() %>% 
  addProviderTiles(providers$Esri.WorldImagery) %>%
  #setView(st_coordinates(cntr_crds)[1], st_coordinates(cntr_crds)[2], zoom = 15) %>%
  fitBounds(lng1 = min(st_coordinates(sitesWGS137)[,1]),
            lng2 = max(st_coordinates(sitesWGS137)[,1]),
            lat1 = min(st_coordinates(sitesWGS137)[,2]),
            lat2 = max(st_coordinates(sitesWGS137)[,2])) %>%
  addPolygons(data = st_zm(WGS137), # removes z and m values, if present
              color = "black", weight = 1,
              fillOpacity = 1, fillColor = ~factpal(section)) %>%
  addCircleMarkers(data = sitesWGS137,
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
            values = as.character(sitesWGS137$panel),
            title = "Sample sites") %>%
  addLegend(position = "bottomright", pal = factpal, 
            values = as.character(WGS137$section),
            title = "sections") %>%
  addScaleBar()

mapview::mapshot(m, file = "../../../lakeDsn/RTP/CH4-137/137_All.png", 
                 remove_controls = NULL)


# Main points
m <- leaflet() %>% 
  addProviderTiles(providers$Esri.WorldImagery) %>%
  fitBounds(lng1 = min(st_coordinates(sitesWGS137)[,1]),
            lng2 = max(st_coordinates(sitesWGS137)[,1]),
            lat1 = min(st_coordinates(sitesWGS137)[,2]),
            lat2 = max(st_coordinates(sitesWGS137)[,2])) %>%
  addPolygons(data = st_zm(WGS137), # removes z and m values, if present
              color = "black", weight = 1,
              fillOpacity = 1, fillColor = ~factpal(section)) %>%
  addCircleMarkers(data = filter(sitesWGS137, panel == "mainSites"),
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
            values = as.character(WGS137$section),
            title = "sections") %>%
  addScaleBar()

mapview::mapshot(m, file = "../../../lakeDsn/RTP/CH4-137/137_Main.png", 
                 remove_controls = NULL)


# OverSample points
m <- leaflet() %>% 
  addProviderTiles(providers$Esri.WorldImagery) %>%
  fitBounds(lng1 = min(st_coordinates(sitesWGS137)[,1]),
            lng2 = max(st_coordinates(sitesWGS137)[,1]),
            lat1 = min(st_coordinates(sitesWGS137)[,2]),
            lat2 = max(st_coordinates(sitesWGS137)[,2])) %>%
  addPolygons(data = st_zm(WGS137),  # removes z and m values, if present
              color = "black", weight = 1,
              fillOpacity = 1, fillColor = ~factpal(section)) %>%
  addCircleMarkers(data = filter(sitesWGS137, panel == "OverSamp"),
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
            values = as.character(WGS137$section),
            title = "sections") %>%
  addScaleBar()

mapview::mapshot(m, file = "../../../lakeDsn/RTP/CH4-137/137_Over.png", 
                 remove_controls = NULL)



# WRITE OBJECTS TO DISK-----------------
# write out table of sample sites for reference in field.  Must be WGS
write.table(sitesWGS137 %>%
              select(panel, siteID, section) %>%
              arrange(panel, section, siteID),
            file = "../../../lakeDsn/RTP/CH4-137/ch4_137_sites.txt",
            row.names = FALSE, sep="\t")


# geopackage
# can write all layers to a geopackage, which behaves much like a geodatabase in ArcGIS.

# Web Mercator for Web Map first

# write out all sites
st_write(obj = sitesMerc137, 
         dsn = file.path( "../../../lakeDsn/RTP/CH4-137", "merc137.gpkg"), 
         layer = "allSitesMerc137", # package appends 'main.' to layer name?
         append=FALSE, # this overwrites existing layer
         driver = "GPKG")

# write out all buffers
st_write(obj = sitesMerc137buffer, 
         dsn = file.path( "../../../lakeDsn/RTP/CH4-137", "merc137.gpkg"), 
         layer = "allSitesMerc137buffer", # package appends 'main.' to layer name?
         append=FALSE, # this overwrites existing layer
         driver = "GPKG")

# write out main sites
st_write(obj = filter(sitesMerc137, panel == "mainSites"),
         dsn = file.path( "../../../lakeDsn/RTP/CH4-137", "merc137.gpkg"), 
         layer = "mainSitesMerc137",
         append=FALSE, # this overwrites existing layer
         driver = "GPKG")

# write out main site buffers
st_write(obj = filter(sitesMerc137buffer, panel == "mainSites"),
         dsn = file.path( "../../../lakeDsn/RTP/CH4-137", "merc137.gpkg"), 
         layer = "mainSitesMerc137buffer",
         append=FALSE, # this overwrites existing layer
         driver = "GPKG")

# write out oversample sites
st_write(obj = filter(sitesMerc137, panel == "OverSamp"),
         dsn = file.path( "../../../lakeDsn/RTP/CH4-137", "merc137.gpkg"), 
         layer = "overSampSitesMerc137",
         append=FALSE, # this overwrites existing layer,
         driver = "GPKG")

# write out oversample site buffers
st_write(obj = filter(sitesMerc137buffer, panel == "OverSamp"),
         dsn = file.path( "../../../lakeDsn/RTP/CH4-137", "merc137.gpkg"), 
         layer = "overSampSitesMerc137buffer",
         append=FALSE, # this overwrites existing layer,
         driver = "GPKG")


# write out lake polygon
st_write(obj = merc137,
         dsn = file.path( "../../../lakeDsn/RTP/CH4-137", "merc137.gpkg"), 
         layer = "merc137",
         append=FALSE, # this overwrites existing layer,
         driver = "GPKG")


st_layers(file.path( "../../../lakeDsn/RTP/CH4-137", "merc137.gpkg"))


# WGS for ArcPad next

# write out all sites
st_write(obj = sitesWGS137, 
         dsn = file.path( "../../../lakeDsn/RTP/CH4-137", "WGS137.gpkg"), 
         layer = "allSitesWGS137", # package appends 'main.' to layer name?
         append=FALSE, # this overwrites existing layer
         driver = "GPKG")

# write out all buffers
st_write(obj = sitesWGS137buffer, 
         dsn = file.path( "../../../lakeDsn/RTP/CH4-137", "WGS137.gpkg"), 
         layer = "sitesWGS137buffer", # package appends 'main.' to layer name?
         append=FALSE, # this overwrites existing layer
         driver = "GPKG")

# write out main sites
st_write(obj = filter(sitesWGS137, panel == "mainSites"),
         dsn = file.path( "../../../lakeDsn/RTP/CH4-137", "WGS137.gpkg"), 
         layer = "mainSitesWGS137",
         append=FALSE, # this overwrites existing layer
         driver = "GPKG")

# write out main site buffers
st_write(obj = filter(sitesWGS137buffer, panel == "mainSites"),
         dsn = file.path( "../../../lakeDsn/RTP/CH4-137", "WGS137.gpkg"), 
         layer = "mainSitesWGS137buffer",
         append=FALSE, # this overwrites existing layer
         driver = "GPKG")

# write out oversample sites
st_write(obj = filter(sitesWGS137, panel == "OverSamp"),
         dsn = file.path( "../../../lakeDsn/RTP/CH4-137", "WGS137.gpkg"), 
         layer = "overSampSitesWGS137",
         append=FALSE, # this overwrites existing layer,
         driver = "GPKG")

# write out oversample site buffers
st_write(obj = filter(sitesWGS137buffer, panel == "OverSamp"),
         dsn = file.path( "../../../lakeDsn/RTP/CH4-137", "WGS137.gpkg"), 
         layer = "overSampSitesWGS137buffer",
         append=FALSE, # this overwrites existing layer,
         driver = "GPKG")


# write out lake polygon
st_write(obj = WGS137,
         dsn = file.path( "../../../lakeDsn/RTP/CH4-137", "WGS137.gpkg"), 
         layer = "WGS137",
         append=FALSE, # this overwrites existing layer,
         driver = "GPKG")


st_layers(file.path( "../../../lakeDsn/RTP/CH4-137", "WGS137.gpkg"))

