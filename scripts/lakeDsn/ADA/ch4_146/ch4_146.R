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


# CANTON LAKE (ch4-146)--------
# Design for CH4-146, Canton Lake, based on NHD lake polygon and ESRI imagery
# Read polygon shapefile
eqArea146 <- st_read(dsn = "../../../lakeDsn/ADA/CH4-146",
                             layer = "eqArea146")  # shapefile name
plot(eqArea146$geometry) # visualize polygon

# Check CRS, must be equal area for grts function
# EPSG code hard to decipher from output, CRS clearly reported as 'NAD_1983_Albers'
# Coordinate System of NAD 1983 Albers (WKID = 0)
st_crs(eqArea146) 
st_crs(eqArea146) == st_crs(5070) # True

# clean up attributes
eqArea146 <- eqArea146 %>%
  mutate(lakeName = "Canton Lake",
         lakeSiteID = "ch4-146") %>%
  select(lakeName, lakeSiteID, Area_km2, strata, section)


# ATTRIBUTES----------
head(eqArea146) # review attributes

# summarize frame by section
temp <- with(eqArea146, tapply(Area_km2, section, sum))
temp <- round(addmargins(temp), 2)
temp


# SET UP FOR GRTS FUNCTION---------
# Call the set.seed function so that the survey designs can be replicateD
set.seed(4447864)

# Create the design list
### Huge lake (30km2) broke into four chunks to facilitate oversample sites.  Using stratification,
### rather than equal probability, to give better control over number of sites per section.
dsgn146 <- list("one" = list(panel=c(mainSites=4),
                             seltype="Equal",
                             over=5),
                "two"=list(panel=c(mainSites=4), # 30 sites/km2
                           seltype="Equal",
                           over=5),
                "three"=list(panel=c(mainSites=3), # 30 sites/km2
                           seltype="Equal",
                           over=5),
                "four"=list(panel=c(mainSites=4), # 30 sites/km2
                           seltype="Equal",
                           over=5))

# create SpatialDesign object
sites146 <- grts(design=dsgn146,
                        DesignID="SU", # SU for stratified, unequal
                        type.frame="area",
                        src.frame="sf.object",
                        sf.object=eqArea146,
                        stratum="strata",
                        shapefile=FALSE) # convert design object to sf, define prj, then write to .shp

# Print the initial six lines of the survey design
head(sites146)


# Print the survey design summary
summary(sites146)


# CRS--------------
class(sites146) # SpatialDesign object
sitesEqArea146 <- st_as_sf(sites146) # convert to sf object
st_crs(sitesEqArea146) # no coordinate reference system?
st_crs(sitesEqArea146) = 5070 # inherits from parent object, per Weber.

# project to WGS84 for plotting with leaflet, ArcPad, and writing out lat/long table  (5070 works fine for mapview)
WGS146 <- eqArea146 %>% st_transform(4326) # 4326 is WGS84, whereas 4269 is NAD83
sitesWGS146 <-  sitesEqArea146 %>% st_transform(4326) # 4326 is WGS84, whereas 4269 is NAD83 

# project to Web Mercator (3857) for use in ArcGIS Pro/geoplatform
merc146 <- eqArea146 %>% st_transform(3857) 
sitesMerc146 <-  sitesEqArea146 %>% st_transform(3857) 

# BUFFER
# Create a 15m radius buffer around each sampling point
# Per ESRI, buffer should be created in Web Mercator, an equidistant projection, not Albers
sitesMerc146buffer <- st_buffer(sitesMerc146, 15) # radius of 15m, diameter of 30m. radius = 45ft = 2 boat lengths
sitesWGS146buffer <- sitesMerc146buffer %>% st_transform(4326) # for use in ArcPad


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
# CH4-146All.png works, but CH4-146AllSites.png does not.


# leaflet colors
# https://www.benjaminbell.co.uk/2019/08/bathymetric-maps-in-r-colour-palettes.html
blue.col <- colorRampPalette(c("darkblue", "lightblue")) # shades of blue work better with white labels
factpal <- colorFactor(blue.col(4), WGS146$section)
factpal.points <- colorFactor(palette = c("red", "black"), domain = sitesWGS146$panel)

# All points
m <- leaflet() %>% 
  addProviderTiles(providers$Esri.WorldImagery) %>%
  #setView(st_coordinates(cntr_crds)[1], st_coordinates(cntr_crds)[2], zoom = 15) %>%
  fitBounds(lng1 = min(st_coordinates(sitesWGS146)[,1]),
            lng2 = max(st_coordinates(sitesWGS146)[,1]),
            lat1 = min(st_coordinates(sitesWGS146)[,2]),
            lat2 = max(st_coordinates(sitesWGS146)[,2])) %>%
  addPolygons(data = st_zm(WGS146), # removes z and m values, if present
              color = "black", weight = 1,
              fillOpacity = 1, fillColor = ~factpal(section)) %>%
  addCircleMarkers(data = sitesWGS146,
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
            values = as.character(sitesWGS146$panel),
            title = "Sample sites") %>%
  addLegend(position = "bottomright", pal = factpal, 
            opacity = 1,
            values = as.character(WGS146$section),
            title = "sections") %>%
  addScaleBar()

mapview::mapshot(m, file = "../../../lakeDsn/ADA/CH4-146/146_All.png", 
                 remove_controls = NULL)


# Main points
m <- leaflet() %>% 
  addProviderTiles(providers$Esri.WorldImagery) %>%
  fitBounds(lng1 = min(st_coordinates(sitesWGS146)[,1]),
            lng2 = max(st_coordinates(sitesWGS146)[,1]),
            lat1 = min(st_coordinates(sitesWGS146)[,2]),
            lat2 = max(st_coordinates(sitesWGS146)[,2])) %>%
  addPolygons(data = st_zm(WGS146), # removes z and m values, if present
              color = "black", weight = 1,
              fillOpacity = 1, fillColor = ~factpal(section)) %>%
  addCircleMarkers(data = filter(sitesWGS146, panel == "mainSites"),
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
            values = as.character(WGS146$section),
            title = "sections") %>%
  addScaleBar()

mapview::mapshot(m, file = "../../../lakeDsn/ADA/CH4-146/146_Main.png", 
                 remove_controls = NULL)


# OverSample points
m <- leaflet() %>% 
  addProviderTiles(providers$Esri.WorldImagery) %>%
  fitBounds(lng1 = min(st_coordinates(sitesWGS146)[,1]),
            lng2 = max(st_coordinates(sitesWGS146)[,1]),
            lat1 = min(st_coordinates(sitesWGS146)[,2]),
            lat2 = max(st_coordinates(sitesWGS146)[,2])) %>%
  addPolygons(data = st_zm(WGS146),  # removes z and m values, if present
              color = "black", weight = 1,
              fillOpacity = 1, fillColor = ~factpal(section)) %>%
  addCircleMarkers(data = filter(sitesWGS146, panel == "OverSamp"),
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
            values = as.character(WGS146$section),
            title = "sections") %>%
  addScaleBar()

mapview::mapshot(m, file = "../../../lakeDsn/ADA/CH4-146/146_Over.png", 
                 remove_controls = NULL)



# WRITE OBJECTS TO DISK-----------------
# write out table of sample sites for reference in field.  Must be WGS
write.table(sitesWGS146 %>%
              select(panel, siteID, section) %>%
              arrange(panel, section, siteID),
            file = "../../../lakeDsn/ADA/CH4-146/ch4-146Sites.txt",
            row.names = FALSE, sep="\t")


# geopackage
# can write all layers to a geopackage, which behaves much like a geodatabase in ArcGIS.

# Web Mercator for Web Map first

# write out all sites
st_write(obj = sitesMerc146, 
         dsn = file.path( "../../../lakeDsn/ADA/CH4-146", "merc146.gpkg"), 
         layer = "allSitesMerc146", # package appends 'main.' to layer name?
         append=FALSE, # this overwrites existing layer
         driver = "GPKG")

# write out all buffers
st_write(obj = sitesMerc146buffer, 
         dsn = file.path( "../../../lakeDsn/ADA/CH4-146", "merc146.gpkg"), 
         layer = "allSitesMerc146buffer", # package appends 'main.' to layer name?
         append=FALSE, # this overwrites existing layer
         driver = "GPKG")

# write out main sites
st_write(obj = filter(sitesMerc146, panel == "mainSites"),
         dsn = file.path( "../../../lakeDsn/ADA/CH4-146", "merc146.gpkg"), 
         layer = "mainSitesMerc146",
         append=FALSE, # this overwrites existing layer
         driver = "GPKG")

# write out main site buffers
st_write(obj = filter(sitesMerc146buffer, panel == "mainSites"),
         dsn = file.path( "../../../lakeDsn/ADA/CH4-146", "merc146.gpkg"), 
         layer = "mainSitesMerc146buffer",
         append=FALSE, # this overwrites existing layer
         driver = "GPKG")

# write out oversample sites
st_write(obj = filter(sitesMerc146, panel == "OverSamp"),
         dsn = file.path( "../../../lakeDsn/ADA/CH4-146", "merc146.gpkg"), 
         layer = "overSampSitesMerc146",
         append=FALSE, # this overwrites existing layer,
         driver = "GPKG")

# write out oversample site buffers
st_write(obj = filter(sitesMerc146buffer, panel == "OverSamp"),
         dsn = file.path( "../../../lakeDsn/ADA/CH4-146", "merc146.gpkg"), 
         layer = "overSampSitesMerc146buffer",
         append=FALSE, # this overwrites existing layer,
         driver = "GPKG")


# write out lake polygon
st_write(obj = merc146,
         dsn = file.path( "../../../lakeDsn/ADA/CH4-146", "merc146.gpkg"), 
         layer = "merc146",
         append=FALSE, # this overwrites existing layer,
         driver = "GPKG")


st_layers(file.path( "../../../lakeDsn/ADA/CH4-146", "merc146.gpkg"))


# WGS for ArcPad next

# write out all sites
st_write(obj = sitesWGS146, 
         dsn = file.path( "../../../lakeDsn/ADA/CH4-146", "WGS146.gpkg"), 
         layer = "allSitesWGS146", # package appends 'main.' to layer name?
         append=FALSE, # this overwrites existing layer
         driver = "GPKG")

# write out all buffers
st_write(obj = sitesWGS146buffer, 
         dsn = file.path( "../../../lakeDsn/ADA/CH4-146", "WGS146.gpkg"), 
         layer = "sitesWGS146buffer", # package appends 'main.' to layer name?
         append=FALSE, # this overwrites existing layer
         driver = "GPKG")

# write out main sites
st_write(obj = filter(sitesWGS146, panel == "mainSites"),
         dsn = file.path( "../../../lakeDsn/ADA/CH4-146", "WGS146.gpkg"), 
         layer = "mainSitesWGS146",
         append=FALSE, # this overwrites existing layer
         driver = "GPKG")

# write out main site buffers
st_write(obj = filter(sitesWGS146buffer, panel == "mainSites"),
         dsn = file.path( "../../../lakeDsn/ADA/CH4-146", "WGS146.gpkg"), 
         layer = "mainSitesWGS146buffer",
         append=FALSE, # this overwrites existing layer
         driver = "GPKG")

# write out oversample sites
st_write(obj = filter(sitesWGS146, panel == "OverSamp"),
         dsn = file.path( "../../../lakeDsn/ADA/CH4-146", "WGS146.gpkg"), 
         layer = "overSampSitesWGS146",
         append=FALSE, # this overwrites existing layer,
         driver = "GPKG")

# write out oversample site buffers
st_write(obj = filter(sitesWGS146buffer, panel == "OverSamp"),
         dsn = file.path( "../../../lakeDsn/ADA/CH4-146", "WGS146.gpkg"), 
         layer = "overSampSitesWGS146buffer",
         append=FALSE, # this overwrites existing layer,
         driver = "GPKG")


# write out lake polygon
st_write(obj = WGS146,
         dsn = file.path( "../../../lakeDsn/ADA/CH4-146", "WGS146.gpkg"), 
         layer = "WGS146",
         append=FALSE, # this overwrites existing layer,
         driver = "GPKG")


st_layers(file.path( "../../../lakeDsn/ADA/CH4-146", "WGS146.gpkg"))

