# STRATIFIED, UNEQUAL PROBABILITY GRTS DESIGN

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



# READ POLYGON SHAPEFILE-----
# USED IN grts() CALL
homeEqArea249 <- st_read(dsn = "../../../lakeDsn",
                             layer = "home")  # shapefile name
plot(homeEqArea249$geometry) # visualize polygon

# Check CRS, must be equal area for grts function
# no EPSG code, but arcGIS reports a Geographic Coordinate system of GCS North American 1983 (WKID 4269) and a Projected
# Coordinate System of NAD 1983 Albers (WKID = 0)
st_crs(homeEqArea249) 
st_crs(homeEqArea249) ==   st_crs(5070) # convert to NAD_1983_Contiguous_USA_Albers


# SET UP FOR GRTS FUNCTION---------
# Call the set.seed function so that the survey designs can be replicateD
set.seed(4447864)

# Create the design list
### We decided to set the number of main sites in the tributary area to 5, since it is a relatively small area (0.4 sq km)
### the unequal probability splits the open water part of the lake into two sections of almost equal area
home249Dsgn <- list("None" = list(panel=c(mainSites=5),
                                            seltype="Equal",
                                            over=0))
# create SpatialDesign object
homeSites249 <- grts(design=home249Dsgn,
                        DesignID="U", # SU for stratified, unequal
                        type.frame="area",
                        src.frame="sf.object",
                        sf.object=homeEqArea249,
                        shapefile=FALSE) # convert design object to sf, define prj, then write to .shp


# CRS--------------
class(homeSites249) # SpatialDesign object
homeSitesEqArea249 <- st_as_sf(homeSites249) # convert to sf object
st_crs(homeSitesEqArea249) # no coordinate reference system?
st_crs(homeSitesEqArea249) = 5070 # inherits from parent object, per Weber.

# project to WGS84 for plotting with leaflet, ArcPad, and writing out lat/long table  (5070 works fine for mapview)
homeWGS249 <- homeEqArea249 %>% st_transform(4326) # 4326 is WGS84, whereas 4269 is NAD83
homeSitesWGS249 <-  homeSitesEqArea249 %>% st_transform(4326) # 4326 is WGS84, whereas 4269 is NAD83

# project to Web Mercator (3857) for use in ArcGIS Pro/geoplatform
homeMerc249 <- homeEqArea249 %>% st_transform(3857) 
homeSitesMerc249 <-  homeSitesEqArea249 %>% st_transform(3857) 

# BUFFER
# Create a 15m radius buffer around each sampling point
# Per ESRI, buffer should be created in Web Mercator, an equidistant projection, not Albers
homeSitesMerc249buffer <- st_buffer(homeSitesMerc249, 15) # radius of 15m, diameter of 30m. radius = 45ft = 2 boat lengths
homeSitesWGS249buffer <- homeSitesMerc249buffer %>% st_transform(4326) # for use in ArcPad

plot(homeWGS249$geometry)
plot(homeSitesWGS249buffer$geometry)

# MAPS---------------------
# Approach: generate static maps to paste in .rmd.  Knitted html can be printed for use in field.
# - ggmap requires an API key from google to provide satellite images.  Credit card required.  No thanks.
# - decided to make interactive map with leaflet, but capture image in .png with mapview::mapshot()
# - per Lil, field crews won't make use of interactive maps


# LEAFLET
# Mapview approach gets close, but I can't quite tweak everything the way I like
# Leaflet alhomes much more control, requires WGS84 CRS.
# setView for zoo level requires manual fiddling with zoom level.  fitBounds() is cleaner
# mapshot() call seems sensitive to length of file name (sometimes?).  Try to keep simple.
# ch4_249_homeAll.png works, but ch4_249_homeAllSites.png does not.


# leaflet colors
factpal <- colorFactor(topo.colors(3), homeWGS249$section)
factpal.points <- colorFactor(palette = c("red", "black"), domain = homeSitesWGS249$panel)

# All points
m <- leaflet() %>% 
  addProviderTiles(providers$Esri.WorldImagery) %>%
  #setView(st_coordinates(cntr_crds)[1], st_coordinates(cntr_crds)[2], zoom = 15) %>%
  fitBounds(lng1 = min(st_coordinates(homeSitesWGS249)[,1]),
            lng2 = max(st_coordinates(homeSitesWGS249)[,1]),
            lat1 = min(st_coordinates(homeSitesWGS249)[,2]),
            lat2 = max(st_coordinates(homeSitesWGS249)[,2])) %>%
  addPolygons(data = homeWGS249,
              color = "black", weight = 1,
              fillColor = ~factpal(section)) %>%
  addCircleMarkers(data = homeSitesWGS249,
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
            values = as.character(homeSitesWGS249$panel),
            title = "Sample sites") %>%
  addLegend(position = "bottomright", pal = factpal, 
            values = as.character(homeWGS249$section),
            title = "sections") %>%
  addScaleBar()

mapview::mapshot(m, file = "../../../lakeDsn/R10/ch4_249 emigrant/ch4_249_home/249_homeAll.png", 
                 remove_controls = NULL)


# Main points
m <- leaflet() %>% 
  addProviderTiles(providers$Esri.WorldImagery) %>%
  fitBounds(lng1 = min(st_coordinates(homeSitesWGS249)[,1]),
            lng2 = max(st_coordinates(homeSitesWGS249)[,1]),
            lat1 = min(st_coordinates(homeSitesWGS249)[,2]),
            lat2 = max(st_coordinates(homeSitesWGS249)[,2])) %>%
  addPolygons(data = homeWGS249,
              color = "black", weight = 1,
              fillColor = ~factpal(section)) %>%
  addCircleMarkers(data = filter(homeSitesWGS249, panel == "mainSites"),
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
            values = as.character(homeWGS249$section),
            title = "sections") %>%
  addScaleBar()

mapview::mapshot(m, file = "../../../lakeDsn/R10/ch4_249 emigrant/ch4_249_home/249_homeMain.png", 
                 remove_controls = NULL)


# OverSample points
m <- leaflet() %>% 
  addProviderTiles(providers$Esri.WorldImagery) %>%
  fitBounds(lng1 = min(st_coordinates(homeSitesWGS249)[,1]),
            lng2 = max(st_coordinates(homeSitesWGS249)[,1]),
            lat1 = min(st_coordinates(homeSitesWGS249)[,2]),
            lat2 = max(st_coordinates(homeSitesWGS249)[,2])) %>%
  addPolygons(data = homeWGS249,
              color = "black", weight = 1,
              fillColor = ~factpal(section)) %>%
  addCircleMarkers(data = filter(homeSitesWGS249, panel == "OverSamp"),
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
            values = as.character(homeWGS249$section),
            title = "sections") %>%
  addScaleBar()

mapview::mapshot(m, file = "../../../lakeDsn/R10/ch4_249 emigrant/ch4_249_home/249_homeOver.png", 
                 remove_controls = NULL)



# WRITE OBJECTS TO DISK-----------------
# write out table of sample sites for reference in field
write.table(homeSitesWGS249 %>%
              select(panel, siteID, section) %>%
              arrange(panel, section, siteID),
            file = "../../../lakeDsn/R10/ch4_249 emigrant/ch4_249_home/ch4_249_homeSites.txt",
            row.names = FALSE, sep="\t")


#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_
# geopackage
# can write all layers to a geopackage, which behaves much like a geodatabase in ArcGIS.

# WGS coordinate system for ArcPad first

# write out all sites
st_write(obj = homeSitesWGS249, 
         dsn = file.path( "../../../lakeDsn", "homeWGS249.gpkg"), 
         layer = "homeAllSitesWGS249", # package appends 'main.' to layer name?
         layer_options = 'OVERWRITE=YES', # overwrite layer if already present
         #update = TRUE, actually not sure how this differs from 'OVERWRITE=YES'
         driver = "GPKG")

# write out all buffers
st_write(obj = homeSitesWGS249buffer, 
         dsn = file.path( "../../../lakeDsn", "homeWGS249.gpkg"), 
         layer = "homeSitesWGS249buffer", # package appends 'main.' to layer name?
         layer_options = 'OVERWRITE=YES', # overwrite layer if already present
         #update = TRUE, actually not sure how this differs from 'OVERWRITE=YES'
         driver = "GPKG")

# write out main sites
st_write(obj = filter(homeSitesWGS249, panel == "mainSites"),
         dsn = file.path( "../../../lakeDsn", "homeWGS249.gpkg"), 
         layer = "homeMainSitesWGS249",
         layer_options = 'OVERWRITE=YES', # overwrite layer if already present
         driver = "GPKG")

# write out main site buffers
st_write(obj = filter(homeSitesWGS249buffer, panel == "mainSites"),
         dsn = file.path( "../../../lakeDsn", "homeWGS249.gpkg"), 
         layer = "homeMainSitesWGS249buffer",
         layer_options = 'OVERWRITE=YES', # overwrite layer if already present
         driver = "GPKG")

# write out oversample sites
st_write(obj = filter(homeSitesWGS249, panel == "OverSamp"),
         dsn = file.path( "../../../lakeDsn", "homeWGS249.gpkg"), 
         layer = "homeOverSampSitesWGS249",
         layer_options = 'OVERWRITE=YES', # overwrite layer if already present,
         driver = "GPKG")

# write out oversample site buffers
st_write(obj = filter(homeSitesWGS249buffer, panel == "OverSamp"),
         dsn = file.path( "../../../lakeDsn", "homeWGS249.gpkg"), 
         layer = "homeOverSampSitesWGS249buffer",
         layer_options = 'OVERWRITE=YES', # overwrite layer if already present,
         driver = "GPKG")


# write out lake polygon
st_write(obj = homeWGS249,
         dsn = file.path( "../../../lakeDsn", "homeWGS249.gpkg"), 
         layer = "homeWGS249",
         layer_options = 'OVERWRITE=YES', # overwrite layer if already present,
         driver = "GPKG")


st_layers(file.path( "../../../lakeDsn", "homeWGS249.gpkg"))



# Web Mercator for Web Map

# write out all sites
st_write(obj = homeSitesMerc249, 
         dsn = file.path( "../../../lakeDsn", "homeMerc249.gpkg"), 
         layer = "homeAllSitesMerc249", # package appends 'main.' to layer name?
         layer_options = 'OVERWRITE=YES', # overwrite layer if already present
         #update = TRUE, actually not sure how this differs from 'OVERWRITE=YES'
         driver = "GPKG")

# write out all buffers
st_write(obj = homeSitesMerc249buffer, 
         dsn = file.path( "../../../lakeDsn", "homeMerc249.gpkg"), 
         layer = "homeSitesMerc249buffer", # package appends 'main.' to layer name?
         layer_options = 'OVERWRITE=YES', # overwrite layer if already present
         #update = TRUE, actually not sure how this differs from 'OVERWRITE=YES'
         driver = "GPKG")

# write out main sites
st_write(obj = filter(homeSitesMerc249, panel == "mainSites"),
         dsn = file.path( "../../../lakeDsn", "homeMerc249.gpkg"), 
         layer = "homeMainSitesMerc249",
         layer_options = 'OVERWRITE=YES', # overwrite layer if already present
         driver = "GPKG")

# write out main site buffers
st_write(obj = filter(homeSitesMerc249buffer, panel == "mainSites"),
         dsn = file.path( "../../../lakeDsn", "homeMerc249.gpkg"), 
         layer = "homeMainSitesMerc249buffer",
         layer_options = 'OVERWRITE=YES', # overwrite layer if already present
         driver = "GPKG")

# write out oversample sites
st_write(obj = filter(homeSitesMerc249, panel == "OverSamp"),
         dsn = file.path( "../../../lakeDsn", "homeMerc249.gpkg"), 
         layer = "homeOverSampSitesMerc249",
         layer_options = 'OVERWRITE=YES', # overwrite layer if already present,
         driver = "GPKG")

# write out oversample site buffers
st_write(obj = filter(homeSitesMerc249buffer, panel == "OverSamp"),
         dsn = file.path( "../../../lakeDsn", "homeMerc249.gpkg"), 
         layer = "homeOverSampSitesMerc249buffer",
         layer_options = 'OVERWRITE=YES', # overwrite layer if already present,
         driver = "GPKG")


# write out lake polygon
st_write(obj = homeMerc249,
         dsn = file.path( "../../../lakeDsn", "homeMerc249.gpkg"), 
         layer = "homeMerc249",
         layer_options = 'OVERWRITE=YES', # overwrite layer if already present,
         driver = "GPKG")


st_layers(file.path( "../../../lakeDsn", "homeMerc249.gpkg"))

