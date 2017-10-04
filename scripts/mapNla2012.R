# SCRIPT TO MAP THE NLA 2012 RESERVOIR SITES.
# MAP LAYERS ARE 1) AGGREGATED LEVEL III ECOREGIONS,
# 2) ALL SAMPLED MANMADE WATER BODIES, AND 3) US STATES BOUNDARIES.

# Ecoregion polygons and sampled manmade water bodies provided
# by ORD Corvallis (details below).  US states boundaries can be
# obtained via get_map() function, which creates a fortified 
# dataframe derived from a WGS84 projection.  To use this object,
# all other layers must also be derived from a WGS84 projection.
# The problem is that I cannot fortify the WGS84 ecoregion polygons 
# (see below).  The workaround is to use US states boundaries 
# obtained from the US Census Bureau.


######################
# AGGREGATED LEVEL III ECOREGION POLYGONS----------------------
# Shapefile provided by Marc Weber on 1/3/2017
# First, create fortified dataframe derived from Albers projection.
ecoRegAlb <- readOGR("inputData/nla2012", # provided in Albers
                     "Aggr_Ecoregions9_2015")
# "Temperate" is misspelled.
ecoRegAlb@data$WSA9_NAME <- as.character(ecoRegAlb@data$WSA9_NAME) # conv to char
ecoRegAlb@data <- mutate(ecoRegAlb@data, 
                         WSA9_NAME = ifelse(WSA9_NAME == "Temporate Plains",
                                            "Temperate Plains", # correct sp
                                            WSA9_NAME),
                         WSA9_NAME = as.factor(WSA9_NAME)) # back to factor
ecoRegAlb@data$id <- rownames(ecoRegAlb@data)
ecoRegAlb.points = fortify(ecoRegAlb, region="id") 
ecoRegAlb.df = plyr::join(ecoRegAlb.points, ecoRegAlb@data, by="id")

# Second, create fortified dataframe derived from WGS projection to ensure
# compability with US states from get_map().
ecoRegWgs <- spTransform(ecoRegAlb,
                         CRS("+proj=longlat +ellps=WGS84 +datum=WGS84"))
ecoRegWgs@data$id <- rownames(ecoRegWgs@data)
# ecoRegWgs.points = fortify(ecoRegWgs, region="id") # Fortify error
ecoRegWgs.points = create_map_table(ecoRegWgs, "id") # use this instead
names(ecoRegWgs.points)[1] = 'id'
ecoRegWgs.df = plyr::join(ecoRegWgs.points, ecoRegWgs@data, by="id")


######################
# NLA 2012 RESERVOIR DATA--------------------------
# Data obtained from Karen Blocksom on 11/22/2016
nla2012 <- read_excel("inputData/nla2012/NLA2012_man-made_lakeorigin_sites.xlsx")

str(nla2012) # 642 systems
unique(nla2012$EVALSTAT)  # All sampled
unique(nla2012$LAKE_ORIGIN) # All man_made
unique(nla2012$LAKE_ORIGIN12) # Man_made subcategories

# Convert to SpatialPointsDataFrame
nla2012Nad <- as.data.frame(nla2012) # First need to strip tbl_df class
coordinates(nla2012Nad) = ~LON_DD83 + LAT_DD83  # define coordinates
proj4string(nla2012Nad) = CRS("+proj=longlat +datum=NAD83") # supplied as NAD83

# Project to Albers Equal Area using proj4string from ecoregions shapefile.
# This layer can be used with any other layer in Albers projection.
# Needed for spsurvey design
nla2012Alb <- spTransform(nla2012Nad, # changes LON_DD83/LAT_DD83 to new datum
                          CRS(proj4string(ecoRegAlb))) 

# Project to WGS84 to ensure compatability with US states map
# from get_map().
nla2012Wgs <- spTransform(nla2012Nad, # changes LON_DD83/LAT_DD83 to new datum
                          CRS("+proj=longlat +ellps=WGS84 +datum=WGS84")) 

# # Optional: write to disk for use in GIS.
# writeOGR(obj = nla2012Alb,          
#          dsn = "inputData/nla2012",
#          layer = "nla2012Alb",
#          driver = "ESRI Shapefile",
#          overwrite_layer = TRUE)


######################
# STATES MAP----------------------
# Can get us states map with map_data function. The function produces
# a fortified df that can be plotted in ggplot.  The original spatial
# object is in WGS84.  All other spatial data in plot must also be in WGS84.
statesWgs <- map_data("state") 

# Can also use states maps obtained from Census Bureau and converted
# to Albers in GIS.
statesAlb <- readOGR("inputData/nla2012", # provided in Albers
                     "cb_2015_us_state_500k_Alb")
statesAlb@data$id <- rownames(statesAlb@data)
statesAlb.points = fortify(statesAlb, region="id") 
statesAlb.df = plyr::join(statesAlb.points, statesAlb@data, by="id")



######################
# MAP----------------------
# Custom color pallette for ecoregion polygons.  Attempted to mirror
# https://www.epa.gov/national-aquatic-resource-surveys/
# ecoregional-results-national-lakes-assessment-2012
cols <- c("Coastal Plains" = "orange1",
          "Northern Appalachians" = "lightpink1",
          "Northern Plains" = "darksalmon",
          "Southern Appalachians" = "mediumturquoise",
          "Southern Plains" = "khaki4",
          "Temperate Plains" = "forestgreen", 
          "Upper Midwest" = "deepskyblue4",
          "Western Mountains" = "saddlebrown",
          "Xeric" = "lightskyblue4")

# FIRST MAP
# Use all externally supplied data projected to Albers.
# Force plotting order for ecoregion polygons to ensure that islands
# (i.e. western mountain areas bounded by Xeric) are filled properly.
options(scipen=999) # to deal with sci notation in axis labels
ggplot() + 
  geom_polygon(data = subset(ecoRegAlb.df, WSA9_NAME == "Coastal Plains"),
               aes(long,lat,group=group,fill=WSA9_NAME)) +
  geom_polygon(data = subset(ecoRegAlb.df, WSA9_NAME == "Northern Appalachians"),
               aes(long,lat,group=group,fill=WSA9_NAME)) +
  geom_polygon(data = subset(ecoRegAlb.df, WSA9_NAME == "Northern Plains"),
               aes(long,lat,group=group,fill=WSA9_NAME)) +
  geom_polygon(data = subset(ecoRegAlb.df, WSA9_NAME == "Southern Appalachians"),
               aes(long,lat,group=group,fill=WSA9_NAME)) +
  geom_polygon(data = subset(ecoRegAlb.df, WSA9_NAME == "Southern Plains"),
               aes(long,lat,group=group,fill=WSA9_NAME)) +
  geom_polygon(data = subset(ecoRegAlb.df, WSA9_NAME == "Upper Midwest"),
               aes(long,lat,group=group,fill=WSA9_NAME)) +
  geom_polygon(data = subset(ecoRegAlb.df, WSA9_NAME == "Xeric"),
               aes(long,lat,group=group,fill=WSA9_NAME)) +
  geom_polygon(data = subset(ecoRegAlb.df, WSA9_NAME == "Western Mountains"),
               aes(long,lat,group=group,fill=WSA9_NAME)) +
  geom_polygon(data = subset(ecoRegAlb.df, WSA9_NAME == "Temperate Plains"),
               aes(long,lat,group=group,fill=WSA9_NAME)) +
  geom_polygon(data=statesAlb.df, aes(x=long, y=lat, group = group),
                colour="cornsilk3", fill=NA, size = 0.1 ) + 
  scale_fill_manual("Ecoregion", values = cols) +
  geom_point(data = data.frame(nla2012Alb), aes(LON_DD83, LAT_DD83),
             size = 1)  +
  ggtitle("NLA 2012 Man Made Water Bodies") +
  coord_equal()

# Optional
ggsave(filename="output/figures/manmadeSampled.tiff",
       width=8,height=5.5, units="in",
       dpi=800,compression="lzw")

# Note strange longitude values.  0 in center of US?


# SECOND MAP
# Map using WGS projection and us states data from map_data()
# Use coord_map to project final image to Albers
ggplot() + 
  geom_polygon(data = subset(ecoRegWgs.df, WSA9_NAME == "Coastal Plains"),
               aes(coordsx,coordsy,group=poly,fill=WSA9_NAME)) +
  geom_polygon(data = subset(ecoRegWgs.df, WSA9_NAME == "Northern Appalachians"),
               aes(coordsx,coordsy,group=poly,fill=WSA9_NAME)) +
  geom_polygon(data = subset(ecoRegWgs.df, WSA9_NAME == "Northern Plains"),
               aes(coordsx,coordsy,group=poly,fill=WSA9_NAME)) +
  geom_polygon(data = subset(ecoRegWgs.df, WSA9_NAME == "Southern Appalachians"),
               aes(coordsx,coordsy,group=poly,fill=WSA9_NAME)) +
  geom_polygon(data = subset(ecoRegWgs.df, WSA9_NAME == "Southern Plains"),
               aes(coordsx,coordsy,group=poly,fill=WSA9_NAME)) +
  geom_polygon(data = subset(ecoRegWgs.df, WSA9_NAME == "Upper Midwest"),
               aes(coordsx,coordsy,group=poly,fill=WSA9_NAME)) +
  geom_polygon(data = subset(ecoRegWgs.df, WSA9_NAME == "Xeric"),
               aes(coordsx,coordsy,group=poly,fill=WSA9_NAME)) +
  geom_polygon(data = subset(ecoRegWgs.df, WSA9_NAME == "Western Mountains"),
               aes(coordsx,coordsy,group=poly,fill=WSA9_NAME)) +
  geom_polygon(data = subset(ecoRegWgs.df, WSA9_NAME == "Temperate Plains"),
               aes(coordsx,coordsy,group=poly,fill=WSA9_NAME)) +
  geom_polygon(data=statesWgs, aes(x=long, y=lat, group = group),
               colour="cornsilk3", fill=NA, size = 0.1 ) + 
  geom_point(data = data.frame(nla2012Wgs), 
             aes(LON_DD83, LAT_DD83), size = 1) +
  scale_fill_manual("Ecoregion", values = cols) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_blank(),
        legend.position = c(1, .5), # move legend to immediate right of plot
        plot.margin=unit(c(0,22,0,0),"mm") ) + #extend right plot mar for legend
  coord_map("albers",  at0 = 45.5, lat1 = 29.5) # Project to Albers

  
ggsave(filename="output/figures/manMadeSampledWgs.tiff",
       width=8,height=5.5, units="in",
       dpi=800,compression="lzw")



