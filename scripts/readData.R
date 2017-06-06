# SCRIPT TO READ IN THE NLA 2012 RESERVOIR DATA

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
# "Temperate" is missspelled.
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
