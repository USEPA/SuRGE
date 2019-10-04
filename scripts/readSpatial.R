# Ecoregion polygons and sampled manmade water bodies provided
# by ORD Corvallis (details below).

# READ ECOREGION SHAPEFILE PROVIDED BY MARC WEBER 
# Original shapefile provided by Marc Weber on 1/3/2017 in Albers.

# Nine EPA aggregated Ecoregions

# sf approach--------------------
ecoR <- st_read(dsn = "C:/Users/JBEAULIE/GitRepository/NRS/inputData/nla2012",
                layer = "Aggr_Ecoregions9_2015")

# "Temperate" is misspelled.
ecoR <- ecoR %>% 
  mutate(WSA9_NAME = as.character(WSA9_NAME), # conv to char
         WSA9_NAME = ifelse(WSA9_NAME == "Temporate Plains",
                            "Temperate Plains", # correct sp
                            WSA9_NAME),
         WSA9_NAME = as.factor(WSA9_NAME)) # back to factor

# Check CRS
st_crs(ecoR) # no EPSG code
ecoR <- st_transform(ecoR, 5070) # convert to CONUS Albers
st_crs(ecoR) # 5070

# quick map test
ggplot(ecoR) +
  geom_sf(aes(fill = WSA9_NAME))


######################
# NLA 2012 RESERVOIR DATA--------------------------
# Data obtained from Karen Blocksom on 11/22/2016
nla2012 <- read_excel("inputData/nla2012/NLA2012_man-made_lakeorigin_sites.xlsx")

str(nla2012) # 642 systems
unique(nla2012$EVALSTAT)  # All sampled
unique(nla2012$LAKE_ORIGIN) # All man_made
unique(nla2012$LAKE_ORIGIN12) # Man_made subcategories

# Convert to SpatialPointsDataFrame-----------------
nla2012 <- st_as_sf(nla2012, coords = c("LON_DD83", "LAT_DD83"), 
                  crs = 4269) %>% # NAD83 for lat/lon
  st_transform(., 5070) # project to CONUS Albers

plot(nla2012$geometry) # looks good


