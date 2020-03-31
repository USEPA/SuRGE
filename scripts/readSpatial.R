# Ecoregion polygons and sampled manmade water bodies provided
# by ORD Corvallis (details below).

# READ ECOREGION SHAPEFILE PROVIDED BY MARC WEBER ------------------
# Original shapefile provided by Marc Weber on 1/3/2017 in Albers.

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




# NLA 2007 RESERVOIR DATA--------------------------
# First data set contains chem, but not COMID
nla07.chem <- read.table("C:/Users/JBEAULIE/GitRepository/NRS/inputData/nla2007/nla2007_chemical_conditionestimates.txt", 
                    header = TRUE, sep = ",", as.is = TRUE, na.strings = "") %>%
  select(SITE_ID, VISIT_NO, CHLA) 

nla07.site <- read.csv("C:/Users/JBEAULIE/GitRepository/NRS/inputData/nla2007/NLA2007_SampledLakeInformation_20091113.csv", 
                         header = TRUE, sep = ",", as.is = TRUE, na.strings = "") %>%
  select(SITE_ID, VISIT_NO, LAKE_SAMP, COM_ID, WSA_ECO9, LAKE_ORIGIN, AREA_HA, DEPTH_X, DEPTHMAX)

# Merge NLA07 files
dim(nla07.chem) # 1252, 3
dim(nla07.site) # 1252, 8
nla07 <- inner_join(nla07.site, nla07.chem)
dim(nla07) # 1252, 10

# Filter for Target-Sampled man-made
nla07 <- nla07 %>%
  filter(LAKE_SAMP == "Target_Sampled",
         LAKE_ORIGIN == "MAN-MADE") %>%
  select(-LAKE_SAMP, -LAKE_ORIGIN) # remove these columns
dim(nla07) # 688, 8

# Aggregate across repeat visits
# No warnings, all good
nla07 <- nla07 %>%
  group_by(SITE_ID) %>%
  summarise(COM_ID = unique(COM_ID), # should be identical for repeat visits at same site.  If not, will throw warning
            WSA_ECO9 = unique(WSA_ECO9), # should be identical for repeat visits at same site.  If not, will throw warning
            CHLA = mean(CHLA),
            AREA_HA = mean(AREA_HA), 
            DEPTH_X = mean(DEPTH_X), # index site depth
            DEPTHMAX = mean(DEPTHMAX) # max observed lake depth
            ) %>%
  rename(INDEX_SITE_DEPTH = DEPTH_X,
         COMID = COM_ID,
         AG_ECO9 = WSA_ECO9)

dim(nla07) # 634, 7

# Check for dups
filter(nla07, duplicated(COMID) | duplicated(COMID, fromLast = TRUE)) # a few duplicated COMID that don't make sense. 
# Probably best to omit these.  No way to know if they were sampled in subsequent NLA surveys
nla07 <- nla07 %>% filter(!(duplicated(COMID) | duplicated(COMID, fromLast = TRUE)))
dim(nla07) # 626, 7





# NLA 2012 RESERVOIR DATA--------------------------
# Data from NLA website containing 'key variables' including chemistry
nla12 <- read.table("C:/Users/JBEAULIE/GitRepository/NRS/inputData/nla2012/NLA_2012_Key_Variables_Data.txt",
                        sep = ",", header = TRUE, as.is = TRUE) %>%
  select(SITE_ID, COMID2012, AGGR_ECO9_2015, LAKE_ORIGIN, INDEX_SITE_DEPTH, CHLX_RESULT, AREA_HA, # variables of interest
         INDEX_LON_DD, INDEX_LAT_DD) %>% # for sf below
  rename(AG_ECO9 = AGGR_ECO9_2015, CHLA = CHLX_RESULT, COMID = COMID2012) # rename for consistency across datasets
dim(nla12) # 1138, 9


# Filter for Target-Sampled man-made
nla12 <- nla12 %>%
  filter(LAKE_ORIGIN == "MAN_MADE") %>%
  select(-LAKE_ORIGIN) # remove column

dim(nla12) # 642, 8
  
# Aggregate across repeat visits
# No warnings, all good
nla12 <- nla12 %>%
  group_by(SITE_ID) %>%
  summarise(COMID = unique(COMID), # should be identical for repeat visits at same site.  If not, will throw warning
            AG_ECO9 = unique(AG_ECO9), # should be identical for repeat visits at same site.  If not, will throw warning
            CHLA = mean(CHLA),
            AREA_HA = mean(AREA_HA), 
            INDEX_SITE_DEPTH = mean(INDEX_SITE_DEPTH), # index site depth
            INDEX_LON_DD = mean(INDEX_LON_DD), # removed below
            INDEX_LAT_DD = mean(INDEX_LAT_DD) # removed below
  )  

dim(nla12) # 581, 8

# Check for dups
filter(nla12, duplicated(COMID) | duplicated(COMID, fromLast = TRUE)) # no dups 

# Convert to SpatialPointsDataFrame
nla12.sf <- st_as_sf(nla12, coords = c("INDEX_LON_DD", "INDEX_LAT_DD"),
                  crs = 4269) %>% # NAD83 for lat/lon
  st_transform(., 5070) # project to CONUS Albers

plot(nla12.sf$geometry) # looks good


# Remove lat/long from df
nla12 <- nla12 %>% select(-INDEX_LON_DD, -INDEX_LAT_DD)
dim(nla12) # 581, 6





# NLA 2017 RESERVOIR DATA--------------------------
# SITE DATA
# Data obtained from Tony Olsen on 10/30/2019
nla17.site <- read_excel("inputData/nla2017/NLA17_Man_Made_Target_Sampled_Sites.xlsx") %>%
  select(SITE_ID, COMID, AG_ECO9, AREA_HA)


# CHEMISTRY DATA
# Chemistry from Paulsen, 10/4/19
# Raw data file has .tab extension, which is just a tab delimited file.
nla17.chem <- read.table("inputData/nla2017/waterChem_wide.tab", sep ="\t", 
                         stringsAsFactors = FALSE, header = TRUE) %>%
  select(CHLA_RESULT_VOL, # chla.result.vol is ug/l
         SITE_ID) %>% #   remove visit no
  rename(CHLA = CHLA_RESULT_VOL) %>%
  group_by(SITE_ID) %>% # 96 duplicate site ids, revisits
  summarize_all(mean, na.rm = TRUE) # aggregate site visists

# Quick inspection
nla17.chem %>% distinct(SITE_ID) %>% # 1091 sites
  summarize(nSites = n())

# Merge site and chem data
nla17 <- dplyr::left_join(nla17.site, nla17.chem) # only keep man-made sites
dim(nla17.chem) # 1091, all sites, man-made and natural
dim(nla17.site) # 522, 4 only man-made sites
dim(nla17) # 522, 5

# Do we have COMID, CHLA, and AREA_HA for all records?
filter(nla17, is.na(CHLA)) # missing 5 chl
filter(nla17, is.na(COMID)) # 0 missing
filter(nla17, is.na(AREA_HA)) # 0 missing 

# Dups?
filter(nla17, duplicated(COMID))

# Spatial data for mapping
nla17.sf <- read_excel("inputData/nla2017/NLA17_Man_Made_Target_Sampled_Sites.xlsx") %>%
  select(LAT_DD83, LON_DD83)

# Convert to SpatialPointsDataFrame
nla17.sf <- st_as_sf(nla17.lat.lon, coords = c("LON_DD83", "LAT_DD83"),
                     crs = 4269) %>% # NAD83 for lat/lon
  st_transform(., 5070) # project to CONUS Albers

plot(nla17.sf$geometry) # looks good


# LAKE MORPHO DATA-----------------------------
# READ DATA
# lakeMorpho data in two places.  First, is from NHDPlusV2, downloaded 11/7/2019.  Also have separate lakeMorpo.gdb 
# downloaded from EDG via link provided by Jeff Hollister on 12/11/2017.  See e-mail from Jeff in lakeMorph directory.
morph.sf <- sf::st_read(dsn = paste0("C:/Users/JBEAULIE/OneDrive - Environmental Protection Agency (EPA)/",
                                   "GIS_data/lakeMorpho/LakeMorphGdb.gdb"),
                      layer = "allRegions", stringsAsFactors = FALSE) 

morph.df <- as.data.frame(morph.sf) %>% 
  select(COMID, MaxDepthCorrect, MeanDepthCorrect) # grab what is needed for survey design

dim(morph.df) # 363314 as compared to 448,512 in NHD.  Missing ones are likely weird FTYPES

morph.df %>% 
  filter(is.na(MeanDepthCorrect)) %>% {dim(.)} # only 7 missing values!

# Duplicated?
duplicated(morph.df$COMID) %>% sum(.) # 7 dups

# Have a look
# these have very similar values.  Just omit one of each duplicated record
filter(morph.df, duplicated(COMID) | duplicated(COMID, fromLast = TRUE)) %>%
  arrange(COMID)

morph.df <- morph.df %>% filter(!duplicated(COMID)) # omit duplicated.


# NHDPlusV2 DATA------------------------
# Downloaded from horizon on 11/7/2019
# Read NHDPlusV2 for entire country.  About 4 minutes with sf.
nhd.sf <- sf::st_read(dsn = paste0("C:/Users/JBEAULIE/OneDrive - Environmental Protection Agency (EPA)/",
                                   "GIS_data/NHDPlusV2/NHDPlusNationalData/NHDPlusV21_National_Seamless_Flattened_Lower48.gdb"),
                      layer = "NHDWaterbody", 
                      stringsAsFactors = FALSE) 

# Check CRS
st_crs(nhd.sf) # 4269, lat lon
nhd.sf <- st_transform(nhd.sf, 5070) # project to CONUS Albers 

# convert to df
nhd.df <- as.data.frame(nhd.sf) %>% # for subsequent merging
  select(COMID, AREASQKM, FTYPE, FCODE, MeanDepth, MaxDepth, MeanDUsed, MeanDCode) %>% # grab stuff we need for design analysis
  rename(NhdMeanDepth = MeanDepth,
         NhdMaxDepth = MaxDepth,
         NhdMeanDUsed = MeanDUsed,
         NhdMeanDCode = MeanDCode) %>%
  mutate_at(vars(contains("Depth")),  # replace -9998 values with NA
            list(~replace(., . == -9998, NA)))

# dimension/dups
str(nhd.df) # 448512  8
duplicated(nhd.df$COMID) %>% sum(.) # 218 dups?
ind.dup <- filter(nhd.df, duplicated(COMID, fromLast = FALSE) | duplicated(COMID, fromLast = TRUE)) %>% 
  select(COMID) %>% pull()

nhd.df %>% filter(COMID %in% ind.dup) %>% {table(.$FTYPE)} # almost all SwampMarsh
# get rid off dup that are not LakePond or Reservoir
nhd.df <- nhd.df %>% 
  filter(!( # exclude those that meet these criteria
    COMID %in% ind.dup & # true if duplicate
      FTYPE %in% c("Estuary", "Ice Mass", "Inundation Area", # TRUE if one of these
                   "Playa", "SwampMarsh")))

nhd.df %>% filter(COMID %in% ind.dup) %>% {table(.$FTYPE)} # only 8 LakePond and 8 Reservoir

filter(nhd.df, duplicated(COMID) | duplicated(COMID, fromLast = TRUE)) %>% arrange(COMID)

# these appear to be real dups, just omit one record of each dup.
nhd.df <- nhd.df %>% filter(!duplicated(COMID))

# Check FTYPE/FCODE
table(nhd.df$FTYPE) # LakePond + Reservoir = 373871 + 5226 = 379097
# NLA excludes: see NLA12 design summary + https://nhd.usgs.gov/userGuide/Robohelpfiles/NHD_User_Guide/Feature_Catalog/Hydrography_Dataset/Complete_FCode_List.htm
fcode <- c(39001, 39006, 39005, 36100, 43601, 43609, 43606, 43607, 43605, 43612)

# How many sytems meet our criteria. 301242! as compared to 363314 in lakemorph.  Looking good.
filter(nhd.df, FTYPE %in% c("LakePond", "Reservoir"), # only these FTYPES
       !(FCODE %in% fcode)) # exclude these FTYPES






# MERGE NLA, ALL YEARS------------------
# Need to create df of unique man-made sampled in 07, 12, and 17
# Priority is give to 17, then 12, then 07

# What NLA12 not present in NLA17
sum(nla12$COMID %in% nla17$COMID) # 241 man-made present in NLA12 and NLA17
nla12.unique <- filter(nla12, !(COMID %in% nla17$COMID)) # NLA12 man-made not present in NLA17
dim(nla12.unique) # 340 in NLA12, not in NLA17


# What NLA07 not present in NLA12 or NLA17?
sum(nla07$COMID %in% c(nla17$COMID, nla12.unique$COMID)) # 181 man-made NLA07 present in NLA12 and/or NLA12
nla07.unique <- filter(nla07, !(COMID %in% c(nla17$COMID, nla12.unique$COMID)))
dim(nla07.unique) # another 445 sites!

dim(nla17) # 522
dim(nla12.unique) # 340
dim(nla07.unique) # 445

nla <- bind_rows(nla17, nla12.unique, nla07.unique)
dim(nla) # 1307, good 07 + 12 + 17

filter(nla, duplicated(COMID)) # no dups

# Add field to identify which NLA
nla <- nla %>% mutate(NLA = ifelse(grepl("NLA17", SITE_ID),
                                   "NLA17",
                                   ifelse(grepl("NLA12", SITE_ID),
                                          "NLA12",
                                          "NLA07")))



# MERGE lakeMorpho, NHD, AND ALL NLA YEARS---------------------------------
# Start with nhd and morph
nhd.morph <- merge(nhd.df, morph.df,
                   by = "COMID", all = TRUE)
dim(morph.df) # 363,307
dim(nhd.df) # 448,084
dim(nhd.morph) # 448,084, good, preserve all nhd objects

sum(duplicated(nhd.morph$COMID)) # 0 dups

# now merge with nla
nla.nhd.morph <- left_join(nla, nhd.morph)
dim(nhd.morph) # 448,084
dim(nla) # 1307
dim(nla.nhd.morph) # 1307


# INSPECT DEPTH ESTIMATES---------------------------------------------
names(nla.nhd.morph)
# We have depth estimates from NLA, NHD, and morpho.  Consolidate into
# best depth estimate.

# How well populated is depth field?
filter(nla.nhd.morph, is.na(NhdMeanDUsed)) %>% # NHD: missing 398 out of 1307.  NHD data not great
  {dim(.)} 

filter(nla.nhd.morph, is.na(MeanDepthCorrect)) %>% # MORPHO: preferred depth source, missing 144 out of 1307, better
  {dim(.)} 

filter(nla.nhd.morph, is.na(INDEX_SITE_DEPTH)) %>% # NLA: missing 529
  {dim(.)}


# Create best depth column from the three depth sources
nla.nhd.morph <- nla.nhd.morph %>%
  # mean depth estimate
  mutate(MeanDBest = ifelse(is.na(MeanDepthCorrect) & !is.na(NhdMeanDUsed), # morpho missing, NHD is sensical
                            NhdMeanDUsed, # use NHD
                            ifelse(is.na(MeanDepthCorrect) & is.na(NhdMeanDUsed) & !is.na(INDEX_SITE_DEPTH), # morpho and nhd estimates missing, NLA present
                                   INDEX_SITE_DEPTH/2, # use index site depth, divide by 2 to estimate mean depth 
                                   ifelse(MeanDepthCorrect == 0 & !is.na(NhdMeanDUsed), # morpho is zero, NHD is sensical
                                   NhdMeanDUsed, # use NHD
                                   ifelse(MeanDepthCorrect == 0 & is.na(NhdMeanDUsed) & !is.na(INDEX_SITE_DEPTH), # morpho is 0, NHD is missing, NLA is available
                                          INDEX_SITE_DEPTH/2, # use index site depth, divide by 2 to estimate mean depth 
                                          ifelse(MeanDepthCorrect == 0 & !is.na(NhdMeanDUsed) & !is.na(INDEX_SITE_DEPTH),
                                          0.45, # replace with 0.45 per NHDPlusV2
                                          MeanDepthCorrect)))))) # else mean depth correct

# Now how many missing best mean depths?
filter(nla.nhd.morph, is.na(MeanDBest)) %>% # 55 missing depth estimate
  {dim(.)}

# how big are these things?
filter(nla.nhd.morph, is.na(MeanDBest)) %>% # exclude weird FTYPES
  select(AREA_HA) %>% pull() # all smaller than 8Ha.  I think these are NLA17 pulled from NHDPlus



# Max depth estimate
# NLA: "INDEX_SITE_DEPTH", "DEPTHMAX".  DEPTHMAX is greatest depth noted. 
# NHD: NhdMaxDepth
# MORPHO: MaxDepthCorrect
# How well populated is max depth field?
filter(nla.nhd.morph, is.na(NhdMaxDepth)) %>% # NHD: missing 414 out of 1307.  NHD data not great
  {dim(.)} 

filter(nla.nhd.morph, is.na(MaxDepthCorrect)) %>% # MORPHO: preferred depth source, missing 144 out of 1307, better
  {dim(.)} 

filter(nla.nhd.morph, is.na(DEPTHMAX)) %>% # NLA: missing 863
  {dim(.)}

filter(nla.nhd.morph, is.na(INDEX_SITE_DEPTH)) %>% # NLA: missing 529
  {dim(.)}

# create best max depth estimate
nla.nhd.morph <- nla.nhd.morph %>%
  mutate(NLA_DEEPEST = ifelse(is.na(DEPTHMAX) & !is.na(INDEX_SITE_DEPTH), # temporary variable to hold deepest NLA value
                              INDEX_SITE_DEPTH,
                              ifelse(!is.na(DEPTHMAX) & is.na(INDEX_SITE_DEPTH),
                                     DEPTHMAX,
                              ifelse(DEPTHMAX > INDEX_SITE_DEPTH,
                                     DEPTHMAX,
                                     INDEX_SITE_DEPTH)))) %>%
  # best max depth estimate  ####################################3PICK UP HERE WITH FISRT LINE !=0
  mutate(MaxDBest = ifelse(is.na(MaxDepthCorrect) & !is.na(NhdMaxDepth) & NhdMaxDepth != 0, # morpho missing, NHD is sensical
                            NhdMaxDepth, # use NHD
                            ifelse(is.na(MaxDepthCorrect) & is.na(NhdMaxDepth) & !is.na(NLA_DEEPEST), # morpho and nhd estimates missing, NLA present
                                   NLA_DEEPEST, # use NLA depth
                                   ifelse(MaxDepthCorrect == 0 & !is.na(NhdMaxDepth), # morpho is zero, NHD is sensical
                                          NhdMaxDepth, # use NHD
                                          ifelse(MaxDepthCorrect == 0 & is.na(NhdMaxDepth) & !is.na(NLA_DEEPEST), # morpho is 0, NHD is missing, NLA is available
                                                 NLA_DEEPEST, # use NLA depth
                                                 MaxDepthCorrect))))) %>% # else morpho data
  select(-NLA_DEEPEST) # remove temporary variable

# Now how many missing best max depths?
filter(nla.nhd.morph, is.na(MaxDBest)) %>% # 55 missing depth estimate, I think these are NLA17 pulled from NHDPlus
  {dim(.)}

# how big are these things?
filter(nla.nhd.morph, is.na(MaxDBest)) %>% 
  select(AREA_HA) %>% pull() # none exceed 8Ha.


# FINALIZE DATA FOR USE IN SURVEY DESIGN FILE-----------------
dsnDat <- nla.nhd.morph



