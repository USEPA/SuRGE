# STRATIFIED, EQUAL PROBABILITY GRTS DESIGN

## DESIGN FOR WAPATO LAKE


## WHEN MODIFIYING FOR ANOTHER LAKE MAKE THE FOLLOWING CHANGES: 
##  MODIFY THE GRTS DESIGN LIST FOR THE NUMBER OF MAIN AND OVERSAMPLE SITES WANTED 
##  FOR EACH STRATA AND EACH SECTION, NOMINALLY:
##    OPEN WATER MAINSITES = 10
##            SECTION A (NORTH) = X
##            SECTION B (SOUTH) = 10-X
##    OPEN WATER OVER SAMPLE = 20
##    TRIBUTARY MAIN SITES = 5
##    TRIBUTARY OVER SAMPLE = 10
##  CHANGE THE ZOOM FACTOR ON LINE 179
##  FIND AND REPLACE ALL INSTANCES OF THE LAKE NAME


# WAPATO LAKE
filter(r10res@data, grepl("Wapato", NAME)) %>%
  select(NAME, AREASQKM, MeanDepth, MaxDepth, pctAggAg, rdaBin, pctAggAgBin)
# 0.77 km^2 PER NHD.  NHDPus V2 REPORTS MAX DEPTH OF 6m, MEAN DEPTH OF 2.7m,
# 16% AGGREGATED AGRICULTURE.  [0,100] RDA BIN.

# VERY DIFFICULT TO IDENTIFY DAM ON IMAGERY, BUT MUST BE ON WEST
# END, DRAINING INTO LAKE CHELAN.  NO NAVIGATION HAZARDS.  SHOULD
# BE AN EASY LAKE TO SAMPLE.  NO NEED FOR UNEQUAL PROBABILITY
# CATEGORIES.

# HYDROLOGY IS DIFFICULT TO MAKE OUT.  STREAMCAT DERVIED FLOW LINES
# DIFFER FROM MAPPED TRIB.  DRAINAGE AREA APPEARS TO BE SMALL.


# A STRATIFIED-EQUAL PROBABILITY GRTS DESIGN


# CREATE OBJECT TO HOLD FIRST PORTION OF C-DRIVE WORKING DIRECTORY ---------
# TO BE USED FOR ALL SHAPEFILES
rootDir <- "C:/Users/JBEAULIE/GitRepository/r10REM/grtsDesign/"

# READ POLYGON SHAPEFILE-----
# ONLY USED TO CONFIRM THE POLYGON LOOKS RIGHT.  OBJECT NOT DIRECTLY USED IN GRTS CALL
wapatoEqArea <- readOGR(dsn = paste(rootDir, "wapato", sep=""), # Could use read.shape from spsurvey package
                            layer = "wapatoEqArea")  # shapefile name
plot(wapatoEqArea) # visualize polygon

# EXTRACT ATTRIBUTE TABLE
attwapato <- read.dbf(filename = paste(rootDir, "wapato/wapatoEqArea", sep=""))

# SET UP FOR GRTS FUNCTION---------
# Call the set.seed function so that the survey designs can be replicate
set.seed(4447864)

# Create the design list
### Five main sites in the tributary.
### the unequal probability splits the open water part of the lake into two sections of almost equal area
wapatoDsgn <- list("open_water" = list(panel=c(mainSites=11), # 16 sites/km2
                                       seltype="Equal", 
                                       over=20),
                   "trib"=list(panel=c(mainSites=4), # 57 sites/km2
                               seltype="Equal",
                               over=10))

wapatoSitesEqArea <- grts(design=wapatoDsgn,
                              DesignID="SU", # SU for stratified, unequal
                              type.frame="area",
                              src.frame="shapefile",
                              in.shape=paste(rootDir, "wapato/wapatoEqArea", sep=""),
                              att.frame=attwapato,
                              stratum="strata",
                              shapefile=TRUE,
                              out.shape=paste(rootDir, "wapato/wapatoSitesEqArea", sep=""),
                              prjfilename=paste(rootDir, "wapato/wapatoEqArea", sep=""))


# Set up for mutating the "lakeSitesEqArea" file to include all of the fill-able fields
# necessary in order to keep the Equal Area projection
wapatoSitesEqArea <- readOGR(dsn = paste(rootDir, "wapato", sep=""), # Could use read.shape from spsurvey package
                                 layer = "wapatoSitesEqArea")  # shapefile name

# add fill-able fields, preparation for analyzing GRTS results
wapatoSitesEqArea@data <- mutate(wapatoSitesEqArea@data, 
                                     deplyDt = "",    # adding all of these colums to the 
                                     deplyTm = "",    # shape file to be filled in the field
                                     chmStTm = "",  # tried to enter them in the order they will be filled
                                     chm_vol = "",
                                     bbblngO = "",                                
                                     wtrDpth = "", 
                                     smDpthS = "",
                                     Tmp_C_S = "",
                                     DOPrc_S = "",
                                     DO__L_S = "",
                                     SpCn__S = "",
                                     pH_S = "",
                                     ORP_S = "",
                                     TrNTU_S = "",
                                     chla_S = "",
                                     smDpthD = "",
                                     Tmp_C_D = "",
                                     DOPrc_D = "",
                                     DO__L_D = "",
                                     SpCn__D = "",
                                     pH_D = "",
                                     ORP_D = "",
                                     TrNTU_D = "",
                                     chla_D = "",
                                     ArExtnrs = "",
                                     DG_Extn = "",
                                     H2O_vol = "",
                                     HeVol = "",
                                     BrPrssr = "",
                                     RtrvDat = "",
                                     RtrvTim = "",
                                     TtTrpVl = "",
                                     TrapExtn = "",
                                     Notes = "",
                                     LatSamp = "",
                                     LongSmp = ""
)



# re-write this mutated file, will keep the equal area projection
writeOGR(obj = wapatoSitesEqArea,  # write projected shapefile to disk for use on field computer
         dsn = paste(rootDir, "wapato", sep=""), 
         layer = "wapatoSitesEqArea",
         driver = "ESRI Shapefile",
         overwrite_layer = TRUE)


# Print the initial six lines of the survey design
head(wapatoSitesEqArea@data)

# Print the survey design summary
summary(wapatoSitesEqArea)

plot(wapatoEqArea)
points(wapatoSitesEqArea$xcoord, wapatoSitesEqArea$ycoord)

# write out table of sample sites for reference in field
# Project to WGS 84 to get lat and longitude
wapatoSites84 <- spTransform(x = wapatoSitesEqArea, #reproject
                                 CRS("+proj=longlat +datum=WGS84"))

wapatoSites84@data <- mutate(wapatoSites84@data, 
                                 long=coordinates(wapatoSites84)[,1], # add long to @data slot
                                 lat=coordinates(wapatoSites84)[,2]) # add lat to @data slot


write.table(wapatoSites84@data %>%
              select(panel, siteID, stratum, mdcaty, long, lat) %>%
              arrange(panel, stratum, mdcaty, siteID),
            file = paste(rootDir, "wapato/wapatoSites.txt", sep=""),
            row.names = FALSE, sep="\t")
