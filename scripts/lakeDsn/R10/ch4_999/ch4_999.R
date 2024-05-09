# STRATIFIED, UNEQUAL PROBABILITY GRTS DESIGN

## DESIGN FOR BAKER RESERVOIR


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


# BAKER RESERVOR
filter(r10res@data, grepl("Baker", NAME)) %>%
  select(AREASQKM, MeanDepth, MaxDepth, pctAggAg, rdaBin, pctAggAgBin)
# 19 km^2 PER NHD.  NHDPus V2 REPORTS MAX DEPTH OF 41m,MEAN DEPTH OF 14.4m,
# 0.0% AGGREGATED AGRICULTURE.  [0,100] RDA BIN.

# DAM ON NORTH. BIGGEST TRIB (600MI2) FLOWS IN ON SOUTH.  2ND LARGEST
# (60 MI2) ENTERS ON WEST, NOT IN TRIB STRATA.


# A STRATIFIED-UNEQUAL PROBABILITY GRTS DESIGN
## UNEQUAL PROBABILITY USED IN AN ATTEMPT TO MAKE SAMPLING EASIER ON THE CREW


# CREATE OBJECT TO HOLD FIRST PORTION OF C-DRIVE WORKING DIRECTORY ---------
# TO BE USED FOR ALL SHAPEFILES
rootDir <- "C:/Users/JBEAULIE/GitRepository/r10REM/grtsDesign/"

# READ POLYGON SHAPEFILE-----
# ONLY USED TO CONFIRM THE POLYGON LOOKS RIGHT.  OBJECT NOT DIRECTLY USED IN GRTS CALL
bakerEqArea <- readOGR(dsn = paste(rootDir, "baker", sep=""), # Could use read.shape from spsurvey package
                          layer = "bakerEqArea")  # shapefile name
plot(bakerEqArea) # visualize polygon

# EXTRACT ATTRIBUTE TABLE
attbaker <- read.dbf(filename = paste(rootDir, "baker/bakerEqArea", sep=""))

# SET UP FOR GRTS FUNCTION---------
# Call the set.seed function so that the survey designs can be replicate
set.seed(4447864)

# Create the design list
### Five main sites in the tributary.
### the unequal probability splits the open water part of the lake into two sections of almost equal area
bakerDsgn <- list("open_water" = list(panel=c(mainSites=10),
                                         seltype="Unequal",
                                         caty.n=c("lower_north" = 3, # 5.6km2; 0.5 sites/km2
                                                  "lower_south" = 3, # 4.6km2; 0.7 sites/km2
                                                  "upper_west" = 2,  # 4km2; 0.5 sites/km2
                                                  "upper_east" = 2), # 4.5km2; 0.4 sites/km2
                                         over=20),
                     "trib"=list(panel=c(mainSites=5), # 6.9 sites/km2
                                 seltype="Equal",
                                 over=10))

bakerSitesEqArea <- grts(design=bakerDsgn,
                            DesignID="SU", # SU for stratified, unequal
                            type.frame="area",
                            src.frame="shapefile",
                            in.shape=paste(rootDir, "baker/bakerEqArea", sep=""),
                            att.frame=attbaker,
                            stratum="strata",
                            mdcaty="section",
                            shapefile=TRUE,
                            out.shape=paste(rootDir, "baker/bakerSitesEqArea", sep=""),
                            prjfilename=paste(rootDir, "baker/bakerEqArea", sep=""))


# Set up for mutating the "lakeSitesEqArea" file to include all of the fill-able fields
# necessary in order to keep the Equal Area projection
bakerSitesEqArea <- readOGR(dsn = paste(rootDir, "baker", sep=""), # Could use read.shape from spsurvey package
                               layer = "bakerSitesEqArea")  # shapefile name

# add fill-able fields, preparation for analyzing GRTS results
bakerSitesEqArea@data <- mutate(bakerSitesEqArea@data, 
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
writeOGR(obj = bakerSitesEqArea,  # write projected shapefile to disk for use on field computer
         dsn = paste(rootDir, "baker", sep=""), 
         layer = "bakerSitesEqArea",
         driver = "ESRI Shapefile",
         overwrite_layer = TRUE)


# Print the initial six lines of the survey design
head(bakerSitesEqArea@data)

# Print/plot the survey design summary
summary(bakerSitesEqArea)

plot(bakerEqArea)
points(bakerSitesEqArea$xcoord, bakerSitesEqArea$ycoord)


### R10 NEEDS LAT IN DECIMAL DEGREES IN GCS NORTH AMERICAN 1983
### (EPSG: 4269). SEE CRS CODE BELOW

## write out table of sample sites for reference in field
bakerSitesNad83 <- spTransform(x = bakerSitesEqArea, #reproject
                                       CRS("+init=epsg:4269")) # NAD 83

bakerSitesNad83@data <- mutate(bakerSitesNad83@data, 
                                       long=coordinates(bakerSitesNad83)[,1], # add long to @data slot
                                       lat=coordinates(bakerSitesNad83)[,2]) # add lat to @data slot


write.table(bakerSitesNad83@data %>%
              select(panel, siteID, stratum, mdcaty, long, lat) %>%
              arrange(panel, stratum, mdcaty, siteID),
            file = paste(rootDir, "baker/bakerSites.txt", sep=""),
            row.names = FALSE, sep="\t")

# re-write this projected shapefile.  Double chech against Eq Area for consistency
# in ArcGIS Pro
writeOGR(obj = bakerSitesNad83,  # write projected shapefile to disk for checking
         dsn = paste(rootDir, "baker", sep=""), 
         layer = "bakerSitesNad83",
         driver = "ESRI Shapefile",
         overwrite_layer = TRUE)


