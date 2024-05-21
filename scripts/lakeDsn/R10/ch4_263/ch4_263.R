# STRATIFIED, UNEQUAL PROBABILITY GRTS DESIGN

## DESIGN FOR SWOFFORD POND


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


# SWOFFORD POND
filter(r10res@data, grepl("Swofford", NAME)) %>%
  select(AREASQKM, MeanDepth, MaxDepth, pctAggAg, rdaBin, pctAggAgBin)
# 0.84 km^2 PER NHD.  NHDPus V2 REPORTS MAX DEPTH OF 8m, MEAN DEPTH OF 2.9m,
# 17% AGGREGATED AGRICULTURE.  [0,100] RDA BIN.

# SWOFFORD POLYGON, CREATED BY R10, HAS AN AREA OF 0.54KM2 WITH
# MACROPHYTE AREAS REMOVED.  DAM ON EAST END.  MAX DEPTH OF 4M.

# BIGGEST TRIB (3MI2) FLOWS IN FROM SE, NEAR DAM.  2ND LARGEST (1MI2)
# ENTERS ON WEST.  THIS SMALL RESERVOIR IS DEEPEST IN MIDDLE, NOT AT DAM.
# WE WANT SITES NEAR EASTERN TRIB, THOUGH THIS PORTION OF RESERVOIRS
# HAS SMALL AREA.  WILL USE MDCATY SECTIONS TO ACCOMPLISH THIS.  NO
# STRATIFICATION, HOWEVER.


# AN UNSTRATIFIED-UNEQUAL PROBABILITY GRTS DESIGN

# CREATE OBJECT TO HOLD FIRST PORTION OF C-DRIVE WORKING DIRECTORY ---------
# TO BE USED FOR ALL SHAPEFILES
rootDir <- "C:/Users/JBEAULIE/GitRepository/r10REM/grtsDesign/"

# READ POLYGON SHAPEFILE-----
# ONLY USED TO CONFIRM THE POLYGON LOOKS RIGHT.  OBJECT NOT DIRECTLY USED IN GRTS CALL
swoffordEqArea <- readOGR(dsn = paste(rootDir, "swofford", sep=""), # Could use read.shape from spsurvey package
                          layer = "swoffordEqArea")  # shapefile name
plot(swoffordEqArea) # visualize polygon

# EXTRACT ATTRIBUTE TABLE
attswofford <- read.dbf(filename = paste(rootDir, "swofford/swoffordEqArea", sep=""))

# SET UP FOR GRTS FUNCTION---------
# Call the set.seed function so that the survey designs can be replicate
set.seed(4447864)

# Create the design list
swoffordDsgn <- list(None=list(panel=c(mainSites=15), # unstratified, therefore 1 list
                               seltype="Unequal",
                               caty.n=c("west" = 11, # 25 sites/km2
                                        "east" = 4), # 40 sites/km2
                               over=20)) # Equal probability, have been using 20 for oversample sites


# Execute survey design  
swoffordSitesEqArea <- grts(design=swoffordDsgn,
                         DesignID="U", # U for unstratified
                         type.frame="area",
                         src.frame="shapefile",
                         in.shape=paste(rootDir, "swofford/swoffordEqArea", sep=""),
                         att.frame=attswofford,
                         mdcaty="section",
                         shapefile=TRUE,
                         prjfilename = paste(rootDir, "swofford/swoffordEqArea", sep=""),
                         out.shape=paste(rootDir, "swofford/swoffordSitesEqArea", sep=""))

# Set up for mutating the "lakeSitesEqArea" file to include all of the fill-able fields
# necessary in order to keep the Equal Area projection
swoffordSitesEqArea <- readOGR(dsn = paste(rootDir, "swofford", sep=""), # Could use read.shape from spsurvey package
                               layer = "swoffordSitesEqArea")  # shapefile name

# add fill-able fields, preparation for analyzing GRTS results
swoffordSitesEqArea@data <- mutate(swoffordSitesEqArea@data, 
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
writeOGR(obj = swoffordSitesEqArea,  # write projected shapefile to disk for use on field computer
         dsn = paste(rootDir, "swofford", sep=""), 
         layer = "swoffordSitesEqArea",
         driver = "ESRI Shapefile",
         overwrite_layer = TRUE)


# Print the initial six lines of the survey design
head(swoffordSitesEqArea@data)

# Print/plot the survey design summary
summary(swoffordSitesEqArea)

plot(swoffordEqArea)
points(swoffordSitesEqArea$xcoord, swoffordSitesEqArea$ycoord)


# write out table of sample sites for reference in field
# Project to WGS 84 to get lat and longitude
swoffordSites84 <- spTransform(x = swoffordSitesEqArea, #reproject
                               CRS("+proj=longlat +datum=WGS84"))

swoffordSites84@data <- mutate(swoffordSites84@data, 
                            long=coordinates(swoffordSites84)[,1], # add long to @data slot
                            lat=coordinates(swoffordSites84)[,2]) # add lat to @data slot


write.table(swoffordSites84@data %>%
              select(panel, siteID, stratum, mdcaty, long, lat) %>%
              arrange(panel, stratum, mdcaty, siteID),
            file = paste(rootDir, "swofford/swoffordSites.txt", sep=""),
            row.names = FALSE, sep="\t")
