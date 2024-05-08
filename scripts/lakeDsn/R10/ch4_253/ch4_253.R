# STRATIFIED, UNEQUAL PROBABILITY GRTS DESIGN

## DESIGN FOR LOST VALLEY.
## ORIGINALLY DESIGNED IN 2018 FOR REM WITH R10.  CODE BELOW
## SHOULD BE MODIFIED FOR SURGE REPO.


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


# LOST VALLEY
# 2.11 km^2.  0% AGGREGATED AGRICULTURE.  [0,100] RDA BIN.
# NHDPus V2 REPORTS MAX DEPTH OF 6.6m, MEAN DEPTH OF 18.0m,
# AND AREA OF 2.1KM2, CONSISTENT WITH R10'S 'Contour_Corrected Depths'
# CONTOURS BASED ON THE lakemorpho METHODOLOGY.  CONTOURS BASED ON
# MEASURED NLA DEPTH ARE MUCH LOWER, HOWEVER (>=3 AND <4M).  CAN'T
# REALLY RECONCILE THESE UNTIL ON-SITE.


# DAM ON MIDDLE OF WESTERN SHORELINE. ONLY TRIB IS TO NW.


# A STRATIFIED-UNEQUAL PROBABILITY GRTS DESIGN
## UNEQUAL PROBABILITY USED IN AN ATTEMPT TO MAKE SAMPLING EASIER ON THE CREW


# CREATE OBJECT TO HOLD FIRST PORTION OF C-DRIVE WORKING DIRECTORY ---------
# TO BE USED FOR ALL SHAPEFILES
rootDir <- "C:/Users/JBEAULIE/GitRepository/r10REM/grtsDesign/"

# READ POLYGON SHAPEFILE-----
# ONLY USED TO CONFIRM THE POLYGON LOOKS RIGHT.  OBJECT NOT DIRECTLY USED IN GRTS CALL
lostValleyEqArea <- readOGR(dsn = paste(rootDir, "lostValley", sep=""), # Could use read.shape from spsurvey package
                          layer = "lostValleyEqArea")  # shapefile name
plot(lostValleyEqArea) # visualize polygon

# EXTRACT ATTRIBUTE TABLE
attlostValley <- read.dbf(filename = paste(rootDir, "lostValley/lostValleyEqArea", sep=""))

# SET UP FOR GRTS FUNCTION---------
# Call the set.seed function so that the survey designs can be replicate
set.seed(4447864)

# Create the design list
### Five main sites in the tributary.
### the unequal probability splits the open water part of the lake into two sections of almost equal area
lostValleyDsgn <- list("open_water" = list(panel=c(mainSites=10),
                                         seltype="Unequal",
                                         caty.n=c("north" = 5, # 4.46sites/km2
                                                  "south" = 5), # 4.9sites/km2
                                         over=20),
                     "trib"=list(panel=c(mainSites=5), # 9.8sites/km2
                                 seltype="Equal",
                                 over=10))

lostValleySitesEqArea <- grts(design=lostValleyDsgn,
                            DesignID="SU", # SU for stratified, unequal
                            type.frame="area",
                            src.frame="shapefile",
                            in.shape=paste(rootDir, "lostValley/lostValleyEqArea", sep=""),
                            att.frame=attlostValley,
                            stratum="strata",
                            mdcaty="section",
                            shapefile=TRUE,
                            out.shape=paste(rootDir, "lostValley/lostValleySitesEqArea", sep=""),
                            prjfilename=paste(rootDir, "lostValley/lostValleyEqArea", sep=""))


# Set up for mutating the "lakeSitesEqArea" file to include all of the fill-able fields
# necessary in order to keep the Equal Area projection
lostValleySitesEqArea <- readOGR(dsn = paste(rootDir, "lostValley", sep=""), # Could use read.shape from spsurvey package
                               layer = "lostValleySitesEqArea")  # shapefile name

# add fill-able fields, preparation for analyzing GRTS results
lostValleySitesEqArea@data <- mutate(lostValleySitesEqArea@data, 
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
writeOGR(obj = lostValleySitesEqArea,  # write projected shapefile to disk for use on field computer
         dsn = paste(rootDir, "lostValley", sep=""), 
         layer = "lostValleySitesEqArea",
         driver = "ESRI Shapefile",
         overwrite_layer = TRUE)


# Print the initial six lines of the survey design
head(lostValleySitesEqArea@data)

# Print the survey design summary
summary(lostValleySitesEqArea)

plot(lostValleyEqArea)
points(lostValleySitesEqArea$xcoord, lostValleySitesEqArea$ycoord)

# write out table of sample sites for reference in field
# Project to WGS 84 to get lat and longitude
lostValleySites84 <- spTransform(x = lostValleySitesEqArea, #reproject
                                    CRS("+proj=longlat +datum=WGS84"))

lostValleySites84@data <- mutate(lostValleySites84@data, 
                                    long=coordinates(lostValleySites84)[,1], # add long to @data slot
                                    lat=coordinates(lostValleySites84)[,2]) # add lat to @data slot


write.table(lostValleySites84@data %>%
              select(panel, siteID, stratum, mdcaty, long, lat) %>%
              arrange(panel, stratum, mdcaty, siteID),
            file = paste(rootDir, "lostValley/lostValleySites.txt", sep=""),
            row.names = FALSE, sep="\t")
