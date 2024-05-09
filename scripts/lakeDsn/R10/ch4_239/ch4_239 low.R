# STRATIFIED, UNEQUAL PROBABILITY GRTS DESIGN

## DESIGN FOR PHILLIPS LAKE UNDER 'LOW' CONDITIONS.


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


# PHILLIPS LAKE
filter(r10res@data, grepl("Phillips", NAME)) %>%
  select(AREASQKM, MeanDepth, MaxDepth, pctAggAg, rdaBin, pctAggAgBin)
# 9.1 km^2 PER NHD.  PHILLIPS AVERAGE = 4.5KM2.  PHILLIPS LOW = 2.2KM2
# NHDPus V2 REPORTS MAX DEPTH OF 10.7m, MEAN DEPTH OF 3.6m,
# 75% AGGREGATED AGRICULTURE.  [0,100] RDA BIN.

# PHILLIPS AVERAGE POLYGON, CREATED BY R10, HAS AN AREA OF 4.5KM2.
# R10 MODELED CONTOURS ARE FOR THE 9.1 KM2 LAKE EXTENT, NOT PARTICULARLY
# USEFUL HERE.



# DAM ON EAST. BIGGEST TRIB (92MI2) FLOWS IN ON WEST.  2ND LARGEST
# ENTERS ON NW AND IS INCLUDED IN TRIB STRATA.


# A STRATIFIED-UNEQUAL PROBABILITY GRTS DESIGN
## UNEQUAL PROBABILITY USED IN AN ATTEMPT TO MAKE SAMPLING EASIER ON THE CREW


# CREATE OBJECT TO HOLD FIRST PORTION OF C-DRIVE WORKING DIRECTORY ---------
# TO BE USED FOR ALL SHAPEFILES
rootDir <- "C:/Users/JBEAULIE/GitRepository/r10REM/grtsDesign/"

# READ POLYGON SHAPEFILE-----
# ONLY USED TO CONFIRM THE POLYGON LOOKS RIGHT.  OBJECT NOT DIRECTLY USED IN GRTS CALL
phillipsLowEqArea <- readOGR(dsn = paste(rootDir, "phillipsLow", sep=""), # Could use read.shape from spsurvey package
                          layer = "phillipsLowEqArea")  # shapefile name
plot(phillipsLowEqArea) # visualize polygon

# EXTRACT ATTRIBUTE TABLE
attphillipsLow <- read.dbf(filename = paste(rootDir, "phillipsLow/phillipsLowEqArea", sep=""))

# SET UP FOR GRTS FUNCTION---------
# Call the set.seed function so that the survey designs can be replicate
set.seed(4447864)

# Create the design list
### Five main sites in the tributary.
### the unequal probability splits the open water part of the lake into two sections of almost equal area
phillipsLowDsgn <- list("open_water" = list(panel=c(mainSites=10),
                                         seltype="Unequal",
                                         caty.n=c("west" = 6, # 5.0 sites/km2
                                                  "east" = 4), # 6.1 sites/km2
                                         over=20),
                     "trib"=list(panel=c(mainSites=5), # 15 sites/km2
                                 seltype="Equal",
                                 over=10))

phillipsLowSitesEqArea <- grts(design=phillipsLowDsgn,
                            DesignID="SU", # SU for stratified, unequal
                            type.frame="area",
                            src.frame="shapefile",
                            in.shape=paste(rootDir, "phillipsLow/phillipsLowEqArea", sep=""),
                            att.frame=attphillipsLow,
                            stratum="strata",
                            mdcaty="section",
                            shapefile=TRUE,
                            out.shape=paste(rootDir, "phillipsLow/phillipsLowSitesEqArea", sep=""),
                            prjfilename=paste(rootDir, "phillipsLow/phillipsLowEqArea", sep=""))


# Set up for mutating the "lakeSitesEqArea" file to include all of the fill-able fields
# necessary in order to keep the Equal Area projection
phillipsLowSitesEqArea <- readOGR(dsn = paste(rootDir, "phillipsLow", sep=""), # Could use read.shape from spsurvey package
                               layer = "phillipsLowSitesEqArea")  # shapefile name

# add fill-able fields, preparation for analyzing GRTS results
phillipsLowSitesEqArea@data <- mutate(phillipsLowSitesEqArea@data, 
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
writeOGR(obj = phillipsLowSitesEqArea,  # write projected shapefile to disk for use on field computer
         dsn = paste(rootDir, "phillipsLow", sep=""), 
         layer = "phillipsLowSitesEqArea",
         driver = "ESRI Shapefile",
         overwrite_layer = TRUE)


# Print the initial six lines of the survey design
head(phillipsLowSitesEqArea@data)

# Print/plot the survey design summary
summary(phillipsLowSitesEqArea)

plot(phillipsLowEqArea)
points(phillipsLowSitesEqArea$xcoord, phillipsLowSitesEqArea$ycoord)


# write out table of sample sites for reference in field
# Project to WGS 84 to get lat and longitude
phillipsLowSites84 <- spTransform(x = phillipsLowSitesEqArea, #reproject
                               CRS("+proj=longlat +datum=WGS84"))

phillipsLowSites84@data <- mutate(phillipsLowSites84@data, 
                               long=coordinates(phillipsLowSites84)[,1], # add long to @data slot
                               lat=coordinates(phillipsLowSites84)[,2]) # add lat to @data slot


write.table(phillipsLowSites84@data %>%
              select(panel, siteID, stratum, mdcaty, long, lat) %>%
              arrange(panel, stratum, mdcaty, siteID),
            file = paste(rootDir, "phillipsLow/phillipsLowSites.txt", sep=""),
            row.names = FALSE, sep="\t")


