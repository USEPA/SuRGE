# STRATIFIED, UNEQUAL PROBABILITY GRTS DESIGN

## DESIGN FOR LOWER GOOSE RESERVOIR UNDER 'AVERAGE' CONDITIONS.


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


# LOWER GOOSE RESERVOR
filter(r10res@data, grepl("Goose", NAME)) %>%
  select(AREASQKM, MeanDepth, MaxDepth, pctAggAg, rdaBin, pctAggAgBin)
# 4.1 km^2 PER NHD.  LOWER GOOSE AVERAGE = 2.43KM2.  
# LOWER GOOSE AT HIGH WATER LEVEL = KM2
# NHDPus V2 REPORTS MAX DEPTH OF 19.4, MEAN DEPTH OF 6.5m,
# 0.02% AGGREGATED AGRICULTURE.  [200,500] RDA BIN.

# DAM ON NORTH. BIGGEST TRIB (600MI2) FLOWS IN ON SOUTH.  2ND LARGEST
# (60 MI2) ENTERS ON WEST, NOT IN TRIB STRATA.


# A STRATIFIED-UNEQUAL PROBABILITY GRTS DESIGN
## UNEQUAL PROBABILITY USED IN AN ATTEMPT TO MAKE SAMPLING EASIER ON THE CREW


# CREATE OBJECT TO HOLD FIRST PORTION OF C-DRIVE WORKING DIRECTORY ---------
# TO BE USED FOR ALL SHAPEFILES
rootDir <- "C:/Users/JBEAULIE/GitRepository/r10REM/grtsDesign/"

# READ POLYGON SHAPEFILE-----
# ONLY USED TO CONFIRM THE POLYGON LOOKS RIGHT.  OBJECT NOT DIRECTLY USED IN GRTS CALL
lowerGooseAverageEqArea <- readOGR(dsn = paste(rootDir, "lowerGooseAverage", sep=""), # Could use read.shape from spsurvey package
                          layer = "lowerGooseAverageEqArea")  # shapefile name
plot(lowerGooseAverageEqArea) # visualize polygon

# EXTRACT ATTRIBUTE TABLE
attlowerGooseAverage <- read.dbf(filename = paste(rootDir, "lowerGooseAverage/lowerGooseAverageEqArea", sep=""))

# SET UP FOR GRTS FUNCTION---------
# Call the set.seed function so that the survey designs can be replicate
set.seed(4447864)

# Create the design list
### Five main sites in the tributary.
### the unequal probability splits the open water part of the lake into two sections of almost equal area
lowerGooseAverageDsgn <- list("open_water" = list(panel=c(mainSites=10),
                                         seltype="Unequal",
                                         caty.n=c("north" = 7, # 5.0 sites/km2
                                                  "south" = 3), # 6.1 sites/km2
                                         over=20),
                     "trib"=list(panel=c(mainSites=5), # 15 sites/km2
                                 seltype="Equal",
                                 over=10))

lowerGooseAverageSitesEqArea <- grts(design=lowerGooseAverageDsgn,
                            DesignID="SU", # SU for stratified, unequal
                            type.frame="area",
                            src.frame="shapefile",
                            in.shape=paste(rootDir, "lowerGooseAverage/lowerGooseAverageEqArea", sep=""),
                            att.frame=attlowerGooseAverage,
                            stratum="strata",
                            mdcaty="section",
                            shapefile=TRUE,
                            out.shape=paste(rootDir, "lowerGooseAverage/lowerGooseAverageSitesEqArea", sep=""),
                            prjfilename=paste(rootDir, "lowerGooseAverage/lowerGooseAverageEqArea", sep=""))


# Set up for mutating the "lakeSitesEqArea" file to include all of the fill-able fields
# necessary in order to keep the Equal Area projection
lowerGooseAverageSitesEqArea <- readOGR(dsn = paste(rootDir, "lowerGooseAverage", sep=""), # Could use read.shape from spsurvey package
                               layer = "lowerGooseAverageSitesEqArea")  # shapefile name

# add fill-able fields, preparation for analyzing GRTS results
lowerGooseAverageSitesEqArea@data <- mutate(lowerGooseAverageSitesEqArea@data, 
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
writeOGR(obj = lowerGooseAverageSitesEqArea,  # write projected shapefile to disk for use on field computer
         dsn = paste(rootDir, "lowerGooseAverage", sep=""), 
         layer = "lowerGooseAverageSitesEqArea",
         driver = "ESRI Shapefile",
         overwrite_layer = TRUE)


# Print the initial six lines of the survey design
head(lowerGooseAverageSitesEqArea@data)

# Print/plot the survey design summary
summary(lowerGooseAverageSitesEqArea)

plot(lowerGooseAverageEqArea)
points(lowerGooseAverageSitesEqArea$xcoord, lowerGooseAverageSitesEqArea$ycoord)


# write out table of sample sites for reference in field
# Project to WGS 84 to get lat and longitude
lowerGooseAverageSites84 <- spTransform(x = lowerGooseAverageSitesEqArea, #reproject
                               CRS("+proj=longlat +datum=WGS84"))

lowerGooseAverageSites84@data <- mutate(lowerGooseAverageSites84@data, 
                               long=coordinates(lowerGooseAverageSites84)[,1], # add long to @data slot
                               lat=coordinates(lowerGooseAverageSites84)[,2]) # add lat to @data slot


write.table(lowerGooseAverageSites84@data %>%
              select(panel, siteID, stratum, mdcaty, long, lat) %>%
              arrange(panel, stratum, mdcaty, siteID),
            file = paste(rootDir, "lowerGooseAverage/lowerGooseAverageSites.txt", sep=""),
            row.names = FALSE, sep="\t")


