# STRATIFIED, UNEQUAL PROBABILITY GRTS DESIGN

##Design for Beulah Reservoir based on NHD lake polygon modified to reflect
##"average" conditions based on Google Earth time series averages.  Polygon
##modifications executed by R10.  Alternative design for larger polygon area
##in beulahAverage.R.  Bathymetry from Portland State University.  


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


# BEULAH RESERVOIR 
# LARGE LAKE (4.8 km^2) WITH TWO TRIB ARMS (NW AND NE). NW TRIB IS THE MALHEUR
# RIVER AND APPEARS TO DRAIN MUCH LARGER AREA THAN NE ARM.
# ~100FT DEEP AT DAM

# A STRATIFIED-UNEQUAL PROBABILITY GRTS DESIGN
## UNEQUAL PROBABILITY USED IN AN ATTEMPT TO MAKE SAMPLING EASIER ON THE CREW
## THIS GRTS DESIGN HAS ALSO BEEN UPDATED TO HAVE THE EXPANDED ATTRIBUTE TABLE IN THE
## "EQAREA" VERSION OF THE SITE SHAPE FILE

# TRIB STRATA EXTENDS TO A DEPTH 8M (26 FT).  THIS IS THE CUTOFF USED FOR
# HARSHA LAKE.  BASING BATHYMETRY ON PORTLAND STATUE UNIVERSITY MAP.  APPEARS
# MORE ACCURATE THAN MODELED CONTOURS.


# CREATE OBJECT TO HOLD FIRST PORTION OF C-DRIVE WORKING DIRECTORY ---------
# TO BE USED FOR ALL SHAPEFILES
rootDir <- "C:/Users/JBEAULIE/GitRepository/r10REM/grtsDesign/"

# READ POLYGON SHAPEFILE-----
# ONLY USED TO CONFIRM THE POLYGON LOOKS RIGHT.  OBJECT NOT DIRECTLY USED IN GRTS CALL
beulahAverageEqArea <- readOGR(dsn = paste(rootDir, "beulahAverage", sep=""), # Could use read.shape from spsurvey package
                             layer = "beulahAverageEqArea")  # shapefile name
plot(beulahAverageEqArea) # visualize polygon

# EXTRACT ATTRIBUTE TABLE
attbeulahAverage <- read.dbf(filename = paste(rootDir, "beulahAverage/beulahAverageEqArea", sep=""))

# SET UP FOR GRTS FUNCTION---------
# Call the set.seed function so that the survey designs can be replicate
set.seed(4447864)

# Create the design list
### Five main sites in the tributary.
### the unequal probability splits the open water part of the lake into two sections of almost equal area
beulahAverageDsgn <- list("open_water" = list(panel=c(mainSites=10),
                                            seltype="Unequal",
                                            caty.n=c("north" = 5, #2.7 sites/km2
                                                     "south" = 5), #2.6 sites/km2
                                            over=20),
                        "trib"=list(panel=c(mainSites=5), #5.2sites/km2
                                    seltype="Equal",
                                    over=10))

beulahAverageSitesEqArea <- grts(design=beulahAverageDsgn,
                               DesignID="SU", # SU for stratified, unequal
                               type.frame="area",
                               src.frame="shapefile",
                               in.shape=paste(rootDir, "beulahAverage/beulahAverageEqArea", sep=""),
                               att.frame=attbeulahAverage,
                               stratum="strata",
                               mdcaty="section",
                               shapefile=TRUE,
                               out.shape=paste(rootDir, "beulahAverage/beulahAverageSitesEqArea", sep=""),
                               prjfilename=paste(rootDir, "beulahAverage/beulahAverageEqArea", sep=""))


# Set up for mutating the "lakeSitesEqArea" file to include all of the fill-able fields
# necessary in order to keep the Equal Area projection
beulahAverageSitesEqArea <- readOGR(dsn = paste(rootDir, "beulahAverage", sep=""), # Could use read.shape from spsurvey package
                                  layer = "beulahAverageSitesEqArea")  # shapefile name

# add fill-able fields, preparation for analyzing GRTS results
beulahAverageSitesEqArea@data <- mutate(beulahAverageSitesEqArea@data, 
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
writeOGR(obj = beulahAverageSitesEqArea,  # write projected shapefile to disk for use on field computer
         dsn = paste(rootDir, "beulahAverage", sep=""), 
         layer = "beulahAverageSitesEqArea",
         driver = "ESRI Shapefile",
         overwrite_layer = TRUE)


# Print the initial six lines of the survey design
head(beulahAverageSitesEqArea@data)

# Print the survey design summary
summary(beulahAverageSitesEqArea)

plot(beulahAverageEqArea)
points(beulahAverageSitesEqArea$xcoord, beulahAverageSitesEqArea$ycoord)


# write out table of sample sites for reference in field
# Project to WGS 84 to get lat and longitude
beulahAverageSites84 <- spTransform(x = beulahAverageSitesEqArea, #reproject
                               CRS("+proj=longlat +datum=WGS84"))

beulahAverageSites84@data <- mutate(beulahAverageSites84@data, 
                               long=coordinates(beulahAverageSites84)[,1], # add long to @data slot
                               lat=coordinates(beulahAverageSites84)[,2]) # add lat to @data slot


write.table(beulahAverageSites84@data %>%
              select(panel, siteID, stratum, mdcaty, long, lat) %>%
              arrange(panel, stratum, mdcaty, siteID),
            file = paste(rootDir, "beulahAverage/beulahAverageSites.txt", sep=""),
            row.names = FALSE, sep="\t")
