# UNSTRATIFIED, EQUAL PROBABILITY GRTS DESIGN
## WHEN MODIFIYING FOR ANOTHER LAKE MAKE THE FOLLOWING CHANGES: 
## MODIFY THE GRTS DESIGN LIST FOR THE NUMBER OF MAIN AND OVERSAMPLE SITES WANTED
## NOMINAL MAINSITES = 15
## NOMINAL OVER SAMPLE = 20
## CHANGE THE ZOOM FACTOR ON LINE 179
## FIND AND REPLACE ALL INSTANCES OF THE LAKE NAME

# SPRINGFIELD RESERVOIR GRTS DESIGN
filter(r10res@data, grepl("Springfield", NAME)) %>%
  select(NAME, STATE, LAT_DD_N83, LON_DD_N83, AREASQKM,
         MeanDepth, MaxDepth, pctAggAg, rdaBin, pctAggAgBin)

# SMALL LAKE (0.21 km^2, MEAN DEPTH = 0.18m, MAX DEPTH =0.38m) 
# WITH NO OBVIOUS 'TRIBUTARY AREA'.
# WILL USE AN UNSTRATIFIED-EQUAL PROBABILITY GRTS DESIGN


# CREATE OBJECT TO HOLD FIRST PORTION OF C-DRIVE WORKING DIRECTORY ---------
# TO BE USED FOR ALL SHAPEFILES
rootDir <- "C:/Users/JBEAULIE/GitRepository/r10REM/grtsDesign/"

# READ/PLOT SHAPEFILE ---------
springfieldEqArea <- readOGR(dsn = paste(rootDir, "springfield", sep=""), # Could use read.shape from spsurvey package
                       layer = "springfieldEqArea")  # shapefile name
plot(springfieldEqArea) # visualize polygon



# EXTRACT ATTRIBUTE TABLE -----------  
attspringfield <- read.dbf(filename = paste(rootDir, "springfield/springfieldEqArea", sep=""))



# SET UP FOR AND RUN GRTS FUNCTION   -------------
# Call the set.seed function so that the survey designs can be replicate
set.seed(4447864)      #4447864

# Create the design list
springfieldDsgn <- list(None=list(panel=c(mainSites=15), # unstratified, therefore 1 list
                            seltype="Equal",
                            over = 20)) # Equal probability, have been using 20 for oversample sites


# Execute survey design  
springfieldSitesEqArea <- grts(design=springfieldDsgn,
                         DesignID="U", # U for unstratified
                         type.frame="area",
                         src.frame="shapefile",
                         in.shape=paste(rootDir, "springfield/springfieldEqArea", sep=""),
                         att.frame=attspringfield,
                         shapefile=TRUE,
                         prjfilename = paste(rootDir, "springfield/springfieldEqArea", sep=""),
                         out.shape=paste(rootDir, "springfield/springfieldSitesEqArea", sep=""))

# new feature added 14 June 2016
# Add additional fields to springfieldSitesEqArea.shp attribute table
springfieldSitesEqArea <- readOGR(dsn = paste(rootDir, "springfield", sep=""), # Could use read.shape from spsurvey package
                            layer = "springfieldSitesEqArea")  # shapefile name

# add fill-able fields, preparation for analyzing GRTS results
springfieldSitesEqArea@data <- mutate(springfieldSitesEqArea@data, 
                                deplyDt = "",    # field to be populated after study 
                                deplyTm = "",    
                                chmStTm = "",    
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
writeOGR(obj = springfieldSitesEqArea,  # write projected shapefile to disk for use on field computer
         dsn = paste(rootDir, "springfield", sep=""), 
         layer = "springfieldSitesEqArea",
         driver = "ESRI Shapefile",
         overwrite_layer = TRUE)

# Print the initial six lines of the survey design ------------
head(springfieldSitesEqArea@data)


# Print the survey design summary
summary(springfieldSitesEqArea)

## write out table of sample sites for reference in field
# Project to WGS 84 to get lat and longitude
springfieldSites84 <- spTransform(x = springfieldSitesEqArea, #reproject
                               CRS("+proj=longlat +datum=WGS84"))

springfieldSites84@data <- mutate(springfieldSites84@data, 
                               long=coordinates(springfieldSites84)[,1], # add long to @data slot
                               lat=coordinates(springfieldSites84)[,2]) # add lat to @data slot


write.table(springfieldSites84@data %>%
              select(panel, siteID, stratum, mdcaty, long, lat) %>%
              arrange(panel, stratum, mdcaty, siteID),
            file = paste(rootDir, "springfield/springfieldSites.txt", sep=""),
            row.names = FALSE, sep="\t")
