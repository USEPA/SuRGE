# THIS CODE WILL BE USED TO READ THE POPULATED SHAPEFILES FROM THE
# L DRIVE.  MICHELLE PLATZ ENTERED DATA INTO THE SHAPEFILE.  SINCE
# WE REALLY DON'T NEED THE FULL SHAPEFILE FOR THE ANALYSIS, WE WILL
# JUST WORK WITH THE .dbf FILE.

# LIBRARY----------
# source("ohio2016/scriptsAndRmd/masterLibrary.R")

# READ IN .dbf FILES-------------------

# 1. Create a list of files to read in.  The completed data files all
# contain the pattern ...SitesEqAreaData.dbf and are stored in 
# L:/Priv/Cin/NRMRL/ReservoirEbullitionStudy/multiResSurvey2016/grtsDesign/
# directory.

rootDir <- "L:/Priv/Cin/NRMRL/ReservoirEbullitionStudy/multiResSurvey2016/grtsDesign/"

fileNames <- list.files(path = rootDir, 
                        pattern = "SitesEqAreaData.dbf", # file names containing this pattern
                        recursive = TRUE) # look in all subdirectories


# 2.  Read in files
mylist <- list()  # Create an empty list to hold data

for (i in 1:length(fileNames)){  # for each file
  # read.dbf is in foreign and spsurvey.  The foreign version allows for
  # use of as.is argument.
  data.i <- foreign::read.dbf(paste(rootDir, fileNames[i], sep = ""),
                    as.is=TRUE)
  mylist[[i]] <- data.i
}


# 3. Strip column names that are not consisent (or necessary) across different .dbf
# files.  Inconsistency related to source of original GIS shapefile.  Also because
# data for Acton 2017 came from Sarah's EC study.

# Vector of columns to remove
columnsToRemove <- c("OBJECTID|Permanent_|FDate|Resolution|GNIS_ID|Elevation|ReachCode|FType|FCode|Connectivi|Issue_Type|Lake_Name_|Reservoir_|QC|ECstudy")
                                  
# remove columns from all dfs in list
mylist1 <- lapply(mylist, function(x) select(x, -matches(columnsToRemove))) # matches allows for multiple terms

# Some dfs in list have column name "GNIS_Name".  Rename to Lake_Name where it appears. 
# ;x needed to have function report whole df.
mylist2 <- lapply(mylist1, function(x) {names(x) <- sub("GNIS_Name", "Lake_Name", names(x));x})

# Add 'section' as column, if not already present.  This happens in equal area designs
mylist3 <- lapply(mylist2, function(x){
  if("section" %in% names(x))  # if 'section' already exists
    x  # then report original df
  else 
    cbind(x, section = NA) # if 'section' doesn't exist, report new column of NAs
})
                                          
# 4.  Arrange columns in identical way to facilitate rbind
mylist4 <- lapply(mylist3, function(x) {
  select(x, noquote(order(colnames(x))))} # sort colnames alphabetically
  ) 

# 5.  Coerce list into dataframe via rbind
 eqAreaData <- do.call("rbind", mylist4)  # Coerces list into dataframe.

# FORMAT DATAFRAME-----------
 eqAreaData <- mutate(eqAreaData, 
                      chmDeplyDtTm = as.POSIXct(paste(trim(deplyDt), # trim removes white space
                                                      trim(chmStTm), sep=""),
                                                format = "%m/%d/%Y%H:%M",
                                                tz="UTC"), # set tz!
                      trapDeplyDtTm = as.POSIXct(paste(trim(deplyDt), # trim removes white space
                                                       trim(deplyTm), sep=""),
                                                 format = "%m/%d/%Y%H:%M",
                                                 tz="UTC"),
                      trapRtrvDtTm = as.POSIXct(paste(trim(RtrvDat), # trim removes white space
                                                       trim(RtrvTim), sep=""),
                                                 format = "%m/%d/%Y%H:%M",
                                                 tz="UTC"),
                      deplyDt = as.Date(deplyDt, format = "%m/%d/%Y"))  

 # Columns that should be converted to numeric
 cols <- c("chm_vol", "wtrDpth", "smDpthS", "Tmp_C_S", "DOPrc_S", "DO__L_S",   
           "SpCn__S", "pH_S", "ORP_S", "TrNTU_S", "chla_S", "smDpthD", "Tmp_C_D", "DOPrc_D", "DO__L_D",   
           "SpCn__D", "pH_D", "ORP_D", "TrNTU_D", "chla_D", "BrPrssr", "TtTrpVl", "LatSamp", "LongSmp")
 
 eqAreaData[, cols] <- lapply(eqAreaData[, cols], as.numeric) # convert to numeric
 
 # NA in character fields (i.e. TrapExtn) shapefile are being read as character values.
 # Convert to NA.
 eqAreaData[, "TrapExtn"] <- ifelse(eqAreaData[, "TrapExtn"] == "NA", 
                                    NA, 
                                    eqAreaData[, "TrapExtn"])
 
 eqAreaData[, "ArExtnrs"] <- ifelse(eqAreaData[, "ArExtnrs"] == "NA", 
                                    NA, 
                                    eqAreaData[, "ArExtnrs"])
 
 eqAreaData[, "DG_Extn"] <- ifelse(eqAreaData[, "DG_Extn"] == "NA", 
                                    NA, 
                                    eqAreaData[, "DG_Extn"])
 
 # BAROMETRIC PRESSURE----------------
 # Assign barometric pressure to dissolved gas sampling site where BP
 # was not recorded.  All lakes have at least one measurement, so assign
 # recorded value to missing sites.
 eqAreaData <- group_by(eqAreaData, Lake_Name) %>% 
   mutate(BrPrssr = 
            # Select observation where dissolved gas was collected (i.e. anywhere a
            # deep sonde measurement was made), but BP wasn't recorded
            ifelse(!is.na(smDpthD) & is.na(BrPrssr),
                   # Set BP equal to any other BP measured at the lake
                   subset(BrPrssr, !is.na(BrPrssr)),
                   BrPrssr)) # else return BP

 
 # HEADSPACE GAS AND WATER VOLUMES----------------
 # Water and gas volumes were not always recorded.  When they were,
 # they weren't associated with a single sample. Assign mean values by lake.  If
 # no data reported for lake, assume he=20ml and water =120ml. Data is recorded
 # as character values.
 
 # Function for executing above
volEst <- function(x, choice1) {
  if (choice1 == "He") {
    # Calculate mean He volume.  deal w/character values
    vol <- strsplit(x, split = ",") %>% unlist() %>% as.numeric() %>% mean(na.rm = TRUE)
    vol <- ifelse(is.nan(vol), 20, vol) # if not reported, assume 20mL
  }
  if (choice1 == "water") {
    # Calculate mean water volume.  deal w/character values
    vol <- strsplit(x, split = ",") %>% unlist() %>% as.numeric() %>% mean(na.rm = TRUE)
    vol <- ifelse(is.nan(vol) | vol >= 140, # if not reported, or erroneous (cant be 140)
                  120, vol) # assume 120mL
  }
  vol # return volume estimate
}
 
 eqAreaData <- mutate(eqAreaData,
                      HeVol = 
                        # Select observation where dissolved gas was collected
                        ifelse(!is.na(DG_Extn),
                               # Set He volume equal to mean for lake
                               volEst(HeVol, "He"),
                               HeVol), # else return He
                      H2O_vol = 
                        # Select observation where dissolved gas was collected
                        ifelse(!is.na(DG_Extn),
                               # Set Water volume equal to mean for lake
                               volEst(H2O_vol, "water"),
                               H2O_vol)) %>% # else return H2O_vol
   ungroup() %>% # remove grouping
   as.data.frame() %>% # remove tbl_df class
   mutate(HeVol = as.numeric(HeVol),
          H2O_vol = as.numeric(H2O_vol))
 
 # CHAMBER VOLUME
 # Calculate chamber volume based on relationship between water level
 # and volume.  See chamberDesign.xlsx in East Fork folder.
 eqAreaData <- mutate(eqAreaData, chmVol.L = (42.057 + (-0.2189 * chm_vol)))
 
 # Deal with instances where chamber volume was not recorded in field.
 # 1.  A site or two missed, whereas volume recorded at most other sites.
 # Caeaser Cr.
 toAdjChmVol <- with(eqAreaData, Lake_Name == "Caesar Creek Lake" & EvalStatus == "sampled" & is.na(chmVol.L))
 adjChmVol <- with(eqAreaData, Lake_Name == "Caesar Creek Lake" & EvalStatus == "sampled" & !is.na(chmVol.L))
 estChemVol <- mean(eqAreaData[adjChmVol, "chmVol.L"])
 eqAreaData[toAdjChmVol, "chmVol.L"] =  estChemVol
 
 # Apple Valley
 toAdjChmVol <- with(eqAreaData, Lake_Name == "Apple Valley Lake" & EvalStatus == "sampled" & is.na(chmVol.L))
 adjChmVol <- with(eqAreaData, Lake_Name == "Apple Valley Lake" & EvalStatus == "sampled" & !is.na(chmVol.L))
 estChemVol <- mean(eqAreaData[adjChmVol, "chmVol.L"])
 eqAreaData[toAdjChmVol, "chmVol.L"] =  estChemVol
 
 # Lake Waynoka
 toAdjChmVol <- with(eqAreaData, Lake_Name == "Lake Waynoka" & EvalStatus == "sampled" & is.na(chmVol.L))
 adjChmVol <- with(eqAreaData, Lake_Name == "Lake Waynoka" & EvalStatus == "sampled" & !is.na(chmVol.L))
 estChemVol <- mean(eqAreaData[adjChmVol, "chmVol.L"])
 eqAreaData[toAdjChmVol, "chmVol.L"] =  estChemVol 
 
 # 2.  Pleasent Hill (PH) sampled day before Charles Mill (CM).  No volume measurements
 # made at CM; replace with mean from PH.
 # First, create logical for conditions that need to be replaced
 adjChmVol <- eqAreaData$Lake_Name == "Charles Mill Lake" & eqAreaData$EvalStatus == "sampled"
 eqAreaData[adjChmVol, "chmVol.L"] = 
   mean(eqAreaData[eqAreaData$Lake_Name == "Pleasant Hill Lake", "chmVol.L"], na.rm = TRUE)

 