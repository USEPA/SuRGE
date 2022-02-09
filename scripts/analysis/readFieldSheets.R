# READ FIELD SHEETS


# 1. Create a list of file paths where the data are stored.  
labs <- c("ADA", "CIN", "DOE", "NAR", "R10", "RTP", "USGS")
paths <- paste0(userPath,  "data/", labs)

# code below creates a list of paths for each file.  This can then be fed into
# a loop for reading each file.  This has been deprecated in favor of fs::dir_ls
# and purr map (see below).  
# Create a list of files to read in.  The completed data files all
# contain the pattern ...surgeDataXXX.xlsx.  This gets everything except 2018 
# R10 data.  Those data files are '...EqAreaData.xlsx' and need to be inspected 
# for consistency with SuRGE data files.

# labs <- c("ADA", "CIN", "DOE", "NAR", "R10", "RTP", "USGS")
# paths <- paste0(userPath,  "data/", labs)

#  for (i in 1:length(labs)) {
#  fileNames.i <- list.files(path = paste0(userPath,  "data/", labs[i]),
#                          pattern = "surgeData", # file names containing this pattern
#                          recursive = TRUE) %>% # look in all subdirectories
#    .[!grepl(c(".pdf|.docx"), .)] # remove pdf and .docx review files
# 
#  if(length(fileNames.i) > 0) { # if any file names were read...
#    fileNames.i <- paste0(labs[i], "/", fileNames.i) # add lab directory to file path
#  }
# 
#  fileNames <- c(fileNames, fileNames.i) # append new names to existing vector
#  }

# 2.  Read in files
# # deprecated in favor of function below
#  mylist.data <- list()  # Create an empty list to hold data
# 
#  for (i in 1:length(fileNames)){  # for each file
#    data.i <- readxl::read_excel(paste0(userPath, "data/", fileNames[i]), skip = 1, sheet = "data") %>%
#      janitor::clean_names()
#    mylist.data[[i]] <- data.i
#  }


# 2. Function for reading 'data' tab of surgeData files.

get_data_sheet <- function(paths){
  #d <-  
  fs::dir_ls(path = paths, # see above
             regexp = 'surgeData', # file names containing this pattern
             recurse = TRUE) %>% # look in all subdirectories
    .[!grepl(c(".pdf|.docx"), .)] %>% # remove pdf and .docx review files
   # .[12] %>%
    # map will read each file in fs_path list generated above
    purrr::map(~read_excel(., skip = 1, sheet = "data", 
                           na = c("NA", "", "N/A", "n/a"))) %>%
    # format data
    map(., function(x){
      janitor::clean_names(x) %>%
        # format lake_id and site_id.  See Wiki
        mutate(lake_id = as.character(lake_id) %>%
                 tolower(.) %>% # i.e. Lacustrine -> lacustrine
                 str_remove(., "ch4_") %>% # remove any ch4_ from lake_id
                 str_remove(., "^0+"), #remove leading zeroes i.e. 078->78
               site_id = as.numeric(gsub(".*?([0-9]+).*", "\\1", site_id))) %>%
        # Format date and time objects
        mutate(across(contains("date"), ~ as.Date(.x, format = "%m.%d.%Y")), # convert date to as.Date
               across(contains("time"), ~ format(.x, format = "%H:%M:%S")), # convert time to character
               trap_deply_date_time = as.POSIXct(x = paste0(trap_deply_date, trap_deply_time),
                                                 format = "%Y-%m-%d%H:%M:%S",
                                                 tz = "UTC"),
               chamb_deply_date_time = as.POSIXct(x = paste0(chamb_deply_date, chamb_deply_time),
                                                  format = "%Y-%m-%d%H:%M:%S",
                                                  tz = "UTC"))
    }) %>%
    map_dfr(., identity) # rbinds into one df
}

# 3. Read 'data' tab of surgeData files.
fld_sheet <- get_data_sheet(paths = paths)


# 4. Function to read 'dissolved.gas' tab of surgeData file.
get_dg_sheet <- function(paths){
  #d <-  
  fs::dir_ls(path = paths, # see above
             regexp = 'surgeData', # file names containing this pattern
             recurse = TRUE) %>% # look in all subdirectories
    .[!grepl(c(".pdf|.docx"), .)] %>% # remove pdf and .docx review files
    
    # map will read each file in fs_path list generated above
    purrr::map(~read_excel(., skip = 1, sheet = "dissolved.gas", 
                           na = c("NA", "", "N/A", "n/a"))) %>%
    # format data
    map(., function(x){
      janitor::clean_names(x) %>%
        # format lake_id and site_id.  See Wiki
        mutate(lake_id = as.character(lake_id) %>%
                 tolower(.) %>% # i.e. Lacustrine -> lacustrine
                 str_remove(., "ch4_") %>% # remove any ch4_ from lake_id
                 str_remove(., "^0+"), #remove leading zeroes i.e. 078->78
               site_id = as.numeric(gsub(".*?([0-9]+).*", "\\1", site_id)))
    }) %>%
    map_dfr(., identity) # rbinds into one df
}

# 5.  Read dissolved gas sheet
dg_sheet <- get_dg_sheet(paths = paths)

dg_sheet %>% print(n=Inf)

dg_sheet %>% filter(is.na(atm_pressure) | is.na(air_temperature) | is.na(water_vol) | is.na(air_vol)) %>%
  distinct(lake_id) %>% print(n=Inf)




# right now have immediate need to get exetainer numbers read in.  Lets just focus on data needed now.

trap.air.extList <- map(mylist.data, function(x) select(x, lake.id, site.id, contains("extn")) %>%
                 select(!contains("notes"))) %>%
  #map(., function(x) any(x == 1389)) # works
  #map(., function(x) any(x %in% c("SG210639", "SG210650"))) # doesn't work
  map(., function(x) pivot_longer(x, !c(lake.id, site.id), names_to = "extn")) %>% # pivot to longer
  map(., function(x) mutate(x, extn = case_when(grepl("trap", extn) ~ "trap",
                                                grepl("air", extn) ~ "air",
                                                TRUE ~ extn)) %>%
        filter(!is.na(value))) %>%
  do.call("rbind", .)

dg.extList <- map(mylist.dg, function(x) select(x, lake.id, site.id, dg.extn)) %>%
  map(., function(x) mutate(x, extn = "dg") %>%
        rename(value = dg.extn) %>%
        filter(!is.na(value))) %>%
  do.call("rbind", .)

extn.list <- rbind(trap.air.extList, dg.extList)

extn.list %>% janitor::get_dupes() # no dups
extn.list %>% filter(is.na(value)) # no missing values
extn.list %>% filter(nchar(value) != 8) # no strangely formatted values.

# code to help identify random samples that showed up on my desk
unknown.extn <- read_excel(paste0(userPath, "data/gases/unknown_exetainers_from_JakeBs_desk_20220106.xlsx"), sheet = "data")

extn.list %>% filter(value %in% unknown.extn$value) # o yes, some of these are samples
inner_join(extn.list, unknown.extn)

#################################################################
######CODE BELOW NOT YET UPDATED FOR SURGE#######################
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

 