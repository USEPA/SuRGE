# THIS SCRIPT WILL BE USED TO PLOT AND CLEAN LGR DATA IN PREPARATION
# FOR CALCULATION OF DIFFUSIVE EMISSION RATE

#1. INSPECT INSTANCES OF NA IN GGA------------
# Time/date stamp first
filter(gga, is.na(RDateTime))
# no NAs for this field [1/5/2023] 
gga <- filter(gga, !is.na(RDateTime)) # strip out missing RDateTime which complicate functions below.


#2.  LOOP TO ASSIGN LAKE NAME, SITEID, AND DEPLY TIME TO LGR OBSERVATIONS.
# Many rows in fld_sheet have NA for chamb_deply_date_time.  For example, at all oversample
# sites where chambers were not deployed.  We want to remove these rows, or the missing
# values complicate the loop.

missing_chamb_deply_date_time <- is.na(fld_sheet$chamb_deply_date_time) # logical for missing chamber deployment times

# NEW 1/4/2023 IN WORK--not sure if this is correct result? 
# Replaces the for loop below.  
# Join with fld_sheet to get site_id and chamb_deply_date_time
# This join duplicates the time series for each station within
# each lake
gga <- gga %>%
  left_join(fld_sheet %>% 
                       filter(!missing_chamb_deply_date_time) %>%
                       select(lake_id, site_id, chamb_deply_date_time), 
                     by = "lake_id")

# # DEPRECATED
# # FOR LOOP REPLACED WITH CODE ABOVE
# for (i in 1:sum(!missing_chamb_deply_date_time)) {  # for each deployment date_time
#   # grab data from ith deployment
#   data.i <- fld_sheet %>% 
#     filter(!missing_chamb_deply_date_time) %>% # exclude rows with no deployment time, select ith observation
#     slice(i) %>% # grab ith row 
#     select(lake_id, site_id, chamb_deply_date_time) %>% # grab deployment date and time
#     as.data.frame() # tibble messes with base R comparisons and subsetting below
#   
#   # Create logical indicator to indicate time window corresponding to
#   # the ith lake_id and site_id.  Subtract 1 minute from deployment time.  Assume
#   # retrieval time is 5 minutes after deployment, add an extra minute to be conservative.
#   # The 1 minute padding to beginning and end of expected deployment will expand 
#   # x-axis range during plotting to ensure the time window capture the full time series.
#   logicalIndicator.i <- gga$RDateTime > (data.i$chamb_deply_date_time.i - 60) & # 1 min < field notes
#     gga$RDateTime < (data.i$chamb_deply_date_time.i + (6*60)) # 6 min > field notes.  Retr time not recorded, assume 5 min after deployment
# 
#   # use logical indicator to associate site_id, retrieval time, and deployment time with
#   # relevant gga time series.  Note that deployment and retrieval times are broken out by
#   # gas (CO2 vs CH4).  This allows different values for each gas if needed.
#   
#   # gga[logicalIndicator.i, "lake_id"] = Lake_Name.i # Set Lake_Name.  This is already coded into gga.
#   gga[logicalIndicator.i, "site_id"] = data.i$site_id # Set siteID 
#   gga[logicalIndicator.i, "co2chamb_deply_date_time"] = data.i$chamb_deply_date_time # Set chamber deployment time 
#   gga[logicalIndicator.i, "co2chamb_retr_date_time"] = data.i$chamb_deply_date_time + (60*5) # Set chamber deployment time 
#   gga[logicalIndicator.i, "ch4chamb_deply_date_time"] = data.i$chamb_deply_date_time # Set chamber deployment time 
#   gga[logicalIndicator.i, "ch4chamb_retr_date_time"] = data.i$chamb_deply_date_time + (60*5) # Set chamber deployment time   
# }
# 
# # POSIXct class was stripped during the loop.  Put it back in here.
# # Umm, I don't think this is necessary.  str(gga) indicates these field retained POSIXct class.
# # gga[ , c("co2DeplyDtTm", "co2RetDtTm", "ch4DeplyDtTm", "ch4RetDtTm")] <- 
# # lapply(gga[ , c("co2DeplyDtTm", "co2RetDtTm", "ch4DeplyDtTm", "ch4RetDtTm")], 
# #        as.POSIXct, origin = "1970-01-01 00:00:00", tz= "UTC")  # set tz!

#3. RECORD ADJUSTMENTS TO TIME SERIES PLOTS-------------
# this is based on manual inspection of each plot.  Must have at least one entry below
# for code to run
              # lake_id,       site_id,     co2DeplyDtTm,            co2RetDtTm,          ch4DeplyDtTm,        ch4RetDtTm
# This order is critical!
adjData <- {c("55", 5, "2022-06-01 09:01:15", "2022-06-01 09:06:30", "2022-06-01 09:01:15", "2022-06-01 09:06:30")
  }

# Coerce to matrix, then to data.frame  
adjDataDf <- matrix(adjData, ncol = 6, byrow = TRUE) %>%
  as.data.frame(stringsAsFactors = FALSE)

# Add column names
colnames(adjDataDf) <- c("lake_id", "site_id", "co2DeplyDtTm", 
                           "co2RetDtTm", "ch4DeplyDtTm", "ch4RetDtTm")

# Convert date/time data from character to POSIXct
adjDataDf <- adjDataDf %>%
  mutate(across(co2DeplyDtTm:ch4RetDtTm,
         ~ as.POSIXct(., format = "%Y-%m-%d %H:%M:%S", tz = "UTC")),
         site_id = as.numeric(site_id)) # match gga format


#4. UPDATE DEPLOYMENT AND RETRIEVAL TIMES BASED ON FIXES ABOVE (SEE POINT 3)----------------

# NEW 1/5/2023 IN WORK-- Replaces the for loop below
# adds co2DeplyDtTm, co2RetDtTm, ch4DeplyDtTm, ch4RetDtTm from adjDataDf to the gga data
gga_2 <- gga %>% #gga
  # Join with adjDataDf 
  left_join(adjDataDf) %>%
  # mutate ensures that all records have deployment and retrieval times for CO2 and CH4
  mutate(co2DeplyDtTm = case_when(is.na(co2DeplyDtTm) ~ chamb_deply_date_time, # if na, then use field sheet data
                            TRUE ~ co2DeplyDtTm), # if not na, then use data supplied from adjDataDf
         co2RetDtTm = case_when(is.na(co2RetDtTm) ~ chamb_deply_date_time + (60*5), # assume retrieval 5 minutes after deployment
                            TRUE ~ co2RetDtTm), # if not na, then use data supplied from adjDataDf
         ch4DeplyDtTm = case_when(is.na(ch4DeplyDtTm) ~ chamb_deply_date_time, # if na, then use field sheet data
                                  TRUE ~ ch4DeplyDtTm), # if not na, then use data supplied from adjDataDf
         ch4RetDtTm = case_when(is.na(ch4RetDtTm) ~ chamb_deply_date_time + (60*5), # assume retrieval 5 minutes after deployment
                                TRUE ~ ch4RetDtTm)) %>% # if not na, then use data supplied from adjDataDf
  group_by(lake_id, site_id) %>%
  filter(RDateTime > (min(c(co2DeplyDtTm, ch4DeplyDtTm)) - 60) & 
           RDateTime < (max(c(co2RetDtTm, ch4RetDtTm)) + 60)) %>%
  ungroup()

# #  DEPRECATED
# #  this loop adds the columns co2DeplyDtTm, co2RetDtTm, ch4DeplyDtTm, 
# # ch4RetDtTm, Lake_Name, siteID from adjDataDf to the object gga     
#   
#   for (i in 1:with(adjDataDf, length(unique(paste(siteID, Lake_Name))))) { # for each unique site x lake combination
#     lake.i <- adjDataDf$Lake_Name[i]  # extract ith lake
#     site.i <- adjDataDf$siteID[i]  # extract ith site
#     data.i <- adjDataDf[i, ]  # extract data.i
#     
#     #Calculate earliest and latest observation we need for this lake x site.  Simply min/max deply time.
#     #This will make sure x-axis range is good for time series plots.
#     #Use do.call the concentate "c" the df (which is actually a list) to a vector that
#     #can be fed to min, while preserving the POSIXct attribute.
#     #Need to set na.rm=TRUE in min function to ignore NA deploy/retr times for
#     #chambers with CH4 ebullition.
#     #http://stackoverflow.com/questions/15659783/why-does-unlist-kill-dates-in-r
#     #Unfortunately, this changes tz, which must be manually reset to UTC
#     start.time.i <- min(do.call("c", data.i[, c("co2DeplyDtTm", "co2RetDtTm", "ch4DeplyDtTm", "ch4RetDtTm")]), na.rm = TRUE)
#     end.time.i <- max(do.call("c", data.i[, c("co2DeplyDtTm", "co2RetDtTm", "ch4DeplyDtTm", "ch4RetDtTm")]), na.rm = TRUE)
#     attr(start.time.i, "tzone") <- "UTC"  # reset time zone!
#     attr(end.time.i, "tzone") <- "UTC"  # reset time zone!
#     
#     #Delete original CO2 and CH4 deployment / retrieval times from gga file.  These will be replaced with 
#     #updated values.
#     gga[gga$Lake_Name == lake.i &  !is.na(gga$Lake_Name) & gga$siteID == site.i & !is.na(gga$siteID), 
#         c("co2DeplyDtTm", "co2RetDtTm", "ch4DeplyDtTm", "ch4RetDtTm")] = NA
#     
#     #Logical indicator indicator block of gga data that should be updated
#     #The extra minute and begining and end extend x-axis range for plotting,
#     #which is good for picking time range for modeling diffusion.
#     logicalIndicator.i <- gga$RDateTime > (start.time.i - 60) & # 1 min < deployment
#       gga$RDateTime < (end.time.i + 60) # 1 min > retrieval
#     
#     # Replace original time stamps with updated numbers
#     # POSIXct and time zone preserved through this step.  Wow!
#     gga[logicalIndicator.i, c("Lake_Name", "siteID", "co2DeplyDtTm", "co2RetDtTm", "ch4DeplyDtTm", "ch4RetDtTm")] =
#       data.i[, c("Lake_Name", "siteID", "co2DeplyDtTm", "co2RetDtTm", "ch4DeplyDtTm", "ch4RetDtTm")]
# }



#5.  PLOT CO2 AND CH4 PROFILES FOR INSPECTION---------------

pdf("output/figures/ggaProfile.pdf", paper = "a4r") # landscape orientation
tic()
for (i in 1:with(gga_2[!is.na(gga_2$lake_id), ], # this eliminates observations without a Lake_Name (LGR data when chamber not deployed)
                 length(unique(paste(site_id, lake_id))))) {  # each combination of site and lake
  print(i)
  site.lake.i <- with(gga_2[!is.na(gga_2$lake_id), ],  # extract unique lake x site combination
                      unique(paste(site_id, lake_id)))[i]
  site.i <- gsub(" .*$", "", site.lake.i)  # extract site.  regex allows for siteIDs of different lengths (i.e. S-01, SU-01)
  lake.i <- substr(site.lake.i, start = nchar(site.i) + 2, stop = nchar(site.lake.i)) # extract lake name
  data.i <- filter(gga_2, lake_id == lake.i, site_id == site.i) %>%  # Pull out GGA data chunk
    select(-GasT_C) # No need to plot gas temperature
  RDate.i <- unique(data.i$RDate)  # for panel title

  plot.i <- ggplot(data.i,  aes(x = RDateTime, y = CH4._ppm)) + 
          geom_point() +
          geom_vline(data = data.i, aes(xintercept = as.numeric(ch4DeplyDtTm))) +
          geom_vline(data = data.i, aes(xintercept = as.numeric(ch4RetDtTm))) +
          scale_x_datetime(labels=date_format("%H:%M")) +
          ggtitle(paste(lake.i, site.i, RDate.i)) +
          theme(axis.text.x = element_text(size = 7),
                plot.title = element_text(size = 11))
  
  plot.ii <- ggplot(data.i,  aes(x = RDateTime, y = CO2._ppm)) + 
          geom_point() +
          geom_vline(data = data.i, aes(xintercept = as.numeric(co2DeplyDtTm))) +
          geom_vline(data = data.i, aes(xintercept = as.numeric(co2RetDtTm))) +
          scale_x_datetime(labels=date_format("%H:%M")) +
          ggtitle(paste(lake.i, site.i)) +
          theme(axis.text.x = element_text(size = 7))
  
  grid.arrange(plot.i, plot.ii, ncol = 2) # use to put two plots per page
}


dev.off() #15 min, 911 pages, 1/6/23

toc()








