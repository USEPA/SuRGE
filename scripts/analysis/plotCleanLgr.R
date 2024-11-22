# THIS SCRIPT WILL BE USED TO PLOT AND CLEAN LGR DATA IN PREPARATION
# FOR CALCULATION OF DIFFUSIVE EMISSION RATE

#####################################################
## TO RUN THE CODE BELOW, YOU MUST FIRST GENERATE THE gga AND fld_sheet DATA 
## OBJECTS.  THIS CAN BE DONE BY RUNNING ALL SCRIPTS IN THE ORDER
## DEFINED IN masterScript.R, OR YOU CAN JUST RUN THESE 5 LINES:
# source("scripts/masterLibrary.R") # Read in renv controlled library
# source("scripts/setUserPath.R") # needed to allow consistent fixed file paths
# source("scripts/analysis/readSurgeLakes.R")
# source("scripts/analysis/readFieldSheets.R") # read surgeData...xlsx.  fld_sheet, dg_sheet
# source("scripts/analysis/readLgr.R") # read raw LGR data





#1. INSPECT INSTANCES OF NA IN GGA------------
# Time/date stamp first
filter(gga, is.na(RDateTime))
# no NAs for this field [2/1/2024] 
gga <- filter(gga, !is.na(RDateTime)) # strip out missing RDateTime which complicate functions below.


#2.  ASSIGN site_id, AND chamb_deply_date_time TO LGR OBSERVATIONS.------------------------
# Many rows in fld_sheet have NA for chamb_deply_date_time.  For example, at all oversample
# sites where chambers were not deployed.  We want to remove these rows, or the missing
# values complicate the loop.


missing_chamb_deply_date_time <- is.na(fld_sheet$chamb_deply_date_time) # logical for missing chamber deployment times

# Join with fld_sheet to get site_id and chamb_deply_date_time
# This join duplicates the time series for each station within
# each lake
gga$visit<-as.numeric(gga$visit)
gga_2 <- gga %>%
  left_join(fld_sheet %>% 
                       filter(!missing_chamb_deply_date_time) %>%
                       select(lake_id, site_id, visit, chamb_deply_date_time), 
                     by = c("lake_id","visit"), relationship = "many-to-many")

#3. ADD CO2 AND CH4 RETRIEVAL AND DEPLOYMENT TIMES
# We may want to model different portions of the time series for CO2 and CH4.
# Here we create fields to hold retrieval and deployment times for each gas.
gga_2 <- gga_2 %>% 
  # mutate ensures that all records have deployment and retrieval times for CO2 and CH4
  # assume deployment time recorded in field is correct, will inspect/modify below.
  mutate(co2DeplyDtTm = chamb_deply_date_time , 
         co2RetDtTm =  chamb_deply_date_time + (60*5), # assume retrieval 5 minutes after deployment
         ch4DeplyDtTm = chamb_deply_date_time,
         ch4RetDtTm = chamb_deply_date_time + (60*5))


#3. RECORD ADJUSTMENTS TO TIME SERIES PLOTS-------------
# COMPLETE 3.1, 3.2, AND 3.3.  REPEAT UNTIL ALL PLOTS HAVE BEEN REVIEWED.
#3.1  Manually inspect each plot and record best deployment and retrieval times
# in lab specific Excel file.  

# specify which lake and site to inspect
lake_id.i <- "69_transitional"  # numeric component of lake_id without leading zero(s), formatted as character
site_id.i <- "10" # numeric component of lake_id, no leading zero(s), formatted as numeric
visit_id.i <- "1"
# this code generates a 3 panel plot used to demonstrate relationship between
# CH4, CO2, and H2O times to stabilization.  This can be deleted after the issue
# has been resolved. [JB 3/29/2024]

# gga_2 %>%
#   filter(lake_id == lake_id.i,
#          site_id == site_id.i,
#          RDateTime > ch4DeplyDtTm - 60, # start plot 1 minute prior to deployment
#          RDateTime < ch4RetDtTm + 300, # extend plot 1 minute post deployment
#          CH4._ppm > 0) %>%
#   select(lake_id, RDateTime, CH4._ppm, CO2._ppm, H2O._ppm,
#          co2DeplyDtTm, co2RetDtTm, ch4DeplyDtTm, ch4RetDtTm) %>%
#   pivot_longer(!c(lake_id, RDateTime,co2DeplyDtTm, co2RetDtTm,
#                   ch4DeplyDtTm, ch4RetDtTm)) %>%
#   ggplot(aes(RDateTime, value)) +
#   geom_point() +
#   geom_vline(aes(xintercept = as.numeric(as.POSIXct("2021-06-28 17:16:22", tz = "UTC")),
#                  color = "deployment"), key_glyph = "path") +
#   geom_vline(aes(xintercept = as.numeric(as.POSIXct("2021-06-28 17:17:03", tz = "UTC")),
#                  color = "CH4 stabilizes"), key_glyph = "path") + # CH4
#   geom_vline(aes(xintercept = as.numeric(as.POSIXct("2021-06-28 17:17:43", tz = "UTC")),
#                  color = "CO2 stabilizes"), key_glyph = "path") + #CO2
#   geom_vline(aes(xintercept = as.numeric(as.POSIXct("2021-06-28 17:21:22", tz = "UTC")),
#                  color = "retrieval"), key_glyph = "path") +
#   scale_color_discrete(breaks = c("deployment", "CH4 stabilizes", "CO2 stabilizes", "retrieval"), name="") +
#   #scale_x_datetime(date_labels = ("%m/%d %H:%M")) +
#   xlab("time (hh:mm)") +
#   facet_wrap(~name, scales = "free", nrow = 3)

plotCh4 <- gga_2 %>% 
  filter(lake_id == lake_id.i, 
         site_id == site_id.i, 
         visit == visit_id.i,
         RDateTime > ch4DeplyDtTm - 60, # start plot 1 minute prior to deployment
         RDateTime < ch4RetDtTm + 60, # extend plot 1 minute post deployment
         CH4._ppm > 0) %>%
  ggplot(aes(RDateTime, CH4._ppm)) + 
  geom_point() +
  geom_vline(aes(xintercept = as.numeric(ch4DeplyDtTm))) +
  geom_vline(aes(xintercept = as.numeric(ch4RetDtTm))) +
  scale_x_datetime(date_labels = ("%m/%d %H:%M")) +
  ggtitle(paste("lake_id =", lake_id.i, "site_id = ", site_id.i))
ggplotly(plotCh4)  

plotCo2 <- gga_2 %>% 
  filter(lake_id == lake_id.i, 
         site_id == site_id.i, 
         RDateTime > co2DeplyDtTm - 60, # start plot 1 minute prior to deployment
         RDateTime < co2RetDtTm + 60, # extend plot 1 minute post deployment
         CO2._ppm > 0) %>%
  ggplot(aes(RDateTime, CO2._ppm)) + 
  geom_point() +
  geom_vline(aes(xintercept = as.numeric(co2DeplyDtTm))) +
  geom_vline(aes(xintercept = as.numeric(co2RetDtTm))) +
  scale_x_datetime(date_labels = ("%m/%d %H:%M")) +
  ggtitle(paste("lake_id =", lake_id.i, "site_id = ", site_id.i))
ggplotly(plotCo2)

plotH2O <- gga_2 %>% 
  filter(lake_id == lake_id.i, 
         site_id == site_id.i, 
         RDateTime > co2DeplyDtTm - 120, # start plot 1 minute prior to deployment
         RDateTime < co2RetDtTm + 60, # extend plot 1 minute post deployment
         CO2._ppm > 0) %>%
  ggplot(aes(RDateTime, H2O._ppm)) + 
  geom_point() +
  geom_vline(aes(xintercept = as.numeric(co2DeplyDtTm))) +
  geom_vline(aes(xintercept = as.numeric(co2RetDtTm))) +
  scale_x_datetime(date_labels = ("%m/%d %H:%M")) +
  ggtitle(paste("lake_id =", lake_id.i, "site_id = ", site_id.i))
ggplotly(plotH2O)

#3.2  Read in refined deployment and retrieval data from Excel files.
# use .xls.  Can read file into R while file is open in Excel, which is convenient.

# list of files containing deployment and retrieval data.
adjDataList <- paste0(userPath,
                     c( "data/ADA/chamberAdjustmentsAda.xls", 
                       "data/USGS/chamberAdjustmentsUSGS.xls", "data/DOE/chamberAdjustmentsDOE.xls"),sep="")

adjDataListb<-paste0(userPath, 
                     c("data/CIN/chamberAdjustmentsCIN.xls","data/NAR/chamberAdjustmentsNAR.xls", "data/RTP/chamberAdjustmentsRTP.xls",
                       "data/R10/chamberAdjustmentsR10.xls"), sep="")
# Read data, but not CIN
adjData <- map_df(adjDataList, # exclude CIN, RTP, R10, and NAR, different formatting
                  readxl::read_xls, 
                  range =cell_cols("DATA!A:L"), # columns A:L
                  col_types = c("text", "numeric","numeric","text",
                                rep("date", 4), 
                                rep("text", 4))) %>% #lake_id is character
  janitor::remove_empty("rows") # remove rows that contain only NA

# Read CIN and NAR data.  date and time fields contain tenths of a second that confuse read_xls
adjDataB <- map_df(adjDataListb, # only CIN, different formatting than others
                  readxl::read_xls, 
                  range =cell_cols("DATA!A:L"), # columns A:L
                  col_types = c("text", "numeric","numeric","text", 
                                rep("text", 8))) %>% # date.time fields must be read as character
  janitor::remove_empty("rows") %>% # remove rows that contain only NA
  mutate(across(contains("DtTm"), ~as.POSIXct(., "%m/%d/%Y %H:%M:%S", tz="UTC")))

#Fix adjustment times for CIN sites with clock issue

tem<-ifelse(adjDataB$lake_id=="67",adjDataB$co2DeplyDtTm+dseconds(CIN_adjustments$Time.Offset[1]),
                          ifelse(adjDataB$lake_id=="68",adjDataB$co2DeplyDtTm+dseconds(CIN_adjustments$Time.Offset[2]),
                                 ifelse(adjDataB$lake_id=="69_riverine",adjDataB$co2DeplyDtTm+dseconds(CIN_adjustments$Time.Offset[3]),
                                        ifelse(adjDataB$lake_id=="70_transitional",adjDataB$co2DeplyDtTm+dseconds(CIN_adjustments$Time.Offset[4]),
                                               ifelse(adjDataB$lake_id=="71",adjDataB$co2DeplyDtTm+dseconds(CIN_adjustments$Time.Offset[5]),
                                                      ifelse(adjDataB$lake_id=="72",adjDataB$co2DeplyDtTm+dseconds(CIN_adjustments$Time.Offset[6]),
                                                             ifelse(adjDataB$lake_id=="75",adjDataB$co2DeplyDtTm+dseconds(CIN_adjustments$Time.Offset[7]),
                                                                    ifelse(adjDataB$lake_id=="79",adjDataB$co2DeplyDtTm+dseconds(CIN_adjustments$Time.Offset[8]),
                                                                           ifelse(adjDataB$lake_id=="149",adjDataB$co2DeplyDtTm+dseconds(CIN_adjustments$Time.Offset[9]),
                                                                                  ifelse(adjDataB$lake_id=="231",adjDataB$co2DeplyDtTm+dseconds(CIN_adjustments$Time.Offset[10]),
                                                                                         ifelse(adjDataB$lake_id=="232"& adjDataB$site_id %in% c("10","14","32","26","4","8","12","20","7","15"),adjDataB$co2DeplyDtTm+dseconds(CIN_adjustments$Time.Offset[11]),
                                                                                                ifelse(adjDataB$lake_id=="232" & adjDataB$site_id %in% c("2","5","13","1","9"),adjDataB$co2DeplyDtTm+dseconds(CIN_adjustments$Time.Offset[14]),
                                                                                                ifelse(adjDataB$lake_id=="236",adjDataB$co2DeplyDtTm+dseconds(CIN_adjustments$Time.Offset[12]),
                                                                                                       ifelse(adjDataB$lake_id=="237",adjDataB$co2DeplyDtTm+dseconds(CIN_adjustments$Time.Offset[13]),
                                                                                                              ifelse(adjDataB$lake_id=="75",adjDataB$co2DeplyDtTm+dseconds(CIN_adjustments$Time.Offset[15]),
                                                                                                               adjDataB$co2DeplyDtTm)))))))))))))))
adjDataB$co2DeplyDtTm<-as_datetime(tem)

ten<-ifelse(adjDataB$lake_id=="67",adjDataB$co2RetDtTm+dseconds(CIN_adjustments$Time.Offset[1]),
            ifelse(adjDataB$lake_id=="68",adjDataB$co2RetDtTm+dseconds(CIN_adjustments$Time.Offset[2]),
                   ifelse(adjDataB$lake_id=="69_riverine",adjDataB$co2RetDtTm+dseconds(CIN_adjustments$Time.Offset[3]),
                          ifelse(adjDataB$lake_id=="70_transitional",adjDataB$co2RetDtTm+dseconds(CIN_adjustments$Time.Offset[4]),
                                 ifelse(adjDataB$lake_id=="71",adjDataB$co2RetDtTm+dseconds(CIN_adjustments$Time.Offset[5]),
                                        ifelse(adjDataB$lake_id=="72",adjDataB$co2RetDtTm+dseconds(CIN_adjustments$Time.Offset[6]),
                                               ifelse(adjDataB$lake_id=="75",adjDataB$co2RetDtTm+dseconds(CIN_adjustments$Time.Offset[7]),
                                                      ifelse(adjDataB$lake_id=="79",adjDataB$co2RetDtTm+dseconds(CIN_adjustments$Time.Offset[8]),
                                                             ifelse(adjDataB$lake_id=="149",adjDataB$co2RetDtTm+dseconds(CIN_adjustments$Time.Offset[9]),
                                                                    ifelse(adjDataB$lake_id=="231",adjDataB$co2RetDtTm+dseconds(CIN_adjustments$Time.Offset[10]),
                                                                           ifelse(adjDataB$lake_id=="232"& adjDataB$site_id %in% c("10","14","32","26","4","8","12","20","7","15"),adjDataB$co2RetDtTm+dseconds(CIN_adjustments$Time.Offset[11]),
                                                                                  ifelse(adjDataB$lake_id=="232" & adjDataB$site_id %in% c("2","5","13","1","9"),adjDataB$co2RetDtTm+dseconds(CIN_adjustments$Time.Offset[14]),
                                                                                  ifelse(adjDataB$lake_id=="236",adjDataB$co2RetDtTm+dseconds(CIN_adjustments$Time.Offset[12]),
                                                                                         ifelse(adjDataB$lake_id=="237",adjDataB$co2RetDtTm+dseconds(CIN_adjustments$Time.Offset[13]),
                                                                                                ifelse(adjDataB$lake_id=="75",adjDataB$co2RetDtTm+dseconds(CIN_adjustments$Time.Offset[15]),
                                                                                                 adjDataB$co2RetDtTm)))))))))))))))
adjDataB$co2RetDtTm<-as_datetime(ten)

teo<-ifelse(adjDataB$lake_id=="67",adjDataB$ch4RetDtTm+dseconds(CIN_adjustments$Time.Offset[1]),
            ifelse(adjDataB$lake_id=="68",adjDataB$ch4RetDtTm+dseconds(CIN_adjustments$Time.Offset[2]),
                   ifelse(adjDataB$lake_id=="69_riverine",adjDataB$ch4RetDtTm+dseconds(CIN_adjustments$Time.Offset[3]),
                          ifelse(adjDataB$lake_id=="70_transitional",adjDataB$ch4RetDtTm+dseconds(CIN_adjustments$Time.Offset[4]),
                                 ifelse(adjDataB$lake_id=="71",adjDataB$ch4RetDtTm+dseconds(CIN_adjustments$Time.Offset[5]),
                                        ifelse(adjDataB$lake_id=="72",adjDataB$ch4RetDtTm+dseconds(CIN_adjustments$Time.Offset[6]),
                                               ifelse(adjDataB$lake_id=="75",adjDataB$ch4RetDtTm+dseconds(CIN_adjustments$Time.Offset[7]),
                                                      ifelse(adjDataB$lake_id=="79",adjDataB$ch4RetDtTm+dseconds(CIN_adjustments$Time.Offset[8]),
                                                             ifelse(adjDataB$lake_id=="149",adjDataB$ch4RetDtTm+dseconds(CIN_adjustments$Time.Offset[9]),
                                                                    ifelse(adjDataB$lake_id=="231",adjDataB$ch4RetDtTm+dseconds(CIN_adjustments$Time.Offset[10]),
                                                                           ifelse(adjDataB$lake_id=="232"& adjDataB$site_id %in% c("10","14","32","26","4","8","12","20","7","15"),adjDataB$ch4RetDtTm+dseconds(CIN_adjustments$Time.Offset[11]),
                                                                                  ifelse(adjDataB$lake_id=="232" & adjDataB$site_id %in% c("2","5","13","1","9"),adjDataB$ch4RetDtTm+dseconds(CIN_adjustments$Time.Offset[14]),
                                                                                  ifelse(adjDataB$lake_id=="236",adjDataB$ch4RetDtTm+dseconds(CIN_adjustments$Time.Offset[12]),
                                                                                         ifelse(adjDataB$lake_id=="237",adjDataB$ch4RetDtTm+dseconds(CIN_adjustments$Time.Offset[13]),
                                                                                                ifelse(adjDataB$lake_id=="75",adjDataB$ch4RetDtTm+dseconds(CIN_adjustments$Time.Offset[15]),
                                                                                                adjDataB$ch4RetDtTm)))))))))))))))
adjDataB$ch4RetDtTm<-as_datetime(teo)

tep<-ifelse(adjDataB$lake_id=="67",adjDataB$ch4DeplyDtTm+dseconds(CIN_adjustments$Time.Offset[1]),
            ifelse(adjDataB$lake_id=="68",adjDataB$ch4DeplyDtTm+dseconds(CIN_adjustments$Time.Offset[2]),
                   ifelse(adjDataB$lake_id=="69_riverine",adjDataB$ch4DeplyDtTm+dseconds(CIN_adjustments$Time.Offset[3]),
                          ifelse(adjDataB$lake_id=="70_transitional",adjDataB$ch4DeplyDtTm+dseconds(CIN_adjustments$Time.Offset[4]),
                                 ifelse(adjDataB$lake_id=="71",adjDataB$ch4DeplyDtTm+dseconds(CIN_adjustments$Time.Offset[5]),
                                        ifelse(adjDataB$lake_id=="72",adjDataB$ch4DeplyDtTm+dseconds(CIN_adjustments$Time.Offset[6]),
                                               ifelse(adjDataB$lake_id=="75",adjDataB$ch4DeplyDtTm+dseconds(CIN_adjustments$Time.Offset[7]),
                                                      ifelse(adjDataB$lake_id=="79",adjDataB$ch4DeplyDtTm+dseconds(CIN_adjustments$Time.Offset[8]),
                                                             ifelse(adjDataB$lake_id=="149",adjDataB$ch4DeplyDtTm+dseconds(CIN_adjustments$Time.Offset[9]),
                                                                    ifelse(adjDataB$lake_id=="231",adjDataB$ch4DeplyDtTm+dseconds(CIN_adjustments$Time.Offset[10]),
                                                                           ifelse(adjDataB$lake_id=="232"& adjDataB$site_id %in% c("10","14","32","26","4","8","12","20","7","15"),adjDataB$ch4DeplyDtTm+dseconds(CIN_adjustments$Time.Offset[11]),
                                                                                  ifelse(adjDataB$lake_id=="232" & adjDataB$site_id %in% c("2","5","13","1","9"),adjDataB$ch4DeplyDtTm+dseconds(CIN_adjustments$Time.Offset[14]),
                                                                                  ifelse(adjDataB$lake_id=="236",adjDataB$ch4DeplyDtTm+dseconds(CIN_adjustments$Time.Offset[12]),
                                                                                         ifelse(adjDataB$lake_id=="237",adjDataB$ch4DeplyDtTm+dseconds(CIN_adjustments$Time.Offset[13]),
                                                                                                ifelse(adjDataB$lake_id=="75",adjDataB$ch4DeplyDtTm+dseconds(CIN_adjustments$Time.Offset[15]),
                                                                                                adjDataB$ch4DeplyDtTm)))))))))))))))
adjDataB$ch4DeplyDtTm<-as_datetime(tep)

# Combine CIN and other data
adjData <- rbind(adjData, adjDataB)

str(adjData)

#3.3. update deployment and retrieval times based on fixes above (see 3.1 and 3.2)
gga_2 <- gga_2 %>% 
  # remove co2DeplyDtTm, co2RetDtTm, ch4DeplyDtTm, and ch4RetDtTm.  They will be replaced with
  # data from adjData or derived from chamb_deply_date_time
  select(-contains("DtTm")) %>%
  # Remove these columns if present.  Won't be present first time through, but
  # will in subsequent iterations.  Will be replaced with data from adjData.
  # This won't throw error if specified columns are absent.
  select_if(!names(.) %in% c("co2Notes", "ch4Notes", "co2Status", "ch4Status")) %>%
  # Join with adjDataDf.
  left_join(., adjData, relationship = "many-to-many")
  # mutate ensures that all records have deployment and retrieval times for CO2 and CH4
  # now that we have gone through all the chamber adjustments, I am not going to use the chamb_deply_date_time anymore
  # mutate(co2DeplyDtTm = case_when(is.na(co2DeplyDtTm) ~ chamb_deply_date_time, # if na, then use field sheet data
  #                           TRUE ~ co2DeplyDtTm), # if not na, then use data supplied from adjDataDf
  #        co2RetDtTm = case_when(is.na(co2RetDtTm) ~ chamb_deply_date_time + (60*5), # assume retrieval 5 minutes after deployment
  #                           TRUE ~ co2RetDtTm), # if not na, then use data supplied from adjDataDf
  #        ch4DeplyDtTm = case_when(is.na(ch4DeplyDtTm) ~ chamb_deply_date_time, # if na, then use field sheet data
  #                                 TRUE ~ ch4DeplyDtTm), # if not na, then use data supplied from adjDataDf
  #        ch4RetDtTm = case_when(is.na(ch4RetDtTm) ~ chamb_deply_date_time + (60*5), # assume retrieval 5 minutes after deployment
  #                               TRUE ~ ch4RetDtTm))  # if not na, then use data supplied from adjDataDf

# GO BACK TO STEP 3.1 TO REVIEW TIME SERIES AFTER INCORPORATING NEW DEPLOYMENT AND RETRIEVAL TIMES
# IF SATISFIED WITH PROFILES, MOVE ON TO STEP 4.


#4. PREPARE DATA TO PLOT ALL TIME SERIES----------------
# Trim data to only those we plan to model, plus 60 second buffer on either side
# of modeling window.
gga_3 <- gga_2 %>%
  group_by(lake_id, site_id, visit, instrument) %>% # for each lake and site....
  filter(RDateTime > (min(c(co2DeplyDtTm, ch4DeplyDtTm)) - 60) & 
           RDateTime < (max(c(co2RetDtTm, ch4RetDtTm)) + 60)) %>%
  ungroup()



#5.  PLOT CO2 AND CH4 PROFILES FOR INSPECTION---------------
# Plot all profiles on a single .pdf
# pdf("output/figures/ggaProfile.pdf", paper = "a4r") # landscape orientation
# tic()
# for (i in 1:with(gga_3[!is.na(gga_3$lake_id), ], # this eliminates observations without a Lake_Name (LGR data when chamber not deployed)
#                  length(unique(paste(site_id, lake_id))))) {  # each combination of site and lake
#   print(i)
#   site.lake.i <- with(gga_3[!is.na(gga_3$lake_id), ],  # extract unique lake x site combination
#                       unique(paste(site_id, lake_id)))[i]
#   site.i <- gsub(" .*$", "", site.lake.i)  # extract site.  regex allows for siteIDs of different lengths (i.e. S-01, SU-01)
#   lake.i <- substr(site.lake.i, start = nchar(site.i) + 2, stop = nchar(site.lake.i)) # extract lake name
#   data.i <- filter(gga_3, lake_id == lake.i, site_id == site.i) %>%  # Pull out GGA data chunk
#     select(-GasT_C) # No need to plot gas temperature
#   RDate.i <- unique(data.i$RDate)  # for panel title
# 
#   plot.i <- ggplot(data.i,  aes(x = RDateTime, y = CH4._ppm)) + 
#           geom_point() +
#           geom_vline(data = data.i, aes(xintercept = as.numeric(ch4DeplyDtTm))) +
#           geom_vline(data = data.i, aes(xintercept = as.numeric(ch4RetDtTm))) +
#           scale_x_datetime(labels=date_format("%H:%M")) +
#           ggtitle(paste(lake.i, site.i, RDate.i)) +
#           theme(axis.text.x = element_text(size = 7),
#                 plot.title = element_text(size = 11))
#   
#   plot.ii <- ggplot(data.i,  aes(x = RDateTime, y = CO2._ppm)) + 
#           geom_point() +
#           geom_vline(data = data.i, aes(xintercept = as.numeric(co2DeplyDtTm))) +
#           geom_vline(data = data.i, aes(xintercept = as.numeric(co2RetDtTm))) +
#           scale_x_datetime(labels=date_format("%H:%M")) +
#           ggtitle(paste(lake.i, site.i)) +
#           theme(axis.text.x = element_text(size = 7))
#   
#   grid.arrange(plot.i, plot.ii, ncol = 2) # use to put two plots per page
# }
# 
# 
# dev.off() #15 min, 911 pages, 1/6/23
# 
# toc()
# 
# 
# 
# 
# 
# 
# 
# 
