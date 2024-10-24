
## Link SuRGE Lakes to RESSED
## August 29, 2024

# CODE BELOW WORKS ON JAKES MACHINE, BUT NOT BRIDGET'S
#### Libraries =================================================================
# library(RODBC)
# library(dplyr)
# library(dbplyr)
# 
# #### Import database ===========================================================
# # Define connection strings
# dbq_string <- paste0("DBQ=", paste0(userPath, "data/siteDescriptors/"),"RESSED_v1.2.mdb")
# driver_string <- "Driver={Microsoft Access Driver (*.mdb, *.accdb)};"
# db_connect_string <- paste0(driver_string, dbq_string)
# 
# # Create .accdb connection
# con <- odbcDriverConnect(db_connect_string)
# 
# # See Tables
# sqlTables(con)
# 
# #### Extract tables to data.frames
# sqlFetch(con, "Lat_Lon_New")
# sqlFetch(con, "RSED06")
# sqlFetch(con, "RSED09")


# manually linked SuRGE sites with RESSED here-- doesn't give us any
# additional sedimentation data 
# 
# RESSEDl <- read.csv(paste0(userPath, "data/siteDescriptors/RESSED_links.csv"))
# 
# RESSED_link <- RESSEDl %>%
#   filter(!is.na(period_yrs)) %>%
#   group_by(RESSED) %>%
#   summarise(
#     siteID = siteID[1],
#     tot_per_seddep = sum(tot_per_seddep),
#     period_yrs = sum(period_yrs),
#     acre.feet.per.yr = tot_per_seddep / period_yrs
#   ) %>%
#   mutate(lake_id = str_extract(siteID, "(\\d+$)") %>% as.numeric) %>% # extract numeric part of lake_id)
#   select(RESSED, lake_id, acre.feet.per.yr)
# 
# RESSED_link<-RESSEDl %>%
#   filter(!is.na(period_yrs))%>%
#   mutate(cubic_meter_sed=1233.4818375*tot_per_seddep)%>%
#   group_by(RESSED)%>%
#   summarise(siteID=siteID[1],cubic_meter_sed=sum(cubic_meter_sed),period_yrs=sum(period_yrs),
#             cubic_meters_per_yr=cubic_meter_sed/period_yrs)%>%
#   mutate(lake_id = str_extract(siteID, "(\\d+$)"))%>% # extract numeric part of lake_id)
#   select(RESSED,lake_id,cubic_meters_per_yr)
# 
# RESSED_link$lake_id<-as.numeric(RESSED_link$lake_id)

#Forced read-in of everything as a character class to preserve the leading zeros in the Reach Code
conus_res <- read.csv(paste0(userPath,
    "data/siteDescriptors/CONUS_Reservoirs_from_FM_Clow.csv"),
  colClasses = "character")

conus_res$Area_sq_m_recalc <- as.numeric(conus_res$Area_sq_m_recalc)
conus_res$Barren_pct <- as.numeric(conus_res$Barren_pct)
conus_res$Mean_Slope <- as.numeric(conus_res$Mean_Slope)
conus_res$Forest_pct <- as.numeric(conus_res$Forest_pct)
conus_res$Crop_pct <- as.numeric(conus_res$Crop_pct)
conus_res$Mean_SOC_0_5_cm <- as.numeric(conus_res$Mean_SOC_0_5_cm)
conus_res$Wetland_pct <- as.numeric(conus_res$Wetland_pct)
conus_res$Mean_Kfact <- as.numeric(conus_res$Mean_Kfact)


conus_res$log_sedimentation <- -1.520 + 0.925 * conus_res$Area_sq_m_recalc +
  0.043 * conus_res$Mean_Slope+-0.379 * conus_res$Forest_pct +
  0.263 * conus_res$Crop_pct


conus_res <- conus_res %>%
  mutate(
    sedimentOC = ifelse(
      Wetland_pct < 0,
      NA,
      17.170 +
        0.0013 * Mean_SOC_0_5_cm +
        19.197 * log10(Wetland_pct + 1)+-26.494 * Barren_pct+-14.098 * Mean_Kfact+-1.275 *
        log10(Area_sq_m_recalc)+-2.055
    )
  )

conus_res$lake_nhdid <- conus_res$Permanent_ID

#GNIS ID and reachcode don't help obtain any additional matches
#Use NHD ID to link the clow dataset to SuRGE IDs 
clow_link <- left_join(
  lagos_links %>%
    select(lake_id, lake_nhdid, lake_namegnis),
  conus_res %>% select(log_sedimentation, sedimentOC, lake_nhdid),
  by = "lake_nhdid",
  na_matches = "never"
)

#113 matches
clow_links <- unique(clow_link) %>%
  filter(!is.na(log_sedimentation))

#There are no additional sedimentation estimates in RESSED that aren't already in Clow
RESSED_link <- clow_link
# RESSED_link <- left_join(clow_link,RESSED_link, by="lake_id")
# RESSED_link$C_sedimentation<-RESSED_link$log_sedimentation*RESSED_link$sedimentOC
