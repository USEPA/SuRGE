
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


# manually linked SuRGE sites with RESSED here

RESSEDl <- read.csv(paste0(userPath, "data/siteDescriptors/RESSED_links.csv"))

RESSED_link <- RESSEDl %>%
  filter(!is.na(period_yrs)) %>%
  group_by(RESSED) %>%
  summarise(
    siteID = siteID[1],
    tot_per_seddep = sum(tot_per_seddep),
    period_yrs = sum(period_yrs),
    acre.feet.per.yr = tot_per_seddep / period_yrs
  ) %>%
  mutate(lake_id = str_extract(siteID, "(\\d+$)") %>% as.numeric) %>% # extract numeric part of lake_id)
  select(RESSED, lake_id, acre.feet.per.yr)

RESSED_link<-RESSEDl %>%
  filter(!is.na(period_yrs))%>%
  mutate(cubic_meter_sed=1233.4818375*tot_per_seddep)%>%
  group_by(RESSED)%>%
  summarise(siteID=siteID[1],cubic_meter_sed=sum(cubic_meter_sed),period_yrs=sum(period_yrs),
            cubic_meters_per_yr=cubic_meter_sed/period_yrs)%>%
  mutate(lake_id = str_extract(siteID, "(\\d+$)"))%>% # extract numeric part of lake_id)
  select(RESSED,lake_id,cubic_meters_per_yr)

RESSED_link$lake_id<-as.character(RESSED_link$lake_id)


#Read in CONUS reservoir data provided by David Clow
lake_cat$OCburialrate <- 0.34 +
  0.046 * lake_cat$tmean8110cat +
  2.020 * lake_cat$kffactcat +
  0.184 * +0.766 * (sum(lake_cat$pctwdwet2019cat, lake_cat$pcthbwet2019cat))

#Forced read-in of everything as a character class to preserve the leading zeros in the Reach Code
conus_res<-read.csv(paste0(userPath,"data/siteDescriptors/CONUS_Reservoirs_from_FM_Clow_clipped_Morris_Kirwan.csv"),colClasses="character")
conus_res$Area_sq_m_recalc<-as.numeric(conus_res$Area_sq_m_recalc)
conus_res$Barren_pct<-as.numeric(conus_res$Barren_pct)
conus_res$Mean_Slope<-as.numeric(conus_res$Mean_Slope)
conus_res$Forest_pct<-as.numeric(conus_res$Forest_pct)
conus_res$Crop_pct<-as.numeric(conus_res$Crop_pct)
conus_res$Mean_SOC_0_5_cm<-as.numeric(conus_res$Mean_SOC_0_5_cm)
conus_res$Wetland_pct<-as.numeric(conus_res$Wetland_pct)
conus_res$Mean_Kfact<-as.numeric(conus_res$Mean_Kfact)


conus_res$log_sedimentation<--1.520+ 0.925*conus_res$Area_sq_m_recalc+
                        0.043*conus_res$Mean_Slope+
                        -0.379*conus_res$Forest_pct+
                        0.263*conus_res$Crop_pct


conus_res<-conus_res %>%
  mutate(sedimentOC=ifelse(Wetland_pct<0,NA, 17.170 + 
                             0.0013*Mean_SOC_0_5_cm +
                        19.197*log10(Wetland_pct+1)+
                        -26.494 * Barren_pct+
                        -14.098 * Mean_Kfact +
                        -1.275 *log10(Area_sq_m_recalc)+
                        -2.055))

conus_res$lake_reachcode<-conus_res$Reach_Code
#Make GNIS ID a numeric field so that the leading zeros are dropped & it 
#matches the GNIS ID in the surge link
conus_res$gnis_id<-as.numeric(conus_res$GNIS_ID)

#Select relevant info to pass to SurGE link

#first link the clow dataset to SurGE IDs using the NHD reach code
clow_link <- left_join(surge_sites_reach_codes %>% 
                                       select(lake_id, lake_reachcode,gnis_name,gnis_id), # only keep minimum needed variables
                                     conus_res %>% select(log_sedimentation,sedimentOC,lake_reachcode,GNIS_ID,Area_sq_m_recalc), # only keep merge variable and lagoslakeid 
                                     by = "lake_reachcode",na_matches="never")

#next link the clow dataset to SuRGE IDs using the GNIS ID
clow_link<- left_join(clow_link %>%
  select(lake_id,lake_reachcode,gnis_name,gnis_id,log_sedimentation,sedimentOC,lake_reachcode),
conus_res %>% select(log_sedimentation,sedimentOC,gnis_id,GNIS_Name),
by= "gnis_id",na_matches="never")

clow_links<-clow_link %>%
  mutate(log_sedimentation=ifelse(is.na(log_sedimentation.x),log_sedimentation.y,log_sedimentation.x))%>%
  mutate(sedimentOC=ifelse(is.na(sedimentOC.x),sedimentOC.y,sedimentOC.x))

clow_links$lake_id<-as.character(clow_links$lake_id)

RESSED_link <- left_join(clow_links,RESSED_link, by="lake_id")

