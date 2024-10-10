
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



#Build Equations for calculating sedimentation for whole dataset

#need to join sediment OC data to lake_cat file

lake_cat$OCburialrate <- 0.34 +
  0.046 * lake_cat$tmean8110cat +
  2.020 * lake_cat$kffactcat +
  0.184 * +0.766 * (sum(lake_cat$pctwdwet2019cat, lake_cat$pcthbwet2019cat))




