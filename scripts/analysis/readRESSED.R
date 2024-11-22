
## Link SuRGE Lakes to RESSED/CLOW
## August 29, 2024

# LEGACY CODE TO READ RESSED-----------------------
# CODE BELOW WORKS ON JAKES MACHINE, BUT NOT BRIDGET'S
#### Libraries
# library(RODBC)
# library(dplyr)
# library(dbplyr)
# 
# #### Import database
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
#
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


# 1. READ SEDIMENTATION DATA FROM DAVID CLOW-----------
# provided by email on 10/8/2024

# Forced read-in of everything as a character class to preserve the leading zeros in the Reach Code
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


# 2. CALCULATE SEDIMENTATION RATES-------------
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


# 3. MATCH SEDIMENTATION RECORDS TO SURGE SITES------------------
#GNIS ID and reachcode don't help obtain any additional matches
#Use NHD ID to link the clow dataset to SuRGE IDs 
clow_link <- left_join(
  lagos_links %>%
    select(lake_id, lake_nhdid, lake_namegnis),
  conus_res %>% select(log_sedimentation, sedimentOC, lake_nhdid),
  by = "lake_nhdid",
  na_matches = "never"
)

#Make list of lakes with missing sedimentation data
clow_missing <- filter(clow_link, is.na(clow_link$log_sedimentation))

#113 matches
clow_links <- unique(clow_link) %>%
  filter(!is.na(log_sedimentation))

#There are no additional sedimentation estimates in RESSED that aren't already in Clow
RESSED_link <- clow_links
# RESSED_link <- left_join(RESSED_link,clow_links, by="lake_id")
# RESSED_link$C_sedimentation<-RESSED_link$log_sedimentation*RESSED_link$sedimentOC


# 4. CAN WE GET PREDICTOR VARIABLES FOR MISSING LAKES------------
# Clow worked at huc12 scale. Step 1 is to identify huc12 code(s) associated with
# the unmatched lakes. nhdplustools has handy functions for this.

# load example lake polygon
lake_poly <- st_read(paste0(userPath, "lakeDsn/2016_survey/acton/actonEqArea.shp"))
lake_hl <- st_read(paste0(userPath, "lakeDsn/2016_survey/harsha/harshaEqArea.shp"))

# use nhdplustools to identify associated polygons
lake_huc <- nhdplusTools::get_huc(lake_poly, type = "huc12") # get associated huc12 boundaries
plot(sf::st_geometry(lake_huc)) # plot huc12
plot(sf::st_geometry(lake_poly), col = "blue", add = TRUE) # add lake

# This is a good start, but includes a HUC that extends far downstream of lake and
# misses an upstream HUC. Probably need to evaluate on a lake by lake basis.


### THIS WORKS IF WE HAVE A NWIS STATION NEAR DAM
# https://gis.stackexchange.com/questions/346303/accessing-watershed-boundary-for-lakes-in-r
# Harsha lake dam
site_id <-  "USGS-03247041" #"USGS-03246500" east fork little miami at Williamsburg
site <- list(featureSource = "nwissite",
             featureID = "USGS-03247041")
site_feature <- nhdplusTools::get_nldi_feature(site)
site_basin <- nhdplusTools::get_nldi_basin(site)
st_crs(site_basin)
lake_hl_wgs <- lake_hl %>% st_transform(4326)

# gets huge basin. Is this what we want? probably.
plot(st_geometry(site_basin))
plot(st_geometry(site_feature), add=TRUE)
plot(st_geometry(lake_hl_wgs), add = TRUE)


# WHAT IF NO NWIS FEATURE. CAN WE USE DAM LAT/LONG?
# YES, THIS WORKS TOO.
# Harsha Lake Pour Points
PLon<- -84.1509940
PLat<- 39.0224387

#Step 1
point <- sf::st_sfc(sf::st_point(c(PLon, PLat)), crs = 4326)
#Get nhdplus flow line comid from point
ID <- nhdplusTools::discover_nhdplus_id(point)


#Get Basin 
nldi <- list(featureSource = "comid", featureID = ID)
basin <- nhdplusTools::get_nldi_basin(nldi_feature = nldi)


leaflet() %>%
  addPolygons(data = basin) %>%
  addCircleMarkers(data = point,
                   #lat = ~lat, lng = ~long,
                   color = "red",
                   radius = 2) %>%
  addTiles() #Add tiles adds defualt map 

# NEXT STEP IS TO CUT RASTER TO WATERSHED BOUNDARY AND AGGREGATE SOIL DATA
st_layers("C:/Users/JBEAULIE/OneDrive - Environmental Protection Agency (EPA)/GIS_data/SURGO/gSSURGO/gSSURGO_OH/gSSURGO_OH.gdb")
surgo <- raster::raster("C:/Users/JBEAULIE/OneDrive - Environmental Protection Agency (EPA)/GIS_data/SURGO/gSSURGO/gSSURGO_OH/gSSURGO_OH.gdb")
surgo_soc <- st_read("C:/Users/JBEAULIE/OneDrive - Environmental Protection Agency (EPA)/GIS_data/SURGO/gSSURGO/gSSURGO_OH/gSSURGO_OH.gdb", 
                     layer = "Valu1") %>%
  as_tibble() %>%
  dplyr::select(mukey, soc0_5) %>%
  mutate(mukey = as.numeric(mukey))
  
table(surgo_soc$mukey)
class(surgo)
str(surgo)
dim(surgo) # 42173 36989     1
surgo
names(surgo) # why only mukey and not soc0_5?
nlayers(surgo) # 1
st_crs(surgo) # 9001
st_crs(basin) #4326

basin <- basin %>% 
  st_transform(st_crs(surgo)) %>%
  st_zm()
st_crs(basin) == st_crs(surgo) # TRUE
plot(st_geometry(basin))


basin_surgo <- raster::extract(surgo, basin)
length(unlist(basin_surgo)) #8875622, double
tibble(mukey = unlist(basin_surgo)) %>%
  left_join(surgo_soc) %>%
  summarize(mean_soc = mean(soc0_5, na.rm = TRUE)) # 791 gC/m2

# this should work, but can't get it to calculate mean of soc0_5,
# rather it calculates mean mukey
raster::extract(surgo, basin, fun = mean) # calculate mean
