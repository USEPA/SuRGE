
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
conus_res <- read_csv(paste0(userPath,
    "data/siteDescriptors/CONUS_Reservoirs_from_FM_Clow.csv")) %>%
  janitor::clean_names() %>%
  mutate(lake_nhdid = permanent_id)

# get surge lakes in conus_res
clow_surge <- conus_res %>%
  select(lake_nhdid, area_sq_m_recalc, 
         barren_pct, crop_pct, forest_pct, wetland_pct,
         mean_kfact, mean_slope, mean_soc_0_5_cm) %>%
  filter(lake_nhdid %in% unique(lagos_links$lake_nhdid)) # unique to eliminate revisist

#link conus_res data to the associated lake_id
sl<-lagos_links%>%
  select(lake_nhdid,lake_id)%>%
  distinct()%>%
  left_join(clow_surge)

#make object match clow_predictors below
clow_surgel<- sl %>%
  mutate(soc0_5=as.double(mean_soc_0_5_cm),basin_slope=as.numeric(mean_slope))%>%
  select(lake_id,area_sq_m_recalc,barren_pct,crop_pct,forest_pct,wetland_pct,mean_kfact,
         soc0_5,basin_slope) %>%
  filter(!is.na(barren_pct)) %>%
  mutate(type="clow")

# colnames(clow_surgel)<-c("lake_id","area_sq_m_recalc","barren_pct","crop_pct",
#                          "forest_pct","forest_pct","wetland_pct","mean_kfact",
#                          "soc0_5","basin_slope")

  
# Any SuRGE sites missing?
clow_missing <- lagos_links %>%
  select(lake_id, lake_nhdid) %>%
  distinct() %>% # eliminate revisits
  filter(!(lake_nhdid %in% conus_res$lake_nhdid))

dim(clow_missing) # 33 missing lakes



# 2. GET DATA FOR MISSING LAKES---------
# landuse and kfact from lake_cat, surface area from morpho
# mean basin slope and soil OM must be calculated

# 2.1 BASIN POLYGONS-------
# Needed for basin slope and soil OM content
# Can get basin polygon from terminal NHDPlusV2 flow line or a NWIS
# feature (see stream stats website for NWIS features)

# Missing sites without NHDPlusV2 flow lines or a NWIS feature. If neither
# are available, attempt to delineate basin at stream stats
# 17, 45: stream stats not available for FL
# 10, 165,  317, # need to double check these in stream stats
# 54 NHD flowline (15596925) don't quite match polygon. Use streamstats polygon
# 67 but got a basin from streamstat
# 116 but got basin from streamstat
# 117 NHD flow lines are weird and no nwis feature. got basin from streamstat
# 205 but got basin from streamstat
# 207 but got basin from streamstat
# 296 but got basin from streamstat
# 316 NHD flow line (945030101) don't quite match polygon. Use streamstats polygon
# 1000 PR "USGS-50048690" seems to be outside extent of nhdplusTools but got basin from streamstats

# read basin polygons generated at https://streamstats.usgs.gov/ss/
streamstats_basins <- list(
  `54` = st_read(paste0(userPath, "lakeDsn/CIN/CH4-054/54_basin_streamstat/layers/globalwatershed.shp")),
  `67` = st_read(paste0(userPath, "lakeDsn/CIN/CH4-067/67_basin_streamstat/layers/globalwatershed.shp")),
  `116` = st_read(paste0(userPath, "lakeDsn/CIN/CH4-116/116_basin_streamstat/layers/globalwatershed.shp")),
  `117` = st_read(paste0(userPath, "lakeDsn/DOE/CH4-117/117_basin_streamstat/layers/globalwatershed.shp")),
  `205` = st_read(paste0(userPath, "lakeDsn/CIN/CH4-205/205_basin_streamstat/layers/globalwatershed.shp")),
  `207` = st_read(paste0(userPath, "lakeDsn/CIN/CH4-207/207_basin_streamstat/layers/globalwatershed.shp")),
  `296` = st_read(paste0(userPath, "lakeDsn/USGS/CH4-296/296_basin_streamstat/layers/globalwatershed.shp")),
  `316` = st_read(paste0(userPath, "lakeDsn/USGS/CH4-316/316_basin_streamstat/layers/globalwatershed.shp")),
  `1000` = st_read(paste0(userPath, "lakeDsn/PR/1000_basin_streamstat/layers/globalwatershed.shp"))
)


# feature list for other lakes. used as input for nhdplusTools::get_nldi_basin
site <- list(
  # need to inspect and merge these in ArcGIS Pro
  `326.1` = list(featureSource = "comid", # no NWIS feature # grabs northern portion of basin
                 featureID = "11339177"), # 326 Comins Lake
  `326.2` = list(featureSource = "comid", # no NWIS feature # southern portion of basin
                 featureID = "11339195"), # 326 Comins Lake
  
  # good
  `2` = list(featureSource = "comid", # also USGS-07278500
             featureID = "15256386"), # 2, Arkabutla
  `230` = list(featureSource = "comid", # no NWIS feature, but downloaded basin from streamstat
               featureID = "10108641"), # yellowbanks
  `231` = list(featureSource = "comid", # no NWIS feature, but downloaded basin from streamstat
               featureID = "12557088"), # 231 Parmley, SD  
  `302` = list(featureSource = "nwissite", # comid "23184743". also downloaded basin from streamstat
               featureID = "USGS-13083500"), # 302
  `331` = list(featureSource = "comid", # no NWIS feature, but downloaded basin from streamstat
               featureID = "24561887"), # 331
  `70` = list(featureSource = "nwissite", # 70 whole watershed, not basin
              featureID = "USGS-06452500"), # 70 Francis Case
  `1031` = list(featureSource = "nwissite",
                featureID = "USGS-03247041"), # 1031, harsha
  `1025` = list(featureSource = "nwissite",
                featureID = "USGS-03232470"),# 1025, paint
  `1022` = list(featureSource = "nwissite",
                featureID = "USGS-04211000"), # 1022, roaming
  `1` = list(featureSource = "comid",
             featureID = "18211224"), # demopolis, 1
  `13` = list(featureSource = "comid", # no nwis feature
              featureID = "8392646"), # 13, concord pond
  `138` = list(featureSource = "comid", # also got basin from streamstat
               featureID = "8875982"), # 138, suck cr.
  `14` = list(featureSource = "comid", # also got basin from streamstat
              featureID = "25394231"), # 14
  `209` = list(featureSource = "comid", # no NWIS feature
               featureID = "11898150"), # 209  
  `210` = list(featureSource = "comid", # no NWIS feature
               featureID = "13436029"), # 210, Dalecarlia 
  `308` = list(featureSource = "comid", # no NWIS feature, streamstat basin seems too large. flowlines don't match NHDPlus flow lines
               featureID = "23075585"), # 308, Wapato 
  `65` = list(featureSource = "comid", # no NWIS feature, but downloaded basin from streamstat
              featureID = "6779031"), # 65
  `1005` = list(featureSource = "comid",
                featureID = "3925161")# 1005, brookville
)


# this tool can be used to get feature information
#site_feature <- map(site, ~nhdplusTools::get_nldi_feature(.x)) # not needed?

# get basins for specified features
site_basin <- map(site, ~nhdplusTools::get_nldi_basin(.x))

# write basin polygons to arcGIS for review
#imap(site_basin, ~st_write(.x, paste0(userPath, "lakeDsn/basins/", .y, "_basin.shp"), append = FALSE))

# Basin for 326 is written out in two parts. These parts were manually edited,
# merged, and dissolved in ArcGIS Pro. Remove the two parts from the list and
# replaced with merged basin polygon.
site_basin <- site_basin %>%
  # remove list items
  #https://purrr.tidyverse.org/reference/list_assign.html#arguments
  list_assign("326.1" = zap()) %>%
  list_assign("326.2" = zap()) %>%
  # add edited 326 basin polygon
  list_assign("326" = st_read(paste0(userPath, "lakeDsn/USGS/CH4-326/326_basin.shp"))) %>%
  c(streamstats_basins) # add stream stats basins


map(site_basin, ~st_crs(.x)) # 4326


# 2.2. BASIN SLOPE---------
# basin slope is derived from digital elevation models for the basin

# get digital elevation model (DEM)
elev_dem <- map(site_basin, ~get_elev_raster(.x, z=9, clip = "locations") %>% # clip to basin
                  rast(.)) # convert to SpatRaster

# preview DEMs
map(elev_dem,  function(x) {
  plot(x)
  #readline(prompt="Press [enter] to continue")
})

# write DEM to disk if desired
# imap(elev_dem, ~writeRaster(.x, paste0(userPath, "lakeDsn/DEM/", .y, "_dem.tiff")))

# calculate slope for each rater cell in DEM
slope <- map(elev_dem, ~terra::terrain(x = .x, v = 'slope', unit = 'degrees'))
# preview slope
map(slope,  function(x) {
  plot(x)
  #readline(prompt="Press [enter] to continue")
})

# calculate mean slope per basin
basin_slope <- map(slope, ~terra::global(.x, fun = "mean", na.rm = TRUE)) %>% 
  bind_rows(.id = "name") %>% # collapse to df, retain list element names
  rename(lake_id = name,
         basin_slope = mean) %>%
  mutate(lake_id = as.numeric(lake_id)) %>%
  magrittr::set_rownames(NULL) # remove row names


# 2.3. SOIL ORGANIC MATTER----------
# CODE BELOW REQUIRES ~ 25 MINUTES TO RUN. UNCOMMENT AND RUN, OR SKIP TO 
# LINE 277 AND LOAD FINAL OBJECT FROM DISK



# Model requires organic matter (g/m2) in top 5 cm of soil. 
# USDA Gridded Soil Survey Geographic (gSSURGO) Database of CONUS soil 
# properties downloaded at https://nrcs.app.box.com/v/soils/folder/233395259341
# The features in the .gdb relevant to this work are the MUKEY raster
# layer and the Valu1 table. The only attribute in the raster is MUKEY
# which uniquely identifies each cell. I used ArcGIS Pro to join the 
# raster to the valu1 table which contains 0-5cm soil organic matter (soc0_5).
# R can't read rasters from .gdb, so I exported the raster to a standalone 
# raster which is read in below. 

# # read conus scale raster.
# # this is a multi-categorical SpatRaster, meaing that MUKEY and soc0_5 are
# # coded as categorical variables. Use terra::catalyze (see below) to convert
# # to numeric
# gsurgo <- terra::rast(paste0(userPath, "data/siteDescriptors/gSSURGO/mu_conus.tif"))
# terra::cats(gsurgo) # list categorical layers
# 
# # Prepare list of basins for clipping to to soil organic matter raster
# site_basin_no_fc <- site_basin %>% 
#   purrr::list_modify("70" = zap()) %>% # remove francis case huge basin!!!
#   purrr::list_modify("1000" = zap()) # remove Puerto Rico site. Not inlcuded in SURGO download
# 
# 
# # clip soil raster to basin polygons
# tic() # about 25 minutes
# soc <- map(site_basin_no_fc, # for each basin polygon
#            ~terra::crop(gsurgo, # crop the soil raster
#                         .x %>% # crop to basin polygon
#                           st_transform(5070) %>% # transform basin polygon CRS to match soil CRS
#                           terra::vect(.), # crop expects a SpatVector so cast sf object as vect
#                         mask = TRUE) %>% # clip to polygon, not polygon extent
#              catalyze(.) %>% # converts categorical  attributes (soc0_5, MUKEY) to numeric
#              terra::global(., fun="mean", na.rm = TRUE), # calculate mean of attributes
#            .progress = list( # show progress bar
#              type = "iterator", 
#              format = "Calculating {cli::pb_bar} {cli::pb_percent}",
#              clear = TRUE))
# toc()
# 
# basin_soc <- map(soc, ~rownames_to_column(.x)) %>%
#   # use this after map function
#   bind_rows(.id = "name") %>% # collapse to df, retain list element names
#   mutate(lake_id = as.numeric(name)) %>%
#   filter(rowname == "soc0_5") %>%
#   rename(soc0_5 = mean) %>%
#   select(-rowname, - name)
# 
# saveRDS(basin_soc, paste0(userPath, "data/siteDescriptors/basin_soc.rds"))
basin_soc<-readRDS(paste0(userPath, "data/siteDescriptors/basin_soc.rds")) # loads basin_soc


# 2.4. BASIN LANDUSE AND KFACT------------
basin_lu <- lake_cat %>% 
  mutate(forest_pct = pctconif2019cat + pctdecid2019cat + pctmxfst2019cat,
         wetland_pct = pcthbwet2019cat + pctwdwet2019cat,
         crop_pct = pctcrop2019cat,
         barren_pct = pctbl2019cat,
         mean_kfact = kffactcat) %>%
  select(lake_id, forest_pct, wetland_pct, crop_pct, barren_pct, mean_kfact)



# 2.5. GET LAKE AREA---------
lake_area <- left_join(clow_missing %>% select(lake_id),
                       morpho %>% select(lake_id, surface_area)) %>%
  rename(area_sq_m_recalc = surface_area)

# 2.6. AGGREGATE CLOW PREDICTORS------
clow_predictors <- inner_join(lake_area, basin_lu) %>%
  inner_join(., basin_soc) %>%
  inner_join(., basin_slope)%>%
  mutate(type="beaulieu")%>%
  mutate(barren_pct=barren_pct/100,
         wetland_pct=wetland_pct/100,
         crop_pct=crop_pct/100,
         forest_pct=forest_pct/100)



# 3. MERGE PREDICTORS FOR MISSING SITES WITH THE LARGER----------
#    CLOW - SURGE DATAFRAME
clow_surge <- bind_rows(clow_surgel, clow_predictors)

# 4. CALCULATE SEDIMENTATION RATES-------------
clow_surge <- clow_surge %>%
  mutate(log_sedimentation = -1.520 + 
           0.925 * area_sq_m_recalc +
           0.043 * basin_slope +
           -0.379 * forest_pct +
           0.263 * crop_pct,
         sedimentOC = case_when(wetland_pct < 0 ~ NA_real_,
                                TRUE ~ 17.170 +
                                  0.0013 * soc0_5 +
                                  19.197 * log10(wetland_pct + 1) +
                                  -26.494 * barren_pct +
                                  -14.098 * mean_kfact +
                                  -1.275 *
                                  log10(area_sq_m_recalc) +
                                  -2.055))
clow_surge$sedimentOC<-ifelse(clow_surge$sedimentOC<0,0.1,clow_surge$sedimentOC)
#Check derived values against clow values

sedplot<-clow_surge %>%
  ggplot(aes(x=area_sq_m_recalc,y=log_sedimentation))+
  geom_point(aes(color=type,alpha=0.4))+
  scale_y_log10()+
  scale_x_log10()
sedplot

ocplot<-clow_surge %>%
  ggplot(aes(x=area_sq_m_recalc,y=soc0_5))+
  geom_point(aes(color=type,alpha=0.4))+
  scale_y_log10()+
  scale_x_log10()
ocplot

ocplot<-clow_surge %>%
  ggplot(aes(x=area_sq_m_recalc,y=sedimentOC))+
  geom_point(aes(color=type,alpha=0.4))+
  scale_y_log10()+
  scale_x_log10()
ocplot

#Create sedimentation link

sedimentation_link<-clow_surge %>%
  mutate(sedimentation_m3y=log_sedimentation)%>%
  select(lake_id,sedimentation_m3y,sedimentOC)%>%
  mutate(C_sedimentation_m3y=sedimentOC*sedimentation_m3y)%>%
  mutate(log_sedimentation=log10(sedimentation_m3y))

#There are no additional sedimentation estimates in RESSED that aren't already in Clow
# RESSED_link <- left_join(RESSED_link,clow_links, by="lake_id")
# RESSED_link$C_sedimentation<-RESSED_link$log_sedimentation*RESSED_link$sedimentOC


