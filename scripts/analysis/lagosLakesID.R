## Create Lake-link Crosswalk, Compile LAGOS Data for Surge Sites 
## Last edited 6/24/2024

#Installed via renv
#devtools::install_github("cont-limno/LAGOSUS", dependencies = TRUE)

# loaded from masterlibrary.R
#library(LAGOSUS)
#library(httr) #for reading trophic status lagos data, only necessary for first run

# [6/12/2024] working on Jake's machine!!  If acting up, skip to line 18
#lagosus_get(dest_folder = lagosus_path()) # run once, then hash out
lg <- lagosus_load(modules = c("locus"))
names(lg)
ll<-(lg$locus$lake_link)
lc<-(lg$locus$lake_characteristics)

# Read lake_link from SharePoint or web if the above isn't working.
# https://portal.edirepository.org/nis/mapbrowse?packageid=edi.854.1
#ll <- read.csv(paste0(userPath, "data/siteDescriptors/lake_link.csv")) # downloded locally
# ll <- read_csv("https://portal.edirepository.org/nis/dataviewer?packageid=edi.854.1&entityid=5488e333ce818597fa3dbfc9b4e0c131") # from web

#Read in the SuRGE sites from the updated eval_status spreadsheet, created a .csv from excel file in share drive
# read in SuRGE site information (lat/long required) and filter to sampled sites
surge_sites <- read_xlsx(paste0(userPath, "surgeDsn/SuRGE_design_20191206_eval_status.xlsx"), na = "NA") %>%
  filter(`EvalStatus Code` == "S") %>% 
  janitor::clean_names() %>%
  dplyr::rename(lake_id = site_id) %>%
  mutate(lake_id = str_extract(lake_id, "(\\d+$)") %>% # extract numeric part of lake_id
           as.numeric()) %>% # convert lake_id to numeric
  rename(nla17_site_id = site_id_2) %>%
  mutate(nhdplusv2_comid = as.character(nhd_plus_waterbody_comid))%>%
  select(lake_id, nla17_site_id, nhdplusv2_comid, gnis_name, lat_dd83, lon_dd83)


#Now create single lake links from lagos
#There are multiple lagosus_legacysiteids and wqp_monitoringlocationidentifiers
#For single nhdplusv2_comid and lagoslakeid 

llc <- ll %>%
  group_by(nhdplusv2_comid) %>%
  filter(row_number() == 1) %>% # alternatives: slice_head(1) and  top_n(n = 1) 
  select(lagoslakeid, lake_nhdid, lake_namegnis, nhdhr_gnisid, nhdplusv2_comid)

lake_links <- left_join(surge_sites, llc, by = "nhdplusv2_comid") %>%
  #add NHD high resolution IDs to the reservoirs that aren't in Lagos or where Lagos doesn't have an nhdplus COMID
  mutate(lake_nhdid = case_when(lake_id == 14 ~ "{26f31221-6370-4bfa-a387-5b9665aae9f3}",
                                lake_id == 1000 ~ "26441842",
                                lake_id == 10 ~ "605A5DB3-01F6-4EC4-9EC4-640CD814795F",
                                lake_id == 1009 ~ "120022128",
                                lake_id == 1010 ~ "120021486",
                                TRUE ~ lake_nhdid)) %>%
  #Now add the Lagos link for 10, 1009, and 1010 since lagos lacked the comid ID to link to SuRGE
  mutate(lagoslakeid = case_when(lake_id == 10 ~ 201797,
                                 lake_id == 1009 ~ 260194,
                                 lake_id == 1010 ~ 260193,
                                 TRUE ~ lagoslakeid)) %>%
# # 1009 Carr Fork Lake is symbolized as a flow path in NHDPlusV2 with comid ID 456124.  Not including here.
# mutate(nhd_plus_waterbody_comid = case_when(lake_id == 1009 ~ 456124
#                                             TRUE ~ nhd_plus_waterbody_comid))
  select(lake_id, lagoslakeid, lake_nhdid, nla17_site_id, nhdplusv2_comid, gnis_name, lat_dd83, lon_dd83) 

lake_links %>% filter(is.na(lagoslakeid)) %>% {dim(.)} # only two lakes w/out LAGOS record!  One is PR.

#Setup file with lagos lake connectivity metrics
lagos_lake_connectivity <- left_join(lake_links, lc, by = "lagoslakeid") %>%
  select(lake_id,lagoslakeid,lake_connectivity_class,lake_connectivity_fluctuates,lake_connectivity_permanent,
         lake_lakes4ha_upstream_ha,lake_lakes4ha_upstream_n,lake_lakes1ha_upstream_ha,lake_lakes1ha_upstream_n,
         lake_lakes10ha_upstream_n,lake_lakes10ha_upstream_ha)

# LAGOS-US
# lagos productivity estimates
# 7 GB, takes about 30 minutes to download.  Download once, save to disk, then
# load from disk.  Much faster.
# preprint link is: https://www.biorxiv.org/content/10.1101/2024.05.10.593626v1
# url_lagos<- "https://portal-s.edirepository.org/nis/dataviewer?packageid=edi.1427.3&entityid=3cb4f20440cbd7b8e828e4068d2ab734"
# httr::GET(url_lagos, progress(), write_disk(tf <- tempfile(fileext = ".csv"))) # about 30 minutes at AWBERC
#lagos <- read.csv(tf) # another 15 minutes

#Temporarily save the entire lagos file so I don't have to download it again
#saveRDS(lagos,"C:/R_Projects/lagos.rds")
#lagos<-readRDS("C:/R_Projects/lagos.rds)

# tic() #~4 minutes
# lagos_ <- as_tibble(lagos) %>%
#   #slice_head(n=5) %>% # subset for testing
#   janitor::clean_names() %>%
#   mutate(month = gsub( " .*$", "", sensing_time) %>% as.Date(., format = "%Y-%m-%d") %>% lubridate::month(.),
#          year = gsub( " .*$", "", sensing_time) %>% as.Date(., format = "%Y-%m-%d") %>% lubridate::year(.)) %>%
#   filter(month %in% 6:9) %>% # only June - September
#   filter(lagoslakeid %in% (lake_links$lagoslakeid))
#   #filter(year %in% c(2018,2020))
# toc()

#look at coverage by site
lagos_site_summary<-lagos_ %>%
  group_by(lagoslakeid)%>%
  summarize(minyear=min(year),maxyear=max(year))

#Decide if we want to save this in the inputData folder?  Or through the userPath folder structure?
#saveRDS(lagos_,"C:/R_Projects/SuRGE/inputData/lagos_.rds")
lagos_ <- readRDS("inputData/lagos_.rds")


