## Look at LAGOS Dataset and Get Lagos ID Links for Surge Sites
## January 17, 2024

#You will need to install LAGOSUS in R the first time you run this
#devtools::install_github("cont-limno/LAGOSUS", dependencies = TRUE)

#set relevant working directory
#setwd("C:/Users/bdeemer/OneDrive - DOI/Documents/Terminal Lakes/Data_Explore")

library(LAGOSUS)
#library(mapview)
#library(dplyr)
#need to install devtools, dplyr, and mapview on SuRGE 

#I think you'll need to run the lagosus_get command once and then can hash it out
lagosus_get(dest_folder = lagosus_path())
lg <- lagosus_load(modules = c("locus"))
names(lg)
ll<-(lg$locus$lake_link)

#Read in the SuRGE sites from the updated eval_status spreadsheet, created a .csv from excel file in share drive
# read in SuRGE site information (lat/long required) and filter to sampled sites
SuRGEex <- read_xlsx(paste0(userPath, "surgeDsn/SuRGE_design_20191206_eval_status.xlsx")) %>%
  filter(`EvalStatus Code` == "S")

#simplify spreadsheet to just contain Surge SiteID, NLA_SITE_ID, NHDPlusCOMID
SuRGE<-SuRGEex %>%
  select(siteID,SITE_ID,NHDPlusWaterbodyCOMID,GNIS_NAME,LAT_DD83,LON_DD83)

colnames(SuRGE)<-c("siteID","NLA_SITE_ID","nhdplusv2_comid","GNIS_NAME","LAT_DD83","LON_DD83")
SuRGE$nhdplusv2_comid<-as.character(SuRGE$nhdplusv2_comid)

#CH4-1010 has an NHDPlus comid that mismatches to Lagos, correct lagos link can be accessed via the NHDPlus comid == 166830448
SuRGE$nhdplusv2_comid<-ifelse(SuRGE$siteID=="CH4-1010","166830448",SuRGE$nhdplusv2_comid)

#Now create single lake links from lagos
#There are multiple lagosus_legacysiteids and wqp_monitoringlocationidentifiers
#For single nhdplusv2_comid and lagoslakeid 

llc<-ll%>%
  group_by(nhdplusv2_comid)%>%
  filter(row_number()==1)%>%
  select(lagoslakeid,lake_nhdid,lake_namegnis,nhdhr_gnisid,nhdplusv2_comid)

SurGE_links<-left_join(SuRGE,llc,by="nhdplusv2_comid")

#Now manually add the NHDhr IDs to the reservoirs that aren't in Lagos or where Lagos doesn't have an nhdplus COMID
SurGE_links$lake_nhdid<-ifelse(SurGE_links$siteID=="CH4-014","{26f31221-6370-4bfa-a387-5b9665aae9f3}",
                                    ifelse(SurGE_links$siteID=="CH4-1000","26441842",
                                                         ifelse(SurGE_links$siteID=="CH4-010", "605A5DB3-01F6-4EC4-9EC4-640CD814795F",
                                                                ifelse(SurGE_links$siteID=="CH4-1009","120022128",
                                                                       ifelse(SurGE_links$siteID=="CH4-1010","120021486", SurGE_links$lake_nhdid)))))

#Now add the Lagos link for CH4-10 & CH4-1009 since lagos lacked the comid ID to link to SuRGE
SurGE_links$lagoslakeid<-ifelse(SurGE_links$siteID=="CH4-010","201797",
                                ifelse(SurGE_links$siteID=="CH4-1009","260194",
                                       ifelse(SurGE_links$siteID=="CH4-1010","260193",SurGE_links$lagoslakeid)))

#Now add the NHD comid ID to CH4-1009 since lagos lacked the comid ID to link to SuRGE
SurGE_links$nhdplusv2_comid<-ifelse(SurGE_links$siteID=="CH4-1009","456124",SurGE_links$nhdplusv2_comid)

write.csv(SurGE_links,file="Surge_nhdhr.csv")
