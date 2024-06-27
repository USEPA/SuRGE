## Read In Renee Brook's Water Isotope Data and Match with Surge Links
## Last Updated 6/26/2024

# MAKE SURE YOU HAVE RUN THE lagosLakesID.R TO CREATE THE
# lake_links OBJECT

#Metadata:
# UNIQUE_ID	unique site ID across all NLA surveys
# DSGN_CYCLE	study year design cycle
# UID	unique sample ID
# SITE_ID	site ID used in the particular NLA
# VISIT_NO	visit number within a study year: 1 or 2 if repeat visit
# DATE_COL	date of sample collection at the site
# LAT_DD83	Nominal latitude in decimal degrees for the sample site
# LON_DD83	Nominal longitude in decimal degrees for the sample site
# H2O_d2H	Stable isotope ratio of hydrogen in water expressed in per mil. The isotope value was determined from a subsample of the NARS water chemistry sample
# H2O_d18O	Stable isotope ratio of oxygen in water expressed in per mil. The isotope value was determined from a subsample of the NARS water chemistry sample
# d_excess	deuterium excess  = d2H - 8 x d18O
# E_I	Evaporation to Inflow ratio (E:I) - the proportion of water entering the lake (inflow from surface and groundwater) that leaves the lake through evaporation (See Brooks et al. 2014 for details). 
# RT_iso	Residence time estimated from (E:I) * (Lake volume)/ (potential evaporation PET from the lake surface area).  Units: years

#Read in the water isotope data that Renee sent as three separate files for the different years
waterisotope17<- read_xlsx(paste0(userPath, "data/siteDescriptors/NLA water isotope data-2007-2017.xlsx"),
                                       sheet = "NLA_17_water_isotopes",na="NA")%>%
                  janitor::clean_names()
#The NLA ID for a given year is different than the NLA ID across years, so pull 
#both out and name them for the lake link
waterisotope17$nla17_site_id<-waterisotope17$site_id
waterisotope17$nla_unique_id<-waterisotope17$unique_id

waterisotope12<-  read_xlsx(paste0(userPath, "data/siteDescriptors/NLA water isotope data-2007-2017.xlsx"),
                            sheet = "NLA_12_water_isotopes",na = "NA")%>%
                  janitor::clean_names()
waterisotope07<- read_xlsx(paste0(userPath, "data/siteDescriptors/NLA water isotope data-2007-2017.xlsx"),
                           sheet = "NLA_07_water isotopes",na = "NA")%>%
                   janitor::clean_names()

#Setup file with desired water isotope data
#lake_links$nla17_site_id<-as.factor(lake_links$nla17_site_id)
water_isotope_data_17 <- left_join(lake_links, waterisotope17, by ="nla17_site_id")%>% 
  select(lake_id,nla17_site_id,nla_unique_id,visit_no,e_i,rt_iso)
colnames(water_isotope_data_17)<-c("lake_id","nla_unique_id","site_id","visit_no","e_i","rt_iso")
water_isotope_data_17$surveyyear<-2017

#create link from lake_id to the nla unique_id
link<-water_isotope_data_17 %>%
  select(lake_id,nla_unique_id)
link<-unique(link)

#I don't see any matches to our sites in the 2012 or 2007 data... I need to make
#sure this is real and not just a coding error
water_isotope_data_12<-filter(waterisotope12,unique_id %in% water_isotope_data_17$nla_unique_id)%>%
  select(unique_id,site_id,visit_no,e_i,rt_iso)
colnames(water_isotope_data_12)<-c("nla_unique_id","site_id","visit_no","e_i","rt_iso")
water_isotope_data_12$site_id<-as.character(water_isotope_data_12$site_id)
water_isotope_data_12$surveyyear<-2012
wid12<-left_join(water_isotope_data_12,link,by="nla_unique_id")

water_isotope_data_07<-filter(waterisotope07,unique_id_1 %in% water_isotope_data_17$nla_unique_id)%>%
  select(unique_id_1,site_id_4,visit_no_5,e_i,rt_iso)
colnames(water_isotope_data_07)<-c("nla_unique_id","site_id","visit_no","e_i","rt_iso")
water_isotope_data_07$surveyyear<-2007
wid07<-left_join(water_isotope_data_07,link,by="nla_unique_id")

#This file contains all the water isotope data for SuRGE sites
#with repeat rows for repeat visits and NAs for sites that don't
#have an NLA id (my count is 37 sites)
water_isotope_all_years<-bind_rows(wid07,wid12,water_isotope_data_17)
