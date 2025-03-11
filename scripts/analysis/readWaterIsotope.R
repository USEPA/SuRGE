## Read In Renee Brook's Water Isotope Data and Match with Surge Links
## Last Updated 3/11/2025

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

#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
#-#- READ SURGE LAKES
# Read in the SuRGE sites from the updated eval_status spreadsheet.
surge_sites_nla <- read_xlsx(paste0(userPath, "surgeDsn/SuRGE_design_20191206_eval_status.xlsx"), na = "NA") %>%
  filter(`EvalStatus Code` == "S") %>% # only sampled
  janitor::clean_names() %>%
  dplyr::rename(lake_id = site_id) %>%
  mutate(lake_id = str_extract(lake_id, "(\\d+$)") %>% # extract numeric part of lake_id
           as.numeric()) %>% # convert lake_id to numeric
  rename(nla17_site_id = site_id_2) %>%
  rename(nla_unique_id = unique_id)%>%
  select(lake_id, sample_year, nla17_site_id,nla_unique_id)

#Read in the water isotope data that Renee sent as three separate files for the different years
#The NLA ID for a given year is different than the NLA ID across years, so pull 
#both out and name them for the lake link
waterisotope17<- read_xlsx(paste0(userPath, "data/siteDescriptors/NLA water isotope data-2007-2017.xlsx"),
                                       sheet = "NLA_17_water_isotopes",na="NA")%>%
                  janitor::clean_names()
waterisotope17$nla17_site_id<-waterisotope17$site_id

waterisotope12<-  read_xlsx(paste0(userPath, "data/siteDescriptors/NLA water isotope data-2007-2017.xlsx"),
                            sheet = "NLA_12_water_isotopes",na = "NA")%>%
                  janitor::clean_names()
waterisotope12$nla12_site_id<-waterisotope12$site_id
waterisotope12$nla_unique_id<-waterisotope12$unique_id

waterisotope07<- read_xlsx(paste0(userPath, "data/siteDescriptors/NLA water isotope data-2007-2017.xlsx"),
                           sheet = "NLA_07_water isotopes",na = "NA")%>%
                   janitor::clean_names()
waterisotope07$nla07_site_id<-waterisotope07$site_id_4
waterisotope07$nla_unique_id<-waterisotope07$unique_id_1

#Setup file with desired water isotope data from each of 3 years
water_isotope_data_17 <- left_join(surge_sites_nla, waterisotope17, by ="nla17_site_id")%>% 
  select(lake_id, nla17_site_id, nla_unique_id, visit_no, e_i, rt_iso)
colnames(water_isotope_data_17)<-c("lake_id","nlaXX_site_id","nla_unique_id","visit_no","e_i","rt_iso")
water_isotope_data_17$surveyyear<-2017

water_isotope_data_12<-left_join(surge_sites_nla, waterisotope12, by ="nla_unique_id")%>% 
  select(lake_id, nla12_site_id, nla_unique_id, visit_no, e_i, rt_iso)
colnames(water_isotope_data_12)<-c("lake_id","nlaXX_site_id","nla_unique_id","visit_no","e_i","rt_iso")
water_isotope_data_12$surveyyear<-2012

water_isotope_data_07<-left_join(surge_sites_nla, waterisotope07, by ="nla_unique_id")%>% 
  select(lake_id, nla07_site_id, nla_unique_id, visit_no_5, e_i, rt_iso)
colnames(water_isotope_data_07)<-c("lake_id","nlaXX_site_id","nla_unique_id","visit_no","e_i","rt_iso")
water_isotope_data_07$surveyyear<-2007

#Check how many visits to each site in 2017
#2017: Data for 111 sites, and repeat data for 18 sites
water_isotope_data_17_agg<-water_isotope_data_17 %>%
  filter(!is.na(e_i))%>%
  group_by(nlaXX_site_id)%>%
  summarise(n=n())
nrow(filter(water_isotope_data_17_agg,n=="2"))

#2012: Data from 81 sites, and repeat visits for 10 sites
water_isotope_data_12_agg <- water_isotope_data_12 %>%
  filter(!is.na(e_i))%>%
  group_by(nlaXX_site_id)%>%
  summarise(n=n())
nrow(filter(water_isotope_data_12_agg,n=="2"))

#2007: Data from 56 sites, and repeat visits for 7 sites
water_isotope_data_07_agg <- water_isotope_data_07 %>%
  filter(!is.na(e_i))%>%
  group_by(nlaXX_site_id)%>%
  summarise(n=n())
nrow(filter(water_isotope_data_07_agg,n=="2"))

#Create a long file with all the NLA data for Surge Sites across years

water_isotope_long<-rbind(water_isotope_data_07,water_isotope_data_12,water_isotope_data_17)
water_isotope_agg<- water_isotope_long %>%
  filter(!is.na(e_i))%>%
  group_by(lake_id)%>%
  summarise(nla_unique_id=nla_unique_id[1],
            E_I=mean(e_i,na.rm=TRUE),
            sdE_I=sd(e_i),
            Retention_Time=mean(rt_iso,na.rm=TRUE),
            sdRT=sd(rt_iso),
            rt_ei_repeat_visits=n()-1) %>%
  select(-nla_unique_id)%>% # get from SuRGE design file
  mutate(Retention_Time_Units="yrs")%>%
  mutate(e_i_units="dimensionless")
summary(water_isotope_agg)

#Fix lake_id to character so it will work with lake link
water_isotope_agg$lake_id<-as.character(water_isotope_agg$lake_id)

#look at total count of repeat visits
nrow(filter(water_isotope_agg,rt_ei_repeat_visits>="1"))
nrow(filter(water_isotope_agg,rt_ei_repeat_visits>="2")) 
nrow(filter(water_isotope_agg,rt_ei_repeat_visits>="3"))
nrow(filter(water_isotope_agg,rt_ei_repeat_visits>="4"))
nrow(filter(water_isotope_agg,rt_ei_repeat_visits>="5"))
nrow(filter(water_isotope_agg,rt_ei_repeat_visits>="6"))


#Create a threshold value for assigning "storage" vs. "run-of-river"
water_isotope_agg$E_I_type<-ifelse(water_isotope_agg$E_I<0.2,"run-of-river","storage")
nrow(filter(water_isotope_agg,E_I_type=="storage"))
nrow(filter(water_isotope_agg,E_I_type=="run-of-river"))

# Run through janitor to enforce SuRGE name conventions
water_isotope_agg <- janitor::clean_names(water_isotope_agg)
