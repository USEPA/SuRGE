## Read in phytoplankton community composition info for SuRGE Lakes
## Received from Avery Tatters on 8/1/2024 and reformatted excel file to get rid of empty rows
## Script last updated on 8/2/2024

#Read in NWI data that Mark Mitchell sent at lake scale
#going down to class after checking that class is consistently reported across lakes
phyto_SuRGE<- read.csv(paste0(userPath, "data/siteDescriptors/SuRGE 2021-23 phyto_from_Tatters.csv"))%>%
  janitor::clean_names()

phyto_SuRGE_s<-phyto_SuRGE %>%
  select(site_id,total_cyano,percent_cyano)

phyto_SuRGE_s$lake_id<-as.character(phyto_SuRGE_s$site_id)

#there are duplicates for nine sites including 147 and 148 which had two visits
#but also including some sites with no repeat visits 
#the dataset is not a complete list as some samples weren't fixed enough & no samples from 2020

phyto_SuRGE_link<- phyto_SuRGE_s %>%
  group_by(lake_id)%>%
  summarise(total_cyano=mean(total_cyano),percent_cyano=mean(percent_cyano))


