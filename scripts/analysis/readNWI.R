## Read in NWI attributes for SuRGE Lakes
## Script last updated on 6/28/2024

#Read in NWI data that Mark Mitchell sent at lake scale
nwi_SuRGE<- read_xlsx(paste0(userPath, "data/siteDescriptors/nwi/SURGE_AllAttributes_NWI_20240124.xlsx"),
                           sheet = "AllAttribute",na="NA")%>%
  janitor::clean_names()

nwi_2016<- read_xlsx(paste0(userPath, "data/siteDescriptors/nwi/2016_survey_AllAttributes_NWI_20240124.xlsx"),
                     sheet = "2016Sites_AllAttribute",na="NA")%>%
  janitor::clean_names()
