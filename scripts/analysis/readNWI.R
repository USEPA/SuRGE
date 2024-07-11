## Read in NWI attributes for SuRGE Lakes
## Script last updated on 7/3/2024

#Read in NWI data that Mark Mitchell sent at lake scale
#Starting with just going down to SubSystem (I'm unclear how to differentiate classes between systems with 
#the current data export format)
nwi_SuRGE<- read_xlsx(paste0(userPath, "data/siteDescriptors/nwi/SURGE_AllAttributes_NWI_20240124.xlsx"),
                           sheet = "AllAttribute",na="NA")%>%
  janitor::clean_names()%>%
  dplyr::rename(lake_id = site_id) %>%
  mutate(lake_id = str_extract(lake_id, "(\\d+$)") %>% # extract numeric part of lake_id
           as.numeric()) %>% # convert lake_id to numeric
  select(lake_id,lacustrine,palustrine,riverine,intermittent,limnetic,littoral,lower_perennial,unknown_perennial,
         upper_perennial,aquatic_bed_x,emergent_x,forested_x,rocky_shore,scrub_shrub_x,streambed,
         unconsolidated_bottom_x, unconsolidated_shore_x,aquatic_bed_y,emergent_y,
          forested_y,scrub_shrub_y,unconsolidated_bottom_y,unconsolidated_shore_y,broad_leaved_deciduous_y)

nwi_2016<- read_xlsx(paste0(userPath, "data/siteDescriptors/nwi/2016_survey_AllAttributes_NWI_20240124.xlsx"),
                     sheet = "2016Sites_AllAttribute",na="NA")%>%
  janitor::clean_names() %>%
  #Need to manually assign lake ids
  mutate(lake_id = c(1001:1014,1033,1015:1032))%>%
  mutate(unconsolidated_bottom_x=unconsolidated_bottom,unconsolidated_bottom_y=NA,
         rocky_shore=NA,aquatic_bed_x=aquatic_bed,aquatic_bed_y=NA,unconsolidated_shore_x=unconsolidated_shore,
         unconsolidated_shore_y=NA)%>%
  select(lake_id,lacustrine,palustrine,riverine,intermittent,limnetic,littoral,lower_perennial,unknown_perennial,
         upper_perennial,aquatic_bed_x,emergent_x,forested_x,rocky_shore,scrub_shrub_x,streambed,
         unconsolidated_bottom_x, unconsolidated_shore_x,aquatic_bed_y,emergent_y,
         forested_y,scrub_shrub_y,unconsolidated_bottom_y,unconsolidated_shore_y,broad_leaved_deciduous_y)
  


#Bind the two datasets together
#Recalculate percentages as fractions of total
nwi<-rbind(nwi_SuRGE,nwi_2016)%>%
  mutate(lacustrineper=lacustrine/(lacustrine+palustrine+riverine),
         palustrineper=palustrine/(lacustrine+palustrine+riverine),
         riverineper=riverine/(lacustrine+palustrine+riverine),
         limneticper=limnetic/(limnetic+littoral),
         littoralper=littoral/(limnetic+littoral),
         intermittentper=intermittent/(intermittent+lower_perennial+unknown_perennial+upper_perennial),
         lower_perennialper=lower_perennial/(intermittent+lower_perennial+unknown_perennial+upper_perennial),
         unknown_perennialper=unknown_perennial/(intermittent+lower_perennial+unknown_perennial+upper_perennial),
         upper_perennialper=upper_perennial/(intermittent+lower_perennial+unknown_perennial+upper_perennial))%>%
         #check if class specific data adds to less than 100
         rowwise()%>%
         mutate(totalclass=sum(aquatic_bed_x,emergent_x,forested_x,rocky_shore,scrub_shrub_x,
                               streambed,unconsolidated_bottom_x,unconsolidated_bottom_x,unconsolidated_shore_x,
                               aquatic_bed_y,emergent_y,forested_y,scrub_shrub_y,unconsolidated_bottom_y,
                               unconsolidated_shore_y,broad_leaved_deciduous_y,na.rm=TRUE))


#Now look at potential geographic bias for containing sub-system information

#link nwi to surge site info
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
#-#- READ SURGE LAKES
# Read in the SuRGE sites from the updated eval_status spreadsheet.
surge_sites_nwi <- read_xlsx(paste0(userPath, "surgeDsn/SuRGE_design_20191206_eval_status.xlsx"), na = "NA") %>%
  filter(`EvalStatus Code` == "S") %>% # only sampled
  janitor::clean_names() %>%
  dplyr::rename(lake_id = site_id) %>%
  mutate(lake_id = str_extract(lake_id, "(\\d+$)") %>% # extract numeric part of lake_id
           as.numeric()) %>% # convert lake_id to numeric
  select(lake_id, sample_year, pstl_code)

#Now join the data to the surge info on state location
#There is no one System type that exists in all the reservoir polygons
#Need to create a true/false for the existence of any sub-system info

nwi_geo<-left_join(nwi,surge_sites_nwi,by="lake_id")%>%
  mutate(subclass=ifelse(!is.na(intermittent)|!is.na(limnetic)|!is.na(littoral)|!is.na(lower_perennial)|
                            !is.na(unknown_perennial)|!is.na(upper_perennial),"TRUE","FALSE"))%>%
  mutate(class=ifelse(!is.na(aquatic_bed_x)|!is.na(emergent_x)|!is.na(forested_x)|
                              !is.na(rocky_shore)|!is.na(scrub_shrub_x)|!is.na(streambed)|
                              !is.na(unconsolidated_bottom_x)|!is.na(unconsolidated_shore_x)|
                              !is.na(aquatic_bed_y)|!is.na(emergent_y)|
                              !is.na(forested_y)|!is.na(scrub_shrub_y)|!is.na(unconsolidated_bottom_y)|
                              !is.na(unconsolidated_shore_y)|!is.na(broad_leaved_deciduous_y),"TRUE","FALSE"))

#Count how many are missing subclass info-- they are all palustrine, which doesn't have a subclass
nwi_geo_nosubclass<-filter(nwi_geo,subclass=="FALSE")


geo_bias<-nwi_geo%>%
  filter(!is.na(pstl_code))%>%
  ggplot(aes(y=class))+
  geom_bar()+
  facet_wrap(~pstl_code)
geo_bias
