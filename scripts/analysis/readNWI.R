## Read in NWI attributes for SuRGE Lakes
## Script last updated on 7/25/2024

#Read in NWI data that Mark Mitchell sent at lake scale
#going down to class after checking that class is consistently reported across lakes
nwi_SuRGE<- read_xlsx(paste0(userPath, "data/siteDescriptors/nwi/SURGE_AllAttributes_NWI_20240124.xlsx"),
                           sheet = "AllAttribute",na="NA")%>%
  janitor::clean_names()%>%
  dplyr::rename(lake_id = site_id) %>%
  mutate(lake_id = str_extract(lake_id, "(\\d+$)") %>% # extract numeric part of lake_id
           as.numeric()) %>% # convert lake_id to numeric
  select(lake_id,lacustrine,palustrine,riverine,intermittent,limnetic,littoral,lower_perennial,unknown_perennial,
         upper_perennial,aquatic_bed_x,emergent_x,forested_x,rocky_shore,streambed,scrub_shrub_x,streambed,
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
         upper_perennial,aquatic_bed_x,emergent_x,forested_x,rocky_shore,streambed,scrub_shrub_x,streambed,
         unconsolidated_bottom_x, unconsolidated_shore_x,aquatic_bed_y,emergent_y,
         forested_y,scrub_shrub_y,unconsolidated_bottom_y,unconsolidated_shore_y,broad_leaved_deciduous_y)
  
#Bind the two datasets together
#Recalculate percentages as fractions of total
nwi<-rbind(nwi_SuRGE,nwi_2016)%>%
  mutate(totper=rowSums(across(c(lacustrine,palustrine,riverine)),na.rm=TRUE),
         lacustrineper=ifelse(is.na(lacustrine),0,lacustrine/totper),
         palustrineper=ifelse(is.na(palustrine),0,palustrine/totper),
         riverineper=ifelse(is.na(riverine),0,riverine/totper),
         limneticper=ifelse(is.na(limnetic),0,limnetic/totper),
         littoralper=ifelse(is.na(littoral),0,littoral/totper),
         intermittentper=ifelse(is.na(intermittent),0,intermittent/totper),
         lower_perennialper=ifelse(is.na(lower_perennial),0,lower_perennial/totper),
         unknown_perennialper=ifelse(is.na(unknown_perennial),0,unknown_perennial/totper),
         upper_perennialper=ifelse(is.na(upper_perennial),0,upper_perennial/totper),
         aquatic_b=rowSums(across(c(aquatic_bed_x,aquatic_bed_y)),na.rm=TRUE),
         aquatic_bed=ifelse(is.na(aquatic_b),0,100*(aquatic_b/totper)),
         emer=rowSums(across(c(emergent_x,emergent_y)),na.rm=TRUE),
         emergent=ifelse(is.na(emer),0,100*(emer/totper)),
         #forested=
         #scrub_shrub=
         #unconsolidated_bottom=
         #unconsolidated_shore=
         rocky_shore=ifelse(is.na(rocky_shore),0,rocky_shore/totper),
         #need to check on how to tease apart the riverine class from the lacustrine class data here
         #rusty also suggested including all intermittent riverine
         riverine_unconsolidated_shore=ifelse(is.na(unconsolidated_shore_y),0,100*(unconsolidated_shore_y/totper)))
         #This was an attempt at getting at shallow habitat, but we are waiting for reclassification from Mark to do better with this
         #shallow=100*(palustrineper+littoralper+riverine_unconsolidated_shore))

nwi_link <- nwi %>%
  select(lake_id, lacustrineper, palustrineper, riverineper, limneticper, littoralper, intermittentper, lower_perennialper,
         upper_perennialper, emergent, aquatic_bed) 

colnames(nwi_link)<-c("lake_id", "lacustrine", "palustrine","riverine", "limnetic", "littoral", "intermittent", "lower_perennial",
                      "upper_perennial", "emergent", "aquatic_bed")

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

#Now look at potential geographic bias for containing sub-system information
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
