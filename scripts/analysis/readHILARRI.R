### Extract links for HILARRI dataset
### August 6, 2024
#must run "nidLakesID.R first
#must also have hylak_link

load(paste0(userPath, "data/siteDescriptors/HILARRI_v1_1/HILARRI_v1_1_Public.Rdata"))
  
#Read in SuRGE sites to link with HILARRI IDs

surge_sites_hilarri <- read_xlsx(paste0(userPath, "surgeDsn/SuRGE_design_20191206_eval_status.xlsx"), na = "NA") %>%
    filter(`EvalStatus Code` == "S") %>% # only sampled
    janitor::clean_names() %>%
    dplyr::rename(lake_id = site_id) %>%
    mutate(lake_id = str_extract(lake_id, "(\\d+$)") %>% # extract numeric part of lake_id
             as.character())%>%
  select(lake_id,gnis_name,pstl_code,xcoord_1,ycoord_1)

#get the name of the lake & coordinates linked to the nid/grand IDs for doublechecking links
# surge_hilarri_link<-left_join(surge_sites_hilarri,nid_link,by="lake_id")
# grandid<-hylak_link %>%
#   mutate(lake_id=as.character(lake_id))%>%
#   select(lake_id,grand_id)
# 
# surge_hilarri_grand_link<-left_join(surge_hilarri_link,grandid,by="lake_id")

nidh<-subset(hilarripublic, hilarripublic$nidid %in% nid_link$nid_i)
nidhn<-filter(nidh,!is.na(nidid))
grandh<-subset(hilarripublic,hilarripublic$grand_id %in% hylak_link$grand_id)
grandhn<-filter(grandh,!is.na(grand_id))

hilarri_link<-bind_rows(nidhn,grandhn)
hilarri_link<-unique(hilarri_link)
