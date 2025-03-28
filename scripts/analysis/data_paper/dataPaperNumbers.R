# 2 hand picked site excluding Falls Lake and 2016 study
lake.list.all %>% 
  filter(site_type == "HAND", # hand picked
         !(lake_id %in% as.character(1001:1033))) # not in 2016 study

# 112 probability sites
lake.list.all %>%
  # deal with subsampled missouri river impoundments
  mutate(
    lake_id = case_when(lake_id %in% c("69_lacustrine", "69_riverine", "69_transitional") ~ "69",
                      lake_id %in% c("70_lacustrine", "70_riverine", "70_transitional") ~ "70",
                      TRUE ~ lake_id)) %>%
  distinct(lake_id, site_type) %>%
  filter(site_type == "PROB")


# 1b. Summarize entire GHG dataset for data paper

mes<-dat %>%
  filter(!is.na(ch4_diffusion_best))%>%
  mutate(ch4diff=ch4_diffusion_best*24)
#2150 individual measurements
mez<-mes %>%
  filter(ch4_diffusion_best==0)
#38 are zero values

cas<-dat %>%
  filter(!is.na(co2_diffusion_best))%>%
  mutate(co2diff=co2_diffusion_best*24)
#2083 individual measurements
caz<-cas %>%
  filter(co2_diffusion_best==0)
#264 are zero values