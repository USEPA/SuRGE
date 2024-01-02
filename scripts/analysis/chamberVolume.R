# CALCULATE CHAMBER VOLUMES


# Calculate chamber volume based on relationship between water level
# and volume.
# CIN/R10/NAR/RTP used same chamber design. See projectDocuments/equipment/chamberDesign.xlsx.
# DOE: # see "...\SuRGE Survey of Reservoir Greenhouse gas Emissions - Documents\data\DOE\DOE-ChamberGradationsWorksheet.xlsx"
# ADA: see "...\SuRGE Survey of Reservoir Greenhouse gas Emissions - Documents\data\ADA\chamberVolume\Chamber Volume Ada.xlsx"



fld_sheet  <- fld_sheet %>% 
  # merge with lake.list to bring in lab attribute
  left_join(., 
        lake.list %>% 
          mutate(lake_id = as.character(lake_id)) %>% # numeric in lake.list, but character in fld_sheet
          select(lake_id, lab)) %>% # merge on lake_id, bring in lab attribute
  mutate(lab = case_when(grepl("lacustrine|riverine|transitional", lake_id) ~ "CIN", # these missouri river impoundments sampled by CIN
                         TRUE ~ lab)) %>%
  mutate(chm_vol_l = case_when(lab == "ADA" ~ (39.26 + (-5.0239 * rowMeans(select(., chamb_grad_a, chamb_grad_b), na.rm = TRUE))), # ADA formula
                               lab == "DOE" ~ (37.718 + (-4.9034 * rowMeans(select(., chamb_grad_a, chamb_grad_b), na.rm = TRUE))), # DOE formual
                               TRUE ~ (42.057 + (-0.2189 * rowMeans(select(., chamb_grad_a, chamb_grad_b), na.rm = TRUE))))) %>% # CIN/R10/NAR/RTP formula
  # NaN may cause errors? 
  mutate(chm_vol_l = if_else(is.na(chm_vol_l), NA_real_, chm_vol_l)) 


# take a peak at results
ggplot(fld_sheet, aes(chm_vol_l)) + geom_freqpoly() # all between ~20 and 40L, seems OK

# inspect instances of missing chamber volume estimates
fld_sheet %>% filter(is.na(chm_vol_l), # missing chamber volume 
                     !is.na(chamb_deply_time)) %>% # chamber deployment time present, therefore chamber deployed
  select(lab, lake_id, visit,  site_id, chm_vol_l, chamb_grad_a, chamb_grad_b) %>%
  print(n=Inf)