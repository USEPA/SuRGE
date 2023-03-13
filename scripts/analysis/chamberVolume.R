# CALCULATE CHAMBER VOLUMES

# CIN/R10/NAR/RTP
# Calculate chamber volume based on relationship between water level
# and volume.  See chamberDesign.xlsx in projectDocuments/equipment.
# NOT WORKING, IN PROGRESS
# fld_sheet <- mutate(fld_sheet, chmVol.L = (42.057 + (-0.2189 * mean(chamb_grad_a, chamb_grad_b, na.rm = TRUE))))

# JC 3/13/2023: I think this produces the desired outcome:
fld_sheet  <- fld_sheet %>% 
  mutate(chmVol.L = (42.057 + (-0.2189 * rowMeans(
  select(., chamb_grad_a, chamb_grad_b), na.rm = TRUE)))) %>%
  # NaN may cause errors? 
  mutate(chmVol.L = if_else(is.na(chmVol.L), NA_real_, chmVol.L))

# ADA 
#has a different design and will supply guidance to convert graduations to volume



# DOE
# see "...\SuRGE Survey of Reservoir Greenhouse gas Emissions - Documents\data\DOE\DOE-ChamberGradationsWorksheet.xlsx"