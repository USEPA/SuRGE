## 2016 data
# load 2016 data
load(paste0(userPath, "data/CIN/2016_survey/eqAreaData.RData")) # loads eqAreaData

dat.2016 <- eqAreaData # rename to dat.2016
remove(eqAreaData) # remove original object

dat.2016 %>% select(Lake_Name, siteID, chla.sample, pheo.sample, TN, TNH4, TNO2,)