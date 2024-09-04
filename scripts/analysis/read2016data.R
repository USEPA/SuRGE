## 2016 data
# load 2016 data
load(paste0(userPath, "data/CIN/2016_survey/eqAreaData.RData")) # loads eqAreaData

dat_2016 <- eqAreaData # rename to dat_2016
remove(eqAreaData) # remove original object

dat_2016 %>% select(Lake_Name, siteID, chla.sample, pheo.sample, TN, TNH4, TNO2,)

# site depth wasn't measured in first few lakes (oops) but we have bathymetry data
# for those lakes.