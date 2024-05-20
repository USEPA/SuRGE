

# MERGE LAKE.LIST AND all_obs------------
# lake.list object is created with readSurgeLakes.R
dim(lake.list) # 342, 36
str(lake.list) # numeric lake_id
unique(lake.list$lake_id) # no lacustrine, riverine, or transitional


# all_obs is created with mergeChemEmissions.R
dim(all_obs) # 2058, 270
str(all_obs) # character lake_id
unique(all_obs$lake_id) # this has lacustrine, riverine, and transitional


# common names
names(lake.list)[names(lake.list) %in% names(all_obs)] # lake_id, eval_status, visit


# [5/6/2022] merge is ugly because lacustrine, riverine, and transitional
# sites haven't been aggregated into a single lake_id yet.  This merge is preliminary

chem_fld <- dplyr::left_join(chem_fld, 
                 lake.list %>% mutate(lake_id = as.character(lake_id)), 
                 by = "lake_id")
dim(chem_fld) #1127, 233 retained all chem_fld observations



# MERGE NLA CHEMISTRY AND CHEM_FLD--------
# All NLA data are an integrated epilimnion sample, whereas
# SuRGE distinguish between deep and shallow sites.

# nla17.chem created with readNla17.R
dim(nla17.chem) #1113, 47

# common names
names(nla17.chem)[names(nla17.chem) %in% names(chem_fld)] # nla_id

str(chem_fld$nla_id) #character
str(nla17.chem$nla_id) #character

chem_fld_nla <- left_join(chem_fld, nla17.chem)
dim(chem_fld_nla) #1127, 279


# COMPARE SURGE AND NLA DATA----------
ggplot(chem_fld_nla %>% filter(sample_depth == "shallow"), 
       aes(no2_3, nla_NITRATE_N*1000)) + 
  geom_point() +
  xlab("SuRGE NO3 (ug N/L)") +
  ylab("NLA NO3 (ug N /L)") +
  ylim(0,3000) +
  geom_abline(slope = 1, intercept = 0) +
  ggtitle("NLA vs Shallow SuRGE Nitrate")


ggplot(chem_fld_nla %>% filter(sample_depth == "shallow"), 
       aes(ph_s, nla_PH)) + 
  geom_point() +
  xlab("SuRGE pH") +
  ylab("NLA pH") +
  geom_abline(slope = 1, intercept = 0) +
  ggtitle("NLA vs Shallow SuRGE pH")


chem_fld_nla %>% select(nla_id, nla_PH, ph_s)
chem_fld_nla %>% filter(lake_id == "54") %>% select(nla_id)
