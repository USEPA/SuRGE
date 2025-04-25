## Implementing reasonable criteria for sonde data
## April 24, 2025

#############################################################
#1. Adjust dat object for deep and shallow sonde measurements
############################################################

#Manually set pH data from lakes 1000, 207, 204, and 54 to NA
dat$deep_ph <- ifelse(dat$lake_id %in% c(1000, 207, 204, 54), NA, dat$deep_ph)
dat$shallow_ph <- ifelse(dat$lake_id %in% c(1000, 207, 204, 54), NA, dat$shallow_ph)

#Manually set turbidity data from lake 1000 and from anomalous reading over 1000 to zero
#Set readings < -1 to NA and small negative values to 0.1
dat$deep_turb <- ifelse(dat$lake_id == 1000, NA, dat$deep_turb)
dat$deep_turb <- ifelse(dat$deep_turb > 1000 , NA, dat$deep_turb)
dat$deep_turb <- ifelse(dat$deep_turb < -1, NA, dat$deep_turb)
dat$deep_turb <- ifelse(dat$deep_turb < 0, 0, dat$deep_turb)

dat$shallow_turb <- ifelse(dat$lake_id == 1000, NA, dat$shallow_turb)
dat$shallow_turb <- ifelse(dat$shallow_turb > 1000 , NA, dat$shallow_turb)
dat$shallow_turb <- ifelse(dat$shallow_turb < -1, NA, dat$shallow_turb)
dat$shallow_turb <- ifelse(dat$shallow_turb < 0, 0, dat$shallow_turb)

#Set very small conductivity values to NA
dat$shallow_sp_cond <- ifelse(dat$shallow_sp_cond < 1, NA, dat$shallow_sp_cond)
dat$deep_sp_cond <- ifelse(dat$deep_sp_cond < 1, NA, dat$deep_sp_cond)

#Set chlorophyll a values that are smaller than -1 to NA and small negative values to 0.1
dat$deep_chla_sonde <- ifelse(dat$deep_chla_sonde <- 1, NA, dat$deep_chla_sonde)
dat$deep_chla_sonde <- ifelse(dat$deep_chla_sonde 0, 0, dat$deep_chla_sonde)

dat$shallow_chla_sonde <- ifelse(dat$shallow_chla_sonde <- 1, NA, dat$shallow_chla_sonde)
dat$shallow_chla_sonde <- ifelse(dat$shallow_chla_sonde < 0, 0, dat$shallow_chla_sonde)

#Set phycocyanin values that are smaller than -1 to NA and small negative values to 0.1
dat$deep_phycocyanin_sonde <- ifelse(dat$deep_phycocyanin_sonde <- 1,
                                     NA,
                                     dat$deep_phycocyanin_sonde)
dat$deep_phycocyanin_sonde <- ifelse(dat$deep_phycocyanin_sonde < 0.1,
                                     0.1,
                                     dat$deep_phycocyanin_sonde)

dat$shallow_phycocyanin_sonde <- ifelse(dat$shallow_phycocyanin_sonde <-
                                          1,
                                        NA,
                                        dat$shallow_phycocyanin_sonde)
dat$shallow_phycocyanin_sonde <- ifelse(dat$shallow_phycocyanin_sonde < 0,
                                        0,
                                        dat$shallow_phycocyanin_sonde)

#############################################################
#2. Adjust depth profiles object for sonde criteria
############################################################

#Manually set pH data from lakes 1000, 207, 204, and 54 to NA
depth_profiles_all$ph <- ifelse(depth_profiles_all$lake_id %in% c(1000, 207, 204, 54),
                                NA,
                                depth_profiles_all$ph)

#Manually set turbidity data from lake 1000  to NA
# and from anomalous reading over 1000 to zero
#Set readings < -1 to NA and small negative values to 0.1
depth_profiles_all$turbidity <- ifelse(depth_profiles_all$lake_id == 1000,
                                       NA,
                                       depth_profiles_all$turbidity)
depth_profiles_all$turbidity <- ifelse(depth_profiles_all$turbidity > 1000,
                                       NA,
                                       depth_profiles_all$turbidity)
depth_profiles_all$turbidity <- ifelse(depth_profiles_all$turbidity < -1,
                                       NA,
                                       depth_profiles_all$turbidity)
depth_profiles_all$turbidity <- ifelse(depth_profiles_all$turbidity < 0,
                                       0,
                                       depth_profiles_all$turbidity)

#Set very small conductivity values to NA
depth_profiles_all$sp_cond <- ifelse(depth_profiles_all$sp_cond < 1, NA, depth_profiles_all$sp_cond)

#Set chlorophyll a values that are smaller than -1 to NA and small negative values to 0.1
depth_profiles_all$chla_sonde <- ifelse(depth_profiles_all$chla_sonde <-
                                          1,
                                        NA,
                                        depth_profiles_all$chla_sonde)
depth_profiles_all$chla_sonde <- ifelse(depth_profiles_all$chla_sonde <
                                          0,
                                        0,
                                        depth_profiles_all$chla_sonde)


#Set phycocyanin values that are smaller than -1 to NA and small negative values to 0.1
depth_profiles_all$phycocyanin_sonde <- ifelse(
  depth_profiles_all$phycocyanin_sonde <- 1,
  NA,
  depth_profiles_all$phycocyanin_sonde
)
depth_profiles_all$phycocyanin_sonde <- ifelse(
  depth_profiles_all$phycocyanin_sonde < 0,
  0,
  depth_profiles_all$phycocyanin_sonde
)
                                   
