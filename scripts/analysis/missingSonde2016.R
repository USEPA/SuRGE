## Interpolate 2016 deep sonde data using Index Site

#First get a list of the site, lake combinations that have sonde data that aren't the index site

#index sites from 2016 found by using dat object... then manually recreate since it is out of 
#the workflow order
# ind2016<-dat %>%
#   filter(index_site == TRUE) %>%
#   filter(sample_year == 2016) %>%
#   select (lake_id, site_id, visit)

lake_id<-c(1001,1002,1003,1004,1005,1006,1007,1008,1009,1010,1011,1012,1013,1014,
             1015,1016,1017,1018,1019,1020,1021,1022,1023,1025,1026,1027,1028,1029,
             1030,1031,1032)
site_id<-c(4,4,6,3,7,3,5,12,4,7,16,4,8,2,15,2,1,2,4,1,3,9,1,4,10,7,7,4,7,3,16)
ind2016<-data.frame(lake_id,site_id)
ind2016$uid<-paste(ind2016$lake_id,ind2016$site_id)

#This is the dataframe to check interpolation method against
dat2016sondeni<-dat_2016 %>%
  mutate(uid=paste(lake_id,site_id))%>%
  filter(!is.na(deep_temp))%>%
  filter(!uid %in% ind2016$uid)%>%
  select(lake_id,site_id,uid,deep_temp,deep_chla_sonde,deep_sp_cond,deep_do_mg,deep_turb,deep_ph,site_depth)

#Get a list of the uids that have missing deep sonde data
dat_2016$uniqueid<-paste(dat_2016$lake_id,dat_2016$site_id)
dat_2016_missing_sonde<-dat_2016 %>%
  filter(site_depth>1)%>%
  filter(!uniqueid %in% dat2016sondeni) %>%
  filter(!uniqueid %in% ind2016)

#Create sonde flag fields & Replace deep sonde NA values with shallow 
# measurements for sites less than 1 meter deep

#Dissolved Oxygen
dat_2016$deep_do_mg_flags <- ifelse(
  dat_2016$site_depth <= 1 &
    is.na(dat_2016$deep_do_mg),
  "l",
  NA
)
dat_2016$deep_do_mg <- ifelse(
  dat_2016$site_depth <= 1 &
    is.na(dat_2016$deep_do_mg),
  dat_2016$shallow_do_mg,
  dat_2016$deep_do_mg
)

#Temperature
dat_2016$deep_temp_flags <- ifelse(
  dat_2016$site_depth <= 1 &
    is.na(dat_2016$deep_temp),
  "l",
  NA
)
dat_2016$deep_temp <- ifelse(
  dat_2016$site_depth <= 1 &
    is.na(dat_2016$deep_temp),
  dat_2016$shallow_temp,
  dat_2016$deep_temp
)

#Conductivity
dat_2016$deep_sp_cond_flags <- ifelse(
  dat_2016$site_depth <= 1 &
    is.na(dat_2016$deep_sp_cond),
  "l",
  NA
)

dat_2016$deep_sp_cond <- ifelse(
  dat_2016$site_depth <= 1 &
    is.na(dat_2016$deep_sp_cond),
  dat_2016$shallow_sp_cond,
  dat_2016$deep_sp_cond
)

#Chlorophyll a
dat_2016$deep_chla_sonde_flags <- ifelse(
  dat_2016$site_depth <= 1 &
    is.na(dat_2016$deep_chla_sonde),
  "l",
  NA
)

dat_2016$deep_chla_sonde <- ifelse(
  dat_2016$site_depth <= 1 &
    is.na(dat_2016$deep_chla_sonde),
  dat_2016$shallow_chla_sonde,
  dat_2016$deep_chla_sonde
)

#Turbidity
dat_2016$deep_turb_flags <- ifelse(
  dat_2016$site_depth <= 1 &
    is.na(dat_2016$deep_turb),
  "l",
  NA
)

dat_2016$deep_turb <- ifelse(
  dat_2016$site_depth <= 1 &
    is.na(dat_2016$deep_turb),
  dat_2016$shallow_turb,
  dat_2016$deep_turb
)

#pH
dat_2016$deep_ph_flags <- ifelse(
  dat_2016$site_depth <= 1 &
    is.na(dat_2016$deep_ph),
  "l",
  NA
)

dat_2016$deep_ph <- ifelse(
  dat_2016$site_depth <= 1 &
    is.na(dat_2016$deep_ph),
  dat_2016$shallow_ph,
  dat_2016$deep_ph
)

#Now use the index site profiles to interpolate deep sonde data for sites >1m deep

for (i in 1:31){
  
  pro<-filter(depth_profile_2016,lake_id == ind2016$lake_id[i])
  
  dattt<-filter(dat_2016_missing_sonde,lake_id==ind2016$lake_id[i])
  # dattt<-filter(datt,!uniqueid %in% (ind2016$uid))
  # dattt<-filter(dattt,!uniqueid %in% (dat2016sondeni))
  
  inttemp=NULL
  intdo=NULL
  intph=NULL
  intchl=NULL
  intturb=NULL
  intcond=NULL
  
  for(j in 1:nrow(dattt)){    
    
  inttemp[j] <- pro$temp[which.min(abs(dattt$site_depth[j] - pro$sample_depth))]
  intdo[j] <- pro$do[which.min(abs(dattt$site_depth[j] - pro$sample_depth))]
  intph[j] <- pro$ph[which.min(abs(dattt$site_depth[j] - pro$sample_depth))]
  intchl[j] <- pro$chla_sonde[which.min(abs(dattt$site_depth[j] - pro$sample_depth))]
  intturb[j] <- pro$turbidity[which.min(abs(dattt$site_depth[j] - pro$sample_depth))]
  intcond[j] <- pro$sp_cond[which.min(abs(dattt$site_depth[j] - pro$sample_depth))]
  
  dat_2016$deep_sp_cond[match(dattt$uniqueid[j], dat_2016$uniqueid)] <- intcond[j]
  dat_2016$deep_sp_cond_flags[match(dattt$uniqueid[j], dat_2016$uniqueid)] <- "l"
  dat_2016$deep_temp[match(dattt$uniqueid[j], dat_2016$uniqueid)] <- inttemp[j]
  dat_2016$deep_temp_flags[match(dattt$uniqueid[j], dat_2016$uniqueid)] <- "l"
  dat_2016$deep_turb[match(dattt$uniqueid[j], dat_2016$uniqueid)] <- intturb[j]
  dat_2016$deep_turb_flags[match(dattt$uniqueid[j], dat_2016$uniqueid)] <- "l"
  dat_2016$deep_ph[match(dattt$uniqueid[j], dat_2016$uniqueid)] <- intph[j]
  dat_2016$deep_ph_flags[match(dattt$uniqueid[j], dat_2016$uniqueid)] <- "l"
  dat_2016$deep_chla_sonde[match(dattt$uniqueid[j], dat_2016$uniqueid)] <- intchl[j]
  dat_2016$deep_chla_sonde_flags[match(dattt$uniqueid[j], dat_2016$uniqueid)] <- "l"
  dat_2016$deep_do_mg[match(dattt$uniqueid[j], dat_2016$uniqueid)] <- intdo[j]
  dat_2016$deep_do_mg_flags[match(dattt$uniqueid[j], dat_2016$uniqueid)] <- "l"

  }
}

#Now do a similar calculation for the measured sonde data that wasn't at the index site to
#see how close it is to the measured values

testdat<-filter(dat2016sondeni,site_depth>=1)
testuid<-testdat %>%
  group_by(lake_id) %>%
  summarise(length=nrow(site_id))

estsonde<-NULLestsondsite_ide<-NULL
estisonde<-NULL

for (i in 1:nrow(testuid)){
  
  pro<-filter(depth_profile_2016,lake_id == testuid$lake_id[i])
  
  datt<-filter(testdat,lake_id==testuid$lake_id[i])
  
  inttemp=NULL
  intdo=NULL
  intph=NULL
  intchl=NULL
  intturb=NULL
  intcond=NULL
  uid=NULL
  
  for(j in 1:nrow(datt)){    
    
    inttemp[j] <- pro$temp[which.min(abs(datt$site_depth[j] - pro$sample_depth))]
    intdo[j] <- pro$do[which.min(abs(datt$site_depth[j] - pro$sample_depth))]
    intph[j] <- pro$ph[which.min(abs(datt$site_depth[j] - pro$sample_depth))]
    intchl[j] <- pro$chla_sonde[which.min(abs(datt$site_depth[j] - pro$sample_depth))]
    intturb[j] <- pro$turbidity[which.min(abs(datt$site_depth[j] - pro$sample_depth))]
    intcond[j] <- pro$sp_cond[which.min(abs(datt$site_depth[j] - pro$sample_depth))]
    uid[j] <- datt$uid[j]
    
    estsonde<-data.frame(inttemp,intdo,intph,intchl,intturb,intcond,uid)
    
  }
  estisonde<-rbind(estisonde,estsonde)
  
}

#match for method check

mcheck<-left_join(estisonde,dat2016sondeni,by="uid")

plot(mcheck$inttemp~mcheck$deep_temp)
abline(a=0,b=1)

mcheck<-filter(mcheck,deep_temp>20)
a<-lm(mcheck$inttemp~mcheck$deep_temp)
summary(a)
plot(mcheck$intchl~mcheck$deep_chla_sonde)
