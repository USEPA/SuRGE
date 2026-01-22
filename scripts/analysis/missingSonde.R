# Fill in Missing Sonde Data by Interpolation
# January 2024


#Create a unique ID for each fld_sheet row to match on
fld_sheet$uniqueid <- paste(fld_sheet$lake_id, fld_sheet$site_id, fld_sheet$visit)

missing_bottom_temps <- fld_sheet %>%
  filter(site_depth > 1) %>%
  filter(is.na(temp_d)) %>%
  select(
    uniqueid,
    lake_id,
    site_id,
    visit,
    site_depth,
    do_mg_d,
    chla_sonde_d,
    ph_d,
    sp_cond_d,
    turb_d
  )
#22 missing deep sonde data overall


#Lake 207 is missing sonde data from most sites.
# Using the NLA17 profile to fill them
nla207 <- read_xlsx(
  paste0(
    userPath,
    "data/CIN/CH4_207_old mans lake/dataSheets/surgeDepthProfile207nla.xlsx"
  )
)
miss207 <- filter(missing_bottom_temps, lake_id == 207)

nlatemp <- NULL
nlado <- NULL
nlaph <- NULL
nlachl <- NULL
nlaturb <- NULL
nlacond <- NULL

#loop through to search and pull nearest water quality data from NLA profile
for (i in 1:12) {
  nlatemp[i] <- nla207$temp.C[which.min(abs(miss207$site_depth[i] - nla207$sample.depth.m))]
  nlado[i] <- nla207$do.mg.l[which.min(abs(miss207$site_depth[i] - nla207$sample.depth.m))]
  nlaph[i] <- nla207$pH[which.min(abs(miss207$site_depth[i] - nla207$sample.depth.m))]
  nlachl[i] <- nla207$chl.a.ug.l[which.min(abs(miss207$site_depth[i] - nla207$sample.depth.m))]
  nlaturb[i] <- nla207$turbidity.ntu[which.min(abs(miss207$site_depth[i] - nla207$sample.depth.m))]
  nlacond[i] <- nla207$sp.cond.us.cm[which.min(abs(miss207$site_depth[i] - nla207$sample.depth.m))]
}

miss207$nlatemp <- nlatemp
miss207$nlado <- nlado
miss207$nlaph <- nlaph
miss207$nlachl <- nlachl
miss207$nlaturb <- nlaturb
miss207$nlacond <- nlacond

#fill out missing sonde data from lake 207

for (i in 1:12) {
  fld_sheet$sp_cond_d[match(miss207$uniqueid[i], fld_sheet$uniqueid)] <- miss207$nlacond[i]
  fld_sheet$sp_cond_d_flags[match(miss207$uniqueid[i], fld_sheet$uniqueid)] <- "I"
  fld_sheet$temp_d[match(miss207$uniqueid[i], fld_sheet$uniqueid)] <- miss207$nlatemp[i]
  fld_sheet$temp_d_flags[match(miss207$uniqueid[i], fld_sheet$uniqueid)] <- "I"
  fld_sheet$turb_d[match(miss207$uniqueid[i], fld_sheet$uniqueid)] <- miss207$nlaturb[i]
  fld_sheet$turb_d_flags[match(miss207$uniqueid[i], fld_sheet$uniqueid)] <- "I"
  fld_sheet$ph_d[match(miss207$uniqueid[i], fld_sheet$uniqueid)] <- miss207$nlaph[i]
  fld_sheet$ph_d_flags[match(miss207$uniqueid[i], fld_sheet$uniqueid)] <- "I"
  fld_sheet$chla_sonde_d[match(miss207$uniqueid[i], fld_sheet$uniqueid)] <- miss207$nlachl[i]
  fld_sheet$chla_sonde_d_flags[match(miss207$uniqueid[i], fld_sheet$uniqueid)] <- "I"
  fld_sheet$do_mg_d[match(miss207$uniqueid[i], fld_sheet$uniqueid)] <- miss207$nlado[i]
  fld_sheet$do_mg_d_flags[match(miss207$uniqueid[i], fld_sheet$uniqueid)] <- "I"
  #and the shallow sites too
  fld_sheet$sp_cond_s[match(miss207$uniqueid[i],fld_sheet$uniqueid)] <-nla207$sp.cond.us.cm[1]
  fld_sheet$sp_cond_s_flags[match(miss207$uniqueid[i], fld_sheet$uniqueid)] <- "I"
  fld_sheet$temp_s[match(miss207$uniqueid[i],fld_sheet$uniqueid)] <-nla207$temp.C[1]
  fld_sheet$temp_s_flags[match(miss207$uniqueid[i], fld_sheet$uniqueid)] <- "I"
  fld_sheet$turb_s[match(miss207$uniqueid[i], fld_sheet$uniqueid)] <- nla207$turbidity.ntu[1]
  fld_sheet$turb_s_flags[match(miss207$uniqueid[i], fld_sheet$uniqueid)] <- "I"
  fld_sheet$ph_s[match(miss207$uniqueid[i], fld_sheet$uniqueid)] <- nla207$pH[1]
  fld_sheet$ph_s_flags[match(miss207$uniqueid[i], fld_sheet$uniqueid)] <- "I"
  fld_sheet$chla_sonde_s[match(miss207$uniqueid[i], fld_sheet$uniqueid)]<- nla207$chl.a.ug.l[1]
  fld_sheet$chla_sonde_s_flags[match(miss207$uniqueid[i], fld_sheet$uniqueid)] <- "I"
  fld_sheet$do_mg_s[match(miss207$uniqueid[i], fld_sheet$uniqueid)]<- nla207$do.mg.l[1]
  fld_sheet$do_mg_s_flags[match(miss207$uniqueid[i], fld_sheet$uniqueid)] <- "I"
}

#Now look at how many additional missing sonde data there are
missing_bottom_tem <- fld_sheet %>%
  filter(site_depth > 1) %>%
  filter(is.na(temp_d)) %>%
  select(lake_id, site_id, visit, site_depth) %>%
  mutate(region = str_extract(lake_id, "(?<=_).*")) %>%
  mutate(site_id = ifelse(!is.na(region), paste0(site_id, "_", region), site_id)) %>%
  mutate(lake_id_2 = str_extract(lake_id, "\\d+(?=\\_)")) %>%
  mutate(lake_id = ifelse(!is.na(region), lake_id_2, lake_id)) %>%
  mutate(uniqueid = paste(lake_id, site_id, visit))
#9 observations

#Pull out lake 70 because it isn't getting a good depth match
missing_bottom_temp <- missing_bottom_tem %>%
  filter(lake_id != "70")

#Load Geopackage
lakes <- st_read(
  paste0(
    userPath,
    "lakeDsn/all_lakes_2025-04-24.gpkg"),
    layer = "all_lakes",
    quiet = TRUE
  )

head(lakes)

points <- st_read(paste0(
  userPath,
  "lakeDsn/all_lakes_2025-04-24.gpkg"),
  layer = "points",
  quiet = TRUE
)

head(points)

#Look to see the depth of the closest sites to each missing point
sitematch <- NULL
depmatch <- NULL
uidmatch <- NULL

for (i in 1:7) {
  datmp <- points[points$lake_id == missing_bottom_temp$lake_id[i], ]
  datmp$distance <- st_distance(datmp)
  uniqueid <- paste(
    missing_bottom_temp$lake_id[i],
    missing_bottom_temp$site_id[i],
    missing_bottom_temp$visit[i]
  )
  datmp$uid <- paste(datmp$lake_id, datmp$site_id, datmp$visit)
  
  position = which(datmp$uid == uniqueid)
  
  a <- min(datmp$distance[, position][datmp$distance[, position] != min(datmp$distance[, position])])
  datmatch <- subset(datmp, datmp$distance[, position] == a)
  
  sitematch[i] <- datmatch$site_id
  uidmatch[i] <- datmatch$uid
  depmatch[i] <- datmatch$site_depth
  
}

missing_bottom_temp$sitematch <- sitematch
missing_bottom_temp$depmatch <- depmatch
missing_bottom_temp$uidmatch <- uidmatch

for (i in 1:7) {
  fld_sheet$sp_cond_d[match(missing_bottom_temp$uniqueid[i], fld_sheet$uniqueid)] <- fld_sheet$sp_cond_d[match(missing_bottom_temp$uidmatch[i], fld_sheet$uniqueid)]
  fld_sheet$sp_cond_d_flags[match(missing_bottom_temp$uniqueid[i], fld_sheet$uniqueid)] <- "I"
  fld_sheet$temp_d[match(missing_bottom_temp$uniqueid[i], fld_sheet$uniqueid)] <- fld_sheet$temp_d[match(missing_bottom_temp$uidmatch[i], fld_sheet$uniqueid)]
  fld_sheet$temp_d_flags[match(missing_bottom_temp$uniqueid[i], fld_sheet$uniqueid)] <- "I"
  fld_sheet$turb_d[match(missing_bottom_temp$uniqueid[i], fld_sheet$uniqueid)] <- fld_sheet$turb_d[match(missing_bottom_temp$uidmatch[i], fld_sheet$uniqueid)]
  fld_sheet$turb_d_flags[match(missing_bottom_temp$uniqueid[i], fld_sheet$uniqueid)] <- "I"
  fld_sheet$ph_d[match(missing_bottom_temp$uniqueid[i], fld_sheet$uniqueid)] <- fld_sheet$ph_d[match(missing_bottom_temp$uidmatch[i], fld_sheet$uniqueid)]
  fld_sheet$ph_d_flags[match(missing_bottom_temp$uniqueid[i], fld_sheet$uniqueid)] <- "I"
  fld_sheet$chla_sonde_d[match(missing_bottom_temp$uniqueid[i], fld_sheet$uniqueid)] <- fld_sheet$chla_sonde_d[match(missing_bottom_temp$uidmatch[i], fld_sheet$uniqueid)]
  fld_sheet$chla_sonde_d_flags[match(missing_bottom_temp$uniqueid[i], fld_sheet$uniqueid)] <- "I"
  fld_sheet$do_mg_d[match(missing_bottom_temp$uniqueid[i], fld_sheet$uniqueid)] <- fld_sheet$do_mg_d[match(missing_bottom_temp$uidmatch[i], fld_sheet$uniqueid)]
  fld_sheet$do_mg_d_flags[match(missing_bottom_temp$uniqueid[i], fld_sheet$uniqueid)] <- "I"
}

## Now interpolate the two missing sites for lake 70 by manually identifying analagous sites from same region

datmp <- points[points$lake_id == 70, ]
datmp$distance <- st_distance(datmp)


uniqueid <- c("70 2_lacustrine 1", "12_riverine 1")
sitematch <- c("11_lacustrine", "3_riverine")
depmatch <- c(2, 2.3)
uidmatch <- c("70 11_lacustrine 1", "70 3_riverine 1")

missing_70 <- data.frame(uniqueid, sitematch, depmatch, uidmatch)

for (i in 1:2) {
  fld_sheet$sp_cond_d[match(missing_70$uniqueid[i], fld_sheet$uniqueid)] <- fld_sheet$sp_cond_d[match(missing_70$uidmatch[i], fld_sheet$uniqueid)]
  fld_sheet$sp_cond_d_flags[match(missing_70$uniqueid[i], fld_sheet$uniqueid)] <- "I"
  fld_sheet$temp_d[match(missing_70$uniqueid[i], fld_sheet$uniqueid)] <- fld_sheet$temp_d[match(missing_70$uidmatch[i], fld_sheet$uniqueid)]
  fld_sheet$temp_d_flags[match(missing_70$uniqueid[i], fld_sheet$uniqueid)] <- "I"
  fld_sheet$turb_d[match(missing_70$uniqueid[i], fld_sheet$uniqueid)] <- fld_sheet$turb_d[match(missing_70$uidmatch[i], fld_sheet$uniqueid)]
  fld_sheet$turb_d_flags[match(missing_70$uniqueid[i], fld_sheet$uniqueid)] <- "I"
  fld_sheet$ph_d[match(missing_70$uniqueid[i], fld_sheet$uniqueid)] <- fld_sheet$ph_d[match(missing_70$uidmatch[i], fld_sheet$uniqueid)]
  fld_sheet$ph_d_flags[match(missing_70$uniqueid[i], fld_sheet$uniqueid)] <- "I"
  fld_sheet$chla_sonde_d[match(missing_70$uniqueid[i], fld_sheet$uniqueid)] <- fld_sheet$chla_sonde_d[match(missing_70$uidmatch[i], fld_sheet$uniqueid)]
  fld_sheet$chla_sonde_d_flags[match(missing_70$uniqueid[i], fld_sheet$uniqueid)] <- "I"
  fld_sheet$do_mg_d[match(missing_70$uniqueid[i], fld_sheet$uniqueid)] <- fld_sheet$do_mg_d[match(missing_70$uidmatch[i], fld_sheet$uniqueid)]
  fld_sheet$do_mg_d_flags[match(missing_70$uniqueid[i], fld_sheet$uniqueid)] <- "I"
}

#The index profile is also missing from Lake 71
#Try filling the profile out with deep and shallow measurements from other sites

willcrk<-fld_sheet %>%
  filter(lake_id=="71") %>%
  select(lake_id,site_id,site_depth,temp_s,temp_s_flags, do_mg_s,do_mg_s_flags,
         sp_cond_s,sp_cond_s_flags,ph_s,ph_s_flags,chla_sonde_s,chla_sonde_s_flags,
         turb_s,turb_s_flags, temp_d,temp_d_flags, do_mg_d,do_mg_d_flags,
         sp_cond_d,sp_cond_d_flags,ph_d,ph_d_flags, chla_sonde_d,chla_sonde_d,turb_d,turb_d_flags)%>%
  mutate(visit=1)

#Create a dataframe to merge with the other depth profile data in the readDepthProfile script
#Site 11 is the index site
surgeDepthProfile71<-willcrk%>%
  select(lake_id,site_id,visit,site_depth,temp_d, do_mg_d,
         sp_cond_d,ph_d, chla_sonde_d,turb_d)%>%
  mutate(sample_depth_m= ifelse(site_depth>4,site_depth-1,site_depth-0.5))%>%
           select(-site_depth)
colnames(surgeDepthProfile71)<-c("lake_id","site_id","visit","temp","do","sp_cond","ph",
                                 "chla_sonde","turbidity","sample_depth")
  
surgeDepthProfile71s<-willcrk%>%
  filter(site_id==11)%>%
  select(lake_id,site_id,visit,temp_s,do_mg_s,
         sp_cond_s,ph_s,chla_sonde_s,turb_s,site_depth)%>%
  mutate(sample_depth=0.1)%>%
  select(-site_depth)
colnames(surgeDepthProfile71s)<-c("lake_id","site_id","visit","temp","do","sp_cond","ph",
                                  "chla_sonde","turbidity","sample_depth")

surgeDepthProfile71<-rbind(surgeDepthProfile71,surgeDepthProfile71s)%>%
  mutate(do_flag="I",sp_cond_flag="I",ph_flag="I",chla_sonde_flag="I",
         turbidity_flag="I")%>%
  # create units column for each analyte
  mutate(across(c("sample_depth", "temp", "do", "sp_cond", "turbidity","chla_sonde"), 
                ~ case_when(
                  str_detect(paste(cur_column()), "depth") ~ "m",
                  str_detect(paste(cur_column()), "temp") ~ "c",
                  str_detect(paste(cur_column()), "do") ~ "mg_l",
                  str_detect(paste(cur_column()), "sp_cond") ~ "us_cm",
                  str_detect(paste(cur_column()), "turb") ~ "ntu",
                  str_detect(paste(cur_column()), "chl") ~ "ug_l",
                  TRUE ~ "FLY UOU FOOLS"), # 
                .names = "{col}_units"))%>%
  #filter out repeat depths to avoid error in the thermo.depth calculation
  filter(site_id !="10")%>%
  filter(site_id != "5")%>%
  mutate(sp_cond_comment="")
surgeDepthProfile71$sp_cond_comment[1]="Modified datasheet from 0.493 to 493"

#Replace deep sonde NA values with shallow measurements for sites less than 1 meter deep & flag those values

#Dissolved Oxygen
fld_sheet$do_mg_d_flags <- ifelse(fld_sheet$site_depth <= 1 &
                                    is.na(fld_sheet$do_mg_d),
                                  "I",
                                  fld_sheet$do_mg_d_flags)
fld_sheet$do_mg_d <- ifelse(
  fld_sheet$site_depth <= 1 &
    is.na(fld_sheet$do_mg_d),
  fld_sheet$do_mg_s,
  fld_sheet$do_mg_s
)
#Temperature
fld_sheet$temp_d_flags <- ifelse(fld_sheet$site_depth <= 1 &
                                   is.na(fld_sheet$temp_d),
                                 "I",
                                 fld_sheet$temp_d_flags)
fld_sheet$temp_d <- ifelse(
  fld_sheet$site_depth <= 1 &
    is.na(fld_sheet$temp_d),
  fld_sheet$temp_s,
  fld_sheet$temp_d
)
#Conductivity
fld_sheet$sp_cond_d_flags <- ifelse(fld_sheet$site_depth <= 1 &
                                      is.na(fld_sheet$sp_cond_d),
                                    "I",
                                    fld_sheet$sp_cond_d_flags)
fld_sheet$sp_cond_d <- ifelse(
  fld_sheet$site_depth <= 1 &
    is.na(fld_sheet$sp_cond_d),
  fld_sheet$sp_cond_s,
  fld_sheet$sp_cond_d
)
#Chlorophyll a
fld_sheet$chla_sonde_d_flags <- ifelse(
  fld_sheet$site_depth <= 1 &
    is.na(fld_sheet$chla_sonde_d),
  "I",
  fld_sheet$chla_sonde_d_flags
)
fld_sheet$chla_sonde_d <- ifelse(
  fld_sheet$site_depth <= 1 &
    is.na(fld_sheet$chla_sonde_d),
  fld_sheet$chla_sonde_s,
  fld_sheet$chla_sonde_d
)
#Turbidity
fld_sheet$turb_d_flags <- ifelse(fld_sheet$site_depth <= 1 &
                                   is.na(fld_sheet$turb_d),
                                 "I",
                                 fld_sheet$turb_d_flags)
fld_sheet$turb_d <- ifelse(
  fld_sheet$site_depth <= 1 &
    is.na(fld_sheet$turb_d),
  fld_sheet$turb_s,
  fld_sheet$turb_d
)
#pH
fld_sheet$ph_d_flags <- ifelse(fld_sheet$site_depth <= 1 &
                                 is.na(fld_sheet$ph_d),
                               "I",
                               fld_sheet$ph_d_flags)
fld_sheet$ph_d <- ifelse(fld_sheet$site_depth <= 1 &
                           is.na(fld_sheet$ph_d),
                         fld_sheet$ph_s,
                         fld_sheet$ph_d)

#Remove Unique ID from fld_sheet Object
fld_sheet <- subset(fld_sheet, select = -uniqueid)
