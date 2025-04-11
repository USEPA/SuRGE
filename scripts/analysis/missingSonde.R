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
#21 missing deep sonde data overall


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
  fld_sheet$sp_cond_d_flags[match(miss207$uniqueid[i], fld_sheet$uniqueid)] <- "l"
  fld_sheet$temp_d[match(miss207$uniqueid[i], fld_sheet$uniqueid)] <- miss207$nlatemp[i]
  fld_sheet$temp_d_flags[match(miss207$uniqueid[i], fld_sheet$uniqueid)] <- "l"
  fld_sheet$turb_d[match(miss207$uniqueid[i], fld_sheet$uniqueid)] <- miss207$nlaturb[i]
  fld_sheet$turb_d_flags[match(miss207$uniqueid[i], fld_sheet$uniqueid)] <- "l"
  fld_sheet$ph_d[match(miss207$uniqueid[i], fld_sheet$uniqueid)] <- miss207$nlaph[i]
  fld_sheet$ph_d_flags[match(miss207$uniqueid[i], fld_sheet$uniqueid)] <- "l"
  fld_sheet$chla_sonde_d[match(miss207$uniqueid[i], fld_sheet$uniqueid)] <- miss207$nlachl[i]
  fld_sheet$chla_sonde_d_flags[match(miss207$uniqueid[i], fld_sheet$uniqueid)] <- "l"
  fld_sheet$do_mg_d[match(miss207$uniqueid[i], fld_sheet$uniqueid)] <- miss207$nlado[i]
  fld_sheet$do_mg_d_flags[match(miss207$uniqueid[i], fld_sheet$uniqueid)] <- "l"
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
    "lakeDsn/all_lakes_2025-01-16.gpkg"),
    layer = "all_lakes",
    quiet = TRUE
  )

head(lakes)

points <- st_read(paste0(
  userPath,
  "lakeDsn/all_lakes_2025-01-16.gpkg"),
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
  fld_sheet$sp_cond_d_flags[match(missing_bottom_temp$uniqueid[i], fld_sheet$uniqueid)] <- "l"
  fld_sheet$temp_d[match(missing_bottom_temp$uniqueid[i], fld_sheet$uniqueid)] <- fld_sheet$temp_d[match(missing_bottom_temp$uidmatch[i], fld_sheet$uniqueid)]
  fld_sheet$temp_d_flags[match(missing_bottom_temp$uniqueid[i], fld_sheet$uniqueid)] <- "l"
  fld_sheet$turb_d[match(missing_bottom_temp$uniqueid[i], fld_sheet$uniqueid)] <- fld_sheet$turb_d[match(missing_bottom_temp$uidmatch[i], fld_sheet$uniqueid)]
  fld_sheet$turb_d_flags[match(missing_bottom_temp$uniqueid[i], fld_sheet$uniqueid)] <- "l"
  fld_sheet$ph_d[match(missing_bottom_temp$uniqueid[i], fld_sheet$uniqueid)] <- fld_sheet$ph_d[match(missing_bottom_temp$uidmatch[i], fld_sheet$uniqueid)]
  fld_sheet$ph_d_flags[match(missing_bottom_temp$uniqueid[i], fld_sheet$uniqueid)] <- "l"
  fld_sheet$chla_sonde_d[match(missing_bottom_temp$uniqueid[i], fld_sheet$uniqueid)] <- fld_sheet$chla_sonde_d[match(missing_bottom_temp$uidmatch[i], fld_sheet$uniqueid)]
  fld_sheet$chla_sonde_d_flags[match(missing_bottom_temp$uniqueid[i], fld_sheet$uniqueid)] <- "l"
  fld_sheet$do_mg_d[match(missing_bottom_temp$uniqueid[i], fld_sheet$uniqueid)] <- fld_sheet$do_mg_d[match(missing_bottom_temp$uidmatch[i], fld_sheet$uniqueid)]
  fld_sheet$do_mg_d_flags[match(missing_bottom_temp$uniqueid[i], fld_sheet$uniqueid)] <- "l"
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
  fld_sheet$sp_cond_d_flags[match(missing_70$uniqueid[i], fld_sheet$uniqueid)] <- "l"
  fld_sheet$temp_d[match(missing_70$uniqueid[i], fld_sheet$uniqueid)] <- fld_sheet$temp_d[match(missing_70$uidmatch[i], fld_sheet$uniqueid)]
  fld_sheet$temp_d_flags[match(missing_70$uniqueid[i], fld_sheet$uniqueid)] <- "l"
  fld_sheet$turb_d[match(missing_70$uniqueid[i], fld_sheet$uniqueid)] <- fld_sheet$turb_d[match(missing_70$uidmatch[i], fld_sheet$uniqueid)]
  fld_sheet$turb_d_flags[match(missing_70$uniqueid[i], fld_sheet$uniqueid)] <- "l"
  fld_sheet$ph_d[match(missing_70$uniqueid[i], fld_sheet$uniqueid)] <- fld_sheet$ph_d[match(missing_70$uidmatch[i], fld_sheet$uniqueid)]
  fld_sheet$ph_d_flags[match(missing_70$uniqueid[i], fld_sheet$uniqueid)] <- "l"
  fld_sheet$chla_sonde_d[match(missing_70$uniqueid[i], fld_sheet$uniqueid)] <- fld_sheet$chla_sonde_d[match(missing_70$uidmatch[i], fld_sheet$uniqueid)]
  fld_sheet$chla_sonde_d_flags[match(missing_70$uniqueid[i], fld_sheet$uniqueid)] <- "l"
  fld_sheet$do_mg_d[match(missing_70$uniqueid[i], fld_sheet$uniqueid)] <- fld_sheet$do_mg_d[match(missing_70$uidmatch[i], fld_sheet$uniqueid)]
  fld_sheet$do_mg_d_flags[match(missing_70$uniqueid[i], fld_sheet$uniqueid)] <- "l"
}

#Replace deep sonde NA values with shallow measurements for sites less than 1 meter deep & flag those values

#Dissolved Oxygen
fld_sheet$do_mg_d_flags <- ifelse(fld_sheet$site_depth <= 1 &
                                    is.na(fld_sheet$do_mg_d),
                                  "l",
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
                                 "l",
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
                                    "l",
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
  "l",
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
                                 "l",
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
                               "l",
                               fld_sheet$ph_d_flags)
fld_sheet$ph_d <- ifelse(fld_sheet$site_depth <= 1 &
                           is.na(fld_sheet$ph_d),
                         fld_sheet$ph_s,
                         fld_sheet$ph_d)

#Remove Unique ID from fld_sheet Object
fld_sheet <- subset(fld_sheet, select = -uniqueid)
