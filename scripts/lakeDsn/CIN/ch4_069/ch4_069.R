bolgDat <- readxl::read_xlsx(path = "../../../lakeDsn/CIN/CH4-069/Lake Oahe 2001 2002 sites.xlsx", 
                             sheet = "revised res eval June092006") %>%
  rename_with(~gsub(" ", "_", .), .cols = everything())

str(bolgDat)

bolgSf <- st_as_sf(bolgDat, coords = c("Long_dd", "Lat_dd"), crs = 4269)

bolgSf <- bolgSf %>% mutate(tempDiff = Surftemp_Y1 - Botttemp_Y1,
                            tempDiffCat = ifelse(tempDiff < 1,
                                                 "<1",
                                                 ifelse(tempDiff >1 & tempDiff<5,
                                                        ">1 & <5",
                                                        ifelse(tempDiff > 5,
                                                               ">5",
                                                               NA)))) %>%
  filter(!is.na(Year1_status),
         Year1_status == "S")
  
plot(bolgSf['TSS_Y1'])


# write out all sites
st_write(obj = bolgSf, 
         dsn = file.path( "../../../lakeDsn/CIN/CH4-069", "bolg069.gpkg"), 
         layer = "pointDat", # package appends 'main.' to layer name?
         append=FALSE, # this overwrites existing layer
         driver = "GPKG")
