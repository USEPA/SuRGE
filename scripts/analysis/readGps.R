# SCRIPT TO READ AND PREVIEW FIELD GPS DATA

# 9/24/2020: CIN 202 field crew collected gps data on Bad Elf and Toughbook.
# Toughbook data comes as .shp, Bad Elf as .gpx.  Below reads in .gpx directly.
# I decided to convert .gpx. to .shp in ArcGIS Pro and add tracklog to existing maps
# I will revisit code below if I want to further analyze the data, otheriwse
# these data will not likely be used in R.



# READ GPS DATA -----------------
  # List of Bad Elf .GPX files
gpxFiles <- list.files("../../../data/", 
                       pattern=".gpx$", recursive = TRUE) # must end with .gpx for Bad Elf data


gpsList <- list()  # Empty list to hold results

for (i in 1:length(gpxFiles)) {  # loop to read and format each file
  {
    gps.i <- st_read(paste("../../../data/", 
                              gpxFiles[i], sep=""),
                     layer = "track_points") %>%
      mutate(lab = sub("\\/.*", "", gpxFiles[i]),
             lakeId = sub(".*[_]([^_]+)[_].*", "\\1", gpxFiles[i]))  # extract lakeId, doesn't always work) # assign data to particular field crew
  }
  
  gpsList[[i]] <- gps.i  # dump in list
}

# Merge files
gps <- do.call("rbind", gpsList)  # Coerces list into single sf object

plot(gps$geometry)

# Format data
#NEED TO THINK THROUGH TIME ZONE ISSUES
gps <- gps %>%  # no timezone on Bad Elf
  rename(RDateTime = time)

st_crs(gps) # 4326, which is WGS84.  Is this OK?  Probably fine.


