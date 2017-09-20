## Suggestion to make this more share-able below.
if(FALSE){
  library(readxl) # read excel files
  library(rgdal) # spatial data
  library(maptools) # spatial data
  library(mapproj) # spatial data
  library(ggplot2) #plotting
  library(micromap) # create_map_table function in place of fortify
  
  # Always load dplyr after plyr and relaimpo!  These packages mask
  # dplyr functions.
  library(plyr)  # for 'join' in ggplot plotting of shapefile
  library(dplyr)   # For data manipulation
  library(spsurvey) # grts design and analysis
}


## Another way to do this, in case people you share code with don't have 
## some of these packages installed
packageList <- c("readxl","rgdal","maptools","mapproj","ggplot2","micromap",
                 "plyr","dplyr","spsurvey")
# Check for packages you don't have yet
newPackages <- packageList[!(packageList %in% installed.packages()[,"Package"])]
# Install new packages
if(length(newPackages) > 0) install.packages(newPackages)
# Load all packages
for(i in 1:length(packageList)){
  library(package = packageList[i], character.only = TRUE)
}
