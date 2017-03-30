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
