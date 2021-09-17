# source scripts in order

source("scripts/masterLibrary.R") # Read in renv controlled library
source("scripts/setUserPath.R") # needed to allow consistent fixed file paths

# Analysis
source("scripts/analysis/readFieldSheets") # read in data from field sheets
#source("scripts/analysis/readLgr.R")
# source("scripts/analysis/readGps.R") # abondonded in favor of ArcGIS
