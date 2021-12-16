# source scripts in order


source("scripts/masterLibrary.R") # Read in renv controlled library
source("scripts/setUserPath.R") # needed to allow consistent fixed file paths

# Analysis
source("scripts/readSurgeLakes.R") # read in survey design file
#source("scripts/analysis/readFieldSheets.R") # read in data from field sheets. under development
source("scripts/analysis/readChemCoc.R") # crosswalk between sample identifiers and lab ids.
# source("scripts/analysis/readChem.R") # under development


#source("scripts/analysis/readLgr.R")
# source("scripts/analysis/readGps.R") # abandoned in favor of ArcGIS

