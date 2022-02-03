# source scripts in order

source("scripts/masterLibrary.R") # Read in renv controlled library
source("scripts/setUserPath.R") # needed to allow consistent fixed file paths

# Analysis
source("scripts/analysis/readSurgeLakes.R") # read in survey design file
source("scripts/analysis/chemSampleList.R") # creates chem.samples.foo, an inventory of all collected chem sample
#source("scripts/analysis/readFieldSheets.R") # read in data from field sheets. under development

# Read chemistry
source("scripts/analysis/readAnionsAda.R") # read ADA lab anions
source("scripts/analysis/readAnionsDaniels.R") # read Kit Daniels anions
source("scripts/analysis/readNutrientsAda.R") # read nutrients ran in ADA lab
source("scripts/analysis/readNutrientsAwberc.R") # read AWBERC lab nutrient results 
source("scripts/analysis/readNutrientsR10_2018.R") # read AWBERC nutrients for 2018 R10
source("scripts/analysis/readOcAda.R") # read ADA TOC/DOC data
source("scripts/analysis/readOcMasi.R") # read 2020 TOC run at MASI lab
source("scripts/analysis/readTteb.R") # TTEB metals, TOC, DOC
#source("scripts/analysis/readPigmentsMicrocystin.R") # NAR chl, phyco, and microcystin
#source("scripts/analysis/readTaxonomy.R") # GB taxonomy
source("scripts/analysis/readChlorophyllR10_2018.R") # 2018 R10 chlorophyll

# Aggregate and review chemistry
source("scripts/analysis/mergeChemistry.R")
# need a script to document blanks and percent agreement among dups
# need a script to strip out blanks and dups


# Diffusive emission rates
#source("scripts/analysis/readLgr.R")
#source("scripts/analysis/plotClean.R") # define deployment/retrieval times for chambers

# Random
#source("scripts/analysis/readGps.R") # inform how much of LGR time series to use per site? 
#source("scripts/analysis/chemSampleList2022.R") # estimate 2022 sample load
#source("scripts/analysis/aggregateNutrientLabDupExample.R") # example code for aggregating lab dups.  Can delete.


