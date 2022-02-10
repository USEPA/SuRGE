# source scripts in order

source("scripts/masterLibrary.R") # Read in renv controlled library
source("scripts/setUserPath.R") # needed to allow consistent fixed file paths

# Analysis
source("scripts/analysis/readSurgeLakes.R") # read in survey design file
source("scripts/analysis/chemSampleList.R") # creates chem.samples.foo, an inventory of all collected chem sample


# Read field sheets and chemistry
source("scripts/analysis/readFieldSheets.R") # read surgeData...xlsx.  fld_sheet, dg_sheet
source("scripts/analysis/readAnionsAda.R") # read ADA lab anions.  ada.anions
source("scripts/analysis/readAnionsDaniels.R") # read Kit Daniels anions. d.anions
source("scripts/analysis/readNutrientsAda.R") # read nutrients ran in ADA lab. ada.nutrients
source("scripts/analysis/readNutrientsAwberc.R") # read AWBERC lab nutrient results. chem21 
source("scripts/analysis/readNutrientsR10_2018.R") # read AWBERC nutrients for 2018 R10. chem18
source("scripts/analysis/readOcAda.R") # read ADA TOC/DOC data.  ada.oc
source("scripts/analysis/readOcMasi.R") # read 2020 TOC run at MASI lab. toc.masi
source("scripts/analysis/readTteb.R") # TTEB metals, TOC, DOC.  tteb.all
#source("scripts/analysis/readPigmentsMicrocystin.R") # NAR chl, phyco, and microcystin
#source("scripts/analysis/readTaxonomy.R") # GB taxonomy
source("scripts/analysis/readChlorophyllR10_2018.R") # 2018 R10 chlorophyll. chl18


# Aggregate and review chemistry
source("scripts/analysis/surgeFieldDuplicatesAndFieldBlanks.Rmd") # document blanks and percent agreement among dups
source("scripts/analysis/mergeChemistry.R") # merge all chem objects
source("scripts/analysis/aggregateFieldDupsStripFieldBlanks.R") # strip out blanks and aggregate field duplicates

# Merge chemistry and field sheets
source("scripts/analysis/mergeChemistryFieldSheets.R") # produces chem_fld
# need to do something with dg_sheet

# Prep 2020 and 2021 data sets for RAPID reporting
source("scripts/analysis/rapidReport.R")


# Diffusive emission rates
#source("scripts/analysis/readLgr.R")
#source("scripts/analysis/plotClean.R") # define deployment/retrieval times for chambers

# Random
#source("scripts/analysis/readGps.R") # inform how much of LGR time series to use per site? 
#source("scripts/analysis/chemSampleList2022.R") # estimate 2022 sample load
#source("scripts/analysis/aggregateNutrientLabDupExample.R") # example code for aggregating lab dups.  Can delete.


