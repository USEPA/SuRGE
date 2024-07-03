# source scripts in order

source("scripts/masterLibrary.R") # Read in renv controlled library
source("scripts/setUserPath.R") # needed to allow consistent fixed file paths

# Analysis
source("scripts/analysis/readSurgeLakes.R") # read in survey design file
source("scripts/analysis/chemSampleList.R") # creates chem.samples.foo, an inventory of all collected chem sample

# Read field sheets and merge sample dates
source("scripts/analysis/readFieldSheets.R") # read surgeData...xlsx.  fld_sheet, dg_sheet
source("scripts/analysis/chamberVolume.R") # read surgeData...xlsx.  fld_sheet, dg_sheet
source("scripts/analysis/sampleDates.R") # df of lake_id, visit, and sample_date 

# Read chemistry
source("scripts/analysis/readAnionsAda.R") # read ADA lab anions.  ada.anions
source("scripts/analysis/readAnionsDaniels.R") # read Kit Daniels anions. d.anions
source("scripts/analysis/readNutrientsAda.R") # read nutrients ran in ADA lab. ada.nutrients
source("scripts/analysis/readNutrientsAwberc.R") # read AWBERC lab nutrient results. chem21 
source("scripts/analysis/readNutrientsR10_2018.R") # read AWBERC nutrients for 2018 R10. chem18
source("scripts/analysis/readOcAda.R") # read ADA TOC/DOC data.  ada.oc
source("scripts/analysis/readOcMasi.R") # read 2020 TOC run at MASI lab. toc.masi
source("scripts/analysis/readTteb.R") # TTEB metals, TOC, DOC.  tteb.all
source("scripts/analysis/readPigmentsMicrocystin.R") # NAR chl, phyco, and microcystin. pigments
#source("scripts/analysis/readTaxonomy.R") # GB taxonomy
source("scripts/analysis/readChlorophyllR10_2018.R") # 2018 R10 chlorophyll. chl18
source("scripts/analysis/readGc.R") # gc_lakeid_agg

# Read other lake characteristics
source("scripts/analysis/readMorpho.R") # morpho
source("scripts/analysis/readNla17.R") # nla17_chem
source("scripts/analysis/hydroLakesID.R") # hylak_link
source("scripts/analysis/lagosLakesID.R") # lagos_links
source("scripts/analysis/nidLakesID.R") #
source("scripts/analysis/readWaterIsotope.R") #Renee Brooks Isotope/Residence Time data
source("scripts/analysis/readNWI.R") #NWI attributes from Mark Mitchell



# Aggregate and review chemistry
source("scripts/analysis/mergeChemistry.R") # merge all chem objects. chemistry_all
source("scripts/analysis/cincinnatiShippingNotes.R") # adding S flag
source("scripts/analysis/aggregateFieldDupsStripFieldBlanks.R") # strip out blanks and aggregate field duplicates.  chemistry
#source("scripts/analysis/surgeFieldDuplicatesAndFieldBlanks.Rmd") # document blanks and percent agreement among dups
#source("scripts/analysis/mergeGc.R") # not written yet.  Need to add dissolved gas to gc_lakeid_agg

# Merge chemistry and field sheets
source("scripts/analysis/mergeChemistryFieldSheets.R") # produces chem_fld.
# need to do something with dg_sheet

# Prep 2020 and 2021 data sets for RAPID reporting
#source("scripts/analysis/rapidReport.R")


# Diffusive emission rates
source("scripts/analysis/readLgr.R") # read raw LGR data
source("scripts/analysis/plotCleanLgr.R") # define deployment/retrieval times for chambers
source("scripts/analysis/calculateDiffusion.R") # diffusive emission rates.  

# Ebullition rates
source("scripts/analysis/ebullitionMassFluxFunction.R") # source function
source("scripts/analysis/readGc.R")
source("scripts/analysis/calculateEbullition.R") # eb_results

# Merge diffusive and ebulitive rates --> calculate total
source("scripts/analysis/calculateTotalEmissions.R")

# Merge emissions, chemistry, and field sheets
source("scripts/analysis/mergeChemEmissions.R") #all_obs


# Random
#source("scripts/analysis/readGps.R") # inform how much of LGR time series to use per site? 
#source("scripts/analysis/chemSampleList2022.R") # estimate 2022 sample load
#source("scripts/analysis/aggregateNutrientLabDupExample.R") # example code for aggregating lab dups.  Can delete.

# Project shapefiles
source("scripts/writeSuRGElakesToGpkg.R")


# Analysis
source("scripts/analysis/inspectMeasurementValues.R")
source("scripts/analysis/mergePredictorsPrepForStatistics.R") # dat


