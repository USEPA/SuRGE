#1.  Merge diffusive and ebullitive emissions
# either load diffusive emissions from disk, or run "calculateEmissions.R"
load("output/diffusiveOUT.RData") # loads "OUT" object.  
# This should really be OUT2. Load into calculateEmissions.R to generate OUT2

dim(OUT2) #1492 observations
dim(eb_results) #1713 observation
emissions <- full_join(eb_results, OUT2)
dim(emissions) #1849



# CALCULATE TOTAL EMISSION RATES------------------
# Only calculate if both diff and ebul were quantified
# tot = NA if is_na(ebul) or is_na(diff)_
emissions <- mutate(emissions, 
                     co2_trate_mg_h = co2_drate_mg_h_best + co2_erate_mg_h,
                     ch4_trate_mg_h = ch4_drate_mg_h_best + ch4_erate_mg_h)



# # CO2 EQUIVALENTS------------------
# source("ohio2016/scriptsAndRmd/co2Equiv.R")
# eqAreaData <- mutate(eqAreaData,
#                      ch4Co2eq = co2Equiv(eqAreaData$ch4.trate.mg.h, 
#                                          choice1 = "CH4"),
#                      co2Co2eq = eqAreaData$co2.trate.mg.h,
#                      totCo2eq = ch4Co2eq + co2Co2eq)