# Packages needed
source("scripts/surgeDsn/surgeDsnMasterLibrary.R")




# READ SPATIAL DATA
# Generate data used for 1) example survey design from Neptune, and 2)
# my exploration of survey design options that fed into Tony Olsen's final
# design.
# NLA 2007 reservoirs, nla07
# NLA 2012 reservoirs, nla12 and nla12.sf.  used in neptuneDsnExample/dsgnNeptune.R
# NLA 2017 reservoirs, nla17
# NLA, ecoregion polygons
# lake morpho, morph.df
# nhdPlusV2, nhd.df and nhd.sf
# unique NLA lakes from 07-17, nla
# unique NLA merged with morph and NHD; dsnDat <- nla.nhd.morph
source("scripts/surgeDsn/surgeDsnReadSpatial.R") 


# Following few scripts are Neptune's tutorial, example design,
# and example analysis.
# Review weight adjustment tutorial
# scripts/neptuneDsnExample/weightsExample.R


# # Neptune GRTS Design
# # Probablistic subset of NLA2012 reservoirs
# source("scripts/neptuneDsnExample/dsgnNeptune.R")
# 
# 
# # Neptune analyze 'simulated results'
# source("scripts/neptuneDsnExample/analysisNeptune.R")



# FOLLOWING FILES WERE PREPARED DURING PREPARATION FOR SURVEY DESIGN WITH TONY OLSEN
source("scripts/sampleSizeCalculator.R") # relationship between n and mean/CI

# scripts/surgeDsn/surgeDesignConsiderations.rmd
# scripts/surgeDsn/surgeDesignPreliminaryReview.rmd
# scripts/surgeDsn/surgeDesignFinalReview.rmd
# scripts/surgeDsn/surgeOverview.rmd
# scripts/surgeDsn/region10_sites.rmd

# Maps of NLA 2012 sampled reservoir AND SuRGE main sites
source("scripts/surgeDsn/makeMap.R")


