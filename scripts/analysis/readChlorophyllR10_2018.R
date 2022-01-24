# Script for reading chlorophyll samples collected by Region 10 in 2018.
# Samples distributed between analysis at AWBERC and a contract lab

# R10 REM CHLOROPHYLL SAMPLES ANALYZED AT AWBERC-----------------------------  

# Chain of Custody forms can be found at: 
# "L:\Priv\Cin\NRMRL\ReservoirEbullitionStudy\ebullition2017\projectDocuments\coc"
# Samples from SFR, MMR, LGR, PLR, and LVR were extracted and analyzed at AWBERC.
# The results can be found at: "L:\Priv\Cin\NRMRL\Chlorophyll\Results\chl_sample_log.csv"
# A copy of this file was moved to: 
# "...Environmental Protection Agency (EPA)\Herger, Lillian - REM_2018\data\chlorophyll\chl_sample_log.csv"
# AND
# "...\SuRGE Survey of Reservoir Greenhouse gas Emissions - Documents\data\algalIndicators\pigments\R10_2018_chl\chl_sample_log.csv"

# Please see project QAPP for Sample_ID conventions:
# "...\SuRGE Survey of Reservoir Greenhouse gas Emissions - Documents\projectDocuments\
#     QAPP, SOPs, manuals, HASP\
#     QAPP for an assessment of methane emissions from Region 10 reservoirs.docx"

# Please see Wiki page https://github.com/USEPA/SuRGE/wiki/Region-10-2018-sampling
# for crosswalk between 2018 lake_id values and SuRGE lake_id values.  Only read in
# data for sites specified in Wiki.

# Column R ("Chla_a_JH") contains the ug of chl a on the filter.  This value
# must be divided by the volume filtered to calculate chla in ug_l.  Volume filtered
# can be found at: "...\SuRGE Survey of Reservoir Greenhouse gas Emissions - Documents\
#                  data\algalIndicators\pigments\surgeFilteredVolumes.xlsx"

# the chla_qual column should reflect holding time violations from time the filter
# was collected until it was extracted.  Filter hold time is in column x.  A 
# value <=60 is good, holding time >60 is a violation and should be given a chla_qual
# value of 1

# The chla_flag column will have a value of "<" for chla levels below the detection
# limit.  I am still trying to determine the DL for these samples [1/24/2022]





# SAMPLES ANALYZED AT BSA LABORATORIES-----------------------
# Samples collected from SFP, WPL, and BKL were extracted, but not run.  They were 
# collected and driven to BSA laboratory by Dana Macke in September 2020.  I 
# provided filtered volumes for these samples.
# [TRYING TO LOCATE THESE DATA]
