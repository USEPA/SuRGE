# LINK HYDRO-LAKES ID WITH SURGE SITES BASED ON LATITUDE/LONGITUDE

# devtools::install_github("https://github.com/lawinslow/hydrolinks")
# # or install from hydrolinks archive can be found here:
# # https://cran.r-project.org/src/contrib/Archive/hydrolinks/
# 
# library(tidyverse)
# library(readxl)
# library(hydrolinks) # add to renv output

# read in SuRGE site information (lat/long required) and filter to sampled sites
surge_sites <- read_xlsx(paste0(userPath, "surgeDsn/SuRGE_design_20191206_eval_status.xlsx")) %>%
  filter(`EvalStatus Code` == "S")

# use lat/long to find associated hylak_id from hydrolinks package
# using a buffer of 20 m from shapefile boundaries
# NOTE: this piece of code will take  a little while to run
hydrolakes_surge <- link_to_waterbodies(surge_sites$LAT_DD83, 
                                       surge_sites$LON_DD83,
                                       surge_sites$siteID, 
                                       dataset = "hydrolakes", buffer = 20)

# hardcode in additional SuRGE sites (i.e., from map viewer)
surge_additions <- data.frame("siteID" = c("CH4-281", 
                                          "CH4-069", 
                                          "CH4-207",
                                          "CH4-098"),
                             "hylak_id" = c(1030644, 
                                            65, 
                                            1055540,
                                            112928))

# merge final dataset with hylak_id
surge_hylak_final <- surge_sites %>%
  full_join(hydrolakes_surge %>%
              full_join(surge_additions,
                        by = c("MATCH_ID" = "siteID", "hylak_id")), 
            by = c("siteID" = "MATCH_ID")) %>%
  dplyr::select(siteID, NHDPlusCOMID, hylak_id, lake_name, SampleYear, LAT_DD83, LON_DD83)

# # OPTIONAL:  output csv with SuRGE information and hylak_id
# write.csv(surge_hylak_final, row.names = F,
#           file = paste0(userPath, "SuRGEdsn/SuRGE_design_hylakID.csv"))