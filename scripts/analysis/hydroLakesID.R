# LINK HYDRO-LAKES ID WITH SURGE SITES BASED ON LATITUDE/LONGITUDE

# devtools::install_github("https://github.com/lawinslow/hydrolinks")
# # or install from hydrolinks archive can be found here:
# # https://cran.r-project.org/src/contrib/Archive/hydrolinks/
# 
# library(tidyverse)
# library(readxl)
# library(hydrolinks) # add to renv output

#  # read in SuRGE site information (lat/long required) and filter to sampled sites
# surge_sites <- read_xlsx(paste0(userPath, "surgeDsn/SuRGE_design_20191206_eval_status.xlsx")) %>%
#   filter(`EvalStatus Code` == "S") %>%
#   janitor::clean_names() %>%
#   dplyr::rename(lake_id = site_id) %>%
#   mutate(lake_id = str_extract(lake_id, "(\\d+$)") %>% # extract numeric part of lake_id
#            as.numeric()) # convert lake_id to numeric
# 
# dim(surge_sites) # 147 sites
# # 
# # # use lat/long to find associated hylak_id from hydrolinks package
# # # using a buffer of 20 m from shapefile boundaries
# # # NOTE: this piece of code will take  a little while to run
# hydrolakes_surge <- link_to_waterbodies(surge_sites$lat_dd83,
#                                        surge_sites$lon_dd83,
#                                        surge_sites$lake_id,
#                                        dataset = "hydrolakes", buffer = 20)
# 
# dim(hydrolakes_surge) # matched 123 SuRGE sites
# 
# # hardcode in additional SuRGE sites (i.e., from map viewer)
# surge_manual <- data.frame("lake_id" = c(281,
#                                          69,
#                                          207,
#                                          98),
#                            "hylak_id" = c(1030644,
#                                           65,
#                                           1055540,
#                                           112928))
# 
# surge_additions = get_shape_by_id(match_id = surge_manual$hylak_id, 
#                                   feature_type = "waterbody", dataset = "hydrolakes") %>%
#   as.data.frame() %>%
#   select(-geometry) %>%
#   mutate(MATCH_ID = surge_manual$lake_id)
# 
# 
# # merge final dataset with hylak_id
# hylak_link <- full_join(hydrolakes_surge, surge_additions) %>%
#   dplyr::select(-contains("pour"), -centroid_x, -lake_name, -country, - continent, -poly_src) %>%
#   clean_names() %>%
#   rename(lake_id = match_id) %>%
#   relocate(lake_id, hylak_id, grand_id, lake_type) %>%
#   rename_with(.cols = -c(lake_id, hylak_id, grand_id), ~paste0("hylak_", .)) %>%
#   mutate(grand_id = case_when(grand_id == 0 ~ NA,
#                               TRUE ~ grand_id))
# 
# dim(hylak_link) #127 matches
# 
# 
# # # OPTIONAL:  output csv with SuRGE information and hylak_id
# write.csv(hylak_link, row.names = F,
#           file = paste0(userPath, "data/siteDescriptors/hylak_link.csv"))

hylak_link <- read.csv(file = paste0(userPath, "data/siteDescriptors/hylak_link.csv"))