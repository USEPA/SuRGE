

# Bring in chemistry data objects----

localName <- "Joe/" # R proj folder at SP
# localName <- "Jake/" # R proj folder at SP

source(paste0(userPath, "rProjects/", localName, "SuRGE/scripts/analysis/readAnionsAda.R")) # read ADA lab anions
# data object name: ada.anions
source(paste0(userPath, "rProjects/", localName, "SuRGE/scripts/analysis/readAnionsDaniels.R")) # read Kit Daniels anions
# data object name: d.anions
source(paste0(userPath, "rProjects/", localName, "SuRGE/scripts/analysis/readNutrientsAda.R")) # read nutrients ran in ADA lab
# data object name: ada.nutrients
source(paste0(userPath, "rProjects/", localName, "SuRGE/scripts/analysis/readNutrientsAwberc.R")) # read AWBERC lab nutrient results
# data object name: chem21
source(paste0(userPath, "rProjects/", localName, "SuRGE/scripts/analysis/readNutrientsR10_2018.R")) # read AWBERC nutrients for 2018 R10
# data object name: chem18
source(paste0(userPath, "rProjects/", localName, "SuRGE/scripts/analysis/readOcAda.R")) # read ADA TOC/DOC data
# data object name: ada.oc
source(paste0(userPath, "rProjects/", localName, "SuRGE/scripts/analysis/readOcMasi.R")) # read 2020 TOC run at MASI lab
# data object name: toc.masi
source(paste0(userPath, "rProjects/", localName, "SuRGE/scripts/analysis/readTteb.R")) # TTEB metals, TOC, DOC
# data object name: tteb.all
source(paste0(userPath, "rProjects/", localName, "SuRGE/scripts/analysis/readChlorophyllR10_2018.R")) # 2018 R10 chlorophyll
# data object name: chl18





# inspect object to merge
# each df contains 10 - 95 observations
list(ada.anions, d.anions, ada.nutrients, chem21, chem18, 
     ada.oc, toc.masi, tteb.all, chl18) %>% 
  map_dfc(., nrow)

# merged object should have at most 462 rows
list(ada.anions, d.anions, ada.nutrients, chem21, chem18, 
     ada.oc, toc.masi, tteb.all, chl18) %>% 
  map_int(., nrow) %>% sum(.) # 462 observations

# merge chem21, chem18, and ada.nutrients
chemistry <- list(ada.anions, d.anions, ada.nutrients, chem21, chem18, 
                      ada.oc, toc.masi, tteb.all, chl18) %>% 
  # map_depth(2, ~select(., -sample_filter)) %>%
  # map_depth(1, function(x) reduce(x, left_join)) %>%
  reduce(full_join) 
#%>%
  # mutate(toc = case_when(
  #   is.na(toc) & !is.na(tteb.toc) ~ tteb.toc, 
  #   TRUE ~ toc)) %>% 
  # mutate(toc_flag = case_when(
  #   is.na(toc_flag) & !is.na(tteb.toc_flag) ~ tteb.toc_flag, 
  #   TRUE ~ toc_flag)) %>% 
  # mutate(toc_units = case_when(
  #   is.na(toc_units) & !is.na(tteb.toc_units) ~ "mg_c_l", 
  #   TRUE ~ toc_units)) %>%
  # select(-contains("tteb")) 

dim(chemistry) #430 rows?

# Any duplicates among unique identifiers?
# 338 duplicates, this is a problem
janitor::get_dupes(chemistry %>% select(lake_id, site_id, sample_depth, sample_type))


