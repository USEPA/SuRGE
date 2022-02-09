

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



# Inspect objects----

# inspect object to merge
# each df contains 10 - 95 observations
list(ada.anions, d.anions, ada.nutrients, chem21, chem18, 
     ada.oc, toc.masi, tteb.all, chl18) %>% 
  map_dfc(., nrow)

# are the unique IDs formatted identically across the dfs? yes
list(ada.anions, d.anions, ada.nutrients, chem21, chem18, 
     ada.oc, toc.masi, tteb.all, chl18) %>% 
  map(., function(x) select(x, lake_id, site_id, sample_depth, sample_type) %>% str(.))


# Merge objects----
# All observations are uniquely identified by a combination of lake_id, site_id,
# sample_depth, and sample_type.  Objects must be joined in proper order to avoid
# unexpected duplicates.  Best to merge objects that share names other than
# the unique identifiers.  For example, all nutrient objects should be joined, all
# anion object should be joined, all chlorophyll should be joined, etc.  After 
# that, we can join together the nutrient, anion, chlorophyll....objects.

# merge objects one at a time to check duplicates
nutrients1 <- chem21 %>%
  full_join(chem18) 
janitor::get_dupes(select(nutrients1, lake_id, site_id, sample_depth, sample_type)) 

nutrients2 <- nutrients1 %>%
  full_join(ada.nutrients)
janitor::get_dupes(select(nutrients2, lake_id, site_id, sample_depth, sample_type)) 
# no dupes

anions <- ada.anions %>%
  full_join(d.anions)
janitor::get_dupes(select(anions, lake_id, site_id, sample_depth, sample_type)) 
# no dupes

oc <- ada.oc %>%
  full_join(toc.masi)
janitor::get_dupes(select(oc, lake_id, site_id, sample_depth, sample_type)) 
# no dupes

metal.chl <- tteb.all %>%
  full_join(chl18)
janitor::get_dupes(select(metal.chl, lake_id, site_id, sample_depth, sample_type)) 
# 2 dupe (lake 275).  lake 275 dupe is known (see readTteb.R)

metal.chl.oc <- metal.chl %>%
  full_join(oc)
janitor::get_dupes(select(metal.chl.oc, lake_id, site_id, sample_depth, sample_type)) 
# 2 dupe (lake 275) lake 275 dupe is known (see readTteb.R)

metal.chl.oc.anions <- metal.chl.oc %>%
  full_join(anions)
janitor::get_dupes(select(metal.chl.oc.anions, lake_id, site_id, sample_depth, sample_type)) 
# 2 dupe (lake 275) lake 275 dupe is known (see readTteb.R)

chemistry_all <- nutrients2 %>%
  full_join(metal.chl.oc.anions)
janitor::get_dupes(select(chemistry, lake_id, site_id, sample_depth, sample_type)) 
# 2 dupe (lake 275) lake 275 dupe is known (see readTteb.R)



# The tteb.all object contains metals and TOC data.  To prevent erroneous duplicates
# when joining tteb.all with other objects containing TOC data, we appended "tteb."
# to TOC column names in tteb.all (see readTteb.R).  Any samples where TOC was run
# at TTEB will have TOC numbers in the tteb.toc columns AND NAs in the TOC columns
# that came from toc.masi and ada.oc.  For any observations that meet these criteria,
# move the tteb OC data into the OC columns, then remove the tteb.toc columns.
# TTEB will also run anions from the 2022 field season.  When we get those data,
# we will need to take a similar approach to deal with anion analytes in tteb,
# ada.anions, and d.anions.
chemistry_all <- chemistry_all %>%
  mutate(toc = case_when(
    is.na(toc) & !is.na(tteb.toc) ~ tteb.toc,
    TRUE ~ toc)) %>%
  mutate(toc_flag = case_when(
    is.na(toc_flag) & !is.na(tteb.toc_flag) ~ tteb.toc_flag,
    TRUE ~ toc_flag)) %>%
  mutate(toc_units = case_when(
    is.na(toc_units) & !is.na(tteb.toc_units) ~ "mg_c_l",
    TRUE ~ toc_units)) %>%
  select(-contains("tteb"))



