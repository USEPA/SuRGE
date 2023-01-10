

# Bring in chemistry data objects----
# 
# localName <- if (grepl("JBEAULIE", userPath)) {
#   "Jake/"
# } else if (grepl("JCOR", userPath, ignore.case = TRUE)) {
#   "Joe/"} 


# # Can source scripts that generate data objects here, or run them from masterScript.R
# source(paste0(userPath, "rProjects/", localName, "SuRGE/scripts/analysis/readAnionsAda.R")) # read ADA lab anions
# # data object name: ada.anions
# source(paste0(userPath, "rProjects/", localName, "SuRGE/scripts/analysis/readAnionsDaniels.R")) # read Kit Daniels anions
# # data object name: d.anions, d anions.aggregated
# source(paste0(userPath, "rProjects/", localName, "SuRGE/scripts/analysis/readNutrientsAda.R")) # read nutrients ran in ADA lab
# # data object name: ada.nutrients
# source(paste0(userPath, "rProjects/", localName, "SuRGE/scripts/analysis/readNutrientsAwberc.R")) # read AWBERC lab nutrient results
# # data object name: chem21
# source(paste0(userPath, "rProjects/", localName, "SuRGE/scripts/analysis/readNutrientsR10_2018.R")) # read AWBERC nutrients for 2018 R10
# # data object name: chem18
# source(paste0(userPath, "rProjects/", localName, "SuRGE/scripts/analysis/readOcAda.R")) # read ADA TOC/DOC data
# # data object name: ada.oc
# source(paste0(userPath, "rProjects/", localName, "SuRGE/scripts/analysis/readOcMasi.R")) # read 2020 TOC run at MASI lab
# # data object name: toc.masi
# source(paste0(userPath, "rProjects/", localName, "SuRGE/scripts/analysis/readTteb.R")) # TTEB metals, TOC, DOC
# # data object name: tteb.all
# source(paste0(userPath, "rProjects/", localName, "SuRGE/scripts/analysis/readChlorophyllR10_2018.R")) # 2018 R10 chlorophyll
# # data object name: chl18
# source(paste0(userPath, "rProjects/", localName, "SuRGE/scripts/analysis/readPigmentsMicrocystin.R")) # 2020+ chloro/phyco
# # data object name: pigments_20_21_22


# Inspect objects----

# inspect object to merge
## each df contains 10 - 175 observations [12/08/2022]
list(ada.anions, d.anions, ada.nutrients, chemCinNutrients, chem18, 
     ada.oc, toc.masi, tteb.all, chl18, pigments_20_21_22) %>% 
  map_dfc(., nrow)

# are the unique IDs formatted identically across the dfs?
## All have lake_id, site_id, sample_depth, sample_type
list(ada.anions, d.anions, ada.nutrients, chemCinNutrients, chem18, 
     ada.oc, toc.masi, tteb.all, chl18, pigments_20_21_22) %>% 
  map(., function(x) select(
    x, lake_id, site_id, sample_depth, sample_type) %>% 
      str(.))
## which ones have a visit field?
### chemCinNutrients, tteb.all, pigments_20_21_22
list(ada.anions, d.anions, ada.nutrients, chemCinNutrients, chem18, 
     ada.oc, toc.masi, tteb.all, chl18, pigments_20_21_22) %>% 
  map_lgl(., function(x) x %>% {"visit" %in% names(.)})


# Merge objects----
# All observations are uniquely identified by a combination of lake_id, site_id,
# sample_depth, and sample_type.  Lakes 281 and 250 were sampled twice, therefore
# objects containing data from either of those lakes (chemCinNutrients, tteb.all, 
# pigments_20_21_22) must have a visit column. This visit column will be propagated
# into the final merged data object.
# Objects must be joined in proper order to avoid unexpected duplicates.  Best to 
# merge objects that share names other than the unique identifiers.  For example, 
# all nutrient objects should be joined, all anion object should be joined, all 
# chlorophyll should be joined, etc.  After that, we can join together the nutrient, 
# anion, chlorophyll....objects.

# The visit column must be present for joins to work properly, as some of the
# data objects contain this column already. In joins where neither object 
# contains the visit column, visit = 1 for all observations. In joins where
# one object has the visit column, use ifelse to replace NAs with visit = 1.

# merge objects one at a time to check duplicates
# When merging objects containing the same analytes, rbind should give
# same result as join.  Either way, the nrow of the joined object should
# be equal to the sum of the row of the original objects.
nutrients1 <- chemCinNutrients %>%
  full_join(chem18) %>%
  mutate(visit = (ifelse( # visit = 1 if visit column is otherwise blank/NA
    is.na(visit), 1, visit)))
# check for unexpected behavior
nrow(chemCinNutrients) + nrow(chem18) == nrow(nutrients1) # TRUE!, good
janitor::get_dupes( 
  select(nutrients1, lake_id, site_id, sample_depth, sample_type, visit)) 
# no dups

nutrients2 <- nutrients1 %>%
  full_join(ada.nutrients) %>%
  mutate(visit = (ifelse( # visit = 1 if visit column is otherwise blank/NA
    is.na(visit), 1, visit)))
# check for unexpected behavior
nrow(nutrients1) + nrow(ada.nutrients) == nrow(nutrients2) # TRUE, good!
janitor::get_dupes(
  select(nutrients2, lake_id, site_id, sample_depth, sample_type, visit)) 
# no dupes

anions <- ada.anions %>%
  full_join(d.anions.aggregated) %>%
  mutate(visit = 1) # only two lakes with visit == 2 are not in these files
# check for unexpected behavior
nrow(ada.anions) + nrow(d.anions.aggregated) == nrow(anions) # TRUE, good!
janitor::get_dupes(
  select(anions, lake_id, site_id, sample_depth, sample_type)) 
# no dupes

oc <- ada.oc %>%
  full_join(toc.masi) %>%
  mutate(visit = 1) # only two lakes with visit == 2 are not in these files
# check for unexpected behavior
nrow(ada.oc) + nrow(toc.masi) == nrow(oc) # TRUE, good!
janitor::get_dupes(
  select(oc, lake_id, site_id, sample_depth, sample_type)) 
# no dupes

pigments <- chl18 %>%
  full_join(pigments_20_21_22) %>%
  mutate(visit = (ifelse( # visit = 1 if visit column is otherwise blank/NA
    is.na(visit), 1, visit)))
# check for unexpected behavior
nrow(chl18) + nrow(pigments_20_21_22) == nrow(pigments) # TRUE, good!
janitor::get_dupes(select(pigments, lake_id, site_id, sample_depth, sample_type)) 


# When joining objects containing different analytes, the nrow of the
# joined object can't be easily predicted, but the objects to be joined can't 
# contain duplicates of the joining variables.
metal.pig <- tteb.all %>%
  full_join(pigments) 
# check for unexpected behavior
janitor::get_dupes(
  select(metal.pig, lake_id, site_id, sample_depth, sample_type, visit)) 
# no dups

metal.pig.oc <- metal.pig %>%
  full_join(oc)
# check for unexpected behavior
janitor::get_dupes(
  select(metal.pig.oc, lake_id, site_id, sample_depth, sample_type, visit)) 
# no dups

metal.pig.oc.anions <- metal.pig.oc %>%
  full_join(anions)
# check for unexpected behavior
janitor::get_dupes(
  select(metal.pig.oc.anions, lake_id, site_id, sample_depth, sample_type, visit)) 
# no dups

chemistry_all <- nutrients2 %>%
  full_join(metal.pig.oc.anions)
# check for unexpected behavior
janitor::get_dupes(
  select(chemistry_all, lake_id, site_id, sample_depth, sample_type, visit)) 
# no dups


# The tteb.all object contains metals, DOC, and TOC data.  To prevent erroneous duplicates
# when joining tteb.all with other objects containing TOC data, we appended "tteb."
# to TOC and DOC column names in tteb.all (see readTteb.R).  Any samples where TOC/DOC was run
# at TTEB will have TOC/DOC numbers in the tteb.toc/tteb.doc columns AND NAs in the TOC/DOC columns
# that came from toc.masi and ada.oc.  For any observations that meet these criteria,
# move the tteb OC data into the OC columns, then remove the tteb.toc columns.
# TTEB will also run anions from the 2022 field season.  When we get those data,
# we will need to take a similar approach to deal with anion analytes in tteb,
# ada.anions, and d.anions.
chemistry_all <- chemistry_all %>%
  mutate(toc = case_when(
    is.na(toc) & !is.na(tteb.toc) ~ tteb.toc,
    TRUE ~ toc)) %>%
  mutate(toc_flags = case_when(
    is.na(toc_flags) & !is.na(tteb.toc_flags) ~ tteb.toc_flags,
    TRUE ~ toc_flags)) %>%
  mutate(toc_units = case_when(
    is.na(toc_units) & !is.na(tteb.toc_units) ~ "mg_c_l",
    TRUE ~ toc_units)) %>%
  mutate(doc = case_when(
    is.na(doc) & !is.na(tteb.doc) ~ tteb.doc,
    TRUE ~ doc)) %>%
  mutate(doc_flags = case_when(
    is.na(doc_flags) & !is.na(tteb.doc_flags) ~ tteb.doc_flags,
    TRUE ~ doc_flags)) %>%
  mutate(doc_units = case_when(
    is.na(doc_units) & !is.na(tteb.doc_units) ~ "mg_c_l",
    TRUE ~ doc_units)) %>%
  select(-contains("tteb"))

janitor::get_dupes(
  select(chemistry_all, lake_id, site_id, sample_depth, sample_type, visit)) 


# Arrange columns----
chemistry_all <- chemistry_all %>%
  relocate(lake_id, site_id, sample_depth, sample_type, visit, 
           sort(colnames(.))) # others arranged alphabetically
