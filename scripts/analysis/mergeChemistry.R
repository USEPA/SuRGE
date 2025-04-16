

# Inspect objects----

# inspect object to merge
## each df contains 10 - 361 observations [4/16/2025]
list(ada.anions, d.anions, ada.nutrients, chemCinNutrients, chem18, 
     ada.oc, toc.masi, tteb.all, chl18, pigments, microcystin, dissolved_gas) %>% 
  map_dfc(., nrow)

# are the unique IDs formatted identically across the dfs?
## All have lake_id, site_id, sample_depth, sample_type
list(ada.anions, d.anions, ada.nutrients, chemCinNutrients, chem18, 
     ada.oc, toc.masi, tteb.all, chl18, pigments, microcystin, dissolved_gas) %>% 
  map(., function(x) select(
    x, lake_id, site_id, sample_depth, sample_type) %>% 
      str(.))
## which ones have a visit field?
### ada.anions, ada.nutrients, ada.oc, chemCinNutrients, tteb.all, pigments, 
### microcystin dissolved_gas
list(ada.anions, d.anions, ada.nutrients, chemCinNutrients, chem18, 
     ada.oc, toc.masi, tteb.all, chl18, pigments, microcystin, dissolved_gas) %>% 
  map_lgl(., function(x) x %>% {"visit" %in% names(.)})


# Merge objects----
# All observations are uniquely identified by a combination of lake_id, site_id,
# sample_depth, and sample_type.  Lakes 281/250 (CIN) and 147/148 (ADA) were sampled 
# twice, therefore objects containing data from those lakes must have a visit column.
# This visit column will be propagated into the final merged data object.
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
  mutate(visit = (ifelse( # visit = 1 if visit column is otherwise blank/NA.  needed for daniels anions
    is.na(visit), 1, visit)))
# # check for unexpected behavior
nrow(ada.anions) + nrow(d.anions.aggregated) == nrow(anions) # TRUE, good!
janitor::get_dupes(
  select(anions, lake_id, site_id, sample_depth, sample_type, visit)) 
# no dupes

oc <- ada.oc %>%
  full_join(toc.masi) %>%
  mutate(visit = (ifelse( # visit = 1 if visit column is blank/NA (all toc.masi data)
  is.na(visit), 1, visit)))
# check for unexpected behavior
nrow(ada.oc) + nrow(toc.masi) == nrow(oc) # TRUE, good!
janitor::get_dupes(
  select(oc, lake_id, site_id, sample_depth, sample_type, visit)) 
# no dupes

pigments_all <- chl18 %>%
  full_join(pigments) %>%
  mutate(visit = (ifelse( # visit = 1 if visit column is otherwise blank/NA
    is.na(visit), 1, visit)))
# check for unexpected behavior
nrow(chl18) + nrow(pigments) == nrow(pigments_all) # TRUE, good!
janitor::get_dupes(select(pigments_all, lake_id, site_id, 
                          sample_depth, sample_type, visit)) 


# When joining objects containing different analytes, the nrow of the
# joined object can't be easily predicted, but the objects to be joined can't 
# contain duplicates of the joining variables.
metal.pig <- tteb.all %>%
  full_join(pigments_all) 
# check for unexpected behavior
janitor::get_dupes(
  select(metal.pig, lake_id, site_id, sample_depth, sample_type, visit)) 

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

metal.pig.oc.anions.nutrients <- nutrients2 %>%
  full_join(metal.pig.oc.anions)
# check for unexpected behavior
janitor::get_dupes(
  select(metal.pig.oc.anions.nutrients, lake_id, site_id, sample_depth, sample_type, visit)) 
# no dups

metal.pig.oc.anions.nutrients.micro <- metal.pig.oc.anions.nutrients %>%
  full_join(microcystin)
# check for unexpected behavior
janitor::get_dupes(
  select(metal.pig.oc.anions.nutrients.micro, lake_id, site_id, sample_depth, 
         sample_type, visit))

chemistry_all <- metal.pig.oc.anions.nutrients.micro %>%
  full_join(dissolved_gas %>%
              select(lake_id, site_id, sample_depth, sample_type, visit,
                     contains("dissolved"), contains("ratio")))
# check for unexpected behavior
janitor::get_dupes(
  select(chemistry_all, lake_id, site_id, sample_depth, sample_type, visit)) 


# no dups
# The tteb.all object contains metals, anion, DOC, and TOC data.  To prevent erroneous duplicates
# when joining tteb.all with other objects containing TOC, DOC, or anion data, we appended "tteb."
# to TOC, DOC, and anion column names in tteb.all (see readTteb.R).  Any samples where TOC/DOC/anions were run
# at TTEB will have TOC/DOC/anion numbers in the tteb.toc/tteb.doc/tteb.cl.... columns AND NAs in the TOC/DOC/cl... columns
# that came from toc.masi, ada.oc, and anions.  For any observations that meet these criteria,
# move the tteb data into the tOC/doc/analyte columns, then remove the tteb. columns.
chemistry_all <- chemistry_all %>%
  # TOC
  mutate(toc = case_when(is.na(toc) & !is.na(tteb.toc) ~ tteb.toc,
                         TRUE ~ toc)) %>%
  mutate(toc_flags = case_when(is.na(toc_flags) & !is.na(tteb.toc_flags) ~ tteb.toc_flags,
                               TRUE ~ toc_flags)) %>%
  mutate(toc_units = case_when(is.na(toc_units) & !is.na(tteb.toc_units) ~ "mg_c_l",
                               TRUE ~ toc_units)) %>%
  
  # DOC
  mutate(doc = case_when(is.na(doc) & !is.na(tteb.doc) ~ tteb.doc,
                         TRUE ~ doc)) %>%
  mutate(doc_flags = case_when(is.na(doc_flags) & !is.na(tteb.doc_flags) ~ tteb.doc_flags,
                               TRUE ~ doc_flags)) %>%
  mutate(doc_units = case_when(is.na(doc_units) & !is.na(tteb.doc_units) ~ "mg_c_l",
                               TRUE ~ doc_units)) %>%
  
  # br
  mutate(br = case_when(is.na(br) & !is.na(tteb.br) ~ tteb.br,
                        TRUE ~ br)) %>%
  mutate(br_flags = case_when(is.na(br_flags) & !is.na(tteb.br_flags) ~ tteb.br_flags,
                               TRUE ~ br_flags)) %>%
  mutate(br_units = case_when(is.na(br_units) & !is.na(tteb.br_units) ~ tteb.br_units,
                              !is.na(br_units) & is.na(tteb.br_units) ~ br_units,
                              !is.na(br_units) & is.na(tteb.br_units) ~ NA_character_,
                               TRUE ~ NA_character_)) %>%
  
  # cl
  mutate(cl = case_when(is.na(cl) & !is.na(tteb.cl) ~ tteb.cl,
                        TRUE ~ cl)) %>%
  mutate(cl_flags = case_when(is.na(cl_flags) & !is.na(tteb.cl_flags) ~ tteb.cl_flags,
                              TRUE ~ cl_flags)) %>%
  mutate(cl_units = case_when(is.na(cl_units) & !is.na(tteb.cl_units) ~ tteb.cl_units,
                              !is.na(cl_units) & is.na(tteb.cl_units) ~ cl_units,
                              !is.na(cl_units) & is.na(tteb.cl_units) ~ NA_character_,
                              TRUE ~ NA_character_)) %>%
  
  # so4
  mutate(so4 = case_when(is.na(so4) & !is.na(tteb.so4) ~ tteb.so4,
                        TRUE ~ so4)) %>%
  mutate(so4_flags = case_when(is.na(so4_flags) & !is.na(tteb.so4_flags) ~ tteb.so4_flags,
                              TRUE ~ so4_flags)) %>%
  mutate(so4_units = case_when(is.na(so4_units) & !is.na(tteb.so4_units) ~ tteb.so4_units,
                              !is.na(so4_units) & is.na(tteb.so4_units) ~ so4_units,
                              !is.na(so4_units) & is.na(tteb.so4_units) ~ NA_character_,
                              TRUE ~ NA_character_)) %>%
  
  # f
  mutate(f = case_when(is.na(f) & !is.na(tteb.f) ~ tteb.f,
                        TRUE ~ f)) %>%
  mutate(f_flags = case_when(is.na(f_flags) & !is.na(tteb.f_flags) ~ tteb.f_flags,
                              TRUE ~ f_flags)) %>%
  mutate(f_units = case_when(is.na(f_units) & !is.na(tteb.f_units) ~ tteb.f_units,
                              !is.na(f_units) & is.na(tteb.f_units) ~ f_units,
                              !is.na(f_units) & is.na(tteb.f_units) ~ NA_character_,
                              TRUE ~ NA_character_)) %>%
  select(-contains("tteb"))

janitor::get_dupes(
  select(chemistry_all, lake_id, site_id, sample_depth, sample_type, visit)) 


# Arrange columns----
chemistry_all <- chemistry_all %>%
  relocate(lake_id, site_id, sample_depth, sample_type, visit, 
           sort(colnames(.))) # others arranged alphabetically


dim(chemistry_all) # 510, 146 [1/24/2025]
