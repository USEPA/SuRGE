

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

# merge a few and check results
foo <- 
ada.anions %>%
  ungroup() %>%
  full_join(ungroup(d.anions)) %>% # 76 observations, correct, no dups
  full_join(ungroup(ada.nutrients)) # 138 observations, no dups
janitor::get_dupes(foo %>% select(lake_id, site_id, sample_depth, sample_type)) # 0 dups!

# add chem21
poo <- foo %>% 
  full_join(ungroup(chem21)) # 230 observations
janitor::get_dupes(poo %>% select(lake_id, site_id, sample_depth, sample_type)) # 129 dups!
# Dups in chem21
janitor::get_dupes(chem21 %>% select(lake_id, site_id, sample_depth, sample_type)) # 2 dups, doesn't explain problem


# lake 119 is duplicated in poo.  take a close look at that lake.
# all look fine?
zoo <- poo %>% 
  filter(lake_id == "119")
str(zoo) # looks fine
unique(zoo$lake_id) # only 119
unique(zoo$site_id) # only 4
unique(zoo$sample_depth) # only deep and shallow
unique(zoo$sample_type) # only unknown

# are there other duplicated names between foo and chem21?
# OK, chem21 and ada.nutrients share analyte names!  I think we need to first
# merge objects that share analyte names!
names(foo)[names(foo) %in% names(chem21)]


# Merge objects----

# merge objects one at a time to check duplicates

nutrients1 <- chem21 %>%
  full_join(chem18) 
janitor::get_dupes(select(nutrients1, lake_id, site_id, sample_depth, sample_type)) 

nutrients2 <- nutrients1 %>%
  full_join(ada.nutrients)
janitor::get_dupes(select(nutrients2, lake_id, site_id, sample_depth, sample_type)) 
# 2 dupe (lake 233)

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
# 2 dupe (lake 275)

metal.chl.oc <- metal.chl %>%
  full_join(oc)
janitor::get_dupes(select(metal.chl.oc, lake_id, site_id, sample_depth, sample_type)) 
# 2 dupe (lake 275)

metal.chl.oc.anions <- metal.chl.oc %>%
  full_join(anions)
janitor::get_dupes(select(metal.chl.oc.anions, lake_id, site_id, sample_depth, sample_type)) 
# 2 dupe (lake 275)

chemistry <- nutrients2 %>%
  full_join(metal.chl.oc.anions)
janitor::get_dupes(select(chemistry, lake_id, site_id, sample_depth, sample_type)) 
# 4 dupe (lake 275, lake 233)
# lake 233 dupe is ; lake 275 dupe is known (see readTteb.R)



chemistry <- chemistry %>%
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



