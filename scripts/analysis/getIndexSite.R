# EXTRACT INDEX SITE LOCATION FROM DEPTH PROFILE DATA
# USE THIS TO FLAG INDEX SITE IN chem_fld


index_site <- depth_profiles_all %>%
  distinct(lake_id, site_id, visit) %>%
  mutate(index_site = TRUE)
