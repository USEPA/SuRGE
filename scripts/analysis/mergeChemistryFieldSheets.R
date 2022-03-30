# MERGING FIELD SHEETS (DATA TAB) AND CLEAN CHEMISTRY FILE (NO DUPS/BLANKS)

# Merge these two object
chemistry
fld_sheet 

# Will merge on common names: lake_id and site_id
names(chemistry)[names(chemistry) %in% names(fld_sheet)] #lake_id and site_id
names(fld_sheet)[names(fld_sheet) %in% names(chemistry)] #lake_id and site_id
class(chemistry$lake_id) == class(fld_sheet$lake_id) # TRUE
class(chemistry$site_id) == class(fld_sheet$site_id) # TRUE

# Check dimensions
dim(chemistry) # 154, 129 [3/30/2022]
dim(fld_sheet) # 1036, 72 [3/30/2022]

# Check for correspondence among unique identifiers
# 7 lakes in chemistry, but not in field sheets.  These are all
# R10 lakes from 2018.  As of 3/30/2022, these field sheets
# haven't been read in yet.
chemistry %>% filter(!(lake_id %in% fld_sheet$lake_id)) %>%
  select(lake_id) %>% distinct() %>% print(n=Inf)


# any lakes in fld_sheets, but not in chemistry? -NO
fld_sheet %>% filter(!(lake_id %in% chemistry$lake_id)) %>%
  select(lake_id) %>% distinct() %>% print(n=Inf)

chem_fld <- full_join(chemistry, fld_sheet)
dim(chem_fld) # 1127, 200 [3/30/2022]

# write to disk for reference in lake reports
save(chem_fld, file = "output/chem_fld.RDATA")
